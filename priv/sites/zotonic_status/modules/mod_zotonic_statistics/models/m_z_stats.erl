%% @author Tah Teche Tende <tahteche@gmail.com>
%% @copyright 2016 Tah Teche Tende
%% Date: 2016-06-20
%%
%% @doc Model for storing system statistics.

%% Copyright 2016 Tah Teche Tende
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.



-module(m_z_stats).
-author("Tah Teche Tende <tahteche@gmail.com>").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    insert/4,
    install_zotonic_stats_table/1
 ]).

-include_lib("zotonic.hrl").

%Handles all date queries. 
%Syntax: m.z_stats.date[QueryString]
m_find_value(date, #m{value=undefined}, _Context) -> 
	#m{value=[{type, date}]};

%Handles date queries over a date range 
%Syntax: m.z_stats.date[query fromDate=2016-12-20 toDate=2016-12-28]
m_find_value({query, Query}, #m{value=Q} = M, _Context) ->
	M#m{value=Query ++ Q};

%Handles date queries for one day
%Syntax: m.z_stats.date[2016-12-20]
m_find_value(Date, #m{value=Query} = M, _Context) ->
	FromDate = [{fromDate, Date}],
	M#m{value=FromDate ++ Query}.


m_to_list(_Source, _Context) ->
    [].

m_value(_Source, _Context) ->
    [].

%Get data from database
get_stats(FromDate, Context) ->
    z_db:assoc("select * from zotonic_stats where time_recorded >= $1 order by created asc",
        [FromDate], Context).
get_stats(FromDate, ToDate, Context) ->
    z_db:assoc("select * from zotonic_stats where time_recorded between $1 and $2 order by timestamp asc",
        [FromDate, ToDate], Context).

%%Insert or update stats into a particular database row
%%TODO: Add support for round robin inserts

insert(Metric, DataPoint, Value, Context) ->
    Key = get_datapoint_db_column(Metric, DataPoint),
    Respond = z_depcache:set(Key, Value, 10, Context),   
    case all_datapoints_cached(Context) of
        true ->
            DataPoints = get_datapoints_from_cache(Context),
            z_db:q("insert into zotonic_stats (time_recorded, system__process_count, system__run_queue,
                system__port_count, io__input, io__output, io__open_tcp_connections, memory__total,
                memory__binary, memory__ets, memory__code, memory__system) 
                values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)", 
                DataPoints,
                Context),
            Respond;
        false -> Respond
    end.

%%
%% Helpers
%%

get_datapoints_from_cache(Context) ->
    Keys = [ DataPoint || {_, DataPoint} <- datapoint_db_column()],
    [ z_depcache:get(Key, Context) || Key <- Keys].

get_datapoint_db_column(Metric, DataPoint) ->
    Key = lists:append(Metric, DataPoint),
    proplist:get_value(Key, datapoint_db_column()).

datapoint_db_column() ->
    [
        {[erlang, system_info, process_count], system__process_count},
        {[erlang, system_info, run_queue], system__run_queue},
        {[erlang, system_info, port_count], system__port_count},
        {[erlang, io, input], io__input},
        {[erlang, io, output], io__output},
        {[erlang, io, open_tcp_connections], io__open_tcp_connections},
        {[erlang, memory, total], memory__total},
        {[erlang, memory, binary], memory__binary},
        {[erlang, memory, ets], memory__ets},
        {[erlang, memory, code], memory__code},
        {[erlang, memory, system], memory__system}
    ].

all_datapoints_cached(Context) ->
    is_cached(system__process_count, Context) andalso
    is_cached(system__run_queue, Context) andalso
    is_cached(system__port_count, Context) andalso
    is_cached(io__input, Context) andalso
    is_cached(io__output, Context) andalso
    is_cached(io__open_tcp_connections, Context) andalso
    is_cached(memory__total, Context) andalso
    is_cached(memory__binary, Context) andalso
    is_cached(memory__ets, Context) andalso
    is_cached(memory__ets, Context) andalso
    is_cached(memory__system, Context).

is_cached(Key, Context) ->
    case z_depcache:get(Key, Context) of
        undefined -> false;
        {ok, _} -> true
    end.

%%
%% Schema
%%

install_zotonic_stats_table(Context) ->
    case z_db:table_exists(zotonic_stats, Context) of
        false ->
            z_db:create_table(zotonic_stats, [
                        #column_def{name=id, type="serial", is_nullable=false},
                        #column_def{name=time_recorded, type="timestamp with time zone", is_nullable=false},
                        #column_def{name=system__process_count, type="real", is_nullable=false},
                        #column_def{name=system__run_queue, type="real", is_nullable=false},
                        #column_def{name=system__port_count, type="real", is_nullable=false},
                        #column_def{name=io__input, type="real", is_nullable=false},
                        #column_def{name=io__output, type="real", length=32, is_nullable=false},
                        #column_def{name=io__open_tcp_connections, type="real", is_nullable=false},
                        #column_def{name=memory__total, type="real", is_nullable=false},
                        #column_def{name=memory__binary, type="real", is_nullable=false},
                        #column_def{name=memory__ets, type="real", is_nullable=false},
                        #column_def{name=memory__code, type="real", is_nullable=false},
                        #column_def{name=memory__system, type="real", is_nullable=false}                        
                    ], Context);
        true ->
            ok
    end.