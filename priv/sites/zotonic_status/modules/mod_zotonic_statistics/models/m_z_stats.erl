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
    
    insert/3,
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
%%TODO: Add support for round robin inserts and use "case...of" to detect failure.

insert(Key, Value, Context) ->  
    db_insert(Key, Value, Context).

%%
%% Helpers
%%


db_insert(Key, Value, Context) ->
    MetricName = {metric_name, Key},
    MetricValue = {metric_value, Value},
    Time = {time_recorded, calendar:universal_time()},
    z_db:insert(zotonic_stats, [Time, MetricName, MetricValue], Context).

%%
%% Schema
%%

%%TODO: Move to different Erlang module
install_zotonic_stats_table(Context) ->
    case z_db:table_exists(zotonic_stats, Context) of
        false ->
            z_db:create_table(zotonic_stats, [
                        #column_def{name=id, type="serial", is_nullable=false},
                        #column_def{name=time_recorded, type="timestamp with time zone", is_nullable=false},
                        #column_def{name=metric_name, type="character varying", length=100, is_nullable=false},
                        #column_def{name=metric_value, type="real", is_nullable=false}                        
                    ], Context);
        true ->
            table_exists
    end.