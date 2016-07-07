%% @author Tah Teche Tende <tahteche@gmail.com>
%% @copyright 2016 Tah Teche Tende
%% @doc Exometer reporter which sends exometer stats for storage in database.

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
-module(z_exometer_db).

%% exometer_report callback API
-export([
    exometer_init/1,
    exometer_info/2,
    exometer_cast/2,
    exometer_call/3,
    exometer_report/5,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_terminate/2,
    exometer_newentry/2,
    exometer_setopts/4
]).

-include("zotonic.hrl").
-include_lib("exometer/include/exometer.hrl").
-include_lib("emqtt/include/emqtt.hrl").

exometer_init(_Opts) ->
    {ok, []}.

exometer_subscribe(_Metric, _DataPoint, _Extra, _Interval, State) ->
    {ok, State}.

exometer_unsubscribe(_Metric, _DataPoint, _Extra, State) ->
    {ok, State}.

exometer_report(Metric, DataPoint, Extra, Value, State) ->
    %% Insert exometer metric to db.
    Context = get_context(Extra),
    m_z_stats:insert(Metric, DataPoint, Value, Context),
    {ok, State}.

exometer_call(Unknown, From, State) ->
    lager:debug("Unknown call ~p from ~p", [Unknown, From]),
    {ok, State}.
 
exometer_cast(Unknown, State) ->
    lager:debug("Unknown cast: ~p", [Unknown]),
    {ok, State}.
 
exometer_info(Unknown, State) ->
    lager:debug("Unknown info: ~p", [Unknown]),
    {ok, State}.

exometer_newentry(_Entry, State) ->
    {ok, State}.

exometer_setopts(_Metric, _Options, _Status, State) ->
    {ok, State}.

exometer_terminate(_, _State) ->
    ok.

%%
%% Helpers
%%

get_context(undefined) ->
    z_context:new(zotonic_status);
get_context(Site) when is_atom(Site) ->
    z_context:new(Site);
get_context(#context{}=Context) ->
    Context.