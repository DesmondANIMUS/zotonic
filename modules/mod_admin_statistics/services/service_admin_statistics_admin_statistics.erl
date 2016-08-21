%% @author Tah Teche <tahteche@gmail.com>
%% @copyright 2016 Tah Teche
%% Date: 2016-8-20
%% @doc Get system statistics.

%% Copyright 2009 Tah Teche
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

-module(service_admin_statistics_admin_statistics).
-author("Tah Teche <tahteche@gmail.com>").

-svc_title("Get system statistics").
-svc_needauth(true).

-export([process_get/2]).

-include_lib("zotonic.hrl").


process_get(_ReqData, Context) ->
    BeginDateTimeString = z_context:get_q("start", Context),
    EndDateTimeString = z_context:get_q("end", Context),
    BeginDateTime = z_convert:to_datetime(BeginDateTimeString),
    EndDateTime = z_convert:to_datetime(EndDateTimeString),
    z_convert:to_json(m_z_stats:get_stats_from_db(BeginDateTime, EndDateTime, Context)).