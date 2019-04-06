%%%-------------------------------------------------------------------
%% @doc Op-based Last-Write-Wins Register (LWW-Register)
%% @end
%%%-------------------------------------------------------------------

-module(crdts_lww_register).

-behaviour(crdts_cmrdt).

%% Exported APIs
-export([start_link/1, value/1, assign/2]).

%% crdts_state_based callback APIs
-export([init/1, handle_query/2, handle_prepare_update/2, handle_effect_update/2]).

%%====================================================================
%% Types, Records, and Macros
%%====================================================================

-define(PAYLOAD, ?MODULE).
-record(?PAYLOAD, {
           value :: any(),
           timestamp :: non_neg_integer()
          }).

%%====================================================================
%% Exported APIs
%%====================================================================

start_link(Name) ->
    crdts_cmrdt:start_link(Name, ?MODULE, []).

value(ServerRef) ->
    crdts_cmrdt:query(ServerRef, value).

assign(ServerRef, Value) ->
    crdts_cmrdt:update(ServerRef, {assign, Value}).

%%====================================================================
%% crdts_state_based callback APIs
%%====================================================================

init([]) ->
    #?PAYLOAD{value = undefined, timestamp = 0}.

handle_query(value, #?PAYLOAD{value = Value}) ->
    Value;
handle_query(Query, _Payload) ->
    {unknown_query, Query}.

handle_prepare_update({assign, Value}, _Payload) ->
    {assign, Value, erlang:system_time()};
handle_prepare_update(Args, _Payload) ->
    {unknown_update, Args}.

handle_effect_update({assign, Value, T1}, Payload = #?PAYLOAD{timestamp = T0}) when T1 > T0 ->
    Payload#?PAYLOAD{value = Value, timestamp = T1};
handle_effect_update({assign, _, _}, Payload) ->
    Payload;
handle_effect_update(_Args, Payload) ->
    Payload.

%%====================================================================
%% Internal functions
%%====================================================================
