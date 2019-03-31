%%%-------------------------------------------------------------------
%% @doc G-Counter
%% @end
%%%-------------------------------------------------------------------

-module(crdts_g_counter).

-behaviour(crdts_state_based).

%% Exported APIs
-export([start_link/3, increment/1, value/1]).

%% crdts_state_based callback APIs
-export([init/1, handle_query/2, handle_update/2, handle_merge/2]).

%%====================================================================
%% Types, Records, and Macros
%%====================================================================

-define(PAYLOAD, ?MODULE).
-record(?PAYLOAD, {
           id :: non_neg_integer(),
           nums :: array:array(non_neg_integer())
          }).

%%====================================================================
%% Exported APIs
%%====================================================================

start_link(Name, Id, NumReplica) ->
    crdts_state_based:start_link(Name, ?MODULE, 1000, {Id, NumReplica}).

increment(ServerRef) ->
    crdts_state_based:update(ServerRef, increment).

value(ServerRef) ->
    crdts_state_based:query(ServerRef, value).

%%====================================================================
%% crdts_state_based callback APIs
%%====================================================================

init({Id, NumReplica}) ->
    Nums = array:new([fixed, {size, NumReplica}, {default, 0}]),
    #?PAYLOAD{id = Id, nums = Nums}.

handle_query(value, Payload) ->
    lists:sum(array:to_list(Payload#?PAYLOAD.nums));
handle_query(Query, _Payload) ->
    {unknown_query, Query}.

handle_update(increment, Payload = #?PAYLOAD{id = Id, nums = Nums0}) ->
    Num = array:get(Id, Nums0),
    Nums1 = array:set(Id, Num + 1, Nums0),
    Payload#?PAYLOAD{nums = Nums1};
handle_update(_Args, Payload) ->
    Payload.

handle_merge(#?PAYLOAD{nums = RemoteNums}, Payload = #?PAYLOAD{nums = LocalNums}) ->
    Nums = array:map(fun (I, N) -> max(N, array:get(I, RemoteNums)) end, LocalNums),
    Payload#?PAYLOAD{nums = Nums}.

%%====================================================================
%% Internal functions
%%====================================================================
