%%%-------------------------------------------------------------------
%% @doc Op-based CRDT
%% @end
%%%-------------------------------------------------------------------

-module(crdts_op_based).

-behaviour(gen_server).

%% gen_server callback APIs
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

%% Exported APIs
-export([start_link/4, query/2, update/2, join/2]).

%%====================================================================
%% Types, Records, and Macros
%%====================================================================

-define(STATE, ?MODULE).
-type payload() :: any().
-record(?STATE, {
           module :: module(),
           payload :: payload(),
           neighbors = gb_sets:empty() :: gb_sets:set()
          }).

%%====================================================================
%% Callbacks
%%====================================================================

-callback init(Args::any()) -> payload().
-callback handle_query(Query::any(), payload()) -> Data::any().
-callback handle_update_source(Args::any(), payload()) -> ToDown::any().
-callback handle_update_downstream(FromSrc::any(), payload()) -> payload().

%%====================================================================
%% Exported APIs
%%====================================================================

start_link(Name, Module, Interval, Args) ->
    gen_server:start_link(Name, ?MODULE, {Module, Interval, Args}, []).

query(ServerRef, Query) ->
    gen_server:call(ServerRef, {query, Query}).

update(ServerRef, Args) ->
    gen_server:cast(ServerRef, {update, Args}).

join(ServerRef, AnotherServerRef) ->
    gen_server:cast(ServerRef, {join, AnotherServerRef}).

%%====================================================================
%% gen_server callback APIs
%%====================================================================

init({Module, Interval, Args}) ->
    Payload = Module:init(Args),
    State = #?STATE{module = Module, payload = Payload},
    erlang:send_after(Interval, self(), broadcast),
    {ok, State}.

handle_cast({update, Args}, State = #?STATE{module = Module, payload = Payload, neighbors = Neighbors}) ->
    % ok = io:format("update: args=~p~n", [Args]),
    FromSrc = Module:handle_update_source(Args, Payload),
    %% reliable broadcast
    ok = lists:foreach(fun (P) -> update_downstream(P, FromSrc) end, gb_sets:to_list(Neighbors)),
    {noreply, State};
handle_cast({update_downstream, FromSrc}, State = #?STATE{module = Module, payload = Payload0}) ->
    % ok = io:format("merge: payload=~p~n", [RemotePayload]),
    Payload1 = Module:handle_update_downstream(FromSrc, Payload0),
    {noreply, State#?STATE{payload = Payload1}};
handle_cast({join, ServerRef}, State = #?STATE{neighbors = Neighbors0}) ->
    % ok = io:format("join: ref=~p~n", [ServerRef]),
    Neighbors1 = gb_sets:add(ServerRef, Neighbors0),
    %% There are no thing corresponding to state-based CRDT's `merge`.
    {noreply, State#?STATE{neighbors = Neighbors1}};
handle_cast(_Req, _State) ->
    {noreply, _State}.

handle_call({query, Query}, _From, State = #?STATE{module = Module, payload = Payload}) ->
    % ok = io:format("query: query=~p~n", [Query]),
    Data = Module:handle_query(Query, Payload),
    {reply, Data, State};
handle_call(_Req, _From, _State) ->
    {noreply, _State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%====================================================================
%% Internal functions
%%====================================================================

update_downstream(ServerRef, FromSrc) ->
    gen_server:cast(ServerRef, {update_downstream, FromSrc}).
