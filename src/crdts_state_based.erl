%%%-------------------------------------------------------------------
%% @doc State-based CRDT: CvRDT (Convergent Replicated Data Type)
%% @end
%%%-------------------------------------------------------------------

-module(crdts_state_based).

-behaviour(gen_server).

%% gen_server callback APIs
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3, terminate/2]).

%% Exported APIs
-export([start_link/4, query/2, update/2, merge/2, join/2]).

%%====================================================================
%% Types, Records, and Macros
%%====================================================================

-define(STATE, ?MODULE).
-type payload() :: any().
-record(?STATE, {
           module :: module(),
           interval :: non_neg_integer(), % millisecond
           payload :: payload(),
           last_payload :: payload(),
           neighbors = gb_sets:empty() :: gb_sets:set()
          }).

%%====================================================================
%% Callbacks
%%====================================================================

-callback init(Args::any()) -> payload().
-callback handle_query(Query::any(), payload()) -> Data::any().
-callback handle_update(Args::any(), payload()) -> payload().
-callback handle_merge(Remote::payload(), Local::payload()) -> payload().

%%====================================================================
%% Exported APIs
%%====================================================================

start_link(Name, Module, Interval, Args) ->
    gen_server:start_link(Name, ?MODULE, {Module, Interval, Args}, []).

query(ServerRef, Query) ->
    gen_server:call(ServerRef, {query, Query}).

update(ServerRef, Args) ->
    gen_server:cast(ServerRef, {update, Args}).

merge(ServerRef, Payload) ->
    gen_server:cast(ServerRef, {merge, Payload}).

join(ServerRef, AnotherServerRef) ->
    gen_server:cast(ServerRef, {join, AnotherServerRef}).

%%====================================================================
%% gen_server callback APIs
%%====================================================================

init({Module, Interval, Args}) ->
    Payload = Module:init(Args),
    State = #?STATE{module = Module, interval = Interval, payload = Payload, last_payload = Payload},
    erlang:send_after(Interval, self(), broadcast),
    {ok, State}.

handle_cast({update, Args}, State = #?STATE{module = Module, payload = Payload0}) ->
    % ok = io:format("update: args=~p~n", [Args]),
    Payload1 = Module:handle_update(Args, Payload0),
    {noreply, State#?STATE{payload = Payload1}};
handle_cast({merge, RemotePayload}, State = #?STATE{module = Module, payload = LocalPayload}) ->
    % ok = io:format("merge: payload=~p~n", [RemotePayload]),
    Payload = Module:handle_merge(RemotePayload, LocalPayload),
    {noreply, State#?STATE{payload = Payload}};
handle_cast({join, ServerRef}, State = #?STATE{neighbors = Neighbors0, payload = Payload}) ->
    % ok = io:format("join: ref=~p~n", [ServerRef]),
    Neighbors1 = gb_sets:add(ServerRef, Neighbors0),
    ok = ?MODULE:merge(ServerRef, Payload),
    {noreply, State#?STATE{neighbors = Neighbors1}};
handle_cast(_Req, _State) ->
    {noreply, _State}.

handle_call({query, Query}, _From, State = #?STATE{module = Module, payload = Payload}) ->
    % ok = io:format("query: query=~p~n", [Query]),
    Data = Module:handle_query(Query, Payload),
    {reply, Data, State};
handle_call(_Req, _From, _State) ->
    {noreply, _State}.

handle_info(broadcast, State = #?STATE{interval = Interval, payload = Payload, last_payload = Payload}) ->
    erlang:send_after(Interval, self(), broadcast),
    {noreply, State};
handle_info(broadcast, State = #?STATE{interval = Interval, payload = Payload, neighbors = Neighbors}) ->
    % ok = io:format("broadcast: payload=~p, neighbors=~p~n", [Payload, Neighbors]),
    ok = lists:foreach(fun (P) -> ?MODULE:merge(P, Payload) end, gb_sets:to_list(Neighbors)),
    erlang:send_after(Interval, self(), broadcast),
    {noreply, State#?STATE{last_payload = Payload}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) -> ok.

%%====================================================================
%% Internal functions
%%====================================================================
