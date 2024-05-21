-module(raft_ra_client).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, [], []).

init([]) -> {ok, #state{}}.

handle_call(_Request, _From, State = #state{}) ->
  erlang:display("CALL"),
  {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
  erlang:display("CAST"),
  {noreply, State}.

handle_info({get, Key, _RaftNodeId, SenderPid}, State = #state{}) ->
  {ok, Value, {_Leader, _}} = ra:consistent_query(ra_kv1, fun(Store) -> maps:get(Key, Store, undefined) end),
  erlang:display("Received ClientGetResponse"),  
  IsFound = Value =/= default,
  SenderPid ! {true, IsFound, Value},
  {noreply, State};

handle_info({put, {Key, Value}, _RaftNodeId, SenderPid}, State = #state{}) ->
  {Success, _, {_NodeId, _LeaderNodeName}} = ra:process_command(ra_kv1, {write, Key, Value}),
  erlang:display("Received ClientPutResponse"),
  SenderPid ! {Success =:= ok, Key},
  {noreply, State};

handle_info(M, State = #state{}) ->
  erlang:display(M),
  {noreply, State}.

terminate(_Reason, _State = #state{}) ->
  ok.

code_change(_OldVsn, State = #state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
