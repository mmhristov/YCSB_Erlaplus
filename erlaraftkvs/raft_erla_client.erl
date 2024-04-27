-module(raft_erla_client).

-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  id = -1,
  raftNodes = [],
  clientPid = -1,
  requestCount = 0
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Id, Name, RaftNodes) ->
  gen_server:start_link({local, Name}, ?MODULE, [Id, RaftNodes], []).

init([Id, RaftNodes]) ->
  % register
  erla_libs_comm:register_proc(Id),
  global:sync(),
  % connect to raft nodes
  [io:format("Attempting to communicate with node ~s, response: ~s~n", [N, net_adm:ping(N)]) || N <- RaftNodes],
  % init state
  {ok, #state{id = Id, raftNodes = RaftNodes}}.

handle_call(_Request, _From, State = #state{}) ->
  erlang:display("CALL"),
  {reply, ok, State}.

handle_cast(_Request, State = #state{}) ->
  erlang:display("CAST"),
  {noreply, State}.

handle_info({get, Key, RaftNodeId, SenderPid}, State = #state{}) ->
  NewReqCount = State#state.requestCount + 1,
  M = #{
    key_mtype => "ClientGetRequest",
    key_mcmd => #{key_idx => 1, key_type => "Get", key_key => Key},
    key_msource =>State#state.id
  },
  erla_libs_comm:send(RaftNodeId, M),
  {noreply, State#state{clientPid = SenderPid, requestCount = NewReqCount}};

handle_info({put, {Key, Value}, RaftNodeId, SenderPid}, State = #state{}) ->
  NewReqCount = State#state.requestCount + 1,
  M = #{
    key_mtype => "ClientPutRequest",
    key_mcmd => #{key_idx => NewReqCount, key_type => "Put", key_key => Key,  key_value => Value},
    key_msource => State#state.id
  },
  erla_libs_comm:send(RaftNodeId, M),
  {noreply, State#state{clientPid = SenderPid, requestCount = NewReqCount}};

handle_info(M = #{key_mtype := "ClientPutResponse"}, State = #state{}) ->
  erlang:display("Received ClientPutResponse"),
  Leader =  maps:get(key_mleaderHint, M),
  Success = maps:get(key_msuccess, M),
  Resp = maps:get(key_mresponse, M),

  Key = maps:get(key_key, Resp),

  State#state.clientPid ! {Success, Key},
  {noreply, State};

handle_info(M = #{key_mtype := "ClientGetResponse"}, State = #state{}) ->
  erlang:display("Received ClientGetResponse"),
  Leader =  maps:get(key_mleaderHint, M),
  Success = maps:get(key_msuccess, M),
  Resp = maps:get(key_mresponse, M),

  IsOk = maps:get(key_ok, Resp),
  Val = maps:get(key_value, Resp),

  State#state.clientPid ! {Success, IsOk, Val},
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
