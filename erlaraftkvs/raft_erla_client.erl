-module(raft_erla_client).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(raft_erla_client_state, {
  id = -1,
  raftNodes = [],
  clientPid = -1
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Id, RaftNodes) ->
  gen_server:start_link({local, 'raftkvs_client'}, ?MODULE, [Id, RaftNodes], []).

init([Id, RaftNodes]) ->
  % register
  erla_libs_comm:register_proc(Id),
  global:sync(),
  % connect to raft nodes
  [io:format("Attempting to communicate with node ~s, response: ~s~n", [N, net_adm:ping(N)]) || N <- RaftNodes],
  % init state
  {ok, #raft_erla_client_state{id = Id, raftNodes = RaftNodes}}.

handle_call(_Request, _From, State = #raft_erla_client_state{}) ->
  erlang:display("CALL"),
  {reply, ok, State}.

handle_cast(_Request, State = #raft_erla_client_state{}) ->
  erlang:display("CAST"),
  {noreply, State}.

handle_info({get, M}, State = #raft_erla_client_state{}) ->
  {noreply, State};

handle_info({put, {Key, Value}, RaftNodeId, SenderPid}, State = #raft_erla_client_state{}) ->
 M = #{key_mtype => "ClientPutRequest",
    key_mcmd => #{
      key_idx => 1, key_type => "Put", key_key => Key,  key_value => Value
    },
    key_msource => State#raft_erla_client_state.id
  },
  erla_libs_comm:send(RaftNodeId, M),
  {noreply, State#raft_erla_client_state{clientPid = SenderPid}};

handle_info(M = #{key_mtype := "ClientPutResponse"}, State = #raft_erla_client_state{}) ->
  erlang:display("Received ClientPutResponse"),
  % #{key_mtype => "ClientPutResponse", key_msuccess => false,
  %   key_mresponse => #{key_idx => ID, key_key => k)}, key_msource => State2#state_raftNode.procvar_ID, key_mleaderHint => L, key_mdest => Client}
  Leader =  maps:get(key_mleaderHint, M),
  Success = maps:get(key_msuccess, M),
  Resp = maps:get(key_mresponse, M),

  Key = maps:get(key_key, Resp),

  State#raft_erla_client_state.clientPid ! {Success, Key},
  {noreply, State};

handle_info({redirect, M, RaftNodeId, SenderPid}, State = #raft_erla_client_state{}) ->
  erla_libs_comm:send(RaftNodeId, M),
  {noreply, State#raft_erla_client_state{clientPid = SenderPid}};

handle_info(M = #{key_mtype := "ClientGetResponse"}, State = #raft_erla_client_state{}) ->
  erlang:display("Received ClientGetResponse"),
  Leader =  maps:get(key_mleaderHint, M),
  Success = maps:get(key_msuccess, M),
  Resp = maps:get(key_mresponse, M),

  IsOk = maps:get(key_ok, Resp),
  Val = maps:get(key_value, Resp),

  State#raft_erla_client_state.clientPid ! {Success, IsOk, Val},
  {noreply, State};

handle_info(M, State = #raft_erla_client_state{}) ->
  erlang:display(M),
  {noreply, State}.

terminate(_Reason, _State = #raft_erla_client_state{}) ->
  ok.

code_change(_OldVsn, State = #raft_erla_client_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
