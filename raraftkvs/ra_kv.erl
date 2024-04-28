%% This module is taken from ra's tutorial.
%%  (see https://github.com/rabbitmq/ra/blob/main/docs/internals/STATE_MACHINE_TUTORIAL.md)
-module(ra_kv).
-behaviour(ra_machine).
-export([apply/3, init/1, start/0]).

-opaque state() :: #{term() => term()}.

-type ra_kv_command() :: {write, Key :: term(), Value :: term()} |
                         {read, Key :: term()}.

init(_Config) -> #{}.

apply(_Meta, {write, Key, Value}, State) ->
    {maps:put(Key, Value, State), ok, _Effects = []};
apply(_Meta, {read, Key}, State) ->
    Reply = maps:get(Key, State, undefined),
    {State, Reply, _Effects = []}.

start() ->
    %% the initial cluster members
    Members = [{ra_kv1, node()}, {ra_kv2, node()}, {ra_kv3, node()}],
    %% an arbitrary cluster name
    ClusterName = <<"ra_kv">>,
    %% the config passed to `init/1`, must be a `map`
    Config = #{},
    %% the machine configuration
    Machine = {module, ?MODULE, Config},
    %% ensure ra is started
    application:ensure_all_started(ra),
    %% start a cluster instance running the `ra_kv` machine
    ra:start_cluster(default, ClusterName, Machine, Members).