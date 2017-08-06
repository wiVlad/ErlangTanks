%%%-------------------------------------------------------------------
%%% @author vlad
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2017 10:12 AM
%%%-------------------------------------------------------------------
-module(crate_sup).
-author("vlad").

-behaviour(supervisor).

%% API
-export([
  start_link/0,
  start_new_crate/0,
  start_link/1
]).

%% Supervisor callbacks
-export([init/1]).
-include("Include/ErlangTanks.hrl").
-include("Include/data.hrl").
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).
start_link(CrateList) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [CrateList]).
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).

init([]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {'?MODULE', {'crate', start_link, []},
    Restart, Shutdown, Type, ['crate']},
  io:format("Crate Supervisor online ~n"),
  {ok, {SupFlags, [AChild]}};

init([CrateList]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {'?MODULE', {'crate', start_link, []},
    Restart, Shutdown, Type, ['crate']},
  io:format("Crate Supervisor recovered ~n"),
  lists:foreach(fun(E)->
    {crate_state,PID,X,Y,Type,Quantity}=E,
    supervisor:start_child(?MODULE, [PID,X,Y,Type,Quantity])
  end,CrateList),
  {ok, {SupFlags, [AChild]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
start_new_crate() ->
  supervisor:start_child(?MODULE, []).
