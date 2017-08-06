%%%-------------------------------------------------------------------
%%% @author vlad
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2017 10:12 AM
%%%-------------------------------------------------------------------
-module(player_sup).
-author("vlad").

-behaviour(supervisor).

%% API
-export([
  start_link/0,start_link/1,
  start_player/3
  ]).

%% Supervisor callbacks
-export([init/1]).
-include("ErlangTanks.hrl").
-include("data.hrl").
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
start_link(PlayerList) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [PlayerList]).
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

    ets:insert(colors, [{"Graphics/redBody.png","Graphics/redTurret.png"},
      {"Graphics/blueBody.png","Graphics/blueTurret.png"},
      {"Graphics/cyanBody.png","Graphics/cyanTurret.png"},
      {"Graphics/greenBody.png","Graphics/greenTurret.png"},
      {"Graphics/purpleBody.png","Graphics/purpleTurret.png"},
      {"Graphics/greyBody.png","Graphics/greyTurret.png"},
      {"Graphics/yellowBody.png","Graphics/yellowTurret.png"}]),

    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

  AChild = {'?MODULE', {'player', start_link, []},
    Restart, Shutdown, Type, ['player']},
  io:format("Players Supervisor online ~n"),
  {ok, {SupFlags, [AChild]}};
init([PlayerList]) ->

  ets:insert(colors, [{"Graphics/redBody.png","Graphics/redTurret.png"},
    {"Graphics/blueBody.png","Graphics/blueTurret.png"},
    {"Graphics/cyanBody.png","Graphics/cyanTurret.png"},
    {"Graphics/greenBody.png","Graphics/greenTurret.png"},
    {"Graphics/purpleBody.png","Graphics/purpleTurret.png"},
    {"Graphics/greyBody.png","Graphics/greyTurret.png"},
    {"Graphics/yellowBody.png","Graphics/yellowTurret.png"}]),

  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {'?MODULE', {'player', start_link, []},
    Restart, Shutdown, Type, ['player']},
  io:format("Players Supervisor recovered ~n"),
  lists:foreach(fun(E)->
    {player_state,ID,Name, Num, BodyIm,TurretIm,XPos,YPos,Score,BodyDir, TurretDir, Ammo, HitPoints}=E,
    supervisor:start_child(?MODULE, [ID,Name, Num, BodyIm,TurretIm,XPos,YPos,Score,BodyDir, TurretDir, Ammo, HitPoints])
                end,PlayerList),
  {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===========================  ========================================

start_player(Name, ID, Num) ->
  BodyIm = ets:first(colors),
  {_Key,TurretIm} = hd(ets:lookup(colors,BodyIm)),
  ets:delete(colors,BodyIm),
  Pid = supervisor:start_child(?MODULE, [Name,Num,ID,BodyIm,TurretIm]),
  io:format("~n~n~p~n~n",[Pid]),
  Pid.
