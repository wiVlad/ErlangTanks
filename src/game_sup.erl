  %%%-------------------------------------------------------------------
%%% @author vlad
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2017 10:12 AM
%%%-------------------------------------------------------------------
-module(game_sup).
-author("vlad").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
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
-spec(start_link(term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(normal) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []);
start_link([GameState,GuiState,CrateList,PlayerList]) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [GameState,GuiState,CrateList,PlayerList]).

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
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Children = [
    ?CHILD(game_manager, supervisor),
    ?CHILD(udp_server, worker),
    ?CHILD(gui_server, worker),
    ?CHILD(shell_sup, supervisor),
    ?CHILD(player_sup, supervisor),
    ?CHILD(crate_sup, supervisor)
    ],
  {ok, {SupFlags, Children}};
init([GameState,GuiState,CrateList,PlayerList]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Children = [
    ?CHILD(udp_server, worker),
    ?CHILD(gui_server, worker),
    ?CHILD(shell_sup, supervisor),
    ?CHILD(player_sup, supervisor),
    ?CHILD(crate_sup, supervisor),
    {game_manager, {game_manager, start_link, [GameState,GuiState,PlayerList,CrateList]}, permanent, 5000, supervisor, [game_manager]}
  ],
  {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
