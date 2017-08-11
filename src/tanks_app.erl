%%%-------------------------------------------------------------------
%%% @author vlad
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Jul 2017 10:41 AM
%%%-------------------------------------------------------------------
-module(tanks_app).
-author("vlad").

-behaviour(application).

-include("ErlangTanks.hrl").
-include("data.hrl").
%% Application callbacks
-export([start/2,
  stop/1,start_phase/3,delete_mnes/0]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).

%Normal start to the application. First make sure no old mnesia data exists.
%Then create new mnesia database on all nodes. Then start the game supervisor.
start(normal, _StartArgs) ->
  io:format("normal start~n"),
  %First clear any existing mnesia data on the participating nodes
  mnesia:delete_table(player_state),
  mnesia:delete_table(crate_state),
  mnesia:delete_table(game_state),
  mnesia:delete_table(gui_state),
  mnesia:delete_table(udp_state),
  rpc:multicall(?NodeList, application, stop, [mnesia]),
  mnesia:delete_schema(?NodeList),
  %Now restart the mnesia database and create the tables on all nodes
  mnesia:create_schema(?NodeList),
  rpc:multicall(?NodeList, application, start, [mnesia]),
  mnesia:create_table(player_state,
    [{attributes, record_info(fields, player_state)},{disc_copies, ?NodeList}]),
  mnesia:create_table(crate_state,
    [{attributes, record_info(fields, crate_state)},{disc_copies, ?NodeList}]),
  mnesia:create_table(gui_state,
    [{attributes, record_info(fields, gui_state)},{disc_copies, ?NodeList}]),
  mnesia:create_table(game_state,
    [{attributes, record_info(fields, game_state)},{disc_copies, ?NodeList}]),
  mnesia:create_table(udp_state,
    [{attributes, record_info(fields, udp_state)},{disc_copies, ?NodeList}]),
  %Two additions ets that are final
  ets:new(ids, [set, named_table, public]),
  ets:new(colors, [set, named_table,public]),
  case game_sup:start_link(normal) of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end;

%When main node fails, backup node takes over.
%The new node will start the application with this function.
start({failover,_Node}, _StartArgs) ->
  io:format("not normal start: ~p~n", [failover]),
  ets:new(ids, [set, named_table, public]),
  ets:new(colors, [set, named_table,public]),
  io:format("~p~n",[[{udp_state, Sock, _Connections}] = ets:tab2list(udp_state)]),
  mnesia:transaction(fun() -> mnesia:delete({udp_state, Sock}) end),
  gen_udp:close(Sock),

  GameState = ets:tab2list(game_state),
  GuiState = ets:tab2list(gui_state),
  CrateList = ets:tab2list(crate_state),
  PlayerList = ets:tab2list(player_state),

  case game_sup:start_link([GameState,GuiState,CrateList,PlayerList]) of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end;
start({takeover,Node}, _StartArgs) ->
  io:format("not normal start: ~p~n", [takeover]),
  ets:new(ids, [set, named_table, public]),
  ets:new(colors, [set, named_table,public]),
  rpc:multicall(?NodeList, ets, tab2list, [udp_state]),
  [{udp_state,Sock, _Connections}] = rpc:call(Node, ets, tab2list, [udp_state]),
  gen_udp:close(Sock),
  case game_sup:start_link(normal) of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).

stop(_State) ->
  io:format("~nstopping~n"),
  ets:delete(colors),
  ets:delete(ids),
  delete_mnes(),
ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================

start_phase(_Phase,_StartType,_PhaseArgs)->
  ok.

%Delete all mnesia tables and data on terminate
delete_mnes() ->
  mnesia:delete_table(player_state),
  mnesia:delete_table(crate_state),
  mnesia:delete_table(game_state),
  mnesia:delete_table(gui_state),
  mnesia:delete_table(udp_state),
  case node() of
    ?MainNode ->
      rpc:multicall(?NodeList, application, stop, [mnesia]);
    _BackupNode ->
      rpc:multicall(?BackupNodes,mnesia,stop,[])
  end,
  mnesia:delete_schema(?NodeList),
  ok.