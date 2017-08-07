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
  stop/1,start_phase/3,delete_mnes/0,traverse_table_and_show/1]).

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

start(normal, _StartArgs) ->
  io:format("normal start~n"),
  mnesia:delete_table(player_state),
  mnesia:delete_table(crate_state),
  mnesia:delete_table(game_state),
  mnesia:delete_table(gui_state),
  mnesia:delete_table(udp_state),
  spawn(mnesia,stop,[]),
  rpc:call(?BackupNode, application, stop, [mnesia]),
  NodeList = [?MainNode,?BackupNode],
  mnesia:delete_schema(NodeList),
  mnesia:create_schema(NodeList),
  mnesia:start(),
  rpc:call(?BackupNode, application, start, [mnesia]),
  mnesia:create_table(player_state,
    [{attributes, record_info(fields, player_state)},{disc_copies, NodeList}]),
  mnesia:create_table(crate_state,
    [{attributes, record_info(fields, crate_state)},{disc_copies, NodeList}]),
  mnesia:create_table(gui_state,
    [{attributes, record_info(fields, gui_state)},{disc_copies, NodeList}]),
  mnesia:create_table(game_state,
    [{attributes, record_info(fields, game_state)},{disc_copies, NodeList}]),
  mnesia:create_table(udp_state,
    [{attributes, record_info(fields, udp_state)},{disc_copies, NodeList}]),
  %mnesia:wait_for_tables([game_manager_tab], 5000),
  ets:new(ids, [set, named_table, public]),
  ets:new(colors, [set, named_table,public]),
  case game_sup:start_link(normal) of
    {ok, Pid} ->
      {ok, Pid};
    Error ->
      Error
  end;
start({failover,_Node}, _StartArgs) ->
  io:format("not normal start: ~p~n", [failover]),
  ets:new(ids, [set, named_table, public]),
  ets:new(colors, [set, named_table,public]),
  [{udp_state, Sock, _Connections}] = ets:tab2list(udp_state),
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
start({takeover,_Node}, _StartArgs) ->
  io:format("not normal start: ~p~n", [takeover]),
  ets:new(ids, [set, named_table, public]),
  ets:new(colors, [set, named_table,public]),
  mnesia:start(),
  rpc:call(?BackupNode, ets, tab2list, [udp_state]),
  [{udp_state,Sock, _Connections}] = rpc:call(?BackupNode, ets, tab2list, [udp_state]),
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
traverse_table_and_show(Table_name)->
  Iterator =  fun(Rec,_)->
    io:format("~p~n",[Rec]),
    []
              end,
  case mnesia:is_transaction() of
    true -> mnesia:foldl(Iterator,[],Table_name);
    false ->
      Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
      mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
  end.


start_phase(_Phase,_StartType,_PhaseArgs)->
  ok.
delete_mnes() ->
  mnesia:delete_table(player_state),
  mnesia:delete_table(crate_state),
  mnesia:delete_table(game_state),
  mnesia:delete_table(gui_state),
  mnesia:delete_table(udp_state),
  case node() of
    ?MainNode -> spawn(?MainNode,mnesia,stop,[]),
      rpc:call(?BackupNode, application, stop, [mnesia]);
    ?BackupNode ->
      spawn(?BackupNode,mnesia,stop,[])
  end,
  mnesia:delete_schema([?MainNode,?BackupNode]),
  ok.