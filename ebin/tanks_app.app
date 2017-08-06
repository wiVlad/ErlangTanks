%%%-------------------------------------------------------------------
%%% @author jon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Aug 2017 00:47
%%%-------------------------------------------------------------------

{application, tanks_app, [
  {description, "Erlang Tanks application project"},
  {vsn, "1"},
  {modules, [game_sup,game_manager,crate,crate_sup,player,player_sup,gui_server,udp_server,shell_server,shell_sup]},
  {registered, [game_sup,game_manager,crate,crate_sup,player,player_sup,gui_server,udp_server,shell_server,shell_sup]},
  {applications, [
    kernel,
    stdlib
  ]},
  {start_phases, []},
  {mod, {tanks_app, []}},
  {env, []}
]}.