%%%-------------------------------------------------------------------
%%% @author jon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Aug 2017 16:30
%%%-------------------------------------------------------------------
-author("jon").

-record(crate_state, {pid, x,y,type,quantity}).
-record(player_state, {id,name, num, bodyIm,turretIm,xPos,yPos,score,bodyDir, turretDir, ammo, hitPoints}).
-record(game_state, {pid, gameInProgress, numOfPlayers}).
-record(gui_state, {panel,grid,time,timer}).
-record(udp_state, {socket,connections}).