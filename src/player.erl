
%%%-------------------------------------------------------------------
%%% @author jon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Represents each player/tank in the game.
%%% @end
%%% Created : 05. Jul 2017 20:14
%%%-------------------------------------------------------------------
-module(player).
-author("jon").

-behaviour(gen_server).

%% API
-export([start_link/5,start_link/12]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-include("ErlangTanks.hrl").
-include("data.hrl").
-record(state, {id,name, num, bodyIm,turretIm,xPos,yPos,score,bodyDir, turretDir, ammo, hitPoints}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term(),Args :: term(), Args :: term(), Args :: term(), Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Name,Num, ID,BodyIm,TurretIm) ->
  gen_server:start_link(?MODULE, [Name,Num,ID,BodyIm,TurretIm], []).
start_link(ID,Name, Num, BodyIm,TurretIm,XPos,YPos,Score,BodyDir, TurretDir, Ammo, HitPoints) ->
  gen_server:start_link(?MODULE, [ID,Name, Num, BodyIm,TurretIm,XPos,YPos,Score,BodyDir, TurretDir, Ammo, HitPoints], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% Creates a new tank for each new connection to the UDP server. The tank is placed
%% on random (x,y) coordinates to begin the game and given starting resource quantities.
%% If failover, recreates an old player
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Name,Num, ID,BodyIm,TurretIm]) ->
  io:format("New Player, ID: ~p ~n", [ID]),
  erlang:send_after(7000, self(), backup),
  erlang:send_after(?TANK_INTERVAL, self(), trigger),
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
  NewX = round(random:uniform()*(1080-90)) ,
  NewY = round(random:uniform()*(720-90)) ,
  gen_server:cast(gui_server, {grid, Name, Num, 50, 50}),
  gen_server:cast(gui_server, {grid, Num, 50, 50}),
  gen_server:cast(gui_server, {grid, Num, 0}),
  gen_server:cast(gui_server, {new,Name,BodyIm,TurretIm, NewX,NewY,0,0}),
  F = fun() ->
    mnesia:write(#player_state{id = ID,name = Name, num = Num, bodyIm = BodyIm, turretIm = TurretIm,
      xPos = NewX ,yPos = NewY,score = 0,bodyDir = 0, turretDir = 0, ammo = 50, hitPoints = 50})
      end,
  mnesia:transaction(F),
  {ok, #state{id = ID,name = Name, num = Num, bodyIm = BodyIm,turretIm = TurretIm,xPos = NewX ,yPos = NewY,score = 0,bodyDir = 0, turretDir = 0, ammo = 50, hitPoints = 50}};
init([ID,Name, Num, BodyIm,TurretIm,XPos,YPos,Score,BodyDir, TurretDir, Ammo, HitPoints]) ->
  io:format("Player recovered, ID: ~p ~n", [ID]),
  erlang:send_after(7000, self(), backup),
  erlang:send_after(?TANK_INTERVAL, self(), trigger),
  gen_server:cast(gui_server, {grid, Name, Num, Ammo, HitPoints}),
  gen_server:cast(gui_server, {grid, Num, Ammo, HitPoints}),
  gen_server:cast(gui_server, {grid, Num, Score}),
  gen_server:cast(gui_server, {new,Name,BodyIm,TurretIm, XPos,YPos,BodyDir,TurretDir}),
  {ok, #state{id = ID,name = Name, num = Num, bodyIm = BodyIm,turretIm = TurretIm,xPos = XPos ,yPos = YPos,score = Score,bodyDir = BodyDir,
    turretDir = TurretDir, ammo = Ammo, hitPoints = HitPoints}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(exit, _From, State = #state{ num = Num, xPos =  X, yPos= Y}) ->
  gen_server:cast(gui_server, {erase,{Num,X,Y}}),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

%Sent by shell, checks to see if there is a hit with player
handle_cast({hit,ShellX,ShellY, From, PlayerPid}, State = #state{ id = ID, num = Num, ammo = Ammo, xPos = X, yPos = Y, hitPoints = HP}) ->
  if
    ?inRange(X,ShellX,Y,ShellY) ->
      if
        HP < 5 ->  HPn = 0;
        true->     HPn = HP - 5
      end,
      gen_server:cast(PlayerPid, {score}),
      gen_server:cast(gui_server, {explosion, X, Y}),
      timer:apply_after(500,gen_server,cast,[gui_server,{background,X,Y,90+30,90+25}]),
      gen_server:cast(gui_server, {grid,Num, Ammo,HPn}),
      supervisor:terminate_child(shell_sup,From);

    true -> HPn = HP
  end,
  if
    (HPn < 1) ->
      gen_server:cast(PlayerPid, {score}),
      io:format("~nplayer ~p terminated~n",[ID]),
      gen_server:cast(gui_server, {erase,{Num,X,Y}}),
      gen_server:cast(udp_server, {exit,ID}),
      gen_server:cast(game_manager, {kill_player,ID});

    true -> ok
  end,
  {noreply, State#state{ hitPoints = HPn }};

%Sent by crate, checks to see if player "ate" crate
handle_cast({crate, Xcrate, Ycrate, Type, Quantity, From}, State = #state{ num = Num, xPos = X, yPos = Y, hitPoints = HP, ammo = Ammo}) ->
  if
    ?inRange(X,Xcrate,Y,Ycrate) ->
      if
        (Type == health) -> HPNew = HP + Quantity, AmmoNew = Ammo;
        (Type == ammo) -> HPNew = HP, AmmoNew = Ammo + Quantity;
        true -> HPNew = HP, AmmoNew = Ammo
      end,
      Fun = fun()->
        mnesia:delete({crate_state, From})
            end,
      mnesia:transaction(Fun),
      supervisor:terminate_child(crate_sup,From);
    true ->
      AmmoNew = Ammo,
      HPNew = HP
  end,
  gen_server:cast(gui_server, {grid,Num, AmmoNew,HPNew}),
  {noreply, State#state{ hitPoints = HPNew, ammo = AmmoNew }};

%Update the score on the GUI grid
handle_cast({score}, State = #state{ score = Score, num = Num}) ->
  gen_server:cast(gui_server, {grid,Num, Score+1}),
  {noreply, State#state{ score = Score + 1 }};

%Declare winner
handle_cast({winner}, State = #state{ name = Name }) ->
  gen_server:cast(gui_server, {winner,Name}),
  {noreply, State};

%"FIRE" was pressed by player, update grid and create a new shell
handle_cast({fire, ID}, State = #state{ id = ID, num = Num, xPos = X, yPos = Y, hitPoints = HP, turretDir = Dir, ammo = Ammo}) ->
  if
    (Ammo > 0) ->
      NewAmmo = Ammo - 1,
      gen_server:cast(gui_server, {grid,Num, NewAmmo,HP}),
      shell_sup:start_new_shell(X,Y,Dir,self());
    true -> NewAmmo = 0
  end,

  {noreply,State#state{ ammo = NewAmmo }};

%Player tank movement
handle_cast({moveBody,ID ,Xspeed, Yspeed,Angle}, State = #state{ id = ID,bodyDir = BodyAngle,turretDir = TurretAngle ,bodyIm = BodyIm,turretIm = TurretIm, xPos = Xcur, yPos = Ycur}) ->
  if
    (((Xspeed + Xcur) >= 0) and ((Xspeed + Xcur) =< 1080-90)) ->
      NewX = Xcur + Xspeed;
    true ->
      NewX = Xcur
  end,
  if
    (((Yspeed + Ycur) >= 0) and ((Yspeed + Ycur) =< 720-90)) ->
      NewY = Ycur + Yspeed;
    true ->
      NewY = Ycur
  end,
  if (Angle =:= 0) ->
    EffAngle =  BodyAngle;
    true -> EffAngle = Angle
  end,

  gen_server:cast(gui_server, {body,BodyIm,TurretIm, Xcur, Ycur, NewX,NewY,EffAngle, TurretAngle}),
  {noreply, State#state{ xPos = NewX, yPos = NewY, bodyDir = EffAngle}};

%Player turret movement
handle_cast({moveTurret,ID, Angle}, State = #state{ id= ID,bodyIm = BodyIm,turretIm = TurretIm, xPos = Xcur, yPos = Ycur, turretDir = TurretAngle, bodyDir = BodyAngle }) ->
  if (Angle =:= 0) ->
    EffAngle =  TurretAngle;
    true -> EffAngle = Angle
  end,
  gen_server:cast(gui_server, {turret,BodyIm,TurretIm, Xcur, Ycur, EffAngle, BodyAngle}),
  {noreply,State#state{turretDir = EffAngle}};

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% Backup signal to re-write data to the Mnesia database.
%% Trigger signal to re-draw the tank so that it wont be blackened
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(backup, State = #state{id = ID,name = Name, num = Num, bodyIm = BodyIm, turretIm = TurretIm,
    xPos = X ,yPos = Y,score = Score,bodyDir = BodyDir, turretDir = TurretDir, ammo = Ammo, hitPoints=Hp}) ->
  F = fun() ->
    mnesia:write(#player_state{id = ID,name = Name, num = Num, bodyIm = BodyIm, turretIm = TurretIm,
      xPos = X ,yPos = Y,score = Score,bodyDir = BodyDir, turretDir = TurretDir, ammo = Ammo, hitPoints=Hp})
      end,
  mnesia:transaction(F),
  erlang:send_after(10000, self(), backup),
  {noreply, State};
handle_info(trigger, State = #state{ bodyDir = BodyAngle,turretDir = TurretAngle ,bodyIm = BodyIm,turretIm = TurretIm, xPos = X, yPos = Y}) ->
  gen_server:cast(gui_server, {body,BodyIm,TurretIm, X, Y, X,Y,BodyAngle, TurretAngle}),
  erlang:send_after(?TANK_INTERVAL, self(), trigger),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%TODO: what happens when 0 HP ? perhaps image of busted tank

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================