
%%%-------------------------------------------------------------------
%%% @author jon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2017 20:14
%%%-------------------------------------------------------------------
-module(player).
-author("jon").

-behaviour(gen_server).

%% API
-export([start_link/5]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-include("include/ErlangTanks.hrl").
-record(state, {id,name, num, bodyIm,turretIm,xPos,yPos,score = 0,bodyDir = 0, turretDir = 0, ammo = 50, hitPoints = 50}).

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
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
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
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
  NewX = round(random:uniform()*(1080-90)) ,
  NewY = round(random:uniform()*(720-90)) ,
  gen_server:call(gui_server, {grid, Name, Num, 50, 50}),
  gen_server:call(gui_server, {grid, Num, 50, 50}),
  gen_server:call(gui_server, {grid, Num, 0}),
  gen_server:call(gui_server, {new,Name,BodyIm,TurretIm, NewX,NewY,0}),
  {ok, #state{id = ID,name = Name, num = Num, bodyIm = BodyIm,turretIm = TurretIm,xPos = NewX ,yPos = NewY}}.

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

handle_call({fire, ID}, _From, State = #state{ id = ID, num = Num, xPos = X, yPos = Y, hitPoints = HP, turretDir = Dir, ammo = Ammo}) ->
  if
    (Ammo > 0) ->
      NewAmmo = Ammo - 1,
      gen_server:call(gui_server, {grid,Num, NewAmmo,HP}),
      shell_sup:start_new_shell(X,Y,Dir,self());
    true -> NewAmmo = 0
  end,

  {reply, ok, State#state{ ammo = NewAmmo }};

handle_call({moveBody,ID ,Xspeed, Yspeed,Angle}, _From, State = #state{ id = ID,bodyDir = BodyAngle,turretDir = TurretAngle ,bodyIm = BodyIm,turretIm = TurretIm, xPos = Xcur, yPos = Ycur}) ->
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

  gen_server:call(gui_server, {body,BodyIm,TurretIm, Xcur, Ycur, NewX,NewY,EffAngle, TurretAngle}),
  {reply, ok, State#state{ xPos = NewX, yPos = NewY, bodyDir = EffAngle}};

handle_call({moveTurret,ID, Angle}, _From, State = #state{ id= ID,bodyIm = BodyIm,turretIm = TurretIm, xPos = Xcur, yPos = Ycur, turretDir = TurretAngle, bodyDir = BodyAngle }) ->
  if (Angle =:= 0) ->
    EffAngle =  TurretAngle;
    true -> EffAngle = Angle
  end,
  gen_server:call(gui_server, {turret,BodyIm,TurretIm, Xcur, Ycur, EffAngle, BodyAngle}),
  {reply, ok, State#state{turretDir = EffAngle}}.



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

handle_cast({hit,ShellX,ShellY, From, PlayerPid}, State = #state{ id = ID, score = Score, num = Num, ammo = Ammo, xPos = X, yPos = Y, hitPoints = HP}) ->
  if
    ?inRange(X,ShellX,Y,ShellY) ->
      HPn = HP - 5,
      gen_server:call(gui_server, {explosion, X, Y}),
      timer:apply_after(500,gen_server,call,[gui_server,{background,X,Y,90+30,90+25}]),
      gen_server:call(gui_server, {grid,Num, Ammo,HPn}),
      supervisor:terminate_child(shell_sup,From);

    true -> HPn = HP
  end,
  if
    (HPn < 1) ->  gen_server:cast(PlayerPid, {score}),
      io:format("~nplayer ~p terminated~n",[ID]),
      gen_server:call(udp_server, {exit,ID}),
      supervisor:terminate_child(player_sup,self());
    true -> ok
  end,
  {noreply, State#state{ hitPoints = HPn }};

handle_cast({crate, Xcrate, Ycrate, Type, Quantity, From}, State = #state{ num = Num, xPos = X, yPos = Y, hitPoints = HP, ammo = Ammo}) ->
  if
    ?inRange(X,Xcrate,Y,Ycrate) ->
      if
        (Type == health) -> HPNew = HP + Quantity, AmmoNew = Ammo;
        (Type == ammo) -> HPNew = HP, AmmoNew = Ammo + Quantity;
        true -> HPNew = HP, AmmoNew = Ammo
      end,
      supervisor:terminate_child(crate_sup,From);
    true ->
      AmmoNew = Ammo,
      HPNew = HP
  end,
  gen_server:call(gui_server, {grid,Num, AmmoNew,HPNew}),
  {noreply, State#state{ hitPoints = HPNew, ammo = AmmoNew }};

handle_cast({score}, State = #state{ score = Score, num = Num, xPos = X, yPos = Y, hitPoints = HP, ammo = Ammo}) ->

  gen_server:call(gui_server, {grid,Num, Score+1}),
  {noreply, State#state{ score = Score + 1 }};

handle_cast({winner}, State = #state{ name = Name }) ->
  gen_server:cast(gui_server, {winner,Name}),
  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(trigger, State = #state{ bodyDir = BodyAngle,turretDir = TurretAngle ,bodyIm = BodyIm,turretIm = TurretIm, xPos = X, yPos = Y}) ->
  gen_server:call(gui_server, {body,BodyIm,TurretIm, X, Y, X,Y,BodyAngle, TurretAngle}),
  erlang:send_after(?CRATE_INTERVAL, self(), trigger),
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