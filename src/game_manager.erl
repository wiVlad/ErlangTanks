%%%-------------------------------------------------------------------
%%% @author jon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% The game manager, handles communication between players,crates,the gui, and the udp server.
%%% @end
%%% Created : 24. Jun 2017 17:50
%%%-------------------------------------------------------------------
-module(game_manager).
-author("jon").

-behaviour(gen_server).

%% API
-export([start_link/0,start_link/4]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).
-include("ErlangTanks.hrl").
-include("data.hrl").
-define(SERVER, game_manager).
-record(state, { numOfPlayers = 0, gameInProgress = false }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server,
%% The second start_link is in case of failover, where the old data is reconfigured into the game
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
  %gen_server is registered under the name "game_manager"
start_link(GameState,GuiState,PlayerList,CrateList) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [GameState,GuiState,PlayerList,CrateList], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server, writes it into database
%% In case of failover, reconfigures the game from old state
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  erlang:send_after(2000, self(), backup), %send periodic signal to itself to update the database
  process_flag(trap_exit, true),
  io:format("Game Manager Online ~n"),
  F = fun() ->
    mnesia:write(#game_state{pid = self(), gameInProgress = false, numOfPlayers =  0}) end,
  mnesia:transaction(F),
  {ok, #state{gameInProgress = false, numOfPlayers =  0}};  %The game starts with gameInProgress = false
%Recreate the game on the new backup node from the spot where the old one crashed
init([GameState,GuiState,PlayerList,CrateList]) ->
  erlang:send_after(2000, self(), backup),
  process_flag(trap_exit, true),
  io:format("Game Manager recovered ~n"),
  {game_state,_Pid, GameStatus, NumOfPlayers} = hd(GameState),
  [{gui_state,_Panel, _Grid,_TimeView,Timer}] = GuiState,
  lists:foreach(fun(E)->
    {player_state,ID,Name, Num, BodyIm,TurretIm,XPos,YPos,Score,BodyDir, TurretDir, Ammo, HitPoints}=E,
    {ok, PlayerPid} = supervisor:start_child(player_sup, [ID,Name, Num, BodyIm,TurretIm,XPos,YPos,Score,BodyDir, TurretDir, Ammo, HitPoints]),
    ets:insert(ids,{ID,PlayerPid}),
    gen_server:cast(udp_server, {new_ip, ID})
                end,PlayerList),
  lists:foreach(fun(E)->
    {crate_state,PID,X,Y,Type,Quantity}=E,
    supervisor:start_child(crate_sup, [PID,X,Y,Type,Quantity])
                end,CrateList),
  if
    (GameStatus == true) ->
      gui_server ! {newTime,Timer},
      gen_server:cast(game_manager,startGame);
    true -> ok
  end,
  {ok, #state{ gameInProgress = GameStatus, numOfPlayers =  NumOfPlayers}}.

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

%Kill a player when his Health Points reach 0
handle_cast({kill_player,Ip}, State = #state{numOfPlayers = Num} ) ->
  case ets:lookup(ids,Ip) of
  [{Ip, Pid}] ->
      ets:delete(ids,Ip),
      gen_server:call(Pid, exit),  %to erase tank from gui
      F = fun() ->
        mnesia:delete({player_state, Ip}) end,
      mnesia:transaction(F), %remove from database
      NewNum = Num -1,
      supervisor:terminate_child(player_sup,Pid);
    [] -> NewNum = Num
  end,
  {noreply, State#state{numOfPlayers = NewNum}};

%Kill a shell process once it hits a tank or leaves boundaries of frame
handle_cast({kill_shell,Pid}, State ) ->
  supervisor:terminate_child(shell_sup,Pid),
  {noreply, State};

%Start the game once "START" button has been pressed in GUI,
%Backup the game_manager every 5 seconds
handle_cast(startGame, State = #state{gameInProgress = _Anything}) ->
  io:format("Game manager ack start ~n"),
  erlang:send_after(5000+?CRATE_MIN_INTERVAL+random:uniform(?CRATE_RANGE_INTERVAL), self(), crateTrigger),
  {noreply, State#state{gameInProgress = true}};

%Game has ended if timer reaches 0
handle_cast(endGame, State = #state{gameInProgress = true}) ->
  io:format("Game ended ~n"),
  {noreply, State#state{gameInProgress = false}};

%Receive message from UDP and pass it on to the player process
handle_cast({Sock,Ip,Request}, State = #state{gameInProgress = false, numOfPlayers =  Num}) ->
  Msg = re:split(Request, "[ ]",[{return,list}]),
  case Msg of
    [PlayerName, "connection","successful"] ->
      {ok, PID} = player_sup:start_player(PlayerName, Ip, Num),
      gen_udp:send(Sock, Ip, 4000, list_to_binary("connected")),
      NewNum = Num + 1,
      ets:insert(ids, {Ip, PID});
    _Any -> NewNum = Num
  end,
  {noreply, State#state{numOfPlayers = NewNum}};

handle_cast({_Sock,Ip,Request}, State = #state{gameInProgress = true, numOfPlayers =  Num}) ->
  Msg = re:split(Request, "[ ]",[{return,list}]),
  case ets:lookup(ids,Ip) of
    [{Ip,Pid}] ->
  case Msg of
    [_PlayerName, "connection","successful"] ->
      NewNum = Num,
      NewStatus = true;
    ["FIRE"] ->
      NewNum = Num,
      NewStatus = true,
      gen_server:cast(Pid, {fire, Ip});
    ["exit"] ->
      NewStatus = true,
      ets:delete(ids,Ip),
      gen_server:call(Pid, exit),  %to erase tank from gui
      F = fun() ->
        mnesia:delete({player_state, Ip}) end,
      mnesia:transaction(F), %remove from database
      NewNum = Num -1,
      supervisor:terminate_child(player_sup,Pid);
    ["Turret", Angle] ->
      NewNum = Num,
      NewStatus = true,
      gen_server:cast(Pid, {moveTurret, Ip, list_to_integer(Angle)});
    ["Body", X, Y, Angle] ->
      NewNum = Num,
      gen_server:cast(Pid, {moveBody, Ip ,list_to_integer(X), list_to_integer(Y),list_to_integer(Angle)}),
      if  %If only 1 player remains, the winner is declared.
        (NewNum == -1) ->  NewStatus = false,  % -1 for debugging purposes
          gen_server:cast(Pid, {winner});
        true -> NewStatus = true
      end

  end;
    _Anything -> NewNum = Num, NewStatus = true
  end,
  {noreply, State#state{gameInProgress = NewStatus,numOfPlayers =  NewNum}};

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

%Start a new crate and start random time interval to create a new one
handle_info(crateTrigger, State = #state{gameInProgress = true}) ->
  crate_sup:start_new_crate(),
  erlang:send_after(?CRATE_MIN_INTERVAL+random:uniform(?CRATE_RANGE_INTERVAL), self(), crateTrigger),
  {noreply, State};

%Backup the game manager every 10 seconds
handle_info(backup, State = #state{gameInProgress = Status,numOfPlayers = Num}) ->
  F = fun() ->
    mnesia:write(#game_state{pid = self(), gameInProgress = Status,numOfPlayers = Num}) end,
  mnesia:transaction(F),
  erlang:send_after(10000, self(), backup),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be th e opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  io:format("Game server terminated~n"),
  ok.
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

