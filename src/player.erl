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
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(max_x,(1024)).
-define(max_y,(768)).

-record(state, {id,xPos,yPos,bodyDir = 0, turretDir = 0, ammo = 50, hitPoints = 10}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([ID]) ->
  io:format("New Player, ID: ~p ~n", [ID]),
  NewX = random:uniform()*500,
  NewY = random:uniform()*500,
  gen_server:call(gui_server, {ID, NewX,NewY,0}),
  {ok, #state{id = ID ,xPos = NewX ,yPos = NewY ,bodyDir = 0, turretDir = 0, ammo = 50, hitPoints = 10}}.

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

handle_call({fire, ID}, _From, State = #state{ id = ID, xPos = X, yPos = Y, turretDir = Dir, ammo = Ammo}) ->
  NewAmmo = Ammo - 1,
  shell_sup:start_new_shell(X,Y,Dir),
  {reply, ok, State = #state{ ammo = NewAmmo }};

handle_call({moveBody, ID ,Xspeed, Yspeed,Angle}, _From, State = #state{ id = ID, xPos = Xcur, yPos = Ycur}) ->
  if
    (((Xspeed + Xcur) < 0) or ((Xspeed + Xcur) > max_x)) ->
      NewX = Xcur;
    true ->
      NewX = Xcur + Xspeed
  end,
  if
    (((Yspeed + Ycur) < 0) or ((Yspeed + Ycur) > max_x)) ->
      NewY = Ycur;
    true ->
      NewY = Ycur + Yspeed
  end,
  gen_server:call(gui_server, {ID, NewX,NewY,Angle}),
  {reply, ok, State = #state{ xPos = NewX, yPos = NewY, bodyDir = Angle}};

handle_call({moveTurret, Angle}, _From, State = #state{ id= ID }) ->
  gen_server:call(gui_server, {ID, Angle}),
  {reply, ok, State = #state{turretDir = Angle}};

handle_call({hit}, _From, State = #state{ xPos = X, yPos = Y, hitPoints = HP}) ->
  HPn = HP - 10,
  gen_server:call(gui_server, {explode, X, Y}),
  {reply, ok, State = #state{ hitPoints = HPn }}.


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
