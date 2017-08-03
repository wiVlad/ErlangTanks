%%%-------------------------------------------------------------------
%%% @author jon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jun 2017 17:50
%%%-------------------------------------------------------------------
-module(game_manager).
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
-include("include/ErlangTanks.hrl").
-define(SERVER, game_manager).
-record(state, { numOfPlayers = 0, gameInProgress = false }).

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
  %gen_server is registered under the name "main_server"

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
init([]) ->
  ets:new(ids, [set, named_table, public]),
  io:format("Game Manager Online ~n"),
  {ok, #state{gameInProgress = false, numOfPlayers =  0}}.

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
handle_call({_Sock,Ip,Request}, _From, State = #state{gameInProgress = true, numOfPlayers =  Num}) ->

  Msg = re:split(Request, "[ ]",[{return,list}]),

  case Msg of
    [_PlayerName, "connection","successful"] ->
      NewNum = Num,
      NewStatus = true;
    ["FIRE"] ->
      io:format("Num of players ~p ~n",[Num]),
      NewNum = Num,
      NewStatus = true,
      [{Ip, Pid}] = ets:lookup(ids, Ip),
      gen_server:call(Pid, {fire, Ip});
    ["exit"] ->
      NewStatus = true,
      [{Ip, Pid}] = ets:lookup(ids, Ip),
      NewNum = Num -1,
      supervisor:terminate_child(player_sup,Pid);
    ["Turret", Angle] ->
      NewNum = Num,
      NewStatus = true,
      [{Ip, Pid}] = ets:lookup(ids, Ip),
      gen_server:call(Pid, {moveTurret, Ip, list_to_integer(Angle)});
    ["Body", X, Y, Angle] ->
      NewNum = Num,
      [{Ip, Pid}] = ets:lookup(ids, Ip),
      gen_server:call(Pid, {moveBody, Ip ,list_to_integer(X), list_to_integer(Y),list_to_integer(Angle)}),
      if
        (NewNum == 1) ->  NewStatus = false,
          gen_server:cast(Pid, {winner});
        true -> NewStatus = true
      end

  end,
  {reply, ok, State#state{gameInProgress = NewStatus,numOfPlayers =  NewNum}};

handle_call({Sock,Ip,Request}, _From, State = #state{gameInProgress = false, numOfPlayers =  Num}) ->
  Msg = re:split(Request, "[ ]",[{return,list}]),

  case Msg of
    [PlayerName, "connection","successful"] ->
      {ok, PID} = player_sup:start_player(PlayerName, Ip, Num),
      gen_udp:send(Sock, Ip, 4000, list_to_binary("connected")),
      NewNum = Num + 1,
      ets:insert(ids, {Ip, PID});
    _Any -> NewNum = Num
  end,
  {reply, ok, State#state{numOfPlayers = NewNum}}.

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

handle_cast(_Request, State = #state{gameInProgress = false}) ->
  io:format("Game manager ack start ~n"),
  erlang:send_after(5000+?CRATE_MIN_INTERVAL+random:uniform(?CRATE_RANGE_INTERVAL), self(), crateTrigger),
  {noreply, State#state{gameInProgress = true}};

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

handle_info(crateTrigger, State) ->
  crate_sup:start_new_crate(),
  erlang:send_after(?CRATE_MIN_INTERVAL+random:uniform(?CRATE_RANGE_INTERVAL), self(), crateTrigger),
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
  ets:delete(ids),
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

