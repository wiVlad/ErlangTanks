%%%-------------------------------------------------------------------
%%% @author vlad
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Each crate in the game is a gen_server
%%% @end
%%% Created : 01. Aug 2017 10:08 AM
%%%-------------------------------------------------------------------
-module(crate).
-author("vlad").

-behaviour(gen_server).

%% API
-export([start_link/0,start_link/5]).

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


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% Two different start_link, one for normal startup, and one for failover startup-
%% in which case the old crates need to be redrawn during initialization.
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link(?MODULE, [0], []).
%Receives old crate's data from game manager and re-creates it
start_link(PID,X,Y,Type,Quantity) ->
  gen_server:start_link(?MODULE, [PID,X,Y,Type,Quantity], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server, generates random crate and plugs it into database
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #crate_state{}} | {ok, State :: #crate_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([0]) ->
  random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
  TypesList =[health,ammo],
  Type = lists:nth(random:uniform(length(TypesList)),TypesList),
  case Type of
     health ->
        Quantity = random:uniform(20);
      ammo ->
        Quantity = random:uniform(10)
  end,
  erlang:send_after(?CRATE_INTERVAL, self(), trigger),
  X = random:uniform(?max_x - 45) , Y = random:uniform(?max_y - 45),
  F = fun() ->
    mnesia:write(#crate_state{pid = self(), x=X,y=Y,type=Type,quantity = Quantity})
      end,
  mnesia:transaction(F),
  {ok, #crate_state{pid = self(), x = X , y = Y, type = Type, quantity = Quantity}};
%If failover, recreate an old crate
init([_Pid,X,Y,Type,Quantity]) ->
  gen_server:cast(gui_server, {crate, X,Y, Type}),
  erlang:send_after(?CRATE_INTERVAL, self(), trigger),
  {ok, #crate_state{pid = self(), x = X , y = Y, type = Type, quantity = Quantity}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #crate_state{}) ->
  {reply, Reply :: term(), NewState :: #crate_state{}} |
  {reply, Reply :: term(), NewState :: #crate_state{}, timeout() | hibernate} |
  {noreply, NewState :: #crate_state{}} |
  {noreply, NewState :: #crate_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #crate_state{}} |
  {stop, Reason :: term(), NewState :: #crate_state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #crate_state{}) ->
  {noreply, NewState :: #crate_state{}} |
  {noreply, NewState :: #crate_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #crate_state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% Trigger - tells the crate to redraw itself and send it location to all players
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #crate_state{}) ->
  {noreply, NewState :: #crate_state{}} |
  {noreply, NewState :: #crate_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #crate_state{}}).
handle_info(trigger, State = #crate_state{x=X,y=Y,type=Type,quantity=Quantity}) ->
  gen_server:cast(gui_server, {crate, X,Y, Type}),
  SendTo = ets:tab2list(ids),
  lists:foreach(fun({_Ip,Pid}) -> gen_server:cast(Pid, {crate,X,Y, Type, Quantity, self()}) end, SendTo),
  erlang:send_after(?CRATE_INTERVAL, self(), trigger),
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
    State :: #crate_state{}) -> term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #crate_state{},
    Extra :: term()) ->
  {ok, NewState :: #crate_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
