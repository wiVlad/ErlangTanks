%%%-------------------------------------------------------------------
%%% @author jon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% The shells being fired from the tanks
%%% @end
%%% Created : 05. Jul 2017 21:14
%%%-------------------------------------------------------------------
-module(shell_server).
-author("jon").

-behaviour(gen_server).

%% API
-export([start_link/4]).

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
-record(state, {x,y,xinc,yinc,dir,playerPid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term(), Args :: term(), Args :: term(), Args :: term()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(X,Y,Dir,PlayerPid) ->
  gen_server:start_link(?MODULE, [X,Y,Dir,PlayerPid], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server, sets the shells direction in correlation with the
%% tank's turret that fired it.
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([X,Y, Dir,PlayerPid]) ->
  X_inc =  - 10 * math:cos( 3.14+(Dir/180)*3.14),  %Get the correct angle
  Y_inc = 10 * math:sin( 3.14+(Dir/180)*3.14),
  erlang:send_after(?SHELL_INTERVAL, self(), trigger),
  {ok, #state{x=X,y=Y,dir=Dir,xinc = X_inc,yinc = Y_inc,playerPid=PlayerPid}}.

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
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% The trigger message tells the GUI to redraw the shell in its new position.
%% Also sends a signal to all players with its own (X,Y) coordinates to know if
%% it has hit a tank.
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(trigger, State = #state{x=X,y=Y,dir=Dir,xinc=Xspeed,yinc=Yspeed,playerPid=PlayerPid}) ->
  if
    ((X<1200) and (X > -45) and (Y < 800) and (Y>-45)) ->  %Boundaries of the game frame
      gen_server:cast(gui_server, {shell, X, Y, X + Xspeed, Y + Yspeed, Dir}),
      lists:foreach(fun({_Ip,Pid}) ->
        if
          (Pid /= PlayerPid) -> gen_server:cast(Pid, {hit,X,Y, self(), PlayerPid});
          true -> ok
        end
       end, ets:tab2list(ids));
    true ->  gen_server:cast(game_manager, {kill_shell,self()})%supervisor:terminate_child(shell_sup,self())
  end,

  erlang:send_after(?SHELL_INTERVAL, self(), trigger),
  {noreply, State#state{x=X+Xspeed,y=Y+Yspeed}}.

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
  io:format("shell terminated~n"),
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
