-module(udp_server).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([send/1]).

-behaviour(gen_server).

-define(SERVER_PORT, 4000).
-define(CLIENT_PORT, 18001).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, null, []).

stop() ->
  gen_server:cast(?MODULE, stop).

init(_Params) ->
  {ok, Sock} = gen_udp:open(?SERVER_PORT),
  io:format("Init Sock ~p~n", [Sock]),
  io:format("UDP Server online ~n"),
  {ok, {server, Sock}}.

terminate(_Reason, {server, Sock}) ->
  gen_udp:close(Sock).

handle_cast(stop, {server, Sock}) ->
  {stop, normal, {server, Sock}}.

handle_info({udp, _Client, Ip, _Port, Msg}, LoopData) ->
  %io:format("receive udp data ~p from ~p~n", [Msg, Ip]),
  gen_server:call(main_server, {Ip,Msg}),
  {noreply, LoopData};

handle_info(Msg, LoopData) ->
  io:format("receive info ~p~n", [Msg]),
  {noreply, LoopData}.

send(Msg) ->
  gen_server:call(?MODULE, {message, Msg}).

handle_call(Request, _From, A) ->
  io:format("asdfadsf receive udp data ~p~n", [Request]),

  case Request of
    {udp, Socket, Host, Port, Bin} ->
      gen_server:call(main_server, Bin);
      _ -> io:format("receive udp data ~p~n", [Request])
  end,
  {reply, ok, A}.
