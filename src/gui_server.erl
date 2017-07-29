%%%-------------------------------------------------------------------
%%% @author jon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2017 19:34
%%%-------------------------------------------------------------------
-module(gui_server).
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
-include_lib("wx/include/wx.hrl").
-define(max_x,(1024)).
-define(max_y,(768)).
-record(state, {}).

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
init([]) ->
  io:format("GUI Server online ~n"),
  %ets:new(colors, [set, named_table]),
  %ets:insert(colors, [{"Graphics/redBody.png","Graphics/redTurret.png"},
  %  {"Graphics/blueBody.png","Graphics/blueTurret.png"},
  %  {"Graphics/cyanBody.png","Graphics/cyanTurret.png"},
  %  {"Graphics/greenBody.png","Graphics/greenTurret.png"}]),
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Main Game Frame", [{size, {?max_x, ?max_y}}]),
  MenuBar = wxMenuBar:new(),
  wxFrame:setMenuBar(Frame, MenuBar),
  Panel = wxPanel:new(Frame),
  wxFrame:connect(Panel, paint),
  Image1 = wxImage:new("Graphics/blueBody.png"),
  Image2 = wxImage:new("Graphics/circle.png"),
  Image3 = wxImage:rotate90(Image1),
  Img = wxImage:rotate(Image2,1.9,{300,300}),
  B1 = wxImage:new("Graphics/redBody.png"),
  T1 = wxImage:new("Graphics/redTurret.png"),
  wxFrame:show(Frame),
  %Tanks = [ {id1, {B1,T1}, {100,100},{0,2},{0,2} }],%,{id2, {B1,T1},{300,300},{5,1},{0,-1}}],

  Paint = wxPaintDC:new(Panel),
  Brush1 = wxBrush:new(),
  wxBrush:setColour(Brush1, ?wxBLACK),
  wxDC:setBrush(Paint,Brush1),
  wxDC:drawRectangle(Paint,{0,0,?max_x,?max_y}),
  wxBrush:destroy(Brush1),
  wxPaintDC:destroy(Paint),
  {ok, {Panel}}.

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
handle_call(Request, _From, {Panel}) ->
  case Request of
    {new,BodyIm,TurretIm,PosX,PosY,Angle} ->
      Body = wxImage:new(BodyIm), Turret = wxImage:new(TurretIm),
      draw_body(Panel, {PosX,PosY}, Body,Angle),
      draw_turret(Panel, {PosX,PosY},Turret,Angle);

    {body,BodyIm,TurretIm,OldX,OldY,NewX,NewY,EffAngle,OldTurretAngle} ->
      Body = wxImage:new(BodyIm), Turret = wxImage:new(TurretIm),
      draw_background(Panel, OldX-15,OldY-10, wxImage:getWidth(Body)+30,wxImage:getHeight(Body)+25),
      draw_body(Panel, {NewX,NewY}, Body,EffAngle),
      draw_turret(Panel, {NewX,NewY},Turret,OldTurretAngle);
     % State = {Panel, maps:update(Player,{{Body,Turret}, {PosX+SpeedX,PosY+SpeedY},{SpeedX,SpeedY},{EffAngle,OldTurretAngle}}, Tanks)};

    {turret,BodyIm,TurretIm,PosX,PosY, EffAngle, OldBodyAngle}  ->
      Body = wxImage:new(BodyIm), Turret = wxImage:new(TurretIm),
      draw_background(Panel, PosX,PosY-5, wxImage:getWidth(Body),wxImage:getHeight(Body)+10),
      draw_body(Panel, {PosX,PosY}, Body,OldBodyAngle),
      draw_turret(Panel, {PosX,PosY},Turret,EffAngle);
     % State = {Panel, maps:update(Player,{{Body,Turret}, {PosX,PosY},{SpeedX,SpeedY},{OldBodyAngle,EffAngle}}, Tanks)};
    {shell, X, Y, NewX,NewY,Dir} ->
      EraseShell = wxImage:new("Graphics/eraseShell.png"),
      ShellPic = wxImage:new("Graphics/Shell.png"),
      draw_shell(Panel, X, Y, NewX, NewY, ShellPic, EraseShell, Dir)

  end,
  {reply, ok, {Panel}}.

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
handle_cast(Request, {Panel}) ->
  case Request of
    {shell, X, Y, NewX,NewY,Dir} ->
      EraseShell = wxImage:new("Graphics/eraseShell.png"),
      ShellPic = wxImage:new("Graphics/Shell.png"),
      draw_shell(Panel, X, Y, NewX, NewY, ShellPic, EraseShell, Dir)
  end,
  {noreply, {Panel}}.

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
  io:format("GUI Server terminated ~n"),
  ets:delete(colors),
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

draw_background(Panel, X,Y,SizeX,SizeY) ->
  Paint = wxPaintDC:new(Panel),
  Brush1 = wxBrush:new(),
  wxBrush:setColour(Brush1, ?wxBLACK),
  wxDC:setBrush(Paint,Brush1),
  wxDC:drawRectangle(Paint,{X,Y,SizeX, SizeY}),
  wxBrush:destroy(Brush1),
  wxPaintDC:destroy(Paint).

draw_body(Panel, {X, Y}, Body, Dir) ->
  ClientDC = wxClientDC:new(Panel),
  Img = wxImage:rotate(Body,3.14+(Dir/180)*3.14,{90, 90}),
  Bitmap = wxBitmap:new(Img),
  wxDC:drawBitmap(ClientDC, Bitmap, {round(X+45-wxImage:getHeight(Img)/2), round(Y-15+60-wxImage:getWidth(Img)/2)}),
  wxBitmap:destroy(Bitmap),
  wxClientDC:destroy(ClientDC).

draw_turret(Panel, {X, Y}, Turret, Dir) ->
  ClientDC = wxClientDC:new(Panel),
  Img = wxImage:rotate(Turret,3.14+(Dir/180)*3.14,{90, 90}), %[ {interpolating, true},{offset_after_rotation, {100,100}}]),getHeight
  Bitmap2 = wxBitmap:new(Img),
  wxDC:drawBitmap(ClientDC, Bitmap2, {round(X+45-wxImage:getHeight(Img)/2), round(Y-15+60-wxImage:getWidth(Img)/2)}),
  wxBitmap:destroy(Bitmap2),
  wxClientDC:destroy(ClientDC).

draw_shell(Panel, Xold, Yold, Xnew, Ynew, ShellPic, ErasePic, Dir) ->
  ClientDC = wxClientDC:new(Panel),
  Erase = wxImage:rotate(ErasePic,Dir/50,{15, 15}),
  Shell = wxImage:rotate(ShellPic,Dir/50,{15, 15}),
  BitmapShellErase = wxBitmap:new(Erase),
  BitmapShell = wxBitmap:new(Shell),
  wxDC:drawBitmap(ClientDC, BitmapShellErase, {round(Xold-wxImage:getHeight(Shell)/2), round(Yold-wxImage:getWidth(Shell)/2)}),
  wxDC:drawBitmap(ClientDC, BitmapShell, {round(Xnew-wxImage:getHeight(Shell)/2), round(Ynew-wxImage:getWidth(Shell)/2)}),
  wxClientDC:destroy(ClientDC).