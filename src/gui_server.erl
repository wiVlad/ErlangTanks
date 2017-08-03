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
-define(max_x,(1000)).
-define(max_y,(700)).
%-record(state, {}).
-record(state,
{
  parent,
  config,
  gl,
  canvas,
  image,
  timer,
  time
}).
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
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Main Game Frame", [ {pos, {0,0}}, {size, {1320,720}}]),
  MenuBar = wxMenuBar:new(),
  wxFrame:setMenuBar(Frame, MenuBar),
  Panel = wxPanel:new(Frame, [{pos, {0,0}},{size,{1080,720}},{style, ?wxBORDER_DOUBLE}]),
  Panel2 = wxPanel:new(Frame, [{pos, {1080,0}},{size,{240,720}},{style, ?wxBORDER_DOUBLE}]),

  %% Setup sizers
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  TextSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel2,
    [{label, "About"}]),
  ButtonSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel2,
    [{label, "Initialize"}]),
  GridSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel2,
    [{label, "Stats"}]),

  %% Create static texts
  Texts = [wxStaticText:new(Panel2, 1, "Erlang Tanks Game", []),
    wxStaticText:new(Panel2, 3, "Jonathan and Vlad",
      [{style, ?wxALIGN_CENTER bor ?wxST_NO_AUTORESIZE}])],

  Image = wxImage:new("Graphics/erlang2.png", []),
  Bitmap = wxBitmap:new(wxImage:scale(Image,
    round(wxImage:getWidth(Image)*0.19),
    round(wxImage:getHeight(Image)*0.25),
    [{quality, ?wxIMAGE_QUALITY_HIGH}])),
  StaticBitmap = wxStaticBitmap:new(Panel2, 1, Bitmap),


  B_Start = wxButton:new(Panel2, 1, [{label,"Start"}]),
  B_Quit = wxButton:new(Panel2, 2, [{label,"Quit"}]),
  wxWindow:connect(Panel2, command_button_clicked),

  Font = wxFont:new(10, ?wxFONTFAMILY_SWISS,
    ?wxFONTSTYLE_NORMAL,
    ?wxFONTWEIGHT_NORMAL, []),
  Grid = wxGrid:new(Panel2, 2, []),
  wxGrid:createGrid(Grid, 8, 3),
  wxGrid:setColLabelValue(Grid, 0, "Ammo"),
  wxGrid:setColLabelValue(Grid, 1, "Health"),
  wxGrid:setColLabelValue(Grid, 2, "Score"),
  wxGrid:setRowLabelSize(Grid, 55),
  wxGrid:setLabelFont(Grid, Font),
  wxGrid:fit(Grid),

  %% Add to sizers
  [wxSizer:add(TextSizer, Text, [{flag, ?wxEXPAND bor ?wxALL},
    {border, 5}]) || Text <- Texts],
  wxSizer:add(TextSizer, StaticBitmap, []),
  wxSizer:add(ButtonSizer, B_Start, [{flag, ?wxTOP bor ?wxBOTTOM bor ?wxEXPAND},
    {border, 5}]),
  wxSizer:add(ButtonSizer, B_Quit, [{flag, ?wxTOP bor ?wxBOTTOM bor ?wxEXPAND},
    {border, 5}]),
  wxSizer:add(GridSizer, Grid, [{flag, ?wxALL},
    {border, 5},{proportion, 1}]),

  Options = [{flag, ?wxEXPAND}, {proportion, 1}],
  wxSizer:add(MainSizer, TextSizer, Options),
  wxSizer:add(MainSizer, ButtonSizer, [{border, 5}, {flag, ?wxALL}]),
  wxSizer:add(MainSizer, GridSizer, [{border, 5}, {flag, ?wxALL}, {proportion, 1}]),
  wxPanel:setSizer(Panel2, MainSizer),

  wxFrame:connect(Panel, paint),
  wxFrame:show(Frame),
  wxPanel:setBackgroundColour(Panel,?wxBLACK),
  wxPanel:setBackgroundColour(Panel2,?wxLIGHT_GREY),
  wxFrame:setMaxSize(Frame,{1320,720}),
  wxFrame:setMinSize(Frame,{1320,720}),
  {ok, {Panel,Grid}}.




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
handle_call(Request, _From, {Panel,Grid}) ->
  case Request of
    {new,_PlayerName,BodyIm,TurretIm,PosX,PosY,Angle} ->
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
      draw_shell(Panel, X, Y, NewX, NewY, ShellPic, EraseShell, Dir);
    {grid,Name, Num, Ammo, HP} ->
      wxGrid:setRowLabelValue(Grid, Num, Name);
    {grid, Num, Score} ->
      wxGrid:setCellValue(Grid, Num, 2,integer_to_list(Score));
    {grid, Num, Ammo, HP} ->
      wxGrid:setCellValue(Grid, Num, 1,integer_to_list(HP)),
      wxGrid:setCellValue(Grid, Num, 0,integer_to_list(Ammo) );
    {explosion, X, Y} ->
      draw_explosion(Panel, X, Y);
    {background, X, Y,SizeX,SizeY} ->
      draw_background(Panel, X-15,Y-10, SizeX,SizeY)

  end,
  {reply, ok, {Panel,Grid}}.

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
handle_cast(Request, {Panel,Grid}) ->
  case Request of
    {crate, X, Y, health} ->
      CratePic = wxImage:new("Graphics/crateHealth.png"),
      draw_crate(Panel, X, Y, CratePic);
    {crate, X, Y, ammo} ->
      CratePic = wxImage:new("Graphics/crateAmmo.png"),
      draw_crate(Panel, X, Y, CratePic);
    {winner, PlayerName} ->
      D = wxMessageDialog:new(Panel,"The Winner is "++PlayerName++"!"),
      wxMessageDialog:showModal(D);
    {shell, X, Y, NewX,NewY,Dir} ->
      EraseShell = wxImage:new("Graphics/eraseShell.png"),
      ShellPic = wxImage:new("Graphics/Shell.png"),
      draw_shell(Panel, X, Y, NewX, NewY, ShellPic, EraseShell, Dir)
  end,
  {noreply, {Panel,Grid}}.

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

handle_info({wx,1,_A,_B,_C}, State) ->
  io:format("STRAT!~n"),
  gen_server:cast(game_manager,startGame),
  {noreply, State};

handle_info({wx,2,_A,_B,_C}, State) ->
  io:format("QUIT!"),
  tanks_app:stop(0),
  {noreply, State};

handle_info(Info, State) ->
  io:format("Got: ~p~n",[Info]),
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
  Erase = wxImage:rotate(ErasePic,3.14+(Dir/180)*3.14,{90, 90}),
  Shell = wxImage:rotate(ShellPic,3.14+(Dir/180)*3.14,{90, 90}),
  BitmapShellErase = wxBitmap:new(Erase),
  BitmapShell = wxBitmap:new(Shell),
  XoS = (Xnew - Xold)*5,
  YoS = (Ynew - Yold)*5,
  wxDC:drawBitmap(ClientDC, BitmapShellErase, {round(Xold+XoS+45-wxImage:getHeight(Shell)/2), round(Yold+YoS+45-wxImage:getWidth(Shell)/2)}),
  wxDC:drawBitmap(ClientDC, BitmapShell, {round(Xnew+XoS+45-wxImage:getHeight(Shell)/2), round(Ynew+YoS+45-wxImage:getWidth(Shell)/2)}),
  wxClientDC:destroy(ClientDC).

draw_explosion(Panel, X, Y) ->
  ClientDC = wxClientDC:new(Panel),
  Img = wxImage:new("Graphics/explosion.png"),
  Bitmap = wxBitmap:new(Img),
  wxDC:drawBitmap(ClientDC, Bitmap, {round(X+45-wxImage:getHeight(Img)/2), round(Y-15+60-wxImage:getWidth(Img)/2)}),
  wx_misc:bell(),
  wxBitmap:destroy(Bitmap),
  wxClientDC:destroy(ClientDC).
draw_crate(Panel, X, Y, Pic)->
  ClientDC = wxClientDC:new(Panel),
  Bitmap = wxBitmap:new(Pic),
  wxDC:drawBitmap(ClientDC, Bitmap, {round(X+45-wxImage:getHeight(Pic)/2), round(Y-15+60-wxImage:getWidth(Pic)/2)}),
  wxBitmap:destroy(Bitmap),
  wxClientDC:destroy(ClientDC).