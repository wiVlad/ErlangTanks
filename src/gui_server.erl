%%%-------------------------------------------------------------------
%%% @author jon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% The GUI for the game. Handles all wxWidgets related actions
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
-include("ErlangTanks.hrl").
-include("data.hrl").
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
%% Initializes the server, create the main game frame and the two panels.
%% Creates the grid, and the timer display
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
  process_flag(trap_exit, true),
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Main Game Frame", [ {pos, {0,0}}, {size, {1320,720}}]),
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

  {A,B,C,D} = (local_ip_v4()),
  Address = "Server IP: "++integer_to_list(A)++"."++integer_to_list(B)++"."++integer_to_list(C)++"."++integer_to_list(D),
  %% Create static texts
  Texts = [wxStaticText:new(Panel2, 1, "Erlang Tanks Game", []),
    wxStaticText:new(Panel2, 3, Address,
      [{style, ?wxALIGN_CENTER bor ?wxST_NO_AUTORESIZE}])],

  Image = wxImage:new("Graphics/erlang2.png", []),
  Bitmap = wxBitmap:new(wxImage:scale(Image,
    round(wxImage:getWidth(Image)*0.19),
    round(wxImage:getHeight(Image)*0.25),
    [{quality, ?wxIMAGE_QUALITY_HIGH}])),
  StaticBitmap = wxStaticBitmap:new(Panel2, 1, Bitmap),

  B_Start = wxButton:new(Panel2, 1, [{label,"Start"},{size, {80, 30}}]),
  B_Quit = wxButton:new(Panel2, 2, [{label,"Quit"}, {size, {80, 30}}]),
  wxWindow:connect(B_Start, command_button_clicked),
  wxWindow:connect(B_Quit, command_button_clicked),

  Seconds = (?GAME_LENGTH) rem 60,
  Minutes = (?GAME_LENGTH) div 60,
  if
    (Seconds > 10) -> TimeText = integer_to_list(Minutes)++":"++integer_to_list(Seconds);
    true ->           TimeText = integer_to_list(Minutes)++":0"++integer_to_list(Seconds)
    end,
  TimeView = wxStaticText:new(Panel2, 33, TimeText, [{style, ?wxALIGN_CENTER bor ?wxST_NO_AUTORESIZE}]),
  wxStaticText:setFont(TimeView,wxFont:new(16, ?wxFONTFAMILY_SWISS,
    ?wxFONTSTYLE_NORMAL,
    ?wxFONTWEIGHT_NORMAL, [])),
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
  wxSizer:add(ButtonSizer, TimeView, [{flag, ?wxTOP bor ?wxBOTTOM bor ?wxLEFT bor ?wxEXPAND},
    {border, 5}]),
  wxSizer:add(GridSizer, Grid, [{flag, ?wxALL},
    {border, 5},{proportion, 1}]),

  Options = [{flag, ?wxEXPAND}, {proportion, 1}],
  wxSizer:add(MainSizer, TextSizer, Options),
  wxSizer:add(MainSizer, ButtonSizer, [{border, 5}, {flag, ?wxALL  bor ?wxEXPAND}]),
  wxSizer:add(MainSizer, GridSizer, [{border, 5}, {flag, ?wxALL}, {proportion, 1}]),
  wxPanel:setSizer(Panel2, MainSizer),

  wxFrame:connect(Panel, paint),
  wxFrame:show(Frame),
  wxPanel:setBackgroundColour(Panel,?wxBLACK),
  wxPanel:setBackgroundColour(Panel2,?wxLIGHT_GREY),
  wxFrame:setMaxSize(Frame,{1320,720}), %Set limits to game frame
  wxFrame:setMinSize(Frame,{1320,720}),

  F = fun() ->
    mnesia:write(#gui_state{panel=Panel,grid=Grid,time=TimeView,timer=?GAME_LENGTH}) end,
  mnesia:transaction(F), %Write into database
  {ok, {Panel,Grid,TimeView,?GAME_LENGTH}}.


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
%% Handling cast messages. Whenever a change needs to be made to the GUI, a cast is made
%% to here. This can be updating the grid, moving the tanks, drawing shells, re-drawing crates, etc.
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(Request, {Panel,Grid,TimeView,Timer}) ->
  case Request of
    {new,_PlayerName,BodyIm,TurretIm,PosX,PosY,BodyAngle,TurretAngle} ->
      Body = wxImage:new(BodyIm), Turret = wxImage:new(TurretIm),
      draw_body(Panel, {PosX,PosY}, Body,BodyAngle),
      draw_turret(Panel, {PosX,PosY},Turret,TurretAngle);

    {body,BodyIm,TurretIm,OldX,OldY,NewX,NewY,EffAngle,OldTurretAngle} ->
      Body = wxImage:new(BodyIm), Turret = wxImage:new(TurretIm),
      draw_background(Panel, OldX-15,OldY-10, wxImage:getWidth(Body)+30,wxImage:getHeight(Body)+25),
      draw_body(Panel, {NewX,NewY}, Body,EffAngle),
      draw_turret(Panel, {NewX,NewY},Turret,OldTurretAngle);

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
    {grid,Name, Num, _Ammo, _HP} ->
      wxGrid:setRowLabelValue(Grid, Num, Name);
    {grid, Num, Score} ->
      wxGrid:setCellValue(Grid, Num, 2,integer_to_list(Score));
    {grid, Num, Ammo, HP} ->
      wxGrid:setCellValue(Grid, Num, 1,integer_to_list(HP)),
      wxGrid:setCellValue(Grid, Num, 0,integer_to_list(Ammo) );
    {explosion, X, Y} ->
      draw_explosion(Panel, X, Y);
    {background, X, Y,SizeX,SizeY} ->
      draw_background(Panel, X-15,Y-10, SizeX,SizeY);
    {crate, X, Y, health} ->
      CratePic = wxImage:new("Graphics/crateHealth.png"),
      draw_crate(Panel, X, Y, CratePic);
    {crate, X, Y, ammo} ->
      CratePic = wxImage:new("Graphics/crateAmmo.png"),
      draw_crate(Panel, X, Y, CratePic);
    {erase, {Num,X,Y}} ->
      wxGrid:deleteRows(Grid, [{pos, Num},{numRows, 1}]),
      draw_background(Panel, X-15,Y-10, 90+30,90+25),
      F = fun() ->
        mnesia:write(#gui_state{panel=Panel,grid=Grid,time=TimeView,timer=Timer}) end,
      mnesia:transaction(F);
    {winner, PlayerName} ->
      D = wxMessageDialog:new(Panel,"The Winner is "++PlayerName++"!"),
      wxMessageDialog:showModal(D)
  end,
  {noreply, {Panel,Grid,TimeView,Timer}}.

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

%"START" button has been pressed, send signal to game manager to start the game
handle_info({wx,1,A,_B,_C}, State) ->
  wxButton:disable(A),
  io:format("START!~n"),
  timer:send_after(1000, {newTime,?GAME_LENGTH}),
  timer:send_after(5000, backup),
  gen_server:cast(game_manager,startGame),
  {noreply, State};

%"QUIT" button has been pressed. Call application:stop(tanks_app).
handle_info({wx,2,_A,_B,_C}, State) ->
  io:format("QUIT! node: ~p~n",[node()]),
  spawn(node(),application,stop,[tanks_app]),
  {noreply, State};
%Create a new backup for the gui_server in the Mnesia database
handle_info(backup, {Panel,Grid,TimeView,Timer}) ->
  F = fun() ->
    mnesia:write(#gui_state{panel=Panel,grid=Grid,time=TimeView,timer=Timer}) end,
  mnesia:transaction(F),
  timer:send_after(5000, backup),
  {noreply, {Panel,Grid,TimeView,Timer}};

%Timer reaches 0:00, end the game
handle_info({newTime,0}, {Panel,Grid,TimeView,Timer}) ->
  wxStaticText:setLabel(TimeView,"0:00"),
  gen_server:cast(game_manager,endGame),
  D = wxMessageDialog:new(Panel,"Time's up!"),
  wxMessageDialog:showModal(D),
  {noreply, {Panel,Grid,TimeView,Timer}};

%Display the new time on the timer display
handle_info({newTime,Timer}, {Panel,Grid,TimeView,_Timer}) ->
  Seconds = (Timer-1) rem 60,
  Minutes = (Timer-1) div 60,
  if
    (Seconds > 9) ->
      wxStaticText:setLabel(TimeView,integer_to_list(Minutes)++":"++ integer_to_list(Seconds));
    (Seconds >= 0) ->
      wxStaticText:setLabel(TimeView,integer_to_list(Minutes)++":0"++ integer_to_list(Seconds))
  end,
  timer:send_after(1000, {newTime,Timer -1}),
  F = fun() ->
    mnesia:write(#gui_state{panel=Panel,grid=Grid,time=TimeView,timer=Timer-1}) end,
  mnesia:transaction(F),
  {noreply, {Panel,Grid,TimeView,Timer-1}};


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
%Draws a black rectangle, used to erase previous position of tank
draw_background(Panel, X,Y,SizeX,SizeY) ->
  Paint = wxPaintDC:new(Panel),
  Brush1 = wxBrush:new(),
  wxBrush:setColour(Brush1, ?wxBLACK),
  wxDC:setBrush(Paint,Brush1),
  wxDC:drawRectangle(Paint,{X,Y,SizeX, SizeY}),
  wxBrush:destroy(Brush1),
  wxPaintDC:destroy(Paint).

%Draw the tank body in the correct direction and (X,Y)
draw_body(Panel, {X, Y}, Body, Dir) ->
  ClientDC = wxClientDC:new(Panel),
  Img = wxImage:rotate(Body,3.14+(Dir/180)*3.14,{90, 90}),
  Bitmap = wxBitmap:new(Img),
  wxDC:drawBitmap(ClientDC, Bitmap, {round(X+45-wxImage:getHeight(Img)/2), round(Y-15+60-wxImage:getWidth(Img)/2)}),
  wxBitmap:destroy(Bitmap),
  wxClientDC:destroy(ClientDC).

%Draw the tank turret in the correct direction and (X,Y)
draw_turret(Panel, {X, Y}, Turret, Dir) ->
  ClientDC = wxClientDC:new(Panel),
  Img = wxImage:rotate(Turret,3.14+(Dir/180)*3.14,{90, 90}), %[ {interpolating, true},{offset_after_rotation, {100,100}}]),getHeight
  Bitmap2 = wxBitmap:new(Img),
  wxDC:drawBitmap(ClientDC, Bitmap2, {round(X+45-wxImage:getHeight(Img)/2), round(Y-15+60-wxImage:getWidth(Img)/2)}),
  wxBitmap:destroy(Bitmap2),
  wxClientDC:destroy(ClientDC).

%Draw the shell
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

%When a shell hits a tank, draw an explosion
draw_explosion(Panel, X, Y) ->
  ClientDC = wxClientDC:new(Panel),
  Img = wxImage:new("Graphics/explosion.png"),
  Bitmap = wxBitmap:new(Img),
  wxDC:drawBitmap(ClientDC, Bitmap, {round(X+45-wxImage:getHeight(Img)/2), round(Y-15+60-wxImage:getWidth(Img)/2)}),
  wx_misc:bell(),
  wxBitmap:destroy(Bitmap),
  wxClientDC:destroy(ClientDC).

%Draw a crate once it has been created
draw_crate(Panel, X, Y, Pic)->
  ClientDC = wxClientDC:new(Panel),
  Bitmap = wxBitmap:new(Pic),
  wxDC:drawBitmap(ClientDC, Bitmap, {round(X+45-wxImage:getHeight(Pic)/2), round(Y-15+60-wxImage:getWidth(Pic)/2)}),
  wxBitmap:destroy(Bitmap),
  wxClientDC:destroy(ClientDC).

%Get local host IP to be displayed on the side panel.
local_ip_v4() ->
  {ok, Addrs} = inet:getifaddrs(),
  hd([Addr || {_, Opts} <- Addrs,
    {addr, Addr} <- Opts,
    size(Addr) == 4, Addr =/= {127,0,0,1}]).