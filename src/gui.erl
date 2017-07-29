%%%-------------------------------------------------------------------
%%% @author vlad
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2017 3:42 PM
%%%-------------------------------------------------------------------
-module(gui).
-author("vlad").

%% API
-export([start/0]).
-include_lib("wx/include/wx.hrl").
-define(max_x,(1024)).
-define(max_y,(768)).


start() ->
  Wx = wx:new(),
  Frame = wxFrame:new(Wx, -1, "Main Game Frame", [{size, {?max_x, ?max_y}}]),
  MenuBar = wxMenuBar:new(),
  wxFrame:setMenuBar(Frame, MenuBar),
  Panel = wxPanel:new(Frame),
  wxFrame:connect(Panel, paint),
  B1 = wxImage:new("Graphics/Shell.png"),
  T1 = wxImage:new("Graphics/Shell.png"),
  wxFrame:show(Frame),
  Tanks = [ {id1, {B1,T1}, {10,10},{0,1},{50,-2} },{id2, {B1,T1},{300,300},{1,1},{132,1}}],
  loop(Panel, Tanks,Frame).



loop(Panel, Tanks,Frame)->
  receive
  after 30 ->
    %{NewX,NewY} = {X+1,Y},
    draw_background(Panel),
    [draw_board(Panel, {PosX+SpeedX,PosY+SpeedY}, Body,Turret,TurretDir+RotateSpeed) || {_ID, {Body,Turret}, {PosX,PosY},{SpeedX,SpeedY},{TurretDir,RotateSpeed}} <- Tanks],
    TanksNew =  [{ID, {Body,Turret}, {PosX+SpeedX,PosY+SpeedY},{SpeedX,SpeedY},{TurretDir+RotateSpeed,RotateSpeed}} || {ID, {Body,Turret}, {PosX,PosY},{SpeedX,SpeedY},{TurretDir,RotateSpeed}} <- Tanks],
    %draw_board(Panel, {NewX,NewY}, Image),
    loop(Panel,TanksNew, Frame)
  end.

draw_background(Panel) ->

  Paint = wxPaintDC:new(Panel),
  Brush1 = wxBrush:new(),
  wxBrush:setColour(Brush1, ?wxBLACK),
  wxDC:setBrush(Paint,Brush1),
  wxDC:drawRectangle(Paint,{0,0,?max_x, ?max_y}),
  wxBrush:destroy(Brush1),
  wxPaintDC:destroy(Paint).


draw_board(Panel, {X, Y}, Body, Turret, Dir) ->
  ClientDC = wxClientDC:new(Panel),
  Img = wxImage:rotate(Turret,Dir/50,{15, 15}),
  Bitmap2 = wxBitmap:new(Img),
  wxDC:drawBitmap(ClientDC, Bitmap2, {round(X+0-wxImage:getHeight(Img)/2), round(Y-0-wxImage:getWidth(Img)/2)}),
  wxClientDC:destroy(ClientDC).


clearScreen(Frame)->

  NewPanel = wxPanel:new(Frame),
  %wxWindow:setSize(Frame, {?max_x+1, ?max_y+1}),
  wxWindow:setSize(Frame, {?max_x, ?max_y}),
  NewPanel.
