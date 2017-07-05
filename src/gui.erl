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
  loop(Panel,#{},Frame).



loop(Panel, Tanks,Frame)->
  receive
    {exit} -> ok;
    {Player} ->
      Paint = wxPaintDC:new(Panel),
      Brush1 = wxBrush:new(),
      wxBrush:setColour(Brush1, ?wxBLACK),
      wxDC:setBrush(Paint,Brush1),
      wxDC:drawRectangle(Paint,{0,0,?max_x,?max_y}),
      wxBrush:destroy(Brush1),
      wxPaintDC:destroy(Paint),


     % loop(Panel, [{Player, { wxImage:new("Graphics/redBody.png"), wxImage:new("Graphics/redTurret.png")}, {100,100},{0,2},{0,2} }|Tanks], Frame);
      loop(Panel, Tanks#{Player => {{ wxImage:new("Graphics/redBody.png"), wxImage:new("Graphics/redTurret.png")}, {100,100},{0,2},{0} }}, Frame);
    {Player,SpeedX,SpeedY, Angle} ->
      BodyPic = wxImage:new("Graphics/redBody.png"),
      {{Body,Turret}, {PosX,PosY},{_SpeedX,_SpeedY},{OldAngle}} = maps:get(Player,Tanks),
      %[draw_background(Panel, PosX,PosY-5, wxImage:getWidth(Body),wxImage:getHeight(Body)+10) || {PlayerName, {Body,Turret}, {PosX,PosY},{_SpeedX,_SpeedY},{TurretDir,RotateSpeed}} <- Tanks, PlayerName =:= Player],
      draw_background(Panel, PosX,PosY-5, wxImage:getWidth(BodyPic),wxImage:getHeight(BodyPic)+10),
     % {_ID, {Body,Turret}, {PosX,PosY},{_SpeedX,_SpeedY},{TurretDir,RotateSpeed}} = hd(Tanks),
     % [draw_board(Panel, {PosX+SpeedX,PosY+SpeedY}, Body,Turret,TurretDir+RotateSpeed) || {PlayerName, {Body,Turret}, {PosX,PosY},{_SpeedX,_SpeedY},{TurretDir,RotateSpeed}} <- Tanks, PlayerName =:= Player],
     % draw_background(Panel, PosX,PosY-5, 200,180),
     % draw_board(Panel, {PosX,SpeedX,PosY,SpeedY}, Body,Turret,TurretDir+RotateSpeed),
      if (Angle =:= 0) ->
         EffAngle =  OldAngle;
        true -> EffAngle = Angle
        end,
      draw_board(Panel, {PosX,SpeedX,PosY,SpeedY}, Body,Turret,EffAngle),
     % TanksNew =  [{ID, {Body,Turret}, {PosX+SpeedX,PosY+SpeedY},{SpeedX,SpeedY},{TurretDir+RotateSpeed,RotateSpeed}} || {ID, {Body,Turret}, {PosX,PosY},{_SpeedX,_SpeedY},{TurretDir,RotateSpeed}} <- Tanks, ID =:= Player],
      TanksNew = maps:update(Player,{{Body,Turret}, {PosX+SpeedX,PosY+SpeedY},{SpeedX,SpeedY},{EffAngle}}, Tanks),
      %draw_board(Panel, {NewX,NewY}, Image),
      loop(Panel,TanksNew, Frame)

 % after 30 ->
    %{NewX,NewY} = {X+1,Y},
    %draw_background(Panel),
   % [draw_board(Panel, {PosX+SpeedX,PosY+SpeedY}, Body,Turret,TurretDir+RotateSpeed) || {_ID, {Body,Turret}, {PosX,PosY},{SpeedX,SpeedY},{TurretDir,RotateSpeed}} <- Tanks],
   % TanksNew =  [{ID, {Body,Turret}, {PosX+SpeedX,PosY+SpeedY},{SpeedX,SpeedY},{TurretDir+RotateSpeed,RotateSpeed}} || {ID, {Body,Turret}, {PosX,PosY},{SpeedX,SpeedY},{TurretDir,RotateSpeed}} <- Tanks],
    %draw_board(Panel, {NewX,NewY}, Image),
   % loop(Panel,TanksNew, Frame)
  end.

draw_background(Panel, X,Y,SizeX,SizeY) ->
 % Circle = wxImage:new("Graphics/circle.png"),
 % ClientDC = wxClientDC:new(Panel),
 % Bitmap2 = wxBitmap:new(Circle),

  Paint = wxPaintDC:new(Panel),
  Brush1 = wxBrush:new(),
  wxBrush:setColour(Brush1, ?wxBLACK),
  wxDC:setBrush(Paint,Brush1),
%  wxDC:drawBitmap(ClientDC, Bitmap2, {round(X+55), round(Y-15+55)}),
  wxDC:drawRectangle(Paint,{X,Y,SizeX, SizeY}),
  wxBrush:destroy(Brush1),
  wxPaintDC:destroy(Paint).
 % wxBitmap:destroy(Bitmap2),
 % wxClientDC:destroy(ClientDC).

draw_board(Panel, {X, X_add, Y, Y_add}, Body, Turret, Dir) ->
  Xnew = X +X_add, Ynew = Y + Y_add,
  Pos = {round(Xnew), round(Ynew)},
  Pos2 = {round(Xnew)+13, round(Ynew)-15},
  ClientDC = wxClientDC:new(Panel),
  Bitmap = wxBitmap:new(Body),
  %wxDC:drawBitmap(ClientDC, Bitmap, Pos),
  Img = wxImage:rotate(Turret,3.14+(Dir/180)*3.14,{90, 90}), %[ {interpolating, true},{offset_after_rotation, {100,100}}]),getHeight
  %Img = wxImage:rotate90(Turret),
  Bitmap2 = wxBitmap:new(Img),
  %wxDC:drawBitmap(ClientDC, Bitmap2, {round(X)+13-wxBitmap:getWidth(Bitmap2), round(Y)-15-wxBitmap:getHeight(Bitmap2)}),
  wxDC:drawBitmap(ClientDC, Bitmap2, {round(Xnew+45-wxImage:getHeight(Img)/2), round(Ynew-15+60-wxImage:getWidth(Img)/2)}),
  wxBitmap:destroy(Bitmap),
  wxClientDC:destroy(ClientDC).


clearScreen(Frame)->

  NewPanel = wxPanel:new(Frame),
  %wxWindow:setSize(Frame, {?max_x+1, ?max_y+1}),
  wxWindow:setSize(Frame, {?max_x, ?max_y}),
  NewPanel.