%% parque header file

-define(max_x,(1080)).
-define(max_y,(720)).
-define(SHELL_INTERVAL, 20).
-define(CRATE_RANGE_INTERVAL, 7000).
-define(CRATE_MIN_INTERVAL, 7000).
-define(CRATE_INTERVAL, 1000).
-define(inRange(Xself,Xother,Yself,Yother), ((abs((Xother) - (Xself+5)) < 30) and (abs((Yother)-(Yself+5))<30))).