https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder

% Primitive actions
primitive_action(go(X, Y, R)).  
primitive_action(push(B, X, Y, R)).      
primitive_action(climbUp(X, B)).  
primitive_action(climbDown(B, X)).      
primitive_action(turnOn(S, B)).    
primitive_action(turnOff(S, B)).


% Successor State Axioms for Primitive Fluents.

changeLocAction(go(_, _, _)).
changeLocAction(push(_, _, _, _)).
changeLocAction(climbUp(_, _)).

robotLoc(L, do(A,S)):-
	A = go(_, L, _); % robot in L when go to L
	A = push(_, _, L, _); % robot in L when push box to L
	A = climbDown(_, L);  % robot in L when climb down box to L
	not(changeLocAction(A)), robotLoc(L,S). % robot in L when previous in l and not changing position

boxLoc(B, L, do(A,S)):-
	A = push(B, _, L, _); % robot in L when push box to L
	not(A = push(B, _, _, _)), boxLoc(B, L, S). % robot in L when previous to L and not push to other location

onTop(B, do(A,S)):-
	A = climbUp(_, B); % onTop when clim up to box
	not(A = climbDown(B, _)), onTop(B, S). % onTop when previous onTop and not climbDown

up(Switch, do(A,S)):-
	A = turnOn(Switch, _);  % up when turn on
	not(A = turnOff(Switch, _)), up(Switch, S). % up when previous up and not turn off


lightOn(Light, do(A,S)):-
	controls(Switch, Light), 
	A = turnOn(Switch, _). % light on when switch up

lightOn(Light, do(A,S)):-
	controls(Switch, Light),
	not(A = turnOff(Switch, _)), % light on when previous on and not turn off
	up(Switch, S).

% Preconditions for Primitive Actions.
poss(go(X, Y, R),S) :- 
	X \= Y,
	in(X, R),  % X in room R
	in(Y, R),  % Y in room R
	robotLoc(X, S). % robot in X

poss(push(B, X, Y, R), S):-
	X \= Y,
	in(X, R), % X in room R
	in(Y, R), % Y in room R
	boxLoc(B, X, S),  % box in X
	robotLoc(X, S). % robot in X

poss(climbUp(X, B), S):-
	boxLoc(B, X, S),  % box in X
	robotLoc(X, S), % robot in X
	not(onTop(B, S)). % robot not on box

poss(climbDown(B, X), S):-
	boxLoc(B, X, S), % box in X
	onTop(B, S). % robot on box

poss(turnOn(Switch, B), S):-
	not(up(Switch, S)), % switch not up
	boxLoc(B, Loc, S), % box in loc
	switchLoc(Switch, Loc), % switch in loc
	onTop(B, S). % robot on box

poss(turnOff(Switch, B), S):-
	up(Switch, S), % switch  up
	boxLoc(B, Loc, S),  % box in loc
	switchLoc(Switch, Loc),  % switch in loc
	onTop(B, S). % robot on box

% entity type

isBox(box1).
isBox(box2).
isBox(box3).
isBox(box4).

isRoom(room1).
isRoom(room2).
isRoom(room3).
isRoom(room4).
isRoom(corridor).

isDoor(door1).
isDoor(door2).
isDoor(door3).
isDoor(door4).


% Initial Situation.

boxLoc(box1, locInitBox1, s0).
boxLoc(box2, locInitBox2, s0).
boxLoc(box3, locInitBox3, s0).
boxLoc(box4, locInitBox4, s0).

in(locInitBox1, room1).
in(locInitBox2, room1).
in(locInitBox3, room1).
in(locInitBox4, room1).

switchLoc(switch1, switch1Loc).
switchLoc(switch2, switch2Loc).
switchLoc(switch3, switch3Loc).
switchLoc(switch4, switch4Loc).

in(switch1Loc, room1).
in(switch2Loc, room2).
in(switch3Loc, room3).
in(switch4Loc, room4).

controls(switch1, light1).
controls(switch2, light2).
controls(switch3, light3).
controls(switch4, light4).


in(door1, room1).
in(door1, corridor).

in(door2, room2).
in(door2, corridor).

in(door3, room3).
in(door3, corridor).

in(door4, room4).
in(door4, corridor).

robotLoc(locInitRobot, s0).
in(locInitRobot, room1).


% definition of executable (legal) situation

executable(s0).
executable(do(A,S)) :- poss(A,S), executable(S).

%% goal of having Box2 in Room2
goalB(S):-
	boxLoc(box2, L, S),
	in(L, room2).

%% plan that achieve the goal of having Box2 in Room2
planB(Plan):-
    Plan = do(push(box2, door2, switch2Loc, room2), do(push(box2, door1, door2, corridor),  do(push(box2, locInitBox2, door1, room1),  do(go(locInitRobot, locInitBox2, room1), s0)))).

%% test plan is executable (legal) and achieves the goal
testB(Plan):-
   planB(Plan),
   executable(Plan),
   goalB(Plan).

%% planB(Plan), executable(Plan),  goalB(Plan).

%% check all lights on 
checkAllLightsON(S):-
	lightOn(light1, S),
	lightOn(light2, S),
	lightOn(light3, S),
	lightOn(light4, S).

turnUpSwitch(Switch, do(climbDown(B, Loc), do(turnOn(Switch, B), do(climbUp(Loc, B), S)))):-
	switchLoc(Switch, Loc),
	pushBoxToLoc(S, B, Loc).


pushBoxToLoc(S, B, Loc):-
	boxLoc(B, Loc, S),
	!.

pushBoxToLoc(do(A, S) , B, Loc):-
	boxLoc(B, BoxLoc, S),
	in(BoxLoc, R),
	in(Loc, R),
	A = push(B, BoxLoc, Loc, R).

%% pushBoxToLoc(S, B, Loc):-
%% 	boxLoc().

allLightsOn(S):-
	checkAllLightsON(S),
	executable(S).


%% acceptable(A, S):-
%% 	boxLoc(box2, BoxLoc, S),
%% 	robotLoc(RoLoc, S),
%% 	BoxLoc \= RoLoc,
%% 	%% !,
%% 	A = go(_, _, _).

%% acceptable(A, S):-
%% 	boxLoc(box2, BoxLoc, S),
%% 	robotLoc(BoxLoc, S),
%% 	%% !,
%% 	A = push(box2, BoxLoc, _, _).


acceptable(go(_, _, _), S):-
	boxLoc(box2, BoxLoc, S),
	robotLoc(RoLoc, S),
	BoxLoc \= RoLoc.
	%% %% !,
	%% A = .

acceptable(push(box2, BoxLoc, _, _), S):-
	boxLoc(box2, BoxLoc, S),
	robotLoc(BoxLoc, S).

%% acceptable(A, S):-
%% 	poss(A,S).

%% goal(S):-
%% 	allLightsOn(S).

%% goal(S):-
%% 	lightOn(light1, S).

%% goal(S).

goal(S):-
	goalB(S).

%% poss(go(locInitRobot, locInitBox1, room1),s0).

% Restore suppressed situation arguments.

restoreSitArg(goal,S,goal(S)).
restoreSitArg(acceptable(A),S,acceptable(A,S)).
restoreSitArg(robotLoc(L),S,robotLoc(L,S)).
restoreSitArg(boxLoc(B, L),S,boxLoc(B, L,S)).
restoreSitArg(onTop(B),S,onTop(B, S)).
restoreSitArg(up(Switch),S,up(Switch, S)).
restoreSitArg(lightOn(B),S,lightOn(B, S)).

show_act_seq(s0).
show_act_seq(do(A,S)):- show_act_seq(S), write(A), nl.

%% proc(my, idPlan(100)).

runE:- do(idPlan(1000),s0,S), show_act_seq(S).

%% do(idPlan, s0, S).


%% lightOn(S), executable(S).

%% S = do(turnOn(switch1, box1), do(climbUp(switch1Loc, box1),  
%% 	do(push(box1, locInitBox1, switch1Loc, room1), do(go(locInitRobot, locInitBox1, room1), s0)))), 
%% lightOn(light1, S),
%% executable(S).

%% S = do(climbUp(switch1Loc, box1),  
%% 	do(push(box1, locInitBox1, switch1Loc, room1), do(go(locInitRobot, locInitBox1, room1), s0))),

%% onTop(box1, S).


%% not(up(switch1, S)),

%% boxLoc(box1, Loc, S),

%% switchLoc(switch1, Loc),

%% onTop(box1, S).

%% boxLoc(box1, Loc, S), % box in loc
%% 	switchLoc(switch1, Loc), % switch in loc
%% 	onTop(box1, S). % robot on box

%% lightOn(light1, S),
%% executable(S).


%% S = do(turnOn(switch1, box1), do(climbUp(switch1Loc, box1),  
%% 	do(push(box1, locInitBox1, switch1Loc, room1), do(go(locInitRobot, locInitBox1, room1), s0)))), 
%% lightOn(light1, S),
%% executable(S).


%% up(switch1, s0).


%% executable(S).


