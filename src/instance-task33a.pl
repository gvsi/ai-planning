% ---------------------------------------------------------------------
%  ----- Informatics 2D - 2015/16 - Second Assignment - Planning -----
% ---------------------------------------------------------------------
%
% Write here you matriculation number (only - your name is not needed)
% Matriculation Number: s_______
%
%
% ------------------------- Problem Instance --------------------------
% This file is a template for a problem instance: the definition of an
% initial state and of a goal.

debug(on).	% need additional debug information at runtime?



% --- Load domain definitions from an external file -------------------

:- ['domain-task33.pl'].		% Replace with the domain for this problem




% --- Definition of the initial state ---------------------------------

% car(carA).
% key(keysA, carA).
% stored(keysA, carA, s0).
% dirty(carA, s0).
% in(carA, parkingLot, s0).

car(carA).
key(keysA, carA).
in(carA, dropOff, s0).

car(carB).
key(keysB, carB).
in(carB, dropOff, s0).

in(agent, dropOff, s0).
connected(dropOff, parkingLot).
connected(parkingLot, dropOff).
connected(parkingLot, pickUp).
connected(pickUp, parkingLot).
occupied(pSpace1, someCar1, s0).
occupied(pSpace2, someCar2, s0).

% --- Goal condition that the planner will try to reach ---------------

goal(S) :- storedInUtilityBox(keysA, carA, S), storedInUtilityBox(keysB, carB, S).
% goal(S) :- in(agent, parkingLot, S).
% goal(S) :- stored(keysA, carA, S).
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
