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

:- ['domain-task32.pl'].		% Replace with the domain for this problem




% --- Definition of the initial state ---------------------------------

car(carA).
key(keysA, carA).
stored(keysA, carA, s0).
dirty(carA, s0).
in(carA, parkingLot, s0).

car(carB).
key(keysB, carB).
isHoldingKey(agent, keysB, carB, s0).
in(carB, dropOff, s0).
parked(carA, s0).

in(agent, dropOff, s0).
connected(dropOff, parkingLot).
connected(parkingLot, dropOff).
connected(parkingLot, pickUp).
connected(pickUp, parkingLot).

% --- Goal condition that the planner will try to reach ---------------

goal(S) :- stored(keysB, carB, S), delivered(carA, S), isHoldingKey(agent, keysA, carA, S).
% goal(S) :- in(agent, parkingLot, S).
% goal(S) :- stored(keysA, carA, S).
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
