% ---------------------------------------------------------------------
%  ----- Informatics 2D - 2015/16 - Second Assignment - Planning -----
% ---------------------------------------------------------------------
%
% Write here you matriculation number (only - your name is not needed)
% Matriculation Number: s1448512
%
%
% ------------------------- Problem Instance --------------------------
% This file is a template for a problem instance: the definition of an
% initial state and of a goal.

debug(on).	% need additional debug information at runtime?



% --- Load domain definitions from an external file -------------------

:- ['domain-task21.pl'].		% Replace with the domain for this problem


% --- Definition of the initial state ---------------------------------

in(agent, dropOff, s0).

car(carA).
in(carA, parkingLot, s0).
parked(carA, s0).

car(carB).
in(carB, dropOff, s0).

connected(dropOff, parkingLot).
connected(parkingLot, dropOff).
connected(parkingLot, pickUp).
connected(pickUp, parkingLot).

% --- Goal condition that the planner will try to reach ---------------

goal(S) :- parked(carB, S), delivered(carA, S), in(agent, dropOff, S).



% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
