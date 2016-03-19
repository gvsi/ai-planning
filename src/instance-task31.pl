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

:- ['domain-task31.pl'].		% Replace with the domain for this problem




% --- Definition of the initial state ---------------------------------

car(carA).
dirty(carA, s0).
car(carB).
connected(dropOff, parkingLot).
connected(parkingLot, dropOff).
connected(parkingLot, pickUp).
connected(pickUp, parkingLot).
in(carA, parkingLot, s0).
parked(carA, s0).
in(carB, dropOff, s0).
in(agent, dropOff, s0).

% --- Goal condition that the planner will try to reach ---------------

goal(S) :- parked(carB, S), delivered(carA, S), in(agent, dropOff, S).



% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
