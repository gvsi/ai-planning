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

:- ['domain-task21.pl'].		% Replace with the domain for this problem




% --- Definition of the initial state ---------------------------------

car(car).
connected(dropOff, parkingLot).
connected(parkingLot, dropOff).
connected(parkingLot, pickUp).
connected(pickUp, parkingLot).
in(car, pickUp, s0).
in(agent, dropOff, s0).

% --- Goal condition that the planner will try to reach ---------------

% goal(S) :- parked(car, S), in(agent, dropOff, S).
goal(S) :- in(car, dropOff, S), in(agent, pickUp, S).


% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
