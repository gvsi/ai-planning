% ---------------------------------------------------------------------
%  ----- Informatics 2D - 2015/16 - Second Assignment - Planning -----
% ---------------------------------------------------------------------
%
% Write here you matriculation number (only - your name is not needed)
% Matriculation Number: s1448512
%
%
% ------------------------- Domain Definition -------------------------
% This file describes a planning domain: a set of predicates and
% fluents that describe the state of the system, a set of actions and
% the axioms related to them. More than one problem can use the same
% domain definition, and therefore include this file


% --- Cross-file definitions ------------------------------------------
% marks the predicates whose definition is spread across two or more
% files
%
% :- multifile name/#, name/#, name/#, ...

:- multifile parked/2, in/3, delivered/2, connected/2



% --- Primitive control actions ---------------------------------------
% this section defines the name and the number of parameters of the
% actions available to the planner
%
% primitive_action( dosomething(_,_) ).	% underscore means `anything'

primitive_action( move(_,_,_) ).
primitive_action( park(_,_,_) ).
primitive_action( drive(_,_,_,_) ).
primitive_action( deliver(_,_,_) ).

% --- Precondition for primitive actions ------------------------------
% describe when an action can be carried out, in a generic situation S
%
% poss( doSomething(...), S ) :- preconditions(..., S).

% Action(Move(Agent, from, to),
% PRECONDITION: In(Agent, from) and Connected(to, from),
% EFFECT: In(Agent, to) and (not In(Agent, from))
poss(move(Who, From, To), S) :-
  in(What, From, S),
  connected(To, From).


% Action(Park(Agent, Car, ParkingLot)),
% PRECONDITION: In(Agent, ParkingLot) and In(Car, ParkingLot) and (not Parked(Car)),
% EFFECT: Parked(Car))
poss(park(Who, What, Where), S) :-
  in(Who, Where, S),
  Where = parkinglot,
  not(From, To).

% Action(Drive(Agent, Car, from, to)),
% PRECONDITION: In(Agent, from) and In(Car, from) and Connected(to, from),
% EFFECT: In(Agent, to) and In(Car, from) and (not Parked(Car))
poss(drive(Who, What, From, To), S) :-
  in(Who, From, S),
  in(What, From, S),
  connected(To, From).

% Action(Deliver(Agent, Car, PickUp)),
% PRECONDITION: In(Agent, PickUp) and In(Car, PickUp),
% EFFECT: Delivered(Car))
poss(deliver(Who, What, Where), S) :-
  in(Who, Where, S),
  in(What, Where, S),
  Where = parkinglot,

% --- Successor state axioms ------------------------------------------
% describe the value of fluent based on the previous situation and the
% action chosen for the plan.
%
% fluent(..., result(A,S)) :- positive; previous-state, not(negative)





% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
