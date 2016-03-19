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

:- multifile in/3, connected/2, car/1, parked/2, delivered/2, dirty/2, key/2, isHoldingKey/4, stored/3.



% --- Primitive control actions ---------------------------------------
% this section defines the name and the number of parameters of the
% actions available to the planner
%
% primitive_action( dosomething(_,_) ).	% underscore means `anything'

primitive_action( move(_,_,_) ).
primitive_action( park(_,_) ).
primitive_action( drive(_,_,_,_,_) ).
primitive_action( deliver(_,_) ).
primitive_action( clean(_,_) ).
primitive_action( grab(_,_,_) ).
primitive_action( store(_,_,_) ).

% --- Precondition for primitive actions ------------------------------
% describe when an action can be carried out, in a generic situation S
%
% poss( doSomething(...), S ) :- preconditions(..., S).

% Action(Move(Agent, from, to),
% PRECONDITION: In(Agent, from) and Connected(to, from),
% EFFECT: In(Agent, to) and (not In(Agent, from))
poss(move(Who, From, To), S) :-
  Who = agent,
  in(Who, From, S),
  connected(To, From).

% Action(Park(Agent, Car, ParkingLot)),
% PRECONDITION: In(Agent, ParkingLot) and In(Car, ParkingLot) and (not Parked(Car)),
% EFFECT: Parked(Car))
poss(park(Who, What), S) :-
  Who = agent,
  car(What),
  not(parked(What, S)),
  in(Who, parkingLot, S),
  in(What, parkingLot, S).

% Action(Drive(Agent, Car, from, to)),
% PRECONDITION: In(Agent, from) and In(Car, from) and Connected(to, from),
% EFFECT: In(Agent, to) and In(Car, from) and (not Parked(Car))
poss(drive(Who, Car, Key, From, To), S) :-
  Who = agent,
  car(Car),
  in(Who, From, S),
  in(Car, From, S),
  connected(To, From),
  key(Key, Car),
  isHoldingKey(Who, Key, Car, S).

% Action(Deliver(Agent, Car, PickUp)),
% PRECONDITION: In(Agent, PickUp) and In(Car, PickUp),
% EFFECT: Delivered(Car))
poss(deliver(Who, What), S) :-
  Who = agent,
  car(What),
  in(Who, pickUp, S),
  in(What, pickUp, S),
  not(dirty(What, S)).

poss(clean(Who, What), S) :-
  Who = agent,
  car(What),
  in(Who, parkingLot, S),
  in(What, parkingLot, S),
  dirty(What, S).

poss(grab(Who, Key, Car), S) :-
  Who = agent,
  in(Who, parkingLot, S),
  not(isHoldingKey(Who, _, _, S)),
  key(Key, Car),
  car(Car),
  parked(Car, S),
  stored(Key, Car, S).

poss(store(Who, Key, Car), S) :-
  Who = agent,
  in(Who, parkingLot, S),
  isHoldingKey(Who, Key, Car, S),
  key(Key, Car),
  car(Car),
  parked(Car, S).

% --- Successor state axioms ------------------------------------------
% describe the value of fluent based on the previous situation and the
% action chosen for the plan.
%
% fluent(..., result(A,S)) :- positive; previous-state, not(negative)

parked(Car, result(A,S)) :-
  A = park(_, Car);
  parked(Car, S), not(A = drive(_, Car, _, _, _)).

% Poss(a, s) => (Delivered(Car, Result(a,s)) <=>
% (a = Deliver(Agent, Car, PickUp) and In(Agent, PickUp) and In(Car, PickUp))
% Delivered(Car, s))
delivered(Car, result(A,S)) :-
  A = deliver(_, Car);
  delivered(Car, S).

in(What, P, result(A,S)) :-
  % in(agent, P, S), in(car, P, S), not(A = move(agent, P, To)), not(A = drive(agent, car, P, To)), connected(To, P);
  % in(agent, From, S), in(car, From, S), A = drive(agent, car, From, P), connected(P, From);
  % in(agent, From, S), A = move(agent, From, P), connected(P, From).
  %
  (What = agent,
    (A = move(What, _, P);
    A = drive(What, _, _, _, P);
    in(What, P, S), not(A = move(What, _, _)), not(A = drive(What, _, _, _, _))
    )
  );
  (car(What),
    (A = drive(_, What, _, _, P);
    in(What, P, S), not(A = drive(_, What, _, _, _))
    )
  ).

dirty(Car, result(A,S)) :-
  A = park(_, Car);
  dirty(Car, S), not(A = clean(_, Car)).

isHoldingKey(Who, Key, Car, result(A,S)) :-
  A = grab(Who, Key, Car);
  isHoldingKey(Who, Key, Car, S), not(A = store(Who, Key, Car)), not(A = deliver(Who, Car)).

stored(Key, Car, result(A,S)) :-
  A = store(_, Key, Car);
  stored(Key, Car, S), not(A = grab(_, Key, Car)).
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
