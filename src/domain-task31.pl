% ---------------------------------------------------------------------
%  ----- Informatics 2D - 2015/16 - Second Assignment - Planning -----
% ---------------------------------------------------------------------
%
% Write here you matriculation number (only - your name is not needed)
% Matriculation Number: s1448512

% --- Cross-file definitions ------------------------------------------

:- multifile in/3, connected/2, car/1, parked/2, delivered/2, dirty/2.

% --- Primitive control actions ---------------------------------------

primitive_action( move(_,_,_) ).
primitive_action( park(_,_) ).
primitive_action( drive(_,_,_,_) ).
primitive_action( deliver(_,_) ).
primitive_action( clean(_,_) ).

% --- Precondition for primitive actions ------------------------------

poss(move(Who, From, To), S) :-
  Who = agent,
  in(Who, From, S),
  connected(To, From).

poss(park(Who, What), S) :-
  Who = agent,
  car(What),
  not(parked(What, S)),
  in(Who, parkingLot, S),
  in(What, parkingLot, S).

poss(drive(Who, What, From, To), S) :-
  Who = agent,
  car(What),
  in(Who, From, S),
  in(What, From, S),
  connected(To, From).

poss(deliver(Who, What), S) :-
  Who = agent,
  car(What),
  in(Who, pickUp, S),
  in(What, pickUp, S),
  not(dirty(What, S)). % only deliver if not dirty

poss(clean(Who, What), S) :-
  Who = agent,
  car(What),
  in(Who, parkingLot, S), % only clean if agent and car are in parking lot
  in(What, parkingLot, S),
  dirty(What, S).

% --- Successor state axioms ------------------------------------------

parked(Car, result(A,S)) :-
  A = park(_, Car);
  parked(Car, S), not(A = drive(_, Car, _, _)).

delivered(Car, result(A,S)) :-
  A = deliver(_, Car);
  delivered(Car, S).

in(Who, P, result(A,S)) :-
  (Who = agent, % if the subject is the agent:
    (A = move(Who, _, P); % if it moves to P
     A = drive(Who, _, _, P); % if it drives a car to P
     (in(Who, P, S), not(A = move(Who, _, _)), not(A = drive(Who, _, _, _))) % if it was in P and didn't move or drive elsewhere
    )
  );
  (car(Who), % if the subject is a car
    (A = drive(_, Who, _, P);  % if it was driven to P
    in(Who, P, S), not(A = drive(_, Who, _, _))) % if it was in P and it wasn't driven away
  ).

dirty(Car, result(A,S)) :-
  A = park(_, Car); % when car is parked, it becomes dirty
  dirty(Car, S), not(A = clean(_, Car)). % if it was dirty and it hasn't been cleaned

% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
