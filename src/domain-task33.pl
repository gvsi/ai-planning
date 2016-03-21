% ---------------------------------------------------------------------
%  ----- Informatics 2D - 2015/16 - Second Assignment - Planning -----
% ---------------------------------------------------------------------
%
% Write here you matriculation number (only - your name is not needed)
% Matriculation Number: s1448512

% --- Cross-file definitions ------------------------------------------

:- multifile in/3, connected/2, car/1, parked/2, delivered/2, dirty/2, key/2, holdingKey/4, storedInUtilityBox/3, occupied/3, fullParking/1.


% --- Primitive control actions ---------------------------------------

primitive_action( move(_,_,_) ).
primitive_action( park(_,_) ).
primitive_action( drive(_,_,_,_,_) ).
primitive_action( deliver(_,_) ).
primitive_action( clean(_,_) ).
primitive_action( grab(_,_,_,_) ).
primitive_action( store(_,_,_) ).

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
  in(What, parkingLot, S),
  not(fullParking(S)).

poss(drive(Who, Car, Key, From, To), S) :-
  Who = agent,
  car(Car),
  in(Who, From, S),
  in(Car, From, S),
  connected(To, From),
  key(Key, Car),
  holdingKey(Who, Key, Car, S).

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

poss(grab(Who, Key, Car, Where), S) :-
  Who = agent,
  not(holdingKey(Who, _, _, S)), % agent cannot be holding other keys
  key(Key, Car),
  car(Car),
  (
   (parked(Car, S), storedInUtilityBox(Key, Car, S), in(Who, Where, S), Where = parkingLot); % if car is parked, keys are stored in utility box in parkingLot
   (not(storedInUtilityBox(Key, Car, S)), in(Who, Where, S), in(Car, Where, S)) % if car is not parked, keys are in the location of the car
  ).

poss(store(Who, Key, Car), S) :-
  Who = agent,
  in(Who, parkingLot, S), % agent must be in the parking lot, where the utility box is
  holdingKey(Who, Key, Car, S), % agent must be holding the key to store
  key(Key, Car),
  car(Car),
  parked(Car, S).

% --- Successor state axioms ------------------------------------------

parked(Car, result(A,S)) :-
  A = park(_, Car);
  parked(Car, S), not(A = drive(_, Car, _, _, _)).

occupied(ParkingSpace, Car, result(A,S)) :-
  A = park(_, Car), not(occupied(ParkingSpace, _, S)),
  ( % parking spaces fill up in order
   ParkingSpace = pSpace1;
   ParkingSpace = pSpace2, occupied(pSpace1, _, S);
   ParkingSpace = pSpace3, occupied(pSpace1, _, S), occupied(pSpace2, _, S);
   ParkingSpace = pSpace4, occupied(pSpace1, _, S), occupied(pSpace2, _, S), occupied(pSpace3, _, S)
  );
  occupied(ParkingSpace, Car, S), not(A = drive(_, Car, _, _, _)).

delivered(Car, result(A,S)) :-
  A = deliver(_, Car);
  delivered(Car, S).

in(Who, P, result(A,S)) :-
  (Who = agent, % if the subject is the agent:
    (A = move(Who, _, P); % if it moves to P
     A = drive(Who, _, _, _, P); % if it drives a car to P
     in(Who, P, S), not(A = move(Who, _, _)), not(A = drive(Who, _, _, _, _)) % if it was in P and didn't move or drive elsewhere
    )
  );
  (car(Who), % if the subject is a car
    (A = drive(_, Who, _, _, P);  % if it was driven to P
    in(Who, P, S), not(A = drive(_, Who, _, _, _)) % if it was in P and it wasn't driven away
    )
  ).

dirty(Car, result(A,S)) :-
  A = park(_, Car); % when car is parked, it becomes dirty
  dirty(Car, S), not(A = clean(_, Car)). % if it was dirty and it hasn't been cleaned

holdingKey(Who, Key, Car, result(A,S)) :-
  A = grab(Who, Key, Car, _); % if agent just grabbed the key
  holdingKey(Who, Key, Car, S), not(A = store(Who, Key, Car)), not(A = deliver(Who, Car)). % if agent was holding key already and didn't store them or left them in a delivered car

storedInUtilityBox(Key, Car, result(A,S)) :-
  A = store(_, Key, Car); % if agent just stored the keys
  storedInUtilityBox(Key, Car, S), not(A = grab(_, Key, Car)). % if keys were already stored and agent didn't grab them

fullParking(result(A,S)) :-
  A = park(_, _),
  ( % if only one of the four spaces wasn't occupied
   (not(occupied(pSpace1, _, S)), occupied(pSpace2, _, S), occupied(pSpace3, _, S), occupied(pSpace4, _, S));
   (occupied(pSpace1, _, S), not(occupied(pSpace2, _, S)), occupied(pSpace3, _, S), occupied(pSpace4, _, S));
   (occupied(pSpace1, _, S), occupied(pSpace2, _, S), not(occupied(pSpace3, _, S)), occupied(pSpace4, _, S));
   (occupied(pSpace1, _, S), occupied(pSpace2, _, S), occupied(pSpace3, _, S), not(occupied(pSpace4, _, S)))
  );
  occupied(pSpace1, SomeCar1, S), occupied(pSpace2, SomeCar2, S), occupied(pSpace3, SomeCar3, S), occupied(pSpace4, SomeCar4, S),
  not(A = drive(_, SomeCar1, _, _, _)), not(A = drive(_, SomeCar2, _, _, _)), not(A = drive(_, SomeCar3, _, _, _)), not(A = drive(_, SomeCar4, _, _, _)). % parking is full and cars don't get driven away
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
