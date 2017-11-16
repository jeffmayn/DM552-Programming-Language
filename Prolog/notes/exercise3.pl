% returns the bigger number of X,Y
maxTwo(X,Y,Res) :- Res is max(X,Y).

% returns the bigger number of X,Y,Z
maxThree(X,Y,Z,Res) :- X1 is max(X,Y), Res is max(X1,Z).

% sums two numbers by e.g sumTwo(10,20,Res).
sumTwo(X,Y,Res) :- Res is X + Y.

% check for even numbers
isEven(X) :- R1 is mod(X,2), R1 is 0.

% find factorial number
nFactorial(0,1) :- !.
nFactorial(X,Res) :- P is (X-1), nFactorial(P,S), Res is (X*S).

% find first element in list, e.g. firstL([1,2,3], Res).
firstL([Res |_],Res).

% last element in list, e.g. lastL([1,2,3], Res).
lastL([Res|[]], Res) :- !.
lastL([_|T],Res) :- lastL(T,Res).

% find element in list, e.g. findL([1,2,3,4,5],3)
findL([X|_],X) :- !.
findL([_|T],X) :- findL(T,X).
