parent(michael, cathy).
parent(melody, cathy).
parent(charles_godon, michael).
parent(hazel, michael).

male(michael).
male(charles_godon).

female(cathy).
female(melody).
female(hazel).

father(X,Y) :- parent(X,Y), male(X).
mother(X,Y) :- parent(X,Y), female(X).

son(X,Y) :- parent(Y,X),male(x).
daughter(X,Y) :- parent(Y,X),female(X).

% we could write it like:
% child(X,Y) :- son(X,Y).
% child(X,Y) :- daughter(X,Y).

% but this is shorter:
child(X,Y) :- parent(Y,X).
