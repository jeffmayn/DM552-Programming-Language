person('Bill').
person('Bob').
person('Alice').
person('John').
person('Phyllis').
person('Phartiphuckboriz').

unknown('Phartiphuckboriz').

male('Bill').
male('Bob').
male('John').

female('Phyllis').
female('Alice').

student('Bill').

parent('Bob', 'Bill').
parent('Alice', 'Bob').

smart(X) :- student(X).
smart(X) :- person(X).

grandparent(X,Y) :- parent(Z, Y), parent(X, Z).
grandmother(X,Y) :- grandparent(X,Y), female(X).
grandfather(X,Y) :- grandparent(X,Y), male(X).
