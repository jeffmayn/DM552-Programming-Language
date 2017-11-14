l = [1,2,3].
member(X,[X|l]).
member(X,[Y|l]) :- member(X,l).
