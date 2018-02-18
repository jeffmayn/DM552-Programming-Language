:- use_module(library(clpb)).

% F1
showSAT(X1,X2,X3,X4,X5) :-
  sat((~X1)*(~X2 + X3 + X1)*(~X1 + ~X2)*(X1 + X2 + ~X4)*(~X4 + X3)*(~X3 + ~X5)),
  labeling([X1,X2,X3,X4,X5]).

% F2
showUNSAT(X,X) :-
  sat((X)*(~X)),
  labeling([X,X]).
