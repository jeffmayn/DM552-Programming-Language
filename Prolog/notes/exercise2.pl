% Exercise 01:
% ------------------------------------
person('John').
person('Bill').

student('John').

smart(X) :- student(X).

% Exercise 02:
% ------------------------------------









% Exercise 03:
% ------------------------------------------------------

% 1.---------------
loves(john,mary).                       % VALID
% Mary.                                 % NOT VALID
% _c1.                                  % NOT VALID
'Hello'.                                % VALID

% 2.--------------
a.                                      % VALID
% A.                                    % NOT VALID
% Paul.                                 % NOT VALID
% 'Hello'.                              % VALID
a_123.                                  % VALID
% _.                                    % NOT VALID
% _abc.                                 % NOT VALID
x2.                                     % VALID

% 3.--------------
