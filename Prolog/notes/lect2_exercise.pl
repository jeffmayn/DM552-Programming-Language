located_in(atlanta,georgia).                      % true
located_in(atlanta,texas).                        % false (because we ruled that atlanta is in geordia)
located_in(austin,texas).
located_in(toronto,ontario).
located_in(X,usa) :- located_in(X,georgia).
located_in(X,usa) :- located_in(X,texas).
located_in(X,canada) :- located_in(X,ontario).
