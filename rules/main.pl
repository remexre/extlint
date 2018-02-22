:- [facts, exprRules].

% The lint/3 predicate is:
%
% lint(+Name, -LocId, +Points).
%
% The Points number is the number of points *docked* if found. So don't make it
% a negative number.

% Dock a point if we find `x = true`, `true = x`, `x = false`, or `false = x`.
lint('boolean equality', Loc, 1) :- expr_eq(_, L, _, Loc), expr_false(L, _).
lint('boolean equality', Loc, 1) :- expr_eq(_, L, _, Loc), expr_true(L, _).
lint('boolean equality', Loc, 1) :- expr_eq(_, _, R, Loc), expr_false(R, _).
lint('boolean equality', Loc, 1) :- expr_eq(_, _, R, Loc), expr_true(R, _).

% Dock a point  if we find `if x then true else false` or
% `if x then false else true`.
lint('redundant if', Loc, 1) :-
	expr_if(_, _, Then, Else, Loc),
	expr_true(Then, _),
	expr_false(Else, _).
lint('redundant if', Loc, 1) :-
	expr_if(_, _, Then, Else, Loc),
	expr_false(Then, _),
	expr_true(Else, _).

% Warn about `==` and `!=`.
lint('pointer equality', Loc, 0) :- expr_ident(_, '==', Loc).
lint('pointer equality', Loc, 0) :- expr_ident(_, '!=', Loc).

% Dock 2 points for `let x = y in x`.
lint('useless let', Loc, 2) :-
	expr_let_name(_, _, Name, _, Body, Loc),
	expr_ident(Body, Name, _).

% Dock a point for List.hd or List.tl.
lint('List.hd', Loc, 1) :- expr_mod_ident(_, 'List', 'hd', Loc).
lint('List.tl', Loc, 1) :- expr_mod_ident(_, 'List', 'tl', Loc).

% Warn about if-then without an else.
lint('if-then without an else', Loc, 0) :- expr_if(_, _, _, Loc).
