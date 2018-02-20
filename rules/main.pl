:- [facts, exprRules].

% Warn if we find `x = true`, `true = x`, `x = false`, or `false = x`.
lint('boolean equality', Loc, 0) :- expr_eq(_, L, _, Loc), expr_false(L, _).
lint('boolean equality', Loc, 0) :- expr_eq(_, L, _, Loc), expr_true(L, _).
lint('boolean equality', Loc, 0) :- expr_eq(_, _, R, Loc), expr_false(R, _).
lint('boolean equality', Loc, 0) :- expr_eq(_, _, R, Loc), expr_true(R, _).

% Warn if we find `if x then true else false` or `if x then false else true`.
lint('redundant if', Loc, 0) :-
	expr_if(_, _, True, False, Loc),
	expr_false(False, _),
	expr_true(True, _).
lint('redundant if', Loc, 0) :-
	expr_if(_, _, False, True, Loc),
	expr_false(False, _),
	expr_true(True, _).
