% The lint/4 predicate is:
%
% lint(+Name, ?Points, -LocId, -AuxData).
%
% The Points number is the number of points *docked* if found. So don't make it
% a negative number.
%
% The AuxData value is passed to long_desc/4.

% Dock a point if we find `x = true`, `true = x`, `x = false`, or `false = x`.
lint('equality with false', 1, Loc, Expr) :- expr_eq(_, L, Expr, Loc), expr_false(L, _).
lint('equality with true', 1, Loc, Expr) :- expr_eq(_, L, Expr, Loc), expr_true(L, _).
lint('equality with false', 1, Loc, Expr) :- expr_eq(_, Expr, R, Loc), expr_false(R, _).
lint('equality with true', 1, Loc, Expr) :- expr_eq(_, Expr, R, Loc), expr_true(R, _).

% Dock a point if we find `if x then true else false` or
% `if x then false else true`.
lint('redundant if', 1, Loc, Cond) :-
	expr_if(_, Cond, Then, Else, Loc),
	expr_true(Then, _),
	expr_false(Else, _).
lint('redundant if', 1, Loc, Cond) :-
	expr_if(_, Cond, Then, Else, Loc),
	expr_false(Then, _),
	expr_true(Else, _).

% Warn about `==` and `!=`.
lint('pointer equality', 0, Loc, '=') :- expr_ident(_, '==', Loc).
lint('pointer equality', 0, Loc, '<>') :- expr_ident(_, '!=', Loc).

% Dock 2 points for `let x = y in x`.
lint('useless let', 2, Loc, [Name, Bound]) :-
	expr_let_name(_, _, Name, Bound, Body, Loc),
	expr_ident(Body, Name, _).

% Dock a point for List.hd or List.tl.
lint('partial list function', 1, Loc, null) :- expr_mod_ident(_, 'List', 'hd', Loc).
lint('partial list function', 1, Loc, null) :- expr_mod_ident(_, 'List', 'tl', Loc).

% Warn about if-then without an else.
lint('if-then without an else', 0, Loc, null) :- expr_if(_, _, _, Loc).

% The long_desc/4 predicate is:
%
% long_desc(+Name, +LocId, +AuxData, -Description).
%
% LocId and AuxData are the corresponding parameters from the lint/4 predicate.
%
% Description is the Markdown text to print.

long_desc('equality with false', Loc, Expr, Desc) :-
	loc_text(Loc, BadText),
	expr_text(Expr, ExprText),
	swritef(Desc, "Instead of `%w`, try writing `not (%w)`.",
		[BadText, ExprText]).
long_desc('equality with true', Loc, Expr, Desc) :-
	loc_text(Loc, BadText),
	expr_text(Expr, ExprText),
	swritef(Desc, "Instead of `%w`, try writing `%w`.",
		[BadText, ExprText]).
long_desc('redundant if', Loc, Cond, Desc) :-
	loc_text(Loc, BadText),
	expr_text(Cond, CondText),
	swritef(Desc, "Instead of `%w`, try writing `%w`.",
		[BadText, CondText]).
long_desc('pointer equality', Loc, Oper, Desc) :-
	loc_text(Loc, BadText),
	swritef(Desc, "`%w` is pointer comparison -- you might want `%w` instead.",
		[BadText, Oper]).
long_desc('useless let', _, [Name, Bound], Desc) :-
	expr_text(Bound, BoundText),
	swritef(Desc, "The `%w` variable is useless -- just write `%w`.",
		[Name, BoundText]).
long_desc('partial list function', Loc, _, Desc) :-
	loc_text(Loc, Fn),
	swritef(Desc, "Instead of the `%w` function, use pattern matching.",
		[Fn]).
long_desc('if-then without an else', _, _, Desc) :-
	swritef(Desc, "You probably don't want to have an if-then without an else.").
