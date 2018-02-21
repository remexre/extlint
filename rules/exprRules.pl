% Boolean "Literals"
expr_false(ExprId, LocId) :-
	'Expression'(ExprId, DescId, LocId, _Attrs),
	'ExpressionDesc'(DescId, 'Construct', CtorLocId, _),
	'Loc'(CtorLocId, CtorIdentId, _),
	'LongIdent'(CtorIdentId, 'Ident', false).
expr_true(ExprId, LocId) :-
	'Expression'(ExprId, DescId, LocId, _Attrs),
	'ExpressionDesc'(DescId, 'Construct', CtorLocId, _),
	'Loc'(CtorLocId, CtorIdentId, _),
	'LongIdent'(CtorIdentId, 'Ident', true).

% If Expressions
expr_if(ExprId, CondId, ThenId, LocId) :-
	'Expression'(ExprId, IfDescId, LocId, _),
	'ExpressionDesc'(IfDescId, 'IfThenElse', CondId, ThenId, ElseOptionId),
	'Option'(ElseOptionId, 'None').
expr_if(ExprId, CondId, ThenId, ElseId, LocId) :-
	'Expression'(ExprId, IfDescId, LocId, _),
	'ExpressionDesc'(IfDescId, 'IfThenElse', CondId, ThenId, ElseOptionId),
	'Option'(ElseOptionId, 'Some', ElseId).

% Name Patterns
pat_name(PatId, Name) :-
	'Pattern'(PatId, DescId, _, _),
	'PatternDesc'(DescId, 'Var', LocId),
	'Loc'(LocId, Name, _).

% Let Expressions
let_binding(Bindings, PatternId, BoundId) :-
	cons(Bindings, BindingId, _),
	'ValueBinding'(BindingId, PatternId, BoundId, _, _).
let_binding(Bindings, PatternId, BoundId) :-
	cons(Bindings, _, Tail),
	let_binding(Tail, PatternId, BoundId).
expr_let_name(ExprId, Rec, Name, BoundId, BodyId, LocId) :-
	expr_let(ExprId, Rec, PatternId, BoundId, BodyId, LocId),
	pat_name(PatternId, Name).
expr_let(ExprId, Rec, PatternId, BoundId, BodyId, LocId) :-
	'Expression'(ExprId, LetDescId, LocId, _),
	'ExpressionDesc'(LetDescId, 'Let', Rec, Bindings, BodyId),
	let_binding(Bindings, PatternId, BoundId).

% Identifiers
expr_ident(ExprId, Name, LocId) :-
	'Expression'(ExprId, IdentId, LocId, _),
	'ExpressionDesc'(IdentId, 'Ident', IdentLocId),
	'Loc'(IdentLocId, LongIdentId, _),
	'LongIdent'(LongIdentId, 'Ident', Name).

% Equals Calls
expr_eq(ExprId, LeftId, RightId, LocId) :-
	'Expression'(ExprId, ApId, LocId, _),
	'ExpressionDesc'(ApId, 'Apply', EqId, Args0Id),
	expr_ident(EqId, '=', _),
	cons(Args0Id, _, LeftId, Args1Id),
	cons(Args1Id, _, RightId, Args2Id),
	nil(Args2Id).
