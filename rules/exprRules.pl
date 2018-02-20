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

expr_eq(ExprId, LeftId, RightId, LocId) :-
	'Expression'(ExprId, ApId, LocId, _),
	'ExpressionDesc'(ApId, 'Apply', EqId, Args0Id),
	'Expression'(EqId, EqDescId, _, _),
	'ExpressionDesc'(EqDescId, 'Ident', EqLocId),
	'Loc'(EqLocId, EqIdentId, _),
	'LongIdent'(EqIdentId, 'Ident', '='),
	cons(Args0Id, _LeftLabel, LeftId, Args1Id),
	cons(Args1Id, _RightLabel, RightId, Args2Id),
	nil(Args2Id).

expr_if(ExprId, CondId, ThenId, LocId) :-
	'Expression'(ExprId, IfDescId, LocId, _),
	'ExpressionDesc'(IfDescId, 'IfThenElse', CondId, ThenId, ElseOptionId),
	'Option'(ElseOptionId, 'None').
expr_if(ExprId, CondId, ThenId, ElseId, LocId) :-
	'Expression'(ExprId, IfDescId, LocId, _),
	'ExpressionDesc'(IfDescId, 'IfThenElse', CondId, ThenId, ElseOptionId),
	'Option'(ElseOptionId, 'Some', ElseId).
