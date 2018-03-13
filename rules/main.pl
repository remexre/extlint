:- [rules, lints].
:- use_module(library(http/json)).

generateLint(Lint) :-
	lint(Name, Points, LocId, AuxData),
	long_desc(Name, LocId, AuxData, Desc),
	'Location'(LocId, StartId, EndId, _),
	'Position'(StartId, _, StartLine, StartCol, _),
	'Position'(EndId, _, EndLine, EndCol, _),
	Lint = ''{desc: Desc, name: Name, points_lost: Points,
	          start: [StartLine, StartCol], end: [EndLine, EndCol]}.

extlintMain(OutPath) :-
	!,
	findall(Lint, generateLint(Lint), Lints),
	open(OutPath, write, OutFile),
	src(0, Path, Src),
	json_write(OutFile, ''{lints: Lints, path: Path, src: Src}).
