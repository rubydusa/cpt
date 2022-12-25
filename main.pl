?- use_module(library(clpfd)).
?- use_module(library(pairs)).

named_var(Name - Var) :-
	put_attr(Var, name, Name).

force_named_var(Var, _) :-
	get_attr(Var, name, _).
force_named_var(Var, Default) :-
	\+ get_attr(Var, name, _),
	put_attr(Var, name, Default).

force_named_vars_([], _).
force_named_vars_([Var | Rest], Default) :-
	NextDefault is Default + 1,
	force_named_var(Var, a(Default)),
	force_named_vars_(Rest, NextDefault).
force_named_vars(Vars) :-
	force_named_vars_(Vars, 0).

/*
  on the assumption the equation being passed is comprised only of operands and clpfd variables,
  no need to worry of infinite recursion since A = (B #= C) throws an error (unifcation with terms is impossible with clpfd variables)
*/

named_equation(Var, VarName) :-
	get_attr(Var, name, VarName).
named_equation(Var, Var) :-
	integer(Var).
named_equation(A #= B, C #= D) :-
	named_equation(A, C),
	named_equation(B, D).
named_equation(A + B, C + D) :-
	named_equation(A, C),
	named_equation(B, D).
named_equation(A - B, C - D) :-
	named_equation(A, C),
	named_equation(B, D).
named_equation(A * B, C * D) :-
	named_eqaution(A, C),
	named_equation(B, D).

equations(NamedVariables, Equations) :-
	maplist(named_var, NamedVariables),
	pair_values(NamedVariables, Variables),
	term_attvars(Variables, AllVariables),
	forced_named_vars(AllVariables).