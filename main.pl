?- use_module(library(clpfd)).
?- use_module(library(pairs)).

/*
 * glossary of terms and their meanings:
 *
 * default(X) - a placeholder for a name for an unnamed variables
 * expression(X, Y) - X is an arithmetic expression (includes equations) that has variables Y
 * signal(X, Y) - X is a signal modifier (private_input, public_input, intermediate, output)
 *                Y is the signal name
 */

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
	force_named_var(Var, default(Default)),
	force_named_vars_(Rest, NextDefault).
force_named_vars(Vars) :-
	force_named_vars_(Vars, 0).

/*
 * on the assumption the equation being passed is comprised only of operands and clpfd variables,
 * no need to worry of infinite recursion since A = (B #= C) throws an error (unifcation with terms is impossible with clpfd variables)
 */

named_expression(Var, expression(VarName, [VarName])) :-
	get_attr(Var, name, VarName).
named_expression(Var, expression(Var, [])) :-
	integer(Var).
named_expression(A #= B, expression(C #= D, Variables)) :-
	named_expression(A, expression(C, Variables0)),
	named_expression(B, expression(D, Variables1)),
	append(Variables0, Variables1, Variables).
named_expression(A + B, expression(C + D, Variables)) :-
	named_expression(A, expression(C, Variables0)),
	named_expression(B, expression(D, Variables1)),
	append(Variables0, Variables1, Variables).
named_expression(A - B, expression(C - D, Variables)) :-
	named_expression(A, expression(C, Variables0)),
	named_expression(B, expression(D, Variables1)),
	append(Variables0, Variables1, Variables).
named_expression(A * B, expression(C * D, Variables)) :-
	named_expression(A, expression(C, Variables0)),
	named_expression(B, expression(D, Variables1)),
	append(Variables0, Variables1, Variables).

equations(NamedVariables, Equations) :-
	maplist(named_var, NamedVariables),
	pair_values(NamedVariables, Variables),
	term_attvars(Variables, AllVariables),
	forced_named_vars(AllVariables).