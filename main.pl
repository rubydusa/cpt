?- use_module(library(clpfd)).
?- use_module(library(pairs)).

/*
 * glossary of terms and their meanings:
 *
 * default(X) - a placeholder for a name for an unnamed variables
 * expression(X, Y) - X is an arithmetic expression (includes equations) that has variables Y
 * equation(X, Y) - X is an arithmetic equation that has variables Y
 * signal(X, Y) - X is a signal modifier (private_input, public_input, intermediate, output)
 *                Y is the signal name
*/

/*
 * In order to replace variables with corresponding ground terms,
 * Variables get attributes representing their "name".
 * The name should include the signal modifier and the signal name in the form signal(X, Y).
 * Check not done here intentionally in order to make it general
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
 * On the assumption the equation being passed is comprised only of valid operands and clpfd variables,
 * no need to worry of infinite recursion since A = (B #= C) throws an error when A is a clpfd var (unifcation with terms is impossible with clpfd variables)
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

clpfd_equation(clpfd:(_ #= _)).
unwrapped_equation(clpfd:(A #= B), A #= B).
as_equation(expression(A #= B, Variables), equation(A #= B, Variables)).

/*
 * replaces every occurance of a variable in an equation of that variable with a corresponding name
 */
variable_equations(NamedVariables, Equations) :-
	% Assign names to known variables 
	maplist(named_var, NamedVariables),

	% Assign names to all other remaining variables
	pairs_values(NamedVariables, Variables),
	term_attvars(Variables, AllVariables),
	force_named_vars(AllVariables),

	% Filter and format equation constraints
	copy_term(AllVariables, AllVariables, Constraints),
	include(clpfd_equation, Constraints, Equations0),
	maplist(unwrapped_equation, Equations0, Equations1),

	% Replace variables with ground terms
	maplist(named_expression, Equations1, Equations2),
	maplist(as_equation, Equations2, Equations).	
	