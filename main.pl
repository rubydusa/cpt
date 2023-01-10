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

forced_named_var(Var, _) :-
	get_attr(Var, name, _).
forced_named_var(Var, Default) :-
	\+ get_attr(Var, name, _),
	put_attr(Var, name, Default).

forced_named_vars_([], _).
forced_named_vars_([Var | Rest], Default) :-
	NextDefault is Default + 1,
	forced_named_var(Var, default(Default)),
	forced_named_vars_(Rest, NextDefault).
forced_named_vars(Vars) :-
	forced_named_vars_(Vars, 0).

ast(Expr, t(LeafType, Name, Args)) :-
	catch(
	      (
	       LeafType=op,
	       compound_name_arguments(Expr, Name, Args0),
	       maplist(ast, Args0, Args)
	      ),
	      Error,
	      ast_error_(Expr, LeafType, Name, Args, Error)
	     ).

ast_error_(Expr, non_op, variable, [Expr], error(instantiation_error, _)).
ast_error_(Expr, non_op, atom, [Expr], error(type_error(compound, Expr), _)).

% safe operations are always permitable
safe_operation(+).
safe_operation(*).
safe_operation(-).

named_expression_(t(non_op, variable, [Var]), expression(t(non_op, atom, [VarName]), []), _) :-
	get_attr(Var, name, VarName).
named_expression_(t(non_op, atom, [Int]), expression(t(non_op, atom, [Int]), []), _) :-
	integer(Int).
named_expression_(t(op, #=, [RHS0, LHS0]), expression(t(op, #=, [RHS, LHS]), Vars), root) :-
	named_expression_(RHS0, expression(RHS, RHSVars), non_root),
	named_expression_(LHS0, expression(LHS, LHSVars), non_root),
	append(RHSVars, LHSVars, Vars).
named_expression_(t(op, Op, [RHS0, LHS0]), expression(t(op, Op, [RHS, LHS]), Vars), non_root) :-
	safe_operation(Op),
	named_expression_(RHS0, expression(RHS, RHSVars), non_root),
	named_expression_(LHS0, expression(LHS, LHSVars), non_root),
	append(RHSVars, LHSVars, Vars).

named_expression(Tree, ExpressionTree) :-
	named_expression_(Tree, ExpressionTree, root).

/*
 * helpers
 */
clpfd_constraint(clpfd:(_)).
clpfd_equation(clpfd:(_ #= _)).
unwrapped_equation(clpfd:(A #= B), A #= B).

/*
 * replaces every occurance of a variable in an equation of that variable with a corresponding name
 */
variable_equations(NamedVariables, Equations) :-
	% Assign names to known variables 
	maplist(named_var, NamedVariables),

	% Assign names to all other remaining variables
	pairs_values(NamedVariables, Variables),
	term_attvars(Variables, AllVariables),
	forced_named_vars(AllVariables),

	% Filter and format equation constraints
	copy_term(AllVariables, AllVariables, Constraints),
	include(clpfd_constraint, Constraints, CLPFDConstraints),
	maplist(clpfd_equation, CLPFDConstraints),
	maplist(unwrapped_equation, CLPFDConstraints, Equations0),

	maplist(ast, Equations0, Equations1),
	maplist(named_expression, Equations1, Equations).

	