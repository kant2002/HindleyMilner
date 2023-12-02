using System.Diagnostics;

// Basic types are constructed with a nullary type constructor

var Integer = new TypeOperator("int", new List<object> { }); // Basic integer
var Bool = new TypeOperator("bool", new List<object> { }); // Basic bool

/*
    The main example program.

    Sets up some predefined types using the type constructors TypeVariable,
    TypeOperator and Function.  Creates a list of example expressions to be
    evaluated. Evaluates the expressions, printing the type or errors arising
    from each.
*/
var var1 = new TypeVariable();

var var2 = new TypeVariable();

var pair_type = new TypeOperator("*", new List<object> { var1, var2 });
var var3 = new TypeVariable();

var my_env = new Dictionary<string, object>
{
	{ "pair", new Function(var1, new Function(var2, pair_type)) },
	{ "true", Bool },
	{ "cond", new Function(Bool, new Function(var3, new Function(var3, var3)))},
	{ "zero", new Function(Integer, Bool)},
	{ "pred", new Function(Integer, Integer)},
	{ "times", new Function(Integer, new Function(Integer, Integer)) },
};

var pair = new Apply(new Apply(new Identifier("pair"),
								new Apply(new Identifier("f"),
											new Identifier("4"))),
				 new Apply(new Identifier("f"),
							new Identifier("true")));
var examples = new object[]
{
	// factorial
        new Letrec("factorial",  // letrec factorial =
               new Lambda("n",  // fn n =>
                      new Apply(
						  new Apply(  // cond (zero n) 1
                              new Apply(new Identifier("cond"),  // cond (zero n)
                                    new Apply(new Identifier("zero"), new Identifier("n"))),
							  new Identifier("1")),
						  new Apply(  // times n
                              new Apply(new Identifier("times"), new Identifier("n")),
							  new Apply(new Identifier("factorial"),
									new Apply(new Identifier("pred"), new Identifier("n")))
						  )
					  )
					  ),  // in
               new Apply(new Identifier("factorial"), new Identifier("5"))
			   ),

	// Should fail:
    // fn x => (pair(x(3) (x(true)))
    new Lambda("x",
			new Apply(
				new Apply(new Identifier("pair"),
						new Apply(new Identifier("x"), new Identifier("3"))),
				new Apply(new Identifier("x"), new Identifier("true")))),

    // pair(f(3), f(true))
    new Apply(
		new Apply(new Identifier("pair"), new Apply(new Identifier("f"), new Identifier("4"))),
		new Apply(new Identifier("f"), new Identifier("true"))),

    // let f = (fn x => x) in ((pair (f 4)) (f true))
    new Let("f", new Lambda("x", new Identifier("x")), pair),

    // fn f => f f (fail)
    new Lambda("f", new Apply(new Identifier("f"), new Identifier("f"))),

    // let g = fn f => 5 in g g
    new Let("g",
		new Lambda("f", new Identifier("5")),
		new Apply(new Identifier("g"), new Identifier("g"))),

    // example that demonstrates generic and non-generic variables:
    // fn g => let f = fn x => g in pair (f 3, f true)
    new Lambda("g",
			new Let("f",
				new Lambda("x", new Identifier("g")),
				new Apply(
					new Apply(new Identifier("pair"),
							new Apply(new Identifier("f"), new Identifier("3"))
							),
					new Apply(new Identifier("f"), new Identifier("true"))))),

    // Function composition
    // fn f (fn g (fn arg (f g arg)))
    new Lambda("f", new Lambda("g", new Lambda("arg", new Apply(new Identifier("g"), new Apply(new Identifier("f"), new Identifier("arg"))))))
};
foreach (var example in examples)
	try_exp(my_env, example);


object analyse(object node, Dictionary<string, object> env, HashSet<object>? non_generic = null)
{
	if (non_generic is null)
	{
		non_generic = new();
	}

	if (node is Identifier ident)
	{
		return get_type(ident.name, env, non_generic);
	}
	else if (node is Apply apply)
	{
		var fun_type = analyse(apply.fn, env, non_generic);
		var arg_type = analyse(apply.arg, env, non_generic);
		var result_type = new TypeVariable();

		unify(new Function(arg_type, result_type), fun_type);

		return result_type;
	}
	else if (node is Lambda lambda)
	{
		var arg_type = new TypeVariable();
		Dictionary<string, object> new_env = new(env);
		new_env[lambda.variableName] = arg_type;
		HashSet<object> new_non_generic = new(non_generic)
		{
			arg_type
		};

		var result_type = analyse(lambda.body, new_env, new_non_generic);

		return new Function(arg_type, result_type);
	}
	else if (node is Let let)
	{
		var defn_type = analyse(let.defn, env, non_generic);

		Dictionary<string, object> new_env = new(env);

		new_env[let.v] = defn_type;

		return analyse(let.body, new_env, non_generic);
	}
	else if (node is Letrec letrec)
	{
		var new_type = new TypeVariable();

		Dictionary<string, object> new_env = new(env);

		new_env[letrec.v] = new_type;

		HashSet<object> new_non_generic = new(non_generic)
		{
			new_type
		};

		var defn_type = analyse(letrec.defn, new_env, new_non_generic);

		unify(new_type, defn_type);

		return analyse(letrec.body, new_env, non_generic);
	}

	throw new InvalidOperationException(string.Format("Unhandled syntax node {0}", node.GetType()));
}

object get_type(string name, Dictionary<string, object> env, HashSet<object> non_generic)
{
	if (env.ContainsKey(name))
		return fresh(env[name], non_generic);
	else if (is_integer_literal(name))
		return Integer;
	else
		throw new ParseError(string.Format("Undefined symbol {0}", name));
}

/// <summary>Makes a copy of a type expression.
/// The type t is copied. The the generic variables are duplicated and the
/// non_generic variables are shared.
/// </summary>
/// <param name="t">A type to be copied.</param>
/// <param name="non_generic">A set of non-generic TypeVariables</param>
object fresh(object t, HashSet<object> non_generic)
{
	Dictionary<TypeVariable, TypeVariable> mappings = new(); // A mapping of TypeVariables to TypeVariables
	return freshrec(t);

	object freshrec(object tp)
	{
		var p = prune(tp);

		if (p is TypeVariable tvp)
		{
			if (is_generic(tvp, non_generic))
			{
				if (!mappings.ContainsKey(tvp))
					mappings[tvp] = new TypeVariable();

				return mappings[tvp];
			}
			else
				return p;
		}
		else if (p is TypeOperator top)
			return new TypeOperator(top.Name, top.Types.Select(x => freshrec(x)).ToList());
		else
		{
			Debug.Assert(false, "Not unified");
			throw new InvalidOperationException();
		}
	}
}

/// <summary>Unify the two types t1 and t2.
/// Makes the types t1 and t2 the same.
/// </summary>
/// <param name="t1">The first type to be made equivalent</param>
/// <param name="t2">The second type to be be equivalent</param>
void unify(object t1, object t2)
{
	var a = prune(t1);
	var b = prune(t2);

	if (a is TypeVariable tva)
	{
		if (a != b)
		{
			if (occurs_in_type(tva, b))
				throw new InferenceError("recursive unification");

			tva.instance = b;
		}
	}
	else if ((a is TypeOperator toa) && (b is TypeVariable tvb))
	{
		unify(b, a);
	}
	else if ((a is TypeOperator toa2) && (b is TypeOperator tob))
	{
		if (toa2.Name != tob.Name || toa2.Types.Count != tob.Types.Count)
		{
			throw new InferenceError(string.Format("Type mismatch: {0} != {1}", a, b));
		}

		foreach ((var p, var q) in toa2.Types.Zip(tob.Types))
		{
			unify(p, q);
		}
	}
	else
	{
		Debug.Assert(false, "Not unified");
	}
}

/// <summary>
/// Returns the currently defining instance of t.
/// As a side effect, collapses the list of type instances. The function Prune
/// is used whenever a type expression has to be inspected: it will always
/// return a type expression which is either an uninstantiated type variable or
/// a type operator; i.e.it will skip instantiated variables, and will
/// actually prune them from expressions to remove long chains of instantiated
/// variables.
/// </summary>
/// <param name="v">The type to be pruned</param>
/// <returns>An uninstantiated TypeVariable or a TypeOperator</returns>
object prune(object t)
{
	if (t is TypeVariable tv)
	{
		if (tv.instance is not null)
		{
			tv.instance = prune(tv.instance);
			return tv.instance;
		}
	}

	return t;
}

/// <summary>Checks whether a given variable occurs in a list of non-generic variables.
/// Note that a variables in such a list may be instantiated to a type term,
/// in which case the variables contained in the type term are considered
/// non - generic.
/// </summary>
/// <remarks>Note: Must be called with v pre-pruned</remarks>
/// <param name="v">The TypeVariable to be tested for genericity</param>
/// <param name="non_generic">A set of non-generic TypeVariables</param>
/// <returns>True if v is a generic variable, otherwise False</returns>
bool is_generic(TypeVariable v, HashSet<object> non_generic)
{
	return !occurs_in(v, non_generic);
}

/// <summary>Checks whether a type variable occurs in a type expression.</summary>
/// <remarks>Note: Must be called with v pre-pruned</remarks>
/// <param name="v">The TypeVariable to be tested for</param>
/// <param name="type2">The type in which to search</param>
/// <returns>True if v occurs in type2, otherwise False</returns>
bool occurs_in_type(TypeVariable v, object type2)
{
	var pruned_type2 = prune(type2);
	if (pruned_type2 == v)

		return true;

	else if (pruned_type2 is TypeOperator pt2)

		return occurs_in(v, pt2.Types);

	return false;
}


/// <summary>Checks whether a types variable occurs in any other types.</summary>
/// <param name="t">The TypeVariable to be tested for</param>
/// <param name="types">The sequence of types in which to search</param>
/// <returns>True if t occurs in any of types, otherwise False</returns>
bool occurs_in(TypeVariable t, IEnumerable<object> types)
{
	return types.Any(t2 => occurs_in_type(t, t2));
}

/// <summary>Checks whether name is an integer literal string.</summary>
/// <param name="name">The identifier to check</param>
/// <returns>True if name is an integer literal, otherwise False</returns>
bool is_integer_literal(string name)
{
	return int.TryParse(name, out _);
}

/// <summary>Try to evaluate a type printing the result or reporting errors.</summary>
/// <param name="env">The type environment in which to evaluate the expression.</param>
/// <param name="node">The root node of the abstract syntax tree of the expression.</param>
void try_exp(Dictionary<string, object> env, object node)
{
	Console.Write($"{node} : ");
	try
	{
		var t = analyse(node, env);
		Console.WriteLine(t);
	}
	catch (ParseError e)
	{
		Console.WriteLine(e.Message);
	}
	catch (InferenceError e)
	{
		Console.WriteLine(e.Message);
	}
}

record Lambda(string variableName, object body)
{
	public override string ToString() => $"(fn {variableName} => {body})";
}
record Identifier(string name)
{
	public override string ToString() => name;
}
record Apply(object fn, object arg)
{
	public override string ToString() => $"({fn} {arg})";
}
record Let(string v, object defn, object body)
{
	public override string ToString() => $"(let {v} = {defn} in {body})";
}
record Letrec(string v, object defn, object body)
{
	public override string ToString() => $"(letrec {v} = {defn} in {body})";
}

class InferenceError(string message) : Exception(message) { }
class ParseError(string message) : Exception(message) { }

/// <summary>
/// A type variable standing for an arbitrary type.
/// All type variables have a unique id, but names are only assigned lazily,
/// when required.
/// </summary>
class TypeVariable
{
	static int next_variable_id = 0;
	static char next_variable_name = 'a';

	char? __name;
	public object? instance;
	public int id;

	public TypeVariable()
	{
		this.id = next_variable_id;
		next_variable_id++;
		instance = null;
		__name = null;
	}

	/// <summary>
	/// Names are allocated to TypeVariables lazily, so that only TypeVariables present after analysis consume names.
	/// </summary>
	public string Name
	{
		get
		{
			if (!__name.HasValue)
			{
				__name = next_variable_name;
				next_variable_name = (char)(next_variable_name + 1);
			}

			return __name.Value.ToString();
		}
	}

	public override string ToString()
	{
		return instance?.ToString() ?? Name;
	}
}

/// <summary>
/// An n-ary type constructor which builds a new type from old
/// </summary>
class TypeOperator(string name, List<object> types)
{
	public string Name { get; } = name;
	public List<object> Types { get; } = types;

	public override string ToString()
	{
		var num_types = Types.Count;

		if (num_types == 0)
			return Name;
		else if (num_types == 2)
			return string.Format("({0} {1} {2})", Types[0], Name, Types[1]);
		else
			return string.Format("{0} {1}", Name, string.Join(' ', Types));
	}
}

class Function(object from_type, object to_type) : TypeOperator("->", new List<object> { from_type, to_type })
{
}