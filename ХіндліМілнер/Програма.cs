// Basic types are constructed with a nullary type constructor

var Цілий = new ТиповийОператор("int", new Список<object> { }); // Basic integer
var Лог = new ТиповийОператор("bool", new Список<object> { }); // Basic bool

/*
    The main example program.

    Sets up some predefined types using the type constructors TypeVariable,
    TypeOperator and Function.  Creates a list of example expressions to be
    evaluated. Evaluates the expressions, printing the type or errors arising
    from each.
*/
var пер1 = new ТиповаЗмінна();

var пер2 = new ТиповаЗмінна();

var тип_пари = new ТиповийОператор("*", new Список<object> { пер1, пер2 });
var пер3 = new ТиповаЗмінна();

var моєОточення = new Словник<рядок, object>
{
    { "pair", new Функція(пер1, new Функція(пер2, тип_пари)) },
    { "true", Лог },
    { "cond", new Функція(Лог, new Функція(пер3, new Функція(пер3, пер3)))},
    { "zero", new Функція(Цілий, Лог)},
    { "pred", new Функція(Цілий, Цілий)},
    { "times", new Функція(Цілий, new Функція(Цілий, Цілий)) },
};

var pair = new Apply(new Apply(new Identifier("pair"),
                                new Apply(new Identifier("f"),
                                            new Identifier("4"))),
                 new Apply(new Identifier("f"),
                            new Identifier("true")));
var приклади = new object[]
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
foreach (var example in приклади)
    спробуватиВираз(моєОточення, example);


object аналізувати(object вузол, Словник<рядок, object> оточення, HashSet<object>? неШаблонні = null)
{
    if (неШаблонні is null)
    {
        неШаблонні = new();
    }

    if (вузол is Identifier ідент)
    {
        return взятиТип(ідент.назва, оточення, неШаблонні);
    }
    else if (вузол is Apply застосування)
    {
        var типФункції = аналізувати(застосування.функція, оточення, неШаблонні);
        var типАргументу = аналізувати(застосування.аргумент, оточення, неШаблонні);
        var типРезультату = new ТиповаЗмінна();

        уніфікувати(new Функція(типАргументу, типРезультату), типФункції);
        return типРезультату;
    }
    else if (вузол is Lambda лямбда)
    {
        var типАргументу = new ТиповаЗмінна();
        Словник<рядок, object> новеОточення = new(оточення);
        новеОточення[лямбда.назваЗмінної] = типАргументу;
        HashSet<object> new_non_generic = new(неШаблонні)
        {
            типАргументу
        };

        var типРезультату = аналізувати(лямбда.тіло, новеОточення, new_non_generic);
        return new Функція(типАргументу, типРезультату);
    }
    else if (вузол is Let нехай)
    {
        var типВизначення = аналізувати(нехай.визначення, оточення, неШаблонні);
        Словник<рядок, object> новеОточення = new(оточення);
        новеОточення[нехай.назваЗмінної] = типВизначення;
        return аналізувати(нехай.тіло, новеОточення, неШаблонні);
    }
    else if (вузол is Letrec letrec)
    {
        var новийТип = new ТиповаЗмінна();
        Словник<рядок, object> новеОточення = new(оточення);
        новеОточення[letrec.назваЗмінної] = новийТип;
        HashSet<object> новіНеШаблонні = new(неШаблонні)
        {
            новийТип
        };

        var типВизначення = аналізувати(letrec.визначення, новеОточення, новіНеШаблонні);
        уніфікувати(новийТип, типВизначення);
        return аналізувати(letrec.тіло, новеОточення, неШаблонні);
    }

    throw new InvalidOperationException(string.Format("Необроблений синтаксичний вузол {0}", вузол.GetType()));
}

object взятиТип(рядок назва, Словник<рядок, object> оточення, HashSet<object> неШаблонні)
{
    if (оточення.МіститьКлюч(назва))
        return свіжий(оточення[назва], неШаблонні);
    else if (чиЦілийЛітерал(назва))
        return Цілий;
    else
        throw new ПомилаРозбору(string.Format("Невизначений символ {0}", назва));
}

/// <summary>Makes a copy of a type expression.
/// The type t is copied. The the generic variables are duplicated and the
/// non_generic variables are shared.
/// </summary>
/// <param name="т">Тип для копіювання.</param>
/// <param name="неШаблонні">Набір не шаблонних ТипвихЗмінних</param>
object свіжий(object т, HashSet<object> неШаблонні)
{
    Словник<ТиповаЗмінна, ТиповаЗмінна> співставлення = new(); // A mapping of TypeVariables to TypeVariables
    return freshrec(т);

    object freshrec(object tp)
    {
        var p = підрізати(tp);

        if (p is ТиповаЗмінна tvp)
        {
            if (єДженеріком(tvp, неШаблонні))
            {
                if (!співставлення.МіститьКлюч(tvp))
                    співставлення[tvp] = new ТиповаЗмінна();

                return співставлення[tvp];
            }
            else
                return p;
        }
        else if (p is ТиповийОператор top)
            return new ТиповийОператор(top.Назва, top.Типи.Select(x => freshrec(x)).ДоСписка());
        else
        {
            Відладка.Вдостовіритися(false, "Не уніфіковано");
            throw new InvalidOperationException();
        }
    }
}

/// <summary>Unify the two types t1 and t2.
/// Makes the types t1 and t2 the same.
/// </summary>
/// <param name="t1">The first type to be made equivalent</param>
/// <param name="t2">The second type to be be equivalent</param>
void уніфікувати(object t1, object t2)
{
    var а = підрізати(t1);
    var б = підрізати(t2);

    if (а is ТиповаЗмінна тза)
    {
        if (а != б)
        {
            if (зустрічаєтьсяУТипу(тза, б))
                throw new ПомилкаВисновку("рекурсивна уніфікація");

            тза.екземпляр = б;
        }
    }
    else if ((а is ТиповийОператор тоа) && (б is ТиповаЗмінна тзб))
    {
        уніфікувати(б, а);
    }
    else if ((а is ТиповийОператор тоа2) && (б is ТиповийОператор тоб))
    {
        if (тоа2.Назва != тоб.Назва || тоа2.Типи.Кількість != тоб.Типи.Кількість)
        {
            throw new ПомилкаВисновку(string.Format("Невідповідність типів: {0} != {1}", а, б));
        }

        foreach ((var л, var м) in тоа2.Типи.Zip(тоб.Типи))
        {
            уніфікувати(л, м);
        }
    }
    else
    {
        Відладка.Вдостовіритися(false, "Не уніфіковано");
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
object підрізати(object т)
{
    if (т is ТиповаЗмінна тз)
    {
        if (тз.екземпляр is not null)
        {
            тз.екземпляр = підрізати(тз.екземпляр);
            return тз.екземпляр;
        }
    }

    return т;
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
логічний єДженеріком(ТиповаЗмінна v, HashSet<object> non_generic)
{
    return !зустрічаєтьсяУ(v, non_generic);
}

/// <summary>Checks whether a type variable occurs in a type expression.</summary>
/// <remarks>Note: Must be called with v pre-pruned</remarks>
/// <param name="т">ТиповаЗмінна яку треба протестувати</param>
/// <param name="тип">The type in which to search</param>
/// <returns>True if v occurs in type2, otherwise False</returns>
логічний зустрічаєтьсяУТипу(ТиповаЗмінна змінна, object тип)
{
    var підрізанийТип = підрізати(тип);
    if (підрізанийТип == змінна)
    {
        return true;
    }
    else if (підрізанийТип is ТиповийОператор пт)
    {
        return зустрічаєтьсяУ(змінна, пт.Типи);
    }

    return false;
}


/// <summary>Перевіряє чи зустрічається типова змінна у інших типах.</summary>
/// <param name="т">ТиповаЗмінна яку треба протестувати</param>
/// <param name="типи">Послідовніть типів у яких треба зробити пошук.</param>
/// <returns>True якщо t зустрічається у будьякому із типів, інакше False</returns>
логічний зустрічаєтьсяУ(ТиповаЗмінна т, IEnumerable<object> типи)
{
    return типи.Any(t2 => зустрічаєтьсяУТипу(т, t2));
}

/// <summary>Перевіряє чи ім'я є рядком із цілим літералом.</summary>
/// <param name="назва">Ідентифікатор для перевірки</param>
/// <returns>True якщо ім'я є цілим літералом, інакше False</returns>
логічний чиЦілийЛітерал(рядок назва)
{
    return ціл.TryParse(назва, out _);
}

/// <summary>Пробує оцінити тип печатаючи результат або повідомляя про помилки.</summary>
/// <param name="оточення">Оточення типів у якому оцінюються вирази.</param>
/// <param name="вузол">Корінний вузол абстрактного дерева синтаксісу виразу.</param>
void спробуватиВираз(Словник<рядок, object> оточення, object вузол)
{
    Консоль.Надрукувати($"{вузол} : ");
    try
    {
        var т = аналізувати(вузол, оточення);
        Консоль.НадрукуватиЛінію(т);
    }
    catch (ПомилаРозбору e)
    {
        Консоль.НадрукуватиЛінію(e.Message);
    }
    catch (ПомилкаВисновку e)
    {
        Консоль.НадрукуватиЛінію(e.Message);
    }
}

record Lambda(рядок назваЗмінної, object тіло)
{
    public override рядок ToString() => $"(fn {назваЗмінної} => {тіло})";
}
record Identifier(рядок назва)
{
    public override рядок ToString() => назва;
}
record Apply(object функція, object аргумент)
{
    public override рядок ToString() => $"({функція} {аргумент})";
}
record Let(рядок назваЗмінної, object визначення, object тіло)
{
    public override рядок ToString() => $"(let {назваЗмінної} = {визначення} in {тіло})";
}
record Letrec(рядок назваЗмінної, object визначення, object тіло)
{
    public override рядок ToString() => $"(letrec {назваЗмінної} = {визначення} in {тіло})";
}

class ПомилкаВисновку(рядок повідомлення) : Exception(повідомлення) { }
class ПомилаРозбору(рядок повідомлення) : Exception(повідомлення) { }

/// <summary>
/// Типова змінна, що означає довільний тип.
/// Усі типові змінні мають унікальні ід, але імена назначаються ліниво,
/// коли потрібно.
/// </summary>
class ТиповаЗмінна
{
    static ціл наступнийІдЗмінної = 0;
    static симв наступнаНазваЗмінної = 'a';

    симв? назва;
    public object? екземпляр;
    public ціл ід;

    public ТиповаЗмінна()
    {
        ід = наступнийІдЗмінної;
        наступнийІдЗмінної++;
        екземпляр = null;
        назва = null;
    }

    /// <summary>
    /// Names are allocated to TypeVariables lazily, so that only TypeVariables present after analysis consume names.
    /// </summary>
    public рядок Назва
    {
        get
        {
            if (!назва.HasValue)
            {
                назва = наступнаНазваЗмінної;
                наступнаНазваЗмінної = (симв)(наступнаНазваЗмінної + 1);
            }

            return назва.Value.ToString();
        }
    }

    public override рядок ToString()
    {
        return екземпляр?.ToString() ?? Назва;
    }
}

/// <summary>
/// n-арний конструктор типу який створює новий тип із старого
/// </summary>
class ТиповийОператор(рядок назва, Список<object> типи)
{
    public рядок Назва { get; } = назва;
    public Список<object> Типи { get; } = типи;

    public override рядок ToString()
    {
        var кількістьТипів = Типи.Кількість;

        if (кількістьТипів == 0)
            return Назва;
        else if (кількістьТипів == 2)
            return рядок.Format("({0} {1} {2})", Типи[0], Назва, Типи[1]);
        else
            return рядок.Format("{0} {1}", Назва, рядок.Join(' ', Типи));
    }
}

class Функція(object ізТипу, object доТипу) : ТиповийОператор("->", new() { ізТипу, доТипу })
{
}