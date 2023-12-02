global using рядок = System.String;
global using байт = System.Byte;
global using обєкт = System.Object;
global using ціл = System.Int32;
global using симв = System.Char;
global using логічний = System.Boolean;
global using Виключення = System.Exception;

using System.Diagnostics;
using System.Text;
using System.Collections;
using System.Diagnostics.CodeAnalysis;
using System.Security.Cryptography.X509Certificates;

static class Консоль
{
	public static void НадрукуватиЛінію(рядок формат)
	{
		System.Console.WriteLine(формат);
	}
	public static void НадрукуватиЛінію(обєкт значення)
	{
		System.Console.WriteLine(значення);
	}
	public static void Надрукувати(рядок формат)
	{
		System.Console.Write(формат);
	}
}

class Список<Т> : IList<Т>
{
	List<Т> список;

	public Список()
	{
		список = new();
	}

	public Список(IEnumerable<Т> послідовність)
	{
		список = new(послідовність);
	}

	public void Додати(Т елемент)
	{
		список.Add(елемент);
	}

	public int Кількість => this.список.Count;

	public int Count => this.список.Count;

	public bool IsReadOnly => ((IList<Т>)список).IsReadOnly;

	public Т this[int індекс] { get => список[індекс]; set => список[індекс] = value; }

	public Т[] ДоМасиву() => список.ToArray();

	public int IndexOf(Т item)
	{
		throw new NotImplementedException();
	}

	public void Insert(int index, Т item)
	{
		throw new NotImplementedException();
	}

	public void RemoveAt(int index)
	{
		throw new NotImplementedException();
	}

	public void Add(Т item)
	{
		список.Add(item);
	}

	public void Clear()
	{
		throw new NotImplementedException();
	}

	public bool Contains(Т item)
	{
		throw new NotImplementedException();
	}

	public void CopyTo(Т[] array, int arrayIndex)
	{
		throw new NotImplementedException();
	}

	public bool Remove(Т item)
	{
		throw new NotImplementedException();
	}

	public IEnumerator<Т> GetEnumerator() => список.GetEnumerator();

	IEnumerator IEnumerable.GetEnumerator() => список.GetEnumerator();
}

class Словник<ТКлюч, ТЗначення> : IEnumerable<KeyValuePair<ТКлюч, ТЗначення>>, IDictionary<ТКлюч, ТЗначення>
	where ТКлюч: notnull
{
	Dictionary<ТКлюч, ТЗначення> словник;

	public Словник()
	{
		словник = new();
	}

	public Словник(IDictionary<ТКлюч, ТЗначення> словник)
	{
		this.словник = new Dictionary<ТКлюч, ТЗначення>(словник);
	}

	public ICollection<ТКлюч> Keys => this.словник.Keys;

	public ICollection<ТЗначення> Values => this.словник.Values;

	public int Count => this.словник.Count;

	public bool IsReadOnly => ((IDictionary<ТКлюч, ТЗначення>)this.словник).IsReadOnly;

	public ТЗначення this[ТКлюч key] { get => словник[key]; set => словник[key] = value; }

	public IEnumerator<KeyValuePair<ТКлюч, ТЗначення>> GetEnumerator() => словник.GetEnumerator();

	public void Додати(ТКлюч ключ, ТЗначення значення) => словник.Add(ключ, значення);
	public void Add(ТКлюч ключ, ТЗначення значення) => словник.Add(ключ, значення);

	public логічний МіститьКлюч(ТКлюч ключ) => словник.ContainsKey(ключ);

	IEnumerator IEnumerable.GetEnumerator() => словник.GetEnumerator();

	public bool ContainsKey(ТКлюч ключ) => МіститьКлюч(ключ);

	public bool Remove(ТКлюч ключ) => this.словник.Remove(ключ);

	public bool TryGetValue(ТКлюч ключ, [MaybeNullWhen(false)] out ТЗначення значення) => this.словник.TryGetValue(ключ, out значення);

	public void Add(KeyValuePair<ТКлюч, ТЗначення> item)
	{
		throw new NotImplementedException();
	}

	public void Clear() => this.словник.Clear();

	public bool Contains(KeyValuePair<ТКлюч, ТЗначення> item)
	{
		throw new NotImplementedException();
	}

	public void CopyTo(KeyValuePair<ТКлюч, ТЗначення>[] array, int arrayIndex)
	{
		throw new NotImplementedException();
	}

	public bool Remove(KeyValuePair<ТКлюч, ТЗначення> item)
	{
		throw new NotImplementedException();
	}
}

class БудіникРядків
{
	StringBuilder будівник = new();

	public void Додати(симв значення)
	{
		будівник.Append(значення);
	}

	public void Додати(рядок? значення)
	{
		будівник.Append(значення);
	}

	public void Додати(симв[]? значення)
	{
		будівник.Append(значення);
	}

	public рядок ДоРядка() => будівник.ToString();
}

static class Відладка
{
	public static void Вдостовіритися(логічний умова)
	{
		Debug.Assert(умова);
	}
	public static void Вдостовіритися(логічний умова, рядок повідомлення)
	{
		Debug.Assert(умова, повідомлення);
	}
}

static class Масив
{
	public static Т[] Пустий<Т>()
	{
		return Array.Empty<Т>();
	}
}

static class Файл
{
	public static рядок[] ПрочитатиУсіРядки(рядок шлях)
	{
		return File.ReadAllLines(шлях);
	}
}

static class ЛінкРозширення
{
	public static Список<Т> ДоСписка<Т>(this IEnumerable<Т> послідовність) => new Список<Т>(послідовність);
}