# Lezione 5/3

## Tipi complessi

### Tuple

```ml
> val t = (4, 5.0, "six");
val t = (4, 5.0, "six"): int * real * string
```
Possiamo anche usare tipi complessi:
```ml
> val t = (1, (2, 3, 4));
val t = (1, (2, 3, 4)): int * (int * int * int)
```

**ACCESSO AGLI ELEMENTI DELLA TUPLA:**

Consideriamo:
```ml
> val t = (4, 5.0, "six");
```
Allora:
```ml
> #1 (t);
val it = 4: int
> #3 (t);
val it = "six": string
> #4 (t);
poly: : error: Type error in function application.
Function: #4 : 'a * 'b * 'c * 'd -> 'd
Argument: (t) : int * real * string
Reason: Can't unify {4: 'a} to int * real * string (Field 4
missing)
```

### Liste

Rappresentate dalle []
```ml
> [1,2,3];
val it = [1, 2, 3]: int list
```
Gli elementi della lista devono essere dello stesso tipo (a differenza della tupla).

**ESEMPI:**
```ml
> [1.0,2.0];
val it = [1.0, 2.0]: real list
```
```ml
> [1,2.0];
poly: : error: Elements in a list have different types.
Item 1: 1 : int
Item 2: 2.0 : real
Reason:
Can't unify int (*In Basis*) with real (*In Basis*)
(Different type constructors)
```
```ml
> [];
val it = []: 'a list
```
Se una lista è vuota, ML non riesce a determinarne il tipo.

**TESTA DELLA LISTA**

```ml
> val L = [2,3,4];
val L = [2, 3, 4]: int list
> val M = [5];
val M = [5]: int list
```
```ml
> hd(L);
val it = 2: int
> hd(M);
val it = 5: int
```

**CODA DELLA LISTA**
```ml
> L;
val it = [2, 3, 4]: int list
> M;
val it = [5]: int list
```
```ml
> tl (L);
val it = [3, 4]: int list
> tl (M);
val it = []: int list
```
ML riesce a determinare il tipo di questa lista vuota.

**CONCATENAZIONE DI LISTE** `@`

Eesmpio:
```ml
> [1,2] @ [3,4];
val it = [1, 2, 3, 4]: int list
```
Entrambe le liste devono essere dello stesso tipo.
```ml
> [1,2] @ ["a","b"];
poly: : error: Type error in function application.
Function: @ : int list * int list -> int list
Argument: ([1, 2], ["a", "b"]) : int list * string list
Reason:
Can't unify int (*In Basis*) with string (*In Basis*)
(Different type constructors)
```

Due diversi tipi di concatenazione:
* `^`: Strings
* `@`: List

****

Un operatore che prende un elemento di tipo `'a` e una lista di tipo `'a list` e li combina
```ml
> 2 :: [3,4];
val it = [2, 3, 4]: int list
> 2 :: nil;
val it = [2]: int list
> 2 :: [];
val it = [2]: int list
```
`::` associa a destra.
```ml
> 1 :: 2 :: 3 :: nil;
val it = [1, 2, 3]: int list
> (1 :: (2:: (3::nil)));
val it = [1, 2, 3]: int list
```
Nell'altro modo non avrebbe senso.

**STRINGHE A LISTE (explode)**
```ml
explode ("abcd");
val it = [#"a", #"b", #"c", #"d"]: char list
> explode ("");
val it = []: char list
```
**STRINGHE A LISTE (implode)**
```ml
implode ([ #"a", #"b", #"c", #"d"]);
val it = "abcd": string
> implode (nil);
val it = "": string
> implode (explode ("xyz"));
val it = "xyz": string
```

## Esercizi:

### es. 1.1
Scrivi le espressione per convertire:
- `123.45` all'intero precedente
- `-123.45` all'intero precedente
- `123.45` all'intero successivo
- `-123.45` all'intero successivo

Soluzione:

```ml
> floor 123.45;
val it = 123: int
> floor ~123.45;
val it = ~124: int
> ceil 123.45;
val it = 124: int
> ceil ~123.45;
val it = ~123: int
```

### es. 1.2

Scrivi le espressione per convertire:
- #”Y” in un intero
- 120 in un carattere
- 97.0 in un carattere
- #”N” in un reale
- #”Z” in una stringa

Soluzione:

```ml
> ord(#"Y");
val it = 89: int
> chr(120);
val it = #"x": char
> chr(round(97.0));
val it = #"a": char
> real(ord(#"N"));
val it = 78.0: real
> str(#"Z");
val it = "Z": string
```
### es. 1.3
Trova e correggi le seguenti espressioni:
- `ceil(4);`
- `if true then 5+6 else 7.0;`

Soluzione:
```ml
> ceil(real(4));
val it = 4: int
> if true then 5+6 else round(7.0);
val it = 11: int
```

### es. 1.4
Trova e correggi le seguenti espressioni:
- `if 0 then 1 else 2;`
- `ord(“a”)`

Soluzione:
```ml
> if false then 1 else 2;
val it = 2: int
> ord(#"a");
val it = 97: int
```

### es. 1.5
Trova e correggi le seguenti espressioni:
- `explode ["bar"];`
- `implode ( #"a", #"b");`
- `["r"]::["a","t"];`

Soluzione:
```ml
> explode("bar");
val it = [#"b", #"a", #"r"]: char list
> implode [#"a", #"b"];
val it = "ab": string
> "r"::["a","t"]
val it = ["r", "a", "t"]: string list
```

### es.1.6
Fai degli esempi di oggetti dei seguenti tipi, senza usare liste vuote:
- `int list list list`
- `(int * char) list`
- `string list * ( int * (real * string)) * int`

Soluzione:
```ml
> [[[1,2],[3,4]],[[5,6],[7,8]]];
val it = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]: int list list list
> [(1, #"A"), (2, #"B")];
val it = [(1, #"A"), (2, #"B")]: (int * char) list
> (["a", "b"], (1, (2.1, "c")), 3);
val it = (["a", "b"], (1, (2.1, "c")), 3): string list * (int * (real * string)) * int
```

### es. 1.7
Fai degli esempi di oggetti dei seguenti tipi, senza usare liste vuote:
- `((int * int) * (bool list) * real) * (real * string)`
- `(bool * int) * char`
- `real * int list list list list`

Soluzione:
```ml
> 

> ((true, 1), #"a");
val it = ((true, 1), #"a"): (bool * int) * char
> 

```