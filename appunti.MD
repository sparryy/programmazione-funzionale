# Programmazione Funzionale

# Lezione 26/2

## Poly

### Comandi base

Per entrare basta scrivere `poly` su terminale, Quando sei dentro vedi `>`.

> Per interrompere `CTRL+C`
>
> Per uscire `CTRL+D`

`rlwrap poly` per utilizzare il cursore liberamente.

### Espressioni

La procedura può non terminare per i seguenti motivi:
- una variabile o costante
- un operatore su argomenti

Ci sono tre notazioni possibli:
1. infissa: a + b
2. prefissa: + a b
3. suffissa: a b +

La seconda e la terza posso essere comode perché non hanno bisogno di parentesi.

Gli operatori vengono visti come funzioni, il `+` prende come parametri `a` e `b`.

Tutte le espressioni finiscono con `;`:

```ml
> 1+2*3;

val it = 7: int
```

#### Tipi di evaluation
- Eager evaluation: prima valuta tutti gli operandi e poi applica l'operatore (con eccezioni come if con || risulterebbero inutili)
- Lazy evaluation: valuta gli operandi solo quando necessario

### Tipi di dati
Collezione di valori con operatori applicabili al loro tipo

Utili per:
- organizzare le informazioni
- identificare gli errori
- ottimizzazione

**TIPI BASE:**
- int (per i negativi uso la tilde `~`)
```ml
val it = 3: int;
val it = ~3: int;
```
- real (per renderlo reale serve almeno una cifra dopo il `.`, posso anche usare notazione esponenziale)
```ml
> ~123.0;
val it = ~123.0: real
> 3E~3;
val it = 0.003: real
> 3.14e12;
val it = 3.14E12: real
```
- bool: `true` e `false`
```ml
> true;
val it = true: bool
> 1=3;
val it = false: bool
```
- char: `#"a"`
```ml
> #"a";
val it = #"a": char
```
- string:
```ml
>"foo";
val it = "foo": string
```

> Special characters for strings:
>
> - `\n`: newline
> - `\t`: tab
> - `\\`: Backslash
> - `\"`: Double-quote
> - `\xyz`: ASCII character with this code (three digits) 

- unit: per espressioni senza valore, vale ()

### Operatori

**Aritmetici:**
* \+ e \-
* \*, /, div (round floor), mod
* ~ meno

**Regole di precedenza:**
1. espressioni tra parentesi
2. operatori unari
3. prodotti e divisioni
4. addizioni e sottrazioni

**Infissi:**

```Function: + : int * int -> int```

`*` è usato per le tuple, `+` ha solo duple

**Stringe:**

`^` concatenazione

```ml
> "house" ^ "cat";
val it = "housecat": string
```

**Comparazioni:**

=, <, >, <=, >=, <>

Uguaglianza e non uguaglianza non si possono fare con i reali.

**Logici:**
- not
- andalso: se il primo è falso non si valuta il secondo
- orelse: se il primo è vero non si valuta il secondo

**if-then-else**

```if <p> then <exp1> else <exp2>```

`else` è necessario

Entrambi i casi devono avere un valore di ritorno che sia dello stesso tipo

