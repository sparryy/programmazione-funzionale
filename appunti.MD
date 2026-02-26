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

```bash
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

Tipi base: // ricopiare gli esempi da moodle
- int (per negativo uso la tilde `~`)
    * val it = 3: int;
    * val it = ~3: int;
- real: per renderlo reale serve almeno una cifra dopo il `.`, posso usare notazione esponenziale
    * 
- bool: `true` e `false`
    * 
- char: `#"a"`
    * 
- string:
    * 
- unit: per espressioni senza valore, vale ()

### operatori

aritmetici:
* \+ e \-
* \*, /, div (round floor), mod
* ~ meno

regole di precedenza:
1. espressioi tra parentesi
2. operatori unari
3. prodotti e divisioni
4. addizioni e sottrazioni

infissi:

```Function: + : int * int -> int```

`*` è usato per le tuple, `+` ha solo duple

stringe

^ concatenazione

> "house" ^ "cat";
val it = "housecat": string

comparazioni

=, <, >, <=, >=, <>

uguaglianza e non uguaglianza non si possono fare con i reali

logici

- not
- andalso: se il primo è falso non si valuta il secondo
- orelse: se il primo è vero non si valuta il secondo

if-then-else

```if <p> then <exp1> else <exp2>```

else è necessario

entrambi i casi devono avere un valore di ritorno che siano dello stesso tipo

