# Lezione 3/3

## Tipi complessi:
 - tupla, può avere tipi diversi all'interno (inclusi altri complessi) e devono avere almeno 2 elementi:  
    * ```val it = (1,2,3): int\*int\*int;```
    * `#1 it`: restituisce il primo elemento della tupla.
 - lista, deve avere stesso tipo:
    * ```val it = [1,2,3]: int list;```  
    * `hd(it)`: testa della lista
    * `tl(it)`: restituisce la lista senza testa
    * `[1,2] @ [3,4]`: concatena
    * `2::[3,4]`: mette un elemento in testa
    * `1::2::3` si trasforma in `[1,2,3]`
    * `nil = []`

## Equivalenza e Compatibilità

### Equivalenza

**Equivalenza per nome**: se la definizione di un tipo è opaca, ovvero se hanno lo stesso nome o stesso identificatore.
Se è la stessa cosa ma la rinomino allora è debolmente equivalente.

**Equivalenza strutturale**: se la definizione di un tipo è trasparente e hanno la stessa struttura.

### Compatibilità

**T** e **S** sono compatibili se:  
 - sono equivalenti  
 - le operazioni di S si possono fare anche su T
 - c'è corrispondenza tra valori (float e int)
 - se i valori di T sono un subset di quelli di S

Allora in questo caso posso fare conversioni (implicite o esplicite).

## Conversioni

- `real(it)`: int to real
- `floor(it)`: real to int (verso il basso)
- `ceil(it)`: real to int(verso l'alto)
- `round(it)`: real to int (o verso l'alto o il basso, se è a metà va verso l numero pari intero più vicino)
- `ord(it)`: char to int
- `chr(it)`: int to char
- `str(it)`: char to str
- `explode(it)`: string to char list
- `implode(it)`: char list to string

## Espressioni

Tutte le espressioni finiscono con `;`
```ml
1+2*3; diventa  
> ```val it = 7: int;```  
```
`if <p> then <esp1> else <esp2>;`   
Anche else è obbligatorio, e i tipi in then e else devono essere uguali:  
`if 5<5 then 3 else 4;`

## Funzioni
Mapping di input in output, possono restituire valori o altre funzioni
non hanno side effect

`map <funzione> [...]`: usa la funzione per ogni valore della lista 