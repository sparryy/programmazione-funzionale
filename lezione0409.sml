(* Lezione 9/4 *)

(*------------------------------------------------*)

(* Es. 1 - Funzioni base *)

(* Apertura file *)
val infile = TextIO.openIn("file.txt");

(* Leggo i primi 5 caratteri *)
TextIO.inputN (infile,5);

(* Leggo una riga *)
TextIO.inputLine (infile);

(* Leggo il primo carattere disponibile senza "consumarlo" *)
TextIO.lookahead;

(* Leggo l'intero file *)
TextIO.input (infile);

(* Chiudo il file *)
TextIO.closeIn(infile);

(*------------------------------------------------*)

(* Es. 2 - Prendi la lista *)

fun getWord(infile) =
    let
        val txt = TextIO.inputN(infile,1)
    in
        if txt = " " orelse TextIO.endOfStream(infile)
        then
            ""
        else
            txt^getWord(infile)
        end;


fun getList(infile) =
    if TextIO.endOfStream (infile)
        then
            []
        else
            getWord(infile)::getList(infile);

val infile = TextIO.openIn("file.txt");
getList(infile);

(*------------------------------------------------*)

(* Es. 3 - Ritorna il terzo *)

exception ShortList of int;

fun returnThird([x,y,z]) = print(Int.toString(z)^"\n")
  | returnThird(x::y::z::w) = print(Int.toString(z)^"\n")
  | returnThird([x,y]) = raise ShortList (2)
  | returnThird([x]) = raise ShortList (1)
  | returnThird([]) = raise ShortList (0);


fun returnThird_safe (l) = returnThird(l) handle
    ShortList(n) => print("List too short.\n It only contains "^Int.toString(n)^" elements.\n");

returnThird_safe [1,2,3,4];
returnThird_safe [1,2];

(*------------------------------------------------*)

(* Es. 4 - Fattoriale *)

exception FactExc of int;

fun fact1 0 = 1
  | fact1 n = if n>0 then n*fact1(n-1) else raise FactExc(n);

fun fact(n) = print(Int.toString(fact1(n))^"\n") handle
    FactExc (n) => print("0\nNegative argument "^Int.toString(n)^" found\n");

fact(4);
fact(~2);

(*------------------------------------------------*)

(* Es. 5 - Tabula *)

fun tabulate(a,d,0,F) = ()
  | tabulate(a,d,n,F) = (
        print(Real.toString(a));
        print("\t");
        print(Real.toString(F(a)));
        print("\n");
        tabulate(a+d,d,n-1,F));

tabulate(1.0,0.1,9,fn x => x*x);

(*------------------------------------------------*)

(* Es. 6 - Simple Map *)

fun simpleMap (F,nil) = nil
  | simpleMap (F,x::xs) = F(x) :: simpleMap(F,xs);

simpleMap(fn x => if x < 0.0 then 0.0 else x, [0.0,1.0,~2.1,~2.3]);

(*------------------------------------------------*)

(* Es. 7 - Riduci *)

exception EmptyList;

fun reduce (F,nil) = raise EmptyList
  | reduce (F,[a]) = a
  | reduce (F,x::xs) = F(x, reduce(F,xs));

reduce(fn (x,a) => if x>a then x else a, [1.1,2.2,4.4,3.3]);

(*------------------------------------------------*)

(* Es. 8 - Filtro *)

fun filter (P,nil) = nil
  | filter (P,x::xs) =
        if P(x) then x::filter(P,xs)
        else filter (P,xs);

filter(fn x => if x > 0.0 then true else false, [1.1,~1.2,~1.3,1.4]);

(*------------------------------------------------*)

(* Es. 9 - Leggi e somma *)

fun readAndSum (infile) =
    if TextIO.endOfStream(infile) then
        0
    else
        let
            val number = TextIO.inputLine(infile)
        in
            valOf(Int.fromString(valOf(number))) + readAndSum(infile)
        end;

val infile = TextIO.openIn("file.txt");
readAndSum(infile);

(*------------------------------------------------*)

(* Es. 10 - Apllica lista funzioni *)

fun applyList (nil, _) = nil
  | applyList (F::Fs, x) = F(x)::applyList(Fs, x);

applyList([fn x => x*x, fn x => x+1], 10);