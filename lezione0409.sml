(* Lezione 9/4 *)

(* commento per exec

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

*)
(* Es. 5 - Tabula *)

