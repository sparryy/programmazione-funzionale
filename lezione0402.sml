(* Lezione 2/4 *)

(*------------------------------------------------*)

(* Es. 1 - Minore di 5 *)

fun is_lower_than5 x = 
    case x < 5 of
    true => 1
    | false => 2;

is_lower_than5(2);

(*------------------------------------------------*)

(* Es. 2 - Potenza di mille *)

fun thousandPower n:real =
    let
        val dieci = n*n*n*n*n*n*n*n*n*n
        val cento = dieci*dieci*dieci*dieci*dieci*dieci*dieci*dieci*dieci*dieci
        val mille = cento*cento*cento*cento*cento*cento*cento*cento*cento*cento
    in 
        mille
    end;

thousandPower(1.1);

(*------------------------------------------------*)

(* Es. 3 - Split *)

fun split(nil) = (nil,nil)
  | split([a]) = ([a],nil)
  | split (a::b::cs) =
    let
        val M = #1 (split (cs));
        val N = #2 (split (cs));
    in
        (a::M,b::N)
    end;

split([1,2,3,4,5,6,7,8,9,0]);

(*------------------------------------------------*)

(* Es.4 - Insieme delle parti 2 *)

fun insertAll (x, nil) = nil
  | insertAll (x, l::ls) = [x::l]@insertAll(x, ls);

fun powerSet(nil) = [nil]
  | powerSet(x::xs) =
    let
        val l = powerSet(xs)
        val m = insertAll(x,l)
    in
        l@m
    end;

powerSet([1,2,3]);

(*------------------------------------------------*)

(* Es.5 - Somma coppie *)

fun sumPairs [] = (0,0)
  | sumPairs ((x,y)::xs) =
    let
        val (l,m) = sumPairs(xs)
    in
        (l+x,m+y)
    end;

sumPairs [(1,2),(3,4),(5,6)];

(*------------------------------------------------*)

(* Es. 6 - Lista Massima *)

fun maxList([x:real]) = x
  | maxList(x::xs) =
    let
        val maxT = maxList(xs)
        val max = if x > maxT then x else maxT 
    in
        max
    end;

maxList([1.1, 1.2, 1.3, 1.5, 4.5, 1.2, 9.0, 10.0, 0.1]);

(*------------------------------------------------*)

(* Es.7 - Doppio Esponente *)

fun doubleExp (x:real, 0) = x
  | doubleExp (x:real, i:int) =
    let
        val a = doubleExp(x, i-1)
    in
        a*a
    end;

doubleExp(1.1, 3);

(*------------------------------------------------*)

(* Es.8 - Somma Lista *)

fun sumList([]) = (0,0)
  | sumList([x]) = (x,0)
  | sumList(x::y::zs) =
    let
       val (l,m) = sumList(zs)
    in
        (x+l,y+m)
    end;

sumList([1,2,3,4]);

(*------------------------------------------------*)

(* Es.9 - Stampa Lista *)

fun printList ([]) = print ""
  | printList (x::xs) = (print(Int.toString(x)^"\n"); printList(xs));

printList([1,2,3]);

(*------------------------------------------------*)

(* Es.10 - Combinazione *)

fun fact 0 = 1
  | fact x = x * fact(x-1);

fun comb (n,m) = 
  (print "n is "; print(Int.toString(n)); print "\n";
   print "m is "; print(Int.toString(m)); print "\n";
   print "result is "; print(Int.toString(fact(n) div (fact(m)*(fact(n-m))))); print "\n");

comb(5,2);

(*------------------------------------------------*)

(* Es.11 - Stampa XS *)

fun printXs 0 = print("X")
  | printXs x = (printXs(x-1); printXs(x-1));

printXs(3);