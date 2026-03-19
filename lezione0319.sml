(* Lezione 19/3 *)

(*------------------------------------------------*)

(* Es. 1 - Fattoriale *)

fun fact x:int =
    if x <= 1 then
        x
    else
        x*fact(x-1)
;

fact(10);

(*------------------------------------------------*)

(* Es.2 - Ciclo *)

fun cyclei (i, l) =
    if(i > 0) then 
        cyclei(i-1, tl(l) @ [hd(l)])
    else
        l
;

cyclei(2,[1,2,3,4]);

(*------------------------------------------------*)

(* Es.3 - Duplica *)

fun duplicate x=
    if x=nil then
        nil
    else
        [hd(x), hd(x)] @ duplicate(tl(x))
;

duplicate([1,2,3,4]);

(*------------------------------------------------*)

(* Es.4 - Lenght *)

fun len x =
    if x=nil then
        0
    else
        1 + len(tl(x))
;

len([1,2,3,4]);

(*------------------------------------------------*)

(* Es.5 - Potenza *)

fun pow (b,e) =
    if e=0 then
        1.0
    else
        b*pow(b, e-1)
;

pow(2.1, 3);

(*------------------------------------------------*)

(* Es.6 - Lista Massima *)

fun maxList x =
    if x = nil then
        ""
    else
        if hd(x) > maxList(tl(x)) then
            hd(x)
        else
            maxList(tl(x))
;

maxList(["a","abc","ab"]);

(*------------------------------------------------*)

(* Es.7 - Fattoriale Pattern *)

fun factP 1 = 1
  | factP x = x * factP(x-1);

factP(10);

(*------------------------------------------------*)

(* Es.9 - Ciclo Pattern *)

fun cycleiP (i, []) = []
  | cycleiP (0, l) = l
  | cycleiP (i, l) = cycleiP(i-1, tl(l) @ [hd(l)]);

cycleiP(2,[1,2,3,4]);

(*------------------------------------------------*)

(* Es.10 - Duplica Pattern *)

fun duplicateP [] = nil
  | duplicateP x = [hd(x), hd(x)] @ duplicateP(tl(x));

duplicateP([1,2,3,4]);

(*------------------------------------------------*)

(* Es.11 - Potenza Pattern *)

fun powP (b, 0) = 1.0
  | powP (b, e) = b* powP(b, e-1);

powP(2.1, 3);

(*------------------------------------------------*)

(* Es.12 - Lista Massima Pattern *)

fun maxListP [] = ""
  | maxListP x = if hd(x) > maxListP(tl(x)) then
                    hd(x)
                else
                    maxListP(tl(x));

maxListP(["a","abc","ab"]);