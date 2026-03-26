(* Lezione 26/3 *)

(*------------------------------------------------*)

(* Es. 1 - Flip *)

fun flip ([]) = []
  | flip ([x]) = [x]
  | flip (x::y::xs) = y::x::flip(xs);

flip([1,2,3,4,5]);

(*------------------------------------------------*)

(* Es. 2 - Rimuovi L *)

fun remove ([], x) = []
  | remove (L, 1) = tl(L)
  | remove (L, x) = [hd(L)]@remove(tl(L), x-1);

remove([1,2,3],3);

(*------------------------------------------------*)

(* Es. 3 - Quadrato *)

fun square (0) = 0
  | square (n) = square(n-1) + 2*n - 1;

square(2);

(*------------------------------------------------*)

(* Es. 4 - Flip 2 *)

fun flip2 ([]) = []
  | flip2 ((x,y)::xs) = if x < y then (x,y)::flip2(xs) else (y,x)::flip2(xs);

flip2 [(5,2),(4,3),(6,5),(1,2)];

(*------------------------------------------------*)

(* Es.5 - Vocale *)

fun vowel ([]) = false
  | vowel (L) = (hd(L) = #"a") orelse (hd(L) = #"e") orelse (hd(L) = #"i") orelse (hd(L) = #"o") orelse (hd(L) = #"u");

vowel [#"a",#"b"];

(*------------------------------------------------*)

(* Es. 6 - Membro *)

fun member (x, []) = false
  | member (x, L) = (x = hd(L)) orelse member(x, tl(L));

member ("b",["aa","c"]);

(*------------------------------------------------*)

(* Es. 7 - Elimina *)

fun delete (x, []) = []
  | delete (x, L) = if x = hd(L) then delete(x, tl(L)) else hd(L)::delete(x, tl(L));

delete (#"a",[#"c",#"b",#"a"]);

(*------------------------------------------------*)

(* Es. 8 - Inserisci *)

fun insert (x, []) = [x]
  | insert (x, L) = if x = hd(L) then L else hd(L)::insert(x, tl(L));

insert (#"a",[#"b",#"c"]);

(*------------------------------------------------*)

(* Es. 9 - Inserisci todos *)

fun insertAll (x, nil) = nil
  | insertAll (x, l::ls) = [x::l]@insertAll(x, ls);

insertAll (#"c",[[#"a",#"t"],[#"a",#"r"],nil]);

(*------------------------------------------------*)

(* Es. 10 - Insieme delle parti *)

fun powerSetR nil = nil
  | powerSetR (l::ls) = [l]::powerSetR(ls)@insertAll(l, powerSetR(ls));

fun powerSet nil = [[]]
  | powerSet (L) = [[]]@powerSetR(L);

powerSet([1,2,3]);

(*------------------------------------------------*)

(* Es. 11 - Prodotto differenza *)

fun prodDiffR (x, []) = 1.0
  | prodDiffR (x, (l::ls)) = (x-l)*prodDiffR(x, ls);

fun prodDiff nil = 1.0
  | prodDiff (l::ls) = prodDiffR(l, ls)*prodDiff(ls);

prodDiff([1.0,2.0,3.0]);

(*------------------------------------------------*)

(* Es. 12 - Uno? *)

fun is_one (1) = "one"
  | is_one (_) = "anything else";

is_one(1);