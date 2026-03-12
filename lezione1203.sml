(* Lezione 12/3 *)

(*------------------------------------------------*)

(* Es. 2.1 - Cubo *)
fun cube x:real = x*x*x;

cube(2.9);

(*------------------------------------------------*)

(* Es. 2.2 - Min3 *)

fun min3 (x, y, z) =
    if x<y then
        if x<z then
            x
        else
            z
    else
        if y < z then
            y
        else
            z
;

fun max3 (x, y, z) =
    if x>y then
        if x>z then
            x
        else
            z
    else
        if y > z then
            y
        else
            z
;

min3(1,2,3);

max3(1,2,3);

(*------------------------------------------------*)

(* Es. 2.3 - Third *)

fun third x = hd(tl(tl(x)));

third([1,2,3,4]);

(*------------------------------------------------*)

(* Es. 2.4 - Reverse *)

fun reverse (x,y,z) = (z,y,x);

reverse(1,"a",3.2);

(*------------------------------------------------*)

(* Es. 2.5 - Thirdchar *)

fun thirdchar x = str(third(explode(x)));

thirdchar("abcd");

(*------------------------------------------------*)

(* Es. 2.6 - Cycle *)

fun cycle x = tl(x) @ [hd(x)];

cycle([1,2,3,4]);

(*------------------------------------------------*)

(* Es. 2.7 - Min, Max, Pair *)

fun min_max_pair (x,y,z) = (min3(x,y,z), max3(x,y,z));

min_max_pair(1,2,3);


fun min3 (x, y, z) =
    if x<y then
        if x<z then
            x
        else
            z
    else
        if y < z then
            y
        else
            z
;

fun max3 (x, y, z) =
    if x>y then
        if x>z then
            x
        else
            z
    else
        if y > z then
            y
        else
            z
;

(*------------------------------------------------*)

(* Es. 2.8 - Sort *)

fun sort (x,y,z) =
    [min3(x,y,z),
     x+y+z-min3(x,y,z)-max3(x,y,z),
     max3(x,y,z)];

sort(1,3,2);

(*------------------------------------------------*)

(* Es. 2.9 - Round *)

fun rnd x:real = real(floor(x))+real(floor(x-real(floor(x))+0.5));

rnd(2.49);

(*------------------------------------------------*)

(* Es. 2.10 - Remove 2° *)

fun rem x = hd(x)::tl(tl(x));

rem([1,2]);