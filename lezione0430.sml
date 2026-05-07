(* Lezione 30/4 *)

(*------------------------------------------------*)

(* Es. 1 - Set *)

signature SET =
sig
    type 'a set
    val emptyset: 'a set
    val isin: 'a * 'a set -> bool
    val addin: 'a * 'a set -> 'a set
    val removefrom: 'a * 'a set -> 'a set
end;

(*------------------------------------------------*)

(* Es. 2 - Structure *)

signature SET =
sig
    type 'a set
end;

structure ListSet: SET = struct
    type 'a set = 'a list
end;

val s: int ListSet.set = [1,2,3];

(*------------------------------------------------*)

(* Es. 3 - Empty Set *)

signature SET =
sig
    type 'a set
    val emptyset: 'a set
end;

structure ListSet: SET = struct
    type 'a set = 'a list
    val emptyset = []
end;

ListSet.emptyset;

(*------------------------------------------------*)

(* Es. 4 - Is In *)

signature SET =
sig
    type 'a set
    val emptyset: 'a set
    val isin: ''a -> ''a set -> bool
end;

structure ListSet: SET = struct
    type 'a set = 'a list
    val emptyset = []
    fun isin a [] = false
      | isin a (x::l) = if a = x then true else isin a l
end;

val t: int ListSet.set = [1,2,3];
val f = ListSet.isin 1 t;

(*------------------------------------------------*)

(* Es. 5 - Add In *)

signature SET =
sig
    type 'a set
    val emptyset: 'a set
    val isin: ''a -> ''a set -> bool
    val addin: ''a -> ''a set -> ''a set
end;

structure ListSet: SET = struct
    type 'a set = 'a list
    val emptyset = []
    fun isin _ [] = false
      | isin a (x::l) = if a = x then true else isin a l
    fun addin x L = if (isin x L) then L else x::L;
end;

val a = ListSet.emptyset;
val b = ListSet.addin 1 a;

(*------------------------------------------------*)

(* Es. 6 - Remove From *)

signature SET =
sig
    type 'a set
    val emptyset: 'a set
    val isin: ''a -> ''a set -> bool
    val addin: ''a -> ''a set -> ''a set
    val removefrom: ''a -> ''a set -> ''a set
end;

structure ListSet: SET = struct
    type 'a set = 'a list
    val emptyset = []
    fun isin _ [] = false
      | isin a (x::l) = if a = x then true else isin a l
    fun addin x L = if (isin x L) then L else x::L;
    fun removefrom _ [] = []
      | removefrom a (x::l) = if a = x then l else x::removefrom x l
end;

val a = ListSet.emptyset;
val b = ListSet.addin 1 a;
val c = ListSet.isin 1 b;
val d = ListSet.removefrom 1 b;
val e = ListSet.isin 1 d;

(*------------------------------------------------*)

(* Es. 7 - Tree *)

datatype 'a T = Lf | Br of 'a * 'a T * 'a T

signature TREE =
sig
    type 'a T
    val countNodes: 'a T -> int
    val depth: 'a T -> int
    val mirror: 'a T -> 'a T
end;

(*------------------------------------------------*)

(* Es. 8 - Tree Structure *)

signature TREE =
sig
    datatype 'a T = Lf | Br of 'a * 'a T * 'a T
    val countNodes: 'a T -> int
    val depth: 'a T -> int
    val mirror : 'a T -> 'a T
end;

structure Tree: TREE = struct
    datatype 'a T = Lf | Br of 'a * 'a T * 'a T
    fun countNodes Lf = 0
      | countNodes (Br(_, l, r)) = 1 + countNodes(l) + countNodes(r)
    fun depth Lf = 0
      | depth (Br(_, l, r)) =
        1 + Int.max(depth l, depth r)
    fun mirror Lf = Lf
      | mirror (Br(v, l, r)) = Br(v, mirror(r), mirror(l))
end;

val a =
    Tree.Br(3,
        Tree.Br(2, Tree.Lf, Tree.Lf),
        Tree.Br(5, Tree.Br(4, Tree.Lf, Tree.Lf), Tree.Lf)
    );
val b = Tree.countNodes a;
val c = Tree.depth a;
val d = Tree.mirror a;

(*------------------------------------------------*)

(* Es. 9 - Map Tree *)

datatype 'a btree =
    Empty
  | Node of 'a * 'a btree * 'a btree

type ('a,'b) mapTree = ('a * 'b) btree

exception Missing

fun lookup lt Empty _ = raise Missing
  | lookup lt (Node((k,v), left, right)) a =
        if lt(a, k) then
            lookup lt left a
        else if lt(k, a) then
            lookup lt right a
        else
            v

fun intLt (x:int, y:int) = x < y

val t : (int, string) mapTree =
    Node((5,"five"),
        Node((3,"three"),
            Node((1,"one"), Empty, Empty),
            Node((4,"four"), Empty, Empty)
        ),
        Node((8,"eight"),
            Empty,
            Node((10,"ten"), Empty, Empty)
        )
    )

val x = lookup intLt t 4

(*------------------------------------------------*)

(* Es. 10 - Map Tree 2 *)

fun assign lt Empty a b =
    Node ((a, b), Empty, Empty)
  | assign lt (Node ((k, v), left, right)) a b =
      if lt(a, k) then
          Node ((k, v), assign lt left a b, right)
      else if lt(k, a) then
          Node ((k, v), left, assign lt right a b)
      else
          Node ((k, b), left, right);

val t0 = Empty;

val t1 = assign intLt t0 5 "five";
val t2 = assign intLt t1 3 "three";
val t3 = assign intLt t2 8 "eight";
val t4 = assign intLt t3 1 "one";
val t5 = assign intLt t4 4 "four";

val t6 = assign intLt t5 6 "six";
val t7 = assign intLt t6 3 "THREE";