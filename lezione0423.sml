(* Lezione 23/4 *)

(* Commento per exec
(*------------------------------------------------*)

(* Es. 1 - Curry *)

fun curry F x y z = F(x,y,z);

val G = curry(fn (x,y,z)=>x*y*z);

G 1 2 3;

(*------------------------------------------------*)

(* Es. 2 - To real *)

val f = map real;
f [1,2,3];

(*------------------------------------------------*)

(* Es. 3 - And booleano *)

val andb = foldl(fn (a,b) => a andalso b) true [true, false, true];

(*------------------------------------------------*)

(* Es. 4 - Implodi *)

val imp = foldr(fn (a,b) => str(a)^b) "" [#"b",#"c"];

(*------------------------------------------------*)

(* Es. 5 - Binary Tree *)

datatype 'a btree =
    Empty |
    Node of 'a * 'a btree * 'a btree;

fun postOrder (Empty) = []
  | postOrder (Node(v,l,r)) = postOrder(l) @ postOrder(r) @ [v];

postOrder(Node ("ML", Node ("as", Node ("a", Empty, Empty), Node ("in", Empty, Empty)), Node ("types", Empty, Empty)));

(*------------------------------------------------*)

(* Es. 6 - Binary Tree 2 *)

fun inOrder (Empty) = []
  | inOrder (Node(v,l,r)) = inOrder(l) @ [v] @ inOrder(r);

inOrder(Node ("ML", Node ("as", Node ("a", Empty, Empty), Node ("in", Empty, Empty)), Node ("types", Empty, Empty)));

(*------------------------------------------------*)

(* Es. 7 - Map Tree *)

datatype ('a,'b) mapTree =
    Empty |
    Node of ('a*'b) * ('a,'b) mapTree * ('a,'b) mapTree;

(*------------------------------------------------*)

(* Es. 8 - Sum Tree *)

fun sumTree (Empty) = 0
  | sumTree (Node((x,y),l,r)) = y+sumTree(l)+sumTree(r);

sumTree (Node(("a",1), Node(("c",2), Empty, Node(("d",3), Empty, Empty)), Empty));

(*------------------------------------------------*)

*)
(* Es. 9 - Conta nodi interni *)

(* poi elimina *)
datatype 'a btree = 
  Empty |
  Node of 'a * 'a btree * 'a btree;

fun countInternalNodes F Empty = 0
  | countInternalNodes F Node(v, l, r) =
    if F(v) = true then 