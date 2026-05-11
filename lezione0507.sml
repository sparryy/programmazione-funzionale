(* Lezione 7/5 *)

(*------------------------------------------------*)

(* Es. 1 - Is on *)

datatype ('label) tree =
    Node of 'label * 'label tree list;

fun isOn x (Node(v, t)) = if x = v then true else foldr (fn(z,y) => z orelse y) false (map (isOn x) t);

isOn 3 (Node(2, [Node (3,nil), Node(5,nil)]));

(*------------------------------------------------*)

(* Es. 2 - Count *)

fun count x (Node(v, t)) = (if x = v then 1 else 0) + foldr (fn(z,y) => z + y) 0 (map (count x) t);

count 2 (Node(2, [Node (3,nil), Node(2,nil)]));

(*------------------------------------------------*)

(* Es. 3 - Depth*)

fun depth (Node(v, t)) = 1 + foldr (fn(x,y) => if x < y then y else x) 0 (map depth t);

depth (Node(2, [Node (3, [Node(4,nil)]), Node(2,nil)]));

(*------------------------------------------------*)

(* Es. 4 - Prenoto *)

fun preOrder (Node(v, t)) = [v] @ foldr (fn(x,y) => x@y) [] (map preOrder t);

preOrder (Node(6, [Node (3, [Node(4,nil)]), Node(2,nil)]));

(*------------------------------------------------*)

(* Es. 5 - MyTree *)

exception Missing;

structure Tree = struct
    type 'a tree = 'a tree
    fun create(a) = Node(a, nil)
    fun build(a, L) = Node(a, L)
    fun subtree(i, (Node(x, nil))) = raise Missing
      | subtree(i, (Node(x, T))) = if i = 1 then hd(T) else subtree(i-1, Node(x, tl(T)))
end;

(*------------------------------------------------*)

(* Es. 6 - Simple Tree *)

signature SIMPLE =
sig
    type st = int tree
    val build: int * st list -> st
    val subtree: int * st -> st
end;

structure SimpleTree: SIMPLE = struct
    type st = int tree
    fun build(a, L) = Tree.build(a, L)
    fun subtree(i, t) = Tree.subtree(i, t)
end;

(*------------------------------------------------*)

(* Es. 7 - Creazione Albero *)

val MyTree = SimpleTree.build(1, [SimpleTree.build(2, nil), SimpleTree.build(3, nil), SimpleTree.build(4, nil)]);

val second = SimpleTree.subtree(2, MyTree);

(*------------------------------------------------*)

(* Es. 8 - Stack *)

exception EmptyStack;

structure Stack = struct
    type 'a stack = 'a list
    fun create() = []
    fun push x s = x::s
    fun pop s = if s = nil then raise EmptyStack else tl(s)
    fun isEmpty s = s = nil
    fun top s = if s = nil then raise EmptyStack else hd(s)
end;

(*------------------------------------------------*)

(* Es. 9 - String Stack *)

signature STRINGS =
sig
    type t = string list
    val create: string list
    val push: string -> string list -> string list
    val pop: string list -> string list
    val isEmpty: string list -> bool
end;

structure StringStack: STRINGS = struct
    type t = string list
    val create = Stack.create()
    fun push x s = Stack.push x s
    fun pop s = Stack.pop s
    fun isEmpty s = Stack.isEmpty s
end;

(*------------------------------------------------*)

(* Es. 10 - Queue *)

exception EmptyQueue;

structure Queue = struct
    type 'a queue = 'a list
    fun create() = []
    fun enqueue a q = q::a
    fun dequeue q = if q = nil then raise EmptyQueue else (hd(q), tl(q))
    fun isEmpty q = if q = nil then true else false
end;

(*------------------------------------------------*)

(* Es. 11 - Pair Queue *)

signature PAIRQUEUE =
sig
    type t = (string*int) list
    val create: (string*int) list
    val enqueue: (string*int) -> (string*int) list -> (string*int) list
    val dequeue: (string*int) list -> (string*int) * (string*int) list
    val isEmpty: (string*int) list -> bool
end;

structure PairQueue: PAIRQUEUE = struct
    type t = (string*int) list
    val create = Queue.create()
    fun enqueue a q = Queue.enqueue a q
    fun dequeue q = Queue.dequeue q
    fun isEmpty q = Queue.isEmpty q
end;