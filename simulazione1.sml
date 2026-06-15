datatype Natural = Zero | Integer of int
datatype tree = Empty | Leaf of Natural | Node of Natural * tree * tree

fun filter_tree (Empty, F) = Empty
  | filter_tree (Leaf(Zero), F) = Leaf(Zero)
  | filter_tree (Leaf(Integer(v)), F) = if F(v) then Leaf(Integer(v)) else Leaf(Zero)
  | filter_tree (Node(Zero, l, r), F) = Node(Zero, filter_tree(l, F), filter_tree(r, F))
  | filter_tree (Node(Integer(v), l, r), F) = if F(v) then Node(Integer(v), filter_tree(l, F), filter_tree(r, F))
                                                      else Node(Zero, filter_tree(l, F), filter_tree(r, F));

val test_tree = Node(Integer 1, Node(Zero, Leaf(Integer 2), Leaf (Integer 5)), Leaf (Integer 4));

fun even n = n mod 2 = 0;
fun odd n = n mod 2 <> 0;

filter_tree(test_tree, odd);
filter_tree(test_tree, even);