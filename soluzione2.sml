datatype cell = Alive | Dead
type grid = cell list list

fun n_rows b = length b
fun n_cols [] = []
  | n_cols (x :: xs) = [length x] @ n_cols(xs)
fun cell_at_x_y g x y =
    if x < 0 orelse y < 0 orelse x >= n_rows g orelse y >= List.nth(n_cols g , x)
    then Dead
    else List.nth((List.nth(g, x)), y)

val neighbors_directions = [(~1,~1), (~1, 0), (~1, 1),(0,~1), (0,1),(1,~1), (1, 0), (1, 1)];
fun count_live_neighbors g x y =
    let
        fun cell_to_int Dead = 0
          | cell_to_int Alive = 1
        fun cell_alive (i, j) = cell_to_int(cell_at_x_y g (x + i) (y + j))
    in
        foldl (fn (d, acc) => cell_alive d + acc) 0 neighbors_directions
    end;

fun cell_next_state g x y =
    case (cell_at_x_y g x y, count_live_neighbors g x y)
    of (Alive, 2) => Alive
     | (Alive, 3) => Alive
     | (Dead, 3) => Alive
     | (_, _) => Dead

fun next_gen g =
    let
        fun new_row x y = if y >= List.nth(n_cols g , x) then [] else cell_next_state g x y :: new_row x (y+1)
        fun full_grid x = if x >= n_rows g then [] else new_row x 0 :: full_grid (x+1)
    in
        full_grid 0
    end;

fun print_grid grid =
    let
        fun showCell Alive = "O\t"
          | showCell Dead = ".\t"
        fun printRow row = (
            List.app (fn cell => print (showCell cell)) row;
            print "\n"
        )
    in
        List.app printRow grid;
        print "\n"
    end;

fun run_generations grid 0 = ()
  | run_generations grid n = (
        print_grid grid;
        run_generations (next_gen grid) (n - 1)
    )

val griglia = [
    [Alive, Dead , Alive, Dead , Alive],
    [Dead , Alive, Dead , Alive, Dead ],
    [Alive, Dead , Alive, Dead , Alive]];

n_rows griglia;
n_cols griglia;
cell_at_x_y griglia 0 1;
count_live_neighbors griglia 0 1;
cell_next_state griglia 0 1;
next_gen griglia;
print_grid griglia;
run_generations griglia 10;