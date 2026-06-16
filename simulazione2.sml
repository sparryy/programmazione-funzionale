datatype cella = Alive | Dead;
datatype grid = Empty | matrice of cella list list;

fun n_rows ([]) = 0
  | n_rows (x) = 1 + n_rows(tl(x));
fun n_cols ([]) = []
  | n_cols (x) = n_rows(hd(x))::n_cols(tl(x));
fun cell_at_x_y (x, y, g) = if x < 0 orelse y < 0 orelse y >= n_rows(g) orelse x >= List.nth(n_cols(g), x)
                            then Dead
                            else List.nth(List.nth(g, y), x);
fun cell_int (x, y, g) = let
        val cols = n_cols(g)
        val rows = n_rows(g)
        val xcol = List.nth(cols, y)
    in
        if x < 0 orelse y < 0 orelse y >= rows orelse x >= xcol then 0 else
            if cell_at_x_y(x, y, g) = Alive then 1 else 0
    end;
fun count_live_neighbours (x, y, g) = cell_int(x-1, y-1, g) +
                                      cell_int(x-1, y, g) +
                                      cell_int(x-1, y+1, g) +
                                      cell_int(x, y-1, g) +
                                      cell_int(x, y+1, g) +
                                      cell_int(x+1, y-1, g) +
                                      cell_int(x+1, y, g) +
                                      cell_int(x+1, y+1, g);
fun cell_next_state (x, y, g) = if cell_at_x_y(x, y, g) = Alive
                                then
                                    if count_live_neighbours(x, y, g) = 2 orelse count_live_neighbours(x, y, g) = 3
                                    then Alive
                                    else Dead
                                else
                                    if count_live_neighbours(x, y, g) = 3
                                    then Alive
                                    else Dead;


val griglia = [
    [Alive, Dead , Alive, Dead , Alive],
    [Dead , Alive, Dead , Dead , Dead],
    [Alive, Alive, Alive, Alive, Alive]];

n_rows(griglia);
n_cols(griglia);
cell_at_x_y(1, 1, griglia);
count_live_neighbours(1, 1, griglia);
cell_next_state(1, 1, griglia);
cell_next_state(4, 0, griglia);
