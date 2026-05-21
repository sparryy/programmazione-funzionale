datatype cella = Alive | Dead;

datatype grid = matrice of cella list list;
(*[[][][][]]*)

structure Automa = struct
    type t = grid
    fun n_rows([]) = 0
      | n_rows(a) = 1 + n_rows(tl(a))
    fun len [] = 0
    |   len (_::t) = 1+len(t)
    fun n_cols [] = []
    |   n_cols (riga::a) = len riga :: n_cols(a)
    and count([]) = 0
     | count(a) = 1+count(tl(a))
    (*se non è fuori dai bounds allora ritorno la cella a posizione x y*)
    fun cell_at_x_y (x,y) a = if (x < 0 orelse x >= List.nth(n_cols(a),x)) andalso (y < 0 orelse y >= n_rows(a)) then Dead else List.nth(List.nth(a, y), x)
    (*funzione ausiliaria per booleano al posto di dead o alive*)
    fun cellAlive(a, (x,y)) = let 
        val nRows = n_rows(a)
        val nCols = n_cols(a)
        val lenCol = List.nth(nCols, x)
        in
        if x>nRows orelse y>lenCol orelse x<0 orelse y<0 
        then false
        else if List.nth(List.nth(a,x),y) = Alive then true else false
        end;

    fun count_live_neighbours (a,(x,y)) = cellAliveInt(a,(x-1,y-1))
                                        + cellAliveInt(a,(x-1,y))
                                        + cellAliveInt(a,(x-1,y+1))
                                        + cellAliveInt(a,(x,y-1))
                                        + cellAliveInt(a,(x,y+1))
                                        + cellAliveInt(a,(x+1,y-1))
                                        + cellAliveInt(a,(x+1,y))
                                        + cellAliveInt(a,(x+1,y+1))
    (*funzione ausiliaria per rendere la somma più semplice*)
    and cellAliveInt(a, (x,y)) = if cellAlive(a, (x,y)) then 1 else 0
    
    fun cell_next_state(a, (x,y)) = 
        let 
            val aliveNeighbours = count_live_neighbours(a,(x,y))
        in 
            if cellAlive(a,(x,y)) 
            then if aliveNeighbours = 2 orelse aliveNeighbours = 3 then Alive else Dead
            else if aliveNeighbours = 3 then Alive else Dead
        end
    (*ciclo lista cicla per ogni sottolista della grid e per ogni elemento ci chiama un ciclo cella, che poi chiama next state*)
    fun next_gen a = cicloLista(0, a)
    and cicloLista (y, a) = if a = nil then [] else cicloCella(0, y, hd(a), a)::cicloLista(y+1, tl(a))
    and cicloCella (x, y, b, a) = if b = nil then [] else cell_next_state(a, (x,y))::cicloCella(x+1, y, tl(b), a)

    fun print_grid a = cicloLista1(0, a)
    and cicloLista1 (y, a) = if a = nil then [] else (cicloCella1(0, y, hd(a), a); print("\n"); cicloLista1(y+1, tl(a)))
    and cicloCella1 (x, y, b, a) = if b = nil then [] else print_cella(a, (x,y))::cicloCella1(x+1, y, tl(b), a)
    and print_cella(a, (x,y)) = if cellAlive(a, (x,y)) then print("|O|") else print("|.|")
    
    fun run_generations (n, a) = if n = 1 then next_gen a else (next_gen a; run_generations(n-1, a))
end;

val griglia = [
    [Alive, Dead , Alive, Dead , Alive],
    [Dead , Alive, Dead , Dead , Dead],
    [Alive, Alive, Alive, Alive, Alive]];


Automa.n_rows(griglia);
Automa.cellAlive(griglia,(0,0));
Automa.n_cols(griglia);
Automa.cell_at_x_y (0,0) griglia;
Automa.cellAlive(griglia,(0,0));
Automa.count_live_neighbours(griglia,(1,1));
Automa.cell_next_state(griglia,(0,1));
Automa.next_gen griglia;
Automa.print_grid(griglia);
Automa.run_generations(2,griglia);

(*Ci scusiamo per il disagio provocato dalla correzione di questo codice
Di solito siamo più bravi
Buona giornat 
 Team 28*)
