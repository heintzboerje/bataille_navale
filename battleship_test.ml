(* Objectif de test: test unitaire du module battleship_game *)

(* Chargement de la bibliothèque de test et du fichier source *)
open battleship ;;
open CPtest ;;

(* Initialise la campagne de test *)
test_reset_report() ;;

(* Bouchon de test *)
let settings : t_params = {
  margin = {contents = 0};
  cell_size = {contents = 0};
  message_size = {contents = 0};
  grid_size = {contents = 4};
  ship_sizes = {contents = []};
};;

(* TESTS ITERATION 2 *)

(* Test fonctionnel cell_index *)
let test_cell_index_func () : unit =
  let (l_res0, l_res1, l_res2) :  (int * int) t_test_result * (int * int) t_test_result * (int * int) t_test_result =(
    test_exec(cell_index, "cell_index   ('D', 4)", ('D', 4)),
                test_exec(cell_index, "cell_index   ('A', 0)", ('A', 1)),
                test_exec(cell_index, "cell_index   ('F', 7)", ('F', 7)) )
  in
        assert_equals_result((3,3), l_res0);
        assert_equals_result((0,0), l_res1);
        assert_equals_result((5, 6), l_res2)
;;

(* Test fonctionnel de generate_grid_matrix *)
let test_generate_grid_matrix_func () : unit =
  let l_res : t_grid t_test_result =
    test_exec(generate_grid_matrix, "generate_grid_matrix   génère une matrice repésentant une grille", 10)
  in
    assert_equals(10, Array.length(test_get(l_res))) ; (* taille de la matrice *)
    assert_equals(10, Array.length((test_get(l_res)).(4))) ; (* taille des tableaux de la matrice *)
    assert_equals(('E', 4), (test_get(l_res)).(3).(4).coord)
;;

(* Test fonctionnel place_ship *)
let test_place_ship_struc () : unit =
  let l_test_grid = [|
    [|{coord = ('A', 1); state = {contents = EMPTY}}; {coord = ('B', 1); state = {contents = EMPTY}}; {coord = ('C', 1); state = {contents = EMPTY}}; {coord = ('D', 1); state = {contents = EMPTY}};|];
    [|{coord = ('A', 2); state = {contents = EMPTY}}; {coord = ('B', 2); state = {contents = EMPTY}}; {coord = ('C', 2); state = {contents = EMPTY}}; {coord = ('D', 2); state = {contents = EMPTY}};|];
    [|{coord = ('A', 3); state = {contents = EMPTY}}; {coord = ('B', 3); state = {contents = EMPTY}}; {coord = ('C', 3); state = {contents = EMPTY}}; {coord = ('D', 3); state = {contents = EMPTY}};|];
    [|{coord = ('A', 4); state = {contents = EMPTY}}; {coord = ('B', 4); state = {contents = EMPTY}}; {coord = ('C', 4); state = {contents = EMPTY}}; {coord = ('D', 4); state = {contents = EMPTY}};|]
    |]
  in let l_res : unit t_test_result =
    test_exec(place_ship, "place_ship   bateau vertical", ([('B', 1); ('B', 2)], l_test_grid));
    test_exec(place_ship, "place_ship   bateau horizontal", ([('A',4); ('B',4)], l_test_grid));
  in
    assert_equals(OCCUPIED, !(l_test_grid.(0).(1).state));
    assert_equals(OCCUPIED, !(l_test_grid.(1).(1).state));
    assert_equals(OCCUPIED, !(l_test_grid.(3).(0).state));
    assert_equals(OCCUPIED, !(l_test_grid.(3).(1).state))
;;

(* Tests structurel positions list *)
(* Couverture cas où la direction est vers le haut *)
let test_positions_list_struc_up () : unit =
  let l_res : (char * int) list t_test_result =
    test_exec(positions_list, "positions_list   direction = UP", (('N', 6), UP, 5))
  in
    assert_equals_result([('N', 6); ('N', 5); ('N', 4); ('N', 3); ('N', 2)], l_res)
;;

(* Couverture cas où la direction est vers le bas *)
let test_positions_list_struc_down () : unit =
  let l_res : (char * int) list t_test_result =
    test_exec(positions_list, "positions_list   direction = DOWN", (('N', 6), DOWN, 5))
  in
    assert_equals_result([('N', 6); ('N', 7); ('N', 8); ('N', 9); ('N', 10)], l_res)
;;


(* Couverture cas où la direction est vers la gauche *)
let test_positions_list_struc_left () : unit =
  let l_res : (char * int) list t_test_result =
    test_exec(positions_list, "positions_list   direction = LEFT", (('N', 6), LEFT, 5))
  in
    assert_equals_result([('N', 6); ('M', 6); ('L', 6); ('K', 6); ('J', 6)], l_res)
;;


(* Couverture cas où la direction est vers la droite *)
let test_positions_list_struc_right () : unit =
  let l_res : (char * int) list t_test_result =
    test_exec(positions_list, "positions_list   direction = RIGHT", (('N', 6), RIGHT, 5))
  in
    assert_equals_result([('N', 6); ('O', 6); ('P', 6); ('Q', 6); ('R', 6)], l_res)
;;

(* Test structurels can_place_ship *)
(* Couverture cas débordement *)
let test_can_place_ship_struc_deb () : unit =
  let l_test_grid = [|
    [|{coord = ('A', 1); state = {contents = EMPTY}}; {coord = ('B', 1); state = {contents = EMPTY}}; {coord = ('C', 1); state = {contents = EMPTY}}; {coord = ('D', 1); state = {contents = EMPTY}};|];
    [|{coord = ('A', 2); state = {contents = EMPTY}}; {coord = ('B', 2); state = {contents = EMPTY}}; {coord = ('C', 2); state = {contents = EMPTY}}; {coord = ('D', 2); state = {contents = EMPTY}};|];
    [|{coord = ('A', 3); state = {contents = EMPTY}}; {coord = ('B', 3); state = {contents = EMPTY}}; {coord = ('C', 3); state = {contents = EMPTY}}; {coord = ('D', 3); state = {contents = EMPTY}};|];
    [|{coord = ('A', 4); state = {contents = EMPTY}}; {coord = ('B', 4); state = {contents = EMPTY}}; {coord = ('C', 4); state = {contents = EMPTY}}; {coord = ('D', 4); state = {contents = EMPTY}};|]
    |]
  in
  let l_res : bool t_test_result = test_exec(can_place_ship, "can_place_ship   test débordement", (('C', 3), RIGHT, 3, l_test_grid, settings))
  in
  assert_equals_result(false, l_res)
;;

(* Couverture cas collision *)
let test_can_place_ship_struc_coll () : unit =
  let l_test_grid = [|
    [|{coord = ('A', 1); state = {contents = EMPTY}}; {coord = ('B', 1); state = {contents = EMPTY}}; {coord = ('C', 1); state = {contents = EMPTY}}; {coord = ('D', 1); state = {contents = EMPTY}};|];
    [|{coord = ('A', 2); state = {contents = EMPTY}}; {coord = ('B', 2); state = {contents = EMPTY}}; {coord = ('C', 2); state = {contents = EMPTY}}; {coord = ('D', 2); state = {contents = EMPTY}};|];
    [|{coord = ('A', 3); state = {contents = EMPTY}}; {coord = ('B', 3); state = {contents = OCCUPIED}}; {coord = ('C', 3); state = {contents = EMPTY}}; {coord = ('D', 3); state = {contents = EMPTY}};|];
    [|{coord = ('A', 4); state = {contents = EMPTY}}; {coord = ('B', 4); state = {contents = EMPTY}}; {coord = ('C', 4); state = {contents = EMPTY}}; {coord = ('D', 4); state = {contents = EMPTY}};|]
    |]
  in
  let l_res : bool t_test_result = test_exec(can_place_ship, "can_place_ship   test collision", (('C', 3), LEFT, 3, l_test_grid, settings))
  in
  assert_equals_result(false, l_res)
;;

(* Couverture cas positions libres *)
let test_can_place_ship_struc_coll () : unit =
  let l_test_grid = [|
    [|{coord = ('A', 1); state = {contents = EMPTY}}; {coord = ('B', 1); state = {contents = EMPTY}}; {coord = ('C', 1); state = {contents = EMPTY}}; {coord = ('D', 1); state = {contents = EMPTY}};|];
    [|{coord = ('A', 2); state = {contents = EMPTY}}; {coord = ('B', 2); state = {contents = EMPTY}}; {coord = ('C', 2); state = {contents = EMPTY}}; {coord = ('D', 2); state = {contents = EMPTY}};|];
    [|{coord = ('A', 3); state = {contents = EMPTY}}; {coord = ('B', 3); state = {contents = EMPTY}}; {coord = ('C', 3); state = {contents = EMPTY}}; {coord = ('D', 3); state = {contents = EMPTY}};|];
    [|{coord = ('A', 4); state = {contents = EMPTY}}; {coord = ('B', 4); state = {contents = EMPTY}}; {coord = ('C', 4); state = {contents = EMPTY}}; {coord = ('D', 4); state = {contents = EMPTY}};|]
    |]
  in
  let l_res : bool t_test_result = test_exec(can_place_ship, "can_place_ship   test collision", (('C', 3), LEFT, 3, l_test_grid, settings))
  in
  assert_equals_result(true, l_res)
;;

(* appel des fonctions de test *)
test_cell_index_func () ;;
test_generate_grid_matrix_func () ;;
test_place_ship_struc () ;;
test_positions_list_struc_down () ;;
test_positions_list_struc_left () ;;
test_positions_list_struc_right () ;;
test_positions_list_struc_up () ;;
test_can_place_ship_struc_coll () ;;
test_can_place_ship_struc_coll () ;;
test_can_place_ship_struc_coll () ;;

test_report () ;;

(*Tests Itération 4*)


(* Test de update_grid *)
let test_update_grid_func () : unit =
  let test_grid : t_grid = generate_grid_matrix(4)
  and l_res : unit test_result = test_exec(update_grid, "update_grid : cellule ('A',1) doit devenir TOUCHED", (('A', 1), TOUCHED, test_grid))
  in
  assert_equals(TOUCHED, !(grid.(0).(0).state))
;;

(* Test de sink_ship *)
let test_sink_ship_func () : unit =
  let test_grid = generate_grid_matrix(5)
  and ship_positions = [('A',1); ('A',2); ('A',3)]
  in
  place_ship(ship_positions, test_grid);
  let l_res : unit test_result = test_exec(sink_ship, "sink_ship : la case doit être marquée DESTROYED", (('A',1), test_grid))
  in
  assert_equals(DESTROYED, !(grid2.(i).(j).state))
;;

(* Test de check_sunk_ship *)
let test_check_sunk_ship () : unit =
  let test_grid = generate_grid_matrix(5)
  and test_ship = { name = "Petit bateau"; positions = [('B',1); ('B',2)]}
  in
  place_ship(test_ship.ship_positions, test_grid);
  sink_ship(test_ship, test_grid);
  let l_res : bool test_result = test_exec(check_sunk_ship, "check_sunk_ship : bateau entièrement touché => true", (test_ship, test_grid));
  in
  assert_equals_result(true, l_res);

(* Test de find_ship *)
let test_find_ship () : unit =
  let test_ships = [
    { name = "Croiseur"; positions = [('A',1); ('A',2)] };
    { name = "Torpilleur"; positions = [('C',3); ('C',4)] }
  ]
  in
  let test_grid = generate_grid_matrix(5) in
  let l_res : t_ship t_test_result =  test_exec(find_ship, "find_ship : doit trouver un navire", (ships, grid4, params)) in
  assert_equals(found_ship.name = "");

  (* Tests de player_shoot *)

  (* 1. Tir raté : case vide *)
  let grid5 = create_empty_grid 5 in
  let ships5 = [] in
  player_shoot (grid5, ships5, params);
  (* Impossible de vérifier exactement où il tire sans adaptation de player_shoot, donc on saute la vérif ici *)

  (* 2. Tir touché : case avec un bateau *)
  let grid6 = create_empty_grid 5 in
  place_ship grid6 [('E',5)];
  let ships6 = [{ name = "Torpilleur"; positions = [('E',5)] }] in
  player_shoot (grid6, ships6, params);
  assert_equals_m "player_shoot : case tirée doit devenir TOUCHED" TOUCHED !(grid6.(4).(4).state);

  (* 3. Tir coulé : dernier morceau d'un bateau *)
  let grid7 = create_empty_grid 5 in
  place_ship grid7 [('C',1); ('C',2)];
  let ships7 = [{ name = "Destroyer"; positions = [('C',1); ('C',2)] }] in
  update_grid (('C',1), TOUCHED, grid7); (* Déjà touché *)
  player_shoot (grid7, ships7, params);
  assert_equals_m "player_shoot : toutes les cases doivent être DESTROYED" DESTROYED !(grid7.(2).(0).state);
  assert_equals_m "player_shoot : toutes les cases doivent être DESTROYED" DESTROYED !(grid7.(2).(1).state);

  (* Test simple de display_grid *)
  let grid8 = create_empty_grid 5 in
  display_grid (grid8, params, JOUEUR);
  assert_true_m "display_grid : affichage sans crash" true;

  test_report ()
;;
