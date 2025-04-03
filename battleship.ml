(**
Dans ce document est défini le projet à
rendre en fin de semestre pour l'année de {i L1
2024-2025}. Le programme est un jeu de bataille navale.
@version 1.2
@author Bentz POLO
@author Nadia MOUACHA
@author Zeneibou NIANG
@author Younes ETTABAA
*)

(* Conventions d'écritures:
  - Les fonctions qui proviennent de l'extérieur doivent etre
  précédées du nom de la bibliothèque d'origine. ex: "CPgraphics.open_graph()"
  - La declaration de types se fait dans la section "Types" ;
  celle des fonctions, dans la section "Functions".
  - Les paramètres doivent porter le préfixe "p_" ; et les variables locales, le préfixe "l_"
  - Chacun documente sa fonction au moment de l'implémenter

  Infos:
  - Les commandes pour l'interpréteur sont rassemblées dans "inter.ml".
  Vous pouvez simplement écrire '#use "inter.ml";;' quand vous tester
  vous fonctions avec l'interpréteur.
*)


(** <h1> Types </h1> *)
(** Type contenant les variables globales de
l'environnement de jeu. Prend en compte la marge
*)
type t_params = {
  margin : int ref; (** Paramètre réglant la marge entre la bordure d'écran et l'espace de jeu *)
  cell_size : int ref; (** Paramètre réglant la taille des cellules des grilles *)
  message_size : int ref; (** Paramètre réglant la taille de la zone d'affichage de message *)
  grid_size : int ref; (** Paramètre réglant la taille, en cellules, des grilles. *)
  ship_sizes : ((string * int) list) ref; (** Liste des bateaux et leurs tailles *)
};;

(** <h1>Functions</h1> *)
(**
Initialise les paramètres du jeu.
@param p_margin valeur de la marge, en pixels.
@param p_cell_size taille des cellules de grille, en pixels.
@param p_message_size hauteur de la zone d'affichage de message, en pixels.
@param p_grid_size taille de la grille, en cellules.
@param p_param structure contenant les paramètres du jeu.
@param ship_sizes Liste contenant des couples (bateau, taille)
@author Zeinebou NIANG
*)
let init_params(p_margin, p_cell_size, p_message_size, p_grid_size, p_ship_sizes, p_params : int * int * int * int * (string * int) list * t_params) : unit =
  p_params.margin := p_margin;
  p_params.cell_size := p_cell_size;
  p_params.message_size := p_message_size;
  p_params.grid_size := p_grid_size;
  p_params.ship_sizes := p_ship_sizes;
;;

(**
Fonction auxiliaire à {!val:display_empty_grids}. Dessine une grille et son système de coordonnées.
@param p_x coordonnée x du point inférieur gauche de la grille.
@param p_y coordonnée y du point inférieur gauche de la grille.
@param p_cell_size taille, en pixel, des cellules.
@param p_size taille, en cellules, de la grille.
@author Nadia MOUACHA
*)
let grid_displayer (p_x, p_y, p_cell_size, p_size : int * int * int * int) : unit =
  let l_x : int ref = ref (p_x + p_cell_size)
  and l_y : int ref = ref p_y
  in
  (* Lignes horizontales et les chiffres *)
  for i=p_size downto 0 do
    if i = 0 then (* Si i=0 on trace simplement la ligne et on n'affiche pas de coordonnée *)
      (CPgraphics.moveto(!l_x, !l_y);
      CPgraphics.lineto(!l_x + (p_cell_size * p_size), !l_y))
    else
      (CPgraphics.moveto(p_x, !l_y);
      CPgraphics.draw_string(string_of_int(i));
      CPgraphics.moveto(!l_x, !l_y);
      CPgraphics.lineto(!l_x + (p_cell_size * p_size), !l_y));
    l_y := !l_y + p_cell_size;
  done;

  l_y := p_y ; (* Réinitialisation de l_y pour revenir au y d'origine *)

  (* Lignes verticales et les lettres *)
  for i=1 to p_size + 1 do
    if i=(p_size + 1) then (* Si i=p_size + 1, on trace la ligne mais on n'affiche pas de coordonnée *)
      (CPgraphics.moveto(!l_x, !l_y);
      CPgraphics.lineto(!l_x, !l_y + (p_cell_size * p_size)))
    else
      (CPgraphics.moveto(!l_x, !l_y);
      CPgraphics.lineto(!l_x, !l_y + (p_cell_size * p_size));
      CPgraphics.draw_char(char_of_int(64 + i)));
    l_x := !l_x + p_cell_size
  done
;;

(**
Réalise l'affichage des deux grilles du jeu, ainsi que les noms de joueurs.
@param p_cell_size taille des cellules, en pixels.
@param p_grid_size taille des grille, en cellules.
@param p_margin taille de la marge, en pixels.
@param p_message_size hauteur de la zone d'affichage de messages, en pixels.
@author Nadia MOUACHA
*)
let display_empty_grids(p_size, p_cell_size, p_margin, p_message_size : int * int * int * int) : unit =
  let l_x : int ref = ref (p_margin)
  and l_y : int ref = ref (p_margin + p_message_size)
  and grid_px : int = p_size * p_cell_size
  in
  CPgraphics.moveto(0, 0);
  grid_displayer(!l_x, !l_y, p_cell_size, p_size); (* grille ordinateur *)
  l_x := !l_x + p_cell_size + grid_px + p_margin ;
  grid_displayer(!l_x, !l_y, p_cell_size, p_size);(* grille joueur *)
  l_y := !l_y + grid_px + (p_cell_size * 2);
  CPgraphics.moveto(!l_x + p_cell_size, !l_y);
  CPgraphics.draw_string("Ordinateur");
  l_x := p_margin + p_cell_size;
  CPgraphics.moveto(!l_x, !l_y);
  CPgraphics.draw_string("Joueur")
;;

(**
Place un bateau sur la grille.
@param p_ship bateau à placer.
@p_grid grille dans où il faut placer le bateau.
@author Bentz POLO
*)
let rec place_ship(p_positions, p_grid : (char * int) list * t_grid): unit =
  if List.is_empty(p_positions) then
    ()
  else
  let l_target : (int * int) = cell_index(List.hd(p_positions))
  in
  p_grid.(snd(l_target)).(fst(l_target)) <- (List.hd(p_positions), OCCUPIED);
  place_ship(List.tl(p_positions), p_grid)
;;

(**
Transforme les coordonnée d'une cellule en celles du pixel en bas à gauche de cette cellule.
@param p_coords coordonnées de la cellules.
@param init_coords coordonnées initiale de la grille.
@param taille des cellules en pixel.
@param p_grid_size taille de la grille en pixel.
@return les coordonnées du pixel inférieur gauche de la cellule.
@author Bentz POLO
*)
let cell_to_pixel(p_coords, init_coords, p_cell_size, p_grid_size : (char * int) * (int * int) * int * int) : int * int =
  let l_x : int = ((int_of_char(fst(p_coords)) - int_of_char('A')) * p_cell_size)
    + fst(init_coords)
    + p_cell_size (* p_cell_size ajouté pour compense le decalage causé par l'affichage des chiffres *)
  and l_y : int = ((p_grid_size - snd(p_coords)) * p_cell_size) +snd(init_coords)
  in
  (l_x, l_y)
;;

(**
Colore une cellule donnée.
@param p_coords coordonnée de la cellule à colorer.
@param p_color couleur à utiliser poue colorer la case.
@param p_params paramètres du jeu.
@author Bentz POLO
*)
let color_cell(p_coords, p_color, p_params : (char * int) * CPgraphics.t_color * t_params) : unit =
  let (l_x , l_y) : int * int = cell_to_pixel(p_coords,(!(p_params.margin), !(p_params.margin) + !(p_params.message_size)), !(p_params.cell_size), !(p_params.grid_size))
  in
  CPgraphics.set_color(p_color);
  CPgraphics.fill_rect(l_x, l_y, !(p_params.cell_size), !(p_params.cell_size))
;;

(* NOTE: List.nth ne fonctionne pas avec les conventions
d'écriture établies en cours*)
(**
Scanne la liste de bateau placés et colores le cases qu'ils occupents
@param p_grid representation matricielle de la grille
@param p_params paramètres du jeu
@author Bentz POLO
*)
let display_grid(p_ships, p_grid , p_params : t_ship list * t_grid * t_params) : unit =
  for i=0 to List.length(p_ships) - 1 do
    for j=0 to List.length((List.nth p_ships i).positions) - 1 do
      color_cell((List.nth (List.nth p_ships i).positions j), CPgraphics.grey, p_params)
    done
  done
;;

(**
Fonction principale du jeu. Lance la fenetre graphique et mets à jour le titre et
enclenche le déroulement du jeu.
@author Zeinebou NIANG
*)
let battleship_game() : unit =
  let settings : t_params = {
    margin = {contents = 0};
    cell_size = {contents = 0};
    message_size = {contents = 0};
    grid_size = {contents = 0};
    ship_sizes = {contents = []};
    }
  in
  init_params(30, 15, 60, 10,
    [("Porte-avions", 5); ("Croiseur", 4); ("Contre-torpilleur", 3); ("Contre-torpilleur", 3); ("Torpilleur", 2)], settings);
  CPgraphics.open_graph(410,290);
  CPgraphics.set_window_title("Battleship Game");
  (* NOTE: Pour l'instant set_text_size renvoit une erreur,
  qui provient du fait qu'il ne trouve la police de caractère*)
  (* CPgraphics.set_text_size( !(settings.cell_size) ); *)
  display_empty_grids(!(settings.grid_size), !(settings.cell_size), !(settings.margin), !(settings.message_size));
  CPgraphics.wait(600)
;;

(* Appel de la fonction principale pour lancer le jeu *)
battleship_game();;
