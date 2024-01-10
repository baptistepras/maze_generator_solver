open Maze 

let printMur numero_ligne indice = 

match numero_ligne mod 2 , indice mod 2 with
| 0 , 0       -> Printf.printf"+"
| 0 , 1       -> Printf.printf"-"
| 1 , 0 | 1, 1-> Printf.printf"|"
| _, _ -> failwith"Probleme avec le modulo" (* cette ligne ne se fait jamais, c'est pur eviter les warnings du pattern matching *)


let printCaractere (layout: int array array) numero_ligne indice = 
  let i1 = layout.(numero_ligne).(indice) in
  match i1 with 
  | 0 -> printMur numero_ligne indice
  | 1 -> Printf.printf" "
  | 2 -> Printf.printf"E"
  | 3 -> Printf.printf"S"
  | _ -> failwith"Valeur interdite dans le layout"

let  printLigne (layout : int array array) numero_ligne indice_max = 
  let rec sousPrintLigne (layout : int array array) numero_ligne indice indice_max =
    if indice > indice_max then (
      Printf.printf"\n"
    )
    else(
      printCaractere layout numero_ligne indice;
      sousPrintLigne layout numero_ligne (indice + 1 ) indice_max ;) in
      
  sousPrintLigne layout numero_ligne 0 indice_max

let rec sousAffiche layout numero_ligne ligne_max indice_max = 
  if numero_ligne > ligne_max then(
    Printf.printf"\n"
  )else(
    printLigne layout numero_ligne indice_max;
    sousAffiche layout (numero_ligne + 1) ligne_max indice_max
  )

let afficheLabyrinthe labyrinthe = 
  let indice_max = labyrinthe.murs_largeur - 1 in
  let ligne_max = labyrinthe.murs_hauteur - 1 in
  sousAffiche labyrinthe.murs 0 ligne_max indice_max;

  ()


let print_2d_array arr =
  Array.iter (fun row ->
    Array.iter (fun elem -> print_int elem; print_string " ") row;
    print_newline ()
  ) arr



let print_line line =
  String.iter (fun c -> let s = transforme c in Printf.printf"%d " s) line;
  Printf.printf"\n"
  
let print_list liste = List.iter (fun x -> Printf.printf"%s\n" x) liste

let printLaby labyrinthe = 
  Printf.printf "Hauteur : %d\n" labyrinthe.hauteur; 
  Printf.printf "Largeur : %d\n" labyrinthe.largeur;
  Printf.printf "Largeur des murs : %d\n" labyrinthe.murs_largeur;
  Printf.printf "Hauteur des murs : %d\n" labyrinthe.murs_hauteur;
  Printf.printf "Plan des murs : \n";
  print_2d_array labyrinthe.murs 
  


let printDirection d = 
  match d with 
  | N -> Printf.printf"N "
  | E -> Printf.printf"E "
  | O -> Printf.printf"O "
  | S -> Printf.printf"S "
let printDirections (directions : direction list) = List.iter printDirection directions

let print_list_int liste = List.iter print_int liste
  

let () = Random.self_init  () 
let laby = Maze.genereLabyrinthe 10 10
let () = afficheLabyrinthe laby
let () = Printf.printf"\n"