let () = Printf.printf"Begin...\n"

type labyrinthe = {
  taille : int*int;
  walls : int array array;
  depart : int*int;
  arrivee : int*int;
}

let path = "test/maze_4x8.laby"

let suite l = 
  match l with 
  | []    -> []
  | e::ll -> ll 

let string_to_char_list (s : string) : char list =
  s |> String.to_seq |> List.of_seq
  
 
let load_file file_path =
  let in_c = open_in file_path in
 
  let rec aux file =
    try
      let line = string_to_char_list(input_line file) in
      line::aux file
    with
      End_of_file -> []  in
  aux in_c

let transforme c = 
  
  match c with 
  | ' ' -> 1
  | 'E' -> 2
  | 'S' -> 3
  | '+' | '-' | '|' -> 0
  | _   -> failwith"Caractère %s interdit" c



let rec transforme_ligne ligne taille indice tableau largeur indice_ligne hauteur = 
  
  if taille < 0 then failwith"Ligne trop grande" else

  match ligne with 
  | c::ll ->
    let b = transforme(c) in
    tableau.(indice) <- b; 
    if (indice = 0 || indice = largeur-1 || indice_ligne = 0 || indice_ligne = hauteur-1) && b <> 0
      then failwith"Labyrinthe pas fermé"
    else transforme_ligne ll (taille - 1) (indice + 1 ) tableau largeur indice_ligne hauteur
  | [] -> if taille <> 0 then failwith"Ligne trop petite" else ()
  
let print_ints list_int = Array.iter (fun x -> Printf.printf"%d" x) list_int; Printf.printf"\n"

let print_tab tab = Array.iter print_ints tab







let rec aux tableau indice_ligne file largeur hauteur = 

  match file with 
  | e::ll ->  transforme_ligne e largeur 0 tableau.(indice_ligne) largeur indice_ligne hauteur; 
  aux tableau (indice_ligne + 1 ) ll largeur hauteur
  | [] -> ()
  
let print_line list_char = List.iter (fun c -> Printf.printf"%c" c) list_char

let print_file list_list_char = List.iter print_line list_list_char;;
  


let rec trouve_depart_ligne ligne trouveD trouveA depart arrivee i j largeur =
  if (trouveA && trouveD) then (depart, arrivee) else

  let c = ligne.(i) in
  match c with 
   | 'E' -> if trouveD then failwith"Plusieurs entrées" else
              trouve_depart_ligne ligne true trouveA (i, j) arrivee (i+1) j largeur
   | 'S' -> if trouveA then failwith"Plusieurs sorties" else
    trouve_depart_ligne ligne trouveD true depart (i, j) (i+1) j largeur
   | _ -> if i = largeur-1 then (depart, arrivee)
            else trouve_depart_ligne ligne trouveD trouveA depart arrivee (i+1) j largeur



let constructeur  file_name = 
  let file = load_file file_name in
  
  let d = (-1, -1)  in
  let a = (-1, -1) in

  (* On définit la taille du labyrinthe*)
  let hauteur = List.length file in
  let largeur =
  match file with 
  | e::ll ->  List.length e
  | _     -> failwith"Y  a rien dans le labyrinthe"   in
  
  
  

  let tab = Array.make_matrix hauteur largeur 0  in

  aux tab 0 file largeur hauteur;

  {taille = (hauteur, largeur); walls = tab; depart = d; arrivee = a}
  
  

  


  




let print_line line =
  List.iter (fun c -> let s = transforme c in Printf.printf"%d " s) line;
  Printf.printf"\n"
  
let print_list liste = List.iter (fun x -> Printf.printf"%s\n" x) liste

let print_walls liste = List.iter print_line liste
let file = load_file path

let a = constructeur path
let () = print_tab a.walls

let fin = Printf.printf"End...\n"