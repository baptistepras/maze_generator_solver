let () = Printf.printf"Begin...\n"

type layout = int array array

type labyrinthe = {
  taille : int*int;
  walls : layout;
  depart : int*int;
  arrivee : int*int;
}



let path = "test/maze_100x100.laby"

let suite l = 
  match l with 
  | []    -> []
  | e::ll -> ll 



let load_file file_path =
  let in_c = open_in file_path in
 
  let rec aux file =
    try
      let line = input_line file in
      line::aux file
    with
      End_of_file -> []  in
  aux in_c

let transforme c = 
  match c with 
  | '+' | '-' | '|' -> 0
  | ' ' -> 1
  | 'E' -> 2
  | 'S' -> 3
  | _   -> failwith"Caractère %s interdit" c

let constructeur file_name = 
  let file = load_file file_name in 
  let depart = 0  in
  let arrivee = 0 in

  (* On définit la taille du labyrinthe*)
  let hauteur = ((List.length file) -1)/2 in
  let largeur =
  match file with 
  | e::ll ->  ((String.length e) -1)/2 
  | _     -> failwith"Y  a rien dans le labyrinthe"   in
  
  let tab = Array.make hauteur (Array.make largeur 1) in

  let transforme_ligne ligne taille numero tableau = 
    (* Prend en argument une ligne string, elle doit avoir une longueur de taille*)
    (* Si numero est paire, le début et la fin doit être un +      *)
    (* Si numero est impaire  le debut et la fin  doit *)


    if (((String.length ligne) -1) / 2) <> taille then  failwith"Ligne pas de la bonne taille" 
    else
    let rec parcours_ligne ligne indice last_element numero tableau =
      let caractere = ligne.[indice] in
      
      
      if indice = 2*taille || indice = 0 then begin
         (*si on est au dernier élément de la liste*)
        if numero mod 2 = 0 then begin  (* si la ligne est paire*)
          
          if caractere <> '+'
            then begin
              failwith"Mauvais dernier element" end
        end else
          if caractere <> '|' then begin
            failwith"Mauvais dernier element" end

        tableau.[numero].[indice] = 0
      end else
        if caractere <> ' '  then begin

          if caractere = 'E' then begin 
            if depart = 1 then begin failwith"Plusieurs entrees"
            end else depart = 1
          end else
            if caractere = 'S' then begin
              if arrivee = 1 then begin failwith"Plusieurs sorties"
              end else arrivee = 1
            end else
              if caractere = last_element then begin
                failwith"Deux caracteres egaux qui se suivent"
              end else
                tableau.[numero].[indice] = transforme caractere
        end else parcours_ligne ligne (indice+1) caractere numero tableau
            
    in
    parcours_ligne ligne 0 ' ' numero tableau
    transforme_ligne (suite file) largeur (numero+1) tableau
  in 
  transforme_ligne file largeur 0 tab 
    
      
          

  
  
  


 



  




let print_line line =
  String.iter (fun c -> let s = transforme c in Printf.printf"%d " s) line;
  Printf.printf"\n"
  
let print_list liste = List.iter (fun x -> Printf.printf"%s\n" x) liste

let print_walls liste = List.iter print_line liste
let file = load_file path

let () = constructeur path

let fin = Printf.printf"End...\n"