open Maze 


(*Affichage utilitaires pour les tests et le debug*)

(*Affichage d'un array 2D d'entiers*)
let print_2d_array arr =
  Array.iter (fun row ->
    Array.iter (fun elem -> print_int elem; print_string " ") row;
    print_newline ()
  ) arr


(*Affichage d'une chaîne de caractères*)
let print_line line =
  String.iter (fun c -> let s = transforme c in Printf.printf"%d " s) line;
  Printf.printf"\n"


(*Affichage d'une liste de chaînes de caractères*)
let print_list liste = List.iter (fun x -> Printf.printf"%s\n" x) liste


(*Affichage de la structure complète du labyrinthe*)
let printLaby labyrinthe = 
  Printf.printf "Hauteur : %d\n" labyrinthe.hauteur; 
  Printf.printf "Largeur : %d\n" labyrinthe.largeur;
  Printf.printf "Largeur des murs : %d\n" labyrinthe.murs_largeur;
  Printf.printf "Hauteur des murs : %d\n" labyrinthe.murs_hauteur;
  Printf.printf "Plan des murs : \n";
  print_2d_array labyrinthe.murs 


(*Affiche une direction*)
let printDirection d = 
  match d with 
  | N -> Printf.printf"N "
  | E -> Printf.printf"E "
  | O -> Printf.printf"O "
  | S -> Printf.printf"S "


(*Affiche une liste de directions*)
let printDirections (directions : direction list) = List.iter printDirection directions
  

(*Affiche une liste d'entiers*)
let print_list_int liste = List.iter print_int liste


(*Affichage simple de la grille du labyrinthe*)

(*Affiche un mur selon sa position dans la grille*)
let printMur numero_ligne indice = 
  match numero_ligne mod 2, indice mod 2 with
  | 0 , 0       -> Printf.printf"+"  (*ligne et colonne pairs*)
  | 0 , 1       -> Printf.printf"-"  (*ligne pair et colonne impair*)
  | 1 , 0 | 1, 1-> Printf.printf"|"  (*ligne et colonne impairs*)
  | _, _ -> failwith"Problème de modulo" (*Cas inexistant, permet simplement d'éviter les warnings à la compilation*)


(*Affichage d'un caractère à partir d'un entier contenu dans la grille*)
let printCaractere (layout: int array array) numero_ligne indice = 
  let i1 = layout.(numero_ligne).(indice) in
  match i1 with 
  | 0 -> printMur numero_ligne indice (*Mur*)
  | 1 -> Printf.printf" " (*Case vide*)
  | 2 -> Printf.printf"S" (*Entrée*)
  | 3 -> Printf.printf"E" (*Sortie*)
  | 4 -> Printf.printf"\027[33mX\027[0m" (*Chemin*)
  | _ -> failwith"Valeur interdite dans la grille"


(*Affiche une ligne de la grille*)
let printLigne layout numero_ligne indice_max ligne_max = 
  let rec sousPrintLigne (layout : int array array) numero_ligne indice indice_max ligne_max=
    if (indice > indice_max) then (
      if (numero_ligne < ligne_max) then (
        Printf.printf"\n";)
      else
        ()
    )
    else (
      printCaractere layout numero_ligne indice;
      sousPrintLigne layout numero_ligne (indice + 1) indice_max  ligne_max; ) 
  in
  sousPrintLigne layout numero_ligne 0 indice_max ligne_max


(*Affiche la grille par sous-fonction*)
let rec sousAffiche layout numero_ligne ligne_max indice_max = 
  if numero_ligne > ligne_max then
    ()
  else (
    printLigne layout numero_ligne indice_max ligne_max;
    sousAffiche layout (numero_ligne + 1) ligne_max indice_max
  )


(*Affiche la grille*)
let afficheLabyrinthe labyrinthe = 
  let indice_max = labyrinthe.murs_largeur - 1 in
  let ligne_max = labyrinthe.murs_hauteur - 1 in
  sousAffiche labyrinthe.murs 0 ligne_max indice_max
  

(*Affichage pretty*)

(*Renvoie si l'entier i est un mur ou pas*)
let estMur i = i = 0


(*Affiche un caractère du bord gauche*)
let bordGauche layout numero_ligne =
  if estMur (layout.(numero_ligne).(1)) then  (*Si la case à droite est aussi un mur*)
    Printf.printf"╠"
  else
    Printf.printf"║"


(*Affiche un caractère du bord droit*)
let bordDroite layout numero_ligne indice_max =
  if estMur (layout.(numero_ligne).(indice_max)) then  (*Si la case à gauche est aussi un mur*)
    Printf.printf"╣\n"
  else
    Printf.printf"║\n"


(*Affiche un caractère du bord haut*)
let bordHaut murs indice =
  if estMur(murs.(1).(indice))then  (*Si la case en bas est aussi un mur*)
    Printf.printf"╦"
  else
    Printf.printf"═"


(*Affiche un caractère du bord bas*)
let bordBas murs indice indice_max = 
  if estMur(murs.(indice_max-1).(indice)) then  (*Si la case en haut est aussi un mur*)
    Printf.printf"╩"
  else
    Printf.printf"═"


(*Affiche la ligne du haut par sous-fonction*)
let rec sousprettyHaut murs indice indice_max =
  if indice > indice_max then
    Printf.printf "╗\n"  (*On termine à droite*)
  else
    let () = bordHaut murs indice in
    sousprettyHaut murs (indice + 1) indice_max


(*Affiche la ligne du haut*)
let prettyHaut  murs largeur_murs = 
  Printf.printf "╔";  (*On commence à gauche*)
  sousprettyHaut murs 1 (largeur_murs - 1)


(*Affiche la ligne du bas par sous-fonction*)
let rec sousprettyBas murs indice indice_max ligne_max =
  if indice > indice_max then
    Printf.printf "╝"  (*On termine à droite*)
  else
    let () = bordBas murs indice ligne_max in
    sousprettyBas murs (indice + 1) indice_max ligne_max


(*Affiche la ligne du bas*)
let prettyBas murs largeur_murs ligne_max= 
  Printf.printf "╚";  (*On commence à gauche*)
  sousprettyBas murs 1 (largeur_murs - 1) ligne_max


(*Renvoie si l'entier n est un chemin ou pas*)
let estChemin n =  n > 1


(*Affiche un chemin*)
let prettyChemin haut gauche bas droite = 
  let h = estChemin haut in
  let g = estChemin gauche in
  let b = estChemin bas in
  let d = estChemin droite in
  let () = 
  match h, g, b, d with  (*Match avec la direction du caractère précédent et du suivant pour l'orientation*)
  | true, true, false, false -> Printf.printf"\027[33m╯\027[0m";
  | true, false, true, false -> Printf.printf"\027[33m│\027[0m";
  | false, false, true, true -> Printf.printf"\027[33m╭\027[0m";
  | true, false, false, true -> Printf.printf"\027[33m╰\027[0m";
  | false, true, true, false -> Printf.printf"\027[33m╮\027[0m";
  | false, true, false, true -> Printf.printf"\027[33m─\027[0m";
  | _, _, _, _ -> Printf.printf" " 
  in ()


(*Affiche un mur*)
let prettyMur haut gauche bas droite = 
  let h = estMur haut in
  let g = estMur gauche in
  let b = estMur bas in
  let d = estMur droite in
  let () = 
  match h, g, b, d with  (*Match avec la direction du caractère précédent et du suivant pour l'orientation*)
  | false, false, false, false -> Printf.printf" ";
  | false, false, false, true | false, true, false, false | false, true, false, true -> Printf.printf"═";
  | true, false, true, false -> Printf.printf"║";
  | true, true, true, false -> Printf.printf"╣";
  | true, false, true, true -> Printf.printf"╠";
  | true, true, false, true -> Printf.printf"╩";
  | false, true, true, true -> Printf.printf"╦";
  | false, false, true, true -> Printf.printf"╔";
  | true, false, false, true -> Printf.printf"╚";
  | true, true, false, false -> Printf.printf"╝";
  | false, true, true, false -> Printf.printf"╗";
  | true, true, true, true -> Printf.printf"╬";
  | _, _, _, _ -> Printf.printf" "
  in ()


(*Affichage d'un caractère à partir d'un entier contenu dans la grille*)
let rec prettyChar murs numero_ligne indice indice_max= 
  if indice <= indice_max then
    let m = murs.(numero_ligne).(indice) in
    let () = match m  with
            | 0 -> prettyMur (murs.(numero_ligne - 1).(indice)) (murs.(numero_ligne).(indice - 1))  (*Mur*)
                             (murs.(numero_ligne + 1).(indice)) (murs.(numero_ligne).(indice + 1));  
            | 1 -> Printf.printf" ";  (*Case vide*)
            | 2 -> Printf.printf "\027[32mX\027[0m";  (*Entrée*)
            | 3 -> Printf.printf "\027[31mX\027[0m";  (*Sortie*)
            | 4 -> prettyChemin (murs.(numero_ligne - 1).(indice)) (murs.(numero_ligne).(indice - 1))  (*Chemin*)
                                (murs.(numero_ligne + 1).(indice)) (murs.(numero_ligne).(indice + 1));
            | _ -> failwith"Erreur" in
    prettyChar murs numero_ligne (indice + 1) indice_max
  else
    ()


(*Affiche une ligne de la grille*)
let prettyLigne murs numero_ligne indice_max ligne_max = 
  let () = bordGauche murs numero_ligne in
  let () = prettyChar murs numero_ligne 1 indice_max in
  bordDroite murs numero_ligne indice_max


(*Affiche la grille par sous-fonction*)
let rec prettyLignes murs numero_ligne indice_max ligne_max = 
  if numero_ligne <= ligne_max then
    let () = prettyLigne murs numero_ligne indice_max ligne_max in
    prettyLignes murs (numero_ligne +1) indice_max ligne_max
  else
    ()


(*Affiche la grille*)
let printPretty laby = 
  let () = prettyHaut laby.murs (laby.murs_largeur-1) in
  let () = prettyLignes laby.murs 1(laby.murs_largeur - 2) (laby.murs_hauteur - 2) in
  prettyBas laby.murs (laby.murs_largeur - 1) (laby.murs_hauteur - 1)
    

(*Main*)

(*On initialise une seed random*)
let () = Random.self_init  () 


(*Messages*)
let help = "Voici les 3 manières d'exécuter ce programme:\n
    1) Afficher un labyrinthe depuis un fichier: ./main.exe print <nom_du_fichier>\n
    2) Afficher un labyrinthe résolu depuis un fichier: ./main.exe solve <nom_du_fichier>\n
    3) Générer aléatoirement un labyrinthe: ./main.exe random <hauteur> <largeur>\n
    <hauteur> et <largeur> doivent être des entiers > 0.\n
    Les paramètres print et solve peuvent être suivis du paramètre --pretty pour utiliser
    l'affichage amélioré.\n 
    Exemple: ./main.exe solve --pretty test/maze_4x8.laby"
let printMessageErreur  = "Arguments invalides, tapez --help pour obtenir de l'aide." 


(*Fonction pour un appel à 1 paramètre*)
let parametre1 tab = 
  if tab.(1) = "--help" then
    Printf.printf"%s" help
  else
    Printf.printf"%s" printMessageErreur


(*Fonction pour un appel à 2 paramètres*)
let parametre2 tab = 
  if tab.(1) = "print" then
    let laby = constructeur tab.(2) in
    afficheLabyrinthe laby
  else
    if tab.(1) = "solve" then
      let laby = resolution (constructeur (Sys.argv.(2))) in
      afficheLabyrinthe laby
    else
      Printf.printf"%s"printMessageErreur


(*Fonction pour un appel à 3 paramètres*)
let parametre3 tab = 
   if tab.(1) = "random" then
    let int1, int2 = tab.(2), tab.(3) in
    (*Vérifie que les paramètres de taille sont bien des entiers strictement supérieurs à 0*)
    if (try ignore (int_of_string int1); ignore (int_of_string int2); true with _ -> false) &&
       (int_of_string int1) > 0 && (int_of_string int2) > 0 then
          let laby = genereLabyrinthe (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3)) in
          afficheLabyrinthe laby
    else 
      Printf.printf"%s"printMessageErreur

   else
     if tab.(1) = "print" && (tab.(2) = "--pretty") then
        let laby = constructeur tab.(3) in
        printPretty laby
      
      else
        if tab.(1) = "solve" && tab.(2) = "--pretty" then
          let laby = constructeur tab.(3) in
          let labySolved = resolution laby in
          printPretty labySolved

        else
          Printf.printf"%s"printMessageErreur


(*Fonction de lecture des paramètres d'exécution*)
let read_parameters = 
  let nb_parameters = Array.length Sys.argv - 1 in 
  let () = 
  match nb_parameters with
   | 1 -> parametre1 Sys.argv
   | 2 -> parametre2 Sys.argv
   | 3 -> parametre3 Sys.argv
   | 0 | _-> Printf.printf"%s" printMessageErreur
  in
  ()


let () = read_parameters;
Printf.printf"\n"
