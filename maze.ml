(*Définition de la structure de donnée du labyrinthe*)
type labyrinthe = {
  largeur : int;  (*largeur du laby en nombre de cases*)
  hauteur : int;  (*hauteur du laby en nombre de cases*)
  murs_largeur : int;  (*largeur du laby en nombre de caractères*)
  murs_hauteur : int;  (*hauteur du laby en nombre de caractères*)
  murs : int array array;  (*grille du laby*)
  depart : int*int;  (*départ (signalé par un E pour entrée dans le fichier)*)
  arrivee : int*int  (*arrivée (signalée par un S pour sortie dans le fichier)*)
}

let path = "test/maze_3x2.laby"


(*Construction d'un labyrinthe à partir d'un fichier*)

(*Donne l'élément suivant d'une liste*)
let suite l = 
  match l with 
  | []    -> []
  | e::ll -> ll 


(*Charge un labyrinthe depuis un fichier .laby en mettant dans une liste
   les caractères lus dans le fichier. 1 string = 1 ligne*)
let load_file file_path =
  let in_c = open_in file_path in
 
  let rec aux file =
    try
      let line = input_line file in
      line::aux file
    with
      End_of_file -> []  in
  aux in_c


(*Transforme un caractère c lu en entier*)
let transforme c = 
  match c with 
  | '+' | '-' | '|' -> 0  (*mur*)
  | ' ' -> 1  (*case vide*)
  | 'S' -> 2  (*exit*)
  | 'E' -> 3  (*start*)
  | _  -> failwith"Caractère %s interdit" c


(*Vérifie que chaque caractère est bien placé dans le labyrinthe*)
let verifie_caractere c numero_ligne indice ligne_max indice_max = 
  (*Si la ligne est paire , si l'indice dans la ligne est pair, on doit avoir un '+', sinon on doit avoir '|'*)
  (*Si l'on est sur la première ligne on vérifie si l'on a un '+' ou un '-'*)
  if indice > indice_max then
    failwith"Ligne trop grande"
  else
    if numero_ligne = 0 || numero_ligne = ligne_max then
      if indice mod 2 = 0 then c = '+' else c = '-' 
    else
      (*Si l'on est au début ou à la fin de la ligne on doit avoir un caractère fermant*)
      if indice = 0 || indice = indice_max then
        if numero_ligne mod 2 = 0 then c = '+' else c = '|'
      else
        (*Sinon on vérifie selon la parité de numero_ligne et indice*)
        match (numero_ligne mod 2), (indice mod 2) with 
        | 0 , 0 ->  c = '+'
        | 0 , 1 ->  c = '-' || c = 'E' || c = 'S' || c = ' '
        | 1 , 0 ->  c = '|' || c = 'E' || c = 'S' || c = ' '
        | 1 , 1 ->  c = 'S' || c = 'E' || c = ' '
        | _ , _ -> failwith"Problème de modulo" (*Cas inexistant, permet simplement d'éviter les warnings à la compilation*)


(*Parcours une string qui correspond à une ligne à la lecture du fichier*)
let rec parcours_ligne ligne numero_ligne array indice indice_max= 
    
  let c = ligne.[indice] in
  array.(numero_ligne).(indice) <- transforme c;
  if indice < indice_max then
    parcours_ligne ligne numero_ligne array (indice+1) indice_max 
  else
    ()


(*Vérifie qu'une ligne soit correcte*)
let rec verifieLigne (ligne:string) numero_ligne indice ligne_max indice_max (depart: int*int) (arrivee:int*int) =
  if indice > indice_max then
    (depart, arrivee)
  else
    let c = ligne.[indice] in
    (* Vérifie si le caractère est au bon endroit *)
    if not(verifie_caractere c numero_ligne indice ligne_max indice_max) then
      failwith "Caractère au mauvais endroit"
    else
      if c = 'S' then
        if depart = (-1, -1) then
          let new_depart = (numero_ligne, indice) in
          verifieLigne ligne numero_ligne (indice + 1) ligne_max indice_max new_depart arrivee
        else
          failwith "Plusieurs départs"
      else if c = 'E' then
        if arrivee = (-1, -1) then
          let new_arrivee = (numero_ligne, indice) in
          verifieLigne ligne numero_ligne (indice + 1) ligne_max indice_max depart new_arrivee
        else
          failwith "Plusieurs arrivées"
      else
        verifieLigne ligne numero_ligne (indice + 1) ligne_max indice_max depart arrivee


(*Vérification d'un fichier par sous-fonction*)
let rec sousVerifie (file:string list) largeur hauteur numero_ligne (depart: int*int) (arrivee:int*int)  = 
  match file with 
  | []    -> (depart, arrivee)
  | ligne::reste -> 
      let nouveau_depart, nouveau_arrivee = verifieLigne ligne numero_ligne 0 (hauteur - 1) (largeur - 1) depart arrivee in 
      sousVerifie reste largeur hauteur (numero_ligne + 1) nouveau_depart nouveau_arrivee


(*Vérification d'un fichier*)
let verifie (file:string list) largeur hauteur = 
  sousVerifie file largeur hauteur 0 (-1, -1) (-1, -1)
  

(*Constructeur d'un labyrinthe*)
let constructeur file_name = 
  let file = load_file file_name in 
  (* On définit la taille du labyrinthe*)
  let hauteur = List.length file in
  let largeur =
  match file with 
  | e::ll ->  String.length e
  | _     -> failwith"Rien dans le labyrinthe"   in
  let depart, arrivee = verifie file largeur hauteur in

  (*0n construit le labyrinthe*)
  let layout = Array.make_matrix hauteur largeur 0 in
  
  (*Effectue le parcours du fichier*)
  let rec parcours file numero_ligne array indice_max =
    match file with 
    | [] -> ()
    | e::ll -> parcours_ligne e numero_ligne array 0 indice_max;
              parcours ll (numero_ligne + 1) array indice_max; in
  parcours file 0 layout (largeur - 1);

  (*Rendu de la structure*)
  let l = (largeur - 1) / 2 in
  let h = (hauteur - 1) / 2 in
  { largeur = l;
  hauteur = h;
  murs_largeur = largeur;
  murs_hauteur = hauteur;
  murs = layout;
  depart = depart;
  arrivee = arrivee }


let file = load_file path


(*Génération d'un labyrinthe random*)

(*Génération d'une ligne vide par sous-fonction*)
let rec sousGenereLigneVide array numero_ligne  indice_max indice = 
  if indice > indice_max then
    ()
  else (
    array.(numero_ligne).(indice) <- 1;
    sousGenereLigneVide array numero_ligne indice_max (indice + 2) 
  )


(*Génération d'une ligne vide*)
let genereLigneVide array numero_ligne indice_max  = 
  sousGenereLigneVide array numero_ligne indice_max 1 


(*Génération d'un labyrinthe vide par sous-fonction*)
let rec sousGenereVide array numero_ligne ligne_max indice_max = 
  if numero_ligne >= ligne_max then
    ()
  else(
    genereLigneVide array numero_ligne indice_max;
    sousGenereVide array (numero_ligne + 2) ligne_max indice_max; )


(*Génération d'un labyrinthe vide*)
let genereLabyVide hauteur largeur depart arrivee= 
  let largeur_murs  =  2 * largeur + 1 in
  let hauteurs_murs =  2 * hauteur + 1 in
  
  (*Un labyrinthe vide est un labyrinthe ou chaque caractère est un mur*)
  let lay = (Array.make_matrix hauteurs_murs largeur_murs 0) in
  sousGenereVide lay 1 (hauteurs_murs-1) (largeur_murs -1);
  lay.(fst depart).(snd depart)   <- 2;
  lay.(fst arrivee).(snd arrivee) <- 3;

  {largeur = largeur;
   hauteur = hauteur;
   murs_largeur = largeur_murs;
   murs_hauteur = hauteurs_murs;
   murs = lay;
   depart = depart;
   arrivee = arrivee;} 
 

(*Renvoie une direction*)
type direction = N | O | S | E

let intDirection n =
  match n with
  | 1 -> N
  | 2 -> O
  | 3 -> S
  | 4 -> E
  | _  -> failwith"Direction inexistante"


(*Renvoie les directions dans lesquelles on peut aller depuis la case courante*)
let directionsPossibles (visitees:bool array array) numero_ligne indice ligne_max indice_max =
  let positions = ref [] in

  (*Est*)
  if indice + 2 < indice_max   && not visitees.(numero_ligne).(indice + 2) then
    positions := E :: !positions
  else
    (*La case à l'Est n'existe pas ou a déjà été visitée*)
    ();

  (*Ouest*)
  if indice - 2 > 0 && not visitees.(numero_ligne).(indice - 2) then
    positions := O :: !positions
  else
    (*La case à l'Ouest n'existe pas ou a déjà été visitée*)
    ();
  
  (*Sud*)
  if numero_ligne + 2 < ligne_max && not visitees.(numero_ligne+2).(indice) then
    positions := S :: !positions
  else
    (*La case au Sud n'existe pas ou a déjà été visitée*)
    ();
  
  (*Nord*)
  if numero_ligne - 2 > 0 && not visitees.(numero_ligne - 2).(indice) then
    positions := N :: !positions
  else
    (*La case au Nord n'existe pas ou a déjà été visitée*)
    ();

  let positions_list = !positions  in
  positions_list


(*Renvoie une liste de nombre choisis aléatoirement*)
let rec generate_unique_random_numbers count range_max acc =
  if count <= 0 then acc
  else
    let num = Random.int range_max in
    if List.mem num acc then
      generate_unique_random_numbers count range_max acc
    else
      generate_unique_random_numbers (count - 1) range_max (num :: acc)


let random_numbers_without_repetition count range_max =
  generate_unique_random_numbers count range_max []


(*Renvoie le numéro de ligne ou colonne selon la direction empruntée*)
let movePosition numero_ligne indice direction = 
  match direction with
  | N -> (numero_ligne - 2, indice)
  | E -> (numero_ligne, indice + 2)
  | O -> (numero_ligne, indice - 2)
  | S -> (numero_ligne + 2, indice)


(*Ouvre le chemin entre deux cases selon une direction donnée*)
let ouvrirChemin (array:int array array) last_pos numero_ligne indice = 
  (*Il a été préalablement vérifié qu'on pouvait aller dans cette direction*)
  let y = ((fst last_pos) + numero_ligne) / 2 in
  let x = ((snd last_pos) + indice) / 2 in
  
  array.(y).(x) <- 1


(*Génération random d'un labyrinthe par sous-fonction*)
let rec sousGenereLaby layout numero_ligne indice (visitees : bool array array) last_pos indice_max ligne_max = 
  if visitees.(numero_ligne).(indice) then
    ()
  else
    (*Ouvre un mur et continue son chemin*)
    ouvrirChemin layout last_pos numero_ligne indice ; 
    visitees.(numero_ligne).(indice) <- true;
    let direc = directionsPossibles visitees numero_ligne indice ligne_max indice_max in
    let nombre_directions = List.length direc in
    if (nombre_directions) > 0 then
      let list_of_numbers = random_numbers_without_repetition nombre_directions nombre_directions in
      let direction = List.nth direc (List.hd list_of_numbers) in 

      
      let new_pos = movePosition numero_ligne indice direction in 

      sousGenereLaby layout (fst new_pos ) (snd new_pos) visitees (numero_ligne, indice) indice_max ligne_max;

      (*On appelle récursivement pour les autres directions possibles*)
      if nombre_directions > 1 then
        let s1 = List.tl list_of_numbers in
        let d1 = List.nth direc (List.hd s1) in
        let pos1 = movePosition numero_ligne indice d1 in
        sousGenereLaby layout (fst pos1) (snd pos1 ) visitees (numero_ligne, indice) indice_max ligne_max;
        if nombre_directions > 2 then
          let s2 = List.tl s1 in
          let d2 = List.nth direc (List.hd s2) in
          let pos2 = movePosition numero_ligne indice d2 in
          sousGenereLaby layout (fst pos2) (snd pos2 ) visitees  (numero_ligne, indice) indice_max ligne_max;
          if nombre_directions > 3 then
            let s3 = List.tl s2 in
            let d3 = List.nth direc (List.hd s3) in
            let pos3 = movePosition numero_ligne indice d3 in
            sousGenereLaby layout (fst pos3) (snd pos3 ) visitees  (numero_ligne, indice) indice_max ligne_max;
          else 
            ()
        else
          ()
      else
        ()
    else
        ()
      

(*Génère aléatoirement la position du départ*)
let genereDepart hauteur largeur = 
  (2*(Random.int hauteur  )+ 1, 2*(Random.int largeur) + 1)


(*Génère aléatoirement la position de l'arrivée*)
let rec genereArrivee depart hauteur largeur = 
  let x = 2*(Random.int largeur) + 1 in
  let y = 2*(Random.int hauteur) + 1 in
  if (y = (fst depart)) && (x = (snd depart)) then
    genereArrivee depart largeur hauteur 
  else
    (y,x)


(*Génération random d'un labyrinthe*)
let genereLabyrinthe hauteur largeur = 
  let dep = genereDepart hauteur largeur in
  let arr = genereArrivee dep hauteur largeur in
  let laby = genereLabyVide hauteur largeur dep arr in
  let visitees = Array.make_matrix laby.murs_hauteur  laby.murs_largeur  false in  (*Matrice 2D des cases déjà visitées*)

  sousGenereLaby laby.murs (fst dep) (snd dep) visitees  dep (laby.murs_largeur - 1) (laby.murs_hauteur - 1) ;
  laby.murs.(fst dep).(snd dep) <- 2;
  laby.murs.(fst arr).(snd arr) <- 3;
  laby


(*Résolution d'un labyrinthe*)

(*Résolution d'un labyrinthe par sous-fonction*)
let rec resolutionRec laby visit chemin case dir =
  (*dir permet de vérifier la présence d'un mur ou non entre l'ancienne case et la nouvelle, selon la direction prise*)
  let fstC, sndC = fst case, snd case in
  let fstD, sndD = fst dir, snd dir in
  if not (fstC >= 0 && fstC < laby.murs_hauteur && sndC >= 0 && sndC < laby.murs_largeur) || 
    visit.(fstC).(sndC) || laby.murs.(fstC + fstD).(sndC + sndD) = 0
    then ([], false)  (*Déplacement impossible = abandon*)
  else
    let chemin = (fstC + fstD, sndC + sndD)::case::chemin in  (*On sauvegarde la case courante dans le chemin*)
    let () = visit.(fstC).(sndC) <- true in (*On coche la case courante comme désormais visitée*)
    if case = laby.arrivee then
      (chemin, true)  (*On est arrivé, on renvoie le chemin*)
    else
      let chemin1 = resolutionRec laby visit chemin (fstC + 2, sndC) (-1, 0) in (*Sud*)
      if snd chemin1 then chemin1 else
      let chemin2 = resolutionRec laby visit chemin (fstC - 2, sndC) (1, 0) in (*Nord*)
      if snd chemin2 then chemin2 else
      let chemin3 = resolutionRec laby visit chemin (fstC, sndC + 2) (0, -1) in (*Est*)
      if snd chemin3 then chemin3 else
      let chemin4 = resolutionRec laby visit chemin (fstC, sndC - 2) (0, 1) in (*Ouest*)
      if snd chemin4 then chemin4
      else ([], false)  (*Aucun chemin n'a abouti*)


(*Résolution d'un labyrinthe par sous-fonction*)
let resolution laby =
  (*On rappelle qu'une case vide se trouve toujours sur un indice impair, un mur sur un indice pair.
     Ainsi, on navigue d'indice impair en indice impair (2 caractères à la fois) en vérifiant
     à chaque fois que l'indice pair entre les deux n'est pas un mur*)
  let visitees = Array.make_matrix laby.murs_hauteur laby.murs_largeur false in  (*Matrice 2D des cases déjà visitées*)
  visitees.(fst laby.depart).(snd laby.depart) <- false;
  let chemin = resolutionRec laby visitees [] laby.depart (0, 0) in (*Cherche le bon chemin*)
    let rec transformeChemin laby chemin = (*Remplace par des 4 dans le laby les caractères du chemin*)
      match chemin with
      | e::ll -> if laby.murs.(fst e).(snd e) = 1 then (*Assure que l'on n'efface pas ni mur, ni arrivée, ni départ pour y passer*)
        laby.murs.(fst e).(snd e) <- 4; transformeChemin laby ll
      | _ -> laby in
    transformeChemin laby (fst chemin)
    
(*Si aucun chemin n'est trouvé, le labyrinthe restera le même qu'au préalable, aucune erreur ne sera invoqué
Cependant, le labyrinthe est supposé connexe, il existe donc normalement toujours un chemin*)
 