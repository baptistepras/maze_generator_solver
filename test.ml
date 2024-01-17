open Maze

(*Pour l'exécuter, remplacer 'main' par 'test' dans le fichier dune ligne 7*)

let () = Printf.printf"Tests:\n\n"

(*Test de la lecture à partir d'un fichier*)
let checkConstructeur =
  let rec arrayEgal sol laby indH indL =
    if (indL = laby.murs_largeur) then

      arrayEgal sol laby (indH + 1) 0
    else if (indH = laby.murs_hauteur) then
      true
    else if (sol.(indH).(indL) = laby.murs.(indH).(indL)) && laby.hauteur = 3 && laby.largeur = 2 && 
      laby.murs_hauteur = 7 && laby.murs_largeur = 5 && laby.arrivee = (5, 1) && laby.depart = (1, 3) then
      arrayEgal sol laby indH (indL + 1)
    else
      false
  in

  let sol =  (*Array du labyrinthe 3x2*)
    [|
      [| 0; 0; 0; 0; 0 |];
      [| 0; 1; 1; 2; 0 |];
      [| 0; 1; 0; 1; 0 |];
      [| 0; 1; 0; 1; 0 |];
      [| 0; 1; 0; 0; 0 |];
      [| 0; 3; 1; 1; 0 |];
      [| 0; 0; 0; 0; 0 |];
    |]
  in
  let laby = constructeur "test/maze_3x2.laby" in
  let result = arrayEgal sol laby 0 0 in
  Printf.printf "Vérification du constructeur (doit renvoyer 1): %b\n" result


(*Test de la lecture à partir d'un fichier faux*)

(*Un caractère inconnu est présent*)
let checkConstructeurFaux1 =
  try
    let laby = constructeur "test/maze_3x2FAUX1.laby" in
    raise (Failure "Le constructeur a bien levé une exception");
  with
  | Failure message -> Printf.printf "Exception levée: %s\n" message
  | _ -> Printf.printf "Autre exception inattendue\n"


(*Un mur n'a pas le bon format*)
let checkConstructeurFaux2 =
  try
    let laby = constructeur "test/maze_3x2FAUX2.laby" in
    raise (Failure "Le constructeur a bien levé une exception");
  with
  | Failure message -> Printf.printf "Exception levée: %s\n" message
  | _ -> Printf.printf "Autre exception inattendue\n"


(*Il y a deux entrées*)
let checkConstructeurFaux3 =
  try
    let laby = constructeur "test/maze_3x2FAUX3.laby" in
    raise (Failure "Le constructeur a bien levé une exception");
  with
  | Failure message -> Printf.printf "Exception levée: %s\n" message
  | _ -> Printf.printf "Autre exception inattendue\n"
  

(* Test du solveur *)
let checkSolve =
  let rec arrayEgal sol laby indH indL =
    if (indL = laby.murs_largeur) then
      arrayEgal sol laby (indH + 1) 0
    else if (indH = laby.murs_hauteur) then
      true
    else if (sol.(indH).(indL) = laby.murs.(indH).(indL)) then
      arrayEgal sol laby indH (indL + 1)
    else
      false
  in

  let sol =  (*Array résolue du labyrinthe 3x2*)
    [|
      [| 0; 0; 0; 0; 0 |];
      [| 0; 4; 4; 2; 0 |];
      [| 0; 4; 0; 1; 0 |];
      [| 0; 4; 0; 1; 0 |];
      [| 0; 4; 0; 0; 0 |];
      [| 0; 3; 1; 1; 0 |];
      [| 0; 0; 0; 0; 0 |];
    |]
  in
  let laby = resolution (constructeur "test/maze_3x2.laby") in
  let result = arrayEgal sol laby 0 0 in
  Printf.printf "Vérification du solveur (doit renvoyer 1): %b\n\n" result
