(* tokens *)
type token = A | B | X

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)

let rec toklist_of_char_list chars =
  match chars with
  | [] -> []
  | 'A' :: t -> A :: toklist_of_char_list t
  | 'B' :: t -> B :: toklist_of_char_list t
  | '=' :: t -> X :: toklist_of_char_list t
  | _ :: _ -> failwith "bad input 2"

let toklist_of_string s =
  let char_list = explode s in
  toklist_of_char_list char_list

  

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
  

let valid l =
let rec check_a tokens =
  match tokens with
  | A :: tail -> check_a tail (* Continua a leggere gli A *)
  | X :: tail -> check_equals tail (* Trova il separatore "=" *)
  | [] -> true (* Parola valida se ci sono solo A *)
  | _ -> false (* Token non valido *)

and check_equals tokens =
  match tokens with
  | B :: tail -> check_b tail (* Trova la sequenza di B dopo "=" *)
  | X :: tail -> check_equals tail
  | _ -> false (* Mancano i B dopo "=" *)

and check_b tokens =
  match tokens with
  | B :: rest -> check_b rest (* Continua a leggere i B *)
  | [] -> true (* Parola valida se finisce con i B *)
  | _ -> false (* Token non valido *)
in
check_a l



(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)
let rec count l token = match l with 
  []-> 0
  |h::t -> if h = token then 1+  count t token else count t token;;



let win l = if ((count l A) != (count l B)) 
  then (if ((count l A) < (count l B)) then B else A) else X;;
   

(* val string_of_winner : token -> string *)
let string_of_winner w = if w = A then "A is Winner" else 
  if w = B then "B is winner" else "Is a tie";;
