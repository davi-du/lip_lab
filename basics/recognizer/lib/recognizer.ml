let rec lang1 l = if l = [] then false else (
  match l with
   [] -> true
  | h::t when h = '0' || h = '1' -> lang1 t
  | _ -> false

);;

let lang2 l = if l = [] then true else (
    match l with
       []->true
      | h::t when h = '0' -> (List.filter(fun x -> x != '1' ) t ) = []
      | h::t when h = '1' -> (List.filter(fun x -> x != '1' ) t) = []
      | _ -> false
);;

let aux l = ((List.hd l ) = '0' &&  (List.hd (List.rev l) = '0') );;

let lang3 l = aux l && (List.filter(fun x -> x != '0'|| x != '1' ) l ) = [];;

let lang4 l = 
  let rec count l = match l with 
      [] -> 0 
    | h::t -> if h = '1' then 1 + count t else 0 + count t 
  in
  count l = 2
;;

let rec lang5 = function
    ['0';'0'] | ['1';'1'] -> true
  | '0'::'0'::l | '1'::'1'::l-> lang5 l
  | _ -> false
;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
