(*--------------------------------------------------------------*)
(* String to Characters to String *)
(*--------------------------------------------------------------*)

(* Turn a string into a list of characters. *)
let string_explode (s : string) : char list =
  tabulate (String.get s) (String.length s)
;;

  
(* Turn a list of characters into a string. *)
let string_implode (l : char list) : string =
  List.fold_left (^) "" (List.map Char.escaped l)
;;



(*--------------------------------------------------------------*)    
(* unfolding is like folding in reverse *)
(*--------------------------------------------------------------*)
                     
(* Compute the even natural numbers up to an exclusive limit. *)
let evens (max : int) : int list =
  unfold (fun b -> (b, b+2)) (fun b -> max <= b) 0 
;;
  


(* Compute the fibonacci sequence up to an exclusive limit. *)
let fib (max : int) : int list =
  unfold (fun (a, b) -> (a, (b, a+b))) (fun (a, b) -> max <= a) (1, 1) 
;;

    

(* Compute Pascal's triangle up to a maximum row length. *)
let pascal (max : int) : int list list =
  unfold (fun b -> (b, List.map2 (+) (0::b) (b @ [0]))) 
    (fun b -> max < (List.length b)) [1] 
;;



(* Implement zip, which converts two lists into a list of tuples.
       e.g. zip [1; 2] ['a'; 'c'] = [(1, 'a'); (2, 'c')]
       Note that if one list is shorter than the other, then the 
       resulting list should have the length of the smaller list.     *)

let zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  unfold (fun (x1, x2) -> ((List.hd x1, List.hd x2), (List.tl x1, List.tl x2)))
    (fun (x1, x2) -> (x1 == []) || (x2 == [])) (l1, l2) 
;;



(*--------------------------------------------------------------*)
(* Let's *safely* have cake!  *)
(*--------------------------------------------------------------*)

(* 3. Return the cupcakes from the cupcake list that contain none of the 
      allergens.                                                         *)
  
let allergy_free (allergens : ingredient list) (cupcakes : cupcake list)
  : cupcake list = 
  List.filter (function Cupcake(_,_,_,ingdt_list) -> 
      List.for_all (fun ingdt ->
          not (List.exists ( fun aller ->
              aller = ingdt
            ) allergens)) ingdt_list) cupcakes 

;;



