(* ------------------------------------------------------------------------*)
(* Q1 : Money in the Bank *)
(* ------------------------------------------------------------------------*)

let open_account (initial_pass: passwd) : bank_account =
  
  let balance = ref 0 in
  let num_failure = ref 0 in
  let password = ref initial_pass in
  
  { update_pass = (fun old_passwd new_passwd -> if old_passwd = (!password) then 
                      (num_failure := 0;
                       password := new_passwd) else
                      (num_failure := (!num_failure) + 1; raise wrong_pass))
                  
  ; deposit = (fun input_passwd amount -> if (!num_failure) = 5 then 
                  raise too_many_failures
                else if input_passwd = (!password) then (
                  if amount < 0 then (num_failure := 0; raise negative_amount)
                  else (num_failure := 0; balance := (!balance) + amount) )
                else (num_failure := (!num_failure) + 1; raise wrong_pass ))
              
  ; retrieve = (fun input_passwd amount -> if (!num_failure) = 5 then
                   raise too_many_failures
                 else if input_passwd = (!password) then(
                   if amount < 0 then (num_failure := 0; raise negative_amount)
                   else if amount > (!balance) then (num_failure := 0; raise not_enough_balance)
                   else (num_failure := 0; balance := (!balance) - amount) )
                 else (num_failure := (!num_failure) + 1; raise wrong_pass ))
               
  ; show_balance = (fun input_passwd -> if (!num_failure) = 5 then 
                       raise too_many_failures
                     else if input_passwd = (!password) then (num_failure := 0; !balance)
                     else (num_failure := (!num_failure) + 1; raise wrong_pass)) 
  }
;;




(* ------------------------------------------------------------------------*)
(* Q2 : I Want to Travel *)
(* ------------------------------------------------------------------------*)
(* TODO: Write some tests for neighbours. Consider creating a graph first,
 and then writing your tests based on it *)

(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* We've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let neighbours_tests: ((string graph * string) * (string * weight) list) list = [
  ( ({nodes = ["Vancouver"; "Richmond"; "Calgary"];
      edges = [ ("Vancouver","Richmond",5);("Vancouver","Calgary",50)]},
     "Vancouver"), ([("Richmond",5);("Calgary",50)] ));
  ( ({nodes = []; edges = []},""), ([]) );
  ( ({nodes = ["Vancouver"; "Burnaby"; "Banff"];
      edges = [("Vancouver", "Burnaby", 8)]},
     "Burnaby"), ([]) );
  ( ({nodes = ["Vancouver"; "Montreal"; "Toronro"];
      edges = [("Vancouver", "Montreal", 300); ("Montreal", "Vancouver", 250)]},
     "Vancouver"), (["Montreal", 300]) ) 
]
;;



(* TODO: Implement neighbours. *)
let neighbours (g: 'a graph) (vertex: 'a) : ('a * weight) list = 
  List.fold_left ( fun output_list (v, neighbour, cost) -> if v = vertex then
                     (neighbour, cost)::output_list else
                     output_list ) [] g.edges 
;;




(* TODO: Implement find_path. *)
let find_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) =
  
  let rec aux_node (node: 'a * weight) (visited : 'a list) acc: ('a list * weight) =
    let (v2, w) = node in
    if v2 = b then ( List.append visited [v2], w+acc)
    else if List.mem v2 visited then raise Fail
    else 
      aux_list (neighbours g v2) (List.append visited [v2]) (w+acc) 
      
        
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) acc: ('a list * weight) =
    match nodes with
    | [] -> raise Fail
    | hd::tl -> try aux_node hd visited acc
        with Fail -> aux_list tl visited acc
                       
  in aux_list (neighbours g a) [a] 0
;;




(* TODO: Implement find_path'. *)
let find_path' (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) = 
  
  let rec aux_node (node: 'a * weight) (visited : 'a list) fc sc : ('a list * weight)=
    let (v2, w2) = node in
    if v2 = b then sc ([v2], w2)
    else if List.mem v2 visited then fc ()
    else
      aux_list (neighbours g v2) (v2::visited) fc (fun (v, w) -> sc (v2::v,w2+w))
  
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) fc sc : ('a list * weight) =
    match nodes with
    | [] -> fc ()
    | hd::tl -> aux_node hd visited (fun () -> aux_list tl visited fc sc) sc 
                  
  in aux_node (a,0) [] (fun () -> raise Fail) (fun (v,w) -> (v,w)) 
;;




(* TODO: Implement find_all_paths *)
let find_all_paths (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) list =
  
  let rec aux_node (node: 'a * weight) (visited : 'a list) acc: ('a list * weight) list =
    let (v2, w2) = node in
    if v2 = b then  ([v2], w2)::acc
    else if List.mem v2 visited then raise Fail
    else
      List.map (fun (path, weight) -> (v2::path, w2+weight)) (aux_list (neighbours g v2) (v2::visited) acc)
  
    
  and aux_list (nodes: ('a * weight) list) (visited: 'a list) acc : ('a list * weight) list =
    match nodes with
    | [] -> []
    | hd::tl -> try List.append (aux_node hd visited acc) (aux_list tl visited acc) 
        with Fail -> aux_list tl visited acc 
    
  in aux_node (a, 0) [] []
  
;;



(* TODO: Implement find_longest_path *)
let find_longest_path (g: 'a graph) (a: 'a) (b: 'a) : ('a list * weight) option =
  
  let all_paths = find_all_paths g a b in 
  
  let rec to_find paths =
    match paths with 
    | [] -> None
    | (p1, w1) :: tl ->
        match tl with
        | [] -> Some (p1, w1)
        | (p2, w2) :: tl2 -> if List.length p1 > List.length p2
            then to_find ((p1, w1) ::tl2)
            else if List.length p1 < List.length p2 then to_find ((p2, w2) ::tl2) 
            else 
              (if w1 >= w2 then to_find ((p1, w1) ::tl2)
               else to_find ((p2, w2) ::tl2) )
             
  in to_find all_paths
;;
    
    




