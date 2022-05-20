(* Section 1 : Lists *)

(* Question 1.1 : Most common element of *sorted* list *)

let mode_tests: (int list * int) list = [
  ([1], 1);
  ([1;2;3;3], 3);
  ([2;3;1;3], 3);
  ([5;4;3;2;2], 2);
  ([3;3;1;1;2], 1);
  ([1;1;1;2;2;2], 1)
] 
;;


let mode (l: 'a list) : 'a = match l with 
  |[] -> failwith "Input is invalid"
  |_ -> 
      let rec aux l ((cur_el, cur_num) : 'a * int) ((max_el, max_num) : 'a * int) =
        match l with 
        |[] -> if cur_num > max_num then cur_el else max_el
        |hd :: tl -> if hd = cur_el then
              if (cur_num + 1) > max_num then
                aux tl (hd, cur_num + 1) (hd, cur_num + 1) 
              else 
                aux tl (hd, cur_num + 1) (max_el, max_num)
            else
              aux tl (hd, 1) (max_el, max_num)
      in aux (List.sort compare l) (List.hd (List.sort compare l), 0) (List.hd (List.sort compare l), 0)
;;




(* Question 1.2 : Most common consecutive pairing *)

let pair_mode_tests: (int list * (int * int) ) list = [
  ([1;2;3;4;5], (1,2));
  ([1;2;1;2;3], (1,2));
  ([1;3;2;3;2;1], (3,2));
  ([1;2], (1,2)); 
  ([1;2;1;2;1], (1,2)) 
] ;;



let pair_mode (l: 'a list) : 'a * 'a = 
  if (List.length l) < 2 then failwith "Invalid input" else
    let l1_hd :: l1_tl  = l in let 
      l2_hd :: l2_tl = List.rev l in let 
      l2_tl = List.rev l2_tl in let
      bi_gram_l = List.combine l2_tl l1_tl in mode bi_gram_l 
;;






(* Section 2 : Custom data types *)

let convert_time ((from_unit, val_) : time_unit value) to_unit : time_unit value =
  match (from_unit, to_unit) with
  | (Second, Hour) -> (Hour, val_ /. 3600.)
  | (Hour, Second) -> (Second, val_ *. 3600.)
  | (Second, Second) -> (Second, val_)
  | (Hour, Hour) -> (Hour, val_)
;;



let convert_dist ((from_unit, val_) : dist_unit value) to_unit : dist_unit value =
  match (from_unit, to_unit) with
  | (Foot, Meter) -> (Meter, val_ *. 0.3048)
  | (Foot, Mile) -> (Mile, val_ /. 5280.)
  | (Meter, Foot) -> (Foot, val_ /. 0.3048)
  | (Meter, Mile) -> (Mile, (val_ /. 0.3048) /. 5280.)
  | (Mile, Foot) -> (Foot, val_ *. 5280.)
  | (Mile, Meter) -> (Meter, (val_ *. 5280.) *. 0.3048)
  | _ -> (to_unit, val_)
;;



let convert_speed ((from_unit, val_) : speed_unit value) to_unit : speed_unit value = 
  let val_1 = convert_dist ((fst from_unit), val_) (fst to_unit) in
  let val_2 = convert_time ((snd from_unit), (1. /. (snd val_1))) (snd to_unit) in 
  to_unit, (1. /. (snd val_2))
;;



let add_speed (a : speed_unit value) ((b_unit, b_val) : speed_unit value) : speed_unit value = 
  let a_converted = convert_speed a b_unit in 
  let total_val = (snd a_converted) +. b_val in
  b_unit, total_val
  
;;



let dist_traveled time ((speed_unit, speed_val) : speed_unit value) : dist_unit value = 
  let time_converted = convert_time time (snd speed_unit) in 
  let dist_val = (snd time_converted) *. speed_val in 
  (fst speed_unit), dist_val 
;;






(* Section 3 : recursive data types/induction *)

let passes_da_vinci_tests : (tree * bool) list = [ 
  (Leaf, true);
  (Branch (10., []), true);
  (Branch (10., [Branch(20.,[Leaf;Leaf])]), false);
  (Branch(10., [Branch(7.,[Branch(5., [Leaf])])]), true);
  (Branch (3., [Branch (3., [Branch (2., []); Leaf; Leaf])]), true);
  (Branch (1., [Branch (3., [Branch (1., [Branch (-2., [Leaf; Leaf])]); Leaf]); Leaf]), false)
] ;;


let  passes_da_vinci t = 
  let rec sum_ l acc = 
    match l with 
    | [] -> acc
    | Leaf :: tl -> sum_ tl acc
    | Branch (width, subtree) :: tl -> sum_ tl (acc +. (width ** 2.)) 
  in 
  let rec check l = match l with 
    | [] -> true
    | Leaf :: tl -> check tl
    | Branch (width, subtree) :: tl -> 
        if not((check subtree) && ((sum_ subtree 0.) <= (width ** 2.))) then false
        else check tl 
  in
  check [t] 
;;

















