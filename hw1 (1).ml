(* Question 1 *)
(* TODO: Write your own tests for the fact function.
         Remember that you should NOT test cases for n < 0.
*)
(* TODO: Correct these tests for the fact function. *)
let fact_tests = [
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (5, 120.);
  (7, 5040.);
  (10, 3628800.)
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float =
  match n with
  | 0 -> 1. 
  | _ -> (float_of_int n) *. (fact (n - 1))
;;


(* TODO: Write your own tests for the binomial function.
         See the provided tests for fact, above, for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 1.);
  ((1, 0), 1.);
  ((2, 0), 1.);
  ((2, 1), 2.);
  ((2, 2), 1.);
  ((3, 0), 1.);
  ((3, 1), 3.);
  ((3, 2), 3.);
  ((3, 3), 1.);
  ((4, 2), 6.);
  ((10, 1), 10.);
  ((10, 2), 45.);
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial (n: int) (k: int): float =
  if n < 0
  then domain ()
  else (if k > n || k < 0
        then domain ()
        else fact n /. (fact k *. fact (n - k))) 
;;


(* TODO: Write a good set of tests for distance. *)
let distance_tests = [
    (* Your test cases go here *)
  (((0, 0), (0, 0)), 0.);
  (((0, 1), (0, 0)), 1.);
  (((0, 4), (0, 0)), 4.);
  (((3, 0), (3, 4)), 4.);
  (((3, 4), (0, 0)), 5.);
  (((0, 1), (0, 0)), 1.);
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let distance ((x1, y1): (int * int)) ((x2, y2): (int * int)) : float =
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  sqrt (float_of_int(dx * dx + dy * dy))
;;


(* Question 2: is_prime *)

(* TODO: Write a good set of tests for is_prime. *)
let is_prime_tests = [
  (2, true);
  (3, true);
  (4, false);
  (5, true);
  (6, false);
  (7, true);
  (8, false);
  (9, false);
  (10, false);
  (11, true);
]
;;

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let is_prime (n: int): bool =
  if n <= 1
  then domain()
  else ( let rec is_not_divisible (n: int)(d: int): bool = 
           match d with
           | 1 -> true
           | _ -> ( match n mod d with
               | 0 -> false
               | _ -> is_not_divisible n (d-1)
             )
         in is_not_divisible n (n-1) 
       )
;;
         


(* Question 3: Riemann Zeta function 
    Implement the `approx_zeta` function.
    You do not need to modify any other parts inside the `zeta` function
*)

let zeta_tests = [
  (3., 1.20205690314079017);
  (4., 1.08232323371051709);
  (12., 1.0002460865533076);
  (20., 1.00000095396203381);
  (2., 1.6449340516623121); 
  (3.6, 1.11598907912137757);
  (15.8, 1.00001755700176065);
] 
;;
  
let zeta (k: float) : float = 
  let rec approx_zeta k acc n sum_so_far = 
    if n ** (0. -. k) < acc then sum_so_far
    else 
      approx_zeta k acc (n +. 1.) (sum_so_far +. n ** (0. -. k)) 
  in 
      (*  Note that we put < 2. while the function still works 
          to evaluate any smaller arguments *)
  if k < 2. 
  then domain () 
  else approx_zeta k epsilon_float 1. 0.
;;


(* Question 4: Fibonacci*)

(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
  (0, 1);
  (1, 1);
  (2, 2);
  (3, 3);
  (4, 5);
  (5, 8);
  (6, 13);
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux n a b = match n with
  |0 -> a
  |1 -> b
  |_ -> fib_aux (n - 1) b (b + a) 
;;


(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n =
  fib_aux n 1 1
;;

