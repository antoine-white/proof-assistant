(* testé avec OCaml version 4.08.1 *)

(* 1 La logique des propositions  *)

(* Question 1 :*)
(* 
- it's raining or it's sunny or it's raining and it's sunny
- it's raining and  it's sunny and it's raining or it's sunny
- it's raining or it's sunny and it's raining or it's sunny
- it's raining and it's monday or it's not raining
- it's monday or it's sunny or  it's not monday
*)

(* 1.1  Syntaxe de la logique des propositions *)
(* 1.1.1  Syntaxe concrète *)

(* Question 1 :*)
(* 
- (P or Q) or (P and Q)
- (P and  Q) and (P or Q)
- (P or Q) and (P or Q)
- (P and R) or (not P)
- (R or Q) or (not R)
*)

type proposition = string
;;

type bi_operator =
  |Or
  |And
  |Impl
;;

type uni_operator = |Not
;;

(** Cannot add \ because it escape char every time **)
let string_of_operator op =
  match op with
  |Or -> "|/"
  |And -> "/|" 
  |Impl -> "==>"
;;

let string_of_uni_operator op =
  match op with
  |Not -> "~"
;;

(* Question 2 :*)
type tformula =
  |Prop of proposition
  |UniApplication of (uni_operator * tformula)
  |BiApplication of (tformula * bi_operator * tformula)
;;

let exemple1 : tformula = BiApplication(BiApplication(Prop("P") , Or , Prop("Q")) , Or , BiApplication(Prop("P") , And , Prop("Q")));;
let exemple2 : tformula = BiApplication(BiApplication(Prop("P") , And , Prop("Q")) , And , BiApplication(Prop("P") , Or , Prop("Q")));;
let exemple3 : tformula = BiApplication(BiApplication(Prop("P") , Or , Prop("Q")) , And , BiApplication(Prop("P") , Or , Prop("Q")));;
let exemple4 : tformula = BiApplication(BiApplication(Prop("P") , Or , Prop("R")) , Or , UniApplication(Not , Prop("P")));;
let exemple5 : tformula = BiApplication(BiApplication(Prop("R") , Or , Prop("Q")) , Or , UniApplication(Not , Prop("R")));;

(* Question 3 :*)
let rec string_of_formula (t : tformula) : string =
  match t with 
  |Prop s -> s
  |UniApplication(conn,s2) -> "(" ^ (string_of_uni_operator conn) ^ " " ^ (string_of_formula s2) ^ ")"
  |BiApplication(s1,conn,s2) -> "(" ^ (string_of_formula s1) ^ " " ^ (string_of_operator conn) ^ " " ^ (string_of_formula s2) ^ ")"
;;

print_string ((string_of_formula exemple1) ^ "\n");;
print_string ((string_of_formula exemple2) ^ "\n");;
print_string ((string_of_formula exemple3) ^ "\n");;
print_string ((string_of_formula exemple4) ^ "\n");;
print_string ((string_of_formula exemple5) ^ "\n");;

(* 2  Vérification de la validité d’une formule propositionnelle *)
(* 2.1  Un interpréteur pour les formules propositionnelles *)

(* Question 1 :*)
type valuation = (string  * bool) list;;

let exempleValuation : valuation = [("Q",true);("P",false);("R",true)]
;;

(* Question 2 :*)
let rec give_value (value : valuation) (name : string) : bool = 
  match value with
  |[] -> failwith (name ^ " not in valuation.")
  |hd::tl -> let (s,v) = hd in if s = name then v else give_value tl name 
  ;;

  give_value exempleValuation "P"
  ;;

(* Question 3 :*)

let apply_bi_ope (op : bi_operator) (left : bool) (right : bool) = 
  match op with 
  |Or -> left || right
  |And -> left && right
  |Impl -> (not left) || right
;;

let apply_uni_ope (op : uni_operator) (right : bool) = 
  match op with 
  | Not -> not right
;;

let rec eval (value : valuation) (formula : tformula) : bool = 
  match formula with 
  |Prop s -> if (compare s "True") = 0 then true else if (compare s "False") = 0 then false else give_value value s
  |UniApplication(op,s) -> apply_uni_ope op (eval value s) 
  |BiApplication(s1,op,s2) -> apply_bi_ope op (eval value s1) (eval value s2)
  ;;


(* Question 4 :*)
print_string ((if (eval exempleValuation exemple1) then "true" else "false") ^ "\n");;
print_string ((if (eval exempleValuation exemple2) then "true" else "false") ^ "\n");;
print_string ((if (eval exempleValuation exemple3) then "true" else "false") ^ "\n");;
print_string ((if (eval exempleValuation exemple4) then "true" else "false") ^ "\n");;
print_string ((if (eval exempleValuation exemple5) then "true" else "false") ^ "\n");;

(* 2.2  Sémantique via les tables de vérité *)

(* Question 1 :*)
(*

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
| P | Q | ((P |/ Q) |/ (P /| Q)) |
|--------------------------------|
| V | V | V                      |
|--------------------------------|
| V | F | V                      |
|--------------------------------|
| F | V | V                      |
|--------------------------------|
| F | F | F                      |
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
|P | Q  | ((P /| Q) /| (P |/ Q)) | 
|--------------------------------|
| V | V | V                      |
|--------------------------------|
| V | F | F                      |
|--------------------------------|
| F | V | F                      |
|--------------------------------|
| V | V | F                      |
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

 *)

(* Question 2 :*)
let is_proposition s = not ((compare s "True") = 0 || (compare s "False") = 0)
;;

let rec vals (formula : tformula) =
  match formula with 
  |Prop s -> if (is_proposition s) then [s] else []
  |UniApplication(op,s) -> (vals s)
  |BiApplication(s1,op,s2) -> (vals s1) @ (vals s2)
;;

(* Question 3 :*)
let drop l = List.fold_right (fun v acc -> if List.mem v acc then acc else v::acc) l []
;;

let set_vars formula = drop (vals formula)
;;

(* Question 4 :*)
(* I cannot find (pow int int -> int) so I guess I have to make my own *)
let rec pow a b = 
  match b with
  | 0 -> 1
  | 1 -> a
  | n -> a * (pow a (n-1))
;;

(* some bit manipulation ...*)
(* in c it'll be : i & (1 << n) *)
let bit n i = (i land (1 lsl n) > 0);;


let enumerate_cases l : valuation list=
  (* recursive function that at each iteration create a sub list that is a valuation *)
  (* using binary.*)
  let rec sub l nb=
    if nb < 0 then []
    else (List.fold_right (fun v acc -> ((v,bit (List.length acc) nb))::acc) l [])::(sub l (nb-1))
  in 
  sub l ((pow 2 (List.length l)) - 1)
  ;;

(* Question 5 : *)
let truth_table formula =
  List.map (fun valua -> (valua,(eval valua formula )) ) (enumerate_cases (set_vars formula))
;;

truth_table exemple1;;

(* 2.3 Tautologies *)

(* Question 1 *)

let toto1 : tformula = BiApplication(Prop("True") , Or , BiApplication(Prop("Q") , Or , Prop("S")))
;;
truth_table toto1;;

let toto2 : tformula = BiApplication(Prop("True") , Or , BiApplication(Prop("Q") , And , Prop("S")))
;;
truth_table toto2;;

let toto3 : tformula = UniApplication(Not,BiApplication(Prop("False") , And , BiApplication(Prop("P") , Or , Prop("S"))))
;;
truth_table toto3;;

(* Question 2 *)

let tautology formula = List.for_all (fun (_,res) -> res) (truth_table formula)
;;

tautology exemple1;;
tautology toto3;;

(* 2.3 Question 3 : 
  A chaque fois que l'on ajoute une variable on augmente *2 les calculs
  donc pour 100 variables, on a 2^100 calcul : 1267650600228229401496703205376
*)

(* 3  Preuves de formules propositionnelles *)

(* 3.1  Les buts de preuve *)

(* Question 1 : *)
type ident = string;;

type goal = (ident * tformula) list
;;

(* Question 2 : *)
let print_goal (g : goal) =
  let rec sub g =
    match g with
    |[] -> ""
    |(_,f)::[] ->  "==============================\n" ^ (string_of_formula f) ^ "\n\n"
    |(i ,f)::tl ->  i ^ " : " ^ (string_of_formula f) ^ "\n" ^ (sub tl) ^ "\n"
  in sub(List.rev g)
  ;;
  
  let g1 : goal = [
    ("",BiApplication(Prop("P") , Or , Prop("Q")));
    ("H1",Prop("P"));
    ("H0",BiApplication(Prop("P") , Or , BiApplication(Prop("Q") , Impl , Prop("S"))));
    ]
    ;;
    
(*    print_string (print_goal g1);;*)
    
(* 3.2  Les tactiques de preuve *)

(* Question 1 : *)

type tactic = 
  | Exact of ident
  | Assume of tformula
  | And_Intro
  | Or_Intro_1
  | Or_Intro_2
  | Impl_Intro
  | Not_Intro
  | And_Elim_1 of ident
  | And_Elim_2 of ident
  | Impl_Elim of ident * ident
  | Not_Elim of ident * ident
  ;;
  
  
(* 3.3  Appliquer une tactique à un but *)

(* Question 1 : *)

let all_idents (g : goal) = List.map (fun (i,_) -> i) g 
;;

let fresh_ident (g : goal)=
  let rec sub i idents = 
    let tmp = ("H"^( string_of_int i)) in
    if List.mem tmp idents 
    then sub (i+1) idents
    else tmp
    in let idents = all_idents g 
  in sub 0 idents
;;
    
    fresh_ident g1;;
    
(* 3.3 Question 2 : *)

let valid_ident g id = List.mem id (all_idents g)
  ;;

valid_ident g1 "H1";;
valid_ident g1 "H5";;

(* 3.3 Question 3 : *)

let formula_from_ident g id =
  if (valid_ident g id)
  then snd(List.find (fun (ident,_) -> ident = id) g)
  else failwith ("Unknown variable name : " ^ id )
  ;;
  
  formula_from_ident g1 "H0";;
  

let apply_tactic (t : tactic) (g : goal) : goal list =
  let tail = (List.tl g) in 
  let head = (List.hd g) in 
  let (_,formula) = head in
  match t with
  | Exact(i) -> let f = (formula_from_ident g i) in if f = formula then [("",Prop("True"))::tail] else failwith ("Formula " ^ (string_of_formula f) ^ " != " ^ (string_of_formula formula) );
  | Assume(f) ->  [(head::((fresh_ident g),f)::tail);(("",f)::tail)] 
  | And_Intro -> 
    (match formula with
    |BiApplication(f1,op,f2) -> 
      (match op with 
      |And -> [(("",f1)::tail);(("",f2)::tail)]
      |_ -> failwith "Formula is not an And")
    |_ -> failwith "Formula is not an And")  
  | Or_Intro_1 ->
    (match formula with
      |BiApplication(f1,op,f2) -> 
        (match op with 
        |Or -> [("",f1)::tail]
        |_ -> failwith "Formula is not an Or")
      |_ -> failwith "Formula is not an Or")  
  | Or_Intro_2 ->
    (match formula with
      |BiApplication(f1,op,f2) -> 
        (match op with 
        |Or -> [("",f2)::tail]
        |_ -> failwith "Formula is not an Or")
      |_ -> failwith "Formula is not an Or")  
  | Impl_Intro ->
    (match formula with
      |BiApplication(f1,op,f2) -> 
        (match op with 
        |Impl -> [("",f2)::(((fresh_ident g),f1)::tail)]
        |_ -> failwith "Formula is not an Implication")
      |_ -> failwith "Formula is not an Implication") 
  | Not_Intro ->
    (match formula with
      |UniApplication(op,f) -> [("",Prop "False")::(((fresh_ident g),f)::tail)]
      |_ -> failwith "Formula is not an Not Formula") 
  | And_Elim_1(i) ->
    (match (formula_from_ident g i) with
      |BiApplication(f1,op,f2) -> 
        (match op with 
        |And -> [head::(((fresh_ident g),f1)::tail)]
        |_ -> failwith "Formula is not an And")
      |_ -> failwith "Formula is not an And"
    )
  | And_Elim_2(i) -> 
    (match (formula_from_ident g i) with
      |BiApplication(f1,op,f2) -> 
        (match op with 
        |And -> [head::(((fresh_ident g),f2)::tail)]
        |_ -> failwith "Formula is not an And")
      |_ -> failwith "Formula is not an And"
    )
  | Impl_Elim(i1,i2) ->
    let formula1 = (formula_from_ident g i2) in 
    let formula2 = (formula_from_ident g i1) in 
    (match formula2 with
      |BiApplication(f1,op,f2) -> 
        (match op with 
        |Impl -> (
          if formula1 = f1
          then [head::(((fresh_ident g),f2)::tail)]
          else failwith "Second hypothesis does not match the assumption of the first hypothesis"
        )
        |_ -> failwith "Formula is not an Implication")
      |_ -> failwith "Formula is not an Implication")
  | Not_Elim(i1,i2) ->
  let formula1 = (formula_from_ident g i1) in 
  let formula2 = (formula_from_ident g i2) in 
    (match formula1 with
      |UniApplication(op,f) -> 
        if formula2 = f
        then [("",Prop "False")::(((fresh_ident g),Prop("False"))::tail)]
        else failwith "Second hypothesis does not match the body of the first hypothesis"
      |_ -> failwith "Formula is not an Not Formula") 
;;

(* 3.3 Question 4 : *)
let form : tformula = BiApplication(BiApplication(BiApplication(Prop("P"),Or,Prop("Q")),Impl,Prop("R")) , Impl , BiApplication(BiApplication(Prop("P"),Impl,Prop("R")) , And , BiApplication(Prop("Q"),Impl,Prop("R"))))
;;

(* code *)
let g2 : goal = [("",form)];;
print_string (print_goal g2);;

print_string "\n-------------Impl_Intro---------------\n";;
let l = (apply_tactic Impl_Intro g2);;
print_string (print_goal (List.hd l));;

print_string "\n-------------And_Intro---------------\n";;
let l2 = (apply_tactic And_Intro (List.hd l));;
print_string (print_goal (List.hd l2));;
print_string (print_goal (List.hd (List.tl l2)));;

print_string "\n-------------Impl_Intro (on subgoal1)---------------\n";;
let l3  = (apply_tactic Impl_Intro (List.hd l2));;
print_string (print_goal (List.hd l3));;

print_string "\n-------------assume (P|/Q). (on subgoal1)---------------\n";;
let l4  = (apply_tactic (Assume(BiApplication(Prop("P"),Or,Prop("Q")))) (List.hd l3));;
print_string (print_goal (List.hd l4));;
print_string (print_goal (List.hd (List.tl l4)));;

print_string "\n-------------Impl_Elim in H0 and H2. (on subgoal 1 -1)---------------\n";;
let l5  = (apply_tactic (Impl_Elim("H0","H2")) (List.hd l4));;
print_string (print_goal (List.hd l5));;

print_string "\n-------------Exact H3. (on subgoal 1 -1) (subgoal 1-1 done)---------------\n";;
let l6  = (apply_tactic (Exact("H3")) (List.hd l5));;
print_string (print_goal (List.hd l6));;

print_string (print_goal (List.hd (List.tl l4)));;

print_string "\n-------------Or_Intro_1. (on subgoal 1 -2) ---------------\n";;
let l7  = (apply_tactic (Or_Intro_1) (List.hd (List.tl l4)));;
print_string (print_goal (List.hd l7));;

print_string "\n-------------Exact H0. (on subgoal 1 -2) (subgoal 1-2 done)---------------\n";;
let l8  = (apply_tactic (Exact("H1")) (List.hd l7));;
print_string (print_goal (List.hd l8));;

print_string "\n-------------Impl_Intro (on subgoal 2)---------------\n";;
let l9  = (apply_tactic Impl_Intro (List.hd (List.tl l2)));;
print_string (print_goal (List.hd l9));;

print_string "\n-------------assume (P|/Q). (on subgoal 2)---------------\n";;
let l10  = (apply_tactic (Assume(BiApplication(Prop("P"),Or,Prop("Q")))) (List.hd l9));;
print_string (print_goal (List.hd l10));;
print_string (print_goal (List.hd (List.tl l10)));;

print_string "\n-------------Impl_Elim in H0 and H2. (on subgoal 2-1)---------------\n";;
let l11 = (apply_tactic (Impl_Elim("H0","H2")) (List.hd l10));;
print_string (print_goal (List.hd l11));;

print_string "\n-------------Exact H3. (on subgoal 2 -1) (subgoal 2-1 done)---------------\n";;
let l12  = (apply_tactic (Exact("H3")) (List.hd l11));;
print_string (print_goal (List.hd l12));;

print_string "\n-------------Or_Intro_2. (on subgoal 2 -2) ---------------\n";;
let l13  = (apply_tactic (Or_Intro_2) (List.hd (List.tl l10)));;
print_string (print_goal (List.hd l13));;

print_string "\n-------------Exact H1. (on subgoal 2 -2) (subgoal 2-2 done)---------------\n";;
let l14  = (apply_tactic (Exact("H1")) (List.hd l13));;
print_string (print_goal (List.hd l14));;

print_string "\n-------------All goals Done---------------\n";;

(* RESULTAT :  

==============================
(((P |/ Q) ==> R) ==> ((P ==> R) /| (Q ==> R)))


-------------Impl_Intro---------------
H0 : ((P |/ Q) ==> R)
==============================
((P ==> R) /| (Q ==> R))



-------------And_Intro---------------
H0 : ((P |/ Q) ==> R)
==============================
(P ==> R)


H0 : ((P |/ Q) ==> R)
==============================
(Q ==> R)



-------------Impl_Intro (on subgoal1)---------------
H0 : ((P |/ Q) ==> R)
H1 : P
==============================
R




-------------assume (P|/Q). (on subgoal1)---------------
H0 : ((P |/ Q) ==> R)
H1 : P
H2 : (P |/ Q)
==============================
R




H0 : ((P |/ Q) ==> R)
H1 : P
==============================
(P |/ Q)




-------------Impl_Elim in H0 and H2. (on subgoal 1 -1)---------------
H0 : ((P |/ Q) ==> R)
H1 : P
H2 : (P |/ Q)
H3 : R
==============================
R






-------------Exact H3. (on subgoal 1 -1) (subgoal 1-1 done)---------------
H0 : ((P |/ Q) ==> R)
H1 : P
H2 : (P |/ Q)
H3 : R
==============================
True





H0 : ((P |/ Q) ==> R)
H1 : P
==============================
(P |/ Q)




-------------Or_Intro_1. (on subgoal 1 -2) ---------------
H0 : ((P |/ Q) ==> R)
H1 : P
==============================
P




-------------Exact H0. (on subgoal 1 -2) (subgoal 1-2 done)---------------
H0 : ((P |/ Q) ==> R)
H1 : P
==============================
True




-------------Impl_Intro (on subgoal 2)---------------
H0 : ((P |/ Q) ==> R)
H1 : Q
==============================
R




-------------assume (P|/Q). (on subgoal 2)---------------
H0 : ((P |/ Q) ==> R)
H1 : Q
H2 : (P |/ Q)
==============================
R




H0 : ((P |/ Q) ==> R)
H1 : Q
==============================
(P |/ Q)




-------------Impl_Elim in H0 and H2. (on subgoal 2-1)---------------
H0 : ((P |/ Q) ==> R)
H1 : Q
H2 : (P |/ Q)
H3 : R
==============================
R






-------------Exact H3. (on subgoal 2 -1) (subgoal 2-1 done)---------------
H0 : ((P |/ Q) ==> R)
H1 : Q
H2 : (P |/ Q)
H3 : R
==============================
True






-------------Or_Intro_2. (on subgoal 2 -2) ---------------
H0 : ((P |/ Q) ==> R)
H1 : Q
==============================
Q




-------------Exact H1. (on subgoal 2 -2) (subgoal 2-2 done)---------------
H0 : ((P |/ Q) ==> R)
H1 : Q
==============================
True




-------------All goals Done---------------
*)