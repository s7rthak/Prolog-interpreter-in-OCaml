exception Error of string                                                                                           (* Exceptions used. *)
exception Unsolvable of string

type constant = Constant of string                                                                                  (* Declaration of all types to be used. *)
type variable = Variable of string
type atofor = Atofor of string * term list and term = C of constant | V of variable | F of atofor
type head = atofor
type body = atofor list
type clause = Fact of head | Rule of head * body
type goal = atofor list
type program = clause list

let var v = Variable v                                                                                              (* Useful functions that convert a type to another or basically my type-casts *)
let var_t v = V (var v)
let var_to_term v = V(v)
let const c = Constant c
let const_t c = C (Constant c)
let const_to_term c = C(c)
let atoform p l = Atofor (p,l)
let atoform_t p l = F (atoform p l)
let atoform_to_term f = F(f)
let fact f = Fact f
let rule h b = Rule (h,b)
let head_of_clause_to_term c = 
  match c with
  | Fact h -> atoform_to_term h
  | Rule (h,b) -> atoform_to_term h
let body_clause c = 
  match c with 
  | Fact h -> []
  | Rule (h,b) -> b

let atofor_pred (x:atofor) = 
  match x with 
  | Atofor(p,l) -> p 

let atofor_equal (x:atofor) (y:atofor) = ((atofor_pred x) = (atofor_pred y))


let string_var v = match v with Variable(a) -> a                                                                    (* String of various types for output purpose. *)
let rec string_term (t: term) = 
  match t with  
  | C(Constant(c)) -> c
  | V(Variable(v)) -> v
  | F(Atofor(p,q)) -> p ^ "(" ^ (string_term_list q) ^ ")" 
and string_term_list (a: term list) =
  match a with
  | [] -> ""
  | x::[] -> string_term x
  | x::y::rest -> (string_term x) ^ "," ^ (string_term_list (y::rest)) 

let fst p =                                                                                                         (* Some utility functions. *)
  match p with
    | (x,y) -> x

let scd p =
  match p with 
    | (x,y) -> y

let orr a b = a || b
let andd a b = a && b

let rec var_in_term v t =                                                                                           (* Checks whether a variable is present in term (Occurs-check). *)
  match t with
    | C(con) -> false
    | V(var) -> if var = v then true else false
    | F(func) -> 
      (match func with
        | Atofor(f,g) -> List.fold_left orr false (List.map (var_in_term v) g)
      )

let var_in_atofor v f =                                                                                             (* Checks if variable is in atomic formula. *)
  match f with 
    | Atofor(f,g) -> List.fold_left orr false (List.map (var_in_term v) g)

module Vset = Set.Make(struct type t = variable let compare = compare end)                                          (* The standard library set module is imported. *)

let rec var_set_term t =                                                                                            (* The set of variables in a term. *)
  match t with
    | C(con) -> Vset.empty
    | V(var) -> Vset.singleton var
    | F(func) ->
      (match func with
        | Atofor(f,g) -> List.fold_left Vset.union Vset.empty (List.map var_set_term g)
      )

let var_set_atofor a =                                                                                              (* The set of variables in atomic formula. *)
  match a with
    | Atofor(f,g) -> List.fold_left Vset.union Vset.empty (List.map var_set_term g)

let var_set_clause c =                                                                                              (* The set of variables in a clause. *)
  match c with
    | Fact(h) -> var_set_atofor h
    | Rule(h,b) -> Vset.union (var_set_atofor h) (List.fold_left Vset.union Vset.empty (List.map var_set_atofor b))

module Sub = Map.Make(struct type t = variable let compare = compare end)                                           (* The standard library map module is imported. A substitution is modelled as a map with keys as variables and values as terms. *)

let rec sub_term s t =                                                                                              (* Applies a substitution to a term. *)
  match t with 
    | C(con) -> t
    | V(var) -> if Sub.mem var s then Sub.find var s else t
    | F(func) -> 
      (match func with
        | Atofor(f,g) -> atoform_t f (List.map (sub_term s) g)
      )
    
let sub_atofor s a =                                                                                                (* Applies a substitution to a atomic formula. *)
  match a with 
    | Atofor(f,g) -> atoform f (List.map (sub_term s) g)

let sub_clause s c =                                                                                                (* Applies a substitution to a clause *)
  match c with
    | Fact(h) -> fact (sub_atofor s h)
    | Rule(h,b) -> rule (sub_atofor s h) (List.map (sub_atofor s) b)

let rand (a:variable) b c = None                                                                                    (* A random function which really has no use other than in union of substitution. *)

let rec aux a1 a2 u =                                                                                               (* Auxiliary functions for mgu-finding. *)
  if a1 = a2 then (true,u) else 
    match a1, a2 with
      | C(con1), C(con2) -> (false,u)
      | C(con1), F(func2) -> (false,u)
      | F(func1), C(con2) -> (false,u)
      | C(con1), V(var2) -> let s = Sub.add var2 (const_to_term con1) u in aux a1 (sub_term s a2) s
      | V(var1), C(con2) -> let s = Sub.add var1 (const_to_term con2) u in aux (sub_term s a1) a2 s
      | V(var1), V(var2) -> let s = Sub.add var1 (var_to_term var2) u in aux (sub_term s a1) a2 s
      | V(var1), F(func2) -> if var_in_atofor var1 func2 then (false,u) else let s = Sub.add var1 (atoform_to_term func2) u in aux (sub_term s a1) a2 s
      | F(func1), V(var2) -> if var_in_atofor var2 func1 then (false,u) else let s = Sub.add var2 (atoform_to_term func1) u in aux a1 (sub_term s a2) s
      | F(func1), F(func2) -> 
        (match func1, func2 with
          Atofor(f1,g1),Atofor(f2,g2) -> if f1 = f2 && List.length g1 = List.length g2 then (fst (auxi g1 g2), Sub.union rand (scd (auxi g1 g2)) u) else (false, u) 
        )
and auxi l1 l2 =
  match l1, l2 with
    | [], [] -> (true,Sub.empty)
    | h1::t1, h2::t2 -> let x1 = auxi t1 t2 in let x2 = aux (sub_term (scd x1) h1) (sub_term (scd x1) h2) Sub.empty in (fst x1 && fst x2,Sub.union rand (scd x1) (scd x2))
    | [], h2::t2 -> raise (Error "wont occur")
    | h1::t1, [] -> raise (Error "wont occur")


let mgu t1 t2 =                                                                                                     (* The abstrcted mgu-finder. *)
  let s = aux t1 t2 Sub.empty in if fst s then scd s else raise (Error "Not-Unifiable")

let mgu_bool t1 t2 = aux t1 t2 Sub.empty                                                                            (* The mgu-finder which returns a pair of bool*Sub.t term which is true if a substitution is found and the required substitution and false otherwise. *)

module LL = Batteries.LazyList                                                                                      (* The LazyList module from the Batteries package (along with Core, it is one of popular packages for OCaml). This package has to be downloaded first through opam(OCaml package-manager). OCaml unlike some other functional languages doesn't provide in-built lazy support. *)

(* let rec solve_sh (database: clause list) q sub =                                                                 (* Here is the eager-evaluation version of goals where all the satisfying substitutions are returned at once. *)
  let pos = List.map (mgu_bool (atoform_to_term q)) (List.map head_of_clause_to_term database) in
    let zipped = List.combine pos database in
        let rem = List.filter (fun e -> fst (fst e)) zipped in 
          let clean = List.map (fun (x,y) -> (Sub.union rand sub (scd x),(body_clause y))) rem in 
            back_tr database clean
and back_tr (database: clause list) options =
  match options with 
  | [] -> []
  | h::t -> (solve_dp database h) @ (back_tr database t)
and solve_dp (database: clause list) (sub,glist) =
  match glist with
  | [] -> [sub]
  | g::rest -> let psub = solve_sh database g sub in
                let nopt = List.map (fun e -> (e,rest)) psub in
                  List.flatten (List.map (solve_dp database) (nopt)) 
let rec solve (database: program) sub (goal_list: goal) =
  match goal_list with 
  | [] -> raise (Unsolvable "goal-less")
  | x::[] -> solve_sh database x sub
  | x::y::rest -> let sol = solve_sh database x sub in 
                    List.flatten (List.map (fun e -> solve database e (List.map (sub_atofor e) (List.tl goal_list))) sol)   *)


let rec solve_sh (database: clause list) q sub =                                                                    (* Here is the lazy version of the above algorithm using LazyLists where satisfying substitutions are found if needed. It supports backtracking and choice points. *)
  let pos = List.map (mgu_bool (atoform_to_term q)) (List.map head_of_clause_to_term database) in
    let zipped = List.combine pos database in
        let rem = List.map (fun e -> ((scd (fst e)),(scd e))) (List.filter (fun e -> fst (fst e)) zipped) in 
          let clean = List.map (fun (x,y) -> let upsub = Sub.union rand sub x in (upsub,(List.map (sub_atofor upsub) (body_clause y)))) rem in 
            print_int (List.length clean); print_newline ();back_tr database clean
and back_tr (database: clause list) options =
  match options with 
  | [] -> LL.nil
  | h::t -> LL.append (solve_dp database h) (back_tr database t)
and solve_dp (database: clause list) (sub,glist) =
  match glist with
  | [] -> LL.cons sub LL.nil
  | g::rest -> let psub = solve_sh database g sub in
                let nopt = LL.map (fun e -> (e,(List.map (sub_atofor e) rest))) psub in
                  LL.concat (LL.map (solve_dp database) (nopt))
let rec solve (database: program) sub (goal_list: goal) =
  match goal_list with 
  | [] -> raise (Unsolvable "goal-less")
  | x::[] -> solve_dp database (sub,goal_list)
  | x::y::rest -> let sol = solve_dp database (sub,goal_list) in 
                    LL.concat (LL.map (fun e -> solve database e (List.map (sub_atofor e) (List.tl goal_list))) sol)

let rec search (database: program) (formula: atofor) =
  match database with
  | [] -> failwith "Not found"
  | x::y -> 
  (match x with 
  | Fact(h) -> if atofor_equal h formula then x else (search y formula)
  | Rule(h,b) -> if atofor_equal h formula then x else (search y formula)
  )

let rec findFactList (database: program) (goal: goal) =
  List.concat (List.map (simplify database) goal)
and simplify (database: program) (formula: atofor) =
  let x = search database formula in
    (match x with
    | Fact(h) -> formula::[]
    | Rule(h,b) -> findFactList database (List.map (sub_atofor (mgu (atoform_to_term h) (atoform_to_term formula))) b)
    )

let variableSet (goal: goal) = List.fold_left (Vset.union) Vset.empty (List.map var_set_atofor goal)

let rec deepSearch sub v = 
  match Sub.find v sub with
  | C(c) -> C(c)
  | V(v) -> deepSearch sub v
  | _ -> raise (Error "unexpected")
  
let findNeededVar sub set =
  let ans = ref Sub.empty in
    Vset.iter (fun s -> ans := Sub.add s (deepSearch sub s) !ans; ()) set; !ans





































                  




(* and solve_dp sh_res database =
  match sh_res with
  | [] -> []
  | h::t -> (solve database [] (List.map (sub_atofor (fst h)) (scd h))) @ solve_dp t database *)
  (* let solve_shallow (database: program) sub (sat: atofor) =
  let pos = List.map (mgu_bool sat) program in
    let zipped = List.combine pos program 
      let shallow = List.filter (fun e -> fst (fst e)) pos in
        let rem = List.map (fun (x,y) -> (scd x,body_clause y)) shallow in
          deep database rem sub
and solve_deep database rem = 
  let rest = List.map (fun e -> solve database (fst e) (List.map (sub_atofor (fst e)) (scd e)) rem in 
    let left = List.filter (fun e -> List.length e > 0) rest in
and solver goal = 
  match goal_list with 
  | *) 
  
  (* let aux database left_goals options = 
    match options with
    | [] -> raise (Unsolvable "unsat")
    | h::t ->  *)