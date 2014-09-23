open P3_core
open P3_e3

(**********************************************************************)
(* testing *)

let get_rules cl = (match cl.sp with
  | TP _ -> (failwith "get_rules: TP")
  | NP np -> (np.np_rhss ()))

let raw_eps = (fun x -> match x with `SS(s,i,j) -> [i]) 
let eps = mktmparser raw_eps (fun ss -> [0]) 
let raw_a1 = (fun x -> match x with `SS(s,i,j) -> if i < j && s.[i]='1' then [i+1] else []) 
let _1 = mktmparser raw_a1 (fun ss -> if content ss = "1" then [1] else failwith "_1 mmb") 

let rec get_alphas xs = (match xs with 
  | [] -> []
  | x::xs -> [([],x::xs)] @ (List.map (fun (xs,ys) -> (x::xs,ys)) (get_alphas xs)))

let rec prefixes xs = match xs with 
  | [] -> [[]]
  | x::xs -> []::(List.map (fun ys -> x::ys) (prefixes xs))

let alphas rs = rs |> List.map prefixes |> List.concat |> List.map List.rev

let test_1 () = 
  let _E = ref (mk_pre_parser()) in
  let _E_alts = lazy(alts[
    (!_E >-- !_E >- !_E) >> (fun ((x,y),z) -> (x+y+z));
    rhs _1;
    rhs eps])
  in
  let _ = _E := mkntparser !_E (fun () -> Lazy.force _E_alts) in
  let txt = "1111" in
  let l = String.length txt in 
  let (o,tmo) = run_earley !_E txt l in
  let rs = get_rules !_E in
  let alphas' = rs |> List.map get_alphas |> List.concat in
  let string_of_sp s = (
    if (s == ((!_E).sp)) then "E" 
    else if (s == _1.sp) then "1" 
    else if (s == eps.sp) then "eps" 
    else failwith "string_of_sp")
  in
  let args : (string ty_rhs_syms_list * string sym_plus) list  = alphas' |> (List.map (fun (xs,ys) -> (List.rev xs,ys |> List.hd |> snd))) in
  let ijs = allpairs (fun x y -> x,y) (upto 0 l) (upto 0 l) in
  let args' = allpairs (fun x y -> x,y) args ijs in
  let f1 ((syms,sp),(i,j)) = 
    let ks = o (syms,sp) (i,j) in
    let (syms,sp) = (List.map (fun (x,y) -> string_of_sp y) syms,string_of_sp sp) in
    (syms,sp,i,j,List.sort Pervasives.compare ks)
  in
  let rs = List.sort Pervasives.compare (List.map f1 args') in
  let expected_results = 
    [([], "1", 0, 0, []); ([], "1", 0, 1, [0]); ([], "1", 0, 2, []);
     ([], "1", 0, 3, []); ([], "1", 0, 4, []); ([], "1", 1, 0, []);
     ([], "1", 1, 1, []); ([], "1", 1, 2, [1]); ([], "1", 1, 3, []);
     ([], "1", 1, 4, []); ([], "1", 2, 0, []); ([], "1", 2, 1, []);
     ([], "1", 2, 2, []); ([], "1", 2, 3, [2]); ([], "1", 2, 4, []);
     ([], "1", 3, 0, []); ([], "1", 3, 1, []); ([], "1", 3, 2, []);
     ([], "1", 3, 3, []); ([], "1", 3, 4, [3]); ([], "1", 4, 0, []);
     ([], "1", 4, 1, []); ([], "1", 4, 2, []); ([], "1", 4, 3, []);
     ([], "1", 4, 4, []); ([], "E", 0, 0, [0]); ([], "E", 0, 1, [0]);
     ([], "E", 0, 2, [0]); ([], "E", 0, 3, [0]); ([], "E", 0, 4, [0]);
     ([], "E", 1, 0, []); ([], "E", 1, 1, [1]); ([], "E", 1, 2, [1]);
     ([], "E", 1, 3, [1]); ([], "E", 1, 4, [1]); ([], "E", 2, 0, []);
     ([], "E", 2, 1, []); ([], "E", 2, 2, [2]); ([], "E", 2, 3, [2]);
     ([], "E", 2, 4, [2]); ([], "E", 3, 0, []); ([], "E", 3, 1, []);
     ([], "E", 3, 2, []); ([], "E", 3, 3, [3]); ([], "E", 3, 4, [3]);
     ([], "E", 4, 0, []); ([], "E", 4, 1, []); ([], "E", 4, 2, []);
     ([], "E", 4, 3, []); ([], "E", 4, 4, [4]); ([], "eps", 0, 0, [0]);
     ([], "eps", 0, 1, []); ([], "eps", 0, 2, []); ([], "eps", 0, 3, []);
     ([], "eps", 0, 4, []); ([], "eps", 1, 0, []); ([], "eps", 1, 1, [1]);
     ([], "eps", 1, 2, []); ([], "eps", 1, 3, []); ([], "eps", 1, 4, []);
     ([], "eps", 2, 0, []); ([], "eps", 2, 1, []); ([], "eps", 2, 2, [2]);
     ([], "eps", 2, 3, []); ([], "eps", 2, 4, []); ([], "eps", 3, 0, []);
     ([], "eps", 3, 1, []); ([], "eps", 3, 2, []); ([], "eps", 3, 3, [3]);
     ([], "eps", 3, 4, []); ([], "eps", 4, 0, []); ([], "eps", 4, 1, []);
     ([], "eps", 4, 2, []); ([], "eps", 4, 3, []); ([], "eps", 4, 4, [4]);
     (["E"], "E", 0, 0, [0]); (["E"], "E", 0, 1, [0; 1]);
     (["E"], "E", 0, 2, [0; 1; 2]); (["E"], "E", 0, 3, [0; 1; 2; 3]);
     (["E"], "E", 0, 4, [0; 1; 2; 3; 4]); (["E"], "E", 1, 0, []);
     (["E"], "E", 1, 1, [1]); (["E"], "E", 1, 2, [1; 2]);
     (["E"], "E", 1, 3, [1; 2; 3]); (["E"], "E", 1, 4, [1; 2; 3; 4]);
     (["E"], "E", 2, 0, []); (["E"], "E", 2, 1, []); (["E"], "E", 2, 2, [2]);
     (["E"], "E", 2, 3, [2; 3]); (["E"], "E", 2, 4, [2; 3; 4]);
     (["E"], "E", 3, 0, []); (["E"], "E", 3, 1, []); (["E"], "E", 3, 2, []);
     (["E"], "E", 3, 3, [3]); (["E"], "E", 3, 4, [3; 4]); (["E"], "E", 4, 0, []);
     (["E"], "E", 4, 1, []); (["E"], "E", 4, 2, []); (["E"], "E", 4, 3, []);
     (["E"], "E", 4, 4, [4]); (["E"; "E"], "E", 0, 0, [0]);
     (["E"; "E"], "E", 0, 1, [0; 1]); (["E"; "E"], "E", 0, 2, [0; 1; 2]);
     (["E"; "E"], "E", 0, 3, [0; 1; 2; 3]);
     (["E"; "E"], "E", 0, 4, [0; 1; 2; 3; 4]); (["E"; "E"], "E", 1, 0, []);
     (["E"; "E"], "E", 1, 1, [1]); (["E"; "E"], "E", 1, 2, [1; 2]);
     (["E"; "E"], "E", 1, 3, [1; 2; 3]); (["E"; "E"], "E", 1, 4, [1; 2; 3; 4]);
     (["E"; "E"], "E", 2, 0, []); (["E"; "E"], "E", 2, 1, []);
     (["E"; "E"], "E", 2, 2, [2]); (["E"; "E"], "E", 2, 3, [2; 3]);
     (["E"; "E"], "E", 2, 4, [2; 3; 4]); (["E"; "E"], "E", 3, 0, []);
     (["E"; "E"], "E", 3, 1, []); (["E"; "E"], "E", 3, 2, []);
     (["E"; "E"], "E", 3, 3, [3]); (["E"; "E"], "E", 3, 4, [3; 4]);
     (["E"; "E"], "E", 4, 0, []); (["E"; "E"], "E", 4, 1, []);
     (["E"; "E"], "E", 4, 2, []); (["E"; "E"], "E", 4, 3, []);
     (["E"; "E"], "E", 4, 4, [4])]
  in
  let _ = assert(rs = expected_results) in
  let _ = assert([4] = run_parser3_string !_E txt) in
  ()
  
(* some basic testing *)
let test_2 () = 
  let _E = ref (mk_pre_parser()) in
  let _F = ref (mk_pre_parser()) in
  let _G = ref (mk_pre_parser()) in
  let _E_alts = lazy(alts[
    (!_E >-- !_E >- !_E) >> (fun ((x,y),z) -> (x+y+z));
    (!_F >-- !_E) >> (fun (x,y) -> x+y);
    (!_G >-- !_E) >> (fun (x,y) -> x+y);
    rhs _1;
    rhs eps])
  in
  let _ = _E := mkntparser !_E (fun () -> Lazy.force _E_alts) in
  (* F parses 11 *)
  let _F_alts = lazy(alts[
    (_1 >-- _1) >> (fun (x,y) -> x+y)])
  in
  let _ = _F := mkntparser !_F (fun () -> Lazy.force _F_alts) in
  (* G parses 111 *)
  let _G_alts = lazy(alts[
    (_1 >-- _1 >- _1) >> (fun ((x,y),z) -> x+y+z)])
  in
  let _ = _G := mkntparser !_G (fun () -> Lazy.force _G_alts) in
  let p = !_E in
  let txt = "1111" in
  let _ = assert([4] = run_parser3_string p txt) in
  (* we also check that the oracle and tmoracle are correct *)
  let (o,tmo) = run_earley p txt (String.length txt) in
  (* for o, we expect that checking (!_F).sp gives us a substring of
     length 2; really we want to tabulate the oracle over certain
     arguments *)
  let _ = assert([0] = o ([],(!_F).sp) (0,2)) in
  let _ = assert([1] = o ([],(!_F).sp) (1,3)) in
  let _ = assert([2] = o ([],(!_F).sp) (2,4)) in
  let _ = assert([2] = o ([],(!_F).sp) (2,4)) in
  let _ = assert([0] = o ([],(!_G).sp) (0,3)) in
  let _ = assert([1] = o ([],(!_G).sp) (1,4)) in
  let _ = assert([] = o ([],(!_G).sp) (0,4)) in
  let _ = assert([0] = o ([],(!_E).sp) (0,4)) in
  ()
  
  

