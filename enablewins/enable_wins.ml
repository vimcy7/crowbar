module Enablewins = struct
  let t = 0
end

module M = Enablewins_imp.Make (Enablewins)
open Crowbar

let q_gen =
  fix (fun _ ->
  choose
   [ map [range 100] (fun i -> M.enable i);
     map [range 100] (fun i -> M.disable i)])

let pp_q ppf q =
    List.iter (fun e -> pp ppf "%d" e) [q] 
    
let q_gen = with_printer pp_q q_gen

  let _ =
    add_test ~name:"Commutativity" [q_gen; q_gen; q_gen] (fun lca a b ->
    let l = M.merge lca a b in
    let r = M.merge lca b a in
   (* Printf.printf("lca a b : %d %d %d\n") lca a b;*)
    check (M.flag l = M.flag r))

  let _ =
    add_test ~name:"Associativity" [q_gen; q_gen; q_gen; q_gen; q_gen] (fun lca1 lca2 a b c ->
    let mx = M.merge lca1 a b in
    let m1 = M.merge lca2 mx c in
    (*Printf.printf("lca1 lca2 a b c mx m1 : %d %d %d %d %d %d %d\n") lca1 lca2 a b c mx m1;*)
    let my = M.merge lca2 b c in
    let m2 = M.merge lca1 a my  in
   (* Printf.printf("lca1 lca2 a b c my m2 : %d %d %d %d %d %d %d\n") lca1 lca2 a b c my m2;*)
    check (M.flag m1 = M.flag m2))

  let _ =
    add_test ~name:"Idempotency" [q_gen; q_gen; q_gen] (fun lca a b ->
    let m1 = M.merge lca a b in
    let m2 = M.merge lca m1 b in
    check (M.flag m1 = M.flag m2))

  let _ =
    add_test ~name:"Positivity" [q_gen; q_gen; q_gen] (fun lca a b ->     
    let l = M.merge lca a b in
    check (l >= 0))

  let _ =
    add_test ~name:"Enablewins" [q_gen; q_gen; q_gen; q_gen; q_gen] (fun lca1 lca2 a b c ->
    let m1 = M.merge lca1 a b in
    let m2 = M.merge m1 b c in
    let m3 = M.merge lca2 m2 c in
    (*Printf.printf("m3 m2 c : %d %d %d\n") m3 m2 c;*)
    check (M.enablewins m3 m2 c))

 (* let _ =
      add_test ~name:"Disablewins" [q_gen; q_gen; q_gen; q_gen; q_gen] (fun lca1 lca2 a b c ->
      let m1 = M.merge lca1 a b in
      let m2 = M.merge m1 b c in
      let m3 = M.merge lca2 m2 c in
     (* Printf.printf("lca1 lca2 a b c : %d %d %d %d %d\n") lca1 lca2 a b c;*)
      check (M.disablewins m3 m2 c))  *)