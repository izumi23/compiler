open Cparse
open Genlab

(*Pour bien coder:
se ramener à un seul  buffer ctrl x 1
ouvrir un deuxième buffer ctrl x 3
aller dessus ctrl x o
ouvrir le fichier cparse.mli ctrl x ctrl f
revenir sur ce buffer ctrl x o
remplir ce giga patern matching*)



let compile out decl_list =


  let todo s = Printf.fprintf out "#TODO %s\n" s in
  let p = Printf.fprintf in



(*var associe à chaque variable locale sa position relative sur la pile.
  Pour cela on maintient à jour le compteur var_count.*)
  let var_count = ref 0 in
  let var = ref (Hashtbl.create 8) in

(*Une liste des noms de fonctions*)
  let functions = ref [] in

  let arg_registers = [|"%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9"|] in




(*Renvoie l'adresse de la variable s, selon qu'elle soit locale ou globale*)

  let var_location s =
    if Hashtbl.mem !var s then
      let i = Hashtbl.find !var s in
      Printf.sprintf "%d(%%rbp)" (-8*i)
    else Printf.sprintf "%s(%%rip)" s
  in




(*Fonctions pour une première lecture, pour stocker les chaînes de caractères*)

  let strings = Hashtbl.create 8 in
  let string_count = ref 0 in

  let rec first_decl_list lcl = List.iter first_decl lcl

  and first_decl = function
  | CDECL _ -> ()
  | CFUN (_, _, _, lc) -> first_code lc

  and first_code (_, c) = match c with
  | CBLOCK (_, lcl) -> List.iter first_code lcl
  | CEXPR e -> first_expr e
  | CIF (_, c1, c2) -> first_code c1; first_code c2
  | CWHILE (_, c) -> first_code c
  | _ -> ()

  and first_expr (_, e) = match e with

  (*le seul cas important!*)

  | STRING s when not (Hashtbl.mem strings s) ->
      p out "        .align 8\n";
      p out ".LC%d:\n        .string %S\n" !string_count s;
      Hashtbl.add strings s !string_count;
      incr string_count

  | CALL (_, lel) -> List.iter first_expr lel
  | EIF (_, e1, e2) -> first_expr e1; first_expr e2
  | ESEQ lel -> List.iter first_expr lel
  | _ -> ()

  in





(*Fonctions principales qui compilent le code*)


  let rec compile_decl_list globl = function
  | [] -> ()
  | h::t -> compile_decl globl h; compile_decl_list globl t



  and compile_mop mop (l, e) = match mop, e with

  | M_MINUS, _ ->
      compile_expr "%rax" (l, e);
      p out "        negq    %%rax\n"

  | M_NOT, _ ->
      compile_expr "%rax" (l, e);
      p out "        notq    %%rax\n"

  | M_POST_INC, VAR s ->
      p out "        addq    $1, %s\n" (var_location s)

  | M_POST_DEC, VAR s ->
      p out "        subq    $1, %s\n" (var_location s)

  | M_PRE_INC, VAR s ->
      p out "        addq    $1, %s\n" (var_location s);
      p out "        addq    $1, %%rax\n"

  | M_PRE_DEC, VAR s ->
      p out "        subq    $1, %s\n" (var_location s);
      p out "        subq    $1, %%rax\n"

  | _ -> failwith "cas impossible"




(*Appelé lors d'un CALL, compile les arguments donnés à la fonction appelée.
  Pour cela on conserve dans i le rang du prochain argument à traiter.*)

  and push_args i = function
  | [] -> ()
  | e::t ->
      if i < 6 then compile_expr arg_registers.(i) e
      else compile_expr "STACK" e;
      push_args (i-1) t




(*Quelques optimisations pour les cas VAR, CST et STRING, où l'on écrit
  directement sur un registre donné ou la pile, selon la valeur de dest.*)

  and compile_expr dest (_, e) = match e with

  | VAR s ->
      if dest = "STACK" then p out "        pushq   %s\n" (var_location s)
      else p out "        movq    %s, %s\n" (var_location s) dest

  | CST n ->
      if dest = "STACK" then p out "        pushq   $%d\n" n
      else p out "        movq    $%d, %s\n" n dest

  | STRING s ->
      let loc = Hashtbl.find strings s in
      if dest = "STACK" then begin
        p out "        leaq    .LC%d(%%rip), %%rax\n" loc;
        p out "        pushq   %%rax\n"
      end
      else p out "        leaq    .LC%d(%%rip), %s\n" loc dest

  | SET_VAR (s, e) ->
      compile_expr "%rax" e;
      p out "        movq    %%rax, %s\n" (var_location s)

  | CALL (s, lel) ->
      if List.length lel > 6 && List.length lel mod 2 = 1 then
        p out "        subq    $8, %%rsp\n";
      push_args (List.length lel - 1) (List.rev lel);
      if List.mem s !functions then p out "        call    %s\n" s
      else p out "        call    %s@PLT\n"s

  | OP1 (mop, e) ->
      compile_mop mop e;

  | _ -> todo "a completer"




  and compile_decl globl = function

  | CDECL (_, s) ->
      if globl then p out "        .comm   %s,8,8\n" s
      else (incr var_count; Hashtbl.add !var s !var_count)

  | CFUN (_, s, vdl, lc) ->
      functions := s :: !functions;
      var_count := 0;
      var := Hashtbl.create 8;
      p out "        .globl  %s\n" s;
      p out "        .type   %s, @function\n%s:\n" s s;
      p out "        pushq   %%rbp\n        movq    %%rsp, %%rbp\n";
      compile_decl_list false vdl;
      compile_code lc




  and compile_code (_,c) = match c with

  | CBLOCK (vdl, lcl) ->
      let n = List.length vdl in
      if n > 0 then
        let offset = 8*(n + (n mod 2)) in
        p out "        subq    $%d, %%rsp\n" offset;
      if n mod 2 = 1 then incr var_count;
      compile_decl_list false vdl;
      List.iter compile_code lcl

  | CRETURN leo ->
      begin
      match leo with
      | None -> ()
      | Some e -> compile_expr "%rax" e
      end;
      p out "        leave\n        ret\n"

  | CEXPR e -> compile_expr "%rax" e


  | _ -> todo ("not cblock")




  in
  p out "        .text\n        .section        .rodata\n";
  first_decl_list decl_list;
  p out "        .text\n";
  compile_decl_list true decl_list


