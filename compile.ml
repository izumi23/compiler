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
  let var_count = ref 0 in
  let var = ref (Hashtbl.create 8) in
  let functions = ref [] in
  let arg_registers = [|"%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9"|] in



(*Renvoie l'adresse de la variable s, selon qu'elle soit locale ou globale*)

  let var_location s =
    if Hashtbl.mem !var s then
      let i = Hashtbl.find !var s in
      Printf.sprintf "%d(%%rbp)" (-8*i)
    else Printf.sprintf "%s(%%rip)" s
  in




(*Fonctions pour une première lecture, pour stocker les chaines de caracteres*)

  let strings = Hashtbl.create 8 in
  let string_count = ref 0 in

  let rec first_decl_list lcl = List.iter first_decl lcl

  and first_decl = function
  | CDECL _ -> ()
  | CFUN (_, _, _, lc) -> first_code lc

  and first_code (_, c) = match c with
  | CBLOCK (vdl, lcl) -> List.iter first_code lcl
  | CEXPR e -> first_expr e
  | CIF (e, c1, c2) ->
      first_expr e; first_code c1; first_code c2
  | CWHILE (e, c) -> first_expr e; first_code c
  | CRETURN None -> ()
  | CRETURN (Some e) -> first_expr e

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
      compile_expr (l, e);
      p out "        negq    %%rax\n"

  | M_NOT, _ ->
      compile_expr (l, e);
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



  and push_arg i = function
  | [] -> ()
  | e::t ->
      compile_expr e;
      if i < 6 then p out "        movq    %%rax, %s\n" arg_registers.(i)
      else p out "        pushq   %%rax\n";
      push_arg (i-1) t



  and compile_expr (_, e) = match e with

  | VAR s ->
      p out "        movq    %s, %%rax\n" (var_location s)

  | CST n ->
      p out "        movq    $%d, %%rax\n" n;

  | STRING s ->
      p out "        leaq    .LC%d(%%rip), %%rax\n" (Hashtbl.find strings s)

  | SET_VAR (s, e) ->
      compile_expr e;
      p out "        movq    %%rax, %s\n" (var_location s)

  | CALL (s, lel) ->
      if List.length lel > 6 && List.length lel mod 2 = 1 then
        p out "        subq    $8, %%rsp\n";
      push_arg (List.length lel - 1) (List.rev lel);
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
      compile_decl_list false vdl;
      if !var_count > 0 then
        p out "        subq    $%d, %%rsp\n" (8*(!var_count+(!var_count mod 2)));
      List.iter compile_code lcl

  | CRETURN leo ->
      begin
      match leo with
      | None -> ()
      | Some e -> compile_expr e
      end;
      p out "        leave\n        ret\n"

  | CEXPR e -> compile_expr e


  | _ -> todo ("not cblock")




  in
  p out "        .text\n        .section        .rodata\n";
  first_decl_list decl_list;
  p out "        .text\n";
  compile_decl_list true decl_list


