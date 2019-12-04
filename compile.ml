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
  let global_var = ref true in
  let var_location = ref "" in



  let rec compile_decl_list = function
  | [] -> ()
  | h::t -> compile_decl h; compile_decl_list t



  and compile_mop = function

  | M_MINUS -> p out "        negq    %%rax\n"
  | M_NOT -> p out "        notq    %%rax\n"
  | M_POST_INC -> p out "        addq    $1, %s\n" !var_location
  | M_POST_DEC -> p out "        subq    $1, %s\n" !var_location

  | M_PRE_INC ->
      p out "        addq    $1, %s\n" !var_location;
      p out "        addq    $1, %%rax\n"

  | M_PRE_DEC ->
      p out "        subq    $1, %s\n" !var_location;
      p out "        subq    $1, %%rax\n"



  and compile_expr = function

  | VAR s ->
      if Hashtbl.mem !var s then begin
        let i = Hashtbl.find !var s in
        var_location := Printf.sprintf "%d(%%rbp)" (-8*i);
        p out "        movq    %s, %%rax\n" !var_location
      end
      else todo "Variable globale"

  | CST n ->
      p out "        movq    $%d, %%rax\n" n;

  | SET_VAR (s, (_, e)) ->
      compile_expr e;
      if Hashtbl.mem !var s then begin
        let i = Hashtbl.find !var s in
        var_location := Printf.sprintf "%d(%%rbp)" (-8*i);
        p out "        movq    %%rax, %s\n" !var_location
      end
      else todo "Variable globale"

  | OP1 (mop, (_, e)) ->
      compile_expr e;
      compile_mop mop;

  | _ -> todo "a completer"




  and compile_decl = function

  | CDECL (_, s) ->
      incr var_count;
      Hashtbl.add !var s !var_count;
      if !global_var then todo "Cas d'une variable globale"

  | CFUN (_, s, vdl, lc) ->
      global_var := false;
      var_count := 0;
      var := Hashtbl.create 8;
      p out "        .globl  %s\n        .type   %s, @function\n%s:\n" s s s;
      p out "        pushq   %%rbp\n        movq    %%rsp, %%rbp\n";
      List.iter compile_decl vdl;
      compile_code lc




  and compile_code (_,c) = match c with

  | CBLOCK (vdl, lcl) ->
      (*a completer*)
      List.iter compile_decl vdl;
      List.iter compile_code lcl

  | CRETURN leo ->
      begin
      match leo with
      | None -> ()
      | Some (_, e) -> compile_expr e
      end;
      p out "        leave\n        ret\n"

  | CEXPR (_, e) -> compile_expr e


  | _ -> todo ("not cblock")


  in
  p out "        .text\n";
  compile_decl_list decl_list


