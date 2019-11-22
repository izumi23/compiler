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
  let todo s = (*Printf.fprintf out "#TODO %s\n" s\n*) () in
  let p = Printf.fprintf in
  let var_count = ref 0 in
  let var = ref (Hashtbl.create 8) in

  let rec compile_decl_list = function
  | [] -> ()
  | h::t -> compile_decl h; compile_decl_list t



  and compile_expr = function

  | CST n ->
      p out "        movq   $%d, %%rax\n" n;

  | SET_VAR (s, (_, e)) ->
      compile_expr e;
      begin
      match Hashtbl.find_opt !var s with
      | None -> failwith "variable non existante"
      | Some i -> p out "        movq   %%rax, %d(%%rbp)\n" (-4*i)
      end

  | _ -> todo "a completer"



  and compile_decl = function

  | CDECL (_, s) ->
      incr var_count;
      Hashtbl.add !var s !var_count;

  | CFUN (_, s, vdl, lc) ->
      var_count := 0;
      var := Hashtbl.create 8;
      p out "        .globl %s\n        .type  %s, @function\n%s:\n" s s s;
      p out "        pushq  %%rbp\n        movq   %%rsp, %%rbp\n";
      List.iter compile_decl vdl;
      compile_code lc



  and compile_code (_,c) = match c with

  | CBLOCK (vdl, lcl) ->
      todo ("cblock ");
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


