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
  let todo s = (*Printf.fprintf out "#TODO %s\n" s*) () in
  let p = Printf.fprintf in

  let rec compile_decl_list = function
  | [] -> ()
  | h::t -> compile_decl h; compile_decl_list t



  and compile_decl = function

  | CDECL (l,s) -> todo ("cdecl " ^ s)

  | CFUN (l,s, vdl, lc) ->
      begin
      todo ("cfun "^s);
      List.iter compile_decl vdl;
      compile_code lc
      end



  and compile_code (_,c) = match c with

  | CBLOCK (vdl, lcl) -> begin
      todo ("cblock ");
      List.iter compile_decl vdl;
      List.iter compile_code lcl
      end

  | CRETURN leo ->
      begin
      match leo with
      | None -> p out "    ret\n"
      | Some (_, expr) ->
        match expr with
          | CST n -> p out "    movq    $%d, %%rax\n    ret\n" n
          | _ -> todo "a faire"
      end

  | _ -> todo ("not cblock")


  in
  p out "    .section .text\n    .global main\nmain:\n";
  compile_decl_list decl_list


