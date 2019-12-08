open Cparse
open Genlab

(*Pour bien coder:
se ramener à un seul  buffer ctrl x 1
ouvrir un deuxième buffer ctrl x 3
aller dessus ctrl x o
ouvrir le fichier cparse.mli ctrl x ctrl f
revenir sur ce buffer ctrl x o
remplir ce giga patern matching*)


(*Questions:
- Quand utilise-t-on a[i]? (et notamment SET_ARRAY x[e1]=e2). En effet le type
  int* n'existe pas
- Une fonction peut-elle retourner un void? Sinon, à quoi sert return; ?*)









(*___________________________Le type environnement_________________________*)



type environment = {
  mutable functions: string list;

  mutable local_var_count: int;           (*espace alloué sur la pile*)
  local_var: (string, int) Hashtbl.t;     (*emplacement de chaque variable*)

  mutable string_count: int;
  strings: (string, int) Hashtbl.t;       (*label de chaque string*)

  mutable if_count: int;
  mutable while_count: int
}


let new_environment () = {
  functions = [];
  local_var_count = 0; local_var = Hashtbl.create 8;
  string_count = 0; strings = Hashtbl.create 8;
  if_count = 0; while_count = 0
}


let new_local_var env s =
  env.local_var_count <- env.local_var_count + 1;
  Hashtbl.add env.local_var s env.local_var_count

let new_empty_local_var env =
  env.local_var_count <- env.local_var_count + 1

let local_var_count env = env.local_var_count

let var_location env s =
  if Hashtbl.mem env.local_var s then
    let i = Hashtbl.find env.local_var s in
    Printf.sprintf "%d(%%rbp)" (-8*i)
  else Printf.sprintf "%s(%%rip)" s


let new_string env s =
  Hashtbl.add env.strings s env.string_count;
  env.string_count <- env.string_count + 1

let string_count env = env.string_count

let exists_string env s =
  Hashtbl.mem env.strings s

let string_location env s =
  Printf.sprintf ".LC%d(%%rip)" (Hashtbl.find env.strings s)


let new_function env s =
  env.functions <- s :: env.functions;
  env.local_var_count <- 0;
  Hashtbl.reset env.local_var

let exists_function env s = List.mem s env.functions






(*___________________________La fonction principale___________________________*)



let compile out decl_list =


  let todo s = Printf.fprintf out "#TODO %s\n" s in
  let p = Printf.fprintf in
  let arg_registers = [|"%rdi"; "%rsi"; "%rdx"; "%rcx"; "%r8"; "%r9"|] in
  let env = new_environment () in




(*Fonctions pour une première lecture, pour stocker les chaînes de caractères*)

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

  | STRING s when not (exists_string env s) ->
      p out "        .align 8\n";
      p out ".LC%d:\n        .string %S\n" (string_count env) s;
      new_string env s

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
      p out "        addq    $1, %s\n" (var_location env s)

  | M_POST_DEC, VAR s ->
      p out "        subq    $1, %s\n" (var_location env s)

  | M_PRE_INC, VAR s ->
      p out "        addq    $1, %s\n" (var_location env s);
      p out "        addq    $1, %%rax\n"

  | M_PRE_DEC, VAR s ->
      p out "        subq    $1, %s\n" (var_location env s);
      p out "        subq    $1, %%rax\n"

  | _ -> todo "a[i]++"




(*Appelé lors d'un CALL, compile les arguments donnés à la fonction appelée.
  Pour cela on conserve dans i le rang du prochain argument à traiter.*)

  and push_args i = function
  | [] -> ()
  | e::t ->
      if i < 6 then compile_expr ~dest:arg_registers.(i) e
      else (compile_expr e; p out "        pushq   %%rax");
      push_args (i-1) t




  and compile_expr ?(dest="%rax") (_, e) = match e with

  | VAR s ->
      p out "        movq    %s, %s\n" (var_location env s) dest

  | CST n ->
      p out "        movq    $%d, %s\n" n dest

  | STRING s ->
      p out "        leaq    %s, %s\n" (string_location env s) dest

  | SET_VAR (s, e) ->
      compile_expr ~dest:dest e;
      p out "        movq    %s, %s\n" dest (var_location env s);

  | CALL (s, lel) ->
      let n = List.length lel in
      if n > 6 && n mod 2 = 1 then p out "        subq    $8, %%rsp\n";
      push_args (n-1) (List.rev lel);
      if exists_function env s then p out "        call    %s\n" s
      else p out "        call    %s@PLT\n" s;
      if n > 6 then p out "        addq    $%d, %%rsp\n" (8*(n-6 + (n mod 2)));
      if dest != "%rax" then p out "        movq    %%rax, %s\n" dest

  | OP1 (mop, e) ->
      compile_mop mop e;

  | _ -> todo "a completer"




  and compile_decl globl = function

  | CDECL (_, s) ->
      if globl then p out "        .comm   %s,8,8\n" s
      else new_local_var env s

  | CFUN (_, s, vdl, lc) ->
      new_function env s;
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
      if n mod 2 = 1 then new_empty_local_var env;
      compile_decl_list false vdl;
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


