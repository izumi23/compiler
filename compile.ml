open Cparse
open Genlab

(*Pour bien coder:
se ramener à un seul  buffer ctrl x 1
ouvrir un deuxième buffer ctrl x 3
aller dessus ctrl x o
ouvrir le fichier cparse.mli ctrl x ctrl f
revenir sur ce buffer ctrl x o
remplir ce giga patern matching*)




(*Question: quelles fonctions retournent sur 64 bits?*)





(*___________________________Le type environnement_________________________*)



type environment = {
  mutable functions: string list;

  mutable stack_size: int;                      (*espace alloué sur la pile*)
  mutable local_var: (string*int) option list;  (*emplacement de chaque variable*)

  mutable string_count: int;
  strings: (string, int) Hashtbl.t;             (*label de chaque string*)

  mutable if_count: int;
  mutable while_count: int
}


let new_environment () = {
  functions = [];
  stack_size = 0; local_var = [];
  string_count = 0; strings = Hashtbl.create 8;
  if_count = 0; while_count = 0
}


let new_local_var env s =
  env.stack_size <- env.stack_size + 1;
  env.local_var <- Some (s, env.stack_size) :: env.local_var

let var_location env s =
  let rec find_var s = function
  | [] -> Printf.sprintf "%s(%%rip)" s
  | Some (s1, i) :: _ when s1 = s -> Printf.sprintf "%d(%%rbp)" (-8*i)
  | _ :: l -> find_var s l
  in
  find_var s env.local_var

let push env =
  env.stack_size <- env.stack_size + 1

let pop env =
  env.stack_size <- env.stack_size - 1

let stack_size_parity env =
  env.stack_size mod 2


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
  env.stack_size <- 0;
  env.local_var <- []

let exists_function env s = List.mem s env.functions


let new_block env =
  env.local_var <- None :: env.local_var

let exit_block env =
  let rec del_var = function
  | None :: l -> l
  | Some _ :: l -> del_var l
  | _ -> failwith "exit_block"
  in
  env.local_var <- del_var env.local_var





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



  and compile_mop mop e =

    compile_expr e;

    let loc = match e with
    | (_, VAR s) -> var_location env s
    | _ -> "(%rdx)"
    in

    match mop with

    | M_MINUS -> p out "        negq    %%rax\n"

    | M_NOT -> p out "        notq    %%rax\n"

    | M_POST_INC -> p out "        addq    $1, %s\n" loc

    | M_POST_DEC -> p out "        subq    $1, %s\n" loc

    | M_PRE_INC ->
        p out "        addq    $1, %s\n" loc;
        p out "        addq    $1, %%rax\n"

    | M_PRE_DEC ->
        p out "        subq    $1, %s\n" loc;
        p out "        subq    $1, %%rax\n"




  and compile_bop bop e1 e2 = match bop with

  | S_ADD

  | S_INDEX ->
      begin
      match e1 with (_, VAR s) ->
      compile_expr e2;
      p out "        movq    %%rax, %%rdx\n";
      p out "        imulq   $8, %%rdx\n";
      p out "        addq    %s, %%rdx\n" (var_location env s);
      p out "        movq    (%%rdx), %%rax\n"
      | _ -> failwith "dans a[i], a doit être une variable"
      end

  | _ -> todo "compile_bop"




  and compile_call s lel =

    let n = List.length lel in
    let args_for_reg = min 6 n and args_to_push = max 0 (n-6) in

    (*padding pour aligner %rsp sur 16 octets*)
    let pad = (stack_size_parity env + args_to_push) mod 2 in
    if pad = 1 then (p out "        subq    $8, %%rsp\n"; push env);

    let eval_and_push e =
      compile_expr e; p out "        pushq   %%rax\n"; push env
    in
    List.iter eval_and_push (List.rev lel);

    for i = 0 to args_for_reg - 1 do
      p out "        popq    %s\n" arg_registers.(i); pop env
    done;

    if exists_function env s then p out "        call    %s\n" s
    else p out "        call    %s@PLT\n" s;

    let size_diff = pad + args_to_push in
    if size_diff > 0 then p out "        addq    $%d, %%rsp\n" (8*(size_diff))




  and compile_expr (_, e) = match e with

  | VAR s ->
      p out "        movq    %s, %%rax\n" (var_location env s)

  | CST n ->
      p out "        movq    $%d, %%rax\n" n

  | STRING s ->
      p out "        leaq    %s, %%rax\n" (string_location env s)

  | SET_VAR (s, e) ->
      compile_expr e;
      p out "        movq    %%rax, %s\n" (var_location env s);

  | SET_ARRAY (s, i, e) ->
      compile_expr e;
      p out "        pushq   %%rax\n"; push env;
      compile_expr i;
      p out "        movq    %%rax, %%rdx\n";
      p out "        imulq   $8, %%rdx\n";
      p out "        addq    %s, %%rdx\n" (var_location env s);
      p out "        popq    %%rax\n"; pop env;
      p out "        movq    %%rax, (%%rdx)\n"

  | CALL (s, lel) ->
      compile_call s lel

  | OP1 (mop, e) ->
      compile_mop mop e;

  | OP2 (bop, e1, e2) ->
      compile_bop bop e1 e2;

  | _ -> todo "a completer"




  (*Lit l'argument numéro i d'une fonction*)

  and pull_args i s =
    if i < 6 then
      let r = arg_registers.(i) in
      p out "        movq    %s, %d(%%rbp)\n" r (-8*(i+1))
    else begin
      p out "        movq    %d(%%rbp), %%rax\n" (8*(i-4));
      p out "        movq    %%rax, %d(%%rbp)\n" (-8*(i+1))
    end



  and compile_decl globl = function

  | CDECL (_, s) ->
      if globl then p out "        .comm   %s,8,8\n" s
      else new_local_var env s

  | CFUN (_, s, vdl, lc) ->
      new_function env s;
      p out "        .globl  %s\n" s;
      p out "        .type   %s, @function\n%s:\n" s s;
      p out "        pushq   %%rbp\n        movq    %%rsp, %%rbp\n";

      let n = List.length vdl in
      if n > 0 then p out "        subq    $%d, %%rsp\n" (8*n);
      compile_decl_list false vdl;
      List.iteri pull_args vdl;
      compile_code lc;




  and compile_code (_,c) = match c with

  | CBLOCK (vdl, lcl) ->
      new_block env;
      let n = List.length vdl in
      if n > 0 then p out "        subq    $%d, %%rsp\n" (8*n);
      compile_decl_list false vdl;
      List.iter compile_code lcl;
      exit_block env


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


