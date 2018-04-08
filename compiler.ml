open Str;;
open List;;
open Map;;

(*Type definitions*)

(* for tokens (lexing) *)
type token =  OpenBrace | CloseBrace | OpenParen | CloseParen
			| Semicolon | BitwiseComplement | LogicalNegation
			| Plus | Minus | Times | ForwardSlash
			| AndToken | OrToken | EqualToken | NotEqualToken
			| LTToken | LTorEqualToken | GTToken | GTorEqualToken
			| AssignToken
			| Keyword of string
			| Integer of int
			| Identifier of string
			| None

(* for AST (parsing) *)
type unary_operator = Negation | BitwiseComplement | LogicalNegation
type binary_operator =  OR | AND | Equal | NotEqual
						| GreaterThan | LessThan | GTorEqual | LTorEqual
						| Add | Subtract | Multiply | Divide


type exp = Call of string*(exp list) | Assign of string*exp | Var of string | BinOp of binary_operator*exp*exp | UnOp of unary_operator*exp | Const of int
type statement = Return of exp | Declare of string*(exp option) | Exp of exp | If of exp*(statement list)*(statement list)
type fun_decl = Function of string*(statement list)
type program = Program of fun_decl list 

(* Generating assembly *)
module V = Map.Make(String);;
type context = 
	{ var_map : int V.t;
	  offset  : int
	}

exception Parse_exn of string
exception Generate_exn of string


(*Reading from file*)

let read_lines (file_name: string): string list =
	let in_channel = open_in file_name in
	let rec read_recursive lines =
		try
    		let line = input_line in_channel in
    		read_recursive (line :: lines)
		with End_of_file ->
  			lines in
  	let lines = read_recursive [] in
  	let _ = close_in in_channel in
  	List.rev (lines)
;;

let rec single_string (lst: string list) (delim: string) : string =
	match lst with
	| [] -> ""
	| hd::tl -> hd^delim^(single_string tl delim)
;;


(*Lexing*)

let to_list (s:string) : char list =
  let rec loop acc i =
    if i = -1 then acc
    else
      loop (s.[i] :: acc) (i-1)
  in loop [] (String.length s - 1)
;;

let translate_to_token (cl: char list) : token = 
	let s = String.concat "" (List.map (String.make 1) cl) in 
	let keywords = ["int"; "return"; "if"; "else"] in
	if s = ""  then None else
	if s = "{" then OpenBrace else
	if s = "}" then CloseBrace else
	if s = "(" then OpenParen else
	if s = ")" then CloseParen else
	if s = ";" then Semicolon else
	if s = "-" then Minus else
	if s = "~" then BitwiseComplement else
	if s = "!" then LogicalNegation else
	if s = "+" then Plus else
	if s = "*" then Times else
	if s = "/" then ForwardSlash else
	if s = "<" then LTToken else
	if s = ">" then GTToken else
	if s = "=" then AssignToken else
	if s = "<=" then LTorEqualToken else
	if s = ">=" then GTorEqualToken else 
	if s = "&&" then AndToken else
	if s = "||" then OrToken else
	if s = "==" then EqualToken else
	if s = "!=" then NotEqualToken else
	if List.mem s keywords then Keyword(s) else
	if Str.string_match (Str.regexp "[0-9]+") s 0 then Integer(int_of_string s) else
	if Str.string_match (Str.regexp "[a-zA-z]") s 0 then Identifier(s) else
	None
;;

let symbols_to_tokens (symbols: char list) : token list = 
	let rec helper (sym: char list) (acc: token list) : token list = 
		match sym with
		| [] -> acc
		| a::[] -> (translate_to_token [a])::acc
		| a::b::tl -> 
			let double = translate_to_token [a; b] in
			if double != None then
				helper tl (double::acc)
			else
				let single = translate_to_token [a] in
				helper (b::tl) (single::acc)
	in
	helper symbols []
;;

let rec lex_helper (input: char list) (word_acc: char list) (symbol_acc: char list) (token_acc: token list) : token list = 
	let symbols = ['{'; '}'; '('; ')'; ';'; '!'; '~'; '+'; '-'; '*'; '/'; '<'; '>'; '&'; '|'; '=' ] in
	let whitespaces = [' '; '\n'; '\r'; '\x0c'; '\t'] in
	match input with
	| [] -> token_acc
	| hd::tl ->
		if List.mem hd whitespaces then
			let w = translate_to_token word_acc in
			let s = symbols_to_tokens symbol_acc in
			(if w != None then lex_helper tl [] [] (w::token_acc) else 
			 if s != []   then lex_helper tl [] [] (s@token_acc) else
							   lex_helper tl [] [] token_acc) 
		else if List.mem hd symbols then
			let w = translate_to_token word_acc in
			(if w != None then lex_helper tl [] (symbol_acc@[hd]) (w::token_acc) else 	
							   lex_helper tl [] (symbol_acc@[hd]) token_acc)
		else
			let s = symbols_to_tokens symbol_acc in
			(if s != [] then lex_helper tl (word_acc@[hd]) [] (s@token_acc) else 	
							   lex_helper tl (word_acc@[hd]) [] token_acc)
;;

let lex (input: char list) : token list = 
	let tokens = lex_helper input [] [] [] in
	List.rev tokens
;;

let print_token (t: token) : string = 
	match t with
	| OpenBrace -> "{"
	| CloseBrace -> "}"
	| OpenParen -> "("
	| CloseParen -> ")"
	| Semicolon -> ";"
	| Minus -> "-"
	| BitwiseComplement -> "~"
	| LogicalNegation -> "!"
	| Plus -> "+"
	| Times -> "*"
	| ForwardSlash -> "/"
	| AndToken -> "AND"
	| OrToken -> "OR"
	| EqualToken -> "=="
	| NotEqualToken -> "!="
	| LTToken -> "<"
	| LTorEqualToken -> "<="
	| GTToken -> ">"
	| GTorEqualToken -> ">="
	| AssignToken -> "="
	| Keyword(s) -> "Keyword "^s
	| Identifier(s) -> "Identifier "^s
	| Integer(x) -> "Integer "^(string_of_int x)
	| None -> "None"
;;



let print_tokens (tokens: token list) : unit =
	let rec print_token_helper (tokens: token list) : string = 
		match tokens with
		| [] -> ""
		| hd::tl -> (print_token hd)^", "^(print_token_helper tl)
	in
	print_string ((print_token_helper tokens)^"\n")
;;

(* Parsing *)

let token_to_binary_operator (t: token) : binary_operator =
	match t with
	| Plus -> Add 
	| Minus -> Subtract
	| Times -> Multiply
	| ForwardSlash -> Divide
	| GTToken -> GreaterThan
	| LTToken ->  LessThan
	| GTorEqualToken -> GTorEqual
	| LTorEqualToken -> LTorEqual
	| EqualToken -> Equal
	| NotEqualToken -> NotEqual
	| AndToken -> AND
	| OrToken -> OR
	| AssignToken -> raise (Parse_exn "This is not a binary operation")
	| OpenBrace | CloseBrace | OpenParen | CloseParen -> raise (Parse_exn "This is not a binary operation")
	| Semicolon | BitwiseComplement | LogicalNegation -> raise (Parse_exn "This is not a binary operation")
	| Keyword(_) | Identifier(_) | Integer(_) | None -> raise (Parse_exn "This is not a binary operation")
;;

let parse_exp_creator   (parse_lower_level: token list -> exp*(token list)) 
						(matching_operators: token list)  =
	let rec f (tokens: token list) : exp*token list = 
		let (lower_exp, tl) = parse_lower_level tokens in
		let rec loop (lst: token list) (acc: exp) : exp*(token list) =
			match lst with
			| hd::tl -> 
				(if List.mem hd matching_operators 
				then let (new_lower_exp, new_tl) = parse_lower_level tl in
					loop new_tl (BinOp((token_to_binary_operator hd), acc, new_lower_exp))
				else (acc, hd::tl))
			| [] -> (acc, []) 
		in
		loop tl lower_exp
	in f
;;

let rec parse_exp_list (tokens: token list) : (exp list)*token list = 
	let rec helper (tokens: token list) (acc: exp list) : (exp list)*(token list) =
		match tokens with
		| CloseParen::tl ->	(acc, tl)
		| _ -> (match parse_exp tokens with
				| (_, []) -> raise (Parse_exn "Missing ')' in function call")
				| (e, CloseParen::tl) -> (e::acc, tl)
				| (e, tok) -> helper tok (e::acc))
	in
	let (params, tl) = helper tokens [] in
	(List.rev params, tl)

and parse_exp (tokens: token list) : exp*(token list) =
	match tokens with
	| Identifier(v)::AssignToken::tl -> 
		let (e, new_tl) = parse_exp tl in
		(Assign(v, e), new_tl) 
	| _ -> 
		let (e, new_tl) = parse_logical_or_exp tokens in
		(e, new_tl)

and parse_logical_or_exp (tokens: token list) : exp*(token list) =
	let f = parse_exp_creator parse_logical_and_exp [OrToken] in
	f tokens

and parse_logical_and_exp (tokens: token list) : exp*(token list) =
	let f = parse_exp_creator parse_equality_exp [AndToken] in
	f tokens

and parse_equality_exp (tokens: token list) : exp*(token list) =
	let f = parse_exp_creator parse_relational_exp [EqualToken; NotEqualToken] in
	f tokens

and parse_relational_exp (tokens: token list) : exp*(token list) =
	let f = parse_exp_creator parse_add_exp [GTToken; LTToken; GTorEqualToken; LTorEqualToken] in
	f tokens

and parse_add_exp (tokens: token list) : exp*(token list) =
	let f = parse_exp_creator parse_mul_exp [Plus; Minus] in
	f tokens

and parse_mul_exp (tokens: token list) : exp*(token list) =
	let f = parse_exp_creator parse_factor [Times; ForwardSlash] in
	f tokens

and parse_factor (tokens: token list) : exp*(token list) =
	print_tokens tokens;
	match tokens with
	| OpenParen::tl -> (match parse_logical_or_exp tl with
						| (e, CloseParen::new_tl) -> (e, new_tl)
						| _ -> raise (Parse_exn "Missing expression or ')' in factor"))
	| Minus::tl -> let (inner_factor, new_tl) = parse_factor tl in
						(UnOp(Negation, inner_factor), new_tl)
	| BitwiseComplement::tl -> let (inner_factor, new_tl) = parse_factor tl in
								(UnOp(BitwiseComplement, inner_factor), new_tl)
	| LogicalNegation::tl -> let (inner_factor, new_tl) = parse_factor tl in
								(UnOp(LogicalNegation, inner_factor), new_tl)
	| Integer(x)::tl -> (Const(x), tl)
	| Identifier(v)::OpenParen::tl -> let (params, new_tl) = parse_exp_list tl in
										(Call(v, params), new_tl)
	| Identifier(v)::tl -> (Var(v), tl)
	| _ -> raise (Parse_exn "parse_factor expects an Integer, UnOp, BinOp, or Var")
;;

let rec parse_if (tokens: token list) : statement*(token list) = 
	match parse_exp tokens with
		| (e, CloseParen::OpenBrace::tl) -> 
			(match parse_statement_list tl with
			| (true_branch, Keyword(a)::Keyword(b)::OpenParen::new_tl) when (a="else" && b="if") -> 
					let (st, final_tl) = parse_if new_tl in
					(If(e, true_branch, [st]), final_tl)
			| (true_branch, Keyword(a)::OpenBrace::new_tl) when a="else" ->
					let (false_branch, final_tl) = parse_statement_list new_tl in
					(If(e, true_branch, false_branch), final_tl)
			| (true_branch, new_tl) -> (If(e, true_branch, []), new_tl))
		| _ -> raise (Parse_exn "Missing a '(' after the condition.")


and parse_statement (tokens: token list) : statement*(token list) = 
	match tokens with
	| Keyword(s)::tl when s="return" ->
		(match parse_exp tl with 
		| (e, Semicolon::tl) -> (Return(e), tl)
		| _ -> raise (Parse_exn "Missing semicolon"))
	| Keyword(s)::Identifier(v)::Semicolon::tl when s="int" ->	
		(Declare(v, None), tl)
	| Keyword(s)::Identifier(v)::AssignToken::tl when s="int" ->
		(match parse_exp tl with 
		| (e, Semicolon::tl) -> (Declare(v, Some(e)), tl)
		| _ -> raise (Parse_exn "Missing semicolon"))
	| Keyword(s)::OpenParen::tl when s="if" -> parse_if tl
	| _ -> 
		(match parse_exp tokens with 
		| (e, Semicolon::tl) -> (Exp(e), tl)
		| _ -> raise (Parse_exn "Missing semicolon"))

and parse_statement_list (tokens: token list) : (statement list)*(token list) =
	let rec helper (tokens: token list) (acc: statement list) : (statement list)*(token list) =
		match tokens with
		| CloseBrace::[] ->	(Return(Const(0))::acc, [])
		| _ -> (match parse_statement tokens with
				| (_, []) -> raise (Parse_exn "Missing '}' in function")
				| (st, CloseBrace::tl) -> (st::acc, tl)
				| (st, tok) -> helper tok (st::acc))
	in
	let (sts, tl) = helper tokens [] in
	(List.rev sts, tl)
;;


let parse_function (tokens: token list) : fun_decl*(token list) =
	match tokens with
	| Keyword(k)::Identifier(v)::OpenParen::CloseParen::OpenBrace::tl when k="int" ->
		let (st_list, new_tl) = parse_statement_list tl in 
		(Function(v, st_list), new_tl)
	| _ -> raise (Parse_exn "Function declaration syntax is incorrect")	
;;

let parse_program (tokens: token list) : program*(token list) = 
	let rec helper (t: token list) (acc: fun_decl list) : (fun_decl list)*(token list) =
		match t with 
		| [] -> (acc, [])
		| _ -> let (f, tl) = parse_function t in
				helper tl (f::acc) 
	in
	let (functions, tl) = helper tokens [] in
	(Program(List.rev functions), tl)
;;

let parse (tokens: token list) : program =
	fst (parse_program tokens) 
;;

(* Generate *)
let rec generate_push_pop (e1: exp) (e2: exp) (ctx: context): string =
	(generate_exp e1 ctx)
	^"pushq %rax\n"
	^(generate_exp e2 ctx)
	^"popq %rcx\n"

and generate_push_params (params: exp list) (ctx: context) : string = 
	let rec helper (p: exp list) (acc: string) : string =
		match p with
		| [] -> acc
		| hd::tl -> helper tl ((generate_exp hd ctx)^"pushq %rax"^acc)
	in
	helper params ""

and generate_exp (e: exp) (ctx: context): string = 
	match e with
	| BinOp(op, e1, e2) -> generate_binop op e1 e2 ctx
	| Const(x) -> Printf.sprintf "movq $%i, %%rax\n" x
	| UnOp(op, inner_factor) -> 
		let inner_fac_assembly = generate_exp inner_factor ctx in
		let unary_op_assembly =
			(match op with
			| Negation -> "neg %rax\n"
			| LogicalNegation -> "cmpq $0, %rax\n movq $0, %rax\n sete %al\n"
			| BitwiseComplement -> "not %rax\n") 
		in
		inner_fac_assembly^unary_op_assembly
	| Assign(v, e1) -> 
		let offset = V.find v ctx.var_map in
		let assign = Printf.sprintf "movq %%rax, %i(%%rbp)\n" offset in
		(generate_exp e1 ctx)
		^assign
	| Var(v) -> 
		let offset = V.find v ctx.var_map in
		Printf.sprintf "movq %i(%%rbp), %%rax\n" offset
	| Call(v, params) ->
		 "pushq %rax\n"
		^"pushq %rdx\n"
		^"pushq %r8\n"
		^"pushq %r9\n"
		^"pushq %r10\n"
		^"pushq %r11\n"
		^(generate_push_params params ctx)
		^(Printf.sprintf "call %s\n" v)
		^"addq $48, %rsp"
		^"popq %rax\n"
		^"popq %rdx\n"
		^"popq %r8\n"
		^"popq %r9\n"
		^"popq %r10\n"
		^"popq %r11\n"


and generate_binop (op: binary_operator) (e1: exp) (e2: exp) (ctx: context) : string = 
	match op with
	| Multiply -> (generate_push_pop e1 e2 ctx)
				  ^"imulq %rcx\n"
	| Divide -> (generate_push_pop e2 e1 ctx)
				^"xor %rdx, %rdx\n"
				^"idivq %rcx\n"
	| Add -> (generate_push_pop e1 e2 ctx)
			 ^"addq %rcx, %rax\n"
	| Subtract -> (generate_push_pop e2 e1 ctx)
					^"subq %rcx, %rax\n"
	| GreaterThan -> (generate_push_pop e2 e1 ctx)
					 ^"cmpq %rcx, %rax\n"
					 ^"movq $0, %rax\n"
					 ^"setg %al\n"
	| LessThan -> (generate_push_pop e2 e1 ctx)
				  ^"cmpq %rcx, %rax\n"
				  ^"movq $0, %rax\n"
				  ^"setl %al\n"
	| GTorEqual -> (generate_push_pop e2 e1 ctx)
				   ^"cmpq %rcx, %rax\n"
				   ^"movq $0, %rax\n"
				   ^"setge %al\n"
	| LTorEqual -> (generate_push_pop e2 e1 ctx)
				   ^"cmpq %rcx, %rax\n"
				   ^"movq $0, %rax\n"
				   ^"setle %al\n"
	| Equal -> (generate_push_pop e1 e2 ctx)
				^"cmpq %rcx, %rax\n"
				^"movq $0, %rax\n"
				^"sete %al\n"
	| NotEqual -> (generate_push_pop e1 e2 ctx)
				^"cmpq %rcx, %rax\n"
				^"movq $0, %rax\n"
				^"setne %al\n"
	| AND -> (generate_push_pop e1 e2 ctx)
			 ^"cmpq $0, %rcx\n"
			 ^"setne %cl\n"
			 ^"cmpq $0, %rax\n"
			 ^"movq $0, %rax\n"
			 ^"setne %al\n"
			 ^"andb %cl, %al\n"
	| OR -> (generate_push_pop e1 e2 ctx)
			^"orq %rcx, %rax\n"
			^"movq $0, %rax\n"
			^"setne %al\n"
;;

let rec generate_statement (st: statement) (ctx: context) (j: int) : string*context*int =
	match st with
	| Return(e) -> ((generate_exp e ctx)
				   ^"movq %rbp, %rsp\n"
				   ^"popq %rbp\n"
				   ^"ret\n",
					ctx, j)
	| Declare (v, e) -> 
		if V.mem v ctx.var_map then 
			let s = Printf.sprintf "variable %s declared twice" v in
			raise (Generate_exn s)
		else
			let evaluate = match e with
					| None -> "movq $0, %rax\n"
					| Some e1 -> generate_exp e1 ctx
			in 
			let assign = Printf.sprintf "movq %%rax, %i(%%rbp)\n" ctx.offset in
			let new_ctx = {	var_map = V.add v ctx. offset ctx.var_map; 
							offset = ctx.offset-8
						}
			in
			(evaluate^assign, new_ctx, j)
	| Exp(e) -> (generate_exp e ctx, ctx, j)
	| If(cond, true_st, false_st) ->
		let (true_code, true_ctx, new_j) = generate_statement_list true_st ctx j in
		let (false_code, false_ctx, final_j) = generate_statement_list false_st true_ctx new_j in
		((generate_exp cond ctx)
		^"cmpq $0, %rax\n"
		^(Printf.sprintf "je .L%i\n" final_j)
		^true_code
		^(Printf.sprintf "jmp .L%i\n" (final_j+1))
		^(Printf.sprintf ".L%i:\n" final_j)
		^false_code
		^(Printf.sprintf ".L%i:\n" (final_j+1)), ctx, final_j+2)

and generate_statement_list (sts: statement list) (ctx: context) (j: int) : string*context*int = 
	let rec helper (sts: statement list) (acc: string) (ctx: context) (j: int): string*context*int =
		match sts with
		| [] -> (acc, ctx, j)
		| hd::tl -> 
			let (s, new_ctx, new_j) = generate_statement hd ctx j in
			helper tl (acc^s) new_ctx new_j
	in
	helper sts "" ctx j
;;

let generate_function (f: fun_decl) (j: int) : string*int = 
	match f with
	| Function(name, sts) -> 
		let (statements, ctx, new_j) = generate_statement_list sts {var_map = V.empty; offset =  -8} j in
		((Printf.sprintf ".globl %s\n%s:\n" name name) 
		^"pushq %rbp\n"
		^"movq %rsp, %rbp\n"
		^statements, new_j)
;;

let generate_function_list (fs: fun_decl list) : string = 
	let rec helper (fs: fun_decl list) (acc: string) (j: int) : string*int = 
		match fs with
		| [] -> (acc, j)
		| hd::tl -> let (f, new_j) = generate_function hd j in
					helper tl (acc^f) new_j
	in
	fst (helper fs "" 0)

let generate (ast: program) : string = 
	match ast with
	| Program(fs) -> generate_function_list fs
;;


let all_lines = single_string (read_lines Sys.argv.(1)) " " in
let assembly_filename = "/mnt/c/Users/Varqa/Documents/Compiler/write_a_c_compiler/"^(List.hd (Str.split (regexp "\\.") Sys.argv.(1)))^".s" in
let out = open_out assembly_filename in
print_tokens (lex (to_list all_lines));
print_string "\n";
Printf.fprintf out "%s" (generate (parse (lex (to_list all_lines))));;


