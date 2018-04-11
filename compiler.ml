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
			| Colon | QuestionMark
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


type exp =    Conditional of exp*exp*exp
			| Assign of string*exp
			| Var of string 
			| BinOp of binary_operator*exp*exp 
			| UnOp of unary_operator*exp 
			| Const of int
type statement = Return of exp | Exp of exp | If of exp*statement*(statement option)
type declaration = Declare of string*(exp option)
type block_item = Statement of statement | Declaration of declaration
type fun_decl = Function of string*(block_item list)
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
	if s = ":" then Colon else
	if s = "?" then QuestionMark else
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
	let symbols = ['{'; '}'; '('; ')'; ';'; '!'; '~'; '+'; '-'; '*'; '/'; '<'; '>'; '&'; '|'; '='; ':'; '?' ] in
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
	| Colon -> ":"
	| QuestionMark -> "?"
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
	| AssignToken | Colon | QuestionMark -> raise (Parse_exn "This is not a binary operation")
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
		let (e, new_tl) = parse_conditional_exp tokens in
		Printf.printf "Bubbled up to parse_exp, the tail is:\n";
		print_tokens new_tl;
		(e, new_tl)

and parse_conditional_exp (tokens: token list) : exp*(token list) = 
	let (condition, first_tl) = parse_logical_or_exp tokens in
	match first_tl with
	| QuestionMark::tl -> 
		(let (true_branch, next_tl) = parse_exp tl in 
		match next_tl with
		| Colon::tl -> 
			let (false_branch, final_tl) = parse_conditional_exp tl in
			(Conditional(condition, true_branch, false_branch), final_tl)
		| _ -> raise (Parse_exn "Missing colon in conditional"))
	| _ -> (condition, first_tl)

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
	Printf.printf "We are in parse_factor\n";
	match tokens with
	| OpenParen::tl -> (match parse_exp tl with
						| (e, CloseParen::new_tl) -> (e, new_tl)
						| _ -> raise (Parse_exn "Missing expression or ')' in factor"))
	| Minus::tl -> let (inner_factor, new_tl) = parse_factor tl in
						(UnOp(Negation, inner_factor), new_tl)
	| BitwiseComplement::tl -> let (inner_factor, new_tl) = parse_factor tl in
								(UnOp(BitwiseComplement, inner_factor), new_tl)
	| LogicalNegation::tl -> let (inner_factor, new_tl) = parse_factor tl in
								(UnOp(LogicalNegation, inner_factor), new_tl)
	| Integer(x)::tl -> (Const(x), tl)
	| Identifier(v)::tl -> (Var(v), tl)
	| _ -> raise (Parse_exn "parse_factor expects an Integer, UnOp, BinOp, or Var")
;;

let rec parse_if (tokens: token list) : statement*(token list) = 
	print_tokens tokens;
	match parse_exp tokens with
		| (e, CloseParen::tl) -> 
			(match parse_statement tl with
			| (true_branch, Keyword(a)::Keyword(b)::OpenParen::new_tl) when (a="else" && b="if") -> 
					let (st, final_tl) = parse_if new_tl in
					(If(e, true_branch, Some(st)), final_tl)
			| (true_branch, Keyword(a)::new_tl) when a="else" ->
					let (false_branch, final_tl) = parse_statement new_tl in
					(If(e, true_branch, Some(false_branch)), final_tl)
			| (true_branch, new_tl) -> (If(e, true_branch, None), new_tl))
		| _ -> 
			print_tokens tokens;
			raise (Parse_exn "Missing a '(' after the condition.")


and parse_statement (tokens: token list): statement*(token list) = 
	match tokens with
	| Keyword(s)::tl when s="return" ->
		(match parse_exp tl with 
		| (e, Semicolon::tl) -> (Return(e), tl)
		| _ -> raise (Parse_exn "Missing semicolon"))
	| Keyword(s)::OpenParen::tl when s="if" -> parse_if tl
	| _ -> 
		(match parse_exp tokens with 
		| (e, Semicolon::tl) -> (Exp(e), tl)
		| _ -> raise (Parse_exn "Missing semicolon"))

and parse_block_item (tokens: token list) : block_item*(token list) = 
	match tokens with
	| Keyword(s)::Identifier(v)::Semicolon::tl when s="int" ->	
		(Declaration(Declare(v, None)), tl)
	| Keyword(s)::Identifier(v)::AssignToken::tl when s="int" ->
		(match parse_exp tl with 
		| (e, Semicolon::tl) -> (Declaration(Declare(v, Some(e))), tl)
		| _ -> raise (Parse_exn "Missing semicolon"))
	| _ -> let (st, new_tokens) = parse_statement tokens in (Statement(st), new_tokens)

and parse_block_item_list (tokens: token list) : (block_item list)*(token list) =
	let rec helper (tokens: token list) (acc: block_item list) : (block_item list)*(token list) =
		match tokens with
		| CloseBrace::[] ->	(Statement(Return(Const(0)))::acc, [])
		| _ -> (match parse_block_item tokens with
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
		let (block_list, new_tl) = parse_block_item_list tl in 
		(Function(v, block_list), new_tl)
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
	(fst (generate_exp e1 ctx 0))
	^"pushq %rax\n"
	^(fst (generate_exp e2 
		{var_map = V.add "" ctx.offset ctx.var_map; 
		 offset = ctx.offset-8} 0))
	^"popq %rcx\n"

and generate_push_params (params: exp list) (ctx: context) : string = 
	let rec helper (p: exp list) (acc: string) : string =
		match p with
		| [] -> acc
		| hd::tl -> helper tl ( (fst (generate_exp hd ctx 0))^"pushq %rax"^acc)
	in
	helper params ""

and generate_exp (e: exp) (ctx: context) (j: int) : string*int = 
	match e with
	| BinOp(op, e1, e2) -> (generate_binop op e1 e2 ctx, j)
	| Const(x) -> (Printf.sprintf "movq $%i, %%rax\n" x, j)
	| UnOp(op, inner_factor) -> 
		let inner_fac_assembly = fst (generate_exp inner_factor ctx 0) in
		let unary_op_assembly =
			(match op with
			| Negation -> "neg %rax\n"
			| LogicalNegation -> "cmpq $0, %rax\n movq $0, %rax\n sete %al\n"
			| BitwiseComplement -> "not %rax\n") 
		in
		(inner_fac_assembly^unary_op_assembly, j)
	| Assign(v, e1) -> 
		let offset = V.find v ctx.var_map in
		let assign = Printf.sprintf "movq %%rax, %i(%%rbp)\n" offset in
		((fst (generate_exp e1 ctx 0))^assign, j)
	| Var(v) -> 
		let offset = V.find v ctx.var_map in
		(Printf.sprintf "movq %i(%%rbp), %%rax\n" offset, j)
	| Conditional(condition, true_exp, false_exp) ->
		let (condition_code, new_j) = generate_exp condition ctx j in
		let (true_code, mid_j) = generate_exp true_exp ctx new_j in
		let (false_code, final_j) = generate_exp false_exp ctx mid_j in
		(condition_code
		^"cmpq $0, %rax\n"
		^(Printf.sprintf "je .L%i\n" final_j)
		^true_code
		^(Printf.sprintf "jmp .L%i\n" (final_j+1))
		^(Printf.sprintf ".L%i:\n" final_j)
		^false_code
		^(Printf.sprintf ".L%i:\n" (final_j+1)), final_j+2)

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
	| Return(e) -> 
		let (ret_code, new_j) = generate_exp e ctx j
		in 
		(ret_code
		^"movq %rbp, %rsp\n"
		^"popq %rbp\n"
		^"ret\n", ctx, j)
	| Exp(e) -> 
		let (code, new_j) = generate_exp e ctx j in
		(code, ctx, new_j)
	| If(cond, true_st, false_st) ->
		let (condition_code, new_j) = generate_exp cond ctx j in
		let (true_code, _, mid_j) = generate_statement true_st ctx new_j in
		let (false_code, _, final_j) = 
			match false_st with
			| Some(st) -> generate_statement st ctx mid_j
			| None -> ("", ctx, mid_j)
		in
		(condition_code
		^"cmpq $0, %rax\n"
		^(Printf.sprintf "je .L%i\n" final_j)
		^true_code
		^(Printf.sprintf "jmp .L%i\n" (final_j+1))
		^(Printf.sprintf ".L%i:\n" final_j)
		^false_code
		^(Printf.sprintf ".L%i:\n" (final_j+1)), ctx, final_j+2)

and generate_declaration (d: declaration) (ctx: context) (j: int) : string*context*int =
	match d with
	| Declare (v, e) -> 
		if V.mem v ctx.var_map then 
			let s = Printf.sprintf "variable %s declared twice" v in
			raise (Generate_exn s)
		else
			let (evaluate, new_j) = match e with
							| None -> ("movq $0, %rax\n", j)
							| Some e1 -> generate_exp e1 ctx j
			in 
			let new_ctx = {	var_map = V.add v ctx.offset ctx.var_map; 
							offset = ctx.offset-8
						}
			in
			(evaluate^"pushq %rax\n", new_ctx, new_j)

and generate_block_item (bi: block_item) (ctx: context) (j: int) : string*context*int = 
	match bi with
	| Statement(st) -> generate_statement st ctx j
	| Declaration(d) -> generate_declaration d ctx j

and generate_block_item_list (block_items: block_item list) (ctx: context) (j: int) : string*context*int = 
	let rec helper (block_items: block_item list) (acc: string) (ctx: context) (j: int): string*context*int =
		match block_items with
		| [] -> (acc, ctx, j)
		| hd::tl -> 
			let (bi, new_ctx, new_j) = generate_block_item hd ctx j in
			helper tl (acc^bi) new_ctx new_j
	in
	helper block_items "" ctx j
;;

let generate_function (f: fun_decl) (j: int) : string*int = 
	match f with
	| Function(name, block_items) -> 
		let (block_items, ctx, new_j) = generate_block_item_list block_items {var_map = V.empty; offset =  -8} j in
		((Printf.sprintf ".globl %s\n%s:\n" name name) 
		^"pushq %rbp\n"
		^"movq %rsp, %rbp\n"
		^block_items, new_j)
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


