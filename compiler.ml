open Str;;
open List;;

(*Type definitions*)

(* for tokens (lexing) *)
type token =  OpenBrace
			| CloseBrace
			| OpenParen
			| CloseParen
			| Semicolon
			| Minus
			| BitwiseComplement
			| LogicalNegation
			| Plus
			| Times
			| ForwardSlash
			| AndToken
			| OrToken
			| EqualToken
			| NotEqualToken
			| LTToken
			| LTorEqualToken
			| GTToken
			| GTorEqualToken
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


type exp = Assign of string*exp | Var of string | BinOp of binary_operator*exp*exp | UnOp of unary_operator*exp | Const of int
type statement = Return of exp | Declare of string*(exp option) | Exp of exp
type fun_decl = Function of string*statement
type program = Program of fun_decl

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
	let keywords = ["int"; "return"] in
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

let rec parse_exp (tokens: token list) : exp*(token list) =
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
	| Identifier(v)::tl -> (Var(v), tl)
	| _ -> raise (Parse_exn "parse_factor expects an Integer, UnOp, BinOp, or Var")
;;

let parse_statement (tokens: token list) : statement*(token list) = 
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
	| _ -> 
		(match parse_exp tokens with 
		| (e, Semicolon::tl) -> (Exp(e), tl)
		| _ -> raise (Parse_exn "Missing semicolon"))
;;

let parse_function (tokens: token list) : fun_decl*(token list) =
	let rec helper (tokens: token list) (acc: statement list) : statement list =
		match parse_statement tokens with
		| (_, []) -> raise (Parse_exn "Missing '}' in function")
		| (st, CloseBrace::[]) -> (st::acc) 
		| (st, tok) -> helper tok (st::acc)
	in
	match tokens with
	| Keyword(k)::Identifier(v)::OpenParen::CloseParen::OpenBrace::tl when k="int" ->
		(*(Function(v, List.rev (helper tl [])), [])*)
		(match parse_statement tl with
		| (st, CloseBrace::[]) -> (Function (v, st), []) 
		| _ -> raise (Parse_exn "Missing '}' in function"))
	| _ -> raise (Parse_exn "Function declaration syntax is incorrect")	
;;

let parse_program (tokens: token list) : program*(token list) = 
	match parse_function tokens with
	| (f, []) -> (Program(f), [])
	| _ -> raise (Parse_exn "Problem in parse_program")
;;

let parse (tokens: token list) : program =
	fst (parse_program tokens) 
;;

(* Generate *)
let rec generate_push_pop (e1: 'a) (e2: 'a) : string =
	(generate_exp e1)
	^"pushq %rax\n"
	^(generate_exp e2)
	^"popq %rcx\n"

and generate_exp (e: exp) : string = 
	match e with
	| BinOp(op, e1, e2) -> generate_binop op e1 e2
	| Const(x) -> Printf.sprintf "movq $%i, %%rax\n" x
	| UnOp(op, inner_factor) -> 
		let inner_fac_assembly = generate_exp inner_factor in
		let unary_op_assembly =
			(match op with
			| Negation -> "neg %rax\n"
			| LogicalNegation -> "cmpq $0, %rax\n movq $0, %rax\n sete %al\n"
			| BitwiseComplement -> "not %rax\n") 
		in
		inner_fac_assembly^unary_op_assembly

and generate_binop (op: binary_operator) (e1: exp) (e2: exp) = 
	match op with
	| Multiply -> (generate_push_pop e1 e2)
				  ^"imulq %rcx\n"
	| Divide -> (generate_push_pop e2 e1)
				^"xor %rdx, %rdx\n"
				^"idivq %rcx\n"
	| Add -> (generate_push_pop e1 e2)
			 ^"addq %rcx, %rax\n"
	| Subtract -> (generate_push_pop e2 e1)
					^"subq %rcx, %rax\n"
	| GreaterThan -> (generate_push_pop e2 e1)
					 ^"cmpq %rcx, %rax\n"
					 ^"movq $0, %rax\n"
					 ^"setg %al\n"
	| LessThan -> (generate_push_pop e2 e1)
				  ^"cmpq %rcx, %rax\n"
				  ^"movq $0, %rax\n"
				  ^"setl %al\n"
	| GTorEqual -> (generate_push_pop e2 e1)
				   ^"cmpq %rcx, %rax\n"
				   ^"movq $0, %rax\n"
				   ^"setge %al\n"
	| LTorEqual -> (generate_push_pop e2 e1)
				   ^"cmpq %rcx, %rax\n"
				   ^"movq $0, %rax\n"
				   ^"setle %al\n"
	| Equal -> (generate_push_pop e1 e2)
				^"cmpq %rcx, %rax\n"
				^"movq $0, %rax\n"
				^"sete %al\n"
	| NotEqual -> (generate_push_pop e1 e2)
				^"cmpq %rcx, %rax\n"
				^"movq $0, %rax\n"
				^"setne %al\n"
	| AND -> (generate_push_pop e1 e2)
			 ^"cmpq $0, %rcx\n"
			 ^"setne %cl\n"
			 ^"cmpq $0, %rax\n"
			 ^"movq $0, %rax\n"
			 ^"setne %al\n"
			 ^"andb %cl, %al\n"
	| OR -> (generate_push_pop e1 e2)
			^"orq %rcx, %rax\n"
			^"movq $0, %rax\n"
			^"setne %al\n"

let generate_statement (st: statement) : string =
	match st with
	| Return(e) -> (generate_exp e)^"ret\n"
;;

let generate_function (f: fun_decl) : string =
	match f with
	| Function(name, st) -> 
		(Printf.sprintf ".globl %s\n%s:\n" name name) ^ (generate_statement st)
;;

let generate (ast: program) : string = 
	match ast with
	| Program(f) -> generate_function f
;;


let all_lines = single_string (read_lines Sys.argv.(1)) " " in
let assembly_filename = "/mnt/c/Users/Varqa/Documents/Compiler/write_a_c_compiler/"^(List.hd (Str.split (regexp "\\.") Sys.argv.(1)))^".s" in
let out = open_out assembly_filename in
print_tokens (lex (to_list all_lines));
print_string "\n";
Printf.fprintf out "%s" (generate (parse (lex (to_list all_lines))));;


