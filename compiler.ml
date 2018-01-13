open Str;;
open List;;

(*Type definitions*)
type token =  OpenBrace
			| CloseBrace
			| OpenParen
			| CloseParen
			| Semicolon
			| Minus
			| BitwiseComplement
			| LogicalNegation
			| Keyword of string
			| Integer of int
			| Identifier of string
			| None


type operator = Minus | BitwiseComplement | LogicalNegation
type exp = Const of int | UnOp of operator*exp
type statement = Return of exp
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
let split_and_flatten (input: string list) (regex: string) : string list = 
	let remove_id (split: Str.split_result) : string =
		match split with
		| Text(s)-> s
		| Delim(s) -> s
	in
	List.map remove_id (List.concat (List.map (Str.full_split (Str.regexp regex)) input )) 
;;

let split_into_strings (str: string) : string list =
	let token_string = Str.split (Str.regexp "[ \n\r\x0c\t]+") str in 
	split_and_flatten token_string "[-!~{}();]"
;;

let translate_to_token (s: string) : token = 
	let keywords = ["int"; "return"] in
	if s = "{" then OpenBrace else
	if s = "}" then CloseBrace else
	if s = "(" then OpenParen else
	if s = ")" then CloseParen else
	if s = ";" then Semicolon else
	if s = "-" then Minus else
	if s = "~" then BitwiseComplement else
	if s = "!" then LogicalNegation else
	if List.mem s keywords then Keyword(s) else
	if Str.string_match (Str.regexp "[0-9]+") s 0 then Integer(int_of_string s) else
	Identifier(s)
 ;;

let rec lex_helper (words: string list) : token list = 
	match words with
	| [] -> []
	| hd::tl -> (translate_to_token hd)::(lex_helper tl)
;;

let lex (program_text: string) : token list = 
	lex_helper (split_into_strings program_text)
;;

let print_token (t: token) : string = 
	match t with
	| OpenBrace -> "OpenBrace"
	| CloseBrace -> "CloseBrace"
	| OpenParen -> "OpenParen"
	| CloseParen -> "CloseParen"
	| Semicolon -> "Semicolon"
	| Minus -> "Minus"
	| BitwiseComplement -> "BitwiseComplement"
	| LogicalNegation -> "LogicalNegation"
	| Keyword(s) -> "Keyword "^s
	| Identifier(s) -> "Identifier "^s
	| Integer(x) -> "Integer "^(string_of_int x)
	| None -> "None"
;;

let rec print_token_helper (tokens: token list) : string = 
	match tokens with
	| [] -> ""
	| hd::tl -> (print_token hd)^", "^(print_token_helper tl)
;;

let print_tokens (tokens: token list) : unit =
	print_string ((print_token_helper tokens)^"\n")
;;

(* Parsing *)
let next_token (tokens: token list) : token*(token list) = 
	match tokens with 
	| [] -> (None, [])
	| hd::tl -> (hd, tl)
;;

let rec parse_exp (tokens: token list) : exp*(token list) = 
	match next_token tokens with
	| (Integer(x), tl) -> (Const(x), tl)
	| (Minus, tl) -> let (inner_exp, new_tl) = parse_exp tl in
						(UnOp(Minus, inner_exp), new_tl)
	| (BitwiseComplement, tl) -> let (inner_exp, new_tl) = parse_exp tl in
						(UnOp(BitwiseComplement, inner_exp), new_tl)
	| (LogicalNegation, tl) -> let (inner_exp, new_tl) = parse_exp tl in
						(UnOp(LogicalNegation, inner_exp), new_tl)
	| _ -> raise (Parse_exn "parse_exp expects an Integer or Unary Operation")
;;

let parse_statement (tokens: token list) : statement*(token list) = 
	match tokens with
	| Keyword(s)::tl when s="return" ->
		(match parse_exp tl with 
		| (e, Semicolon::tl) -> (Return(e), tl)
		| _ -> raise (Parse_exn "Missing semicolon"))
	| _ -> raise (Parse_exn "Missing 'return' keyword")
;;

let parse_function (tokens: token list) : fun_decl*(token list) = 
	match tokens with
	| Keyword(k)::Identifier(v)::OpenParen::CloseParen::OpenBrace::tl when k="int" ->
		(match parse_statement tl with
		| (st, CloseBrace::[]) -> (Function(v, st), [])
		| _ -> raise (Parse_exn "Missing statement or '}' in function"))
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

let rec generate_expression (e: exp) : string =
	match e with
	| Const(x) -> Printf.sprintf "movl $%i, %%eax\n" x
	| UnOp(op, inner_exp) -> 
		let inner_exp_assembly = generate_expression inner_exp in
		let unary_op_assembly =
			(match op with
			| Minus -> "neg %eax\n"
			| LogicalNegation -> "cmpl $0, %eax\n movl $0, %eax\n sete %al\n"
			| BitwiseComplement -> "not %eax\n") in
		inner_exp_assembly^unary_op_assembly


let generate_statement (st: statement) : string =
	match st with
	| Return(e) -> 
		(generate_expression e)^"ret\n"
;;

let generate_function (f: fun_decl) : string =
	match f with
	| Function(name, st) -> (Printf.sprintf ".globl %s\n%s:\n" name name) ^ (generate_statement st)
;;

let generate (ast: program) : string = 
	match ast with
	| Program(f) -> generate_function f
;;


let all_lines = single_string (read_lines Sys.argv.(1)) " " in
let assembly_filename = "/mnt/c/Users/Varqa/Documents/Compiler/write_a_c_compiler/"^(List.hd (Str.split (regexp "\\.") Sys.argv.(1)))^".s" in
let out = open_out assembly_filename in
print_tokens (lex all_lines);
Printf.fprintf out "%s" (generate (parse (lex all_lines)));;


