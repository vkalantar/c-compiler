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
			| Keyword of string
			| Integer of int
			| Identifier of string
			| None

(* for AST (parsing) *)
type mul_div_operator = Multiply | Divide
type add_sub_operator = Add | Subtract
type unary_operator = Negation | BitwiseComplement | LogicalNegation

type factor = Const of int | UnOp of unary_operator*factor | Parenthetical of exp
and
term = TermBinOp of mul_div_operator*term*term | Factor of factor
and
exp = ExpBinOp of add_sub_operator*exp*exp | Term of term

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
	split_and_flatten token_string "[- { } ( ) ; ! ~ \\+ \\* \\/ ]"
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
	if s = "+" then Plus else
	if s = "*" then Times else
	if s = "/" then ForwardSlash else
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
	| Plus -> "Plus"
	| Times -> "Times"
	| ForwardSlash -> "ForwardSlash"
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


let rec parse_factor (tokens: token list) : factor*(token list) =
	print_tokens tokens;
	print_string "parse_factor\n";
	match tokens with
	| OpenParen::tl -> (match parse_exp tl with
						| (e, CloseParen::new_tl) -> (Parenthetical(e), new_tl)
						| _ -> raise (Parse_exn "Missing expression or ')' in factor"))
	| Minus::tl -> let (inner_factor, new_tl) = parse_factor tl in
						(UnOp(Negation, inner_factor), new_tl)
	| BitwiseComplement::tl -> let (inner_factor, new_tl) = parse_factor tl in
						(UnOp(BitwiseComplement, inner_factor), new_tl)
	| LogicalNegation::tl -> let (inner_factor, new_tl) = parse_factor tl in
						(UnOp(LogicalNegation, inner_factor), new_tl)
	| Integer(x)::tl -> print_tokens tl;
	print_string "parse_factor after\n";(Const(x), tl)
	| _ -> raise (Parse_exn "parse_factor expects an Integer, UnOp, or BinOp")

(*and parse_term (tokens: token list) : term*(token list) = 
	let rec helper (tokens: token list) (factor_acc: factor list) (op_acc: mul_div_operator) : (factor list)(mul_div_operator list)*(token list) = 
		match parse_factor tokens with 
		| (fac, Times::tl) -> helper tl (fac::factor_acc) (Multiply::op_acc)
		| (fac, ForwardSlash::tl) -> helper tl (fac::factor_acc) (Divide::op_acc)
		| (fac, lst) -> (factor_acc, op_acc, lst)
	in
	match parse_factor tokens with
	| (fac, Times::tl) -> 
		let (facs, ops, new_tl) = helper tl [fac] [Multiply] in
		let facs = List.rev facs in
		let ops = List.rev ops in
		match (ops, facs) with
		| (op::tl, f::tl) -> BinOp
	| (fac, ForwardSlash::tl) -> helper tl [fac] [Divide]
	| (fac, lst) -> (Factor(fac), lst)*)

(*and parse_exp (tokens: token list) : exp*(token list) = 
	print_tokens tokens;
	print_string "parse_exp\n";
	match parse_term tokens with
	| (t, Plus::tl) -> let (new_t, new_tl) = parse_term tl in
						(ExpBinOp(Add, t, new_t), new_tl)
	| (t, Minus::tl) -> let (new_t, new_tl) = parse_term tl in
							print_tokens new_tl;
							print_string "parse_exp after\n";
						(ExpBinOp(Subtract, t, new_t), new_tl) 
	| (t, lst) -> (Term(t), lst)
;;*)
and parse_term (tokens: token list) : term*(token list) = 
	let (fac, tl) = parse_factor tokens in
	let rec loop (lst: token list) (acc: term) : term*(token list) =
		match lst with
		| Times::tl -> 
			let (new_fac, new_tl) = parse_factor tl in
			loop new_tl (TermBinOp(Multiply, acc, Factor(new_fac)))
		| ForwardSlash::tl -> 
			let (new_fac, new_tl) = parse_factor tl in
			loop new_tl (TermBinOp(Divide, acc, Factor(new_fac)))
		| _ -> (acc, lst)
	in
	loop tl (Factor(fac))

and parse_exp (tokens: token list) : exp*(token list) =
	let (t, tl) = parse_term tokens in
	let rec loop (lst: token list) (acc: exp) : exp*(token list) =
		match lst with
		| Plus::tl -> 
			let (new_term, new_tl) = parse_term tl in
			loop new_tl (ExpBinOp(Add, acc, Term(new_term)))
		| Minus::tl -> 
			let (new_term, new_tl) = parse_term tl in
			loop new_tl (ExpBinOp(Subtract, acc, Term(new_term)))
		| _ -> (acc, lst)
	in
	loop tl (Term(t))
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
let rec generate_factor (f: factor) : string =
	match f with 
	| Const(x) -> Printf.sprintf "movq $%i, %%rax\n" x
	| UnOp(op, inner_factor) -> 
		let inner_fac_assembly = generate_factor inner_factor in
		let unary_op_assembly =
			(match op with
			| Negation -> "neg %rax\n"
			| LogicalNegation -> "cmpq $0, %rax\n movq $0, %rax\n sete %al\n"
			| BitwiseComplement -> "not %rax\n") in
		inner_fac_assembly^unary_op_assembly
	| Parenthetical(e) -> generate_expression e

and generate_term (t: term) : string =
	match t with
	| Factor(f) -> generate_factor f
	| TermBinOp(op, t1, t2) ->
		(match op with
		| Multiply -> (generate_term t1)
						^"pushq %rax\n"
						^(generate_term t2)
						^"popq %rcx\n"
						^"imulq %rcx\n"
		| Divide -> (generate_term t2)
					^"pushq %rax\n"
					^(generate_term t1)
					^"popq %rcx\n"
					^"xor %rdx, %rdx\n"
					^"idivq %rcx\n")

and generate_expression (e: exp) : string =
	match e with
	| Term(t) -> generate_term t
	| ExpBinOp(op, e1, e2) ->
		(match op with 
		| Add -> (generate_expression e1)
				^"pushq %rax\n"
				^(generate_expression e2)
				^"popq %rcx\n"
				^"addq %rcx, %rax\n"
		| Subtract -> (generate_expression e2)
						^"pushq %rax\n"
						^(generate_expression e1)
						^"popq %rcx\n"
						^"subq %rcx, %rax\n")
;;

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
print_string "\n";
Printf.fprintf out "%s" (generate (parse (lex all_lines)));;


