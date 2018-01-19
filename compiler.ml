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
			| Keyword of string
			| Integer of int
			| Identifier of string
			| None

(* for AST (parsing) *)

type or_operator = OR 
type and_operator = AND
type eq_operator = Equal | NotEqual
type rel_operator = GreaterThan | LessThan | GTorEqual | LTorEqual
type add_sub_operator = Add | Subtract
type mul_div_operator = Multiply | Divide
type unary_operator = Negation | BitwiseComplement | LogicalNegation

type binary_operator = 	  AddSubOp of add_sub_operator 
						| MulDivOp of mul_div_operator
						| RelOp of rel_operator
						| EqOp of eq_operator
						| AndOp of and_operator
						| OrOp of or_operator

type log_or_exp = LogOrBinOp of or_operator*log_or_exp*log_or_exp | LogAndExp of log_and_exp
and log_and_exp = LogAndBinOp of and_operator*log_and_exp*log_and_exp | EqExp of eq_exp
and eq_exp = EqBinOp of eq_operator*eq_exp*eq_exp | RelExp of rel_exp
and rel_exp = RelBinOp of rel_operator*rel_exp*rel_exp | AddExp of add_exp
and add_exp = AddBinOp of add_sub_operator*add_exp*add_exp | MulExp of mul_exp
and mul_exp = MulBinOp of mul_div_operator*mul_exp*mul_exp | Factor of factor
and factor = Const of int | UnOp of unary_operator*factor | Parenthetical of (*log_or_exp*) add_exp

(*type exp =    LogOr of log_or_exp
			| LogAnd of log_and_exp
			| Eq of eq_exp
			| Rel of rel_exp
			| AddSub of add_exp
			| MulDiv of mul_exp*)


type statement = Return of (*log_or_exp*) add_exp
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
	| Plus -> AddSubOp(Add)
	| Minus -> AddSubOp(Subtract)
	| Times -> MulDivOp(Multiply)
	| ForwardSlash -> MulDivOp(Divide)
	| GTToken -> RelOp(GreaterThan)
	| LTToken ->  RelOp(LessThan)
	| GTorEqualToken -> RelOp(GTorEqual)
	| LTorEqualToken -> RelOp(LTorEqual)
	| EqualToken -> EqOp(Equal)
	| NotEqualToken -> EqOp(NotEqual)
	| AndToken -> AndOp(AND)
	| OrToken -> OrOp(OR)
	| OpenBrace | CloseBrace | OpenParen | CloseParen -> raise (Parse_exn "This is not a binary operation")
	| Semicolon | BitwiseComplement | LogicalNegation -> raise (Parse_exn "This is not a binary operation")
	| Keyword(_) | Identifier(_) | Integer(_) | None -> raise (Parse_exn "This is not a binary operation")
;;

			
let or_op_creator (op: binary_operator) (e1: log_or_exp) (e2: log_or_exp) : log_or_exp =
	match op with
	| OrOp(f) -> LogOrBinOp(f, e1, e2)
	| _ -> raise (Parse_exn "Problem with or_op_creator")
;;

let and_op_creator (op: binary_operator) (e1: log_and_exp) (e2: log_and_exp) : log_and_exp =
	match op with
	| AndOp(f) -> LogAndBinOp(f, e1, e2)
	| _ -> raise (Parse_exn "Problem with and_op_creator")
;;

let eq_op_creator (op: binary_operator) (e1: eq_exp) (e2: eq_exp) : eq_exp =
	match op with
	| EqOp(f) -> EqBinOp(f, e1, e2)
	| _ -> raise (Parse_exn "Problem with eq_op_creator")
;;

let rel_op_creator (op: binary_operator) (e1: rel_exp) (e2: rel_exp) : rel_exp =
	match op with
	| RelOp(f) -> RelBinOp(f, e1, e2)
	| _ -> raise (Parse_exn "Problem with rel_op_creator")
;;

let mul_op_creator (op: binary_operator) (e1: mul_exp) (e2: mul_exp) : mul_exp =
	match op with
	| MulDivOp(f) -> MulBinOp(f, e1, e2)
	| _ -> raise (Parse_exn "Problem with mul_op_creator")
;;

let add_op_creator (op: binary_operator) (e1: add_exp) (e2: add_exp) : add_exp =
	match op with
	| AddSubOp(f) -> AddBinOp(f, e1, e2)
	| _ -> raise (Parse_exn "Problem with add_op_creator")
;;

let parse_exp_creator   (parse_lower_level: token list -> 'b*(token list)) 
						(matching_operators: token list) 
						(bin_op_creator: binary_operator -> 'a -> 'a -> 'a)
						(raiser: 'b -> 'a) : (token list -> 'a*(token list)) =
	let rec f (tokens: token list) : 'a*token list = 
		let (lower_exp, tl) = parse_lower_level tokens in
		let rec loop (lst: token list) (acc: 'a) : 'a*(token list) =
			match lst with
			| hd::tl -> 
				(if List.mem hd matching_operators 
				then let (new_lower_exp, new_tl) = parse_lower_level tl in
					loop new_tl (bin_op_creator (token_to_binary_operator hd) acc (raiser new_lower_exp))
				else (acc, hd::tl))
			| [] -> (acc, []) 
		in
		loop tl (raiser lower_exp)
	in f
;;

let rec parse_logical_or_exp (tokens: token list) : log_or_exp*(token list) =
	let f = parse_exp_creator parse_logical_and_exp [OrToken] or_op_creator (fun (e: log_and_exp) -> LogAndExp(e)) in
	f tokens

and parse_logical_and_exp (tokens: token list) : log_and_exp*(token list) =
	let f = parse_exp_creator parse_equality_exp [AndToken] and_op_creator (fun (e: eq_exp) -> EqExp(e)) in
	f tokens

and parse_equality_exp (tokens: token list) : eq_exp*(token list) =
	let f = parse_exp_creator parse_relational_exp [EqualToken; NotEqualToken] eq_op_creator (fun (e: rel_exp) -> RelExp(e)) in
	f tokens

and parse_relational_exp (tokens: token list) : rel_exp*(token list) =
	let f = parse_exp_creator parse_add_exp [GTToken; LTToken; GTorEqualToken; LTorEqualToken] rel_op_creator (fun (e: add_exp) -> AddExp(e)) in
	f tokens

and parse_add_exp (tokens: token list) : add_exp*(token list) =
	let f = parse_exp_creator parse_mul_exp [Plus; Minus] add_op_creator (fun (e: mul_exp) -> MulExp(e)) in
	f tokens

and parse_mul_exp (tokens: token list) : mul_exp*(token list) =
	let f = parse_exp_creator parse_factor [Times; ForwardSlash] mul_op_creator (fun (e: factor) -> Factor(e)) in
	f tokens

and parse_factor (tokens: token list) : factor*(token list) =
	print_tokens tokens;
	print_string "parse_factor\n";
	match tokens with
	| OpenParen::tl -> (match parse_add_exp tl with
						| (e, CloseParen::new_tl) -> (Parenthetical(e), new_tl)
						| _ -> raise (Parse_exn "Missing expression or ')' in factor"))
	| Minus::tl -> let (inner_factor, new_tl) = parse_factor tl in
						(UnOp(Negation, inner_factor), new_tl)
	| BitwiseComplement::tl -> let (inner_factor, new_tl) = parse_factor tl in
						(UnOp(BitwiseComplement, inner_factor), new_tl)
	| LogicalNegation::tl -> let (inner_factor, new_tl) = parse_factor tl in
						(UnOp(LogicalNegation, inner_factor), new_tl)
	| Integer(x)::tl -> print_tokens tl;
		print_string "parse_factor after\n";
		(Const(x), tl)
	| _ -> raise (Parse_exn "parse_factor expects an Integer, UnOp, or BinOp")
;;
(*and parse_mul_exp (tokens: token list) : mul_exp*(token list) = 
	let (fac, tl) = parse_factor tokens in
	let rec loop (lst: token list) (acc: mul_exp) : mul_xp*(token list) =
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

and parse_add_exp (tokens: token list) : add_expexp*(token list) =
	let (t, tl) = parse_term tokens in
	let rec loop (lst: token list) (acc: add_exp) : add_exp*(token list) =
		match lst with
		| Plus::tl -> 
			let (new_term, new_tl) = parse_mul_exp tl in
			loop new_tl (AddBinOp(Add, acc, Term(new_term)))
		| Minus::tl -> 
			let (new_term, new_tl) = parse_term tl in
			loop new_tl (AddBinOp(Subtract, acc, Term(new_term)))
		| _ -> (acc, lst)
	in
	loop tl (MulExp(t))
;;*)

let parse_statement (tokens: token list) : statement*(token list) = 
	match tokens with
	| Keyword(s)::tl when s="return" ->
		(match parse_add_exp tl with 
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

let rec generate_add_exp (e: add_exp) : string =
	match e with
	| MulExp(e1) -> generate_mul_exp e1
	| AddBinOp(op, e1, e2) ->
		(match op with 
		| Add -> (generate_add_exp e1)
				^"pushq %rax\n"
				^(generate_add_exp e2)
				^"popq %rcx\n"
				^"addq %rcx, %rax\n"
		| Subtract -> (generate_add_exp e2)
						^"pushq %rax\n"
						^(generate_add_exp e1)
						^"popq %rcx\n"
						^"subq %rcx, %rax\n")

and generate_factor (f: factor) : string =
	match f with 
	| Const(x) -> Printf.sprintf "movq $%i, %%rax\n" x
	| UnOp(op, inner_factor) -> 
		let inner_fac_assembly = generate_factor inner_factor in
		let unary_op_assembly =
			(match op with
			| Negation -> "neg %rax\n"
			| LogicalNegation -> "cmpq $0, %rax\n movq $0, %rax\n sete %al\n"
			| BitwiseComplement -> "not %rax\n") 
		in
		inner_fac_assembly^unary_op_assembly
	| Parenthetical(e) -> generate_add_exp e

and generate_mul_exp (e: mul_exp) : string =
	match e with
	| Factor(f) -> generate_factor f
	| MulBinOp(op, e1, e2) ->
		(match op with
		| Multiply -> (generate_mul_exp e1)
						^"pushq %rax\n"
						^(generate_mul_exp e2)
						^"popq %rcx\n"
						^"imulq %rcx\n"
		| Divide -> (generate_mul_exp e2)
					^"pushq %rax\n"
					^(generate_mul_exp e1)
					^"popq %rcx\n"
					^"xor %rdx, %rdx\n"
					^"idivq %rcx\n")
;;

let generate_statement (st: statement) : string =
	match st with
	| Return(e) -> 
		(generate_add_exp e)^"ret\n"
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
print_tokens (lex (to_list all_lines));
print_string "\n";
Printf.fprintf out "%s" (generate (parse (lex (to_list all_lines))));;


