
exception NoAnswer

(* string list  => string list
produce only the strings that start with capitals
*)
fun only_capitals (string_list ) =

List.filter (fn x => Char.isUpper(String.sub (x,0))) string_list
(* x might be a pattern *)

(* string list  => string list
produce only the strings that start with capitals, closests to the list beggining
*)
fun longest_string1(string_list) = 
 foldl(fn(s,b)=> if String.size(s) > String.size(b) then s else b) "" string_list

(* string list  => string list
produce only the strings that start with capitals closest to the list end
*)
fun longest_string2(string_list) = 
 foldl(fn(s,base)=> if String.size(s) >= String.size(base) then s else base) "" string_list

(* string list  => string list
produce only the strings that start with capitals,  closests to the list beggining
*)

fun curry f string_list = f(string_list) 

val longest_string3  = curry longest_string1 

(* string list  => string list
produce only the strings that start with capitals,  closest to the list end
*)
val  longest_string4 = curry  longest_string2

(* (int * int -> bool) -> string list -> string *)
fun longest_string_helper f = 
 foldl(fn(s,base)=> if f(String.size(s), String.size(base)) then s else base) "" 

(* string list => string
Return the longest string that start with uppercase orelse "" 
Assume all string have at least one charachter 
*)

val longest_capitalized =  fn string_list =>
    (longest_string1 o only_capitals) string_list

(* string => string 
reverse the chahrvhetrs of the string  *)
fun rev_string string = 
  (implode o rev o explode) string



(*   (’a -> ’b option) -> ’a list -> ’b *)
fun first_answer f list = 
 case list of
 [] => raise NoAnswer
 | x::xs => case f(x) of  NONE => first_answer f xs 
                          |SOME y =>  y
                     
(* (’a -> ’b list option) -> ’a list -> ’b list option*)
(*produce a NONE if the function retrned None for any elemnt otherwise make a list of all the returned  *)
fun all_answers f list = 
let fun loop (list ,acc)= 
       case list of 
         [] => SOME acc
         |x::xs => case f(x) of  NONE => NONE
                                |SOME lst =>   loop(xs ,(lst @ acc))
in loop(list, []) end


datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(* pattern -> int
count all the Wildcards in the given pattern *)
fun count_wildcards (p)  = 
    g (fn x => 1) (fn x => 0) p

(* pattern -> int 
 count all the Wildcards in the given pattern  plus the sum of sizes of string in Variables *)
fun count_wild_and_variable_lengths (p) = 
    g (fn x => 1) (fn x => String.size x) p


(* (string,pattern) => int
count how many times the string appered in a variable
 *)
fun count_some_var (s,p) = 
    g (fn x => 0) (fn x => if x = s then 1 else 0) p


(* pattern => string list
produce all the strings in the variable pattern  *)
fun all_strings p =
	case p of
	    Wildcard          => []
	  | Variable x        => [x]
	  | TupleP ps         => List.foldl (fn (p,i) => (all_strings p) @ i) [] ps
	  | ConstructorP(_,p) => all_strings p
	  | _                 => []
   
(* string list => Bool
produce true if there is no dublicate in the list  *)
fun check_string list_string = 
    case list_string of 
	[] => true
	|x::xs => if List.exists (fn s => s = x) xs then false else check_string(xs)

(* pattern => Bool
return true if and only if all the variables contain diffrent strings *)
  fun check_pat (p) = 
     (check_string o all_strings) p  
    

(* (valu, pattern) => (string,valu) list option *)
fun match (v,p) = 
 case (v,p) of
 (_,Wildcard) => SOME []
  |( v, Variable s) => SOME [(s,v)]
  |(Unit, UnitP) => SOME []
  |(Const v, ConstP p) => if p <> v then NONE else SOME []
  |(Tuple vs, TupleP ps) => if length (vs) <> length (ps) then NONE else  all_answers (fn (x,y)=> match (x,y)) (ListPair.zip(vs,ps))
  |(Constructor (s1,v), ConstructorP (s2,p)) => if s1 <> s2 then NONE else match (v,p)
  | _ => NONE
  

(* Valu , List of pattern => (string, valu) list option *)
fun first_match value list_pattern = 
     ( SOME(first_answer (fn p => match (value,p)) list_pattern) handle NoAnswer => NONE)



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

