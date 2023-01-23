(* Coursera Programming Languages, Homework 3, Provided Code *)
exception NoAnswer

(*problem 1 *)
fun only_capitals xs=
  List.filter (fn x => Char.isUpper(String.sub(x,0))) xs

(*problem 2 *)
fun longest_string1 xs=
  case xs of
       [] => ""
     | _  => List.foldl (fn (x, y) => if String.size x > String.size y then x
                                         else y) "" xs

(*problem 3 *)
fun longest_string2 xs=
  case xs of
       [] => ""
     | _  => List.foldl (fn (x, y) => if String.size x >= String.size y then x
                                         else y) "" xs
(*problem 4 *)                          
fun longest_string_helper f xs=
  List.foldl (fn (x, y) => if f(String.size x,String.size y) then x else y) "" xs

val longest_string3 = longest_string_helper (fn (x,y) => x > y) 
val longest_string4 = longest_string_helper (fn (x,y) => x >= y) 

(*problem 5*)
val longest_capitalized = longest_string1 o only_capitals

(*problem 6*)
fun rev_string s = (String.implode o List.rev o String.explode) s

                  (*====================================================*)
(*problem 7*)
fun first_answer f xs=
  case xs of 
       [] => raise NoAnswer
     | x::xs' => let val ans = f x
                  in case ans of
                          SOME e => e
                        |  _ => first_answer f xs'
                  end
                          
(*problem 8*)
fun all_answers f xs=
  let 
    fun helper acc ys=
         case ys of 
           [] => SOME acc
         | y::ys' => case f y of
                         NONE => NONE
                      | SOME ls => helper (ls@acc) ys'
  in 
    helper [] xs
  end

                  (*====================================================*)
(*Provided function*)
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

(*problem 9*)
fun count_wildcards p=
  g (fn ()=> 1) (fn x => 0) p

fun count_wild_and_variable_lenghts p=
  g (fn ()=> 1) (String.size) p

fun count_some_var (s, p)=
  g (fn ()=> 0) (fn x => if x=s then 1 else 0) p
 

(*problem 10*)
fun check_pat p=
  let fun get_varaibles ps=  
          case ps of
             Variable x => [x]
            | TupleP y => List.foldl (fn (p, i)=> (get_varaibles p) @ i ) [] y
            | ConstructorP(_, y) => get_varaibles y
            | _ => []
      fun is_unique ps =
        case ps of
             [] => true 
           | x::xs' =>  not (List.exists (fn y => x=y) xs') andalso is_unique xs' 
  in is_unique (get_varaibles p)
  end
                        

(*problem 11*)
fun match (v, p)=
  case (v,p) of
       (_, Wildcard) => SOME []
     | (v, Variable s) => SOME [(s,v)]
     | (Unit, UnitP) => SOME []
     | (Const i, ConstP ii) => if i = ii then SOME [] else NONE
     | (Tuple x, TupleP y) => if length x = length y
                              then let val pairs = ListPair.zip (x, y)
                                    in all_answers match pairs
                                    end
                              else NONE
     | (Constructor(s1, vp), ConstructorP(s2, pp)) => if s1=s2 then match (vp,pp) else NONE
     | _ => NONE


(*problem 12*)
fun first_match v ps=
  SOME (first_answer (fn p => match(v,p)) ps)
  handle NoAnswer => NONE
