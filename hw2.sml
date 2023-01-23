(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)


(* string , string list -> Option 
produce NONE if string is not in the list else poudce SOME list , \
where the list is identical to the original 
Assume: String appear at most once
 !!!!*)
fun rev (list,acc) =
  case list of 
  [] => acc
  |x::xs => rev(xs, x::acc)


 


fun all_except_option1(s, sl) = 
 let fun helper(sl) = 
    case sl of
    [] => []
    | x::xs => if same_string(s,x) then helper(xs) else  x :: helper(xs)

    val result = helper(sl)
    
 in  if length result < length (sl) then SOME result else NONE  end 
   


fun all_except_option2(s,sl) = 
case sl of 
 [] => NONE
 |x::xs => if same_string(x,s) then SOME xs else case all_except_option3(s,xs) of 
                                            NONE => NONE 
                                            |SOME y =>  SOME(x::y)



(* string list list  , string -> string list
The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result.
 *)

fun get_substitutions1( list_sl ,s) = 
case list_sl of 
 [] => []
 | sl::rest =>   case all_except_option2(s,sl) of
                          NONE => get_substitutions1( rest ,s)
                          | SOME xs => xs @ get_substitutions1( rest ,s)



fun get_substitutions2 (list_sl, s) = 
let fun helper (st_list,rsf) = 
    case st_list of 
    [] => rsf
    | sl::rest => case all_except_option2(s,sl) of
                                                NONE => helper (rest,rsf)
                                               | SOME xs =>  helper (rest,rsf  @ xs)
in 

helper(list_sl,[])
end




 (* string list list  , full_name 
  produce a list of full_name by subsitiing the first name 
  start with the original name
 *)

fun similar_names(ls_stl,full_name) = 
let 
 val  {first=x, middle=y, last=z} = full_name
 fun helper(sl) = 
case sl of 
 [] => []
 | n::rest => {first=n, last =z, middle=y} :: helper(rest)

 in  full_name::helper(get_substitutions2(ls_stl,x))  end
 
             

	     

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


fun card_color card = 
case card of 
(Clubs, _) => Black
 |(Spades, _) => Black
 | _ => Red



fun card_value card = 
case card of 
(_, Num x) => x
 |(_, Ace) => 11
 |_ => 10

(* card list , card , exceptipn =>  card list 
produce a card list excluding the card , if no card raise exceptipn

*)
fun remove_card (cs, c, e) = 
let fun found(c1) = 
     c1 = c 

     fun helper(card_list) = 
     case card_list of 
     [] => raise e
     | x::xs => if found(x) then xs  else x::helper(xs) 
in
helper(cs) 
end

(* card list -> Bool 
produce true if all the cards has the same color *)
(* !!! *)
fun all_same_color cs= 
let 
     fun all_same_as_first(color, list) = 
         case list of 
           [] => true
          | x::xs => color = card_color(x) andalso all_same_as_first(color,xs)
in
 case cs of 
 [] => true
 |card0::cards0 => all_same_as_first(card_color(card0),cards0)
 end


(* list of cards => int
 produce the sum of values of the cards
 *)

fun sum_cards cs = 
let fun sum_cards0(cards, rsf) = 
    case cards of 
    [] => rsf
     |x::xs => sum_cards0(xs, card_value(x) + rsf)
in 
sum_cards0(cs,0)
end



(* list of cards , int -> int 
if the sum of the card list > goal  => 3 * (sum -goal)
else  => goal - sum
if the held cards all the same color divide the result by 2

*)

fun score (cs, goal) = 
let val cards_sum = sum_cards(cs)
in
case (cards_sum  > goal, all_same_color(cs)) of
(true, true) => (3 * (cards_sum - goal)) div 2
 |(false, true) =>  (goal - cards_sum) div 2
 |(false, false) => (goal - cards_sum)
 |(true, false) => 3 * (cards_sum - goal)
 end


fun score2(cs, goal) = 
let val cards_sum = sum_cards(cs)
     in 
        (if cards_sum > goal then cards_sum - goal else goal - cards_sum)  div (if all_same_color(cs) then 2 else 1 ) end
 

(* card list, moves list , int  -> exception | int
 • The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
with a larger held-cards and a smaller card-list.

*)
fun officiate (cs,ml,goal) = 
let 
  fun helper(cs0, ml0, held_cards) = 
   case (cs0, ml0, held_cards) of
     (card1::rest_cards, Draw::rest_moves, held_Cards0) => if sum_cards(card1::held_Cards0) > goal
                                                           then score(card1::held_Cards0,goal)
                                                           else helper(rest_cards, rest_moves, card1::held_Cards0)                                                                                    
   |(cards, Discard c::rest_moves, held_Cards0) => helper(cards, rest_moves, remove_card(held_Cards0, c, IllegalMove))                                                                                 
   | (_, [], held_Cards0) => score(held_Cards0,goal)
   | ([], _, held_Cards0) => score(held_Cards0,goal)
 in
  helper(cs,ml, [])
end
            
(* list cards => list cards
replace one ACE to Num 1
 *)
fun replace_Ace (cs0) =
     case cs0 of 
     [] => []
     |(s,Ace)::cards => (s,Num 1)::cards
     |card::cards => card::replace_Ace(cards)


(* list cards, goal => int
find a sum that is less than the goal if exist
 *)

fun find_least_sum (cs, goal) = 
let 
    val sum = sum_cards(cs)
    val card_list = cs
in
if sum <= goal then sum else   let val sum1 = sum_cards(card_list) 
                                   val ace_replaced  = replace_Ace(card_list)
                                   val sum2 = sum_cards(ace_replaced)
                               in if sum1 = sum2 then sum1 else find_least_sum(ace_replaced, goal) end

end
 
fun score_challenge (cs, goal) = 
let 
    val first_score = score(cs, goal) 
    val ace_replaced = replace_Ace(cs)
    val second_score = score(ace_replaced,goal)
in
if first_score <= second_score then  first_score else score_challenge(ace_replaced,goal)
   
end


fun officiate_challenge(cs,ml,goal) = 
let 
  fun helper(cs0, ml0, held_cards) = 
   case (cs0, ml0, held_cards) of
     (card1::rest_cards, Draw::rest_moves, held_Cards0) => if find_least_sum(card1::held_Cards0,goal) > goal
                                                           then score_challenge(card1::held_Cards0,goal)
                                                           else helper(rest_cards, rest_moves, card1::held_Cards0)                                                                                    
   |(cards, Discard c::rest_moves, held_Cards0) => helper(cards, rest_moves, remove_card(held_Cards0, c, IllegalMove))                                                                                 
   | (_, [], held_Cards0) => score_challenge(held_Cards0,goal)
   | ([], _, held_Cards0) => score_challenge(held_Cards0,goal)
 in
  helper(cs,ml, [])
end




(* card list , int => move list
calling officiate with the card-list, the goal, and the move-list has this behavior:

• The value of the held cards never exceeds the goal.
• A card is drawn whenever the goal is more than 10 greater than the value of the held cards. As a
detail, you should (attempt to) draw, even if no cards remain in the card-list.
• If a score of 0 is reached, there must be no more moves.
• If it is possible to reach a score of 0 by discarding a card followed by drawing a card, then this
must be done. Note careful_player will have to look ahead to the next card, which in many card
games is considered “cheating.” Also note that the previous requirement takes precedence: There
must be no more moves after a score of 0 is reached even if there is another way to get back to 0
 *)

fun pos_reach_zero(card, held_cards, goal) =
let 
val diff = goal - card_value(card)
val sum = sum_cards(held_cards)
val card_wanted_value = sum - diff

fun helper(held_cards) = 
  case held_cards of
  []=> NONE
  |x::xs => if card_value(x) = card_wanted_value then SOME x else helper(xs)
in 
 if card_wanted_value > 0  then helper(held_cards)  else NONE
end

val test44 = pos_reach_zero( (Hearts, Num 7),[(Hearts, Num 2), (Hearts, Num 4),(Hearts, Num 4),(Hearts, Num 4)],17) = SOME (Hearts, Num 4)



fun careful_player (cs, goal) = 
let 
fun helper(list_cards, ml, held_cards) = 
  let val sum_held_cards = sum_cards(held_cards) 
      val held_cards_sofar = held_cards
      val move_list = ml 
in
case (list_cards, ml, held_cards) of 

  ([],ml,held_cards) => rev(ml,[])
 |(card1::rest_cards, ml, held_cards) => if score(held_cards,goal) = 0 
                                         then rev(ml,[])
                                         else case pos_reach_zero(card1,held_cards,goal) of
                                                               
                                                 NONE => if (goal  > sum_cards(card1::held_cards) orelse score(card1::held_cards,goal) = 0 ) 
                                                          then helper(rest_cards,Draw::ml,card1::held_cards)
                                                          else rev(ml,[])
                                                |SOME c => helper(card1::rest_cards,Discard c::ml,remove_card(held_cards,c,IllegalMove))  end   
in
  helper(cs,[],[])
end 



val test_hi = careful_player([(Hearts, Num 2), (Hearts, Num 4),(Hearts, Num 4),(Hearts, Num 4),(Hearts, Num 7)],17)