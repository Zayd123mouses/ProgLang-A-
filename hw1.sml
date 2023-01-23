(* homework 1.sml *)

(* Data defition
 
 Date  = (2022,1,1)
 is a triprle 
 (int * int* int)
 where first int is a year 
 the second int is the month [1,12]
 the last int is the day [1,365]

 *)

 (* template 
 
fun (xs : int * int * int) =

(... #1 xs
     #2 xs
     #3 xs)


  *)

(* (int * int * int)  (int * int * int) -> bool *)
(* produce true if the first date is older than the second date (is less than the second date) *)
(*!!!!!!  *)


fun is_older(xs: (int * int * int) , ys: (int * int * int)) =
#1 xs < #1 ys orelse (#1 xs = #1 ys andalso #2 xs <  #2 ys)
orelse (#1 xs = #1 ys andalso #2 xs =  #2 ys andalso #3 xs < #3 ys)
 
(* 
#1 xs < #1 ys orelse if #1 xs  > #1 ys 
                      then false
                      else   #2 xs <  #2 ys orelse if #2 xs > #2 ys
                                                  then false 
                                                  else #3 xs < #3 ys  orelse 
                                                  if #3 xs > #3 ys
                                                  then false
                                                  else false *)


(*  (int * int * int) list  , int -> int 
 produce how many dates in the list has the same month as a given month
*)

fun number_in_month(xs: (int * int * int) list , y: int) =  
 if null xs
 then 0
 else if #2 (hd xs) = y 
         then 1 + number_in_month(tl xs, y)
          else number_in_month(tl xs, y)

(* (int * int * int) list  , int list -> int
  produce the number of date the has month in the list int
  ASSUME : no repeated month in the list of int
 *)
fun number_in_months(xs: (int * int * int) list , ys: int list) = 
if null ys 
then 0 
else number_in_month (xs , (hd ys)) + number_in_months(xs, tl ys)


(* (int * int * int) list  , int -> list (int * int * int) 
 produce how many dates in the list has the same month as a given month *)
fun dates_in_month(xs: (int * int * int) list , y: int) = 
if null xs 
then []
else  if #2 (hd xs) = y 
      then  (hd xs) :: dates_in_month(tl xs, y)
      else dates_in_month(tl xs, y)

(* (int * int * int) list  , int list ->  (int * int * int) list
  produce (int * int * int) listof date the has month in the list int
  ASSUME : no repeated month in the list of int
 *)


fun dates_in_months(xs: (int * int * int) list , ys: int list) =
if null ys 
then []
else dates_in_month(xs, hd ys) @ dates_in_months(xs, tl ys)

(* string list , int -> string 
 produce the nth elemnt of the list based of the given integer
 Assume the position of the firts elemnt is 1
 Assume: the given number is valid (not bigger than the length of the list )
*)

fun get_nth (xs: string list, y: int)  = 
(* context preserver accumulator 
acc is Integer: the current position of the elemnt 1 based index
["hi", "there", "how", "are", "you"] outer call

["hi", "there", "how", "are", "you"] 1
["hi", "there", "how", "are", "you"] 2
["hi", "there", "how", "are", "you"] 3
["hi", "there", "how", "are", "you"] 4
["hi", "there", "how", "are", "you"] 5

*)

let fun helper(xs: string list,  acc: int) = 
    if acc = y 
    then hd xs
    else helper(tl xs,  acc + 1)

in 

helper (xs, 1)

end


(* (int * int * int) -> String

 convert the date into a string representaion
 *)

fun date_to_string(xs : (int * int * int) ) = 

let 
 val months = ["January", "February", "March", "April","May", "June", "July",
              "August", "September", "October", "November", "December"]

 fun helper(x: int) = 
 " " ^ Int.toString x
 in
         
get_nth(months, #2 xs) ^ helper(#3 xs) ^ "," ^  helper(#1 xs) 

end 

(* int, int list -> int 
return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more
 Assume the entire list sums to more than the passed in
  value
*)

fun number_before_reaching_sum (x: int, ys: int list) = 
(* context preserver accumulator and tail recursive accumulator
sum : int ; the sum of values so far
pos: integer; the cureent position of the elemnt so far 
Assume 0-based index

[1,2,3,4,5] outer call

[1,2,3,4,5] 0  0
[  2,3,4,5] 1, 1
[    3,4,5] 3, 2
[      4,5] 6, 3
[        5] 10, 4

 *)
let fun helper (ys:int list, sum_so_far: int, pos: int) = 
        if sum_so_far >= x
        then pos - 1
        else helper(tl ys , hd ys + sum_so_far , pos + 1) 
in 
helper(ys, 0, 0)
end

(* int -> int 
 produce the month that day belong
 day is [1, 365]

 *)

 fun what_month(x: int) = 
 let val months  = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

 in 
 1 + number_before_reaching_sum(x, months)
 end

(* day year , day year ->  int list

produce a list of month int starting from day one till reach day 2 ,  or empty list if day 1 > day 2 

 *)

fun month_range(day1: int , day2: int) = 
if day1 > day2 
then []
else what_month(day1) :: month_range(day1 + 1, day2)


(* dates list -> (int * int * int ) option 
 get the oldest date in the list , and if there is no dates in the list return NONE

*)

fun oldest (xs: (int * int * int) list ) = 
if null xs 
then NONE
else
let
 fun helper(xs: (int * int * int) list, oldest: (int * int * int)) =
    if null xs 
    then oldest
    else if is_older(hd xs, oldest) 
         then helper(tl xs, hd xs)
         else helper(tl xs, oldest)
in 
SOME (helper(tl xs , hd xs))
end
  
(* (int * int * int) list  , int list ->  (int * int * int) list
  produce (int * int * int) listof date the has month in the list int
  ASSUME : there is repeated month in the list of int
 *)


fun member(month: int, rsf: int list) = 
  if null rsf
  then false
  else (hd rsf) = month orelse member(month, tl rsf)


fun remove_dub (months: int list, rsf: int list) = 
  if  null months
  then rsf
  else if member((hd months), rsf) then remove_dub (tl months, rsf)
                                 else   remove_dub (tl months, ((hd months) :: rsf))




 fun number_in_months_challenge(xs:(int * int * int) list, months0:int list) = 
 if null months0
 then 0 

 else 
 let 
 
  val new_months = remove_dub(months0, [])
  in 
   number_in_month(xs , (hd new_months)) + number_in_months_challenge(xs, tl new_months)
  end
  

(* (int * int * int) list  , int list ->  (int * int * int) list
  produce (int * int * int) listof date the has month in the list int
  ASSUME :  repeated month in the list of int
 *)

fun dates_in_months_challenge((xs: (int * int * int) list , months0: int list)) = 
if null months0
then []

else 
let 
val new_months = remove_dub(months0, [])
in 
dates_in_month (xs, hd new_months) @ dates_in_months_challenge(xs, tl new_months)

end



(* 01/01/1800 to 31/12/9999. *)
(* int * int * int -> boolean 
produce true if the date range from  01/01/1800 to 31/12/9999) *)

fun reasonable_date(date: (int * int * int)) = 
let 
 fun leap_year (y: int) =
  ((y mod 400) = 0 orelse (y mod 4) = 0 ) andalso  (y mod 100) <> 0
  in

 ( (#1 date > 0) andalso (  (#2 date >= 1) andalso (#2 date <= 12) ) ) 
 
 andalso  if member(#2 date, [4,6,9,11])
          then #3 date <= 31 andalso  #3 date > 0
          else if leap_year(#1 date) andalso #2 date = 2
               then #3 date <= 29 andalso #3 date >0
               else if #2 date = 2 then #3 date <= 28 andalso #3 date > 0 else #3 date <= 30 andalso #3 date > 0
          
end
