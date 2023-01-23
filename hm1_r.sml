(* This is a comment *)
(* is_older: takes 2 int lists and determines if x1 is older than x2*)
(* dateList  = year,month,day*)
(* int list*int list -> bool *)
fun is_older(x1 : int * int *int, x2 : int*int*int) =
    let 
        val y1 = #1 x1
        val m1 = #2 x1
        val d1 = #3 x1
        val y2 = #1 x2
        val m2 = #2 x2
        val d2 = #3 x2
    in
	y1 < y2 orelse (y1=y2 andalso m1 < m2)
	orelse (y1 = y2 andalso m1 = m2 andalso d1<d2)
    end

(*number_in_month*)
(*(int*int*int) list, int -> int*)
fun number_in_month(x1: (int*int*int) list, month : int) =
    if null x1
    then 0
    else if #2 (hd x1) = month
    then number_in_month(tl x1, month)+1
    else number_in_month(tl x1, month)

(*number_in_months*)
(*(int*int*int) list, int list -> int *)

fun number_in_months(dates : (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd(months)) + number_in_months(dates, tl months)

(*dates_in_month*)
(* (int*int*int)list, int -> (int*int*int) list*)

fun dates_in_month(dates : (int*int*int)list, month : int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)
	
(*dates_in_months*)
(* (int*int*int)list, int list -> (int*int*int)list*)

fun dates_in_months(dates : (int*int*int) list, months :int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*get_nth*)
(*string list, int -> string*)
fun get_nth(words: string list, n :int) =
    if n = 1
    then hd words
    else get_nth(tl words,n-1)
(*date_to_string converts int form of date to "month date, year" where month is    spelled *)
(*int list -> string*)
fun date_to_string(date : int *int*int ) =
    let
	val months = ["January","February","March","April","May","June","July",
		      "August","September","October","November","December"]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^
	", " ^  Int.toString(#1 date)
    end
	
(*number_before_reaching_sum returns number of last element such that the sum of all preceding elements is still > sum *)
	(* int list, int -> int *)
fun number_before_reaching_sum(sum: int, numbers : int list) =
    if hd numbers > sum
    then 0
    else 1 + number_before_reaching_sum(sum- hd(numbers), tl numbers)
(*what_month takes a number 1-365 and determines month*)
(* int -> int*)
fun what_month(day : int) =
    let
	val month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(day, month)+1
    end

(*month_range returns int list of all months between day1 and day2 *)
(* int, int -> int list *)
fun month_range(day1 : int, day2 : int)=
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1+1, day2)

(* oldest evaluates a list of dates and returns oldest date as
 int*int*int option*)
(* (int*int*int) list -> (int*int*int)option *)
fun oldest (dates :(int*int*int)list)=
    if null dates
    then NONE
    else
	let
	    val tail_oldest = oldest(tl dates)
	in
	    if isSome tail_oldest andalso is_older(valOf tail_oldest, hd dates)
	    then tail_oldest
	    else SOME(hd dates)
	end

	    
    
