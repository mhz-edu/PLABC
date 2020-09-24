(* Produce true if date1 comes before date2 *)
(* ASSUME: Dates date1 and date2 have positive year, a month between 1 and 12,
 * and a day no greater than 31 *)

fun is_older(date1:(int * int * int), date2:(int * int * int)):bool =
  if (#1 date1) = (#1 date2) then
    if (#2 date1) = (#2 date2) then
      if (#3 date1) = (#3 date2) then
        false
      else (#3 date1) < (#3 date2)
    else (#2 date1) < (#2 date2)
  else (#1 date1) < (#1 date2)

(* Produce how many dates in the list are in the given month *)

fun number_in_month(dates:(int * int * int) list, month:int):int =
  if null dates then 0
  else if #2 (hd dates) = month then
    number_in_month(tl dates, month) + 1
  else number_in_month(tl dates, month)

(* Produce the number of dates in the list of dates that are 
 * in any of the months in the list of months.
 * ASSUME: the list of months has no number repeated. *)

fun number_in_months(dates:(int * int * int) list, months:int list):int = 
  if null dates orelse null months then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* Produce list of dates from a given list of dates that are in the given month *)

fun dates_in_month(dates:(int * int * int) list, month:int):(int * int * int) list =
  if null dates then []
  else if #2 (hd dates) = month then
    (hd dates)::dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)

(* Produce list of dates from a given list of dates that are 
 * in any month of given list of months
 * ASSUME: the list of months has no number repeated.*)

fun dates_in_months(dates:(int * int * int) list, months:int list):(int * int * int) list =
  if null dates orelse null months then []
  else dates_in_month(dates, hd months)@dates_in_months(dates, tl months)

(* Produce n-th element of the given list of strings, where head is 1st *)

fun get_nth(strings:string list, n:int):string = 
  if null strings then ""
  else if n = 1 then hd strings
  else get_nth(tl strings, n - 1)

(* Produce string representation of date in format Month day, year *)

fun date_to_string(date:(int * int * int)):string = 
  let val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in
    get_nth(months,#2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* Produce a number of first elements of given list which sum is less than given sum
 * ASSUME:  1. Given list contains all positive int numbers 
 *          2. Given sum is positive int number
 *          3. Sum of all number in list is greater than given sum *)

fun number_before_reaching_sum(sum:int, numbers:int list):int = 
  let
    fun sum_first_n(numbers, n) = 
      if n = 0 then hd numbers
      else hd numbers + sum_first_n(tl numbers, n - 1);
    fun num(sum, numbers, n) = 
      if null numbers then 0
      else if sum_first_n(numbers, n) >= sum then n
      else num (sum, numbers, n + 1)
  in
    num (sum, numbers, 0)
  end

(* Produce a number of month the given day of the year is in
 * ASSUME: 1. Year has 365 days
 *         2. Leap year does not count *)

fun what_month(day: int):int = 
  let
    val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, months) + 1
  end

(* Produce a list [m1,m2,...,mn] where m1 is the month of day1,
 * m2 is the month of day1+1, ..., and mn is the month of day day2.*)

fun month_range (day1:int, day2: int):int list =
  if day2 < day1 then []
  else if day1 = day2 then what_month(day1)::[]
  else what_month(day1)::month_range(day1 + 1, day2)

(* Produce option of earliest date in the given list or NONE for empty list *)

fun oldest(dates:(int * int * int) list):(int * int * int) option =
  let
    fun oldest_1(dates, earliest_date) = 
      if null dates then SOME earliest_date
      else if is_older(earliest_date, hd dates) then oldest_1(tl dates, earliest_date)
      else oldest_1(tl dates, hd dates)
  in
    if null dates then NONE
    else oldest_1(tl dates, hd dates)
  end
  
(* Produce the number of dates in the list of dates that are 
 * in any of the months in the list of months.
 * NOTE: the list of months is allowed to has repeated numbers. *)

fun number_in_months_challenge(dates:(int * int * int) list, months:int list):int =
  let
    fun number_in_list(numbers, n) = 
      if null numbers then false
      else (hd numbers) = n orelse number_in_list(tl numbers, n)
    fun remove_dups(numbers) = 
      if null numbers then []
      else if number_in_list(tl numbers, hd numbers) then remove_dups(tl numbers)
      else hd numbers::remove_dups(tl numbers)
  in
    number_in_months(dates, remove_dups(months))
  end

(* Produce list of dates from a given list of dates that are 
 * in any month of given list of months
 * ASSUME: the list of months is allowed to has repeated numbers.*)

fun dates_in_months_challenge(dates:(int * int * int) list, months:int list):(int * int * int) list =
  let
    fun number_in_list(numbers, n) = 
      if null numbers then false
      else (hd numbers) = n orelse number_in_list(tl numbers, n)
    fun remove_dups(numbers) = 
      if null numbers then []
      else if number_in_list(tl numbers, hd numbers) then remove_dups(tl numbers)
      else hd numbers::remove_dups(tl numbers)
  in
    dates_in_months(dates, remove_dups(months))
  end

(* Produce true if date is in common era *)

fun reasonable_date(date:int * int * int):bool = 
  let
    val mnths = [(1,31), (2,29), (3,31), (4,30), (5,31), (6,30), (7,31), (8,31), (9,30), (10,31), (11,30), (12,31)]
    fun leap_year (year) = 
      if year mod 4 <> 0 then false
      else if year mod 100 <> 0 then true
      else if year mod 400 <> 0 then false
      else true
    fun month_days(month, months:(int * int) list) = 
      if null months then 0
      else if #1 (hd months) = month then #2 (hd months)
      else month_days(month, tl months)
  in
    if (#1 date) < 1 then false
    else if (#2 date) < 1 orelse (#2 date) > 12 then false
    else if (#3 date) < 1 orelse (#3 date) > 31 then false
    else if (#2 date) = 2 andalso (#3 date) = 29 andalso not(leap_year(#1 date)) then false
    else if (#3 date) > month_days(#2 date, mnths) then false
    else true
  end
