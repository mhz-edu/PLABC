(* Homework1 Extended Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val is_older_test1 = is_older ((1,2,3),(2,3,4)) = true
val is_older_test2 = is_older ((1,12,31),(2,3,4)) = true
val is_older_test3 = is_older ((1,2,3),(1,2,3)) = false
val is_older_test4 = is_older ((2,3,4),(1,2,3)) = false
val is_older_test5 = is_older ((1,2,2),(1,2,3)) = true
val is_older_test6 = is_older ((1,1,2),(1,2,3)) = true

val number_in_month_test1 = number_in_month ([],2) = 0
val number_in_month_test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val number_in_month_test3 = number_in_month ([(2012,2,28),(2013,2,1),(2018,2,1)],2) = 3

val number_in_months_test1 = number_in_months ([],[2,3,4]) = 0
val number_in_months_test2 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val number_in_months_test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2]) = 1
val number_in_months_test4 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,2,28)],[2]) = 2
val number_in_months_test5 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val number_in_months_test6 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[6,5,1]) = 0

val dates_in_month_test1 = dates_in_month ([],2) = []
val dates_in_month_test2 = dates_in_month ([(2012,2,28),(2013,12,1),(2011,3,31)],2) = [(2012,2,28)]
val dates_in_month_test3 = dates_in_month ([(2012,2,28),(2013,12,1)],3) = []
val dates_in_month_test4 = dates_in_month ([(2012,5,28),(2013,5,1),(2011,5,31)],5) = [(2012,5,28),(2013,5,1),(2011,5,31)]

val dates_in_months_test1 = dates_in_months ([],[2,3,4]) = []
val dates_in_months_test2 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val dates_in_months_test3 = dates_in_months ([(2012,12,28),(2013,12,1),(2011,3,31),(2011,8,28)],[2,3,4]) = [(2011,3,31)]
val dates_in_months_test4 = dates_in_months ([(2012,2,28),(2013,8,1),(2011,3,31),(2011,4,28)],[12]) = []
val dates_in_months_test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,8,9]) = [(2012,2,28),(2011,3,31)]
val dates_in_months_test6 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[12,2,3,4]) = [(2013,12,1),(2012,2,28),(2011,3,31),(2011,4,28)]
val dates_in_months_test7 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val get_nth_test1 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val get_nth_test2 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"
val get_nth_test3 = get_nth (["hi", "there", "how", "are", "you"], 6) = ""
val get_nth_test4 = get_nth ([], 6) = ""

val date_to_string_test1 = date_to_string (2013, 6, 1) = "June 1, 2013"
val date_to_string_test2 = date_to_string (20, 12, 31) = "December 31, 20"

val number_before_reaching_sum_test1 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val number_before_reaching_sum_test2 = number_before_reaching_sum (10, []) = 0
val number_before_reaching_sum_test3 = number_before_reaching_sum (10, [100,2,3,4,5]) = 0
val number_before_reaching_sum_test4 = number_before_reaching_sum (10, [9,2,3,4,5]) = 1
val number_before_reaching_sum_test5 = number_before_reaching_sum (10, [1,2,3,3,5]) = 4

val what_month_test1 = what_month 70 = 3
val what_month_test2 = what_month 1 = 1
val what_month_test3 = what_month 365 = 12

val month_range_test1 = month_range (31, 34) = [1,2,2,2]
val month_range_test2 = month_range (34, 31) = []
val month_range_test3 = month_range (31, 31) = [1]
val month_range_test4 = month_range (31, 32) = [1,2]

val oldest_test1 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val oldest_test2 = oldest([]) = NONE
val oldest_test3 = oldest([(2012,2,28),(2012,3,31),(2012,4,28)]) = SOME (2012,2,28)
val oldest_test4 = oldest([(2012,3,28),(2012,3,31),(2012,3,1)]) = SOME (2012,3,1)

val number_in_months_challenge_test1 = number_in_months_challenge ([],[2,2,3,4]) = 0
val number_in_months_challenge_test2 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val number_in_months_challenge_test3 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2, 2]) = 1
val number_in_months_challenge_test4 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,2,28)],[2, 2]) = 2
val number_in_months_challenge_test5 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,2,3,3,4]) = 3
val number_in_months_challenge_test6 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[6,6,5,1]) = 0

val dates_in_months_challenge_test1 = dates_in_months_challenge ([],[2,2,3,4]) = []
val dates_in_months_challenge_test2 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val dates_in_months_challenge_test3 = dates_in_months_challenge ([(2012,12,28),(2013,12,1),(2011,3,31),(2011,8,28)],[2,3,3,4]) = [(2011,3,31)]
val dates_in_months_challenge_test4 = dates_in_months_challenge ([(2012,2,28),(2013,8,1),(2011,3,31),(2011,4,28)],[12]) = []
val dates_in_months_challenge_test5 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,8,8,9]) = [(2012,2,28),(2011,3,31)]
val dates_in_months_challenge_test6 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[12,2,3,4,4]) = [(2013,12,1),(2012,2,28),(2011,3,31),(2011,4,28)]
val dates_in_months_challenge_test7 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val reasonable_date_test1 = reasonable_date ((0,2,28)) = false
val reasonable_date_test2 = reasonable_date ((2012,22,28)) = false
val reasonable_date_test3 = reasonable_date ((2012,2,55)) = false
val reasonable_date_test4 = reasonable_date ((~23,2,55)) = false
val reasonable_date_test5 = reasonable_date ((1600,2,29)) = true
val reasonable_date_test6 = reasonable_date ((1700,2,29)) = false
val reasonable_date_test7 = reasonable_date ((2020,2,29)) = true
val reasonable_date_test8 = reasonable_date ((2019,2,29)) = false
val reasonable_date_test9 = reasonable_date ((2019,6,31)) = false