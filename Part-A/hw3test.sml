(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val only_capitals_test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val only_capitals_test2 = only_capitals [] = []
val only_capitals_test3 = only_capitals ["A","asd","Cbc"] = ["A","Cbc"]
val only_capitals_test4 = only_capitals ["A","B123","cBc"] = ["A","B123"]

val longest_string1_test1 = longest_string1 ["A","bc","C"] = "bc"
val longest_string1_test2 = longest_string1 [] = ""
val longest_string1_test3 = longest_string1 ["A","bc","C123vbn"] = "C123vbn"
val longest_string1_test4 = longest_string1 ["A","bc","C","bsp","oop"] = "bsp"

val longest_string2_test1 = longest_string2 ["A","bc","C"] = "bc"
val longest_string2_test2 = longest_string2 [] = ""
val longest_string2_test3 = longest_string2 ["A","bc","C123vbn"] = "C123vbn"
val longest_string2_test4 = longest_string2 ["A","bc","C","bsp","oop"] = "oop"

val longest_string3_test1 = longest_string3 ["A","bc","C"] = "bc"
val longest_string3_test2 = longest_string3 [] = ""
val longest_string3_test3 = longest_string3 ["A","bc","C123vbn"] = "C123vbn"
val longest_string3_test4 = longest_string3 ["A","bc","C","bsp","oop"] = "bsp"

val longest_string4_test1 = longest_string4 ["A","bc","C"] = "bc"
val longest_string4_test2 = longest_string4 [] = ""
val longest_string4_test3 = longest_string4 ["A","bc","C123vbn"] = "C123vbn"
val longest_string4_test4 = longest_string4 ["A","bc","C","bsp","oop"] = "oop"
val longest_string4_test5 = longest_string4 ["A","B","C"] = "C"

val longest_capitalized_test1 = longest_capitalized ["A","bc","C"] = "A"
val longest_capitalized_test2 = longest_capitalized ["A","bc","C","Bsp","Oop"] = "Bsp"
val longest_capitalized_test3 = longest_capitalized [] = ""
val longest_capitalized_test4 = longest_capitalized ["a","bc","c123vbn"] = ""

val rev_string_test1 = rev_string "abc" = "cba"
val rev_string_test2 = rev_string "" = ""
val rev_string_test3 = rev_string "A" = "A"
val rev_string_test4 = rev_string "tenet" = "tenet"

val first_answer_test1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val first_answer_test2 = ((first_answer (fn x => if x > 10 then SOME x else NONE) [1,2,3,4,5]; false) handle NoAnswer => true)
val first_answer_test3 = ((first_answer (fn x => if x > 10 then SOME x else NONE) []; false) handle NoAnswer => true)

val all_answers_test1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val all_answers_test2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val all_answers_test3 = all_answers (fn x => if String.size x >= 3 then SOME [x] else NONE) ["and", "bag"] = SOME ["and", "bag"]
val all_answers_test4 = all_answers (fn x => if x > 4 then SOME [x] else NONE) [5,6,7] = SOME [5,6,7]

val count_wildcards_test1 = count_wildcards Wildcard = 1
val count_wildcards_test2 = count_wildcards (Variable "s") = 0
val count_wildcards_test3 = count_wildcards (ConstructorP("a", Wildcard)) = 1
val count_wildcards_test4 = count_wildcards (TupleP [Wildcard, Variable "a", Wildcard]) = 2
val count_wildcards_test5 = count_wildcards (UnitP) = 0
val count_wildcards_test6 = count_wildcards (ConstP 3) = 0

val count_wild_and_variable_lengths_test1 = count_wild_and_variable_lengths (Variable("a")) = 1
val count_wild_and_variable_lengths_test2 = count_wild_and_variable_lengths (Variable("abc")) = 3
val count_wild_and_variable_lengths_test3 = count_wild_and_variable_lengths (Wildcard) = 1
val count_wild_and_variable_lengths_test4 = count_wild_and_variable_lengths (ConstructorP("a", Wildcard)) = 1
val count_wild_and_variable_lengths_test5 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "abc", Variable "test"]) = 8
val count_wild_and_variable_lengths_test6 = count_wild_and_variable_lengths (UnitP) = 0
val count_wild_and_variable_lengths_test7 = count_wild_and_variable_lengths (ConstP 3) = 0

val count_some_var_test1 = count_some_var ("x", Variable("x")) = 1
val count_some_var_test2 = count_some_var ("z", Variable("x")) = 0
val count_some_var_test3 = count_some_var ("z", Wildcard) = 0
val count_some_var_test4 = count_some_var ("z", UnitP) = 0
val count_some_var_test5 = count_some_var ("z", ConstP 1) = 0
val count_some_var_test6 = count_some_var ("z", ConstructorP("z", Wildcard)) = 0
val count_some_var_test7 = count_some_var ("abc", TupleP [Wildcard, Variable "abc", Variable "abc"]) = 2

val check_pat_test1 = check_pat (Variable("x")) = true
val check_pat_test2 = check_pat (Wildcard) = true
val check_pat_test3 = check_pat (UnitP) = true
val check_pat_test4 = check_pat (ConstP 1) = true
val check_pat_test5 = check_pat (ConstructorP("z", Wildcard)) = true
val check_pat_test6 = check_pat (TupleP [Wildcard, Variable "abc", Variable "abc"]) = false
val check_pat_test7 = check_pat (TupleP [Wildcard, Variable "abc", Variable "zxc"]) = true

val check_pat_test8 = check_pat (TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Variable "x"]) = false
val check_pat_test9 = check_pat (TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"]) = false
val check_pat_test10 = check_pat (ConstructorP ("hi",TupleP[Variable "x",Variable "x"])) = false
val check_pat_test11 = check_pat (ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false

val match_test1 = match (Const(1), UnitP) = NONE
val match_test2 = match (Const(1), Wildcard) = SOME []
val match_test3 = match (Const(1), Variable("a")) = SOME [("a", Const(1))]
val match_test4 = match (Unit, UnitP) = SOME []
val match_test5 = match (Const(1), ConstP 1) = SOME []
val match_test6 = match (Const(1), ConstP 2) = NONE
val match_test7 = match (Const(1), TupleP [Wildcard, Variable "abc", Variable "abc"]) = NONE
val match_test8 = match (Tuple [Const(1)], TupleP [Wildcard, Variable "abc", Variable "zxc"]) = NONE
val match_test9 = match (Tuple [Const(1), Const(2), Const(3)], TupleP [Wildcard, Variable "abc", Variable "zxc"]) = SOME [("abc", Const(2)),("zxc", Const(3))]
val match_test10 = match (Tuple [Const(1), Const(2), Const(3)], TupleP [Wildcard, ConstP 42, Variable "zxc"]) = NONE
val match_test11 = match (Constructor("asd", Const(1)), ConstP 2) = NONE
val match_test12 = match (Constructor("asd", Const(1)), ConstructorP("zxc", ConstP(1))) = NONE
val match_test13 = match (Constructor("asd", Const(1)), ConstructorP("asd", UnitP)) = NONE
val match_test14 = match (Constructor("asd", Const(1)), ConstructorP("asd", ConstP(1))) = SOME []

val first_match_test1 = first_match Unit [UnitP] = SOME []
val first_match_test2 = first_match Unit [ConstP 1, Wildcard, UnitP] = SOME []
val first_match_test3 = first_match (Const 1) [ConstP 1, Wildcard, UnitP] = SOME []
val first_match_test4 = first_match (Const 1) [UnitP, UnitP, Variable("a")] = SOME [("a", Const(1))]
val first_match_test5 = first_match (Const 1) [UnitP, UnitP, UnitP] = NONE

val typecheck_patterns_test1 = typecheck_patterns ([], [TupleP[Variable("x"),Variable("y")],TupleP[Wildcard,Wildcard]]) = SOME (TupleT[Anything,Anything])
val typecheck_patterns_test2 = typecheck_patterns ([], [TupleP[Wildcard,TupleP[Wildcard,Wildcard]],TupleP[Wildcard,Wildcard]]) = SOME (TupleT[Anything,TupleT[Anything,Anything]])
val typecheck_patterns_test3 = typecheck_patterns ([], [ConstP 10, Variable("a")]) = SOME IntT
val typecheck_patterns_test4 = typecheck_patterns ([], [ConstP 10, Variable("a"), ConstructorP("SOME",Variable "x")]) = NONE
val typecheck_patterns_test5 = typecheck_patterns ([], [TupleP [Variable("a"), ConstP 10, Wildcard],
                                                        TupleP [Variable("b"), Wildcard, ConstP 11],
                                                        Wildcard]) = SOME (TupleT [Anything, IntT, IntT])
val typecheck_patterns_test6 = typecheck_patterns ([("Red", "color", UnitT),
                                                    ("Green", "color", UnitT),
                                                    ("Blue", "color", UnitT)], [ConstructorP("Red", UnitP), Wildcard]) = SOME (Datatype "color")
val typecheck_patterns_test7 = typecheck_patterns ([("Sedan", "auto", Datatype "color"),
                                                    ("Truck", "auto", TupleT [IntT, Datatype "color"]),
                                                    ("SUV", "auto", UnitT)], [ConstructorP("Sedan", Variable("a")),
                                                                              ConstructorP("Truck", TupleP[Variable("b"),Wildcard]),
                                                                              Wildcard]) = SOME (Datatype "auto")
val typecheck_patterns_test8 = typecheck_patterns ([("Empty", "list", UnitT),
                                                    ("List", "list", TupleT [Anything, Datatype "list"])],
                                                    [ConstructorP("Empty", UnitP),
                                                    ConstructorP("List", TupleP[ConstP 10, ConstructorP("Empty", UnitP)]),
                                                    Wildcard]) = SOME (Datatype "list")
val typecheck_patterns_test9 = typecheck_patterns ([("Empty", "list", UnitT),
                                                    ("List", "list", TupleT [Anything, Datatype "list"])],
                                                    [ConstructorP("Empty", UnitP),
                                                    ConstructorP("List", TupleP[Variable("k"),Wildcard])]) = SOME (Datatype "list")
val typecheck_patterns_test10 = typecheck_patterns ([("Empty", "list", UnitT),
                                                    ("List", "list", TupleT [Anything, Datatype "list"])],
                                                    [ConstructorP("Empty", UnitP),
                                                    ConstructorP("List", TupleP[ConstructorP("Sedan", Variable("c")),Wildcard])]) = SOME (Datatype "list")
