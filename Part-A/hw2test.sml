(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val all_except_option_test1 = all_except_option ("string", ["string"]) = SOME []
val all_except_option_test2= all_except_option ("string", []) = NONE
val all_except_option_test3 = all_except_option ("test", ["string", "test", "123"]) = SOME ["string", "123"]
val all_except_option_test4 = all_except_option ("abc", ["string", "test"]) = NONE
val all_except_option_test5 = all_except_option ("abc", ["string", "123"]) = NONE

val get_substitutions1_test1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val get_substitutions1_test2 = get_substitutions1 ([["foo", "buzz", "bar"],["there"]], "foo") = ["buzz", "bar"]
val get_substitutions1_test3 = get_substitutions1 ([["foo","buzz"],["there"],["foo","bar"]], "foo") = ["buzz","bar"]

val get_substitutions2_test1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val get_substitutions2_test2 = get_substitutions2 ([["foo", "buzz", "bar"],["there"]], "foo") = ["buzz", "bar"]
val get_substitutions2_test3 = get_substitutions2 ([["foo","buzz"],["there"],["foo","bar"]], "foo") = ["buzz","bar"]

val similar_names_test1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val similar_names_test2 = similar_names ([], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", middle="W", last="Smith"}]
val similar_names_test3 = similar_names ([["foo", "bar"], ["buzz"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", middle="W", last="Smith"}]

val card_color_test1 = card_color (Clubs, Num 2) = Black
val card_color_test2 = card_color (Hearts, Queen) = Red

val card_value_test1 = card_value (Clubs, Num 2) = 2
val card_value_test2 = card_value (Hearts, Queen) = 10
val card_value_test3 = card_value (Spades, Ace) = 11

val remove_card_test1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val remove_card_test2 = remove_card ([(Clubs, Num 2),(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Clubs, Num 2)]
val remove_card_test3 = remove_card ([(Hearts, Ace),(Spades, Ace),(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Spades, Ace),(Hearts, Ace)]

val all_same_color_test1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val all_same_color_test2 = all_same_color [(Hearts, Ace), (Clubs, Ace)] = false
val all_same_color_test3 = all_same_color [(Hearts, Ace)] = true
val all_same_color_test4 = all_same_color [(Clubs,Ace),(Spades,Ace),(Diamonds,Ace)] = false


val sum_cards_test1 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val sum_cards_test2 = sum_cards [(Clubs, Num 2),(Hearts, Queen)] = 12
val sum_cards_test3 = sum_cards [(Clubs, Num 2),(Hearts, Queen),(Spades, Ace)] = 23

val score_test1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val score_test2 = score ([(Hearts, Queen),(Clubs, Num 4)],10) = 12
val score_test3 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
val score_test4 = score ([(Hearts, Queen),(Diamonds, Num 5)],10) = 7
val score_test5 = score ([],10) = 5

val officiate_test1 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val officiate_test2 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val officiate_test3 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
val officiate_test4 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[], 15) = 7
val officiate_test5 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw,Discard(Hearts, Num 2)], 15) = 7
val officiate_test6 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw,Draw], 15) = 9

val score_challenge_test1 = score_challenge ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val score_challenge_test2 = score_challenge ([(Hearts, Queen),(Clubs, Num 4)],10) = 12
val score_challenge_test3 = score_challenge ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
val score_challenge_test4 = score_challenge ([(Hearts, Queen),(Diamonds, Num 5)],10) = 7
val score_challenge_test5 = score_challenge ([(Hearts, Queen),(Spades, Ace)],20) = 3
val score_challenge_test6 = score_challenge ([(Hearts, Queen),(Spades, Ace)],18) = 7
val score_challenge_test7 = score_challenge ([],18) = 9

val officiate_challenge_test1 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val officiate_challenge_test2 = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val officiate_challenge_test3 = ((officiate_challenge([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
val officiate_challenge_test4 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[], 15) = 7
val officiate_challenge_test5 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[Draw,Discard(Hearts, Num 2)], 15) = 7
val officiate_challenge_test6 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)],[Draw,Draw], 15) = 9

val careful_player_test1 = careful_player([(Hearts, Num 2),(Clubs, Num 4)],10) = [Draw, Discard (Hearts, Num 2), Draw]
val careful_player_test2 = careful_player([(Hearts, Num 2),(Clubs, Num 4)],20) = [Draw, Draw, Draw]
val careful_player_test3 = careful_player([(Hearts, Num 2),(Clubs, Ace),(Spades, Queen)],21) = [Draw, Draw, Discard (Hearts, Num 2), Draw]
val careful_player_test4 = careful_player([(Spades, Queen),(Hearts, Num 2),(Clubs, Ace)],21) = [Draw, Draw, Discard (Hearts, Num 2), Draw]
val careful_player_test5 = careful_player([(Spades, Queen),(Clubs, Ace)],21) = [Draw, Draw]
val careful_player_test6 = careful_player([(Hearts, Num 2),(Clubs, Ace),(Spades, Queen)],15) = [Draw, Draw, Discard (Clubs, Ace), Draw]