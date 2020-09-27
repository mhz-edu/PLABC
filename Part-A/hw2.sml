
(* code provided by the course *)
(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* ----- solutions for problem 1 ----- *)

(* Problem 1 - 1a *)
(* Produce NONE if str is not in the strings list,
   else return SOME lst where lst is identical to strings without str in it
   ASSUME: str is in strings at most once *)
fun all_except_option (str, strings) = 
    case strings of
          [] => NONE
        | hd :: tl => if same_string(str, hd)
                      then SOME tl
                      else (case all_except_option (str, tl) of
                                NONE => NONE
                                | SOME lst => SOME (hd :: lst))
    
(* Problem 2 - 1b *)
(* Select all lists of strings (substitutions) from given 
   list of lists (list of substitutions) that contain given 
   string and return combined list of selected substitutions 
   but without the string 
   ASSUME: Each substitutions has no repeats inside *)
fun get_substitutions1 (subslist, str) =
    case subslist of
      [] => []
    | hd :: tl => case all_except_option(str, hd) of
                      NONE => get_substitutions1(tl, str)
                    | SOME lst => lst @ get_substitutions1(tl, str)

(* Problem 3 - 1c *)
(* Tail recursive get_substitutions *)
fun get_substitutions2 (subslist, str) =
    let
        fun helper(subslist, acc) = 
            case subslist of
                  [] => acc
                | hd :: tl => case all_except_option(str, hd) of
                                    NONE => helper(tl, acc)
                                  | SOME lst => helper(tl, acc @ lst)
    in
        helper(subslist, [])
    end

(* Problem 4 - 1d *)
(* Produce list of full name records from given full name record
   and list of substitions by substituting first name of the given 
   record is substituted with the matching from the substitution list *)
fun similar_names (subslist, fullname) = 
    let
        val {first=fname, middle=mname, last=lname} = fullname
        fun helper (lst, acc) = 
            case lst of
                  [] => acc
                | hd :: tl => helper(tl, acc @ [{first=hd, middle=mname, last=lname}]) 
    in
        case subslist of
              [] => [fullname]
            | _ :: _ => helper(get_substitutions2(subslist, fname), [fullname])
    end

(* code provided by the course *)    
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* ----- solutions for problem 2 ----- *)

(* Problem 5 - 2a *)
(* Produce color of given card*)
fun card_color c =
    case c of
         (Clubs, _) => Black
        | (Spades, _) => Black
        | (Diamonds, _) => Red
        | (Hearts, _) => Red

(* Problem 6 - 2b *)
(* Produce value of given card*)
fun card_value c =
    case c of
      (_, Num i) => i
    | (_, Ace) => 11
    | (_, _) => 10

(* Problem 7 - 2c *)
(* Remove from given list of cards cs the first occurence of 
   the given card c. If card c is not found raise and exception e *)
fun remove_card (cs, c, e) = 
    case cs of
          [] => raise e
        | hd :: tl => if hd = c
                      then tl
                      else hd :: remove_card(tl, c, e)

(* Problem 8 - 2d *)
(* Produce true if all cards in the given list have the same color *)
fun all_same_color cs = 
    let
        fun helper(cs, base_card) = 
        (* Assume cards list has at least 1 card *)
            case cs of
                  hd :: [] => card_color(hd) = card_color(base_card)
                | hd :: tl => card_color(hd) = card_color(base_card) andalso helper(tl, base_card)
    in
        case cs of
            [] => true
            | hd :: [] => true
            | hd :: tl => helper(tl, hd)
        
    end

(* Problem 9 - 2e *)
(* Produce sum of values of the cards in the given list *)
fun sum_cards cs = 
    let
        fun helper (cs, acc) = 
            case cs of
                  [] => acc
                | hd :: tl => helper(tl, acc + card_value(hd)) 
    in
        helper(cs, 0)
    end

(* Problem 10 - 2f *)
(* Produce a score for the given card list and the goal *)
fun score (cs, goal) =
    let
        val sum = sum_cards(cs)
        val color_mult = if all_same_color(cs)
                         then 2
                         else 1
    in
        case cs of
            [] => goal div color_mult
            | _ :: _ => if sum > goal
                        then (sum - goal) * 3 div color_mult
                        else (goal - sum) div color_mult
    end

(* Problem 11 - 2g *)
(* Produce final game score by checking list of player moves against
   list of cards and a goal *)
fun officiate (cs, moves, goal) = 
    let
        fun helper(cs, moves, goal, hand) = 
            case moves of
                [] => score(hand, goal)
                | Draw :: rest_moves => (case cs of
                                               [] => score(hand, goal)
                                             | new_card :: rest_cards => let
                                                                             val new_hand = new_card :: hand
                                                                             val current_score = score(new_hand, goal)
                                                                         in
                                                                             if current_score > goal
                                                                             then current_score
                                                                             else helper(rest_cards, rest_moves, goal, new_hand) 
                                                                         end)
                | Discard c :: rest_moves => helper(cs, rest_moves, goal, remove_card(hand, c, IllegalMove))
            
    in
        helper(cs, moves, goal, [])
    end

(* Problem 12 - 3a-1 *)
(* Produce a score for the given card list and the goal 
   Challenge: Ace can have a value of 1 or 11 *)
fun score_challenge (cs, goal) =
    let
        val sum = sum_cards(cs)
        val color_mult = if all_same_color(cs)
                         then 2
                         else 1
        fun num_of_aces(cs) = 
            let
                fun helper(cs, acc) = 
                    case cs of
                          [] => acc
                        | (_, Ace) :: tl => helper(tl, acc + 1)
                        | _ :: tl => helper(tl, acc) 
            in
                helper(cs, 0)
            end
        val sum_with_aces = sum - 10 * num_of_aces(cs)
    in
        case cs of
            [] => goal div color_mult
            | _ :: _ => if goal > sum
                        then (goal - sum) div color_mult
                        else if goal > sum_with_aces
                             then if (sum - goal) * 3 > (goal - sum_with_aces)
                                  then (goal - sum_with_aces) div color_mult
                                  else (sum - goal) * 3 div color_mult
                             else (sum_with_aces - goal) * 3 div color_mult
    end

(* Problem 13 - 3a-2 *)
(* Produce final game score by checking list of player moves against
   list of cards and a goal 
   Challenge: Ace can have a value of 1 or 11 *)
fun officiate_challenge (cs, moves, goal) = 
    let
        fun helper(cs, moves, goal, hand) = 
            case moves of
                [] => score_challenge(hand, goal)
                | Draw :: rest_moves => (case cs of
                                               [] => score_challenge(hand, goal)
                                             | new_card :: rest_cards => let
                                                                             val new_hand = new_card :: hand
                                                                             val current_score = score_challenge(new_hand, goal)
                                                                         in
                                                                             if current_score > goal
                                                                             then current_score
                                                                             else helper(rest_cards, rest_moves, goal, new_hand) 
                                                                         end)
                | Discard c :: rest_moves => helper(cs, rest_moves, goal, remove_card(hand, c, IllegalMove))
            
    in
        helper(cs, moves, goal, [])
    end

(* Problem 14 - 3b *)
(* Produce list of moves that suits the careful player description
   for a given listf of cards and a goal *)
fun careful_player (cs, goal) =
    let
        fun get_largest_card cs = 
            (* Assume that cs is not empty *)
            let
                fun helper(cs, largest) = 
                    case cs of
                          [] => largest
                        | hd :: tl => if card_value(hd) > card_value(largest)
                                      then helper(tl, hd)
                                      else helper(tl, largest)
            in
                case cs of
                    hd :: tl => helper(tl, hd)

            end
        (* Returns SOME card from the list that needs to be discarded
           to get sum=goal after discarding and drawing next_card *)
        fun check_next(cs, next_card, goal) =
            let
                val sum_with_nc = sum_cards(next_card :: cs)
                fun helper(cs, sum) = 
                    case cs of
                          [] => NONE
                        | hd :: tl => if sum - card_value(hd) = goal
                                      then SOME hd
                                      else helper(tl, sum)
            in
                if  sum_with_nc < goal
                then NONE
                else helper(cs, sum_with_nc)
            end
        fun helper (cs, goal, hand, moves) =
            case cs of
                  [] => if goal - sum_cards(hand) > 10
                        then moves @ [Draw]
                        else moves
                | current_card :: rest_cards => case hand of
                                                      [] => helper(rest_cards, goal, current_card :: hand, moves @ [Draw])
                                                    | _ :: _ => let
                                                                    val largest = get_largest_card(hand)
                                                                    val nc_checked = check_next(hand, current_card, goal)
                                                                in
                                                                    case nc_checked of
                                                                          SOME c => moves @ [(Discard c),Draw]
                                                                        | NONE => if goal - sum_cards(hand) > 10
                                                                                  then helper(rest_cards, goal, current_card :: hand, moves @ [Draw])
                                                                                  else helper(cs, goal, remove_card(hand, largest, IllegalMove), moves @ [(Discard largest)])
                                                                end
    in
        helper (cs, goal, [], [])
    end