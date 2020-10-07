(* code provided by the course *)

exception NoAnswer

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(* ----- solutions for problems ----- *)

(* Problem 1 *)
(* Returns a list that has only those strings from the given list 
   that starts from capital letter
   ASSUME: all strings are at least 1 character long *)
val only_capitals = 
	List.filter (fn s => Char.isUpper(String.sub(s, 0)))

(* Problem 2 *)
(* Returns the longest string in the given list of strings 
   For empty list returns empty string. For tie returns string 
   closer to the beginning of the list *)
val longest_string1 = 
	List.foldl (fn (current, longest) => if String.size current > String.size longest
	                                     then current
									     else longest) ""

(* Problem 3 *)
(* Same as Problem 2 but in case of tie returns string
   closer to the end of the list *)
val longest_string2 = 
	List.foldl (fn (current, longest) => if String.size current >= String.size longest
	                                     then current
									     else longest) ""

(* Problem 4 *)
(* Helper function for longest strings *)
fun longest_string_helper f = 
    List.foldl (fn (current, longest) => if f(String.size current, String.size longest)
	                                     then current
									     else longest) ""
(* Problem 4a *)
(* Same as Problem 2 *)
val longest_string3 = longest_string_helper (op >)

(* Problem 4b *)
(* Same as Problem 3 *)
val longest_string4 = longest_string_helper (op >=)

(* Problem 5 *)
(* Returns the longest string that starts with the capital letter
   in the given list of strings. For empty list returns empty string.
   For tie returns string closer to the beginning of the list 
   ASSUME: all strings are at least 1 character long *)
val longest_capitalized = longest_string1 o only_capitals

(* Problem 6 *)
(* Returns a string with character of given string in reverse order *)
val rev_string = String.implode o rev o String.explode

(* Problem 7 *)
(* Returns value of the first non-NONE option that is produced by the given function
   applied to the elements of the given list.
   If there is no such element, raise NoAnswer. *)
fun first_answer f lst = 
    case lst of
          [] => raise NoAnswer
        | hd :: tl  => case f hd of
                             NONE => first_answer f tl
                           | SOME x => x

(* Problem 8 *)
(* Returns SOME list of valus of all non-NONE option that is produced by 
   the given function applied to the elements of the given list. 
   If there are no such elements, return NONE. *)
fun all_answers f lst = 
    let
        fun helper(lst, acc) = 
            case lst of
                  [] => SOME acc
                | hd :: tl => case f hd of
                                    NONE => NONE
                                  | SOME x => helper(tl, acc @ x)
    in
        helper(lst, [])
    end

(* Problem 9a *)
(* Returns number of Wildcard patterns in given pattern. *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* Problem 9b *)
(* Returns number of Wildcard patterns plus the sum of the 
   string lengths of all the variables in given pattern. *)
val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size(s))

(* Problem 9c *)
(* Returns the number of times given string appears as
   variable name in given pattern. *)
fun count_some_var (str, p) = 
    g (fn _ => 0) (fn s => if s = str then 1 else 0) p

(* Problem 10 *)
(* Returns true if all variables in the given pattern have distinct names *)
fun check_pat p = 
    let
        fun get_var_names (p, acc) = 
            case p of
                  Variable s => s :: acc
                | ConstructorP (_, pat) => get_var_names (pat, acc)
                | TupleP plist => List.foldl get_var_names acc plist
                | _ => acc
        fun check_no_dups str_list = 
            case str_list of
                  [] => true
                | _ :: [] => true  
                | hd :: tl => List.all (fn s => s <> hd) tl andalso check_no_dups(tl)
    in
       check_no_dups (get_var_names(p, []))
    end

(* Problem 11 *)
(* Produce NONE if given patter does not match with the given value,
   otherwise SOME lst where lst is the list of bindings *)
fun match (v, p) = 
    case (v, p) of
          (_, Wildcard) => SOME []
        | (v, Variable x) => SOME [(x, v)]
        | (Unit, UnitP) => SOME []
        | (Const x, ConstP y) => if x = y then SOME [] else NONE
        | (Constructor(vl_str, vl), ConstructorP(pat_str, pat)) => if vl_str = pat_str then match(vl, pat) else NONE
        | (Tuple vlist, TupleP plist) => (all_answers match (ListPair.zipEq (vlist, plist)) handle UnequalLengths => NONE)
        | (_, _) => NONE

(* Problem 12 *)
(* Returns NONE if givenn values does not match with any pattern 
   in the given list or the SOME lst where lst is the list of bindings 
   from the first matched pattern *)
fun first_match vl plist=
    (SOME (first_answer (fn pat => match(vl, pat)) plist)) handle NoAnswer => NONE

(* Challenge Problem *)
(* Return SOME typ that all patterns in the given list can have *)
(* Idea was to map individual patterns from given list to particular types
   and then identify the most common for all of them by kind of scoring *)
fun typecheck_patterns (dtypes, plist) = 
    let
        (* returns datatype that matches given constructor *)
        fun get_dt s dtlist = 
            (SOME (first_answer (fn (x,y,z) => if x = s then SOME (x,y,z) else NONE) dtlist)) handle NoAnswer => NONE
        (* Converts typ option list to typ list option *)
        fun oplst_conv l:typ list option = 
            case l of
                 [] => SOME []
                | NONE :: _ => NONE
                | SOME t :: tl => (case oplst_conv tl of
                                         NONE => NONE
                                       | SOME l => SOME (t :: l))
        (* typecheck single pattern *)
        fun helper pt = 
            case pt of
                Wildcard => SOME Anything
                | UnitP => SOME UnitT
                | ConstP _ => SOME IntT
                | Variable _ => SOME Anything
                | ConstructorP (s, pat) => (case (get_dt s dtypes) of
                                                  NONE => NONE
                                                | SOME (x,y,z) => (case (helper pat) of
                                                                        NONE => NONE
                                                                      | SOME _ => SOME (Datatype y)))
                | TupleP ptlist => (case (oplst_conv(List.map helper ptlist)) of
                                        NONE => NONE
                                      | SOME lst => SOME (TupleT lst))
        (* remove duplicate values from lists *)
        fun remove_dups_list lst = 
            case lst of
                  [] => []
                | hd :: tl => if (List.exists (fn x => x = hd) tl)
                                   then remove_dups_list tl
                                   else hd :: remove_dups_list tl
        (* Prioritizing types *)
        fun gen_score dt = 
            case dt of
                  UnitT => 0
                | IntT => 1
                | Anything => 2
                | Datatype _ => 3
                | TupleT _ => 4
        (* Getting list of type-priority pairs *)
        fun get_gen_type f tlist = 
            ListPair.zip(tlist, (List.map f tlist))
        (* Deriving type with the max priority and filtering out other types with smaller priority *)
        fun get_max pair_list = 
            let
                fun h (pair_list, (mt, ms)) = 
                    case pair_list of 
                          [] => (mt, ms)
                        | (x,y) :: tl => if y > ms then h (tl, (x,y)) else h (tl, (mt, ms))
                val (mtp, msc) = h (pair_list, (UnitT, 0))
            in
                List.map (fn (x,_) => x) (List.filter (fn (_,y) => y >= msc) pair_list)
            end
        (* Converts [[a1,a2...an]...[z1,z2...zn]] into [[a1...z1]...[an...zn]] *)
        fun conv l =
            let
                val flist = List.map (fn (x :: _) => x) l
                val rlist = List.map (fn (_ :: x) => x) l
            in 
                case rlist of
                      [] :: _ => [flist]
                    | (_ :: _) :: _ => flist :: conv rlist  
            end
        (* Priority of types inside TupleT *)
        fun intuple_score dt = 
            case dt of
                  UnitT => 0
                | IntT => 2
                | Anything => 1
                | Datatype _ => 3
                | TupleT _ => 4
        
        (* Getting list of type-priority pairs *)
        val get_gen_type1 = List.map (get_gen_type intuple_score)
        (* Getting typ list from typ list list *)
        fun get3 tlist = 
            case conv (List.map get_max tlist) of
                  [[]] => []
                | hd :: _ => hd
        (* Analyze contents of lists of inside TupleT types 
           and getting back most lenient*) 
        fun analyze tlist =
            get3(get_gen_type1(List.map remove_dups_list (conv (List.map (fn (TupleT x) => x) tlist))))
            
        (* Processing list of typ mappings *)
        fun proc tlist: typ option = 
            case tlist of
                  [] => NONE
                | x :: [] => SOME x
                | x :: _ => let
                                val (hd :: tl) = (remove_dups_list(get_max(get_gen_type gen_score tlist)))
                            in
                                (case hd of
                                  TupleT _ => let  
                                                val r = SOME (analyze (hd :: tl))
                                            in 
                                                (case r of
                                                    SOME [] => NONE
                                                    | SOME l => SOME (TupleT l))
                                            end
                                | _ => (case (hd :: tl)  of
                                            [] => NONE
                                          | x::[] => SOME x
                                          | x::_ => SOME x))
                            end                

    in
        case oplst_conv(List.map helper plist) of
              NONE => NONE
            | SOME l => proc l
    end