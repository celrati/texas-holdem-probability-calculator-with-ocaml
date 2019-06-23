(* types *)

type rang    = A | R | D | V | Point of int;;

type couleur = Pique | Coeur | Carreau | Trefle;;


type carte   = Carte of rang * couleur;;
type donne   = Donne of carte * carte;;
type flop    = Flop of carte * carte * carte;;
type river   = River of carte;;
type turn    = Turn of carte;;
type table   = Table5 of carte * carte * carte * carte * carte |
    Table4 of carte * carte * carte * carte |
	Table3 of carte * carte * carte;;
type comb = Comb of carte * carte * carte * carte * carte;;
type comb_type =   Quinte_Flush 
		 | Couleur  
		 | Suite   
		 | Carte_Haute 
		 | Carre
		 | Full
		 | Paire
		 | Double_Paire
		 | Brelan;;

type combX = CarreX of int * int
	     | FullX of int * int
	     | BrelanX of int * int * int
	     | Double_PaireX of int * int * int
	     | PaireX of int * int * int * int
	     | CarteNor of int * int * int * int * int;;
		 
		     

(* the comb are sorted from the highest to the lowest*)
(* for testint*)

let get_couleur cr = match cr with
  | Carte(r,c) -> c;;
let get_rang   cr = match cr with
  | Carte(r,c) -> r;;
let get_value_rang rg = match rg with
  | V -> 11
  | D -> 12
  | R -> 13
  | A -> 14 (*also has the value of 1*)
  | Point(p) -> p;;
let get_value_couleur cl = match cl with
  | Pique -> 1
  | Coeur -> 2
  | Carreau -> 3
  | Trefle -> 4;;


let is_Quinte_flush cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
     (*special case *)
     if get_rang c1 = A && get_rang c2 = Point(5) && get_rang c3 = Point(4) && get_rang c4 = Point(3) && get_rang c5 = Point(2) &&
        get_couleur c1 = get_couleur c2 &&
       get_couleur c2 = get_couleur c3 &&
       get_couleur c3 = get_couleur c4 &&
       get_couleur c4 = get_couleur c5  then true 
       else

     if (get_value_rang (get_rang c1) ) = ( get_value_rang (get_rang c2) + 1) &&
       (get_value_rang (get_rang c2) ) = ( get_value_rang (get_rang c3) + 1) &&
       (get_value_rang (get_rang c3) ) = ( get_value_rang (get_rang c4) + 1) &&
       (get_value_rang (get_rang c4) ) = ( get_value_rang (get_rang c5) + 1) &&
       get_couleur c1 = get_couleur c2 &&
       get_couleur c2 = get_couleur c3 &&
       get_couleur c3 = get_couleur c4 &&
       get_couleur c4 = get_couleur c5  then true else false;;

let is_Carre cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
     if ((get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2) ) &&
       (get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) ) &&
	 (get_value_rang (get_rang c3) ) = (get_value_rang (get_rang c4) )) ||
       
      ((get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) ) &&
       (get_value_rang (get_rang c3) ) = (get_value_rang (get_rang c4) ) &&
	  (get_value_rang (get_rang c4) ) = (get_value_rang (get_rang c5) )) then
       true else false;;

let is_Full cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
      if ((get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2) ) &&
       (get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) ) &&
	 (get_value_rang (get_rang c4) ) = (get_value_rang (get_rang c5) )) ||
       
      ((get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2) ) &&
       (get_value_rang (get_rang c3) ) = (get_value_rang (get_rang c4) ) &&
	  (get_value_rang (get_rang c4) ) = (get_value_rang (get_rang c5) )) then
       true else false;;

let is_Couleur cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
     if get_couleur c1 = get_couleur c2 &&
       get_couleur c2 = get_couleur c3 &&
       get_couleur c3 = get_couleur c4 &&
       get_couleur c4 = get_couleur c5 && not (is_Quinte_flush cm) then true
     else false;;

let is_Suite cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
     (*special case*)
     if  get_rang c1 = A && get_rang c2 = Point(5) && get_rang c3 = Point(4) && get_rang c4 = Point(3) && get_rang c5 = Point(2) &&
       not (is_Quinte_flush cm) then true 
       else

     if (get_value_rang (get_rang c1) ) = ( get_value_rang (get_rang c2) + 1) &&
       (get_value_rang (get_rang c2) ) = ( get_value_rang (get_rang c3) + 1) &&
       (get_value_rang (get_rang c3) ) = ( get_value_rang (get_rang c4) + 1) &&
       (get_value_rang (get_rang c4) ) = ( get_value_rang (get_rang c5) + 1) &&
       not (is_Quinte_flush cm) then true else false;;

let is_Brelan cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
     if (((get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2) ) &&
	 (get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) )) ||
       
       ((get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) ) &&
	   (get_value_rang (get_rang c3) ) = (get_value_rang (get_rang c4) )) ||
       
       ((get_value_rang (get_rang c3) ) = (get_value_rang (get_rang c4) ) &&
	   (get_value_rang (get_rang c4) ) = (get_value_rang (get_rang c5) )) )
       
       && not (is_Full cm) && not (is_Carre cm) then true else false;;

let is_DoublePaire cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
    if (((get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2) ) &&
	(get_value_rang (get_rang c3) ) = (get_value_rang (get_rang c4) )) ||
       
       ((get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2) ) &&
	   (get_value_rang (get_rang c4) ) = (get_value_rang (get_rang c5) )) ||
       
       ((get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) ) &&
	   (get_value_rang (get_rang c4) ) = (get_value_rang (get_rang c5) )) )
       && not (is_Full cm) && not (is_Carre cm) then true else false;;


let is_Paire cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
      if (( (get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2))) ||
      ((get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) )) ||
      ((get_value_rang (get_rang c3) ) = (get_value_rang (get_rang c4) )) ||
      ((get_value_rang (get_rang c4) ) = (get_value_rang (get_rang c5) )) )
      && not (is_Full cm) && not (is_Carre cm) && not (is_Brelan cm)
       && not (is_DoublePaire cm) then true else false;;

let is_CarteHaute cm = if
    not (is_Paire cm) && not (is_DoublePaire cm) && not (is_Brelan cm) &&
      not (is_Suite cm) && not (is_Couleur cm) && not (is_Full cm) &&
      not (is_Carre cm) && not (is_Quinte_flush cm) then true else false;;

(*
now we're gonna implement the sorting functions.
to sort compare and sort each combination 
from the highest ransk to the lowest
so that we're gonna need a function compare of two cards
thats return false if the first argument is less or equal to it's second
argument..
and after we use it to sort our combination
*)

let compareTwoCards cr1 cr2 =
  if get_value_rang (get_rang cr1) > get_value_rang (get_rang cr2) then 1 else
    if get_value_rang (get_rang cr1) < get_value_rang (get_rang cr2) then -1 else
      0;;
let compareTwoCardsInverse cr1 cr2 =
  if compareTwoCards cr1 cr2 = 1 then -1
    else if compareTwoCards cr1 cr2 = -1 then 1 else 0;;

(*
now the functions thats gonne sort the combination
the idea behind this function is to transform the combination
 to a list of cards
and sort the list after return back the list to combination
*)

(* transform the combination into a list*)
let combToList cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
  let l = ref [] in
  l := c1::(!l) ;
  l := c2::(!l) ;
  l := c3::(!l) ;
  l := c4::(!l) ;
  l := c5::(!l) ; !l;;

(* now we do the reverse card list into a combination *)

let listToComb l = match l with
  | c1::c2::c3::c4::c5::[] -> Comb(c1,c2,c3,c4,c5)
  | _::_::_::_::_::_::_  -> failwith "impossible case"
  | _::_::_::_::[]  -> failwith "impossible case"
  | _::_::_::[]  -> failwith "impossible case"
  | _::_::[]  -> failwith "impossible case"
  | _::[] -> failwith "impossible case"
  | [] -> failwith "empy list";;


(*we re gonna sort the Combination decreasinly*)

let sortTheCombinationDecreasing comb =
  let l  = (combToList comb) in
  let l' = List.sort (compareTwoCardsInverse) l in
  listToComb l';;

let getCombTypeComb cm' =
  if (is_Quinte_flush cm') then Quinte_Flush else
    if (is_Carre cm') then Carre else
      if (is_Full cm') then Full else
	if (is_Couleur cm') then Couleur else
	  if( is_Suite cm') then Suite else
	    if( is_Brelan cm') then Brelan else
	      if( is_DoublePaire cm') then Double_Paire else
		if( is_Paire cm') then Paire else
		   Carte_Haute;;


(*functions in case of tie between combinations for the Paire,Full,doublPaire,brelan,carre *)

let getReelCarre cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
     
     if ((get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2) ) &&
       (get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) ) &&
	 (get_value_rang (get_rang c3) ) = (get_value_rang (get_rang c4) )) then
       CarreX(get_value_rang (get_rang c1),get_value_rang(get_rang c5))
       
       else
       CarreX(get_value_rang(get_rang c2),get_value_rang(get_rang c1));;

let getReelFull cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
      if ((get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2) ) &&
       (get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) ) &&
	  (get_value_rang (get_rang c4) ) = (get_value_rang (get_rang c5) )) then
	FullX(get_value_rang (get_rang c1),get_value_rang (get_rang c4))
	  else
       FullX(get_value_rang (get_rang c3),get_value_rang  (get_rang c1));; 

let getReelBrelan cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
     if ((get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2) ) &&
	 (get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) ))  then
	 BrelanX(get_value_rang (get_rang c1),get_value_rang (get_rang c4), get_value_rang (get_rang c5))
       else if
       ((get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) ) &&
	   (get_value_rang (get_rang c3) ) = (get_value_rang (get_rang c4) )) then
	 BrelanX(get_value_rang (get_rang c2),get_value_rang (get_rang c1),get_value_rang (get_rang c5)) else
	 BrelanX(get_value_rang (get_rang c3), get_value_rang (get_rang c1),get_value_rang (get_rang c2));;

let getReelDoublePaire cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
    if ((get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2) ) &&
	(get_value_rang (get_rang c3) ) = (get_value_rang (get_rang c4) )) then
	Double_PaireX(get_value_rang(get_rang c1),get_value_rang(get_rang c3),get_value_rang (get_rang c5))
	  else if
       
       ((get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2) ) &&
	   (get_value_rang (get_rang c4) ) = (get_value_rang (get_rang c5) )) then
	    Double_PaireX(get_value_rang(get_rang c1),get_value_rang (get_rang c4),get_value_rang(get_rang c3))
	  else
	    Double_PaireX(get_value_rang(get_rang c2),get_value_rang(get_rang c4),get_value_rang(get_rang c1));;

let getReelPaire cm = match cm with
    | Comb(c1,c2,c3,c4,c5) ->
       if ( (get_value_rang (get_rang c1) ) = (get_value_rang (get_rang c2)) ) then
	 PaireX(get_value_rang(get_rang c1),get_value_rang(get_rang c3),get_value_rang(get_rang c4),get_value_rang(get_rang c5))
	 else   
	 if ((get_value_rang (get_rang c2) ) = (get_value_rang (get_rang c3) )) then
	     PaireX(get_value_rang(get_rang c2),get_value_rang(get_rang c1),get_value_rang(get_rang c4),get_value_rang(get_rang c5))
	   else
	   if ((get_value_rang (get_rang c3) ) = (get_value_rang (get_rang c4) )) then
	       PaireX(get_value_rang(get_rang c3),get_value_rang(get_rang c1),get_value_rang(get_rang c2),get_value_rang(get_rang c5))
	   else
	     PaireX(get_value_rang(get_rang c4),get_value_rang(get_rang c1),get_value_rang(get_rang c2),get_value_rang(get_rang c3));;

let getReelQuinteFlush cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
     (*special case *)
     if get_rang c1 = A && get_rang c2 = Point(5) && get_rang c3 = Point(4) && get_rang c4 = Point(3) && get_rang c5 = Point(2) &&
        get_couleur c1 = get_couleur c2 &&
       get_couleur c2 = get_couleur c3 &&
       get_couleur c3 = get_couleur c4 &&
       get_couleur c4 = get_couleur c5  then
       CarteNor(get_value_rang(get_rang c2),get_value_rang(get_rang c3),get_value_rang(get_rang c4),get_value_rang(get_rang c5),get_value_rang(get_rang c1))      
     else
       CarteNor(get_value_rang(get_rang c1),get_value_rang(get_rang c2),get_value_rang(get_rang c3),get_value_rang(get_rang c4),get_value_rang(get_rang c5)) ;;

let getReelCouleur cm = match cm with
    | Comb(c1,c2,c3,c4,c5) ->
      CarteNor(get_value_rang(get_rang c1),get_value_rang(get_rang c2),get_value_rang(get_rang c3),get_value_rang(get_rang c4),get_value_rang(get_rang c5)) ;;

let getReelSuite cm = match cm with
    | Comb(c1,c2,c3,c4,c5) ->
     (*special case*)
     if  get_rang c1 = A && get_rang c2 = Point(5) && get_rang c3 = Point(4) && get_rang c4 = Point(3) && get_rang c5 = Point(2) &&
       not (is_Quinte_flush cm) then
       CarteNor(get_value_rang(get_rang c2),get_value_rang(get_rang c3),get_value_rang(get_rang c4),get_value_rang(get_rang c5),get_value_rang(get_rang c1))  
     else
      CarteNor(get_value_rang(get_rang c1),get_value_rang(get_rang c2),get_value_rang(get_rang c3),get_value_rang(get_rang c4),get_value_rang(get_rang c5)) ;;

let getReelCarteHaute cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
     CarteNor(get_value_rang(get_rang c1),get_value_rang(get_rang c2),get_value_rang(get_rang c3),get_value_rang(get_rang c4),get_value_rang(get_rang c5)) ;;
       
    
let getTieCombination cm = match getCombTypeComb cm with
  | Quinte_Flush -> getReelQuinteFlush cm
  | Carre -> getReelCarre cm
  | Full -> getReelFull cm
  | Couleur -> getReelCouleur cm
  | Suite -> getReelSuite cm
  | Brelan -> getReelBrelan cm
  | Double_Paire -> getReelDoublePaire cm
  | Paire -> getReelPaire cm
  | Carte_Haute -> getReelCarteHaute cm;;
       


let getValueOfCombinationType cm = match getCombTypeComb cm with
  | Carte_Haute  -> 1
  | Paire        -> 2
  | Double_Paire -> 3
  | Brelan       -> 4
  | Suite        -> 5
  | Couleur      -> 6
  | Full         -> 7
  | Carre        -> 8
  | Quinte_Flush -> 9;;

(*compare two combination with the same type *)


let compareTwoCombinationWithTheSameLevel cm1 cm2 =  match cm1,cm2 with
  |  CarteNor(c1,c2,c3,c4,c5) , CarteNor(c1',c2',c3',c4',c5') ->
      if( c1 > c1' ) then 1
     else if c1 < c1' then -1 else
       if( c2 > c2' ) then 1 else
	 if ( c2 < c2' ) then -1 else
	   if( c3  > c3' ) then 1 else
	     if ( c3  < c3' ) then -1 else
	       if( c4  > c4' ) then 1 else
		 if ( c4 < c4' ) then -1 else
		   if( c5  > c5' ) then 1 else
		     if ( c5 < c5' ) then -1 else 0
		       
  | CarreX(a,b) , CarreX(c,d) -> if a > c then 1
    else if a < c then -1
    else if b > d then 1
    else if b < d then -1
    else 0
  | FullX(a,b) , FullX(c,d) -> if a > c then 1
    else if a < c then -1
    else if b > d then 1
    else if b < d then -1
    else 0
  | BrelanX(a,b,c) , BrelanX(d,e,f) -> if a > d then 1
    else if a < d then -1
    else if b > e then 1
    else if b < e then -1
    else if c > f then 1
    else if c < f then -1
      else 0
   | Double_PaireX(a,b,c) , Double_PaireX(d,e,f) -> if a > d then 1
    else if a < d then -1
    else if b > e then 1
    else if b < e then -1
    else if c > f then 1
    else if c < f then -1
      else 0
   | PaireX(a,b,c,d) , PaireX(x,y,z,t) -> if a > x then 1
     else if a < x then -1
     else if b > y then 1
     else if b < y then -1
     else if c > z then 1
     else if c < z then -1
     else if d > t then 1
     else if d < t then -1
       else 0
   | _, _ -> failwith "impossiblecases" 
    ;;

(* we sort the combination inside this function then we process *)

let compare_comb_aux cm1 cm2 =
  let c1' =(*(sortTheCombinationDecreasing cm1) in*) cm1 in
  let c2' =(*(sortTheCombinationDecreasing cm2) in*) cm2 in
  if ( getValueOfCombinationType c1' ) > ( getValueOfCombinationType c2' ) then
    1 else
    if ( getValueOfCombinationType c1' ) < ( getValueOfCombinationType c2' ) then
     -1
   else (compareTwoCombinationWithTheSameLevel (getTieCombination c1') (getTieCombination c2'));;

(*
get the best combination between two
like for example
A x x x 2
x x x 2 A
we get the best
*)
(*
let getTheBestCombinationAXXX2 cm = match cm with
  | Comb(c1,c2,c3,c4,c5) ->
     if (get_rang c1) = A && (get_rang c5) = Point(2) then
       let cm2 = Comb(c2,c3,c4,c5, Carte( A' ,get_couleur  c1) ) in
       let n = compare_comb_aux cm cm2 in
       if n = 1 then cm else cm2
     else cm;;
*)
let compare_comb cm1 cm2 =
  let c1 =(* getTheBestCombinationAXXX2*) cm1 in
  let c2 =(* getTheBestCombinationAXXX2*) cm2 in
  compare_comb_aux c1 c2;;



let compute_comb dn tb = match dn,tb with
  | Donne(c1,c2) , Table5(a1,a2,a3,a4,a5) ->
     (* there is no need give attention of the order because we're gonna
sort them anyway...
     *)
     let l = ref [] in
     l := sortTheCombinationDecreasing (Comb(c1,a1,a2,a3,a4))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,a1,a2,a3,a5))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,a1,a2,a4,a5))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,a1,a3,a4,a5))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,a2,a3,a4,a5))::(!l);

     l := sortTheCombinationDecreasing (Comb(c2,a1,a2,a3,a4))::(!l);
     l := sortTheCombinationDecreasing (Comb(c2,a1,a2,a3,a5))::(!l);
     l := sortTheCombinationDecreasing (Comb(c2,a1,a2,a4,a5))::(!l);
     l := sortTheCombinationDecreasing (Comb(c2,a1,a3,a4,a5))::(!l);
     l := sortTheCombinationDecreasing (Comb(c2,a2,a3,a4,a5))::(!l);

     l := sortTheCombinationDecreasing (Comb(a1,a2,a3,a4,a5))::(!l);
    
     l := sortTheCombinationDecreasing (Comb(c1,c2,a1,a2,a3))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a1,a2,a4))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a1,a2,a5))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a1,a3,a4))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a1,a3,a5))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a1,a4,a5))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a2,a3,a4))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a2,a3,a5))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a2,a4,a5))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a3,a4,a5))::(!l); !l
       
  |Donne(c1,c2) , Table4(a1,a2,a3,a4) -> (*just to avoid matching problemes exaustif*)
     let l = ref [] in
     l := sortTheCombinationDecreasing (Comb(c1,a1,a2,a3,a4))::(!l);
     l := sortTheCombinationDecreasing (Comb(c2,a1,a2,a3,a4))::(!l);
     
     l := sortTheCombinationDecreasing (Comb(c1,c2,a1,a2,a3))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a1,a2,a4))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a1,a3,a4))::(!l);
     l := sortTheCombinationDecreasing (Comb(c1,c2,a2,a3,a4))::(!l); !l
       
  |Donne(c1,c2) , Table3(a1,a2,a3) ->
     let l = ref [] in
     l := sortTheCombinationDecreasing ( Comb(c1,c2,a1,a2,a3))::(!l) ;!l
       ;;
     
    
(*     
compute_comb (Donne(c1,c2)) (Table(c3,c4,c5,c1',c2'));;
*)

let compare_comb_inverse c1 c2 = let n = compare_comb c1 c2 in
				 if n = 1 then -1 else
				   if n = -1 then 1
				   else 0;;

let rec get_the_max_combination_from_list l c1 = match l with
  | [] -> c1
  | c::tl ->
     if (compare_comb c c1) = 1 then get_the_max_combination_from_list tl c else
       get_the_max_combination_from_list tl c1;;




let compare_hands dn1 dn2 tb =
  let l1 = compute_comb dn1 tb in
  let l2 = compute_comb dn2 tb in
 (* let l1' =  (List.sort (compare_comb_inverse) (l1)) in
    let l2' =  (List.sort (compare_comb_inverse) (l2)) in *)
  compare_comb (get_the_max_combination_from_list l1 (List.hd l1)) (get_the_max_combination_from_list l2 (List.hd l2));;
  




(*probability*)


let getRangOfNumber n = match n with
  | 11 -> V
  | 12 -> D
  | 13 -> R
  | 14 -> A
  | p  -> Point(p);;

let getColorOfNumber n = match n with
| 1 -> Pique
| 2 -> Coeur
| 3 -> Carreau
| 4 -> Trefle
| n  -> failwith "ERROR";;

(**)
let get_proba_from_lists d1 d2 l' =
  let rec aux l c1 c2 c3 = match l with
    | [] -> c1,c2,c3
    | Comb(a1,a2,a3,a4,a5)::tl -> let n = compare_hands d1 d2 (Table5(a1,a2,a3,a4,a5)) in
			      if n = 1 then aux tl (c1+1) c2 c3 else
				if n = - 1 then aux tl c1 (c2+1) c3 else
				  aux tl c1 c2 (c3+1)
  in
  aux l' 0 0 0 ;;

let get_list_of_all_cards_without a b a' b' x y z  =
  let l = ref [] in
  for i = 1 to 4 do
    for j = 2 to 14 do

     if ( i <> get_value_couleur ( get_couleur a )  || j <> get_value_rang (get_rang a )) &&

	   ( i <> get_value_couleur ( get_couleur b )  || j <> get_value_rang (get_rang b )) &&

	   ( i <> get_value_couleur ( get_couleur a' )  || j <> get_value_rang (get_rang a' )) &&

	   ( i <> get_value_couleur ( get_couleur b' )  || j <> get_value_rang (get_rang b' )) &&

	   ( i <> get_value_couleur ( get_couleur x )  || j <> get_value_rang (get_rang x )) &&

	   ( i <> get_value_couleur ( get_couleur y )  || j <> get_value_rang (get_rang y )) &&

	   ( i <> get_value_couleur ( get_couleur z )  || j <> get_value_rang (get_rang z )) then

      l := Carte((getRangOfNumber j),(getColorOfNumber i))::(!l)
    done;    
  done;!l;;

let get_list_of_all_cards_without2 a b a' b' x y   =
  let l = ref [] in
  for i = 1 to 4 do
    for j = 2 to 14 do

     if ( i <> get_value_couleur ( get_couleur a )  || j <> get_value_rang (get_rang a )) &&

	   ( i <> get_value_couleur ( get_couleur b )  || j <> get_value_rang (get_rang b )) &&

	   ( i <> get_value_couleur ( get_couleur a' )  || j <> get_value_rang (get_rang a' )) &&

	   ( i <> get_value_couleur ( get_couleur b' )  || j <> get_value_rang (get_rang b' )) &&

	   ( i <> get_value_couleur ( get_couleur x )  || j <> get_value_rang (get_rang x )) &&

	   ( i <> get_value_couleur ( get_couleur y )  || j <> get_value_rang (get_rang y )) then

      l := Carte((getRangOfNumber j),(getColorOfNumber i))::(!l)
    done;    
    done;!l;;

let get_list_of_all_cards_without3 a b a' b' x    =
  let l = ref [] in
  for i = 1 to 4 do
    for j = 2 to 14 do

     if ( i <> get_value_couleur ( get_couleur a )  || j <> get_value_rang (get_rang a )) &&

	   ( i <> get_value_couleur ( get_couleur b )  || j <> get_value_rang (get_rang b )) &&

	   ( i <> get_value_couleur ( get_couleur a' )  || j <> get_value_rang (get_rang a' )) &&

	   ( i <> get_value_couleur ( get_couleur b' )  || j <> get_value_rang (get_rang b' )) &&

	   ( i <> get_value_couleur ( get_couleur x )  || j <> get_value_rang (get_rang x )) then

      l := Carte((getRangOfNumber j),(getColorOfNumber i))::(!l)
    done;    
    done;!l;;


let get_proba_simple_from_list l' =
  let rec aux l c = match l with
    | [] -> c
    | (d1,d2,t5)::tl -> if compare_hands d1 d2 t5 = 1 then aux tl (c+1) else aux tl c
  in
  aux l' 0;;
     
     



let proba_double d1 d2 t1 = match d1,d2,t1 with
  | Donne(a,b) , Donne(a',b') , Table5(x,y,z,t,e) -> let n = compare_hands d1 d2 t1 in
						     if n = 1 then (1. , 0.) else
						       if n = -1 then (0. , 1.)
							 else (0. , 0.)
  | Donne(a,b) , Donne(a',b') , Table4(x,y,z,t) ->
     let l = ref [] in
     for i = 1 to 4 do
       for j = 2 to 14 do
	 
	     if ( i <> get_value_couleur ( get_couleur a )  || j <> get_value_rang (get_rang a )) &&
	   ( i <> get_value_couleur ( get_couleur b )  || j <> get_value_rang (get_rang b )) &&
	   ( i <> get_value_couleur ( get_couleur a' )  || j <> get_value_rang (get_rang a' )) &&
	   ( i <> get_value_couleur ( get_couleur b' )  || j <> get_value_rang (get_rang b' )) &&
	   ( i <> get_value_couleur ( get_couleur x )  || j <> get_value_rang (get_rang x )) &&
	   ( i <> get_value_couleur ( get_couleur y )  || j <> get_value_rang (get_rang y )) &&
	   ( i <> get_value_couleur ( get_couleur z )  || j <> get_value_rang (get_rang z )) &&
	   ( i <> get_value_couleur ( get_couleur t )  || j <> get_value_rang (get_rang t)) then
	       l := (*sortTheCombinationDecreasing*)
	 (Comb(x,y,z,t,Carte((getRangOfNumber j),(getColorOfNumber i))))::(!l)

	 done;
     done;
     let p1 , p2 , p3= get_proba_from_lists d1 d2 !l in
     
     ( float_of_int p1 /. float_of_int (List.length !l)  ) , ( float_of_int p2 /. float_of_int (List.length !l)  )
       
  | Donne(a,b) ,Donne(a',b'), Table3(x,y,z)->
let l' = ref [] in
  let l = get_list_of_all_cards_without a b a' b' x y z in 
  for i = 0 to ((List.length l) -2) do
  	 for j = (i+1) to ((List.length l) -1) do
  	   l' := (*sortTheCombinationDecreasing*)
	 (Comb(x,y,z,(List.nth l i),(List.nth l j)))::(!l')

		done;
	done;
	let p1 , p2 , p3= get_proba_from_lists d1 d2 !l' in
     
	( float_of_int p1 /. float_of_int (List.length !l')  ) , ( float_of_int p2 /. float_of_int (List.length !l')  );;

(*proba simple*)

let proba_simple d1 t1 = match d1,t1 with
  | Donne(a,b) , Table5(c1,c2,c3,c4,c5) ->
     let l = get_list_of_all_cards_without a b c1 c2 c3 c4 c5 in
     let c = ref 0 in
     let length_total = ref 0 in 
     for i = 0 to ((List.length l) - 2) do
       for j = (i+1) to ((List.length l) - 1) do
	 
	 length_total := !length_total + 1;
        
	 if (compare_hands (Donne(a,b)) (Donne((List.nth l j) ,(List.nth l i))) (Table5(c1,c2,c3,c4,c5))) = 1 then
	   c := !c + 1;
	 
       done;
     done;(float_of_int !c /. float_of_int !length_total)
     

     
  | Donne(a,b) , Table4(c1,c2,c3,c4) ->
      let l = get_list_of_all_cards_without2 a b c1 c2 c3 c4 in
      let l' = ref [] in 
     for i = 0 to ((List.length l) - 3) do
       for j = (i+1) to ((List.length l) - 2) do
	 for k = (j+1) to ((List.length l) -1) do

	   
	   l' :=  ( (Donne(a,b)) , (Donne((List.nth l j) ,(List.nth l i)))  ,(Table5(c1,c2,c3,c4,(List.nth l k))))::(!l');
	   l' :=  ((Donne(a,b)),(Donne((List.nth l k) ,(List.nth l i))),(Table5(c1,c2,c3,c4,(List.nth l j))))::(!l');
	   l' :=  ((Donne(a,b)),(Donne((List.nth l j) ,(List.nth l k))),(Table5(c1,c2,c3,c4,(List.nth l i))))::(!l');
	 
	 done;
       done;
     done; let p = (get_proba_simple_from_list !l') in (float_of_int p /. float_of_int (List.length !l'))



     
  | Donne(a,b) , Table3(c1,c2,c3) ->
      let l = get_list_of_all_cards_without3 a b c1 c2 c3  in
      let l' = ref [] in 
     for i = 0 to ((List.length l) - 4) do
       for j = (i+1) to ((List.length l) - 3) do
	 for k = (j+1) to ((List.length l) -2) do
	   for g = (k+1) to ((List.length l) -1) do

	   
	   l' :=  ((Donne(a,b)),(Donne((List.nth l i) ,(List.nth l j))),(Table5(c1,c2,c3,(List.nth l k),(List.nth l g))))::(!l');
	   l' :=  ((Donne(a,b)),(Donne((List.nth l i) ,(List.nth l k))),(Table5(c1,c2,c3,(List.nth l j),(List.nth l g))))::(!l');
	   l' :=  ((Donne(a,b)),(Donne((List.nth l i) ,(List.nth l g))),(Table5(c1,c2,c3,(List.nth l j),(List.nth l k))))::(!l');
	   
	   l' :=  ((Donne(a,b)),(Donne((List.nth l j) ,(List.nth l k))),(Table5(c1,c2,c3,(List.nth l i),(List.nth l g))))::(!l');
	   l' :=  ((Donne(a,b)),(Donne((List.nth l j) ,(List.nth l g))),(Table5(c1,c2,c3,(List.nth l i),(List.nth l k))))::(!l');
	   
	   l' :=  ((Donne(a,b)),(Donne((List.nth l k) ,(List.nth l g))),(Table5(c1,c2,c3,(List.nth l i),(List.nth l j))))::(!l');

	   
	   done;	   
	 done;
       done;
     done; let p = (get_proba_simple_from_list !l') in (float_of_int p /. float_of_int (List.length !l'))
     ;;

(*readfrom file*)


let split_donne1 str =
  let rec aux pos l =
    if pos = String.length str then l else
    if (String.get str pos) = ' '  then aux (pos+1) (pos::l) else
      aux (pos+1) l
  in let l' = aux 0 [] in
     [String.sub str 0 ((List.hd l')) ;
     String.sub str ((List.hd l')+1) ((String.length str) -(List.hd l') -1)  ];;



let split_table str =
  let rec aux pos l =
    if pos = String.length str then l else
    if (String.get str pos) = ' '  then aux (pos+1) (pos::l) else
      aux (pos+1) l
  in let l' =List.rev (aux 0 []) in
     match List.length l' with
     | 2 ->
	
	[
      String.sub str 0 (List.hd l') ;
      String.sub str ((List.hd l')+1) ((List.nth l' 1) - (List.nth l' 0) -1) ;
      String.sub str ((List.nth l' 1) +1) ((String.length str) -(List.nth l' 1)-1)
	]
     | 3 ->
	[
      String.sub str 0 (List.hd l') ;
      String.sub str ((List.hd l')+1) ((List.nth l' 1) - (List.nth l' 0) -1) ;
      String.sub str ((List.nth l' 1) +1) ((List.nth l' 2) -(List.nth l' 1)-1);
      String.sub str ((List.nth l' 2) +1) ((String.length str) -(List.nth l' 2)-1)
	]
     | 4 ->
     [
      String.sub str 0 (List.hd l') ;
      String.sub str ((List.hd l')+1) ((List.nth l' 1) - (List.nth l' 0) -1) ;
      String.sub str ((List.nth l' 1) +1) ((List.nth l' 2) -(List.nth l' 1)-1);
      String.sub str ((List.nth l' 2) +1) ((List.nth l' 3) -(List.nth l' 2)-1);
      String.sub str ((List.nth l' 3) +1) ((String.length str) -(List.nth l' 3)-1)
     ]
			 | n -> failwith "impossible cases";;


let getRangCardFromString st = 
  if String.sub st 0 1 = "A" then A else
     if String.sub st 0 1 = "V" then V else
	if String.sub st 0 1 = "R" then R else
	  if String.sub st 0 1 = "D" then D else
	    
	    if String.sub st 0 2 = "10" then Point(10) else
	      if String.sub st 0 1 = "9" then Point(9) else
		if String.sub st 0 1 = "8" then Point(8) else
		  if String.sub st 0 1 = "7" then Point(7) else
		    if String.sub st 0 1 = "6" then Point(6) else
		      if String.sub st 0 1 = "5" then Point(5) else
			if String.sub st 0 1 = "4" then Point(4) else
			  if String.sub st 0 1 = "3" then Point(3) else
			     Point(2)
	    
let getColorCardFromString st = match String.length st with
  | 2 ->
     if String.sub st 1 1  = "t" then Trefle else
       Pique
  | 3 ->
     if String.sub st 1 2 = "ca" then Carreau else
       if String.sub st 1 2 = "co" then Coeur else
	 if String.sub st 2 1 = "p" then Pique else
	   Trefle
  | 4 -> if String.sub st 2 2 = "ca" then Carreau else Coeur
  | n -> failwith "impossible Case";;
  

(*reading*)

type dnx = Dnx of donne | None;;
  

 
let getTheDonne1 str =
  let l1 = split_donne1 str in
  let r1 = getRangCardFromString  (List.nth l1 0) in
  let r2 = getRangCardFromString  (List.nth l1 1) in
  let c1 = getColorCardFromString  (List.nth l1 0) in
  let c2 = getColorCardFromString  (List.nth l1 1) in
  Dnx(Donne(Carte(r1,c1), Carte(r2,c2)));;


let getTheDonne2 str =
  if str = "?" then None else
    getTheDonne1 str;;

let getTheTable str =
  let l1 = split_table str in
  match (List.length l1) with
  | 3 ->
     let r1 = getRangCardFromString  (List.nth l1 0) in
     let r2 = getRangCardFromString  (List.nth l1 1) in
     let r3 = getRangCardFromString  (List.nth l1 2 ) in
     let c1 = getColorCardFromString  (List.nth l1 0) in
     let c2 = getColorCardFromString  (List.nth l1 1) in
     let c3 = getColorCardFromString  (List.nth l1 2) in
     Table3(Carte(r1,c1),Carte(r2,c2),Carte(r3,c3))
  | 4 ->
     let r1 = getRangCardFromString  (List.nth l1 0) in
     let r2 = getRangCardFromString  (List.nth l1 1) in
     let r3 = getRangCardFromString  (List.nth l1 2 ) in
     let r4 = getRangCardFromString  (List.nth l1 3 ) in
     let c1 = getColorCardFromString  (List.nth l1 0) in
     let c2 = getColorCardFromString  (List.nth l1 1) in
     let c3 = getColorCardFromString  (List.nth l1 2) in
     let c4 = getColorCardFromString  (List.nth l1 3 ) in
     Table4(Carte(r1,c1),Carte(r2,c2),Carte(r3,c3),Carte(r4,c4))
  | 5 ->
     let r1 = getRangCardFromString  (List.nth l1 0) in
     let r2 = getRangCardFromString  (List.nth l1 1) in
     let r3 = getRangCardFromString  (List.nth l1 2 ) in
     let r4 = getRangCardFromString  (List.nth l1 3 ) in
     let r5 = getRangCardFromString  (List.nth l1 4 ) in
     let c1 = getColorCardFromString  (List.nth l1 0) in
     let c2 = getColorCardFromString  (List.nth l1 1) in
     let c3 = getColorCardFromString  (List.nth l1 2) in
     let c4 = getColorCardFromString  (List.nth l1 3 ) in
     let c5 = getColorCardFromString  (List.nth l1 4 ) in
     Table5(Carte(r1,c1),Carte(r2,c2),Carte(r3,c3),Carte(r4,c4),Carte(r5,c5))
  | n -> failwith "impossiblecases";;





let getFinalProbaX d1 d2 t = match d1,d2 with
  | Dnx(d) , None -> (proba_simple d t , 99.) (*just a trick*)
  | Dnx(d) , Dnx(d') -> proba_double d d' t
  | _,_ -> failwith "impossible case";;

let getTheProbaFinalFromFile couple = match couple with
  | (1.,_) -> print_string "joueur 1 gagne \n"
  | (_,1.) -> print_string "joueur 2 gagne \n"
  | (f,99.) -> print_string "joueur 1 :"; print_float f;print_string "\n"
  | (a,b)  -> print_string "joueur 1 : " ; print_float a;
    print_string "\n";print_string "joueur 2 : ";print_float b;print_string "\n";;

let file = Sys.argv.(1);;



let file = open_in file;;

let donne1 = input_line file;;
let donne2 = input_line file;;
let table1 = input_line file;;
let k = split_table table1;;
getColorCardFromString (List.nth k 2);;

let d1 = getTheDonne1 donne1;;
let d2 = getTheDonne2 donne2;;
let t1 = getTheTable table1;;

let px = getFinalProbaX d1 d2 t1;;
getTheProbaFinalFromFile px;;

