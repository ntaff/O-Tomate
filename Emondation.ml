(*                      Bibliothèque sur les listes                             *)  

let rec appartient = function 
(a,b::l)-> if a=b then true else appartient(a,l)
|(_,[])-> false;;
(*appartient : 'a * 'a list -> bool = <fun>*)

let rec union l = function 
(a::l2)-> if appartient(a,l) then union l l2 else a:: (union l l2)
| []->l;;
(*union : 'a list -> 'a list -> 'a list = <fun>*)

let rec enleve a = function
 x::q -> if x = a then q else x::(enleve a q)
 | [] -> [] ;;

let rec intersection l1 = function
	| [] -> []
	| a :: l2 -> if appartient(a,l1) then a::(intersection (enleve a l1) l2) else intersection l1 l2 ;;

let rec long = function
(_::l)->1+long(l)
|[]-> 0;;						

							
(* Représentation des automates non-déterministes *)
type etatN = {acceptN : bool ; tN : char -> int list};;
		
type afn = {sigmaN: char list; (* l'alphabet *)
			nN: int; (* Q est l'ensemble {1..N} *)
			initN: int list; (* les états initiaux *)
			eN : int -> etatN};;
			

(* Fonction transitN *)
exception  PasTransition ;;

let transitN = fun (aut, i, c) ->
	try (aut.eN(i)).tN(c) 
	with Match_failure _-> raise PasTransition;;
	
(* Automate exemple *)
let an1  = {sigmaN= ['a';'b'] ; nN = 6; initN = [1] ; 
			eN = function	
			    1 -> {acceptN = false ;
				      tN = function 
					       'a'->[3]}
				|2 -> {acceptN = true ;
				      tN = function 
					       'a'->[2] 
						   |'b'-> [1] }		   
				|3 -> {acceptN = true ;
				      tN = function 
					       'a'->[4]
						   |'b'->[5]   }	
				|4 -> {acceptN = true ;
					   tN = function
							'a' -> [3]}
				|5 -> {acceptN = false ;
						tN = function 
							'a' -> [5]
							|'b' -> [6]}
				|6 -> {acceptN = false ;
						tN = function 
							'a' -> [5]
							|'b' -> [6]}
		};;

(* Alphabet augmenté *)	
let sigmaAug = ['a';'b';'é'];;

(* I. Déterminer les états accessibles *)


let rec eAccessibles (aut : afn) (i : int) (s : char list) = match s with
	p::s -> try 
				let l = transitN(aut, i, p) in union l (eAccessibles aut i s)
			with _ -> [i];;
(* Tests *)
eAccessibles an1 1 sigmaAug ;; (*- : int list = [1 ; 3]*)
eAccessibles an1 3 sigmaAug ;; (*- : int list = [3 ; 5 ; 4]*)

		
let rec eAccessiblesListe (aut : afn) (li : int list) = match li with
	(i::li) -> union (eAccessibles aut i sigmaAug) (eAccessiblesListe aut li)
	| [] -> [];;

(* Tests *)
eAccessiblesListe an1 [2 ;6] ;; (*- : int list = [6 ; 5 ; 1 ; 2]*)


let rec auxParcours (aut : afn) (li : int list) = match li with
	li -> let suc = eAccessiblesListe aut li in if long li = long suc then suc
												else union suc (auxParcours aut suc);;
												
(* Tests *)
auxParcours an1 [2 ; 3] ;; (* - : int list = [6; 3; 5; 4; 1; 2] *)
auxParcours an1 [1] ;; (* - : int list = [6; 5; 4; 1; 3] *)


let rec etatsAccessibles (aut : afn) = match aut with
	aut -> let init = aut.initN in auxParcours aut init;;
	
etatsAccessibles an1 ;; (*- : int list = [6 ; 5 ; 4 ; 1 ; 3]*)


(* II. Construction de l’automate inverse, recherche des sommets coaccessibles *)

let autoVide = function
	aut -> let (auto : afn) = {sigmaN= aut.sigmaN ; 
								nN = aut.nN ; 
								initN = aut.initN ; 
								eN = function c -> { acceptN = (let z = aut.eN(c) in z.acceptN) ; 
									tN = function	
										_ -> raise PasTransition }} in auto;;

(* Tests *)
let test1 = autoVide an1 ;; (* val test1 : afn = {sigmaN = ['a' ; 'b'] ; nN = 6 ; initN = [1] ; eN = <fun>} *)
