(* ******************************************************************* *)
 (* Bibliothèque sur les listes *)  
let rec appartient = function 
(a,b::l)-> if a=b then true else appartient(a,l)
|(_,[])-> false;;
appartient : 'a * 'a list -> bool = <fun>

let rec union l = function 
(a::l2)-> if appartient(a,l) then union l l2 else a:: (union l l2)
| []->l;;
union : 'a list -> 'a list -> 'a list = <fun>

let rec long = function
(_::l)->1+long(l)
|[]-> 0;;

(* Fin de la biliothèque sur les listes *)
(* ******************************************************************* *)
(*				Bibliothèque sur les chaînes de caractères 					 *)


let string_of_char = String.make 1 ;;

let tetec = function
| "" -> failwith "Erreur : chaine vide"
| s -> s.[0] ;;
(*val tetec : string -> char = <fun>*)

let tetes = fun s -> string_of_char (tetec(s));;

let reste = function 
| "" -> failwith "Erreur : chaine vide"
| s -> String.sub s 1  ((String.length s) - 1 ) ;;
(*val reste : string -> string = <fun>*)


(* 		Un type pour les AFN 	*)

type etatN = {acceptN : bool ; tN : char -> int list};;

type afn = {sigmaN: char list; (* l'alphabet *)
			nN: int; (* Q est l'ensemble {1..N} *)
			initN: int list; (* les états initiaux *)
			eN : int -> etatN};; 
			

				(* Les exemples *)
				
				
let an1  = {sigmaN= ['a';'b'] ; nN = 4; initN = [1] ; 
			eN = function	
			    1 -> {acceptN = false ;
				      tN = function
					       'é'->[2] }
				|2 -> {acceptN = false ;
				      tN = function
					       'é'->[1;3] }		   
				|3 -> {acceptN = true ;
				      tN = fun 
					       'é'->[1;4] }
				|4 -> {acceptN = false ;
				      tN = function 
					       'é'->[1] }						   
		};;

let an2  = {sigmaN= ['a';'b'] ; nN = 4; initN = [1] ; 
			eN = function 	
			    1 -> {acceptN = false ;
				      tN = function 
							'a' -> [2] }
				|2 -> {acceptN = false ;
				      tN = function 
					       'é' -> [3] }		   
				|3 -> {acceptN = true ;
				      tN = function
					      'b' -> [2]
						| 'é' -> [4]}	
				|4 -> {acceptN = false ;
						tN = function
							'b' -> [4]
							| 'a' -> [2]}
		};;	

		
		(* PasTransition *)
exception PasTransition ;;
		
let transitN = fun (aut, i, c) ->
	try (aut.eN(i)).tN(c) 
	with Match_failure _-> raise PasTransition;;
		
(* ********************************************************* *)		
	(* Déterminer la clotûre de chaque état *)

let clotAux = fun aut i ->
   try transitN(aut,i,'é') 
   with  PasTransition -> [i];;
   
   
let clotAux2 = fun a -> 
   let rec aux = function 
       (i::l)-> union (clotAux  a i ) (union (aux l) [i]) 
       |[]->  []
   in aux;;
(*clotAux2 : AFN -> int list -> int list = <fun>*)

clotAux2 an2 [1];;
(*- : int list = [1; 2]*)

let rec clotAux3  = fun a l -> let l2 = clotAux2 a l in
       if long l = long l2 then l else clotAux3 a l2;;
(*clotAux3 : AFN -> int list -> int list = <fun>*)

clotAux3 an2 [1];;
(*- : int list = [3; 1; 4; 2]*)

(* et enfin *)
let cloture = fun a i -> clotAux3 a [i];;
(*cloture : AFN -> int -> int list = <fun>*)


(* Tests *)
cloture an2 1 ;; (* - : int list = [1] *)
cloture an2 2 ;; (* - : int list = [2; 4; 3] *)
cloture an2 3 ;; (* - : int list = [3; 4] *)
cloture an2 4 ;; (* - : int list = [4] *)

(* check *)

(*cloture an2 1;;
- : int list = [3 ; 1 ; 4 ; 2]*)

(* Transitions étendues *)
(* val etend : afn -> int -> char -> int list = <fun> *)	
let etend a e c = let rec auxetend l = match l with
	(l1::l2) -> (try ((a.eN l1).tN c)@(auxetend l2) with _ -> auxetend l2)
	|[] -> []
						in auxetend (cloture a e);;
	
(* Vérifie si dans la cloture p donnée un des état est acceptant *)
(* val etat_acceptant : afn -> int -> bool = <fun> *)
let etat_acceptant a e = let rec aux_accept l = match l with
	(l1::l2) -> (a.eN l1).acceptN || aux_accept l2
	|[] -> false
							in aux_accept (cloture a e);;
							
etat_acceptant an2 2;; (* - : bool = true *)
etat_acceptant an2 1;; (* - : bool = false *)


let rec elimineEps aut = (let rec aux_elimine n aut2 =
								0 -> aut
								|n -> (let rec aux2_elimine alpha aut2 = match alpha with
											(lettre::alphabet)-> aux_elimine alphabet (etend aut2 n lettre)
											|[] -> aut2 in aux2_elimine aut.sigmaN aut2)
								in
									let aut2 = {sigmaN=aut.sigmaN; nN=aut.nN; initN=aut.initN;
													eN=function 
														0->{acceptN=false;
															tN=function 
																'o'->[0] }}
									in aux_elimine aut.nN aut2);;
