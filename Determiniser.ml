(* ----------------------- Bibliothèque sur les listes -------------------------- *)
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


(* -------------------- RAPPELS ------------------ *)

														
(* Représentation d'un automate déterministe *)
type etat = {accept : bool ; t : char -> int};;
 
type afd = {sigma : char list ; (* l'alphabet *)
			n : int ; (* Q est l'ensemble des états de 1 à n *)
			init : int ; (* l'état initial *)
			e : int -> etat};;
							
(* Représentation des automates non-déterministes *)
type etatN = {acceptN : bool ; tN : char -> int list};;
		
type afn = {sigmaN: char list; (* l'alphabet *)
			nN: int; (* Q est l'ensemble {1..N} *)
			initN: int list; (* les états initiaux *)
			eN : int -> etatN};;

			
(* Fonctions de transition *)
exception  PasTransition ;;

let transitN = function (aut, i, c) ->
	try (aut.eN(i)).tN(c) 
	with Match_failure _-> raise PasTransition;;
	
(* Liste des états Acceptants *)
let etatsAcceptants auto = 
	let rec auxi auto = function 
		| 0 -> []
		| i -> if (auto.eN(i)).acceptN then i::(auxi auto (i-1)) else auxi auto (i-1) 
	in auxi auto (auto.nN) ;;
(* val etatsAcceptants : afn -> int list = <fun> *)


(* Exemple *)

let an1  = {sigmaN= ['a';'b'] ; nN = 3; initN = [1] ; 
			eN = function	
			    1 -> {acceptN = false ;
				      tN = function 
					       'a'->[1;2]
						   |'b'-> [1]}
				|2 -> {acceptN = false ;
				      tN = function  
						   'b'-> [3] }		   
				|3 -> {acceptN = true ;
				      tN = function 
					       'a'->[3]
						   |'b'->[3]   }	
				
		};;


		
		
(* Engendré l'ensemble des parties non vide de 1,...,n *)				
let rec aux  x = function
	| [] -> []
	| a :: q -> (x::a) :: (aux x q) ;;
		
let rec parties = function
	| 0 -> [[]]
	| n -> parties(n-1)@( aux n (parties(n-1))) ;;

let rec enleve = function 
	| [] -> []
	| x :: q -> if long(x) = 0 then q else x :: (enleve q) ;;
	

(* Trier les listes *)	
let rec insere x = function 
	| [] -> [x] 
	| a :: q -> if x < a then x :: a ::q else a :: (insere x q) ;;

let rec tri = function 
	| [] -> []
	| x :: q -> insere x (tri q) ;;
	
(*Multi-tri*)	
let rec multi_tri = function
	| [] -> []
	| x :: q -> (tri x)::(multi_tri q) ;;
	
(* Finalement, la liste des nouveaux états de l'automate : *)

let nouveauxetats = function n -> multi_tri (enleve(parties n)) ;;
(*val nouveauxetats : int -> int list list = <fun>
# nouveauxetats 3 ;;
- : int list list = [[1]; [2]; [1; 2]; [3]; [1; 3]; [2; 3]; [1; 2; 3]] *)



let rec qui_est_en_position (x : 'a)(l : 'a list) = match l with
	a::reste when a = x -> 0
	|a::reste -> 1 + qui_est_en_position x reste;;

let rec qui_est_a_la_position (n : int) (l : 'a list) = match l with
	x::r when n = 1 -> x
	|x::r -> qui_est_a_la_position (n-1) r;;
	
	
	
let rec aux 

let autoDet (aut : afn) = 
	
