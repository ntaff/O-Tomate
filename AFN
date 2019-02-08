(*"*********************************************************)
(********************* Nicolas *****************************)
(******************* Taffoureau ****************************)
(******************** 8/02/2019 ****************************)
(***********************************************************)

	(* Bibliothèque sur les listes *) 
	
		let rec appartient = function 
		(a,b::l)-> if a=b then true else appartient(a,l)
		|(_,[])-> false;;
		(* appartient : 'a * 'a list -> bool = <fun> *)

		let rec union l = function 
		(a::l2)-> if appartient(a,l) then union l l2 else a:: (union l l2)
		| []->l;;
		(* union : 'a list -> 'a list -> 'a list = <fun> *)

		let rec long = function
		(_::l)->1+long(l)
		|[]-> 0;;

	(* Bibliothèque sur les chaînes de caractères *)

		let string_of_char = String.make 1 ;;

		let tetec = function
		| "" -> failwith "Erreur : chaine vide"
		| s -> s.[0] ;;
		(* val tetec : string -> char = <fun> *)

		let tetes = fun s -> string_of_char (tetec(s));;

		let reste = function 
		| "" -> failwith "Erreur : chaine vide"
		| s -> String.sub s 1  ((String.length s) - 1 ) ;;
		(* val reste : string -> string = <fun> *)

(* ******************************************************************* *)
		

	(* Un type pour les AFN *)

			type etatN = {acceptN : bool ; tN : char -> int list};;

			type afn = {sigmaN: char list; (* l'alphabet *)
					nN: int; (* Q est l'ensemble {1..N} *)
					initN: int list; (* les états initiaux *)
					eN : int -> etatN};; 

						
		(* Exemples d'automates finis non deterministes *)


			let an1  = {sigmaN= ['a';'b'] ; nN = 3; initN = [1] ; 
				eN = function	
					1 -> {acceptN = false ;
						  tN = function 
							   'a'->[2] }
					|2 -> {acceptN = true ;
						  tN = function 
							   'a'->[2] 
							   |'b'-> [2;3] }		   
					|3 -> {acceptN = false ;
						  tN = function 
							   'a'->[2]
							   |'b'->[3]   }					   
			};;

			let an2  = {sigmaN= ['a';'b'] ; nN = 3; initN = [1;3] ; 
				eN = function	
					1 -> {acceptN = false ;
						  tN = function 
							   'a'->[2]
							   |'b'->[2]}
					|2 -> {acceptN = true ;
						  tN = function 
							   'a'->[3] 
							           }		   
					|3 -> {acceptN = false ;
						  tN = function 
							   'a'->[1]
							   |'b'->[1;2]}					   
			};;
			
		(* 2. Lecture d’un mot par un AFN *)		
		(* val acceptN : afn -> string -> bool = <fun> *)	
		
		(* On prend un afn et un mot *)
		let acceptN (automate : afn) (mot : string) =
			(* On défini une fonction intermédiaire qui parcourt le mot  *)
			let rec aux (etat : int) (b : bool) (mot : string) = match mot with
				"" -> b; (* Si le mot est vide on renvoie si l'état courant est acceptant ou non *)
				|w -> try (let state = (automate.eN etat).tN (tetec w) (* On applique la transition avec la première lettre du mot courant *)
								in aux2 state (reste w)) with _ -> false (* On vérifie avec la seconde fonction intermédiaire aux2 que la lettre est dans la liste des transitions possibles, sinon on renvois false *)
				(* On défini une fonction intermédiaire qui parcourt les états *)				
			and aux2 (etat : int list) (mot : string) = match etat with
					(a::reste) -> aux a (automate.eN a).acceptN mot || aux2 reste mot (* Pour chaque transition possible on véfifie que la premiere lettre du mot (aux) est reconnu. *)
					|[] -> false (* Si il n'y a plus ou pas de transition possible on renvoit false *)	
				in
					let etat_initial = automate.initN (* On recupère la liste des états initiaux *)
						in
					aux2 etat_initial mot;;	(* On commence bien sûr le parcours par les états initiaux *)	
		
		(* Jeu de tests *)
		acceptN an1 "abba";; (* ----> - : bool = true *)
		acceptN an1 "bbbbaaaaabababababababbbababababababbbbabaaaabbababababababababababababbababbaababbabbbabaabbabababaabbbbbbabbaba";; (* ----> - : bool = false *)
		acceptN an1 "abbbbaaaaabababaabbababababababababababababbababbbbbaaaabababbababbaababbabbbabaabbabababaabbbbbbabbab";; (* ----> - : bool = true *)
		acceptN an1 "test";; (* ----> - : bool = false *)
			
			
		acceptN an2 "abba";; (* ----> - : bool = false *)
		acceptN an2 "bab";; (* ----> - : bool = true *)
		acceptN an2 "bababaaaa";; (* ----> - : bool = true *)
		acceptN an2 "bbbbaaaaabababababababbbababababababbbbabaaaabbababababababababababababbababbaababbabbbabaabbabababaabbbbbbabbaba";; (* ----> - : bool = false *)
		acceptN an2 "abbbbaaaaabababaabbababababababababababababbababbbbbaaaabababbababbaababbabbbabaabbabababaabbbbbbabbab";; (* ----> - : bool = false *)
		acceptN an2 "test";; (* ----> - : bool = false *)	
