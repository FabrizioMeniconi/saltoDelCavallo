
exception NotFound;;
exception BadCell;;
exception BadN;;

(* creazione di un nuovo tipo scacchiera *)
type 'a scacchiera = Scacchiera of (int -> ((int * int) -> (int * int) list));;

(* data la dimensione della scacchiera e la cella restituisce la lista delle mosse possibili *)
let mossePossibili n (x,y)= 
	if n > 0
	then if x <= n && y <= n && x > 0 && y > 0 
		then let mosse =  [(x-2,y+1);(x-2,y-1);(x+2,y+1);(x+2,y-1);(x-1,y-2);(x-1,y+2);(x+1,y-2);(x+1,y+2)]
		in let rec semplifica = function
			[] -> []
			|(x,y)::rest -> if x>n || y>n || x<=0 || y<=0
				then semplifica rest
				else (x,y)::semplifica rest
		in semplifica mosse
		else raise BadCell
	else raise BadN;;

(* stampa una lista *)
let rec print_list = function 
	[] -> ()
	| (x,y)::l -> print_string "(" ; print_int x; print_string "," ; print_int y; print_string ")" ; print_string " " ; print_list l;;

(* stampa una lista di liste *)
let rec print_listlist = function
	[] -> ()
	| x::l -> print_string "[" ; print_list x ; print_string "]"; print_listlist l;;

print_newline ();;

(*funzione successore per il grafo dei cammini *)
let succpath (Scacchiera succ) n path =
	let rec aux = function
		[] -> []
		|b::rest -> if List.mem b path 
			then aux rest
			else (b::path)::(aux rest)
in aux (succ n (List.hd path));;


(* cerca un cammino chiuso che percorra tutte le celle du una scacchiera per il salto del cavallo *)
let trovaCammino (Scacchiera succ) n succhpath start = 

	(* verifica se il cammino preso in analisi corrisponde a quello che sto gercando *)
	let verifica path =
		if (List.length path = n*n) && List.mem (List.hd(List.rev path)) (succ n (List.hd path))
			then true
			else false
		
	(* scorre il grafo dei cammini *)
	in let rec search = function 
		[] -> raise NotFound 
		| path::rest -> print_list path;  print_newline ();
			if verifica path then path  
				else search (rest @ succpath (Scacchiera succ) n path)
	in search [[start]];;


let rec leggiCasella n =
     let i = read_int () 
     in 
       if i <= n && i > 0 then 
         i
       else 
         let () = 
	     let () = print_string "Il numero deve essere compreso tra 1 e "; print_int n; print_string ".";
             in print_newline () 

         in leggiCasella n;;

let () = print_string "Inserisci la dimensione della scacchiera: ";;

let n = read_int ();;

let () = print_string "Inserisci la riga: ";; 

let riga = leggiCasella n;;

let () = print_string "Inserisci la colonna: ";;

let colonna = leggiCasella n;;

let () = print_string "le mosse possibili dalla casella ("; print_int riga;  print_string ","; print_int colonna; print_string ") sono: ";;

print_listlist (succpath (Scacchiera mossePossibili) n [(riga,colonna)]);;

let () = print_newline ();;


print_list (trovaCammino (Scacchiera mossePossibili) n succpath (riga, colonna));;