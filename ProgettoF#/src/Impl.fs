
(*
 * SharpSolver - Progetto di Programmazione e Calcolo a.a. 2018-19
 * Impl.fsi: implementazioni degli studenti
 * (C) 2018 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)

module SharpSolver.Impl

open Absyn
open Prelude
open System


//let rationalize (x : float) : rational = raise (NotImplementedException ())
let rationalize (x : float) : rational = 
    let get_num_and_den x =                                   //funzione per ottenere il numeratore e il denominatore di un rational 
        let a = x.ToString().Split([|'.';','|])                       //divido la parte intera dalla parte decimale dopo averlo convertito in string
        let rec per n =                       
            match n with
            |0 -> 1
            |_ -> 10 * per (n-1)                              //moltiplico per "elevato alla 10" per la lunghezza della parte decimale
        in ((int)(a.[0]+a.[1]), (per (a.[1].Length)))
                                                              //per il numeratore ho unito la parte intera con la decimale
                                                              //per il denominatore ho usato la funzione "per"

    match x with                              
    |0. -> rational.Zero                      
    |1. -> rational.One
    |_ -> let (num,den) = (get_num_and_den x) in rational (num,den)   
        

//let monomial_degree (m : monomial) : int = raise (NotImplementedException ())
let monomial_degree (m : monomial) : int = 
     match m with
     | Monomial ( coeff , deg ) -> deg                        //mi restituisce il grado del monomio

//let monomial_negate (m : monomial) : monomial = raise (NotImplementedException ())
let monomial_negate (m : monomial) : monomial =
    match m with
    | Monomial ( coeff , deg ) -> Monomial ( - coeff , deg )  //nego il coefficente per negare il monomio 

//let polynomial_degree (p : polynomial) : int = raise (NotImplementedException ())
let polynomial_degree (p : polynomial) : int =
    match p with
    |Polynomial monoList ->                                   // decostruiamo il polinomio ottenendo una lista di monomi
        let list = List.map (fun (Monomial (coeff, deg)) -> (coeff, deg)) monoList in      //da lista di monomi a lista di rational*int
        List.maxBy snd list |> snd                            // restituisco il degree di valore più alto 

         


//let polynomial_negate (p : polynomial) : polynomial = raise (NotImplementedException ())
let polynomial_negate (p : polynomial) : polynomial =
    match p with 
    |Polynomial monomialList ->Polynomial(List.map monomial_negate monomialList)   
    
    //richiamo la funzione monomial_negate usando List.map per negare tutto il polinomio 


    
    
//let normalized_polynomial_degree (np : normalized_polynomial) : int = raise (NotImplementedException ())
let normalized_polynomial_degree (np : normalized_polynomial) : int = 
    match np with 
    |NormalizedPolynomial np ->    // decostruiamo un normalized_polynomial ottenendo un rational array
        if np.Length = 0 then 0    // se la lunghezze di np = 0 allora il grado è 0              
        else (np.Length - 1)       // altrimenti restutiamo la lunghezza - 1 ovvero il grado più alto 

    
//let normalize (p : polynomial) : normalized_polynomial = raise (NotImplementedException ())
let normalize (p : polynomial) : normalized_polynomial = 
    let polyToOrderedMonolist = 
        match p with 
        |Polynomial monomialList ->                                                            //decostruiamo p da polynomial a monomial list 
                    let list = (List.sortBy (fun (Monomial(_, deg)) -> deg) monomialList) in   //crea una lista ordinata di monomi in base al grado
                    list |> List.map (fun (Monomial (coeff, deg)) -> (coeff, deg))             //restituisce un rational(coeff) * int(deg) list  
    in
    let rec sumCoeffs (lst :( rational * int)list)  (outlst :( rational * int)list) =                         //outlst è un accumulatore per l'output
        match lst with
        |[] -> outlst                                                   
        |[(coeff1, deg1)] -> outlst@[(coeff1, deg1)]          
        |(coeff1, deg1)::(coeff2, deg2)::xs ->
            if (deg1 = deg2) then                           //controllo se i gradi di due monomi sono uguali
                if ((coeff1+coeff2)<> rational.Zero) then sumCoeffs (((coeff1+coeff2), deg1)::xs) (outlst)  //se la somma dei coefficenti è diverso da 0, viene aggiunta alla lista da scorrere
                else sumCoeffs (xs) (outlst)                // altrimenti si scorre la lista senza aggiungere nulla
            else sumCoeffs ((coeff2, deg2)::xs) (outlst@[(coeff1, deg1)])  //in tutti gli altri casi aggiungo il secondo monomio alla lista da scorrere, e il primo nell'output
            
    in
    let rec normalizeOrderedMonolist (orderedMonolist:( rational * int)list) (count : int) : rational list =   
        match orderedMonolist with 
        |[]->[]                             
        |(coeff,deg)::xs when deg = count       //se il grado è = al contatore 
                                -> coeff::(normalizeOrderedMonolist xs (count + 1))    //aggiungo il coefficiente alla lista in output e aumento di 1 il count
        |(coeff,deg)::xs-> rational.Zero::(normalizeOrderedMonolist ((coeff,deg)::xs) (count + 1))  //altrimenti aggiungo un rational.Zero (0,0)
    in 
   
    let normalized_poly_array = (normalizeOrderedMonolist (sumCoeffs polyToOrderedMonolist []) 0)|> List.toArray  //creo alla fine un array
    
    in NormalizedPolynomial(normalized_poly_array)

    



//let derive (p : polynomial) : polynomial = raise (NotImplementedException ())
let rec derive (p : polynomial) : polynomial =       //funzione per calcolare la derivata di un polinomio già normalizzato
        let normArr = normalize p in                 //normArr restituisce un p normalized_polynomial
        match normArr with
        |NormalizedPolynomial norm ->                //decostruiamo e otteniamo un rational array
            let normLst = norm |> Array.toList in    //viene convertito in lista
            let rec deriveAux (lst : rational list) (count : int) (out:monomial list) =
                match lst with
                |[] -> out
                |coeff::xs ->
                    if coeff = rational.Zero||count=0 then deriveAux xs (count+1) out                // controllo se il coefficiente o il count = 0, se sono uguali non aggiungo nulla
                    else if count=1 then deriveAux xs (count+1) (out@[Monomial(coeff, 0)])           // se il count = 1 allora aggiungo all'output il coefficiente e il grado 0
                    else deriveAux xs (count+1) (out@[Monomial(coeff*(rational)count, count-1)])     // in tutti gli altri casi moltiplico il coefficiente per il count e diminuisco il grado di 1
            in (Polynomial(List.rev(deriveAux normLst 0 [])))                                     





//let reduce (e : expr) : polynomial = raise (NotImplementedException ())
let reduce (e : expr) : polynomial = 
    let rec aux e =
        match e with 
        |Poly e -> e                            //se l'input è Poly, lo restituisco com'è
        |Derive e -> derive (aux e)             // altrimenti lo derivo richiamando aux ricorsivamente
    in aux e



//let solve0 (np : normalized_polynomial) : bool = raise (NotImplementedException ())
let solve0 (np : normalized_polynomial) : bool = 
    match np with 
    |NormalizedPolynomial np -> np = [||] || np.[0] = rational.Zero     //controllo se l'array è vuoto o se il coefficiente è = 0



//let solve1 (np : normalized_polynomial) : rational = raise (NotImplementedException ())
let solve1 (np : normalized_polynomial) : rational =
    match np with 
    |NormalizedPolynomial np ->
        if(np.[0] = rational.Zero ) then rational.Zero                   //controllo se il coefficiente è 0
        else (-np.[0])/np.[1]                                            //funzione che risolve l'equazione di primo grado




//let solve2 (np : normalized_polynomial) : (float * float option) option = raise (NotImplementedException ())
let solve2 (np : normalized_polynomial) : (float * float option) option =
    match np with
    |NormalizedPolynomial n ->
        let delta = (rational.Pow (n.[1], 2)) - (rational 4 * n.[2] * n.[0]) in                       // delta = b**2 - 4ac
        if delta < rational.Zero then None
        else if delta = rational.Zero then 
            let x = (float)((-n.[1]) / (rational 2 * n.[2])) in                                       // -b / 2a
            Some (x, None)
        else 
            let x1 = (((float)(-n.[1])) + (rational.Sqrt delta)) / (float)(rational 2 * n.[2]) in     // -b +- sqrt b**2 - 4ac   / 2a
            let x2 = (((float)(-n.[1])) - (rational.Sqrt delta)) / (float)(rational 2 * n.[2]) in
            Some (x1, Some x2)



