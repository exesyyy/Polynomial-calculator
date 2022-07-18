(*
 * SharpSolver - Progetto di Programmazione e Calcolo a.a. 2018-19
 * Main.fs: console e codice main
 * (C) 2018 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)

module SharpSolver.Main

open Microsoft.FSharp.Text.Lexing
open Absyn
open System
open Prelude
open Microsoft.FSharp.Text
open Impl


// funzioni di logging e printing
//

let hout hd fmt =
    if not <| String.IsNullOrWhiteSpace hd then
        printf "[%s]%s" hd (new String (' ', max 1 (Config.prefix_max_len - String.length hd)))
        stdout.Flush ()
    printfn fmt

let chout col hd fmt =
    let c = Console.ForegroundColor
    Console.ForegroundColor <- col
    Printf.kprintf (fun s -> hout hd "%s" s; Console.ForegroundColor <- c) fmt

let out fmt = hout "" fmt
let cout col fmt = chout col "" fmt

let norm fmt = chout ConsoleColor.Yellow "norm" fmt
let redux fmt = chout ConsoleColor.Magenta "redux" fmt
let sol fmt = chout ConsoleColor.Green "sol" fmt
let ident fmt = chout ConsoleColor.Green "ident" fmt

let degree fmt = chout ConsoleColor.Blue "degree" fmt
//let derivate fmt = chout ConsoleColor.Blue "derivate" fmt

let error fmt = chout ConsoleColor.Red "error" fmt   

// interprete dei comandi e delle espressioni
//

let interpreter_loop () =
    while true do
        printf "\n%s" Config.prompt_prefix          // stampa il prompt
        stdout.Flush ()                             // per sicurezza flusha lo stdout per vedere la stampa del prompt senza end-of-line
        let input = Console.ReadLine ()             // leggi l'input scritto dall'utente
        let lexbuf = LexBuffer<_>.FromString input  // crea un lexbuffer sulla stringa di input

        // funzione locale per il pretty-printing degli errori
        let localized_error msg =
            let tabs = new string (' ', Config.prompt_prefix.Length + lexbuf.StartPos.Column)
            let cuts = new string ('^', let n = lexbuf.EndPos.Column - lexbuf.StartPos.Column in if n > 0 then n else 1)
            cout ConsoleColor.Yellow "%s%s\n" tabs cuts
            error "error at %d-%d: %s" lexbuf.StartPos.Column lexbuf.EndPos.Column msg 
        
        // blocco con trapping delle eccezioni
        try
            let line = Parser.line Lexer.tokenize lexbuf    // invoca il parser sul lexbuffer usando il lexer come tokenizzatore
            #if DEBUG
            hout "absyn" "%+A" line
            hout "pretty" "%O" line
            //hout "degree" "%+A" line
            #endif

            // interpreta la linea in base al valore di tipo line prodotto dal parsing
            match line with
            | Cmd "help" ->
                out "%s" Config.help_text

            | Cmd ("quit" | "exit") ->
                out "%s" Config.exit_text
                exit 0

            // TODO: se volete supportare altri comandi, fatelo qui (opzionale)
            
            | Cmd s -> error "unknown command: %s" s    // i comandi non conosciuti cadono in questo caso

            // TODO: aggiungere qui sotto i pattern per i casi Expr ed Equ con relativo codice per, rispettivamente, normalizzare espressioni e risolvere equazioni
            |Expr e ->     
              
                let pol = reduce e
                redux "%O" pol                                            //stampo redux
                norm "%O" (normalize pol)                                 //stampo il polinomio normalizzato 
                degree "%i"(normalized_polynomial_degree(normalize pol))  //stampo il grado del polinomio normalizzato
            
            |Equ (e1,e2) ->
                
                let addEqu (e1:polynomial,e2:polynomial):polynomial =
                    match (e1,(polynomial_negate e2)) with                //il secondo polinomio viene negato 
                    |(Polynomial e1,Polynomial e2) ->                     //scompatto i polinomi in 2 liste di monomi
                        (Polynomial(e1@e2))                               //unisco le due liste ottenendo un polinomio unico    

                let outPuts (e1:polynomial,e2:polynomial) =                 
                    let normPol = normalize (addEqu(e1, e2))              //creo una variabile che contiene l'unione dei due polinomi normalizzati

                    redux "%O = %O" e1 e2
                    norm  "%O = 0" normPol
                    degree "%i"(normalized_polynomial_degree normPol)
                    match (normalized_polynomial_degree normPol) with    //controllo il grado del polinomio normalizzato
                    |0-> 
                        ident "%b" (solve0 normPol)                      //se è 0 richiamo solve 0, ovvero l'identità con output bool                  
                    |1->
                        sol "x = %O" (solve1 normPol)                    //se è 1 richiamo solve 1, e trovo l'unica soluzione di x                 
                    |2->
                        let solved = (solve2 normPol) in                 //se è 2, richiamo solve 2, e stampo in base al tipo di output 
                        match solved with 
                        |None                -> sol "No real solution"   
                        |Some(x,None)        -> sol "x = %O" x
                        |Some(x1,Some x2)    -> sol "x1 = %O vel x2 = %O" x1 x2

                    |_-> failwith "Cubic or higher equation not implemented"
                
                outPuts(reduce (e1),reduce (e2))                       
                
            | _ -> raise (NotImplementedException (sprintf "unknown command or expression: %O" line))
                   
        // gestione delle eccezioni
        with LexYacc.ParseErrorContextException ctx ->
                let ctx = ctx :?> Parsing.ParseErrorContext<Parser.token>
                localized_error (sprintf "syntax error%s" (match ctx.CurrentToken with Some t -> sprintf " at token <%O>" t | None -> ""))

           | Lexer.LexerError msg -> localized_error msg 

           | :? NotImplementedException as e -> error "%O" e
        
           | e -> localized_error e.Message


// funzione main: il programma comincia da qui
//

[<EntryPoint>]
let main _ = 
    let code =
        try
            interpreter_loop ()                 // chiama l'interprete
            0                                   // ritorna il codice di errore 0 (nessun errore) al sistema operativo se tutto è andato liscio
        with e -> error "fatal error: %O" e; 1  // in caso di eccezione esce con codice di errore 1
    #if DEBUG
    Console.ReadKey () |> ignore                // aspetta la pressione di un tasto prima di chiudere la finestra del terminare 
    #endif
    code


