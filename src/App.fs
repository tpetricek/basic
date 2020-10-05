#if INTERACTIVE
#else
module App
#endif

module X = 
  let chrC64 arg = 
    "X"

let chrC64 arg = 
  let e = Browser.Dom.document.createElement("textarea")
  e.innerHTML <- "&#" + string (57344 + int arg) + ";"
  e.innerText


type Token =
  | Equals
  | Semicolon 
  | Ident of string
  | Operator of char list
  | Bracket of char
  | Number of float
  | String of string

let str rcl = System.String(Array.rev(Array.ofSeq rcl))
let isLetter c = (c >= 'A' && c <= 'Z') || c = '$'
let isOp c = "+-*/<>".Contains(string c)
let isBracket c = "()".Contains(string c)
let isNumber c = (c >= '0' && c <= '9')

let rec tokenize toks = function
  | c::cs when isLetter c -> ident toks [c] cs
  | c::cs when isNumber c -> number toks [c] cs
  | c::cs when isOp c -> operator toks [c] cs
  | c::cs when isBracket c -> tokenize ((Bracket c)::toks) cs
  | '='::cs -> tokenize (Equals::toks) cs
  | ';'::cs -> tokenize (Semicolon::toks) cs
  | '"'::cs -> strend toks [] cs
  | ' '::cs -> tokenize toks cs
  | [] -> List.rev toks
  | cs -> failwithf "Cannot tokenize: %s" (str (List.rev cs))

and strend toks acc = function
  | '"'::cs -> tokenize (String(str acc)::toks) cs
  | c::cs -> strend toks (c::acc) cs
  | [] -> failwith "End of string not found"

and ident toks acc = function
  | c::cs when isLetter c -> ident toks (c::acc) cs
  | '$'::input -> tokenize (Ident(str ('$'::acc))::toks) input
  | input -> tokenize (Ident(str acc)::toks) input

and operator toks acc = function
  | c::cs when isOp c -> operator toks (c::acc) cs
  | input -> tokenize (Operator(List.rev acc)::toks) input

and number toks acc = function
  | c::cs when isNumber c -> number toks (c::acc) cs
  | '.'::cs when not (List.contains '.' acc) -> number toks ('.'::acc) cs
  | input -> tokenize (Number(float (str acc))::toks) input

let tokenizeString s = tokenize [] (List.ofSeq s)

//tokenizeString "10 PRINT \"{CLR/HOME}\""
//tokenizeString "20 PRINT CHR$(205.5 + RND(1))"
//tokenizeString "40 GOTO 20"

type Value =
  | StringValue of string
  | NumberValue of float
  | BoolValue of bool

type Expression = 
  | Variable of string
  | Const of Value
  | Binary of char list * Expression * Expression
  | Function of string * Expression list

type Command = 
  | Print of Expression * bool
  | Goto of int
  | Poke of Expression * Expression
  | Assign of string * Expression
  | If of Expression * Command
  | Get of string
  | Stop
  | List 
  | Run 
  | Empty

let rec parseBinary left = function
  | (Operator o)::toks -> 
      let right, toks = parseExpr toks
      Binary(o, left, right), toks
  | (Ident "AND")::toks -> 
      let right, toks = parseExpr toks
      Binary(['&'], left, right), toks
  | (Ident "OR")::toks -> 
      let right, toks = parseExpr toks
      Binary(['|'], left, right), toks
  | Equals::toks -> 
      let right, toks = parseExpr toks
      Binary(['='], left, right), toks
  | toks -> left, toks

and parseExpr = function
  | (String s)::toks -> parseBinary (Const(StringValue s)) toks
  | (Number n)::toks -> parseBinary (Const(NumberValue n)) toks
  | (Bracket '(')::toks -> 
      let expr, toks = parseExpr toks
      match toks with 
      | Bracket ')'::toks -> parseBinary expr toks
      | _ -> failwith "Missing closing bracket"
  | (Ident i)::(Bracket '(')::toks ->
      let rec loop args toks = 
        match toks with 
        | (Bracket ')')::toks -> List.rev args, toks
        | _ -> 
            let arg, toks = parseExpr toks 
            loop (arg::args) toks
      let args, toks = loop [] toks
      parseBinary (Function(i, args)) toks
  | (Ident v)::toks -> parseBinary (Variable v) toks
  | toks -> failwithf "Parsing expr failed. Unexpected: %A" toks

//parseExpr (tokenizeString "(X=0) AND (Y<P)")

let rec parseInput toks = 
  let line, toks = 
    match toks with
    | (Number ln)::toks -> Some(int ln), toks
    | _ -> None, toks
  match toks with 
  | [] -> line, Empty
  | (Ident "LIST")::[] -> line, List
  | (Ident "STOP")::[] -> line, Stop
  | (Ident "RUN")::[] -> line, Run
  | (Ident "GOTO")::(Number lbl)::[] -> line, Goto(int lbl)
  | (Ident "GET$")::(Ident var)::[] -> line, Get(var)
  | (Ident "POKE")::toks -> 
      let arg1, toks = parseExpr toks
      let arg2, toks = parseExpr toks
      if toks <> [] then failwithf "Parsing POKE failed. Unexpected: %A" toks
      line, Poke(arg1, arg2)      
  | (Ident "IF")::toks -> 
      let arg1, toks = parseExpr toks
      match toks with 
      | (Ident "THEN")::toks ->
          let _, cmd = parseInput toks
          line, If(arg1, cmd)      
      | _ ->
          failwithf "Parsing IF failed. Expected THEN."
  | (Ident "PRINT")::toks -> 
      let arg, toks = parseExpr toks
      let nl = 
        if toks = [Semicolon] then false
        elif toks <> [] then failwithf "Parsing PRINT failed. Unexpected: %A" toks
        else true
      line, Print(arg, nl)
  | (Ident id)::Equals::toks ->
      let arg, toks = parseExpr toks
      if toks <> [] then failwithf "Parsing = failed. Unexpected: %A" toks
      line, Assign(id, arg)
  | _ -> failwithf "Parsing command failed. Unexpected: %A" toks

//parseInput (tokenizeString "10 PRINT \"{CLR/HOME}\"")
//parseInput (tokenizeString "20 PRINT CHR$(205.5 + RND(1))")
//parseInput (tokenizeString "30 GOTO 20")

type Program = 
  list<int * Command>

let rec update (line, src, cmd) = function
  | [] -> [line, src, cmd]
  | (l, s, c)::p when line = l && cmd = Empty -> p
  | (l, s, c)::p when line = l -> (l, src, cmd)::p
  | (l, s, c)::p when line < l && cmd = Empty -> (l, s, c)::p
  | (l, s, c)::p when line < l -> (line, src, cmd)::(l, s, c)::p
  | (l, s, c)::p -> (l, s, c)::(update (line, src, cmd) p)

let rnd = System.Random()
let variables = System.Collections.Generic.Dictionary<string, Value>()
let mutable screen = Array.init 25 (fun _ -> Array.create 40 ' ')
let mutable cursor = 0, 0

let newLine () = 
  let cl, cc = cursor
  cursor <- 
    if cl + 1 >= 25 then 
      screen <- Array.init 25 (fun l -> Array.init 40 (fun c -> if l = 24 then ' ' else screen.[l+1].[c]))
      24, 0
    else cl+1, 0

let backSpace () = 
  let cl, cc = cursor
  cursor <- 
    if cc - 1 < 0 then 
      if cl - 1 < 0 then 0, 39
      else cl - 1, 39
    else cl, cc-1
  let cl, cc = cursor
  screen.[cl].[cc] <- ' '

let print s =
  for c in s do 
    let cl, cc = cursor
    if int c = 57491 then 
      screen <- Array.init 25 (fun _ -> Array.create 40 ' ')
      cursor <- 0, 0
    else
      screen.[cl].[cc] <- c
      cursor <- 
        if cc + 1 >= 40 then      
          if cl + 1 >= 25 then 
            screen <- Array.init 25 (fun l -> Array.init 40 (fun c -> if l = 24 then ' ' else screen.[l+1].[c]))
            24, 0
          else cl + 1, 0
        else cl, cc + 1
      
let rec evaluate = function
  | Const v -> v
  | Variable(v) ->
      variables.[v]
  | Binary(c, l, r) -> 
      match evaluate l, evaluate r with 
      | BoolValue l, BoolValue r -> 
          match c with 
          | ['&'] -> BoolValue (l && r)
          | ['|'] -> BoolValue (l || r)
          | _ -> failwithf "Operator %A not supported" c
      | StringValue l, StringValue r -> 
          match c with 
          | ['='] -> BoolValue (l = r)
          | ['<'; '>'] -> BoolValue (l <> r)
          | _ -> failwithf "Operator %A not supported" c
      | NumberValue l, NumberValue r -> 
          match c with 
          | ['+'] -> NumberValue (l + r)
          | ['-'] -> NumberValue (l - r)
          | ['*'] -> NumberValue (l * r)
          | ['/'] -> NumberValue (l / r)
          | ['>'] -> BoolValue (l > r)
          | ['<'] -> BoolValue (l < r)
          | ['='] -> BoolValue (l = r)
          | _ -> failwithf "Operator %A not supported" c
      | _ -> failwith "Binary expects matching arguments"
  | Function("RND", [arg]) ->
      match evaluate arg with 
      | NumberValue arg -> NumberValue(float (rnd.Next(int arg + 1)))
      | _ -> failwith "RND requires numeric argument"
  | Function("CHR$", [arg]) ->
      match evaluate arg with 
      | NumberValue arg -> 
          StringValue(chrC64 (int arg))
      | _ -> failwith "CHR$ expects numeric argument"
  | Function("ASC", [arg]) ->
      match evaluate arg with 
      | StringValue arg -> 
          NumberValue(float (int arg.[0]))
      | _ -> failwith "ASC expects string argument"
  | Function _ -> failwith "Only ASC, RND and CHR$ supported"

let format = function
  | StringValue s -> s
  | NumberValue n -> string n
  | BoolValue true -> "TRUE"
  | BoolValue false -> "FALSE"

type Resumption = 
  | More of (unit -> Resumption)
  | GetKey of (string -> Resumption)
  | Done

let rec run (ln, _, cmd) program = 
  let next () = 
    if ln <> -1 then 
      match program |> List.tryFind (fun (l, _, _) -> l > ln) with 
      | Some ln -> run ln program
      | _ -> Done
    else Done 
  match cmd with 
  | Stop -> Done
  | Empty -> Done
  | List ->
      for _, s, _ in program do 
        print s
        printf "%s" s
        newLine ()
      Done
  | Run ->
      if not (List.isEmpty program) then 
        More (fun () -> run (List.head program) program)
      else Done
  | Goto lbl ->
      match program |> List.tryFind (fun (l, _, _) -> l = lbl) with 
      | Some ln -> More (fun () -> run ln program)
      | None -> failwithf "Line %d not found in program: %A" lbl program
  | If(cond, cmd) ->
      if evaluate cond = BoolValue true then 
        run (ln, "", cmd) program
      else 
        next ()
  | Get var ->
      GetKey (fun s ->
        variables.[var] <- StringValue s
        next () )
  | _ ->
    match cmd with 
    | Stop | Empty | Get _ | List | Run | Goto _ | If _ -> failwith "should not happen"
    | Assign(v, expr) ->
        variables.[v] <- evaluate expr
    | Poke(loc, v) ->
        match evaluate loc, evaluate v with 
        | NumberValue n, StringValue s when int n < 40*25->
            screen.[int n/40].[int n%40] <- s.[0]
        | _ -> failwith "wrong arguments for POKE"
    | Print(e, nl) ->
        print (format (evaluate e))
        if nl then newLine ()
    next ()


module Console = 
  let printScreen () =
    (*try 
      System.Console.CursorLeft <- 0
      System.Console.CursorTop <- 0
    with _ -> ()*)
    for l in 0 .. 24 do
      for c in 0 .. 39 do        
        System.Console.Write(screen.[l].[c])
      System.Console.WriteLine()

  let rec finish = function
    | Done -> printScreen()
    | GetKey _ -> failwith "TBD"
    | More f -> 
        let r = f () 
        printScreen()
        finish r

  let input src program = 
    match parseInput (tokenizeString src) with 
    | Some(ln), cmd -> update (ln, src, cmd) program
    | None, cmd -> run (-1, "", cmd) program |> finish; program
    
  let nada () = 
    []
    |> input "2200 POKE ((Y*40)+X) CHR$(209)"
    |> input "10 X = 0"
    |> input "20 PRINT X"
    |> input "30 X = X + 1"
    |> input "40 IF X < 30 THEN GOTO 20"
    |> input "RUN"

module Browser = 
  open Browser.Dom

  let outpre = document.getElementById("out")
  let mutable prog = [] 
  let mutable instr = ""
  let mutable running = false
  let mutable inpbuf = []

  let printScreen () =
    let s = 
      [| for l in 0 .. 24 do
           for c in 0 .. 39 do yield screen.[l].[c]
           yield '\n' |]
    outpre.innerText <- System.String(s)

  let rec finish = function
    | Done -> running <- false; printScreen()
    | GetKey f -> 
        let s = 
          if List.isEmpty inpbuf then "" else 
            let h = inpbuf.Head
            inpbuf <- inpbuf.Tail
            h
        let r = f s
        printScreen()
        finish r
    | More f ->    
        window.setTimeout
          ( (fun () -> 
                if running then
                  let r = f ()
                  printScreen()
                  finish r), 50 )
        |> ignore

  let input src program = 
    match parseInput (tokenizeString src) with 
    | Some(ln), cmd -> update (ln, src, cmd) program
    | None, cmd -> 
        running <- true
        run (-1, "", cmd) program |> finish 
        program

  window.onkeypress <- fun e ->
    if not running && e.ctrlKey = false && e.key.Length = 1 then
      print e.key
      printScreen ()
      instr <- instr + e.key

  window.onkeyup <- fun e ->  
    if e.ctrlKey && e.keyCode = 67. then
      running <- false
    if running then       
      if e.keyCode = 38. then inpbuf <- (char 145).ToString() :: inpbuf // up
      elif e.keyCode = 40. then inpbuf <- (char 17).ToString() :: inpbuf // down
      elif e.key.Length = 1 then inpbuf <- e.key.ToUpper() :: inpbuf
      //printfn "%A %A" e.key e.keyCode
    else
    if e.keyCode = 8. then
      if instr.Length > 0 then instr <- instr.Substring(0, instr.Length-1)
      backSpace ()      
      printScreen ()
    if e.keyCode = 13. then
      newLine()
      try 
        prog <- input instr prog
        printf "%s" instr
      with e ->
        printf "Something went wrong: %s" e.Message
      instr <- ""
      
  prog <- 
   [
    "1000 X=0"
    "2000 POKE X CHR$(32)"
    "2010 X=X+1"
    "2020 POKE X CHR$(209)"
    "2030 GOTO 2000"

    "1010 DX=1"
    "1010 Y=0"
    "1020 DX=1"
    "1030 DY=1"
    "2010 X=X+DX"
    "2020 Y=Y+DY"
    "2030 IF X=40 THEN DX=0-1"
    "2040 IF X=40 THEN X=38"
    "2050 IF X<0 THEN DX=1"
    "2060 IF X<0 THEN X=2"
    "2200 POKE ((Y*40)+X) CHR$(209)"
    "2210 GOTO 2000"

    "1030 DY=0"
    "2000 POKE ((Y*40)+X) CHR$(32)"

    "2070 IF Y=25 THEN DY=0-1"
    "2080 IF Y=25 THEN Y=23"
    "2090 IF Y<0 THEN DY=1"
    "2100 IF Y<0 THEN Y=2"

    "1030 DY=1"

    "10 K$=\"\""
    "20 GET$ K$"                             
    "30 IF K$=\"\" THEN GOTO 20"
    "40 PRINT ASC(K$)"
    "50 STOP"

    "1040 P=10"
    "2500 K$=\"\""
    "2510 K=0"
    "2520 GET$ K$"
    "2530 IF K$<>\"\" THEN K=ASC(K$)"
    "2540 IF K=145 THEN P=P-1"
    "2550 IF K=17 THEN P=P+1"
    "2560 POKE ((P-1)*40) CHR$(32)"
    "2561 POKE ((P+0)*40) CHR$(182)"
    "2562 POKE ((P+1)*40) CHR$(182)"
    "2563 POKE ((P+2)*40) CHR$(182)"
    "2564 POKE ((P+3)*40) CHR$(182)"
    "2565 POKE ((P+4)*40) CHR$(182)"
    "2566 POKE ((P+5)*40) CHR$(32)"
    "2570 GOTO 2500"

    // "P=10"
    // "GOTO 2500"
    "2560 IF P>0 THEN POKE ((P-1)*40) CHR$(32)"
    "2566 IF P<20 THEN POKE ((P+5)*40) CHR$(32)"
    "2551 IF P<0 THEN P=0"
    "2552 IF P>20 THEN P=20"

    // "GOTO 1000"
    "2050 IF X<1 THEN DX=1"
    "2060 IF X<1 THEN X=3"
    // "GOTO 1000"

    "2210"
    "2570 GOTO 2000"
    "2021 IF (X=0) AND (Y<P) THEN GOTO 3000"
    "2022 IF (X=0) AND (Y>(P+4)) THEN GOTO 3000"
    "3000 STOP"
    
    "10"
    "20"
    "30"
    "40"
    "50"

    "900 PRINT CHR$(147)"
    "3000 PRINT CHR$(147);"
    "3010 S=0"
    "3030 S=S+1"
    "3040 PRINT \"\""
    "3050 IF S<11 THEN GOTO 3030"
    "3060 PRINT \"               GAME OVER\""

   ] |> List.fold (fun p l -> input l p) []

  printScreen ()
    (*
  prog <- 
    []
    |> input "10 PRINT CHR$(205.5 + RND(1));"
    |> input "20 GOTO 10"
    //|> input "RUN"
    //*)