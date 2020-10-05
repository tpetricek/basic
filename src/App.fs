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
  list<int * string * Command>

let rec update (line, src, cmd) = function
  | [] -> [line, src, cmd]
  | (l, s, c)::p when line = l && cmd = Empty -> p
  | (l, s, c)::p when line = l -> (l, src, cmd)::p
  | (l, s, c)::p when line < l && cmd = Empty -> (l, s, c)::p
  | (l, s, c)::p when line < l -> (line, src, cmd)::(l, s, c)::p
  | (l, s, c)::p -> (l, s, c)::(update (line, src, cmd) p)

type State = 
  { Random : System.Random 
    Variables : Map<string, Value> 
    Screen : char[][]
    Cursor : int * int 
    Program : Program }

and Resumption = 
  | More of State * (State -> Resumption)
  | GetKey of State * (State -> string -> Resumption)
  | Done of State

let newLine (state & { Cursor = cl, _ }) = 
  if cl + 1 >= 25 then 
    { state with 
        Screen = Array.init 25 (fun l -> 
          if l = 24 then Array.create 40 ' '
          else state.Screen.[l+1] )
        Cursor = 24, 0 }
  else { state with Cursor = cl+1, 0 }

let backSpace (state & { Cursor = cl, cc }) = 
  let cl, cc =
    if cc - 1 < 0 then 
      if cl - 1 < 0 then 0, 39
      else cl - 1, 39
    else cl, cc-1
  { state with 
      Cursor = cl, cc
      Screen = Array.init 25 (fun l -> 
        if l = cl then Array.init 40 (fun c -> if c = cc then ' ' else state.Screen.[l].[c]) 
        else state.Screen.[l]) }

let print s state =
  let mutable state = state
  for c in s do 
    let cl, cc = state.Cursor
    if int c = 57491 then 
      state <- { state with Screen = Array.init 25 (fun _ -> Array.create 40 ' '); Cursor = 0, 0 }
    else
      let screen = Array.init 25 (fun l -> 
        if l = cl then Array.init 40 (fun i -> if i = cc then c else state.Screen.[l].[i]) 
        else state.Screen.[l]) 
      if cc + 1 >= 40 then      
        if cl + 1 >= 25 then 
          let screen = Array.init 25 (fun l -> 
            if l = 24 then Array.create 40 ' '
            else state.Screen.[l+1] )
          state <- { state with Cursor = 24, 0; Screen = screen }
        else state <- { state with Screen = screen; Cursor = cl + 1, 0 }
      else state <- { state with Screen = screen; Cursor = cl, cc + 1 }
  state
  
let rec evaluate state = function
  | Const v -> v
  | Variable(v) ->
      state.Variables.[v]
  | Binary(c, l, r) -> 
      match evaluate state l, evaluate state r with 
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
      match evaluate state arg with 
      | NumberValue arg -> NumberValue(float (state.Random.Next(int arg + 1)))
      | _ -> failwith "RND requires numeric argument"
  | Function("CHR$", [arg]) ->
      match evaluate state arg with 
      | NumberValue arg -> 
          StringValue(chrC64 (int arg))
      | _ -> failwith "CHR$ expects numeric argument"
  | Function("ASC", [arg]) ->
      match evaluate state arg with 
      | StringValue arg -> 
          NumberValue(float (int arg.[0]))
      | _ -> failwith "ASC expects string argument"
  | Function _ -> failwith "Only ASC, RND and CHR$ supported"

let format = function
  | StringValue s -> s
  | NumberValue n -> string n
  | BoolValue true -> "TRUE"
  | BoolValue false -> "FALSE"

let rec run (ln, _, cmd) state (program:Program) = 
  let next state = 
    if ln <> -1 then 
      match program |> List.tryFind (fun (l, _, _) -> l > ln) with 
      | Some ln -> run ln state program
      | _ -> Done state
    else Done state
  match cmd with 
  | Stop -> Done state
  | Empty -> Done state
  | List ->
      let mutable state = state
      for _, s, _ in program do 
        state <- state |> print s |> newLine
        printf "%s" s
      Done state
  | Run ->
      if not (List.isEmpty program) then 
        More (state, fun state -> run (List.head program) state program)
      else Done state
  | Goto lbl ->
      match program |> List.tryFind (fun (l, _, _) -> l = lbl) with 
      | Some ln -> More (state, fun state -> run ln state program)
      | None -> failwithf "Line %d not found in program: %A" lbl program
  | If(cond, cmd) ->
      if evaluate state cond = BoolValue true then 
        run (ln, "", cmd) state program
      else 
        next state
  | Get var ->
      GetKey (state, fun state s ->
        next { state with Variables = Map.add var (StringValue s) state.Variables }
      )
  | Assign(v, expr) ->
      next { state with Variables = Map.add v (evaluate state expr) state.Variables }
  | Poke(loc, v) ->
      match evaluate state loc, evaluate state v with 
      | NumberValue n, StringValue s when int n < 40*25->
          let screen = Array.init 25 (fun l ->
            if l = int n/40 then Array.init 40 (fun c -> if c = int n%40 then s.[0] else state.Screen.[l].[c])
            else state.Screen.[l] )
          next { state with Screen = screen }
      | _ -> failwith "wrong arguments for POKE"
  | Print(e, nl) ->
      let state = print (format (evaluate state e)) state 
      let state = if nl then newLine state else state
      next state


let initial =
  { Random = System.Random()
    Variables = Map.empty
    Screen = Array.init 25 (fun _ -> Array.create 40 ' ')
    Cursor = 0, 0 
    Program = [] }

type Message = 
  | Input of string
  | Tick of Resumption

  (*

let rec finish = function
  | Done state -> running <- false; printScreen()
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
      *)
let input (src:string) state = 
  match parseInput (tokenizeString src) with 
  | Some(ln), cmd -> 
      Done({ state with Program = update (ln, src, cmd) state.Program })
  | None, cmd -> 
      run (-1, "", cmd) state state.Program
  
type RunAgent() = 
  let printScreen = Event<_>()
  let agent = MailboxProcessor.Start(fun inbox ->    

    let rec accept src state = async {
      let state = print src state |> newLine
      printScreen.Trigger(state.Screen)
      return! run (input src state) }

    and ready state = async { 
      match! inbox.Receive() with
      | Input src ->
          return! accept src state
      | Tick _ -> 
          return! ready state } // should not happen

    and run resump = async {
      match resump with 
      | Done state -> 
          printScreen.Trigger(state.Screen)
          return! ready state
      | More(state, f) ->
          printScreen.Trigger(state.Screen)
          let op = async {
            do! Async.Sleep(10)
            let r = f state
            //printScreen.Trigger(state.Screen)
            inbox.Post(Tick r) }
          Async.StartImmediate(op)
          return! running state }

    and running state = async {
      match! inbox.Receive() with 
      | Tick resump -> return! run resump
      | Input src -> return! accept src state }

    ready initial )

  member x.Input(src) = agent.Post(Input src)
  member x.PrintScreen = printScreen.Publish

let agent = RunAgent() 

(* 
module Browser = 
  open Browser.Dom

  let outpre = document.getElementById("out")
  let mutable instr = ""
  let mutable running = false
  let mutable inpbuf = []

  let printScreen state =
    let s = 
      [| for l in 0 .. 24 do
           for c in 0 .. 39 do yield state.Screen.[l].[c]
           yield '\n' |]
    outpre.innerText <- System.String(s)

  let rec finish = function
    | Done state -> running <- false; printScreen()
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
*)  
type Input = 
  | Code of string
  | Rem of string

let inputs = 
  [|
  Rem "Start with simple hello world..."
  Code "PRINT \"HELLO WORLD\""

  Rem "Create the famous maze"
  Code "10 PRINT CHR$(147);"
  Code "20 PRINT CHR$(205.5 + RND(1));"
  Code "30 GOTO 20"
  Code "RUN"
  Code "10"
  Code "20"
  Code "30"

  Rem "Create a ball moving right"
  Code "PRINT CHR$(147);"
  Code "1000 X=0"
  Code "2000 POKE X CHR$(32)"
  Code "2010 X=X+1"
  Code "2020 POKE X CHR$(209)"
  Code "2030 GOTO 2000"
  Code "RUN"

  Rem "Create a ball bouncing left right"
  Code "PRINT CHR$(147);"
  Code "LIST"
  Code "1010 DX=1"
  Code "1010 Y=0"
  Code "1020 DX=1"
  Code "1030 DY=1"
  Code "2010 X=X+DX"
  Code "2020 Y=Y+DY"
  Code "2030 IF X=40 THEN DX=0-1"
  Code "2040 IF X=40 THEN X=38"
  Code "2050 IF X<0 THEN DX=1"
  Code "2060 IF X<0 THEN X=2"
  Code "2200 POKE ((Y*40)+X) CHR$(209)"
  Code "2210 GOTO 2000"
  Code "RUN"

  Rem "Oops, try again with DY=0"    
  Code "1030 DY=0"
  Code "RUN"
  
  Rem "Add bouncing from top and bottom"
  Code "PRINT CHR$(147);"
  Code "LIST"
  Code "2000 POKE ((Y*40)+X) CHR$(32)"
  Code "2070 IF Y=25 THEN DY=0-1"
  Code "2080 IF Y=25 THEN Y=23"
  Code "2090 IF Y<0 THEN DY=1"
  Code "2100 IF Y<0 THEN Y=2"
  Code "RUN"

  Rem "Oops, try again with DY=1"
  Code "1030 DY=1"
  Code "RUN"

  Rem "Figure out how to handle input"    
  Code "PRINT CHR$(147);"
  Code "10 K$=\"\""
  Code "20 GET$ K$"                             
  Code "30 IF K$=\"\" THEN GOTO 20"
  Code "40 PRINT ASC(K$)"
  Code "50 STOP"

  Rem "Type some key e.g. up arrow"    
  Code "RUN"
  Rem "Type some key e.g. down arrow"    
  Code "RUN"

  Code "1040 P=10"
  Code "2500 K$=\"\""
  Code "2510 K=0"
  Code "2520 GET$ K$"
  Code "2530 IF K$<>\"\" THEN K=ASC(K$)"
  Code "2540 IF K=145 THEN P=P-1"
  Code "2550 IF K=17 THEN P=P+1"
  Code "2560 POKE ((P-1)*40) CHR$(32)"
  Code "2561 POKE ((P+0)*40) CHR$(182)"
  Code "2562 POKE ((P+1)*40) CHR$(182)"
  Code "2563 POKE ((P+2)*40) CHR$(182)"
  Code "2564 POKE ((P+3)*40) CHR$(182)"
  Code "2565 POKE ((P+4)*40) CHR$(182)"
  Code "2566 POKE ((P+5)*40) CHR$(32)"
  Code "2570 GOTO 2500"

  // "P=10"
  // "GOTO 2500"
  Code "2560 IF P>0 THEN POKE ((P-1)*40) CHR$(32)"
  Code "2566 IF P<20 THEN POKE ((P+5)*40) CHR$(32)"
  Code "2551 IF P<0 THEN P=0"
  Code "2552 IF P>20 THEN P=20"

  // "GOTO 1000"
  Code "2050 IF X<1 THEN DX=1"
  Code "2060 IF X<1 THEN X=3"
  // "GOTO 1000"

  Code "2210"
  Code "2570 GOTO 2000"
  Code "2021 IF (X=0) AND (Y<P) THEN GOTO 3000"
  Code "2022 IF (X=0) AND (Y>(P+4)) THEN GOTO 3000"
  Code "3000 STOP"
    
  Code "10"
  Code "20"
  Code "30"
  Code "40"
  Code "50"

  Code "900 PRINT CHR$(147)"
  Code "3000 PRINT CHR$(147);"
  Code "3010 S=0"
  Code "3030 S=S+1"
  Code "3040 PRINT \"\""
  Code "3050 IF S<11 THEN GOTO 3030"
  Code "3060 PRINT \"               GAME OVER\""

  |] 
   
   //|> List.fold (fun p l -> input l p) []

//  printScreen ()
    (*
  prog <- 
    []
    |> input "10 PRINT CHR$(205.5 + RND(1));"
    |> input "20 GOTO 10"
    //|> input "RUN"
    //*)

  //prog <- inputs |> Seq.fold (fun p l -> match l with Code c when c <> "RUN" -> input c p | _ -> p) []
//(*
open Browser.Dom

let outpre = document.getElementById("out")

let printScreen (screen:_[][]) =
  let s = 
    [| for l in 0 .. 24 do
         for c in 0 .. 39 do yield screen.[l].[c]
         yield '\n' |]
  outpre.innerText <- System.String(s) 

agent.PrintScreen.Add(printScreen)
printScreen initial.Screen

window.onload <- fun _ ->
  let h = float (100 * inputs.Length) + window.innerHeight
  window.document.body.setAttribute("style", "height:" + string h + "px")
  let mutable lastLine = -1
  window.onscroll <- fun e ->
    let currentLine = int window.scrollY / 100
    if currentLine > lastLine then
      for l in lastLine + 1 .. currentLine do
        match inputs.[l] with
        | Rem s ->
            document.getElementById("rem").innerText <- s
        | Code c ->
            agent.Input(c)
      lastLine <- currentLine
      //*)
