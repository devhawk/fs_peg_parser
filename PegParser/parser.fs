#light 
#nowarn "40"

//---------------------------------------------------------------------------------------------
//Simple list, string, array utility functions 

///L2S == List to String
//I'm not sure, but I'll bet converting the list to and array before converting it to a string
//is pretty inefficent. At some point, I'll probably want to re-write this
let rec L2S (input : char list) = new System.String(List.to_array input)

//---------------------------------------------------------------------------------------------
//AST Types

///AST Type for Range production
type Range =
| Single of char
| Dual of char * char
    with
    override this.ToString() = 
        match this with
        | Single x -> sprintf "Range.Single (%A)" x
        | Dual (x,y) -> sprintf "Range.Dual (%A,%A)" x y

///AST Type for Suffix production
type Suffix =
| Question
| Star
| Plus
    with
    override this.ToString() = 
        match this with
        | Question -> "Suffix.Question"
        | Star -> "Suffix.Star"
        | Plus -> "Suffix.Plus"

///AST Type for Prefix production
type Prefix =
| And
| Not
    with
    override this.ToString() = 
        match this with
        | And -> "Prefix.And"
        | Not -> "Prefix.Not"

///AST Type for Primary production
type Primary =
| Identifier of string
| Expression of Expression
| Literal of string
| Class of Range list
| Dot 
    with
    static member Exp2Str (exp : Expression) =
        let sb = new System.Text.StringBuilder()
        for sequence in exp do
            sb.Append(" Sequence") |> ignore
            for si in sequence do
                sb.AppendFormat(" {0}", si) |> ignore
        sb.ToString()
    override this.ToString() = 
        match this with
        | Identifier i -> sprintf "Primary.Identifier %s" i
        | Expression e -> sprintf "Primary.Expression %s" (Primary.Exp2Str e)
        | Literal l -> sprintf "Primary.Literal %s" l
        | Class rl -> 
            let sb = new System.Text.StringBuilder("Primary.Class ")
            for r in rl do
                sb.AppendFormat("{0}, ", r) |> ignore
            sb.ToString()
        | Dot -> "Primary.Dot"

///AST Type for Sequence Item production
and SequenceItem =
    { 
        primaryItem: Primary;
        itemPrefix: Prefix option;     
        itemSuffix: Suffix option;
    }
    with 
    override this.ToString() = 
        let sb = new System.Text.StringBuilder("SequenceItem ")
        if Option.is_some this.itemPrefix then 
            sb.AppendFormat("{0} ", (Option.get this.itemPrefix)) |> ignore
        sb.Append(this.primaryItem) |> ignore
        if Option.is_some this.itemSuffix then 
            sb.AppendFormat(" {0}", (Option.get this.itemSuffix)) |> ignore
        sb.ToString()

and Sequence = SequenceItem list

and Expression = Sequence list

///AST Type for Definition production
type Definition = 
    {
        name: string;
        exp: Expression;
    }
    with
    override this.ToString() = 
        sprintf "Definition (name: %A, exp: %A)" this.name (Primary.Exp2Str this.exp)

//---------------------------------------------------------------------------------------------
//parseBuffer type

let S2PB input  = List.of_seq input 
let (!!) input = S2PB  input

///The NC (aka Next Char) AP function returns a tuple of the top character from the 
///input buffer and the remainder of the input buffer. 
//I'm using an AP function rather than the native hd :: tl syntax so I can later change
//my input buffer type without affecting all the code in my parser
let (|NC|_|) = function 
    | i :: input -> Some(i, input)
    | [] -> None

///The TK AP function checks the top of the input buffer for the 
///specified token string, returning the remaining input buffer if the token
///is found
//Like NC, one of the main purposes of this function is to abstract the input
//buffer type so I can change it without affecting the rest of the parsing code
let (|TK|_|) token  input =
    let rec ParseToken token input =
        match token,(|NC|_|) input with
        | t :: [], Some(i, input) when i = t -> Some(input)
        | t :: token, Some(i, input) when i = t-> ParseToken token input
        | _ -> None
    ParseToken (List.of_seq token) input

//FP == Failure Predicate
let (|FP|_|) f input =
    match f input with
    | Some(_) -> None
    | None -> Some(input)

//SP == Success Predicate
let (|SP|_|) f input =
    match f input with
    | Some(_) -> Some(input)
    | None -> None

//ZOM == Zero Or More    
let rec (|ZOM|) f input = 
    match f input with 
    | Some(i,input) -> 
        let j,input = (|ZOM|) f input
        (i :: j, input)
    | None -> [], input

//OOM == One Or More        
let (|OOM|_|) f input = 
    match (|ZOM|) f input with
    | [], input -> None
    | v, input -> Some(v,input)
    
//ZOO == Zero Or One
let (|ZOO|) f input = 
    match f input with 
    | Some(i,input) -> Some(i), input
    | None -> None,input

//---------------------------------------------------------------------------------------------
//PEG Recursive Descent Parser
            
///EndOfFile <- !.
let (|EndOfFile|_|) = function 
    | NC (_) -> None
    | _ -> Some()

///EndOfLine <- '\r\n' / '\n' / '\r'    
let (|EndOfLine|_|) = function
    | TK "\r\n" (input) -> Some(input)
    | TK "\n" (input) -> Some(input)
    | TK "\r" (input) -> Some(input)
    | _ -> None

///Space <- ' ' / '\t' / EndOfLine
let (|Space|_|) = function
    | TK " " (input) -> Some(input)
    | TK "\t" (input) -> Some(input)
    | EndOfLine (input) -> Some(input)
    | _ -> None

///Comment <- ’#’ (!EndOfLine .)* EndOfLine
let (|Comment|_|) = 
    let NotEOL = function
        | FP (|EndOfLine|_|) (NC (c, input)) -> Some(c, input)
        | _ -> None
    function 
    | TK "#" (ZOM NotEOL (cl, EndOfLine (input))) -> Some(input) 
    | _ -> None

///Spacing <- (Space / Comment)*
//Note, spacing always matches (hence the lack of _| in the AP name. If there is no spacing, the 
//current input parse buffer is returned unchanged
let rec (|Spacing|) input = 
    match input with
    | Space (input) -> (|Spacing|) input
    | Comment (input) -> (|Spacing|) input
    | _ -> input
    
///DOT <- '.' Spacing
let (|DOT|_|) = function
    | TK "." (Spacing(input)) -> Some(input)
    | _ -> None
    
///CLOSE <- ')' Spacing
let (|CLOSE|_|) = function
    | TK ")" (Spacing(input)) -> Some(input)
    | _ -> None
    
///OPEN <- '(' Spacing
let (|OPEN|_|) = function
    | TK "(" (Spacing(input)) -> Some(input)
    | _ -> None
    
///PLUS <- '+' Spacing
let (|PLUS|_|) = function
    | TK "+" (Spacing(input)) -> Some(input)
    | _ -> None

///STAR <- '*' Spacing
let (|STAR|_|) = function
    | TK "*" (Spacing(input)) -> Some(input)
    | _ -> None
    
///QUESTION <- '?' Spacing
let (|QUESTION|_|) = function
    | TK "?" (Spacing(input)) -> Some(input)
    | _ -> None
    
///NOT <- '!' Spacing
let (|NOT|_|) = function
    | TK "!" (Spacing(input)) -> Some(input)
    | _ -> None

///AND <- '&' Spacing    
let (|AND|_|) = function
    | TK "&" (Spacing(input)) -> Some(input)
    | _ -> None

///SLASH <- '/' Spacing    
let (|SLASH|_|) = function
    | TK "/" (Spacing(input)) -> Some(input)
    | _ -> None

///LEFTARROW <- '<-' Spacing    
let (|LEFTARROW|_|) = function
    | TK "<-" (Spacing(input)) -> Some(input)
    | _ -> None

///Char <- '\\' [nrt'"\[\]\\]
/// / '\\' [0-2][0-7][0-7]
/// / '\\' [0-7][0-7]
/// / '\\' [0-7]
/// / !'\\' .    
let (|Char|_|) = 
       
    let (|InRange|_|) upper input =
        let i2c value = Char.chr(Char.code '0' + value)
        let c2i value = Char.code value - Char.code '0'
        match input with
        | NC (c, input) when (i2c 0) <= c && c <= (i2c upper) ->
            Some((c2i c), input)
        | _ -> None
        
    function
    | TK @"\" (NC(c, input)) when List.exists (fun x -> x=c) ['n';'r';'t';'\'';'"';'[';']';'\\'] -> 
        match c with
        | 'n' -> Some('\n', input)
        | 'r' -> Some('\r', input)
        | 't' -> Some('\t', input)
        | _ -> Some(c, input)
    | TK @"\" (InRange 2 (i1, InRange 7 (i2, InRange 7 (i3, input)))) ->
        Some(Char.chr (i1 * 64 + i2 * 8 + i3), input)
    | TK @"\" (InRange 7 (i1, InRange 7 (i2, input))) ->
        Some(Char.chr (i1 * 8 + i2), input)
    | TK @"\" (InRange 7 (i1, input)) ->
        Some(Char.chr (i1), input)
    | NC(c, input) when c <> '\\' -> Some(c, input)
    | _ -> None  

///Range <- Char '-' Char / Char
let (|Range|_|) = function
    | Char (c1, TK "-" (Char (c2, input))) -> 
        Some(Range.Dual (c1, c2), input)
    | Char (c1, input) -> 
        Some(Range.Single(c1), input) 
    | _ -> None

///Class <- '[' (!']' Range)* ']' Spacing    
let (|Class|_|) =
    let Ranges = function
        | FP ((|TK|_|) "]") (Range (range, input)) -> Some(range,input)
        | _ -> None
    function 
    | TK "[" (ZOM Ranges (ranges, TK "]" (Spacing(input)))) -> Some(ranges, input)
    | _ -> None

///Literal <- ['] (!['] Char)* ['] Spacing
///         / ["] (!["] Char)* ["] Spacing
let (|Literal|_|) = 
    let Chars delimiter input =
        match input with
        | FP ((|TK|_|) delimiter) (Char (c,input)) -> Some(c,input)
        | _ -> None
    function 
    | TK "'"  (ZOM (Chars "'")  (chars, TK "'"  (Spacing(input)))) -> Some(L2S chars, input)
    | TK "\"" (ZOM (Chars "\"") (chars, TK "\"" (Spacing(input)))) -> Some(L2S chars, input)
    | _ -> None

///Identifier <- [a-zA-Z_] [a-zA-Z0-9_]* Spacing    
//I rewrote this to make IdentStart and IdentCont local to the Identifier function. I did this
//Since they aren't accessed outside of the identifier function anyway
let (|Identifier|_|) = 
    let (|IdentStart|_|) = function
        | NC (c, input) when ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c = '_') -> 
            Some(c, input)
        | _ -> None
    let IdentCont = function
        | IdentStart (c, input) -> Some(c,input)
        | NC (c, input) when ('0' <= c && c <= '9') -> Some(c,input)
        | _ -> None
    function 
    | IdentStart (c, ZOM IdentCont (chars, Spacing (input))) -> Some(L2S (c :: chars), input)
    | _ -> None

///Primary <- Identifier !LEFTARROW
///         / OPEN Expression CLOSE
///         / Literal / Class / DOT
let rec (|Primary|_|) = function
    | Identifier (id, FP (|LEFTARROW|_|) (input)) -> Some(Primary.Identifier(id), input)
    | OPEN ( Expression (exp, CLOSE (input))) -> Some(Primary.Expression(exp), input)
    | Literal (lit, input) -> Some(Primary.Literal(lit), input)
    | Class (cls, input) -> Some(Primary.Class(cls), input)
    | DOT (input) -> Some(Primary.Dot, input)
    | _ -> None
    
///SequenceItem <- (AND / NOT)? Primary (QUESTION / STAR / PLUS)?
//changed from the Original Grammar: 
//    Prefix <- (AND / NOT)? Suffix
//    Suffix <- Primary (QUESTION / STAR / PLUS)?
and (|SequenceItem|_|) =
    let (|Suffix|) input = 
        match input with
        | QUESTION(input) -> Some(Suffix.Question), input
        | STAR(input) -> Some(Suffix.Star),input
        | PLUS(input) -> Some(Suffix.Plus),input
        | _ -> None,input
    let (|Prefix|) input =
        match input with 
        | AND(input) -> Some(Prefix.And),input
        | NOT(input) -> Some(Prefix.Not),input
        | _ -> None,input
    function
    | Prefix (pfix, Primary(primary, Suffix(sfix, input))) -> 
        Some({primaryItem=primary;itemPrefix=pfix;itemSuffix=sfix}, input)
    | _ -> None

///Sequence <- SequenceItem*
and (|Sequence|) = function ZOM (|SequenceItem|_|) (items, input) -> items, input

///Expression <- Sequence (SLASH Sequence)*
and (|Expression|) = 
    let SlashSequence = function
        | SLASH (Sequence (sq, input)) -> Some(sq,input)
        | _ -> None
    function 
    | Sequence (sq, ZOM SlashSequence (sql, input)) -> sq::sql, input
    
///Definition <- Identifier LEFTARROW Expression
let (|Definition|_|) = function
    | Identifier (id, LEFTARROW (Expression (ex, input))) -> 
        Some({name=id;exp=ex}, input) 
    | _ -> None
    
///Grammar <- Spacing Definition+ EndOfFile
let (|Grammar|_|) = function
    | Spacing (OOM (|Definition|_|) (dl, EndOfFile)) -> Some(List.to_array dl)
    | _ -> None