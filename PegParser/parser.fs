#light 

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
let (|NC|_|) input = 
    match input with 
    | i :: input -> Some(i, input)
    | [] -> None

///The TOKEN AP function checks the top of the input buffer for the 
///specified token string, returning the remaining input buffer if the token
///is found
//Like NC, one of the main purposes of this function is to abstract the input
//buffer type so I can change it without affecting the rest of the parsing code
let (|TOKEN|_|) token  input =
    let rec ParseToken token input =
        match token,(|NC|_|) input with
        | t :: [], Some(i, input) when i = t -> Some(input)
        | t :: token, Some(i, input) when i = t-> ParseToken token input
        | _ -> None
    ParseToken (List.of_seq token) input

//---------------------------------------------------------------------------------------------
//PEG Recursive Descent Parser
            
///EndOfFile <- !.
let (|EndOfFile|_|) input = 
    match input with
    | NC (_) -> None
    | _ -> Some()

///EndOfLine <- '\r\n' / '\n' / '\r'    
let (|EndOfLine|_|) input = 
    match input with
    | TOKEN "\r\n" (input) -> Some(input)
    | TOKEN "\n" (input) -> Some(input)
    | TOKEN "\r" (input) -> Some(input)
    | _ -> None

///Space <- ' ' / '\t' / EndOfLine
let (|Space|_|) input = 
    match input with
    | TOKEN " " (input) -> Some(input)
    | TOKEN "\t" (input) -> Some(input)
    | EndOfLine (input) -> Some(input)
    | _ -> None

///Comment <- '#' ((!EndOfLine / !EndOfFile) .)* EndOfLine?
let (|Comment|_|) input = 
    let rec (|CommentContent|_|) input = 
        match input with
        | EndOfLine (input) -> Some(input)
        | EndOfFile -> Some(input)
        | NC (_,input) -> (|CommentContent|_|) input
        | _ -> None
    match input with
    | TOKEN "#" (CommentContent (input)) -> Some(input)
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
let (|DOT|_|) input =
    match input with
    | TOKEN "." (Spacing(input)) -> Some(input)
    | _ -> None
    
///CLOSE <- ')' Spacing
let (|CLOSE|_|) input =
    match input with
    | TOKEN ")" (Spacing(input)) -> Some(input)
    | _ -> None
    
///OPEN <- '(' Spacing
let (|OPEN|_|) input =
    match input with
    | TOKEN "(" (Spacing(input)) -> Some(input)
    | _ -> None
    
///PLUS <- '+' Spacing
let (|PLUS|_|) input =
    match input with
    | TOKEN "+" (Spacing(input)) -> Some(input)
    | _ -> None

///STAR <- '*' Spacing
let (|STAR|_|) input =
    match input with
    | TOKEN "*" (Spacing(input)) -> Some(input)
    | _ -> None
    
///QUESTION <- '?' Spacing
let (|QUESTION|_|) input =
    match input with
    | TOKEN "?" (Spacing(input)) -> Some(input)
    | _ -> None
    
///NOT <- '!' Spacing
let (|NOT|_|) input =
    match input with
    | TOKEN "!" (Spacing(input)) -> Some(input)
    | _ -> None

///AND <- '&' Spacing    
let (|AND|_|) input =
    match input with
    | TOKEN "&" (Spacing(input)) -> Some(input)
    | _ -> None

///SLASH <- '/' Spacing    
let (|SLASH|_|) input =
    match input with
    | TOKEN "/" (Spacing(input)) -> Some(input)
    | _ -> None

///LEFTARROW <- '<-' Spacing    
let (|LEFTARROW|_|) input =
    match input with
    | TOKEN "<-" (Spacing(input)) -> Some(input)
    | _ -> None

///Char <- '\\' [nrt'"\[\]\\]
/// / '\\' [0-2][0-7][0-7]
/// / '\\' [0-7][0-7]
/// / '\\' [0-7]
/// / !'\\' .    
let (|Char|_|) input = 
       
    let (|InRange|_|) upper input =
        let i2c value = Char.chr(Char.code '0' + value)
        let c2i value = Char.code value - Char.code '0'
        
        match input with
        | NC (c, input) when (i2c 0) <= c && c <= (i2c upper) ->
            Some((c2i c), input)
        | _ -> None
        
    match input with
    | TOKEN @"\" (NC(c, input)) when List.exists (fun x -> x=c) ['n';'r';'t';'\'';'"';'[';']';'\\'] -> 
        match c with
        | 'n' -> Some('\n', input)
        | 'r' -> Some('\r', input)
        | 't' -> Some('\t', input)
        | _ -> Some(c, input)
    | TOKEN @"\" (InRange 2 (i1, InRange 7 (i2, InRange 7 (i3, input)))) ->
        Some(Char.chr (i1 * 64 + i2 * 8 + i3), input)
    | TOKEN @"\" (InRange 7 (i1, InRange 7 (i2, input))) ->
        Some(Char.chr (i1 * 8 + i2), input)
    | TOKEN @"\" (InRange 7 (i1, input)) ->
        Some(Char.chr (i1), input)

    | NC(c, input) when c <> '\\' -> Some(c, input)
    | _ -> None  

///Range <- Char '-' Char / Char
let (|Range|_|) input =
    match input with
    | Char (c1, TOKEN "-" (Char (c2, input))) -> 
        Some(Range.Dual (c1, c2), input)
    | Char (c1, input) -> 
        Some(Range.Single(c1), input) 
    | _ -> None

///Class <- '[' (!']' Range)* ']' Spacing    
let (|Class|_|) input =

    let rec (|Ranges|_|) ranges input = 
        match input with
        | TOKEN "]" (_) -> Some(ranges, input)
        | Range(range, input) -> (|Ranges|_|) (ranges @ [range]) input
        | _ -> None
    
    match input with
    | TOKEN "[" (Ranges [] (ranges, TOKEN "]" (Spacing(input)))) -> Some(ranges, input)
    | _ -> None

///Literal <- ['] (!['] Char)* ['] Spacing
///         / ["] (!["] Char)* ["] Spacing
let (|Literal|_|) input =

    let rec (|LitChars|_|) delimiter chars input =
        match input with
        | TOKEN delimiter (_) -> Some(L2S chars, input)
        | Char (c, input) -> 
            (|LitChars|_|) delimiter (chars @ [c]) input
        | _ -> None
    
    match input with
    | TOKEN "'"  (LitChars "'"  [] (lit, TOKEN "'"  (Spacing(input)))) -> 
        Some(lit, input)
    | TOKEN "\"" (LitChars "\"" [] (lit, TOKEN "\"" (Spacing(input)))) -> 
        Some(lit, input)
    | _ -> None

///Identifier <- [a-zA-Z_] [a-zA-Z0-9_]* Spacing    
//I rewrote this to make IdentStart and IdentCont local to the Identifier function. I did this
//Since they aren't accessed outside of the identifier function anyway
let (|Identifier|_|) input = 
       
    let (|IdentStart|_|) input =
        match input with
        | NC (c, input) when ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c = '_') -> 
            Some(c, input)
        | _ -> None
    let rec (|IdentCont|_|) chars input = 
        match input with
        | IdentStart (c, input) -> 
            (|IdentCont|_|) (chars @ [c]) input
        | NC (c, input) when ('0' <= c && c <= '9') -> 
            (|IdentCont|_|) (chars @ [c]) input
        | _ -> Some(chars, input)
        
    match input with
    | IdentStart (c, IdentCont [] (chars, Spacing (input))) -> 
        Some(L2S (c :: chars), input)
    | _ -> None

///Primary <- Identifier !LEFTARROW
///         / OPEN Expression CLOSE
///         / Literal / Class / DOT
let rec (|Primary|_|) input =

    let (|NotLEFTARROW|_|) input =
        match input with
        | LEFTARROW (_) -> None
        | _ -> Some(input)

    match input with
    | Identifier (id, NotLEFTARROW (input)) -> 
        Some(Primary.Identifier(id), input)
    | OPEN ( Expression (exp, CLOSE (input))) -> 
        Some(Primary.Expression(exp), input)
    | Literal (lit, input) -> 
        Some(Primary.Literal(lit), input)
    | Class (cls, input) -> 
        Some(Primary.Class(cls), input)
    | DOT (input) -> 
        Some(Primary.Dot, input)
    | _ -> None
    
///SequenceItem <- (AND / NOT)? Primary (QUESTION / STAR / PLUS)?
//changed from the Original Grammar: 
//    Prefix <- (AND / NOT)? Suffix
//    Suffix <- Primary (QUESTION / STAR / PLUS)?
and (|SequenceItem|_|) input =
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
    match input with
    | Prefix (pfix, Primary(primary, Suffix(sfix, input))) -> 
        Some({primaryItem=primary;itemPrefix=pfix;itemSuffix=sfix}, input)
    | _ -> None

///Sequence <- SequenceItem*
and (|Sequence|) input =
    let rec ParseSequence items input =
        match input with
        | SequenceItem (item, input) -> ParseSequence (items @ [item]) input
        | _ -> items, input
    ParseSequence [] input

///Expression <- Sequence (SLASH Sequence)*
and (|Expression|) input =
    let rec ParseExpression seqList input =
        match input with
        | SLASH (Sequence (sq,input)) -> ParseExpression (seqList @ [sq]) input
        | _ -> seqList,input
    match input with
    | Sequence (sq, input) -> ParseExpression [sq] input
    
///Definition <- Identifier LEFTARROW Expression
let (|Definition|_|) input =
    match input with
    | Identifier (id, LEFTARROW (Expression (ex, input))) -> 
        Some({name=id;exp=ex}, input) 
    | _ -> None

///Grammar <- Spacing Definition+ EndOfFile
let (|Grammar|_|) input =
    let rec ParseDefinitions dl input =
        match input with
        | Definition (d, input) -> ParseDefinitions (dl @ [d]) input
        | _ -> Some(dl, input)
    let (|OneOrMoreDefintions|_|) input = 
        match input with
        | Definition (d, input) -> ParseDefinitions [d] input
        | _ -> None
    match input with
    | Spacing (OneOrMoreDefintions (dl, EndOfFile)) -> Some(List.to_array dl)
    | _ -> None