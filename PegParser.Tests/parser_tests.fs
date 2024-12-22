#light

#R @"..\PegParser\pegparser.dll"
#R @"..\xUnit.net\xunit.dll"

#nowarn "25" //Turn off Incomplete Pattern Match warning
#nowarn "62" //Turn off Some contruct needs parens warning

open Parser
open Xunit

(*
type  parser_tests =   
    class   
        new () = {}   
          
        [<Test>]  
        member test_NC() =   
            let Some(c,text) = NC !!"test" 
            Assert.Equal(c, 't')   
            Assert.Equal(text, !!"est")

        [<Test>]  
        member test_NC_empty_string() =   
            let ret = NC !!"" 
            Assert.Equal(None, ret)
    end*)
(*
[<Test>]   
let test_NC  =    
    let Some(c,text) = NC !!"test" 
    Assert.Equal(c, 't')   
    Assert.Equal(text, !!"est")

[<Test>]   
let test_NC_empty_string =    
    let ret = NC !!"" 
    Assert.Equal(None, ret)
    *)
    
[<Fact>]  
let test_NC () =   
    let Some(c,text) = (|NC|_|) !!"test" 
    Assert.Equal(c, 't')   
    Assert.Equal(text, !!"est")

[<Fact>]  
let test_NC_empty_string () =   
    let ret = (|NC|_|) !!"" 
    Assert.Equal(None, ret)

(*[<Fact>]  
let test_TOKEN_notAP () =
    let Some(text) = TOKEN "test" !!"testme"
    Assert.Equal(text, !!"me")*)

[<Fact>]  
let test_TOKEN () =
    let Some(text) = (|TOKEN|_|) "test" !!"testme"
    Assert.Equal(text, !!"me")

[<Fact>]  
let test_TOKEN_single_char () =
    let Some(text) = (|TOKEN|_|) "t" !!"testme"
    Assert.Equal(text, !!"estme")
    
[<Fact>]
let test_TOKEN_no_match () =
    let ret = (|TOKEN|_|) "test" !!"nomatch"
    Assert.Equal(None, ret)

[<Fact>] 
let test_TOKEN_end_of_file () =
    let ret = (|TOKEN|_|) "tet" !!""
    Assert.Equal(None, ret);

[<Fact>]  
let test_EndOfFile() =
    let ret = (|EndOfFile|_|) !!""
    Assert.Equal(Some(), ret)
    
[<Fact>]  
let test_EndOfFile_no_match () =
    let ret = (|EndOfFile|_|) !!"test"
    Assert.Equal(None, ret)

[<Fact>]  
let test_OPEN () =
    let Some(text) = (|OPEN|_|) !!"(test"
    Assert.Equal(!!"test", text)

[<Fact>]  
let test_OPEN_space () =
    let Some(text) = (|OPEN|_|) !!"(   test"
    Assert.Equal(!!"test", text)

[<Fact>]  
let test_OPEN_no_match () =
    let ret = (|OPEN|_|) !!"test"
    Assert.Equal(None, ret)
    
[<Fact>]  
let test_Comment () =
    let ret = (|Comment|_|) !!"# a comment\rmore text"
    Assert.Equal(Some(!!"more text"), ret)

[<Fact>]  
let test_Comment_slashrn () =
    let ret = (|Comment|_|) !!"# a comment\r\nmore text"
    Assert.Equal(Some(!!"more text"), ret)

[<Fact>]  
let test_Comment_no_match () = 
    let ret = (|Comment|_|) !!"a comment\rmore text"
    Assert.Equal(None, ret)

[<Fact>]  
let test_Comment_eof () = 
    let ret = (|Comment|_|) !!"#a comment"
    Assert.Equal(Some(!!""), ret)

[<Fact>]  
let test_Char_slash53test () =
    let Some(c,text) = (|Char|_|) !!"\\53test"
    Assert.Equal((Char.chr (0o53)), c)
    Assert.Equal(!!"test", text)

[<Fact>]  
let test_Char_slashntest () =
    let Some(c,text) = (|Char|_|) !!"\\ntest"
    Assert.Equal('\n', c)
    Assert.Equal(!!"test", text)

[<Fact>]  
let test_Char_slashttest () =
    let Some(c,text) = (|Char|_|) !!"\\ttest"
    Assert.Equal('\t', c)
    Assert.Equal(!!"test", text)

[<Fact>]  
let test_Char_slashrtest () =
    let Some(c,text) = (|Char|_|) !!"\\rtest"
    Assert.Equal('\r', c)
    Assert.Equal(!!"test", text)

[<Fact>]  
let test_Char_slash277test () =
    let Some(c,text) = (|Char|_|) !!"\\277test"
    Assert.Equal((Char.chr (0o277)), c)
    Assert.Equal(!!"test", text)


[<Fact>]  
let test_Char_slash7test () =
    let Some(c,text) = (|Char|_|) !!"\\7test"
    Assert.Equal((Char.chr (0o7)), c)
    Assert.Equal(!!"test", text)

[<Fact>]  
let test_Range_test () =
    let Some(c,text) = (|Range|_|) !!"test"
    Assert.Equal(Range.Single('t'), c)
    Assert.Equal(!!"est", text)

[<Fact>]  
let test_Range_tdashest () =
    let Some(c,text) = (|Range|_|) !!"t-est"
    Assert.Equal(Range.Dual('t', 'e'), c)
    Assert.Equal(!!"st", text)
    
    
[<Fact>]  
let test_Class_singleRange () = 
    let Some(rangeList,text) = (|Class|_|) !!"[a-z]test"
    let ranges = List.to_array rangeList
    Assert.Equal(1, ranges.Length)
    Assert.Equal(Range.Dual('a','z'), ranges.[0])

[<Fact>]  
let test_Class_twoRanges () = 
    let Some(rangeList,text) = (|Class|_|) !!"[a-zA-Z]test"
    let ranges = List.to_array rangeList
    Assert.Equal(2, ranges.Length)
    Assert.Equal(Range.Dual('a','z'), ranges.[0])
    Assert.Equal(Range.Dual('A','Z'), ranges.[1])
    
[<Fact>]
let test_Class_twoRanges_singlechars () = 
    let Some(rangeList,text) = (|Class|_|) !!"[aZ]test"
    let ranges = List.to_array rangeList
    Assert.Equal(2, ranges.Length)
    Assert.Equal(Range.Single('a'), ranges.[0])
    Assert.Equal(Range.Single('Z'), ranges.[1])

[<Fact>]
let test_Literal_single_quot () =
    let Some(literal,text) = (|Literal|_|) !!"'abcd'test"
    Assert.Equal("abcd", literal)
    Assert.Equal(!!"test", text)

[<Fact>]
let test_Literal_single_quot_with_double_quote_embedded () =
    let Some(literal,text) = (|Literal|_|) !!"'ab\"cd'test"
    Assert.Equal("ab\"cd", literal)
    Assert.Equal(!!"test", text)

[<Fact>]
let test_Literal_double_quote () =
    let Some(literal,text) = (|Literal|_|) !!"\"abcd\"test"
    Assert.Equal("abcd", literal)
    Assert.Equal(!!"test", text)

[<Fact>]
let test_Literal_double_quote_with_single_quote_embedded () =
    let Some(literal,text) = (|Literal|_|) !!"\"a'bcd\"test"
    Assert.Equal("a'bcd", literal)
    Assert.Equal(!!"test", text)
    
[<Fact>]
let test_Identifier () =
    let Some(identfier, text) = (|Identifier|_|) !!"test me"
    Assert.Equal("test", identfier)
    Assert.Equal(!!"me", text)

[<Fact>]
let test_Identifier_underscore_start () =
    let Some(identfier, text) = (|Identifier|_|) !!"_test me"
    Assert.Equal("_test", identfier)
    Assert.Equal(!!"me", text)

[<Fact>]
let test_Identifier_underscore_middle () =
    let Some(identfier, text) = (|Identifier|_|) !!"te_st me"
    Assert.Equal("te_st", identfier)
    Assert.Equal(!!"me", text)
    
[<Fact>]
let test_Identifier_number_middle () =
    let Some(identfier, text) = (|Identifier|_|) !!"te5st me"
    Assert.Equal("te5st", identfier)
    Assert.Equal(!!"me", text)

[<Fact>]
let test_Identifier_number_start_fail () =
    let v= (|Identifier|_|) !!"5test me"
    Assert.Equal(None, v)

[<Fact>]  
let test_PEG_grammar () =
    let Some(defs) = (|Grammar|_|) !! @"# Hierarchical syntax
Grammar <- Spacing Definition+ EndOfFile
Definition <- Identifier LEFTARROW Expression
Expression <- Sequence (SLASH Sequence)*
Sequence <- Prefix*
Prefix <- (AND / NOT)? Suffix
Suffix <- Primary (QUESTION / STAR / PLUS)?
Primary <- Identifier !LEFTARROW
/ OPEN Expression CLOSE
/ Literal / Class / DOT
# Lexical syntax
Identifier <- IdentStart IdentCont* Spacing
IdentStart <- [a-zA-Z_]
IdentCont <- IdentStart / [0-9]
Literal <- ['] (!['] Char)* ['] Spacing
/ [""] (![""] Char)* [""] Spacing
Class <- '[' (!']' Range)* ']' Spacing
Range <- Char '-' Char / Char
Char <- '\\' [nrt'""\[\]\\]
/ '\\' [0-2][0-7][0-7]
/ '\\' [0-7][0-7]?
/ !'\\' .
LEFTARROW <- '<-' Spacing
SLASH <- '/' Spacing
AND <- '&' Spacing
NOT <- '!' Spacing
QUESTION <- '?' Spacing
STAR <- '*' Spacing
PLUS <- '+' Spacing
OPEN <- '(' Spacing
CLOSE <- ')' Spacing
DOT <- '.' Spacing
Spacing <- (Space / Comment)*
Comment <- '#' (!EndOfLine .)* EndOfLine
Space <- ' ' / '\t' / EndOfLine
EndOfLine <- '\r\n' / '\n' / '\r'
EndOfFile <- !." 
    Assert.NotEmpty(defs)
    Assert.Equal(29, defs.Length)
