// See the file alphabet-cipher.md for detailed information.
open System

type Message = string
type Keyword = string

let defaultPhrase = "abcdefghijklmnopqrstuvwxyz"
let convertToInt (value:char) = Convert.ToInt32(value) - Convert.ToInt32('a')
let convertFromInt (value:int) = Convert.ToChar(value + Convert.ToInt32('a'))


let normalize value key : Keyword =
    let rec internalNormalize value (key:Keyword) =
        if (key.Length >= value) then key.Substring(0, value)
        else (key + key) |> internalNormalize value

    internalNormalize value key

let offset (value:int) (phrase:string) = 
    let h = phrase |> Seq.take value |> String.Concat
    let t = phrase.Substring value

    String.Concat(t, h)

 
let encode (key:Keyword) (message:Message) : Message =
    key 
    |> normalize message.Length
    |> Seq.zip message
    |> Seq.map (fun (s, m) -> defaultPhrase |> offset (convertToInt s) |> Seq.item (convertToInt m))
    |> String.Concat
    
    

let decode (key:Keyword) (message:Message) : Message =
    key 
    |> normalize message.Length
    |> Seq.zip message
    |> Seq.map (fun (s, m) -> defaultPhrase |> offset (convertToInt m) |> Seq.findIndex (fun x -> x = s) |> convertFromInt )
    |> String.Concat


let decipher (cipher:Message) (message:Message) : Keyword =
    "decypherme"

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =
    test <@ normalize 29 "vigilance"  = "vigilancevigilancevigilancevi" @>
    test <@ normalize 9 "vigilance" = "vigilance" @>
    test <@ normalize 6 "vigilance" = "vigila" @>

    test <@offset 0 "abcdefg" = "abcdefg"@>
    test <@offset 1 "abcdefg" = "bcdefga"@>
    test <@offset 2 "abcdefg" = "cdefgab"@>
    test <@offset 3 "abcdefg" = "defgabc"@>
    test <@offset 4 "abcdefg" = "efgabcd"@>
    test <@offset 5 "abcdefg" = "fgabcde"@>
    test <@offset 6 "abcdefg" = "gabcdef"@>
                                   
    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    // test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    // test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
