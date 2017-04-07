// See the file alphabet-cipher.md for detailed information.
open System

type Message = string
type Keyword = string

let normalize (key:Keyword) (message:Message) : Keyword =
    let rec internalNormalize (key:Keyword) (message:Message) : Keyword =
        if (key.Length >= message.Length) then key.Substring(0, message.Length)
        else internalNormalize (key + key) message

    internalNormalize key message

let offset (value:int) (phrase:string) = 
    let h = phrase |> Seq.take value |> String.Concat
    let t = phrase.Substring value

    String.Concat(t, h)

 
let encode (key:Keyword) (message:Message) : Message =
    "encodeme"

let decode (key:Keyword) (message:Message) : Message =
    "decodeme"

let decipher (cipher:Message) (message:Message) : Keyword =
    "decypherme"

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =
    test <@ normalize "vigilance" "meetmeontuesdayeveningatseven" = "vigilancevigilancevigilancevi" @>
    test <@ normalize "vigilance" "meetmeont" = "vigilance" @>
    test <@ normalize "vigilance" "meetme" = "vigila" @>

    test <@offset 0 "abcdefg" = "abcdefg"@>
    test <@offset 1 "abcdefg" = "bcdefga"@>
    test <@offset 2 "abcdefg" = "cdefgab"@>
    test <@offset 3 "abcdefg" = "defgabc"@>
    test <@offset 4 "abcdefg" = "efgabcd"@>
    test <@offset 5 "abcdefg" = "fgabcde"@>
    test <@offset 6 "abcdefg" = "gabcdef"@>
                                   
    // verify encoding
    //test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    //test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    //test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    //test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    // test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    // test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
