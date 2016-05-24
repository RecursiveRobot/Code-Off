open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__ , @"..\code_off-10\code_off-10.in")
let outputPath = Path.Combine(__SOURCE_DIRECTORY__ , @"..\code_off-10\code_off-10.out")

let morseDictionary =
    [
        'A', ".-"
        'B', "-..."
        'C', "-.-."
        'D', "-.."
        'E', "."
        'F', "..-."
        'G', "--."
        'H', "...."
        'I', ".."
        'J', ".---"
        'K', "-.-"
        'L', ".-.."
        'M', "--"
        'N', "-."
        'O', "---"
        'P', ".--."
        'Q', "--.-"
        'R', ".-."
        'S', "..."
        'T', "-"
        'U', "..-"
        'V', "...-"
        'W', ".--"
        'X', "-..-"
        'Y', "-.--"
        'Z', "--.."
        '0', "-----"
        '1', ".----"
        '3', "...--"
        '4', "....-"
        '5', "....."
        '6', "-...."
        '7', "--..."
        '8', "---.."
        '9', "----."
        '2', "..---"
        '.', ".-.-.-"
        ',', "--..--"
    ] |> Map.ofSeq

let stringJoin sep (s : string seq) =
    String.Join (sep, s)

let charToMorseString c =
    match morseDictionary.TryFind c with
    | Some s -> s
    | None -> ""

let obfuscateString (s : string) =
    let encodeRun r =
        match r with
        | [] -> String.Empty
        | dot :: _ when dot = '.' -> string r.Length
        | dash :: _ when dash = '-' -> string('A' + char(r.Length - 1))
        | other :: _ -> new string(r |> List.toArray)

    let rec obfuscateStringImpl currentRun remainder result =
        match remainder with
        | [] -> 
            result + encodeRun currentRun
        | (currentChar :: tail) when not currentRun.IsEmpty && currentChar = currentRun.Head -> 
            obfuscateStringImpl (currentRun @ [currentChar]) tail result
        | (currentChar :: tail) -> 
            obfuscateStringImpl [currentChar] tail (result + encodeRun currentRun)

    obfuscateStringImpl [] (s.ToCharArray() |> Array.toList) String.Empty


let processLine (s : string) =
    let processWord (w : string) =
        w.ToCharArray()
        |> Seq.map charToMorseString
        |> stringJoin "|"

    s.Split ' '
    |> Seq.map processWord
    |> stringJoin "/"
    |> obfuscateString
    
let answer = 
    File.ReadLines inputPath
    |> Seq.map processLine

File.WriteAllLines(outputPath, answer)