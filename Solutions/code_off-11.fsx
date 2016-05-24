open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__ , @"..\code_off-11\code_off-11.in")
let outputPath = Path.Combine(__SOURCE_DIRECTORY__ , @"..\code_off-11\code_off-11.out")

#load "code_off-10.fsx"

let stringJoin sep (s : string seq) =
    String.Join (sep, s)

let buildString (c : char array) =
    new string(c)

let morseStringToChar s =
    match ``Code_off-10``.morseDictionary |> Map.toSeq |> Seq.tryFind (fun a -> snd a = s) with
    | Some s -> fst s
    | None -> ' '

let decodeChar (c : char) =
    match Int32.TryParse (string c) with
    | (true, count) 
        -> new string(Array.create count '.')
    | (false, _) 
        -> new string(Array.create ((int)c - (int)'A' + 1) '-')

let processLine (s : string) =
    let processMorseRun (w : string) =
        w.ToCharArray()
        |> Seq.map decodeChar
        |> stringJoin ""
        |> morseStringToChar

    let processWord (w : string) =
        w.Split '|' 
        |> Array.map processMorseRun
        |> buildString

    s.Split '/'
    |> Seq.map processWord
    |> stringJoin " "

let answer = 
    File.ReadLines inputPath
    |> Seq.map processLine

File.WriteAllLines(outputPath, answer)