open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__ , @"..\code_off-1\code_off-1.in")
let outputPath = Path.Combine(__SOURCE_DIRECTORY__ , @"..\code_off-1\code_off-1.out")

let charValue = function
| uc when (int)uc - (int)'A' < 26 -> (int)uc - (int)'A' + 1
| lc when (int)lc - (int)'a' < 26 -> (int)lc - (int)'a'
| _ -> 0

let rowValue (r : string) =
    r.ToCharArray()
    |> Seq.sumBy charValue

let duplicateRows rows input =
    rows
    |> Seq.filter (fun a -> a <> input && (rowValue a = rowValue input))

let reverseString (a : string) =
    new String(a.ToCharArray() |> Array.rev)

let isPalindrome a =
    a = reverseString a

let input = 
    File.ReadAllLines inputPath
    |> Seq.skip 1
    
let answer = 
    input
    // Problem statement doesn't include how a "list of matching rows" should be represented - opting for ; as separator...
    |> Seq.map (fun a -> 
        [
            a;
            isPalindrome a |> string;
            String.Join(";", duplicateRows input a)
        ])
    |> Seq.fold (fun state elem -> Seq.append state elem) Seq.empty

File.WriteAllLines(outputPath, answer)