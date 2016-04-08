open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__ , @"..\code_off-3\code_off-3-1.in")
let outputPath = Path.Combine(__SOURCE_DIRECTORY__ , @"..\code_off-3\code_off-3.out")

type TerrainType =
    | Land
    | Water
    | MarkedArea

type Map = TerrainType [] []

type Coord =
    {
        x : int;
        y : int;
    }

let parseTerrainType = function
    | l when l = '#' -> Land
    | w when w = '.' -> Water
    | m when m = '*' -> MarkedArea
    | _ -> failwith "Invalid Terrain Type"

let map =
    File.ReadAllLines inputPath
    |> Seq.map (fun a -> Array.map parseTerrainType (a.ToCharArray()))
    |> Seq.toArray

let waterTiles =
    map
    |> Array.mapi (fun yi row -> row |> Array.filter (fun elem -> elem = Water) |> Array.mapi (fun xi elem -> {x = xi; y = yi}))
    |> Array.fold (fun state elem -> Array.append state elem) Array.empty