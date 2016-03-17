open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__ , @"..\code_off-2\code_off-2.in")
let outputPath = Path.Combine(__SOURCE_DIRECTORY__ , @"..\code_off-2\code_off-2.out")

type Source =
    {
        index : int;
        volume : int
    }

type Destination = 
    { 
        index : int;
        capacity : int; 
        suitableSources : int array 
    }

type Distribution =
    {
        sourceIndex : int;
        destinationIndex : int;
        volume : int;
    }

let input =
    File.ReadAllLines inputPath

let numberOfSources =
    Int32.Parse input.[0]

let sources =
    input
    |> Seq.skip 1
    |> Seq.take numberOfSources
    |> Seq.mapi (fun i row -> { index = i; volume = Int32.Parse row })
    |> Seq.toList

let parseDestinationRow i (row : string) =
    let values = row.Split([|","|], StringSplitOptions.RemoveEmptyEntries)
    
    {
        index = i; 
        capacity = Int32.Parse values.[0]; 
        suitableSources = 
            values 
            |> Seq.skip 1 
            |> Seq.map Int32.Parse
            |> Seq.toArray
    }

let destinations =
    input
    |> Seq.skip (numberOfSources + 2)
    |> Seq.mapi parseDestinationRow
    |> Seq.toList


let remainingSources dist =
    let remainingSource (src : Source) =
        {src with volume = src.volume - (dist |> Seq.filter (fun a -> a.sourceIndex = src.index) |> Seq.sumBy (fun a -> a.volume))}
    
    sources
    |> Seq.map remainingSource
    |> Seq.filter (fun a -> a.volume > 0)


let remainingDestinations dist =
    let remainingSourceIds originalSources =
        let remainingSourcesIds = 
            remainingSources dist
            |> Seq.map (fun a -> a.index)

        Seq.append remainingSourcesIds originalSources
        |> Seq.groupBy id
        |> Seq.filter (fun (a, b) -> Seq.length b > 1)
        |> Seq.map fst

    let remainingDestination (dest : Destination) =
        {dest 
            with 
                suitableSources = (remainingSourceIds dest.suitableSources) |> Seq.toArray}

    destinations
        |> Seq.filter (fun a -> Seq.exists (fun b -> b.destinationIndex = a.index) dist = false)
        |> Seq.map remainingDestination
        |> Seq.filter (fun a -> a.suitableSources.Length > 0)


let applicableSourcesPrioritizedByAvailability dist dest =
    let sourceAvailability srcIndex =
        let remainingVolume =
            (remainingSources dist
            |> Seq.filter (fun a -> a.index = srcIndex)
            |> Seq.sumBy (fun a -> a.volume)) + 1

        let remainingCapacity =
            (remainingDestinations dist
            |> Seq.filter (fun d -> Seq.exists (fun a -> (a = srcIndex) && (Seq.length d.suitableSources = 1)) d.suitableSources)
            |> Seq.sumBy (fun a -> a.capacity)) + 1
            
        remainingVolume / remainingCapacity

    dest.suitableSources
    |> Seq.sortBy sourceAvailability
    |> Seq.map (fun a -> (Seq.find (fun (b : Source) -> b.index = a) (remainingSources dist)))


let distributeContents =
    let rec distributeContentsImpl dist =
        let remainingDests = remainingDestinations dist

        let prioritizedDests =
            remainingDests
            |> Seq.sortBy (fun a -> a.suitableSources.Length)
            |> Seq.toList

        match prioritizedDests with
        | [] -> Console.WriteLine "Empty Dest list"; dist
        | head :: tail ->
            let dest = head
            let source = (applicableSourcesPrioritizedByAvailability dist head) |> Seq.head
            let newDist =
                {
                    destinationIndex = dest.index;
                    sourceIndex = source.index;
                    volume = Math.Min (dest.capacity,source.volume);
                }

            distributeContentsImpl (newDist :: dist)
        
    distributeContentsImpl List.empty


let distributions =
    [0..destinations.Length-1]
    |> Seq.map (fun a -> distributeContents |> Seq.tryFind (fun b -> b.destinationIndex = a))
    |> Seq.map (function
        | Some dist -> (dist.sourceIndex |> string) + "," + (dist.volume |> string)
        | None -> "0")
    |> Seq.toList

let remainingVolume =
    ((sources |> Seq.sumBy (fun a -> a.volume)) - (distributeContents |> Seq.sumBy (fun a -> a.volume)))

let answer =     
    (remainingVolume |> string)
    ::
    distributions

File.WriteAllLines(outputPath, answer)