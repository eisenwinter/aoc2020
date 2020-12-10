open System.IO

let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let content = "input.txt" |> read |> Seq.map int64 |> Seq.toList

let (?+) k (m : Map<int64,int64>) =
    match m.TryFind(k) with 
    | Some x -> x 
    | None -> 0L

let combinations l : int64 =
    let charger = l |> List.max |> (+) 3L
    let withCharger = l @ [ charger ] |> Seq.sort |> Seq.toList
    let rec loop l' (p : Map<int64,int64>) : int64 =
        match l' with 
        | [] -> p.[charger]
        | hd :: tl ->   
                    let v = [ 3L; 2L; 1L ] |> Seq.sumBy (fun f -> (?+) (hd - f) p)
                    let p' = p.Add(hd, v)
                    loop tl p'
    let m = Map.empty.Add(0L, 1L)
    loop withCharger m
 
content 
    |> combinations
    |> printfn "%A"