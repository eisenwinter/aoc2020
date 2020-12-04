open System.IO

let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let procc former shift =
    let countAndShift (agg : int * int) (i : string) = 
        match i.[(0 + (snd agg)) % i.Length] with
            | '#' -> ((fst agg) + 1, (snd agg)+ shift) 
            | _ -> (fst agg, (snd agg) + shift)
    "input.txt"
    |> read
    |> Seq.skip 1
    |> former
    |> Seq.fold countAndShift (0,shift)
    |> fst


let seconds l = 
    l 
    |> Seq.mapi (fun i e -> e,i)
    |> Seq.filter (fun (e,i) -> i % 2 = 1)
    |> Seq.map fst

[1;3;5;7]
    |> List.map (procc id)
    |> List.map int64
    |> List.fold (fun x  y  -> x * y) (int64 1)
    |> (*) (int64 (procc seconds 1))
    |> printfn "Output is %A"