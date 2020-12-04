open System.IO

let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}



let countAndShift (agg : int * int) (i : string) = 
    match i.[(0 + (snd agg)) % i.Length] with
        | '#' -> ((fst agg) + 1, (snd agg)+ 3) 
        | _ -> (fst agg, (snd agg) + 3)

"input.txt"
    |> read
    |> Seq.fold countAndShift (0,0)
    |> fst
    |> printfn "Trees hit %A"