open System.IO

let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let content = "input.txt" |> read |> Seq.map int 

content 
    |> Seq.sort
    |> Seq.pairwise 
    |> Seq.map (fun (a,b) -> b - a )
    |> Seq.groupBy (fun a -> a)
    |> Seq.map (fun (a,b) -> (a, match a with 
                                            | 3 -> b |> Seq.length |> (+) 1 //+1 one phone at 3 higher
                                            | 1 -> b |> Seq.length |> (+) 1 //+1 of the outlet
                                            | _ -> b |> Seq.length  ))
    |> Seq.fold (fun acc (_,b) -> b * acc ) 1
    |> printfn "%A"