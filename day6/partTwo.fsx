let read (filePath:string) = System.IO.File.ReadAllText(filePath)
let content = "input.txt" |> read

let intersect' = 
    content.Split([| "\r\n\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun f-> f.Split([| "\r\n" |], System.StringSplitOptions.RemoveEmptyEntries))
    |> Array.toList
    |> Seq.map (fun f  -> f |> Seq.toList |> Seq.map (fun g -> g |> Set.ofSeq ))
    |> Seq.map (fun f-> f |> Seq.reduce Set.intersect) 

intersect'
    |> Seq.map (fun f -> f |> Seq.length)
    |> Seq.sum
    |> printfn "%A"