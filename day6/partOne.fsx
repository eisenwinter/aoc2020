let read (filePath:string) = System.IO.File.ReadAllText(filePath)
let content = "input.txt" |> read


let groups = 
    content.Split([| "\r\n\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let distinct (i : string) : int =
    i 
    |> Seq.toList
    |> Seq.filter (fun f -> f <> ' ' && f <> '\r' && f <> '\n')
    |> Seq.distinct
    |> Seq.length

groups
    |> Seq.map distinct
    |> Seq.sum
    |> printfn "%A"