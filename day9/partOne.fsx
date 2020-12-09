open System.IO
let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
let content = "input.txt" |> read

let preamble i =
    content
    |> Seq.skip i
    |> Seq.take 25
    |> Seq.map int64

let keySet i : int64 Set =
    (preamble i)
    |> Seq.collect (fun g -> (preamble i) |> Seq.map (fun h -> if g <> h then Some (g + h) else None))
    |> Seq.choose id
    |> Set.ofSeq

let payload = 
    content
    |> Seq.skip 25 
    |> Seq.map int64

payload 
    |> Seq.mapi (fun i f ->  if (keySet i).Contains f |> not then Some f else None)
    |> Seq.choose id
    |> Seq.head
    |> printfn "%A"