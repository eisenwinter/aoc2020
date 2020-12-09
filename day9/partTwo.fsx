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
    |> Seq.toArray

let getInvalidNumber =
    payload 
        |> Seq.mapi (fun i f ->  if (keySet i).Contains f |> not then Some f else None)
        |> Seq.choose id
        |> Seq.head

let rec find (l : int64 array) s e a nr =
    match a with 
    | x when x = nr -> Some (s,e)
    | x when x > nr -> None 
    | _ -> find l s (e + 1) (a+l.[e]) nr

let sumMinMax l (i,a) =
    let window = l |> Seq.skip i |> Seq.take (a - i)
    (+) (window |> Seq.min) (window |> Seq.max)  

let toFind = getInvalidNumber

payload
    |> Seq.indexed
    |> Seq.choose (fun (i,_) -> find payload i (i+1) payload.[i] toFind)
    |> Seq.head
    |> sumMinMax payload
    |> printfn "%A"
