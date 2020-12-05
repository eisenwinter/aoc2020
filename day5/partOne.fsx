open System.IO

let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let ch (c : char) = 
     match c with 
        | 'F' -> '0'
        | 'B' -> '1'
        | 'L' -> '0'
        | 'R' -> '1'
        | _ -> ' '

let str (s : string) : int = 
    s
    |> Seq.toList
    |> Seq.map ch
    |> Seq.fold (fun (a : string) f -> a + string f ) ""
    |> fun x -> System.Convert.ToInt32(x,2)

"input.txt"
    |> read
    |> Seq.map str
    |> Seq.max
    |> printfn "%A"