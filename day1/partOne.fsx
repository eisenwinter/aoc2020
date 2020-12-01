open System.IO

let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let input = "input.txt" |> read |> Seq.map int
let number = 
    input 
    |> Seq.map (fun x -> 2020 - x) 
    |> Seq.filter (fun x -> Seq.contains x input)  
    |> Seq.head
printfn "Number is %i" (number * (2020 - number))