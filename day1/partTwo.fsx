open System.IO

let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let input = "input.txt" |> read |> Seq.map int
let sorted = input |> Seq.sort
let bound = input |> Seq.length

let findRemaining x = 
    sorted |> Seq.tryFind (fun y -> (2020 - x) = y)

let rec findSecond x i =
    match i with 
    | b when bound <= b -> None
    | _ -> 
        let z =  sorted |> Seq.skip i |> Seq.head
        let x' = z |> (+) x |> findRemaining
        match x' with 
            | Some y -> Some (y * x * z) 
            | _ -> (findSecond x (i+1))

sorted 
    |> Seq.map (fun x -> (findSecond x 0))
    |> Seq.choose id
    |> Seq.head
    |> (printfn "Number is %i")