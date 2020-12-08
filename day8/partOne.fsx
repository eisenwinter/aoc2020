open System.IO

let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
let content = "input.txt" |> read


type Command =
    | Acc of int
    | Nop
    | Jmp of int

type HandheldProgram =
    Command array

let instruction (p' : string) =
    match p'.Substring(0, 3) with
    | "acc" -> Acc <| int (p'.Substring 3)
    | "jmp" -> Jmp <| int (p'.Substring 3)
    | "nop" -> Nop
    | _ -> failwith "Unknown instruction"


let createProgram i : HandheldProgram =
    i
    |> Seq.map instruction
    |> Seq.toArray

let initializeScan (p : HandheldProgram) =
    let rec failOnInfiniteLoop ip a (v : int Set)  : int =
        match v.Contains ip with 
        | true -> a
        | _ ->
            match p.[ip] with 
            | Acc ac -> failOnInfiniteLoop (ip + 1) (a + ac) (v.Add ip)
            | Jmp jm -> failOnInfiniteLoop (ip + jm) a (v.Add ip)
            | _ -> failOnInfiniteLoop (ip + 1) a (v.Add ip)

    failOnInfiniteLoop 0 0 Set.empty

content 
    |> createProgram
    |> initializeScan
    |> printfn "%A"