open System.IO

let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
let content = "input.txt" |> read


type Command =
    | Acc of int
    | Nop of int
    | Jmp of int

type HandheldProgram =
    Command array

let instruction (p' : string) =
    match p'.Substring(0, 3) with
    | "acc" -> Acc <| int (p'.Substring 3)
    | "jmp" -> Jmp <| int (p'.Substring 3)
    | "nop" -> Nop <| int (p'.Substring 3)
    | _ -> failwith "Unknown instruction"

let convert c i f=
    if i = f then 
        match c with 
        | Jmp c -> Nop c 
        | Nop c -> Jmp c 
        | x -> x
    else 
        c

let createProgram i : HandheldProgram =
    i
    |> Seq.map instruction
    |> Seq.toArray

let wayPoints (p : HandheldProgram) : (HandheldProgram * int Set) =
    let rec failOnInfiniteLoop ip a (v : int Set)  : int Set =
        match v.Contains ip with 
        | true -> v
        | _ ->
            match p.[ip] with 
            | Acc ac -> failOnInfiniteLoop (ip + 1) (a + ac) (v.Add ip)
            | Jmp jm -> failOnInfiniteLoop (ip + jm) a (v.Add ip)
            | _ -> failOnInfiniteLoop (ip + 1) a (v.Add ip)

    p, failOnInfiniteLoop 0 0 Set.empty

let terminates p sw : int option =
    let rec terminates' ip a (v : int Set)  : int option =
        match ip with 
        | x when x = Array.length p -> Some a
        | x when v.Contains x -> None 
        | _ ->
            match convert p.[ip] ip sw with 
            | Acc ac -> terminates' (ip + 1) (a + ac) (v.Add ip)
            | Jmp jm -> terminates' (ip + jm) a (v.Add ip)
            | _ -> terminates' (ip + 1) a (v.Add ip)
    terminates' 0 0 Set.empty

let findCandidate (p : HandheldProgram, v  : int Set) = 
    v 
    |> Seq.map (fun g -> terminates p g)
    |> Seq.choose id
    |> Seq.head

content 
    |> createProgram
    |> wayPoints
    |> findCandidate
    |> printfn "%A"