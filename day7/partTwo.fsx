open System.IO
open System.Text.RegularExpressions
let read (filePath:string) = System.IO.File.ReadAllText(filePath)
let content = "input.txt" |> read

let trim' (s : string) : string =
    s.Trim([|'\n' ;'\r' |])

let map' (s : string) =
    let grpIfExi (m : Match) (grp : string) =  
        match System.String.IsNullOrWhiteSpace m.Groups.[grp].Value with
        | true -> None 
        | _ ->  Some m.Groups.[grp].Value
    s
    |> Regex(@"(?<main>[a-z\s]+)\sbags contain (no other bags|(?<inner>[0-9]\s[a-z\s]*bag(s|)(,|.)*))").Matches
    |> Seq.cast
    |> Seq.filter (fun (f : Match) -> f.Success)
    |> Seq.map (fun f -> (trim' f.Groups.["main"].Value, grpIfExi f "inner"))
    |> Seq.map (fun f -> match f with 
                         | (h, Some x) -> (h, x 
                                                |> Regex(@"(?<inner>(?<count>[0-9]+)\s(?<color>[a-z\s]*))\sbag(s|)(,|.)").Matches 
                                                |> Seq.cast 
                                                |> Seq.map(fun (g : Match) -> (int g.Groups.["count"].Value,g.Groups.["color"].Value))
                                                |> Seq.toList)
                         | (h, None) -> (h, []))

let c' (t : (int * string)) : int = 
    match t with
    | (x , _) -> x


let rec getCount (d : int) (f : string) (l : (string * (int * string)list)list) =
    let s' = l 
            |> List.filter (fun g -> fst g = f)
            |> List.collect snd
            |> List.sumBy c'
    let children =
        l
        |> List.filter (fun g -> fst g = f)
        |> List.collect snd
        |> List.collect (fun (k,v) -> getCount (k) v l)

    [s'] @ children
    |> List.map (fun g -> g * d)

content
    |> map' 
    |> Seq.toList
    |> getCount 1 "shiny gold"
    |> Seq.sum
    |> printfn "%A"

