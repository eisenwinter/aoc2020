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



let rec getAlias (f : string) (l : (string * (int * string)list)list) =
    let a = l 
            |> List.map (fun (g,h) -> g, h |> List.map (fun k -> snd k))
            |> List.filter (fun (_,h) -> h |> List.contains f)
            |> List.map fst 
            |> (@) [f] 
    let intersect a b =
        Set.intersect (Set.ofList a) (Set.ofList b)
        |> Set.isEmpty
        |> not
    l 
    |> List.map (fun (g,h) -> g, (h |> List.map (fun k -> snd k)))
    |> List.filter (fun (_,h) -> intersect h a)
    |> List.map fst
    |> List.fold (fun g h -> g @ [h] @ (getAlias h l) ) []
    |> List.distinct


content
    |> map' 
    |> Seq.toList
    |> getAlias "shiny gold"
    |> Seq.length
    |> printfn "%A"


