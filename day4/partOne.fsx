open System.IO
open System.Text.RegularExpressions

type Passport = (string * string) list 


let regex = Regex(@"(?<passport>((byr|iyr|eyr|hgt|hcl|ecl|pid|cid):[a-zA-Z0-9#]+(\s|\r\n|))*)(^\s*$|\z)", RegexOptions.Multiline)

let resolve (x : string) : Passport = 
    x 
    |> Regex(@"(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):[a-zA-Z0-9#]+").Matches
    |> Seq.cast
    |> Seq.filter (fun (f : Match) -> f.Success)
    |> Seq.map (fun x -> x.Value.Split([| ':' |]))
    |> Seq.map  (fun x -> (x.[0], x.[1]))
    |> Seq.toList


let createPassport (m : Match)  : Passport option = 
    match System.String.IsNullOrWhiteSpace m.Groups.["passport"].Value with
        | true -> None 
        | _ ->  Some (resolve m.Groups.["passport"].Value)

let validate passport : bool =
    let d = dict passport
    [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    |> Seq.fold (fun v e -> d.ContainsKey(e) && v) true 

File.ReadAllText("input.txt")
    |> regex.Matches
    |> Seq.cast
    |> Seq.map createPassport
    |> Seq.choose id
    |> Seq.filter validate
    |> Seq.length
    |> printfn "%A"