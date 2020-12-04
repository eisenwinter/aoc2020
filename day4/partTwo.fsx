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


let validHeight (x : string) : bool =
    match x.Substring(x.Length - 2, 2) with
        | "in" -> (int (x.Substring(0,x.Length-2))) >= 59 && (int (x.Substring(0,x.Length-2)))  <= 76
        | "cm" -> (int (x.Substring(0,x.Length-2))) >= 150 && (int (x.Substring(0,x.Length-2)))  <= 193
        | _ -> false


let validate' tpl : bool =
    match tpl with 
        | ("byr", x) when (int x) >= 1920 && (int x) <= 2002 -> true
        | ("iyr", x) when (int x) >= 2010 && (int x) <= 2020 -> true
        | ("eyr", x) when (int x) >= 2020 && (int x) <= 2030 -> true
        | ("hgt", x) when Regex("([0-9]{2}in|[0-9]{3}cm)").IsMatch(x) -> validHeight x
        | ("hcl", x) when Regex("#[0-9a-f]{6}").IsMatch(x) -> true
        | ("ecl", "amb") -> true
        | ("ecl", "blu") -> true
        | ("ecl", "brn") -> true
        | ("ecl", "gry") -> true
        | ("ecl", "grn") -> true
        | ("ecl", "hzl") -> true
        | ("ecl", "oth") -> true
        | ("pid", x) when Regex("[0-9]{9}").IsMatch(x) -> true
        | ("cid", _) -> true
        | _ -> false

let validate passport : bool =
    let d = dict passport
    [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
    |> Seq.forall (fun e -> d.ContainsKey(e)) 

let validateFields passport : bool =
    passport 
    |> Seq.forall validate'

File.ReadAllText("input.txt")
    |> regex.Matches
    |> Seq.cast
    |> Seq.map createPassport
    |> Seq.choose id
    |> Seq.filter validate
    |> Seq.filter validateFields
    |> Seq.length
    |> printfn "%A" //131 correct - why 