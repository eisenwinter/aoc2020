open System.IO
open System.Text.RegularExpressions
let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type PasswordRecord =  {
        min:int; 
        max:int; 
        mustContain:char; 
        password:string
    }

let regex = Regex(@"(?<min>[0-9]*)-(?<max>[0-9]*)\s(?<char>[a-z]?):\s(?<pwd>[a-z0-9]*)")
let toPwdRecord txt : PasswordRecord=
    let r = regex.Match txt
    {min = (int r.Groups.["min"].Value); max= (int r.Groups.["max"].Value); mustContain = char r.Groups.["char"].Value; password = r.Groups.["pwd"].Value}


let validatePassword record =
    record.password 
        |> Seq.toList
        |> Seq.filter (fun f -> f = record.mustContain)
        |> Seq.length
        |> fun l -> (>=) l record.min && (<=) l record.max

"input.txt"
    |> read
    |> Seq.map toPwdRecord
    |> Seq.filter validatePassword
    |> Seq.length
    |> printfn "Valid passwords: %i" 

