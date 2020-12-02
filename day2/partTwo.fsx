open System.IO
open System.Text.RegularExpressions
let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type passwordRecord =  {
        posOne:int; 
        posTwo:int; 
        mustContain:char; 
        password:string
    }

let regex = new Regex(@"(?<min>[0-9]*)-(?<max>[0-9]*)\s(?<char>[a-z]?):\s(?<pwd>[a-z0-9]*)")
let toPwdRecord txt : passwordRecord=
    let r = regex.Match txt
    {
        posOne = (int r.Groups.["min"].Value); 
        posTwo = (int r.Groups.["max"].Value); 
        mustContain = char r.Groups.["char"].Value; 
        password = r.Groups.["pwd"].Value
    }


let validatePassword record =
     (record.password.[record.posOne-1] = record.mustContain) <> (record.password.[record.posTwo-1] = record.mustContain)
 
"input.txt"
    |> read
    |> Seq.map toPwdRecord
    |> Seq.filter validatePassword
    |> Seq.length
    |> printfn "Valid passwords: %i" 