open System.IO
open System.Text.RegularExpressions
let read (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

type PasswordRecord =  {
        PosOne:int; 
        PosTwo:int; 
        MustContain:char; 
        Password:string
    }

let regex = Regex(@"(?<min>[0-9]*)-(?<max>[0-9]*)\s(?<char>[a-z]?):\s(?<pwd>[a-z0-9]*)")
let toPwdRecord txt : PasswordRecord=
    let r = regex.Match txt
    {
        PosOne = (int r.Groups.["min"].Value); 
        PosTwo = (int r.Groups.["max"].Value); 
        MustContain = char r.Groups.["char"].Value; 
        Password = r.Groups.["pwd"].Value
    }


let validatePassword record =
     (record.Password.[record.PosOne-1] = record.MustContain) <> (record.Password.[record.PosTwo-1] = record.MustContain)
 
"input.txt"
    |> read
    |> Seq.map toPwdRecord
    |> Seq.filter validatePassword
    |> Seq.length
    |> printfn "Valid passwords: %i" 