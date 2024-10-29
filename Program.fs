open System

// Counter data structure (see Python's collections.Counter)
type Counter(counts: Map<char, int>) =
    // counts character occurences in a word, if the score there is 0
    static member FromString(ans: string, score: seq<int>) =
        let notScored = Seq.countBy (fun (c, n) -> if n = 0 then c else ' ')
        Counter((Seq.zip ans score) |> notScored |> Map.ofSeq)

    // the count of a character
    member this.Count(c: char) = Map.tryFind c counts |> Option.defaultValue 0

    // decrement the count for a given character
    member this.Decrement(c: char) =
        let count = Map.tryFind c counts |> Option.defaultValue 0
        if count > 0 then
            Counter(Map.add c (count - 1) counts)
        else
            this  // doesn't decrement a count of 0

let mark guess ans = 
    let markGreen = Seq.map2 (fun c1 c2 -> if c1 = c2 then 2 else 0)
    let markYellow guess ans score = 
        // Counter avoids repeatedly assigning yellow
        let mutable unused = Counter.FromString(ans, score)
        let letterScore c n = if n = 0 && unused.Count c > 0 then 
                                unused <- unused.Decrement(c)
                                1 
                              else n
        Seq.map2 letterScore guess score

    // use the score from `markGreen` to correctly `markYellow`
    markGreen guess ans |> markYellow guess ans

let scoreMatches score guess ans = Seq.forall2 (=) score (mark guess ans)

let numLeft guess score answers = 
    let sameScore ans = if scoreMatches score guess ans then 1 else 0
    answers |> Array.Parallel.map sameScore |> Array.sum

let rateGuess answers guess =
    let numAnswersLeft ans = numLeft guess (mark guess ans) answers
    answers |> Array.map numAnswersLeft |> Array.sum 

let bestGuess answers guesses =
    guesses |> Array.map (rateGuess answers) |> Array.zip guesses |> Array.minBy snd |> fst

let mutable guess = "roate"
let mutable answers = IO.File.ReadAllLines("answers.txt")
let guesses = IO.File.ReadAllLines("guesses.txt")

while Seq.length answers > 1 do
    printfn "The best guess is: '%s'" guess
    printf "Enter the score i.e. 00102: "
    let score = Console.ReadLine() |> Seq.map (fun c -> int (c - '0'))

    answers <- Array.filter (scoreMatches score guess) answers
    guess <- bestGuess answers guesses

printfn "The best word is %A" answers
