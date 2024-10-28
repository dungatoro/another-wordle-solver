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
    
let ans = "dimps" 
let guess = "tiddy" 
printfn "%A" (mark guess ans |> Seq.toList)
