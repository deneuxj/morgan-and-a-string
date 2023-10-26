
let morganAndString (lineA : string, lineB : string) =
    let rec work (idxA, idxB) : char seq =
        if idxA < lineA.Length && idxB < lineB.Length then
            let a = lineA.[idxA]
            let b = lineB.[idxB]
            if a < b then
                seq {
                    yield a
                    yield! work (idxA + 1, idxB)
                }
            elif a > b then
                seq {
                    yield b
                    yield! work (idxA, idxB + 1)
                }
            else
                let seqA = work (idxA + 1, idxB)
                let seqB = work (idxA, idxB + 1)
                let seqAB = Seq.zip seqA seqB
                let rec selectFirst (seq : seq<char * char>) =
                    match Seq.isEmpty seq with
                    | true -> seqA
                    | false ->
                        let (a, b) = Seq.head seq
                        if a < b then seqA
                        elif a > b then seqB
                        else selectFirst (Seq.skip 1 seq)
                seq {
                    yield a
                    yield! selectFirst seqAB
                }
        elif idxA < lineA.Length then
            lineA.[idxA..] :> char seq
        else
            lineB.[idxB..] :> char seq
    let chars = work (0, 0)
    new string(Array.ofSeq chars)

let numTestCases = System.Console.ReadLine() |> int
for i in 1..numTestCases do
    let lineA = System.Console.ReadLine()
    let lineB = System.Console.ReadLine()
    let result = morganAndString(lineA, lineB)
    printfn "%s" result
