let comparePrefixes (seqA : char seq, seqB : char seq) =
    let it1 = seqA.GetEnumerator()
    let it2 = seqB.GetEnumerator()
    let rec work() =
        let hasNext1 = it1.MoveNext()
        let hasNext2 = it2.MoveNext()
        if hasNext1 && hasNext2 then
            let a = it1.Current
            let b = it2.Current
            if a < b then -1
            elif a > b then 1
            else work()
        elif hasNext1 then
            1
        elif hasNext2 then
            -1
        else
            0
    work()

[<Struct>]
type Decision =
    | AdvanceA of xa:int
    | AdvanceB of xb:int

let morganAndString (lineA : string, lineB : string) =
    let rec commonConstantPrefixLength c (idxA, idxB) n =
        if idxA < lineA.Length && idxB < lineB.Length then
            let a = lineA.[idxA]
            let b = lineB.[idxB]
            if a = b && b = c then
                commonConstantPrefixLength c (idxA + 1, idxB + 1) (n + 1)
            else
                n
        else
            n

    let getSelectedChars (idxA, idxB) choice : char seq =
        seq {
            let s, idx =
                match choice with
                | AdvanceA _ -> (lineA, idxA)
                | AdvanceB _ -> (lineB, idxB)
            let n =
                match choice with
                | AdvanceA n | AdvanceB n -> n
            for i in 0..n - 1 do
                yield s.[idx + i]
        }

    let updateIndices (idxA, idxB) choice =
        match choice with
        | AdvanceA n -> (idxA + n, idxB)
        | AdvanceB n -> (idxA, idxB + n)

    let getSequence (idxA, idxB) choices =
        seq {
            let mutable idxA = idxA
            let mutable idxB = idxB
            for choice in choices do
                let chars = getSelectedChars (idxA, idxB) choice
                yield! chars
                let idxA', idxB' = updateIndices (idxA, idxB) choice
                idxA <- idxA'
                idxB <- idxB'
        }

    let cache = System.Collections.Generic.Dictionary<(int * int), Decision>()

    let rec decide(idxA, idxB) =
        seq {
            match cache.TryGetValue((idxA, idxB)) with
            | true, res ->
                yield res
                yield! decide(updateIndices (idxA, idxB) res)
            | false, _ ->
                if idxA < lineA.Length && idxB < lineB.Length then
                    let choice, nextChoices =
                        let a = lineA.[idxA]
                        let b = lineB.[idxB]
                        if a < b then
                            AdvanceA 1, []
                        elif a > b then
                            AdvanceB 1, []
                        else
                            let skipLen = commonConstantPrefixLength a (idxA, idxB) 0
                            assert (skipLen > 0)
                            let indicesA = updateIndices (idxA, idxB) (AdvanceA skipLen)
                            let indicesB = updateIndices (idxA, idxB) (AdvanceB skipLen)
//                            let indicesX = updateIndices indicesA (AdvanceB skipLen)
//                            let seqBoth = decide indicesX |> getSequence indicesX
                            let seqA = decide indicesA |> getSequence indicesA
                            let seqB = decide indicesB |> getSequence indicesB
                            let cmpAB = comparePrefixes (seqA, seqB)
                            // let cmpBX = cmpCharSeqs (seqB, seqBoth)
                            // let cmpAX = cmpCharSeqs (seqA, seqBoth)
                            // if cmpAB <= 0 && cmpAX <= 0 then
                            //     AdvanceA skipLen, []
                            // elif cmpBX <= 0 && cmpAB >= 0 then
                            //     AdvanceB skipLen, []
                            // else
                            //     AdvanceA skipLen, [AdvanceB skipLen]
                            if cmpAB < 0 then
                                AdvanceA skipLen, []
                            else
                                AdvanceB skipLen, []
                    cache.Add((idxA, idxB), choice)
                    yield choice
                    let idxA, idxB = ((idxA, idxB), choice :: nextChoices) ||> Seq.fold updateIndices
                    yield! decide(idxA, idxB)
                elif idxA < lineA.Length then
                    yield AdvanceA (lineA.Length - idxA)
                elif idxB < lineB.Length then
                    yield AdvanceB (lineB.Length - idxB)
                else
                    ()
        }


    let chars =
        decide(0, 0)
        |> getSequence (0, 0)
    new string(Array.ofSeq chars)

/// Depending on the environment, read from stdin or from a file
let getNextLine =
    match System.Environment.GetCommandLineArgs() with
    | [| _; fileName |] ->
        let file = System.IO.File.OpenText(fileName)
        fun () -> file.ReadLine()
    | _ ->
        fun () -> System.Console.ReadLine()

// Main program
let numTestCases = getNextLine() |> int
for i in 1..numTestCases do
    let lineA = getNextLine()
    let lineB = getNextLine()
    let result = morganAndString(lineA, lineB)
    printfn "%s" result
