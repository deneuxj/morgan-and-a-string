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

let (|Smaller|Same|Larger|) n =
    if n < 0 then
        Smaller
    elif n > 0 then
        Larger
    else
        Same
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

    let rec tryGetNext (c, line : string, idx, n) =
        if idx >= line.Length then
            None, n
        else
            let c2 = line.[idx]
            if c2 = c then
                tryGetNext (c, line, idx + 1, n + 1)
            else
                Some c2, n

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
                    let a = lineA.[idxA]
                    let b = lineB.[idxB]
                    if a < b then
                        yield AdvanceA 1
                        yield! decide(idxA + 1, idxB)
                    elif a > b then
                        yield AdvanceB 1
                        yield! decide(idxA, idxB + 1)
                    else
                        let a2, n2 = tryGetNext (a, lineA, idxA + 1, 1)
                        let b2, m2 = tryGetNext (b, lineB, idxB + 1, 1)
                        let a2 = defaultArg a2 'z'
                        let b2 = defaultArg b2 'z'
                        if a2 > a && b2 > a then
                            // Put as much of 'a' as possible from both A and B
                            yield AdvanceA n2
                            yield AdvanceB m2
                            yield! decide(idxA + n2, idxB + m2)
                        elif a2 < a && b2 < a then
                            // Put as little of 'a' as possible, next call to 'decide' will output a smaller char
                            if n2 < m2 then
                                yield AdvanceA n2
                                yield! decide(idxA + n2, idxB)
                            elif n2 > m2 then
                                yield AdvanceB m2
                                yield! decide(idxA, idxB + m2)
                            else
                                assert(n2 = m2)
                                // Same number of 'a' in both strings, choose depending on which of next char is smallest
                                if a2 < b2 then
                                    yield AdvanceA n2
                                    yield! decide(idxA + n2, idxB)
                                elif a2 > b2 then
                                    yield AdvanceB m2
                                    yield! decide(idxA, idxB + m2)
                                else
                                    // Can't decide depending on next char, try each decision and see which one is right
                                    let choiceA = decide(idxA + n2, idxB)
                                    let seqA = getSequence (idxA + n2, idxB) choiceA
                                    let choiceB = decide(idxA, idxB + m2)
                                    let seqB = getSequence (idxA, idxB + m2) choiceB
                                    if comparePrefixes (seqA, seqB) < 0 then
                                        yield AdvanceA n2
                                        yield! choiceA
                                    else
                                        yield AdvanceB m2
                                        yield! choiceB
                        elif a2 < a then
                            // Get to a2 as fast as possible
                            assert(b2 > a)
                            yield AdvanceA n2
                            yield! decide(idxA + n2, idxB)
                        else
                            // Get to b2 as fast as possible
                            assert(a2 > a && b2 < a)
                            yield AdvanceB m2
                            yield! decide(idxA, idxB + m2)
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
