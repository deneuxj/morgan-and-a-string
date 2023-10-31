﻿let comparePrefixes (seqA : char seq, seqB : char seq) =
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
        | AdvanceA n ->
            assert (n > 0)
            (idxA + n, idxB)
        | AdvanceB n ->
            assert (n > 0)
            (idxA, idxB + n)

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

    let rec decide breakTie (idxA, idxB) =
        seq {
            if idxA < lineA.Length && idxB < lineB.Length then
                let a = lineA.[idxA]
                let b = lineB.[idxB]
                if a < b then
                    yield AdvanceA 1
                elif a > b then
                    yield AdvanceB 1
                else
                    assert(a = b)
                    let a2, n2 = tryGetNext (a, lineA, idxA + 1, 1)
                    let b2, m2 = tryGetNext (b, lineB, idxB + 1, 1)
                    let a2 = defaultArg a2 'z'
                    let b2 = defaultArg b2 'z'
                    if a2 > a && b2 > a then
                        // Put as much of 'a' as possible from both A and B
                        yield AdvanceA n2
                        yield AdvanceB m2
                    elif a2 < a && b2 < a then
                        // Put as little of 'a' as possible, next call to 'decide' will output a smaller char
                        if n2 < m2 then
                            yield AdvanceA n2
                        elif n2 > m2 then
                            yield AdvanceB m2
                        else
                            assert(n2 = m2)
                            if idxA + n2 = lineA.Length && idxB + m2 = lineB.Length then
                                // It doesn't matter which one we choose, both suffixes are identical
                                yield AdvanceA n2
                            else
                                // Same number of 'a' in both strings, choose depending on which of next char is smallest
                                if a2 < b2 then
                                    yield AdvanceA n2
                                elif a2 > b2 then
                                    yield AdvanceB m2
                                else
                                    yield! breakTie (idxA, idxB, n2)
                    elif a2 < a then
                        // Get to a2 as fast as possible
                        assert(b2 > a)
                        yield AdvanceA n2
                    else
                        // Get to b2 as fast as possible
                        assert(a2 > a && b2 < a)
                        yield AdvanceB m2
            elif idxA < lineA.Length then
                yield AdvanceA (lineA.Length - idxA)
            elif idxB < lineB.Length then
                yield AdvanceB (lineB.Length - idxB)
            else
                ()
        }

    let rec repeatDecide breakTie (idxA, idxB) =
        seq {
            let choices = decide breakTie (idxA, idxB)
            if Seq.isEmpty choices then
                ()
            else
                let idxA, idxB = ((idxA, idxB), choices) ||> Seq.fold updateIndices
                yield! choices
                yield! repeatDecide breakTie (idxA, idxB)
        }

    let cache = System.Collections.Generic.Dictionary<(int * int), Decision>()

    let rec breakTieBranch(idxA, idxB, n) =
        seq {
            match cache.TryGetValue((idxA, idxB)) with
            | true, decision ->
                yield decision
                let idxA, idxB = updateIndices (idxA, idxB) decision
                yield! decide breakTieBranch (idxA, idxB)
            | false, _ ->
                let choiceA = repeatDecide breakTieBranch (idxA + n, idxB)
                let seqA = getSequence (idxA + n, idxB) choiceA
                let choiceB = repeatDecide breakTieBranch (idxA, idxB + n)
                let seqB = getSequence (idxA, idxB + n) choiceB
                match comparePrefixes (seqA, seqB) with
                | Smaller ->
                    cache.Add((idxA, idxB), AdvanceA n)
                    yield AdvanceA n
                    yield! choiceA
                | Same | Larger ->
                    cache.Add((idxA, idxB), AdvanceB n)
                    yield AdvanceB n
                    yield! choiceB
        }

    let rec breakTieSingle(idxA, idxB, n) =
        seq {
            let ahead = decide breakTieSingle (idxA + n, idxB + n)
            match Seq.tryHead ahead with
            | Some (AdvanceA _) | None ->
                yield AdvanceA n
            | Some (AdvanceB _) ->
                yield AdvanceB n
        }

    let charsSingle =
        repeatDecide breakTieSingle (0, 0)
        |> getSequence (0, 0)

    let charsBranch =
        repeatDecide breakTieBranch (0, 0)
        |> getSequence (0, 0)

    // let iter = (Seq.zip charsSingle charsBranch).GetEnumerator()
    // while iter.MoveNext() do
    //     let x, y = iter.Current
    //     if x <> y then
    //         failwithf "Mismatch: %c <> %c" x y
    
    new string(Array.ofSeq charsBranch)

let morganAndString2 (lineA: string, lineB: string) =
    let outLen = lineA.Length + lineB.Length
    let rec yieldNextPos (n : int, active : int Set) =
        seq {
            // if n % 100 = 0 then
            //     eprintfn "n = %d out of %d, size = %d" n outLen active.Count
            if n >= outLen then
                ()
            else
                let bestChar, nextActive =
                    active
                    |> Seq.collect (fun idxA ->
                        seq {
                            if idxA < lineA.Length then
                                yield lineA.[idxA], idxA + 1
                            let idxB = n - idxA
                            if idxB < lineB.Length then
                                yield lineB.[idxB], idxA
                        }
                    )
                    |> Seq.groupBy fst
                    |> Seq.minBy fst
//                eprintf "%c" bestChar
                yield bestChar
                let nextActive =
                    nextActive
                    |> Seq.map snd
                    |> Set.ofSeq
                    |> Set.filter (fun idxA ->
                        let idxB = n + 1 - idxA
                        assert (idxB >= 0)
                        idxA = 0 || idxB = 0 || idxA + 1 >= lineA.Length || idxB + 1 >= lineB.Length ||
                        lineA.[idxA] <> lineA.[idxA - 1] || lineA.[idxA] <> lineA.[idxA + 1] ||
                        lineB.[idxB] <> lineB.[idxB - 1] || lineB.[idxB] <> lineB.[idxB + 1]
                    )
                yield! yieldNextPos (n + 1, nextActive)
        }
    yieldNextPos (0, Set [0])
    |> Array.ofSeq
    |> System.String

/// Depending on the environment, read from stdin or from a file
let getNextLine =
    match System.Environment.GetCommandLineArgs() |> List.ofArray |> List.tail with
    |  fileName :: _ ->
        let file = System.IO.File.OpenText(fileName)
        fun () -> file.ReadLine()
    | _ ->
        fun () -> System.Console.ReadLine()

let getNextExpected =
    match System.Environment.GetCommandLineArgs() |> List.ofArray |> List.tail with
    | _ :: fileName :: _ ->
        let file = System.IO.File.OpenText(fileName)
        fun () -> file.ReadLine() |> Some
    | _ ->
        fun () -> None

// Main program
let numTestCases = getNextLine() |> int
let mutable success = true

for i in 1..numTestCases do
    let lineA = getNextLine()
    let lineB = getNextLine()
    let result = morganAndString2(lineA, lineB)
    printfn "%s" result
    match getNextExpected() with
    | Some expected ->
        if result <> expected then
            success <- false
            failwithf "Test %d failed: Expected \n%s, got \n%s" i expected result
    | None ->
        ()

if not success then
    failwith "Test failed"
