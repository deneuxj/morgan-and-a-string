let morganAndString2 (lineA: string, lineB: string) =
    // Update the lowest character and the list of positions where it occurs.
    // The list of positions is sorted and can contain duplicates.
    let inline updateAcc ((lowestChar, res) as acc) (c, idx) =
        if c < lowestChar then
            (c, [idx])
        else if c = lowestChar then
            (lowestChar, idx :: res)
        else
            acc

    // In a sorted list, remove duplicates.
    let inline sortedDistinct (idxs : int list) =
        let rec work prev rest =
            match rest with
            | [] -> []
            | h :: t ->
                if h = prev then
                    work prev t
                else
                    h :: work h t
        work (-1) idxs

    let outLen = lineA.Length + lineB.Length

    // n is the number of characters we have already output, active is a list of positions in line A
    // It implicitly defines the positions in line B.
    // Each such pair of positions in line A and B defines the next candidate character to output.
    // There are multiple pairs because of ties between characters in line A and B.
    // As we output characters, we update the list of active positions by throwing away positions that have bad candidates, and adding new positions from the good candidates.
    // We also keep the active list sorted and distinct.
    let rec yieldNextPos (n : int, active : int list) =
        seq {
            // if n % 100 = 0 then
            //     eprintfn "n = %d out of %d, size = %d" n outLen active.Count
            if n >= outLen then
                ()
            else
                let bestChar, nextActive =
                    // 'z' is higher than any other character we can encounter, so it will always be replaced by a better candidate if possible.
                    (('z', []), active)
                    ||> List.fold (fun acc idxA ->
                        let acc =
                            if idxA < lineA.Length then
                                let a = lineA.[idxA]
                                updateAcc acc (a, idxA + 1)
                            else
                                acc
                        let idxB = n - idxA
                        if idxB < lineB.Length then
                            let b = lineB.[idxB]
                            updateAcc acc (b, idxA)
                        else
                            acc
                    )
                let nextActive =
                    nextActive
                    |> sortedDistinct
                    // In situations where line A and B have long sequences of the same character, there is no point in interleaving advances in them.
                    // To avoid that, we throw away pairs of positions that have the same character as the previous and next position in both lines.
                    // In other words, at least one of the pointers must be at the start or end of a sequence of the same character.
                    |> List.filter (fun idxA ->
                        let idxB = n + 1 - idxA
                        assert (idxB >= 0)
                        idxA = 0 || idxB = 0 || idxA + 1 >= lineA.Length || idxB + 1 >= lineB.Length ||
                        lineA.[idxA] <> lineA.[idxA - 1] || lineA.[idxA] <> lineA.[idxA + 1] ||
                        lineB.[idxB] <> lineB.[idxB - 1] || lineB.[idxB] <> lineB.[idxB + 1]
                    )
//                eprintf "%c" bestChar
                yield bestChar
                yield! yieldNextPos (n + 1, nextActive)
        }
    yieldNextPos (0, [0])
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
