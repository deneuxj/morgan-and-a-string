let morganAndString2 (lineA: string, lineB: string) =
    let inline updateAcc ((lowestChar, res) as acc) (c, idx) =
        if c < lowestChar then
            (c, [idx])
        else if c = lowestChar then
            (lowestChar, idx :: res)
        else
            acc

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
    let rec yieldNextPos (n : int, active : int list) =
        seq {
            // if n % 100 = 0 then
            //     eprintfn "n = %d out of %d, size = %d" n outLen active.Count
            if n >= outLen then
                ()
            else
                let bestChar, nextActive =
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
