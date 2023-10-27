
let morganAndString (lineA : string, lineB : string) =
    let rec skipEquals eqChar (idxA, idxB) n =
        if idxA < lineA.Length && idxB < lineB.Length then
            let a = lineA.[idxA]
            let b = lineB.[idxB]
            if a = eqChar && b = eqChar then
                // Keep looking for next position where one of the lines ends or differs from eqChar
                skipEquals eqChar (idxA + 1, idxB + 1) (n + 1)
            elif eqChar <= a && eqChar <= b then
                // Advance both lines to put as many smaller eqChars first.
                n, n
            elif a <= eqChar && a <= b then
                // Advance line A to get to smaller a as soon as possible.
                n, 0
            else
                assert(b <= eqChar && b <= a)
                // Advance line B to get to smaller b as soon as possible.
                0, n
        elif idxA < lineA.Length then
            let a = lineA.[idxA]
            if eqChar <= a then
                // Advance both lines to put as many smaller eqChars before a
                n, n
            else
                // Advance line A to get to smaller a as soon as possible.
                n, 0
        elif idxB < lineB.Length then
            let b = lineB.[idxB]
            if eqChar <= b then
                // Advance both lines to put as many smaller eqChars before b
                n, n
            else
                // Advance line B to get to smaller b as soon as possible.
                0, n
        else
            // Advance both lines to the end
            n, n

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
                let advA, advB = skipEquals a (idxA + 1, idxB + 1) 1
                seq {
                    yield! Seq.init (advA + advB) (fun _ -> a)
                    yield! work (idxA + advA, idxB + advB)
                }
        elif idxA < lineA.Length then
            lineA.[idxA..] :> char seq
        else
            lineB.[idxB..] :> char seq

    let chars = work (0, 0)
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
