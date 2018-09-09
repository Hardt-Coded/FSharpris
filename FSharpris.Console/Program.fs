namespace FSharpris.Console


module Program =


    open FSharpris.Domain
    open System
    open System.Runtime
    open System

    let mutable lastPressedKey = 0

    let sharpLine = String.init (fieldWidth+8) (fun _ -> "#")

    let rowToString row =
        ("###|",row) 
        ||> List.fold (fun state item ->
            let ch = (if (item > 0) then "#" else " ")
            sprintf "%s%s" state ch
            )
        |> sprintf "%s|###"

    let drawTitle ()= 
        Console.WriteLine(sharpLine)
        Console.WriteLine("FSharpris")
        Console.WriteLine(sharpLine)
    
    let drawState gamemodel =
        Console.Write("State: ")
        match gamemodel.GameState with
        |New -> Console.WriteLine("New")
        |Running -> Console.WriteLine("Running")
        |Stopped -> Console.WriteLine("Stopped")

    
    let drawMatrix matrix =
        matrix
        |> List.iter (fun row ->
                row |> rowToString |> Console.WriteLine
            )


    let drawGame gamemodel =
        let gamefield = gamemodel.GameField
        let brickState = gamemodel.BrickState
        match brickState with
        |NoBrick -> 
            let (Gamefield newMatrix) = gamefield
            drawMatrix newMatrix
        |Brick (_,_,_,_,brickLayer) ->
            let flattenGamefield =  gamefield |> Helpers.flattenGamefield
            let flattenBrickLayer = brickLayer |> Helpers.flattenBricklayer
            
            // merge both
            let newMatrix =
                (flattenGamefield, flattenBrickLayer)
                ||> Helpers.orOperationOnFlattenFields
                |> Helpers.unflatten
            
            drawMatrix newMatrix
            
        
    
    


    let render gamemodel =
        Console.SetCursorPosition(0,0)
        drawTitle()
        gamemodel |> drawState
        gamemodel |> drawGame 
        
        
        


    let rnd = Random()

    let randmomRotation () =
        let r = rnd.Next(1,4)
        match r with
        | 1 -> R0
        | 2 -> R90
        | 3 -> R180
        | 4 -> R270
        | _ -> R0
    
    let randomX () =
        rnd.Next(1,fieldWidth)

    let randomBrick () =
        let r = rnd.Next(1,7)
        match r with
        | 1 -> I
        | 2 -> J
        | 3 -> L
        | 4 -> O
        | 5 -> S
        | 6 -> T
        | 7 -> Z
        | _ -> failwith "wtf? rnd whats happen?"

    let gameLoop (gameCmd:unit->GameCommands) (innerGameCmd:unit->RunningGameCommands) gamemodel =
        let rec loop gamemodel count = async {
            
            gamemodel |> render
            do! Async.Sleep 50
            gamemodel |> render
            match gamemodel.GameState with
            | New -> 
                match gameCmd() with
                | StartGame -> return! loop (gamemodel |> startGame) count
                | StoppGame  -> return! loop gamemodel count
                | Nothing -> return! loop gamemodel count
            | Running ->
                
                let gamemodel = 
                    if (count % 10L = 0L || count = 0L) then
                        gamemodel |> nextInnerGameMove randmomRotation randomBrick randomX
                    else
                        gamemodel
                match gameCmd() with
                | StartGame -> return! loop (gamemodel |> innerGameCommandHandler (innerGameCmd())) (count + 1L)
                | StoppGame  -> return! loop (gamemodel |> stoppGame) (count + 1L)
                | Nothing ->
                    return! loop (gamemodel |> innerGameCommandHandler (innerGameCmd())) (count + 1L)
            | Stopped -> 
                match gameCmd() with
                | StartGame -> return! loop (gamemodel |> startGame) count
                | StoppGame  -> return! loop gamemodel count
                | Nothing -> return! loop gamemodel count
        }

        loop gamemodel 0L |> Async.Start

    let gameCommand () =
        StartGame

    let randomInnerCommands () =
        let r = rnd.Next(1,4)
        match r with
        | 1 -> MoveLeft
        | 2 -> MoveRight
        | 3 -> Rotate
        | _ -> DoNothing
    
    let (|KeyA|KeyD|KeyS|Space|None|) keyInt = 
        if (ConsoleKey.A |> int = keyInt) then KeyA
        elif (ConsoleKey.D |> int = keyInt) then KeyD
        elif (ConsoleKey.S |> int = keyInt) then KeyS
        elif (ConsoleKey.Spacebar |> int = keyInt) then Space
        else None


    let keyboardInnerCommands () =        
        let cmd =
            match lastPressedKey with
            | KeyA -> MoveLeft
            | KeyD -> MoveRight
            | KeyS -> MoveDown
            | Space -> Rotate
            | _ -> DoNothing
        
        lastPressedKey <- 0
        cmd


    [<EntryPoint>]
    let main argv = 
        Console.Clear()
        let gameModel = initGame()
        
        gameModel |> gameLoop gameCommand keyboardInnerCommands

        // getting key entries
        Seq.initInfinite (fun i -> 0) 
        |> Seq.iter (fun _ ->
            lastPressedKey <-  Console.ReadKey(true).Key |> int
        )

        Console.ReadLine() |> ignore
        0 // return an integer exit code
