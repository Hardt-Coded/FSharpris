namespace FSharpris.Console


module Program =


    open FSharpris.Domain
    open System
    open System.Runtime
    open System

    let mutable lastPressedKey = 0

    let sharpLine = String.init (fieldWidth+8) (fun _ -> "#")

    let stoneInColor color =
            Console.BackgroundColor <- color
            Console.Write(" ")
            Console.BackgroundColor <- ConsoleColor.Black
    
    let renderStone i =
        match i with            
        | 1 -> stoneInColor ConsoleColor.Cyan
        | 2 -> stoneInColor ConsoleColor.Blue
        | 3 -> stoneInColor ConsoleColor.Magenta
        | 4 -> stoneInColor ConsoleColor.Yellow
        | 5 -> stoneInColor ConsoleColor.Green
        | 6 -> stoneInColor ConsoleColor.White
        | 7 -> stoneInColor ConsoleColor.Red
        | _ -> stoneInColor ConsoleColor.Black

    let rowToString row =
        
        Console.Write("###|")
        row |> List.iter (renderStone)
        Console.WriteLine("|###")
        

    let drawTitle ()=         
        Console.WriteLine("FSharpris")
        Console.WriteLine(sharpLine)

    let renderPreview gamemodel =
        match gamemodel.PreviewNextBrick with
        | NoPreviewNextBrick -> ()
        | PreviewNextBrick (brick,rotation) ->
            Console.WriteLine("Next Brick:")
            let brick =
                brick
                |> createBrick rotation
            let restLines = 4 - brick.Length
            brick
            |> List.iter (fun row -> 
                row |> List.iter (renderStone)
                [1..5] |> List.iter (fun _ -> stoneInColor ConsoleColor.Black)
                Console.WriteLine()
            )
            // remove res lines
            [1..restLines] |> List.iter (fun _ -> Console.WriteLine("        "))
    
    let drawState gamemodel =
        Console.Write("State: ")
        match gamemodel.GameState with
        |New -> Console.WriteLine("New    ")
        |Running -> Console.WriteLine("Running")
        |Pause -> Console.WriteLine("Pause  ")
        |Lost -> Console.WriteLine("Stopped")   
        let (Level level) = gamemodel.Level
        let (Score score) = gamemodel.Score        
        Console.Write("Level: {0} ## ",level)
        Console.Write("Score: {0} ## ",score)
        Console.WriteLine("RemovedLines: {0}",gamemodel.CountRemovedLines)
        renderPreview gamemodel
    
    let drawMatrix matrix =
        matrix
        |> List.iter (fun row ->
                row |> rowToString
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

    let randomRotation () =
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
            let (Level levelInt) = gamemodel.Level
            gamemodel |> render
            do! Async.Sleep (50 / (levelInt + 1))
            gamemodel |> render
            match gamemodel.GameState with
            | New -> 
                match gameCmd() with
                | StartGame -> return! loop (gamemodel |> startGame) count
                | StoppGame  -> return! loop gamemodel count                
                | Nothing -> return! loop gamemodel count
            | Pause ->
                return! loop (gamemodel |> innerGameCommandHandler (innerGameCmd())) (count + 1L)
            | Running ->
                
                let gamemodel = 
                    if (count % 10L = 0L || count = 0L) then
                        gamemodel |> nextInnerGameMove gamemodel.Level randomRotation randomBrick randomX
                    else
                        gamemodel
                match gameCmd() with
                | StartGame -> return! loop (gamemodel |> innerGameCommandHandler (innerGameCmd())) (count + 1L)
                | StoppGame  -> return! loop (gamemodel |> stoppGame) (count + 1L)
                | Nothing ->
                    return! loop (gamemodel |> innerGameCommandHandler (innerGameCmd())) (count + 1L)
            | Lost -> 
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
    
    let (|KeyA|KeyD|KeyS|Space|KeyP|None|) keyInt = 
        if (ConsoleKey.A |> int = keyInt) then KeyA
        elif (ConsoleKey.D |> int = keyInt) then KeyD
        elif (ConsoleKey.S |> int = keyInt) then KeyS
        elif (ConsoleKey.Spacebar |> int = keyInt) then Space
        elif (ConsoleKey.P |> int = keyInt) then KeyP
        else None


    let keyboardInnerCommands () =        
        let cmd =
            match lastPressedKey with
            | KeyA -> MoveLeft
            | KeyD -> MoveRight
            | KeyS -> MoveDown
            | KeyP -> TogglePause
            | Space -> Rotate
            | _ -> DoNothing
        
        lastPressedKey <- 0
        cmd


    [<EntryPoint>]
    let main argv = 
        Console.Clear()
        let gameModel = initGame randomBrick randomRotation true 0
        
        gameModel |> gameLoop gameCommand keyboardInnerCommands

        // getting key entries
        Seq.initInfinite (fun i -> 0) 
        |> Seq.iter (fun _ ->
            lastPressedKey <-  Console.ReadKey(true).Key |> int
        )

        Console.ReadLine() |> ignore
        0 // return an integer exit code
