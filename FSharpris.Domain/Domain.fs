namespace FSharpris

module Domain =

    let fieldWidth = 10
    let fieldHeight = 18
    

    type BrickType = I | J | L | O | S | T | Z
    type Rotation = R0 | R90 | R180 | R270
    type Move = Left | Right | Down
    type Collision = Collision | NoCollision

    type GameField = Gamefield of matrix: int list list
    type BrickLayer = Bricklayer of matrix: int list list

    type BrickState =
        | NoBrick 
        | Brick of BrickType * rotation:Rotation * x:int * y:int * matrix:BrickLayer
    

    type FlattenFields = FlattenFields of int list
    module FlattenFields =
        let unwrap (FlattenFields f) = f
        let create f = FlattenFields f

    type GameState = New | Running | Pause |Lost

    type Level = Level of int
    module Level =
        let unwrap (Level l) = l
        let create l = Level l

    type Score = Score of int
    module Score =
        let unwrap (Score s) = s
        let create s = Score s

    
    type PreviewNextBrick = 
        | NoPreviewNextBrick
        | PreviewNextBrick of BrickType * Rotation
        

    type GameModel = {
        GameState:GameState
        BrickState: BrickState
        GameField:GameField
        PreviewNextBrick:PreviewNextBrick        
        Level:Level
        Score:Score
        CountRemovedLines:int
    }

    type GameCommands =
        | StartGame        
        | StoppGame
        | Nothing

    type RunningGameCommands =
        | MoveLeft
        | MoveRight
        | MoveDown
        | Rotate
        | TogglePause
        | DoNothing

    let emptyLine = [ for _ in [1..fieldWidth] do yield 0]    
    // 2 dimensional matrix filled with 0
    let emptyGameField = Gamefield ([ for _ in [1..fieldHeight] do yield emptyLine  ])
    let emptyBrickLayer = Bricklayer [ for _ in [1..fieldHeight] do yield emptyLine ]

    let newGameModel = {
        GameState = New
        BrickState = NoBrick
        GameField = emptyGameField
        PreviewNextBrick = NoPreviewNextBrick        
        Level = Level 0
        Score = Score 0
        CountRemovedLines = 0
    }

    // Helper Split function
    // this function runs recursivly and cuts the number of items from the list as long nothing is left
    // Seq.truncate gives the max num of entries back. Other than Seq.take, it return also the rest of your items
    // when your list have lesser items

    let split length (xs: #seq<'T>) =
        let rec loop xs =
            [
                yield Seq.truncate length xs |> Seq.toList
                match Seq.length xs <= length with
                | false -> yield! loop (Seq.skip length xs)
                | true -> ()
            ]
        loop xs




    let createBrick rotation brick =
        match brick, rotation with
        | I, R0 -> [ [1;1;1;1] ]
        | I, R90 -> [ [1];[1];[1];[1] ]
        | I, R180 -> [ [1;1;1;1] ]
        | I, R270 -> [ [1];[1];[1];[1] ]

        | J, R0 -> [ [2;0;0];[2;2;2] ]
        | J, R90 -> [[2;2];[2;0];[2;0]]
        | J, R180 -> [ [2;2;2];[0;0;2] ]
        | J, R270 -> [[0;2];[0;2];[2;2]]

        | L, R0 -> [[3;3;3];[3;0;0]]
        | L, R90 -> [[3;3];[0;3];[0;3]]
        | L, R180 -> [[0;0;3];[3;3;3]]
        | L, R270 -> [[3;0];[3;0];[3;3]]

        | O, R0 -> [[4;4];[4;4]]
        | O, R90 -> [[4;4];[4;4]]
        | O, R180 -> [[4;4];[4;4]]
        | O, R270 -> [[4;4];[4;4]]

        | S, R0 -> [[0;5;5];[5;5;0]]
        | S, R90 -> [[5;0];[5;5];[0;5]]
        | S, R180 -> [[0;5;5];[5;5;0]]
        | S, R270 -> [[5;0];[5;5];[0;5]]

        | T, R0 -> [[0;6;0];[6;6;6]]
        | T, R90 -> [[6;0];[6;6];[6;0]]
        | T, R180 -> [[6;6;6];[0;6;0]]
        | T, R270 -> [[0;6];[6;6];[0;6]]

        | Z, R0 -> [[7;7;0];[0;7;7]]
        | Z, R90 -> [[0;7];[7;7];[7;0]]
        | Z, R180 -> [[7;7;0];[0;7;7]]
        | Z, R270 -> [[0;7];[7;7];[7;0]]



    module Helpers =
        open System.Xml.Linq
        open System.Xml.Linq

        let private flatten list =
            FlattenFields (([],list) ||> List.fold (fun state item -> state @ item ))

        let flattenBricklayer (Bricklayer fields) =
            fields |> flatten

        let flattenGamefield (Gamefield fields) =
            fields |> flatten

                

        let unflatten (FlattenFields flattenFields) = 
            flattenFields |> split fieldWidth

        let unflattenBrickLayer flattenFields =
            flattenFields |> unflatten |> Bricklayer

        let unflattenGamefield flattenFields =
            flattenFields |> unflatten |> Gamefield


        let orOperationOnFlattenFields (FlattenFields a) (FlattenFields b) =
            (a,b) 
            ||> Seq.map2 (fun itema itemb -> if itema > 0 || itemb > 0 then itema + itemb else 0)
            |> Seq.toList
            |> FlattenFields

        let checkOverlappingOf2FlattenFields (FlattenFields a) (FlattenFields b) =
            (a,b) 
            ||> Seq.exists2 (fun itema itemb -> itema>0 && itemb>0)

        let addScores (Score x) (Score y)= Score (x + y)

        let getBrickHeight (brick: int list list) = brick.Length
        let getBrickWidth (brick: int list list) =
            brick
            |> List.map (fun line -> line.Length) 
            |> List.max   


    let normalizeCoordinates x y brickwidth brickHeight =
        let x = 
            if x < 1 then 1
            elif x + brickwidth - 1 > fieldWidth then fieldWidth - brickwidth + 1
            else x
        let y =
            if y < 1 then 1
            // no restiction to the bottom, because collision handling count on that
            else y
        (x,y)

    // Creates a 2d array, that represence the current player brick on the game field
    let createBrickLayer rotation x y brick =
        let b = brick |> createBrick rotation
        let brickHeight = b |> Helpers.getBrickHeight
        let brickWidth = b |> Helpers.getBrickWidth            

        let (x,y) = normalizeCoordinates x y brickWidth brickHeight
    
        let startRowOnBrick = if (brickHeight - y) <= 0 then 0 else (brickHeight - y)
        let possiblePartialBrick = b.[startRowOnBrick..brickHeight - 1];    
        let brickInFullLine = 
            possiblePartialBrick
            |> List.map (fun line -> 
                [for _ in [1..x-1] do yield 0] @ line @ [for _ in [x + brickWidth..fieldWidth] do yield 0]             
                )
        let countFirstEmptyLines = if (brickHeight < y) then y - brickHeight else 0
        let countLastEmptyLine = fieldHeight - countFirstEmptyLines - possiblePartialBrick.Length
        let field = 
            [for _ in [1..countFirstEmptyLines] do yield emptyLine] @ 
            brickInFullLine @ 
            [for _ in [1..countLastEmptyLine] do yield emptyLine]
        Brick (brick,rotation,x,y,Bricklayer field)
 
    let calculateScore (Level level) lines = 
        let linePoint =
            match lines with
            | 0 -> 0
            | 1 -> 40
            | 2 -> 100
            | 3 -> 300
            | 4 -> 1200
            | _ -> failwith "What the heck? You should never be able to remove more than 4 lines!"
        Score (linePoint*(level+1))

    let removeFullLines level (Gamefield gamefield) =
        let isNotFull row = not (row |> List.forall (fun i -> i > 0))
        let withoutFullLines =
            gamefield
            |> List.filter isNotFull
        let missingLineCount = gamefield.Length - withoutFullLines.Length
        let score = missingLineCount |> calculateScore level
        (Gamefield ([for _ in [1..missingLineCount] do yield emptyLine] @ withoutFullLines), score, missingLineCount)
    
    let updateGameField level brickLayer gameField =
        let flattenGamefield = gameField |> Helpers.flattenGamefield
        let flattenBrickLayer = brickLayer |> Helpers.flattenBricklayer

        let newGameFieldMatrix =
            (flattenBrickLayer, flattenGamefield)
            ||> Helpers.orOperationOnFlattenFields
        
        newGameFieldMatrix 
        |> Helpers.unflattenGamefield
        |> removeFullLines level

    let checkCollision gameField brickState =    
        match brickState with
        | NoBrick -> NoCollision, brickState
        | Brick (_,_,_,y,o) ->
            if (y > fieldHeight) then Collision, brickState
            else
                let fb = o |> Helpers.flattenBricklayer
                let fg = gameField |> Helpers.flattenGamefield
                match (fb,fg) ||> Helpers.checkOverlappingOf2FlattenFields with
                | true -> Collision, brickState
                | false -> NoCollision, brickState

    let moveBrick move brickstate =
        match brickstate with
        | NoBrick -> brickstate        
        | Brick (brick,rotation,x,y,matrix) ->
            match move with
            | Left -> brick |> createBrickLayer rotation (x - 1) y
            | Right -> brick |> createBrickLayer rotation (x + 1) y
            | Down -> brick |> createBrickLayer rotation x (y + 1)

    let rotateBrick brickstate =
        match brickstate with
        | NoBrick -> brickstate       
        | Brick (brick,rotation,x,y,matrix) ->
            let newRotation =
                match rotation with
                | R0 -> R90
                | R90 -> R180
                | R180 -> R270
                | R270 -> R0
            brick |> createBrickLayer newRotation x y


    let initGame randomBrick randomRotation previewBricks = 
        match previewBricks with
        | false -> newGameModel
        | true -> {newGameModel with PreviewNextBrick = PreviewNextBrick (randomBrick(),randomRotation()) }

    let innerGameCommandHandler cmd gamemodel =
        
        let calcGameModelFromCollison (collision,newbrickstate) =
            match collision with
            | NoCollision -> {gamemodel with BrickState = newbrickstate}
            | Collision -> gamemodel

        match cmd with 
        | TogglePause -> 
            match gamemodel.GameState with
            | Pause -> { gamemodel with GameState = Running}
            | Running -> { gamemodel with GameState = Pause}
            | _ -> gamemodel
            
        | DoNothing -> gamemodel

        | MoveLeft -> 
            gamemodel.BrickState 
            |> moveBrick Left 
            |> checkCollision gamemodel.GameField
            |> calcGameModelFromCollison

        | MoveRight -> 
            gamemodel.BrickState 
            |> moveBrick Right 
            |> checkCollision gamemodel.GameField
            |> calcGameModelFromCollison

        | MoveDown -> 
            gamemodel.BrickState 
            |> moveBrick Down 
            |> checkCollision gamemodel.GameField
            |> calcGameModelFromCollison

        | Rotate -> 
            gamemodel.BrickState 
            |> rotateBrick 
            |> checkCollision gamemodel.GameField
            |> calcGameModelFromCollison
        
      
    // not pure anymore      
    let nextInnerGameMove currentLevel randomRotation randomBrick randomX gamemodel =
        match gamemodel.BrickState with
        | NoBrick ->
            // use Preview Brick?
            let (newBrick,previewNextBrickState) =
                match gamemodel.PreviewNextBrick with
                | NoPreviewNextBrick -> randomBrick() |> createBrickLayer (randomRotation()) (randomX()) 1, NoPreviewNextBrick
                | PreviewNextBrick (brick,rotation) ->
                    brick |> createBrickLayer (rotation) (randomX()) 1, (PreviewNextBrick (randomBrick(),randomRotation()))
            
            // here check collision, if so the game ends
            match newBrick |> checkCollision gamemodel.GameField with
            | (NoCollision,_) -> {gamemodel with BrickState = newBrick; PreviewNextBrick = previewNextBrickState} 
            | (Collision,_) ->
                {gamemodel with BrickState = NoBrick; GameState = Lost; PreviewNextBrick = previewNextBrickState}    
        | Brick _ ->
            let nextMoveDown = gamemodel.BrickState |> moveBrick Down
            match nextMoveDown |> checkCollision gamemodel.GameField with
            | (NoCollision,_) -> {gamemodel with BrickState = nextMoveDown}
            | (Collision,_) ->
                match gamemodel.BrickState with
                | NoBrick -> {gamemodel with GameField = emptyGameField;GameState = Lost}
                | Brick (_,_,_,_,matrix) ->
                    let (mergedGamefield,score,missingLineCount) = gamemodel.GameField |> updateGameField currentLevel matrix
                    let sumCountRemovedLines = gamemodel.CountRemovedLines + missingLineCount
                    {
                        gamemodel with 
                            GameField = mergedGamefield
                            BrickState = NoBrick
                            Score = (Helpers.addScores gamemodel.Score score)
                            CountRemovedLines = sumCountRemovedLines
                            Level = Level (sumCountRemovedLines / 10)
                    }
        
        
    let startGame gamemodel =
        match gamemodel.GameState with
        | New -> {gamemodel with GameState = Running; GameField = emptyGameField}
        | Running -> gamemodel
        | Pause -> gamemodel
        | Lost -> {gamemodel with GameState = Running; GameField = emptyGameField}

    let stoppGame gamemodel =
        match gamemodel.GameState with
        | New -> gamemodel
        | Running -> {gamemodel with GameState = Lost; GameField = emptyGameField}
        | Pause -> gamemodel
        | Lost -> gamemodel

    let pauseGame gamemodel =
        match gamemodel.GameState with
        | New -> gamemodel
        | Running -> {gamemodel with GameState = Pause; GameField = emptyGameField}
        | Pause -> gamemodel
        | Lost -> gamemodel

    
    
    
    

    




