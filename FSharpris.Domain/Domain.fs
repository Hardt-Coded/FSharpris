namespace FSharpris

module Domain =

    let fieldWidth = 14
    let fieldHeight = 20

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

    type GameState = New | Running | Lost

    type GameModel = {
        GameState:GameState
        BrickState: BrickState
        GameField:GameField
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
        | DoNothing

    let emptyLine = [ for _ in [1..fieldWidth] do yield 0]    
    // 2 dimensional matrix filled with 0
    let emptyGameField = Gamefield ([ for _ in [1..fieldHeight] do yield emptyLine  ])
    let emptyBrickLayer = Bricklayer [ for _ in [1..fieldHeight] do yield emptyLine ]

    let newGameModel = {GameState = New; BrickState = NoBrick; GameField = emptyGameField}

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

        | J, R0 -> [ [1;0;0];[1;1;1] ]
        | J, R90 -> [[1;1];[1;0];[1;0]]
        | J, R180 -> [ [1;1;1];[0;0;1] ]
        | J, R270 -> [[0;1];[0;1];[1;1]]

        | L, R0 -> [[1;1;1];[1;0;0]]
        | L, R90 -> [[1;1];[0;1];[0;1]]
        | L, R180 -> [[0;0;1];[1;1;1]]
        | L, R270 -> [[1;0];[1;0];[1;1]]

        | O, R0 -> [[1;1];[1;1]]
        | O, R90 -> [[1;1];[1;1]]
        | O, R180 -> [[1;1];[1;1]]
        | O, R270 -> [[1;1];[1;1]]

        | S, R0 -> [[0;1;1];[1;1;0]]
        | S, R90 -> [[1;0];[1;1];[0;1]]
        | S, R180 -> [[0;1;1];[1;1;0]]
        | S, R270 -> [[1;0];[1;1];[0;1]]

        | T, R0 -> [[0;1;0];[1;1;1]]
        | T, R90 -> [[1;0];[1;1];[1;0]]
        | T, R180 -> [[1;1;1];[0;1;0]]
        | T, R270 -> [[0;1];[1;1];[0;1]]

        | Z, R0 -> [[1;1;0];[0;1;1]]
        | Z, R90 -> [[0;1];[1;1];[1;0]]
        | Z, R180 -> [[1;1;0];[0;1;1]]
        | Z, R270 -> [[0;1];[1;1];[1;0]]



    module Helpers =
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
            ||> Seq.map2 (fun itema itemb -> if itema > 0 || itemb > 0 then 1 else 0)
            |> Seq.toList
            |> FlattenFields

        let checkOverlappingOf2FlattenFields (FlattenFields a) (FlattenFields b) =
            (a,b) 
            ||> Seq.exists2 (fun itema itemb -> itema>0 && itemb>0)


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
        let brickHeight = b.Length        
        let brickWidth = 
            b 
            |> List.map (fun line -> line.Length) 
            |> List.max

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
 
    let removeFullLines (Gamefield gamefield) =
        let isNotFull row = not (row |> List.forall (fun i -> i > 0))
        let withoutFullLines =
            gamefield
            |> List.filter isNotFull
        let missingLineCount = gamefield.Length - withoutFullLines.Length
        Gamefield ([for _ in [1..missingLineCount] do yield emptyLine] @ withoutFullLines)
    
    let updateGameField brickLayer gameField =
        let flattenGamefield = gameField |> Helpers.flattenGamefield
        let flattenBrickLayer = brickLayer |> Helpers.flattenBricklayer

        let newGameFieldMatrix =
            (flattenBrickLayer, flattenGamefield)
            ||> Helpers.orOperationOnFlattenFields
        
        newGameFieldMatrix 
        |> Helpers.unflattenGamefield
        |> removeFullLines

    let checkCollision gameField brickState =    
        match brickState with
        | NoBrick -> NoCollision
        | Brick (_,_,_,y,o) ->
            if (y > fieldHeight) then Collision
            else
                let fb = o |> Helpers.flattenBricklayer
                let fg = gameField |> Helpers.flattenGamefield
                match (fb,fg) ||> Helpers.checkOverlappingOf2FlattenFields with
                | true -> Collision
                | false -> NoCollision

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


    let initGame () = newGameModel


    let innerGameCommandHandler cmd gamemodel =
        let newBrickState =
            match cmd with
            | MoveLeft -> gamemodel.BrickState |> moveBrick Left
            | MoveRight -> gamemodel.BrickState |> moveBrick Right
            | MoveDown -> gamemodel.BrickState |> moveBrick Down
            | Rotate -> gamemodel.BrickState |> rotateBrick
            | DoNothing -> gamemodel.BrickState
        match newBrickState |> checkCollision gamemodel.GameField with
        | NoCollision -> {gamemodel with BrickState = newBrickState}
        | Collision -> gamemodel
      
    // not pure anymore      
    let nextInnerGameMove randomRotation randomBrick randomX gamemodel =
        match gamemodel.BrickState with
        | NoBrick ->
            let newBrick = randomBrick() |> createBrickLayer (randomRotation()) (randomX()) 1
            // here check collision, if so the game ends
            match newBrick |> checkCollision gamemodel.GameField with
            | NoCollision -> {gamemodel with BrickState = newBrick} 
            | Collision ->
                {gamemodel with BrickState = NoBrick; GameState = Lost}    
        | Brick _ ->
            let nextMoveDown = gamemodel.BrickState |> moveBrick Down
            match nextMoveDown |> checkCollision gamemodel.GameField with
            | NoCollision -> {gamemodel with BrickState = nextMoveDown}
            | Collision ->
                match gamemodel.BrickState with
                | NoBrick -> {gamemodel with GameField = emptyGameField;GameState = Lost}
                | Brick (_,_,_,_,matrix) ->
                    let mergedGamefield = gamemodel.GameField |> updateGameField matrix
                    {gamemodel with GameField = mergedGamefield; BrickState = NoBrick}
        
        
    let startGame gamemodel =
        match gamemodel.GameState with
        | New -> {gamemodel with GameState = Running; GameField = emptyGameField}
        | Running -> gamemodel
        | Lost -> {gamemodel with GameState = Running; GameField = emptyGameField}

    let stoppGame gamemodel =
        match gamemodel.GameState with
        | New -> gamemodel
        | Running -> {gamemodel with GameState = Lost; GameField = emptyGameField}
        | Lost -> gamemodel

    
    
    
    

    




