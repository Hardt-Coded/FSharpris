namespace FSharpris.Xamarin

open System
open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms
open FSharpris.Domain

module App = 
    
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

    type Msg = 
        | StartGame 
        | IngameMoveLeft 
        | IngameMoveRight
        | IngameMoveDown
        | IngameRotate
        | IngameMoveDownTick

    

    let init () = initGame(), Cmd.none

    let timerCmd = 
        async { do! Async.Sleep 200
                return IngameMoveDownTick }
        |> Cmd.ofAsyncMsg

    let update msg model =
        match msg with
        | StartGame -> model |> startGame, timerCmd
        | IngameMoveLeft -> model |> innerGameCommandHandler MoveLeft ,Cmd.none
        | IngameMoveRight -> model |> innerGameCommandHandler MoveRight, Cmd.none
        | IngameMoveDown -> model |> innerGameCommandHandler MoveDown, Cmd.none
        | IngameRotate -> model |> innerGameCommandHandler Rotate, Cmd.none
        | IngameMoveDownTick -> model |> nextInnerGameMove randomRotation randomBrick randomX, if model.GameState = Running then  timerCmd else Cmd.none

    
    
        
    // render row with little decorations
    let renderRow dispatch index row = 
        [
            
            for (idx,item) in row |> List.indexed do 
                if item > 0 then 
                    yield 
                        View.BoxView(backgroundColor=Color.GreenYellow)
                            .GridRow(index).GridColumn(idx)
            
        ]
        
    
    let renderMatrix dispatch matrix = 
        matrix
        |> List.indexed
        |> List.map (fun (i,row)-> row |> renderRow dispatch i)

    let renderGame dispatch (gamemodel: GameModel) =
        let gamefield = gamemodel.GameField
        let brickState = gamemodel.BrickState
        match brickState with
        |NoBrick -> 
            let (Gamefield newMatrix) = gamefield
            newMatrix |> renderMatrix dispatch
        |Brick (_,_,_,_,brickLayer) ->
            let flattenGamefield =  gamefield |> Helpers.flattenGamefield
            let flattenBrickLayer = brickLayer |> Helpers.flattenBricklayer            
            // merge both
            let newMatrix =
                (flattenGamefield, flattenBrickLayer)
                ||> Helpers.orOperationOnFlattenFields
                |> Helpers.unflatten
            newMatrix |> renderMatrix dispatch
            

    let view (model: GameModel) dispatch =
        View.ContentPage(            
          content = 
            match model.GameState with
            | New -> 
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    verticalOptions = LayoutOptions.Center,
                    children=[
                        View.Label(text="FSharpris",horizontalOptions = LayoutOptions.Center,fontSize=32)
                        View.Button(text="Start Game!",command = (fun () -> dispatch StartGame))
                    ]
                )
            | Running ->
                View.Grid(
                    coldefs = [box "0.2*";box "0.6*";box "0.2*"],
                    rowdefs = [box "0.1*";box "0.8*";box "0.1*"],
                    backgroundColor = Color.DarkViolet,
                    children = [
                        yield View.Grid(
                            coldefs = [for _ in [1..fieldWidth] do yield box "*"],
                            rowdefs = [for _ in [1..fieldHeight] do yield box "*"],
                            rowSpacing = 0.0,
                            columnSpacing = 0.0,
                            backgroundColor=Color.White,
                            children = [                               
                                // Game itself
                                for x in model |> renderGame dispatch do
                                    for y in x do
                                        yield y                                
                            ],
                            gestureRecognizers=[View.TapGestureRecognizer(command=(fun () -> dispatch IngameRotate))]).GridRow(1).GridColumn(1)
                        yield View.Grid(gestureRecognizers=[View.TapGestureRecognizer(command=(fun () -> dispatch IngameMoveLeft))]).GridRow(1).GridColumn(0)
                        yield View.Grid(gestureRecognizers=[View.TapGestureRecognizer(command=(fun () -> dispatch IngameMoveRight))]).GridRow(1).GridColumn(2)
                        yield View.Grid(gestureRecognizers=[View.TapGestureRecognizer(command=(fun () -> dispatch IngameMoveDown))]).GridRow(2).GridColumn(0).GridColumnSpan(3)
                        
                    ]
                )
                
                    
            | Lost ->
                View.StackLayout(
                    orientation = StackOrientation.Vertical,
                    verticalOptions = LayoutOptions.Center,
                    children=[
                        View.Label(text="You Lost !")
                        View.Button(text="Start New Game!",command = (fun () -> dispatch StartGame))
                    ]
                )
              )

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram init update view

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWithDynamicView app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/tools.html for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/models.html for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


