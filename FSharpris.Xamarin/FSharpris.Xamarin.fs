namespace FSharpris.Xamarin

open System
open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms
open Xamarin.Essentials
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
        | TogglePreviewBricks of bool
        | IngameTogglePause
        | IngameMoveLeft 
        | IngameMoveRight
        | IngameMoveDown
        | IngameRotate
        | IngameMoveDownTick

    let init preview () = initGame randomBrick randomRotation preview, Cmd.none

    let timerCmd model = 
        async { do! Async.Sleep (500 / ((model.Level |> Level.unwrap) + 1))
                return IngameMoveDownTick }
        |> Cmd.ofAsyncMsg

    let update msg model =
        match msg with
        | StartGame -> model |> startGame, (timerCmd model)
        | TogglePreviewBricks b -> 
            if b then
                init true ()
            else
                init false ()
        | IngameTogglePause -> model |> innerGameCommandHandler TogglePause ,if model.GameState = Pause then (timerCmd model) else Cmd.none
        | IngameMoveLeft -> model |> innerGameCommandHandler MoveLeft ,Cmd.none
        | IngameMoveRight -> model |> innerGameCommandHandler MoveRight, Cmd.none
        | IngameMoveDown -> model |> innerGameCommandHandler MoveDown, Cmd.none
        | IngameRotate -> model |> innerGameCommandHandler Rotate, Cmd.none
        | IngameMoveDownTick -> model |> nextInnerGameMove model.Level randomRotation randomBrick randomX, if model.GameState = Running then  (timerCmd model) else Cmd.none


    let getBrickColor = function
    | 1 -> Color.Cyan
    | 2 -> Color.RosyBrown
    | 3 -> Color.Orange
    | 4 -> Color.Yellow
    | 5 -> Color.Green
    | 6 -> Color.MediumVioletRed
    | 7 -> Color.Red
    | _ -> Color.Black        

    // render row with little decorations
    let renderRow dispatch index row = 
        [
            for (idx,item) in row |> List.indexed do 
                if item > 0 then 
                    

                    yield dependsOn (idx,index,item) (fun _ (idx,index,item) -> 
                        View.Grid(
                            children = [                            
                                View.BoxView(backgroundColor=(item |> getBrickColor),margin=2.0)
                            ]
                           ).GridRow(index).GridColumn(idx))
        ]
        
    
    let renderMatrix dispatch matrix = 
        matrix
        |> List.indexed
        |> List.map (fun (i,row)-> dependsOn (i,row) (fun _ (i,row) -> row |> renderRow dispatch i))


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
            let result = newMatrix |> renderMatrix dispatch
            result
            
    let renderPreview gamemodel =
        View.StackLayout(
            orientation = StackOrientation.Vertical,
            horizontalOptions = LayoutOptions.Center,
            verticalOptions = LayoutOptions.Center,
            children= [
                match gamemodel.PreviewNextBrick with
                | NoPreviewNextBrick -> 
                    yield View.Label(
                        text="No Preview",
                        horizontalOptions = LayoutOptions.Fill,
                        verticalOptions = LayoutOptions.Fill,
                        horizontalTextAlignment = TextAlignment.Center,
                        verticalTextAlignment = TextAlignment.Center)
                | PreviewNextBrick (brickType,rotation) ->
                    let brick = brickType |> createBrick rotation
                    let brickWidth = brick |> Helpers.getBrickWidth 
                    let brickHeight = brick |> Helpers.getBrickHeight 
                    let stoneSizeInPixel = DeviceDisplay.ScreenMetrics.Width / 3.0 / 6.0 / DeviceDisplay.ScreenMetrics.Density                    
                    let marginX = 4 - brickWidth
                    let marginY = 4 - brickHeight
                    yield View.Grid(
                        coldefs = [for _ in [1..8] -> (stoneSizeInPixel / 2.0)],
                        rowdefs = [for _ in [1..8] -> (stoneSizeInPixel / 2.0)],
                        rowSpacing = 0.0,
                        columnSpacing = 0.0,  
                        horizontalOptions = LayoutOptions.Center,
                        verticalOptions = LayoutOptions.Center,
                        backgroundColor = Color.FromHex("#293133"),
                        margin=stoneSizeInPixel / 2.0,
                        children = [                    
                            for (y,row) in brick |> List.indexed do
                                for (x,item) in row |> List.indexed do
                                    
                                    if item > 0 then
                                        yield View.Grid(
                                                children = [                            
                                                    View.BoxView(backgroundColor=(item |> getBrickColor),margin=2.0)                                                    
                                                ]
                                               ).GridRow((y * 2) + marginY).GridColumn((x * 2) + marginX).GridRowSpan(2).GridColumnSpan(2)
                        ]
                    )
                ])

    let renderState model dispatch =
        View.StackLayout(
            orientation= StackOrientation.Vertical,
            children =[
                fix (fun () -> View.Label(text="Score:",horizontalOptions=LayoutOptions.Fill,horizontalTextAlignment=TextAlignment.Center))
                dependsOn model.Score (fun model score -> View.Label(text= sprintf "%i" (score |> Score.unwrap),horizontalOptions=LayoutOptions.Fill,horizontalTextAlignment=TextAlignment.Center))
                fix (fun () -> View.Label(text="Level:",horizontalOptions=LayoutOptions.Fill,horizontalTextAlignment=TextAlignment.Center))
                dependsOn model.Level (fun model level -> View.Label(text= sprintf "%i" (level |> Level.unwrap),horizontalOptions=LayoutOptions.Fill,horizontalTextAlignment=TextAlignment.Center))
                fix (fun () -> View.Label(text="Lines:",horizontalOptions=LayoutOptions.Fill,horizontalTextAlignment=TextAlignment.Center))
                dependsOn model.CountRemovedLines (fun model count -> View.Label(text= sprintf "%i" count,horizontalOptions=LayoutOptions.Fill,horizontalTextAlignment=TextAlignment.Center))                
            ]
        )
        
    let renderGameArea model dispatch = 
        View.Grid(
            coldefs = fix (fun () ->[box "*";box "*";box "*"]),
            rowdefs = fix (fun () ->[box "auto";box "auto";box "auto";box "*";box "*"]),
            verticalOptions = fix (fun () -> LayoutOptions.Fill),
            horizontalOptions = fix (fun () -> LayoutOptions.Fill),
            backgroundColor = fix (fun () -> Color.Gray),
            children = [
                // Screenpart size for squared stones
                let screenPartInPixel = DeviceDisplay.ScreenMetrics.Width / 3.0 * 2.0
                let stoneSize = screenPartInPixel / DeviceDisplay.ScreenMetrics.Density / ((fieldWidth+2)|>float)
                yield View.Grid(
                    coldefs = fix (fun () -> [ for _ in [1..fieldWidth] -> stoneSize]),
                    rowdefs = fix (fun () -> [for _ in [1..fieldHeight] -> stoneSize]),
                    rowSpacing = fix (fun () -> 0.0),
                    columnSpacing = fix (fun () -> 0.0),
                    backgroundColor=fix (fun () -> Color.FromHex("#293133")),
                    margin = stoneSize,                    
                    children = [
                                                // Game itself                                    
                        for x in model |> renderGame dispatch do
                            for y in x do
                                yield y
                        if model.GameState = Pause then
                            yield fix (fun () -> 
                                View.BoxView(                                
                                    verticalOptions = LayoutOptions.Fill,
                                    horizontalOptions = LayoutOptions.Fill,
                                    opacity = 0.3,
                                    backgroundColor = Color.White
                                ).GridRowSpan(fieldHeight).GridColumnSpan(fieldWidth))
                            yield fix (fun () ->
                                View.StackLayout(
                                    orientation = StackOrientation.Vertical,
                                    verticalOptions = LayoutOptions.CenterAndExpand,                                
                                    children=[
                                        View.Label(text="Paused!",horizontalOptions = LayoutOptions.Center,fontSize=32)                            
                                    ]).GridRowSpan(fieldHeight).GridColumnSpan(fieldWidth)
                              )
                                    
                    ],
                    gestureRecognizers=fix (fun () -> [View.TapGestureRecognizer(command=(fun () -> dispatch IngameRotate))]))
                    .GridRow(1).GridColumn(1).GridColumnSpan(2)
                yield fix (fun () -> 
                    View.Label(text="FSharpris",
                        fontSize=32,
                        verticalTextAlignment=TextAlignment.Center,
                        horizontalTextAlignment=TextAlignment.Center,
                        verticalOptions=LayoutOptions.Fill,
                        horizontalOptions=LayoutOptions.Fill,
                        textColor=Color.White).GridRow(0).GridColumn(0).GridColumnSpan(3))
                yield View.StackLayout(
                        orientation=StackOrientation.Vertical,
                        verticalOptions=LayoutOptions.Center,
                        horizontalOptions=LayoutOptions.Center,
                        children = [
                            yield renderPreview model
                            yield renderState model dispatch
                            yield View.Button(
                                text=(if model.GameState = Pause then "Resume" else "Pause"),
                                command = fun () -> dispatch IngameTogglePause) 
                        ]
                    ).GridRow(1).GridColumn(0)                
                yield fix (fun () -> 
                    View.Grid(
                        children = [View.Label(margin=5.0,backgroundColor=Color.DarkGray,text="Rotate",horizontalTextAlignment=TextAlignment.Center,verticalTextAlignment=TextAlignment.Center)],
                        gestureRecognizers=[View.TapGestureRecognizer(command=(fun () -> dispatch IngameRotate))]).GridRow(3).GridColumn(1))
                yield fix (fun () -> 
                    View.Grid(                        
                        children = [View.Label(margin=5.0,backgroundColor=Color.DarkGray,text="Left",horizontalTextAlignment=TextAlignment.Center,verticalTextAlignment=TextAlignment.Center)],
                        gestureRecognizers=[View.TapGestureRecognizer(command=(fun () -> dispatch IngameMoveLeft))]).GridRow(3).GridColumn(0).GridRowSpan(2))
                yield fix (fun () -> 
                    View.Grid(
                        children = [View.Label(margin=5.0,backgroundColor=Color.DarkGray,text="Right",horizontalTextAlignment=TextAlignment.Center,verticalTextAlignment=TextAlignment.Center)],
                        gestureRecognizers=[View.TapGestureRecognizer(command=(fun () -> dispatch IngameMoveRight))]).GridRow(3).GridColumn(2).GridRowSpan(2))
                yield fix (fun () -> 
                    View.Grid(
                        children = [View.Label(margin=5.0,backgroundColor=Color.DarkGray,text="Down",horizontalTextAlignment=TextAlignment.Center,verticalTextAlignment=TextAlignment.Center)],
                        gestureRecognizers=[View.TapGestureRecognizer(command=(fun () -> dispatch IngameMoveDown))]).GridRow(4).GridColumn(1))
                        
            ]
                )
    let view (model: GameModel) dispatch =
        View.ContentPage(            
          content = 
            match model.GameState with
            | New -> 
                
                    View.StackLayout(
                        orientation = StackOrientation.Vertical,
                        verticalOptions = LayoutOptions.Center,
                        children=[
                            yield View.Label(text="FSharpris",horizontalOptions = LayoutOptions.Center,fontSize=32)
                            yield View.Label(text="Use Brick Preview",horizontalOptions = LayoutOptions.Center,fontSize=20)
                            yield View.Switch(
                                isToggled= (if model.PreviewNextBrick = NoPreviewNextBrick then false else true),
                                horizontalOptions = LayoutOptions.Center,
                                toggled = (fun on -> dispatch (TogglePreviewBricks on.Value))
                                )
                            yield View.Button(text="Start Game!",command = (fun () -> dispatch StartGame))
                        ]
                )
            | Running -> renderGameArea model dispatch
            | Pause -> renderGameArea model dispatch                
            | Lost ->
                
                    View.StackLayout(
                        orientation = StackOrientation.Vertical,
                        verticalOptions = LayoutOptions.Center,
                        children=[
                            yield View.Label(text="You Lost !",horizontalOptions = LayoutOptions.Center,fontSize=32)
                            yield View.Label(text="Use Brick Preview",horizontalOptions = LayoutOptions.Center,fontSize=20)
                            yield View.Switch(
                                isToggled= (if model.PreviewNextBrick = NoPreviewNextBrick then false else true),
                                horizontalOptions = LayoutOptions.Center,
                                toggled = (fun on -> dispatch (TogglePreviewBricks on.Value))
                                )
                            yield View.Button(text="Start New Game!",command = (fun () -> dispatch StartGame))
                        ]
                    )
              )

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram (init true) update view

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
//#if DEBUG
//        |> Program.withConsoleTrace
//#endif
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


