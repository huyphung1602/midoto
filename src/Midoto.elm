-- START:module
module Midoto exposing (main)
-- END:module

-- START:import
import Browser
import Browser.Events exposing (onKeyUp)
import Browser.Dom as Dom
import Html exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Html.Attributes exposing (..)
import Json.Decode as JsonDecode
import Json.Encode as JsonEncode
import Task
import Time
import Ports
-- END:import

-- START:main
main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
-- END:main

-- START:storage
saveTodos : List Todo -> Cmd msg
saveTodos todos =
    JsonEncode.list todoEncoder todos
        |> JsonEncode.encode 0
        |> Ports.storeTodos


todoEncoder : Todo -> JsonEncode.Value
todoEncoder todo =
    JsonEncode.object
        [ ( "id", JsonEncode.int todo.id )
        , ( "name", JsonEncode.string todo.name )
        , ( "workedTime", JsonEncode.float todo.workedTime )
        , ( "previousWorkedTime", JsonEncode.float todo.workedTime )
        , ( "status", JsonEncode.string <| todoStatusEncoder todo.status )
        ]

todoStatusEncoder : TodoStatus -> String
todoStatusEncoder status =
    case status of
        Active ->
            "Active"
        Incomplete ->
            "Incomplete"
        Completed ->
            "Completed"

todoStatusDecoder : JsonDecode.Decoder TodoStatus
todoStatusDecoder =
    JsonDecode.string |>
        JsonDecode.andThen
            (\str ->
                case str of
                    "Active" -> JsonDecode.succeed Active
                    "Incomplete" -> JsonDecode.succeed Incomplete
                    "Completed" -> JsonDecode.succeed Completed
                    _ -> JsonDecode.fail "Invalid TodoStatus"
            )

todoDecoder : JsonDecode.Decoder Todo
todoDecoder =
    JsonDecode.map5 Todo
        (JsonDecode.field "id" JsonDecode.int)
        (JsonDecode.field "name" JsonDecode.string)
        (JsonDecode.field "workedTime" JsonDecode.float)
        (JsonDecode.field "previousWorkedTime" JsonDecode.float)
        (JsonDecode.field "status" todoStatusDecoder)

todosDecoder : JsonDecode.Decoder (List Todo)
todosDecoder =
    JsonDecode.list todoDecoder

decodeStoredTodos : String -> List Todo
decodeStoredTodos todosJson =
    case JsonDecode.decodeString todosDecoder todosJson of
        Ok todos ->
            todos
        Err _ ->
            []

-- END:storage

-- START:model
type TodoStatus
    = Active
    | Incomplete
    | Completed

type RightPanel
    = CommandList
    | CompletedTodos

type alias Todo =
    { id : Int
    , name : String
    , workedTime : Float
    , previousWorkedTime : Float
    , status : TodoStatus
    }

type alias Model =
    { todos : List Todo
    , inputText : String
    , isShowForm : Bool
    , isWorking : Bool
    , time : Time.Posix
    , startTime : Time.Posix
    , zone : Time.Zone
    , rightPanel : RightPanel
    }

defaultInputText : String
defaultInputText = "/"

init : Maybe String -> (Model, Cmd Msg)
init flags =
    let
        initTodos =
            case flags of
                Just todosJson ->
                    decodeStoredTodos todosJson
                Nothing ->
                    []
    in
    ( { todos = initTodos
      , inputText = defaultInputText
      , isShowForm = False
      , isWorking = False
      , time = Time.millisToPosix 0
      , startTime = Time.millisToPosix 0
      , zone = Time.utc
      , rightPanel = CommandList
      }
    , Cmd.none
    )
-- END:model

-- START:update
type Msg
    = NoOp
    | PressCharacter Char
    | PressControl String
    | ChangeInput String
    | AddTodo (List String)
    | Check Int
    | Uncheck Int
    | Delete Int
    | ActiveOn Int
    | Start Int
    | Stop
    | Show RightPanel
    | Tick Time.Posix


-- END:update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PressCharacter keyChar ->
            let
                cmd = if model.isShowForm then Cmd.none else Task.attempt (\_ -> NoOp) (Dom.focus "input-command-box")
            in
            ({ model | isShowForm = (keyChar == 'i' || model.isShowForm ) }
            , cmd
            )
        PressControl keyStr ->
            ({ model | isShowForm = checkControlKey keyStr model.isShowForm }
            , Cmd.none
            )
        ChangeInput input ->
            ({ model | inputText = input }
            , Cmd.none
            )
        NoOp ->
            ({ model | inputText = defaultInputText }
            , Cmd.none
            )         
        AddTodo options ->
            let
                newTodos = addToTodos (String.join " " options) model.todos
            in
            ({ model
                | todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        Check index ->
            let
                newTodos = setCompleteTodo index Completed model.todos
            in
            ({ model
                | todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        Uncheck index ->
            let
                newTodos = setCompleteTodo index Incomplete model.todos
            in
            ({ model
                | todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        Delete index ->
            let
                newTodos = deleteTodo index model.todos
            in
            ({ model
                | todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        ActiveOn index ->
            let
                newTodos = setActiveTodo index model.todos
            in
            ({ model
                | todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        Start index->
            let
                newTodos = updatePreviousWorkedTime model.todos |> startTodo index
            in
            ({ model
                | isWorking = True
                , startTime = model.time
                , todos = newTodos
                , inputText = defaultInputText
            }
            , saveTodos newTodos
            )
        Stop ->
            let
                newTodos = updatePreviousWorkedTime model.todos
            in
            ({ model
                | isWorking = False
                , inputText = defaultInputText
                , todos = newTodos
            }
            , saveTodos newTodos
            )
        Tick newTime ->
            ({ model
                | time = newTime
                , todos = if model.isWorking then updateWorkedTime model else model.todos
            }
            , Cmd.none
            )
        Show newRightPanel ->
            ({ model
                | rightPanel = newRightPanel
            }
            , Cmd.none
            )

lastElem : List a -> Maybe a
lastElem list =
    case list of
        [] ->
            Nothing
        [last] ->
            Just last
        _::rest ->
            lastElem rest

addToTodos : String -> List Todo -> List Todo
addToTodos input todos =
    let
        lastIndex =
            case lastElem todos of
                Just t ->
                    t.id + 1
                Nothing ->
                    1
    in
    todos ++ [ Todo lastIndex input 0 0 Incomplete]

setCompleteTodo : Int -> TodoStatus -> List Todo -> List Todo
setCompleteTodo index completedStatus todos =
    List.map (\todo ->
        if todo.id == index then
            { todo | status = completedStatus }
        else
            todo
    ) todos

setActiveTodo : Int -> List Todo -> List Todo
setActiveTodo index todos =
    List.map (\todo ->
        if todo.id == index then
            { todo | status = Active }
        else if todo.status == Active then
            { todo | status = Incomplete }
        else
            todo
    ) todos

hasActiveTodo : List Todo -> Bool
hasActiveTodo todos =
    (List.filter (\todo -> todo.status == Active) todos |> List.length) > 0

startTodo : Int -> List Todo -> List Todo
startTodo index todos =
    List.map (\todo ->
        if todo.id == index then
            { todo | status = Active }
        else
            { todo | status = Incomplete }
    ) todos

deleteTodo : Int -> List Todo -> List Todo
deleteTodo index todos =
    List.filter (\todo -> todo.id /= index) todos

updatePreviousWorkedTime : List Todo -> List Todo
updatePreviousWorkedTime todos =
    List.map (\todo -> { todo | previousWorkedTime = todo.workedTime }) todos

updateWorkedTime : Model -> List Todo
updateWorkedTime model =
    List.map (\todo ->
        if todo.status == Active then
            { todo | workedTime = todo.previousWorkedTime + (floatFromPosix model.zone model.time) - (floatFromPosix model.zone model.startTime) }
        else
            todo
    ) model.todos

floatFromPosix : Time.Zone -> Time.Posix -> Float
floatFromPosix zone time =
    (Time.toHour zone time) * 3600 + (Time.toMinute zone time) * 60 + (Time.toSecond zone time) |> toFloat

checkControlKey : String -> Bool -> Bool
checkControlKey keyStr isShowForm =
    if isShowForm && (keyStr == "Enter" || keyStr == "Escape") then
        False
    else
        isShowForm

-- START:subscription
keyDecoder : JsonDecode.Decoder Msg
keyDecoder =
    JsonDecode.map toKey (JsonDecode.field "key" JsonDecode.string)

toKey : String -> Msg
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            PressCharacter char

        _ ->
            PressControl string

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ onKeySub, (timerSub model) ]

onKeySub : Sub Msg
onKeySub =
    onKeyUp keyDecoder

timerSub : Model -> Sub Msg
timerSub model =
    Time.every 1000 Tick
-- END:subscription

-- START:view
view : Model -> Html Msg
view model =
    div [ style "width" "100vw", style "height" "100vh", style "overflow" "hidden" ]
        [
            Html.form 
                ([ onSubmit (parseMsg (tokenize model.inputText) model.todos)
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "position" "absolute"
                , style "height" "100vh"
                , style "width" "100vw"
                , style "background" "rgba(0, 0, 0, 0.5)"
                ] ++ displayForm model.isShowForm)
                [ input
                    ([ placeholder "What do you want to do?"
                    , id "input-command-box"
                    , value model.inputText
                    , onInput ChangeInput
                    ] ++ styleInputBox
                    )
                    []
                ]
            , div
                styleApplicationBody
                [ div styleOfListBox <| (h3 [style "margin-left" "20px"] [ text "On Going Tasks" ])::(List.map viewTodo <| onGoingTodos model.todos)
                , div styleOfListBox <| viewRightPanel model
                ]

        ]

viewRightPanel : Model -> List (Html Msg)
viewRightPanel model =
    case model.rightPanel of
        CommandList ->
            viewCommandList commandElements
        CompletedTodos ->
            viewCompletedTodos model.todos

viewCompletedTodos : List Todo -> List (Html Msg)
viewCompletedTodos todos =
    (h3 [style "margin-left" "20px"] [ text "Completed Tasks" ])::(List.map viewTodo <| completedTodos todos)

viewCommandList : List (Html Msg) -> List (Html Msg)
viewCommandList cmdElems =
    (h3 [style "margin-left" "20px"] [ text "List of Commands" ])::(List.map viewCommand <| cmdElems)

viewCommand : Html Msg -> Html Msg
viewCommand cmdElement =
    p [style "line-height" "32px"]
        [ div
            [ style "margin-left" "16px"
            , style "margin-right" "16px"
            ]
            [ cmdElement ]
        ]

commandElements : List (Html Msg)
commandElements =
    [ span []
        [ strong [] [ text "Press i" ]
        , text " to show the command palette"
        ]
    , span []
        [ strong [] [ text "/add or /a" ]
        , text " [your task name] or"
        , strong [] [ text " type your task name" ]
        , text " to add a new task"
        ]
    , span []
        [ strong [] [ text "/wk" ]
        , text " [task index] to select working task"
        ]
    , span []
        [ strong [] [ text "/start" ]
        , text " to start or continue counting working time on a task. "
        , strong [] [ text "/start" ]
        , text " [task index] to select a working task and start it at the same time."
        ]
    , span []
        [ strong [] [ text "/stop" ]
        , text " to stop working time on a task"
        ]
    , span []
        [ strong [] [ text "/check or /c" ]
        , text " [task index] to complete a task"
        ]
    , span []
        [ strong [] [ text "/uncheck or /u" ]
        , text " [task index] to complete a task"
        ]
    , span []
        [ strong [] [ text "/delete or /d" ]
        , text " [task index] to delete a task"
        ]
    , span []
        [ strong [] [ text "/0" ]
        , text " to show the list of commands"
        ]
    , span []
        [ strong [] [ text "/1" ]
        , text " to show the completed tasks"
        ]
    ]

viewTodo : (Int, Todo) -> Html Msg
viewTodo (uiIndex, todo) =
    p []
        [ div
            [ style "text-decoration"
                (if todo.status == Completed then
                    "line-through"
                else
                    "none"
                )
            , style "font-weight"
                (if todo.status == Active then
                    "600"
                else
                    "300"
                )
            , style "display" "flex"
            , style "justify-content" "space-between"
            , style "margin-left" "16px"
            , style "margin-right" "16px"
            ]
            [ span [] [ text <| (String.fromInt uiIndex) ++ ". " ++ todo.name ]
            , span [ style "display"
                    ( if todo.status == Active || todo.status == Completed then
                        "flex"
                    else
                        "none"
                    )
                ]
                [ text <| parseWorkingTimeToString todo.workedTime ]
            ]
        ]

onGoingTodos : List Todo -> List (Int, Todo)
onGoingTodos todos =
    List.filter (\todo -> todo.status /= Completed) todos |> List.indexedMap (\x y -> (x+1, y))

completedTodos : List Todo -> List (Int, Todo)
completedTodos todos =
    List.filter (\todo -> todo.status == Completed) todos |> List.indexedMap (\x y -> (x+1, y))

getTrueIndex : Maybe Int -> List (Int, Todo) -> Maybe Int
getTrueIndex uiIndex todoTuples =
    case todoTuples of
        [] ->
            Nothing
        [(_, x)] ->
            Just x.id
        xs ->
            List.filter (\(x, _) -> uiIndex == Just x) xs |> List.map (\(_, y) -> y.id) |> List.head

tokenize : String -> List String
tokenize input =
    String.words input

toTwoCharString : Float -> String
toTwoCharString num =
    if num > 9 then
        String.fromFloat num
    else
        "0" ++ String.fromFloat num

parseWorkingTimeToString : Float -> String
parseWorkingTimeToString workingTime =
    let
        hours = workingTime / (3600)
        hoursRounded = hours |> truncate |> toFloat
        minutes = (workingTime - hoursRounded * 3600) / 60
        minutesRouded = minutes |> truncate |> toFloat
        seconds = workingTime - hoursRounded * 3600 - minutesRouded * 60
    in
        (String.fromFloat hoursRounded) ++ ":" ++ toTwoCharString minutesRouded ++ ":" ++ toTwoCharString seconds

parseCommandUseIndex : (Int -> Msg) -> List String -> List (Int, Todo) -> Msg
parseCommandUseIndex command list todoTuples =
    let
        parseMaybeInt maybeInt =
            case maybeInt of
                Just i ->
                    command i
                Nothing ->
                    NoOp
    in
    case list of
        [] ->
            NoOp
        [x] ->
            parseMaybeInt (getTrueIndex (String.toInt x) todoTuples)
        x::_ ->
            parseMaybeInt (getTrueIndex (String.toInt x) todoTuples)

parseMsg : List String -> List Todo -> Msg
parseMsg list todos =
    case list of
        [] ->
            NoOp
        [x] ->
            case String.toLower x of
                "/start" ->
                    parseCommandUseIndex Start ["1"] (onGoingTodos todos)
                "/stop" ->
                    Stop
                "/0" ->
                    Show CommandList
                "/1" ->
                    Show CompletedTodos
                _ ->
                    NoOp
        x::xs ->
            case String.toLower x of
                "/add" ->
                    AddTodo xs
                "/a" ->
                    AddTodo xs
                "/check" ->
                    parseCommandUseIndex Check xs (onGoingTodos todos)
                "/c" ->
                    parseCommandUseIndex Check xs (onGoingTodos todos)
                "/uncheck" ->
                    parseCommandUseIndex Uncheck xs (completedTodos todos)
                "/uc" ->
                    parseCommandUseIndex Uncheck xs (completedTodos todos)
                "/delete" ->
                    parseCommandUseIndex Delete xs (onGoingTodos todos)
                "/d" ->
                    parseCommandUseIndex Delete xs (onGoingTodos todos)
                "/wk" ->
                    parseCommandUseIndex ActiveOn xs (onGoingTodos todos)
                "/start" ->
                    parseCommandUseIndex Start xs (onGoingTodos todos)
                _ ->
                    AddTodo list

displayForm : Bool -> List (Html.Attribute msg)
displayForm isShowForm =
    if isShowForm then
        [ style "display" "flex" ]
    else
        [ style "display" "none"]

styleApplicationBody : List (Html.Attribute msg)
styleApplicationBody =
    [ style "display" "flex"
    , style "flex-direction" "row"
    , style "justify-content" "space-evenly"
    , style "align-items" "center"
    , style "padding-right" "50px"
    , style "width" "100%"
    , style "height" "100%"
    , style "overflow" "hidden"
    ]

styleOfListBox : List (Html.Attribute msg)
styleOfListBox =
    [ style "padding" "10px"
    , style "border" "1px solid #d1d1d1"
    , style "border-radius" "4px"
    , style "width" "40%"
    , style "height" "80%"
    , style "font-size" "16px"
    , style "overflow" "hidden"
    , style "box-shadow" "0 3px 10px rgb(0 0 0 / 0.2)"
    ]

styleInputBox : List (Html.Attribute msg)
styleInputBox =
    [ style "font-size" "16px"
    , style "letter-spacing" "0.8px"
    , style "height" "45px"
    , style "width" "500px"
    , style "padding" "0px 10px"
    , style "border" "1px solid #666"
    , style "border-radius" "4px"
    , style "box-shadow" "0 0 50px rgba(0, 0, 0, 0.25)"
    ]

-- END:view
