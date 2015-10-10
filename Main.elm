module Battleship (main) where

-- Core
import String
import Keyboard
import Time
-- Evan
import Html
import Html.Attributes
import Html.Events
import StartApp
import Effects
-- 3rd Party
-- Battleship
import Player
import Ship
import Grid

---- HELPERS ----

-- TODO For some reason using this as a signal-seed doesn't work - the model stays `defaultModel`; probably because the signal somehow updates _before_ `computer = defaultCompter` occurs
startTime =
  Signal.map fst <| Time.timestamp <| Signal.constant ()

freeze signal =
  Signal.sampleOn (Signal.constant 0) signal

---- MAIN ----
main = .html <|
  StartApp.start
    { init = (defaultModel, Effects.none)
    , view = view
    , update = (\ a m -> (update a m, Effects.none))
    , inputs =
      [ Signal.map toggleOrientation
          <| Keyboard.isDown 68 {- D -}
      , Signal.map randomizeOpponent <| Time.every 1000
      ]
    }

---- INPUT PROCESSORS ----

toggleOrientation : Bool -> Action
toggleOrientation bool =
  if bool then
    SetupOrientationToggle
  else
    NoOp

randomizeOpponent : Float -> Action
randomizeOpponent float =
  SetupRandomOpponent <| floor float

---- MODEL ----
defaultModel : Model
defaultModel =
  { state = Setup
  , selectedShipId = Just 0
  , hoverPos = Nothing
  , player = Player.defaultPlayer
  , computer = Player.defaultComputer
  }
-- Model
type alias Model =
  { state : State
  , selectedShipId : Maybe Int
  , hoverPos : Maybe (Int, Int)
  , player : Player.Player
  , computer : Player.Player
  }
-- State
type State
  = Setup
  | Play
  | GameOver

(:=) = (,)

---- VIEW ----

-- Global wrapper

wrapper : List Html.Html -> Html.Html
wrapper htmlList =
  Html.main'
    [ Html.Attributes.style
      [ "display" := "flex"
      , "flex-direction" := "column"
      , "align-items" := "center"
      , "margin" := "50px 0px"
      ]
    ] htmlList

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    aimShoot =
      if model.state == Play then
        Just 
         { hover = Signal.forwardTo address PlayAim
         , click = Signal.forwardTo address PlayShoot
         }
      else
        Nothing
    selectedShipId = model.selectedShipId
    spacer = Html.div
      [Html.Attributes.style ["height" := "40px"]] []
    content =
      wrapper <| (setupControlsView address model.player selectedShipId) ++
        [ spacer
        , Grid.toHtml aimShoot model.computer.trackingGrid
        , spacer
        , Player.field Nothing model.computer
        ]
  in
    content
  --case model.state of
  --  Setup ->
  --    content
  --  Play ->
  --    content
  --    --wrapper
  --    --  [ Html.div []
  --    --    [ Html.div [] [ Html.text "Player1" ]
  --    --    , Player.field Nothing model.player
  --    --    , Html.div [] [ Html.text "Player2" ]
  --    --    , Player.field aimShoot model.computer
  --    --    ]
  --    --  ]
  --  GameOver ->
  --    wrapper
  --      [ Player.field Nothing model.player
  --      , Player.field Nothing model.computer
  --      ]


setupControlsView : Signal.Address Action -> Player.Player -> Maybe Int -> List Html.Html
setupControlsView address player selectedShipId =
  let
    hoverClick = Just
      { hover = Signal.forwardTo address SetupShowShip
      , click = Signal.forwardTo address SetupAddShip
      }
    shipSelector = Html.div [Html.Attributes.style ["display" := "flex", "overflow" := "hidden", "border-radius" := "10px"]] <|
      List.map (shipFieldView address selectedShipId) (Player.getShips player)
    hint = Html.div [Html.Attributes.style ["margin" := "20px 0px"]] [ Html.text "Press \"D\" to change ship's orientation" ]
  in
    [ shipSelector
    , hint
    , Player.field hoverClick player
    ]

-- Depending on the Action render the proper html input.
-- TODO this might belong in the Ship module
shipFieldView : Signal.Address Action -> Maybe Int -> Ship.Ship -> Html.Html
shipFieldView address selectedShipId ship  =
  let
    isSelected =
      case selectedShipId of
        Just id -> id == ship.id
        Nothing -> False
  in
    shipListView address ship isSelected

shipListView : Signal.Address Action -> Ship.Ship -> Bool -> Html.Html
shipListView address ship isSelected =
  let
    selectedStyle =
      if isSelected then
        [ "background-color" := "gray" ]
      else
        []
    direction =
      case ship.orientation of
        Ship.Horizontal -> "row"
        Ship.Vertical -> "column"
    hiddenStyle =
      if ship.added then
        ["cursor" := "auto"]
      else []
    box = Html.div
      [ Html.Attributes.style <|
        [ "width" := "20px"
        , "height" := "20px"
        , "border" := "1px solid gray"
        , "border-radius" := "3px"
        , "margin" := "1px"
        , "vertical-align" := "middle"
        ] ++ selectedStyle
      ] []
    clickEvent =
      Html.Events.onMouseDown address <|
        if not isSelected then
          SetupSelectShip (Just ship.id)
        else
          SetupOrientationToggle
  in
    Html.div
    ([ Html.Attributes.style <|
      [ "display" := "flex"
      , "flex-direction" := direction
      , "align-items" := "center"
      , "justify-content" := "center"
      , "width" := "150px"
      , "height" := "150px"
      , "background-color" := "lightgray"
      , "margin" := "0px 1px"
      , "cursor" := "pointer"
      ] ++ hiddenStyle
    ] ++ if ship.added then [] else [clickEvent])
    <| if ship.added then [] else List.repeat ship.length box

---- UPDATE ----
type Action
  = SetupRandomOpponent Int
  | SetupOrientationToggle
  | SetupSelectShip (Maybe Int)
  | SetupShowShip (Maybe (Int, Int))
  | SetupAddShip (Int, Int)
  | SetupPlay
  | PlayAim (Maybe (Int, Int))
  | PlayShoot (Int, Int)
  | NoOp

update : Action -> Model -> Model
update action model =
  case action of
    SetupRandomOpponent seed ->
      -- TODO For some reason freezing the time signal never updates the computer player, but if not frozen this function has to run each time the signal updates, which is inefficient
      if Player.allShipsAdded model.computer then
        model
      else
        { model | computer <- Player.random seed }
    SetupOrientationToggle ->
      case model.selectedShipId of
        Just id ->
          { model | player <- Player.turnShip id model.hoverPos model.player }
        Nothing ->
          model
    SetupSelectShip shipId ->
      { model | selectedShipId <- shipId }
    SetupShowShip pos ->
      case model.selectedShipId of
        Just id ->
          { model | player <- Player.moveShip id pos model.player
                  , hoverPos <- pos }
        Nothing ->
          { model | hoverPos <- Nothing}
    SetupAddShip _ ->
      case model.selectedShipId of
        Just id ->
          let
            newPlayer = Player.addShip id model.player
            nextShipId = Player.nextNotAddedShipId newPlayer
            ready = nextShipId == Nothing -- Being a little smart here: if there are no ships left to be added, then we're ready to play!
            nextModel =
              { model | player <- newPlayer
                      , selectedShipId <- nextShipId }
          in
            if not ready then nextModel else { nextModel | state <- Play }
        Nothing ->
          model
    SetupPlay ->
      { model | state <- Play }
    PlayAim position ->
      model
    PlayShoot pos ->
      { model | computer <- Player.shoot pos model.computer }
    NoOp -> model

toIntOrDefaultOrZero : String -> Int -> Int
toIntOrDefaultOrZero stringToConvert default =
  if stringToConvert == "" then 0 else
  case String.toInt stringToConvert of
    Ok n -> n
    _ -> default