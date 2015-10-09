module Battleship (main) where

-- Core
import String
import Keyboard
-- Evan
import Html
import Html.Attributes
import Html.Events
import StartApp
import Effects
-- 3rd Party
-- Battleship
import Grid
import Player
import Fleet
import Ship

---- MAIN ----
main = .html <|
  StartApp.start
    { init = (defaultModel, Effects.none)
    , view = view
    , update = (\ a m -> (update a m, Effects.none))
    , inputs =
      [ Signal.map toggleOrientation isDpressed
      ]
    }

---- INPUTS ----

isDpressed = Keyboard.isDown 68
toggleOrientation bool =
  if bool then
    SetupOrientationToggle
  else
    NoOp

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
    aimShoot = Just
      { hover = Signal.forwardTo address PlayAim
      , click = Signal.forwardTo address PlayShoot
      }
    selectedShipId = model.selectedShipId
  in
  case model.state of
    Setup ->
      setupControlsView address model.player selectedShipId
    Play ->
      wrapper
        [ Html.div []
          [ Html.div [] [ Html.text "Player1" ]
          , Player.toHtml Nothing model.player
          , Html.div [] [ Html.text "Player2" ]
          , Player.toHtml aimShoot model.computer
          ]
        ]
    GameOver ->
      wrapper
      --[ Html.div []
        [ Player.toHtml Nothing model.player
        , Player.toHtml Nothing model.computer
        ]
      --]


setupControlsView : Signal.Address Action -> Player.Player -> Maybe Int -> Html.Html
setupControlsView address player selectedShipId =
  let
    hoverClick = Just
      { hover = Signal.forwardTo address SetupShowShip
      , click = Signal.forwardTo address SetupAddShip
      }
  in
  if Player.allShipsAdded player then
    Html.button [ Html.Events.onClick address SetupPlay ] [ Html.text "Start the game!" ]
  else
    let
    shipSelector = Html.div [Html.Attributes.style ["display" := "flex", "overflow" := "hidden", "border-radius" := "10px"]] <|
      List.map (shipFieldView address selectedShipId) (Player.getShips player)
    help = Html.div [Html.Attributes.style ["margin" := "20px 0px"]] [ Html.text "Press \"D\" to change ship's orientation" ]
    in
    wrapper (shipSelector :: help :: [Grid.toHtml hoverClick player.primaryGrid])

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
  = SetupOrientationToggle
  | SetupSelectShip (Maybe Int)
  | SetupShowShip (Maybe (Int, Int))
  | SetupAddShip ()
  | SetupPlay
  | PlayAim (Maybe (Int, Int))
  | PlayShoot ()
  | NoOp

update : Action -> Model -> Model
update action model =
  case action of
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
          in
            { model | player <- newPlayer
                    , selectedShipId <- nextShipId }
        Nothing ->
          model
    SetupPlay ->
      { model | state <- Play }
    PlayAim position ->
      model
    PlayShoot _ -> model
    NoOp -> model

toIntOrDefaultOrZero : String -> Int -> Int
toIntOrDefaultOrZero stringToConvert default =
  if stringToConvert == "" then 0 else
  case String.toInt stringToConvert of
    Ok n -> n
    _ -> default
