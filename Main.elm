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
import AI

---- MAIN ----
main = .html <|
  StartApp.start
    { init = (defaultModel, Effects.none)
    , view = view
    , update = (\ a m -> (update a m, Effects.none))
    , inputs =
      [ Signal.map toggleOrientation <| Keyboard.isDown 68 {- D -}
      , Signal.map updateSeed <| Time.every 3000
      ]
    }

---- INPUT PROCESSORS ----

updateSeed : Float -> Action
updateSeed fl =
  UpdateSeed <| floor fl

toggleOrientation : Bool -> Action
toggleOrientation bool =
  if bool then
    SetupOrientationToggle
  else
    NoOp

---- MODEL ----

defaultModel : Model
defaultModel =
  { state = Setup
  , selectedShipId = Just 0 -- Potentially not reliable if we want to change the underlying model, a better way would be to explicitly get the first ID
  , hoverPos = Nothing
  , player = Player.defaultPlayer
  , computer = Player.defaultComputer
  , seed = 0
  }
-- Model
type alias Model =
  { state : State
  , selectedShipId : Maybe Int
  , hoverPos : Maybe (Int, Int)
  , player : Player.Player
  , computer : Player.Player
  , seed : Int
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
  in
    wrapper <| (setupControlsView address model selectedShipId) ++
      [ spacer
      , Player.viewTrackingGrid aimShoot model.player
      -- for debugging.
      , spacer
      , Player.viewPrimaryGrid Nothing model.computer
      ]


setupControlsView : Signal.Address Action -> Model -> Maybe Int -> List Html.Html
setupControlsView address model selectedShipId =
  let
    hoverClick = Just
      { hover = Signal.forwardTo address SetupShowShip
      , click = Signal.forwardTo address SetupAddShip
      }
    shipSelector = Html.div
      [ Html.Attributes.style
        [ "display" := "flex"
        , "overflow" := "hidden"
        , "border-radius" := "10px"
        ]
      ] <| List.map (shipFieldView address selectedShipId) (Player.getShips model.player)
    hint = Html.div
      [ Html.Attributes.style ["margin" := "20px 0px"] ]
      [ Html.text "Press \"D\" to change ship's orientation" ]
  in
    [ shipSelector
    , hint
    , Player.previewShip hoverClick model.hoverPos selectedShipId model.player
    ]

shipFieldView : Signal.Address Action -> Maybe Int -> Ship.Ship -> Html.Html
shipFieldView address selectedShipId ship =
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
  | PlayAim (Maybe (Int, Int))
  | PlayShoot (Int, Int)
  | UpdateSeed Int
  | NoOp

update : Action -> Model -> Model
update action model =
  case action of
    SetupRandomOpponent seed ->
      -- TODO For some reason freezing the time signal never updates the
      -- computer player, but if not frozen this function has to run each time
      -- the signal updates, which is inefficient
      if Player.allShipsAdded model.computer then
        model
      else
        { model | computer <- Player.random seed }
    SetupOrientationToggle ->
      case model.selectedShipId of
        Just shipId ->
          { model | player <- Player.updateShip shipId Ship.toggleOrientation model.player }
        Nothing ->
          model
    SetupSelectShip shipId ->
      { model | selectedShipId <- shipId }
    SetupShowShip maybePos ->
      { model | hoverPos <- maybePos }
    SetupAddShip pos ->
      case model.selectedShipId of
        Just shipId ->
          let
            newPlayer =
              model.player
                |> Player.updateShip shipId (\ship -> Ship.setLocation pos ship)
                |> Player.addShip shipId
            nextShipId = Player.nextNotAddedShipId newPlayer
          in
            { model |
                player <- newPlayer,
                computer <- Player.random model.seed,
                selectedShipId <- nextShipId,
                -- If nextShipId is `Nothing`, It's time to `Play`
                state <- if nextShipId == Nothing then Play  else model.state
            }
        Nothing ->
          model
    PlayAim position ->
      model
    PlayShoot pos ->
      let
        (player, computer) = Player.shoot pos model.player model.computer
        (newComputer, newPlayer) = AI.randomShot model.seed computer player
      in
      { model |
        player <- newPlayer,
        computer <- newComputer
      }
    UpdateSeed seed ->
      { model | seed <- seed }
    NoOp -> model
