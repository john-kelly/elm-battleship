module Battleship where

-- Core
import String
-- Evan
import Html
import Html.Attributes
import Html.Events
import StartApp.Simple as StartApp
-- 3rd Party
-- Battleship
import Grid
import Player
import Ship

---- MAIN ----
main =
  StartApp.start { model = defaultModel, view = view, update = update }

---- MODEL ----
defaultModel : Model
defaultModel =
  { state = Setup
  , player = Player.defaultPlayer
  , computer = Player.defaultComputer
  }
-- Model
type alias Model =
  { state : State
  , player : Player.Player
  , computer : Player.Player
  }
-- State
type State
  = Setup
  | Play
  | GameOver

---- VIEW ----
view : Signal.Address Action -> Model -> Html.Html
view address model =
  case model.state of
    Setup -> setupControlsView address model.player
    Play ->
      Html.div []
        [ Html.div [] [ Html.text (toString model) ] ]
    GameOver ->
      Html.div []
        [ Html.div [] [ Html.text (toString model) ] ]


setupControlsView : Signal.Address Action -> Player.Player -> Html.Html
setupControlsView address player =
  if Player.allShipsAdded player then
    Html.button [ Html.Events.onClick address SetupPlay ] []
  else
    let
    html = player
      |> Player.getShips
      |> List.map (shipFieldView address)
    in
    Html.div [] (html ++ [Html.text (toString player.fleet), Grid.toHtml player.primaryGrid])


-- Depending on the Action render the proper html input.
-- TODO this might belong in the Ship module
shipFieldView : Signal.Address Action -> Ship.Ship -> Html.Html
shipFieldView address ship =
  if not ship.added then
    Html.div []
    [ Html.input -- Ship Row Input
      [ Html.Attributes.value (toString (Ship.getRow ship))
      , Html.Events.on "input" Html.Events.targetValue (Signal.message address << SetupRowField ship.id)
      ]
      []
    , Html.input -- Ship Column Input
      [ Html.Attributes.value (toString (Ship.getColumn ship))
      , Html.Events.on "input" Html.Events.targetValue (Signal.message address << SetupColumnField ship.id)
      ]
      []
    , Html.input -- Ship Orientation
      [ Html.Attributes.type' "radio"
      , Html.Attributes.checked (ship.orientation == Ship.Horizontal)
      , Html.Events.onClick address (SetupOrientationField ship.id)
      ]
      []
    , Html.button -- Add Ship
      [ Html.Events.onClick address (SetupAddShip ship.id)
      ]
      []
    ]
  else Html.div [] []

---- UPDATE ----
type Action
  = SetupOrientationField Int
  | SetupRowField Int String
  | SetupColumnField Int String
  | SetupAddShip Int
  | SetupPlay

update : Action -> Model -> Model
update action model =
  case action of
    SetupOrientationField shipId ->
      { model | player <- Player.updateShip shipId Ship.toggleOrientation model.player }
    SetupRowField shipId rowAsString ->
      let
      updateRow ship =
        Ship.setRow (toIntOrDefaultOrZero rowAsString (Ship.getRow ship)) ship
      in
      { model | player <- Player.updateShip shipId updateRow model.player }
    SetupColumnField shipId columnAsString ->
      let
      updateColumn ship =
        Ship.setColumn (toIntOrDefaultOrZero columnAsString (Ship.getColumn ship)) ship
      in
      { model | player <- Player.updateShip shipId updateColumn model.player }
    SetupAddShip shipId ->
      { model | player <- Player.addShip shipId model.player}
    SetupPlay ->
      { model | state <- Play }

toIntOrDefaultOrZero : String -> Int -> Int
toIntOrDefaultOrZero stringToConvert default =
  if stringToConvert == "" then 0 else
  case String.toInt stringToConvert of
    Ok n -> n
    _ -> default
