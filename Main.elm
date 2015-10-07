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
        [ Html.div []
          [ Html.div [] [ Html.text "Player1" ]
          , Player.toHtml model.player
          , Html.div [] [ Html.text "Player2" ]
          , Player.toHtml model.computer
          ]
        ]
    GameOver ->
      Html.div []
        [ Html.div [] [ Player.toHtml model.player, Player.toHtml model.computer ] ]


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
    Html.div [] (html ++ [Grid.toHtml player.primaryGrid])


-- Depending on the Action render the proper html input.
-- TODO this might belong in the Ship module
shipFieldView : Signal.Address Action -> Ship.Ship -> Html.Html
shipFieldView address ship =
  if not ship.added then
    Html.div [ Html.Attributes.style [("margin-bottom", "5px")] ]
    [ Html.input -- Ship Row Input
      [ Html.Attributes.value (toString (Ship.getRow ship))
      , Html.Events.on "input" Html.Events.targetValue (Signal.message address << SetupRowField ship.id)
      ]
      []
    , Html.input -- Ship Column Input
      [ Html.Attributes.style [("margin-right", "5px")]
      , Html.Attributes.value (toString (Ship.getColumn ship))
      , Html.Events.on "input" Html.Events.targetValue (Signal.message address << SetupColumnField ship.id)
      ]
      []
      -- Ship orientation:
    , orientationSwitch address ship.id ship.orientation
    , Html.button -- Add Ship
      [ Html.Events.onClick address (SetupAddShip ship.id)
      ]
      [ Html.text "Add ship" ]
    ]
  else Html.div [] []

orientationSwitch : Signal.Address Action -> Int -> Ship.Orientation -> Html.Html
orientationSwitch address shipId orientation =
  let
    outerDivStyle = Html.Attributes.style
      [ ("display", "inline-block")
      , ("border", "1px solid gray")
      , ("border-radius", "4px")
      , ("margin-right", "5px")
      -- Fix the border-radius corners glitch
      , ("overflow", "auto"), ("vertical-align", "middle")
      ]
    innerDivStyle =
      [ ("display", "inline-block")
      , ("padding", "5px")
      , ("cursor", "pointer")
      ]
    hCheckedStyle =
      case orientation of
        Ship.Horizontal ->
          ("background-color", "#DCDCDC")
        Ship.Vertical ->
          ("background-color", "white")
    vCheckedStyle =
      case orientation of
        Ship.Horizontal ->
          ("background-color", "white")
        Ship.Vertical ->
          ("background-color", "#DCDCDC")
  in
  Html.div [outerDivStyle]
  [ Html.div
    [ Html.Attributes.style <| ("border-right", "1px solid gray") :: hCheckedStyle :: innerDivStyle
    , Html.Events.onMouseDown address (SetupOrientationField shipId)
    ]
    [ Html.text "Horizontal" ]
  , Html.div
    [ Html.Attributes.style <| vCheckedStyle :: innerDivStyle
    , Html.Events.onMouseDown address (SetupOrientationField shipId)
    ]
    [ Html.text "Vertical" ]
  ]

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
