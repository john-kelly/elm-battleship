module Battleship (main) where

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
import Fleet
import Ship

---- MAIN ----
main =
  StartApp.start { model = defaultModel, view = view, update = update }

---- MODEL ----
defaultModel : Model
defaultModel =
  { state = Setup
  , selectedShipId = Nothing
  , player = Player.defaultPlayer
  , computer = Player.defaultComputer
  }
-- Model
type alias Model =
  { state : State
  , selectedShipId : Maybe Int
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
  Html.div
    [ Html.Attributes.style
      [ ("margin", "50px")
      , ("text-align", "center")
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
    html = Html.div [Html.Attributes.style ["display" := "inline-block", "text-align" := "right"]] <|
      List.map (shipFieldView address selectedShipId) (Player.getShips player)
        --player
        --  |> Player.getShips
        --  |> List.map (shipFieldView address)
    in
    wrapper (html :: [Grid.toHtml hoverClick player.primaryGrid])


-- Depending on the Action render the proper html input.
-- TODO this might belong in the Ship module
shipFieldView : Signal.Address Action -> Maybe Int -> Ship.Ship -> Html.Html
shipFieldView address selectedShipId ship  =
  let
    isSelected =
      case selectedShipId of
        Just id -> id == ship.id
        Nothing -> False
    hiddenStyle =
      if ship.added then
        ["visibility" := "hidden"]
      else []
  in
    Html.div [ Html.Attributes.style <| ["margin-bottom" := "5px"] ++ hiddenStyle ]
    [ -- Ship selector:
      shipListView address ship isSelected
      -- Ship orientation:
    , orientationSwitch address ship.id ship.orientation
    ]

shipListView : Signal.Address Action -> Ship.Ship -> Bool -> Html.Html
shipListView address ship isSelected =
  let
    selectedStyle =
      if isSelected then
        [ "background-color" := "gray" ]
      else
        []
    box = Html.div
      [ Html.Attributes.style <|
        [ "display" := "inline-block"
        , "width" := "20px"
        , "height" := "20px"
        , "border" := "1px solid gray"
        , "border-radius" := "3px"
        , "margin-right" := "1px"
        , "vertical-align" := "middle"
        ] ++ selectedStyle
      ] []
  in
    Html.div
    [ Html.Attributes.style
      [ "display" := "inline-block"
      , "margin-right" := "5px"
      , "cursor" := "pointer"
      ]
    , Html.Events.onMouseDown address <|
        if not isSelected then
          SetupSelectShip (Just ship.id)
        else
          SetupSelectShip Nothing
    ] <| List.repeat ship.length box

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
  | SetupSelectShip (Maybe Int)
  | SetupShowShip (Maybe (Int, Int))
  | SetupAddShip ()
  | SetupPlay
  | PlayAim (Maybe (Int, Int))
  | PlayShoot ()

update : Action -> Model -> Model
update action model =
  case action of
    SetupOrientationField shipId ->
      { model | player <- Player.updateShip shipId Ship.toggleOrientation model.player }
    SetupSelectShip shipId ->
      { model | selectedShipId <- shipId }
    SetupShowShip position ->
      case model.selectedShipId of
        Just id ->
          let -- TODO Put all this horror inside a function
          oldShip =
            case Fleet.getShip id model.player.fleet of
              Just ship ->
                ship
              Nothing -> -- Never gonna happen
                Ship.init 1 Ship.Horizontal (0,0)
          grid = Grid.hideShip oldShip model.player.fleet model.player.primaryGrid
          in
          case position of
            Just pos ->
              let
                newShip =
                  case Fleet.getShip id model.player.fleet of
                    Just ship ->
                      Ship.setLocation pos ship
                    Nothing -> -- Never gonna happen
                      Ship.init 1 Ship.Horizontal (0,0)
                g = Grid.showShip newShip model.player.fleet grid
                fn s = Ship.setLocation pos s
              in
              { model | player <- Player.updateGrid g <| Player.updateShip id fn model.player }
            Nothing ->
              { model | player <- Player.updateGrid grid model.player }
        Nothing ->
          model
    SetupAddShip _ ->
      case model.selectedShipId of
        Just id ->
          let newPlayer = Player.addShip id model.player
          in
          if newPlayer == model.player then
            model
          else
            { model | player <- newPlayer
                    , selectedShipId <- Nothing }
        Nothing ->
          model
    SetupPlay ->
      { model | state <- Play }
    PlayAim position ->
      model
    PlayShoot _ -> model

toIntOrDefaultOrZero : String -> Int -> Int
toIntOrDefaultOrZero stringToConvert default =
  if stringToConvert == "" then 0 else
  case String.toInt stringToConvert of
    Ok n -> n
    _ -> default