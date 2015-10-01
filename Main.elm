-- Core
-- Evan
import Html
import Html.Events as Events
import StartApp.Simple as StartApp
-- 3rd Party
-- Battleship
import Ship

main =
  StartApp.start { model = model, view = view, update = update }

-- Model --
type alias Model = Ship.Ship
model : Model
model =
  Ship.init 4 Ship.Horizontal (0,0)

-- View --
view : Signal.Address Action -> Model -> Html.Html
view address model =
  Html.div []
    [ Html.div [] [Html.text (toString (Ship.getShipCoordinates model))] ]

-- Update --
type Action = Thing
update : Action -> Model -> Model
update action model =
  model
