import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp

---- MODEL ----

type alias Model =
  { players : List Player
  , selected : Player
  }

type alias Player =
  { name : String
  , points : Int
  , id : Int
  }

model : Model
model =
   { players =
      [ { name = "Nikola Tesla"
        , points = 0
        , id = 0
        }
      , { name = "Grace Hopper"
        , points = 0
        , id = 1
        }
      , { name = "Ada Lovelace"
        , points = 0
        , id = 2
        }
      , { name = "Albert Einstein"
        , points = 0
        , id = 3
        }
      , { name = "Carl Friedrich Gauss"
        , points = 0
        , id = 4
        }
      , { name = "Marie Curie"
        , points = 0
        , id = 5
        }
      , { name = "Claude Shannon"
        , points = 0
        , id = 6
        }
      ]
  , selected =
      { name = "Claude Shannon"
      , points = 0
      , id = 6
      }
  }

---- UPDATE ----
type Action
  = Select Player
  | Increment Int

update : Action -> Model -> Model
update action model =
  case action of
    Select selected ->
      { model | selected = selected }
    Increment id ->
      let updatePlayer p = if p.id == id then { p | points = p.points + 5 } else p
      in
        { model | players = List.map updatePlayer model.players }

---- VIEW ----
helper : List a -> a -> Int -> Int
helper lst elem offset =
  case lst of
    []      -> -1
    x :: xs ->
      if x == elem then offset
      else helper xs elem (offset + 1)

indexOf lst element =
  helper lst element 0

view : Address Action -> Model -> Html
view address model =
  let
    sortedList = List.sortBy .points model.players
    reversedList = List.reverse sortedList
  in
    div [ class "container" ]
      [ header [] [ h1 [] [ text "Leaderboard" ] ]
      , ul [] (List.map (playerItem address reversedList) reversedList)
      , controls address model.selected
      ]

playerItem : Address Action -> List Player -> Player -> Html
playerItem address lst player =
  li
    [ onClick address (Select player) ]
    [ span [ class "position" ] [ text (toString (indexOf lst player + 1)) ]
    , span [ class "name" ] [ text player.name ]
    , span [ class "points" ] [ text (toString player.points) ]
    ]

controls : Address Action -> Player -> Html
controls address selected =
  footer []
    [ span [] [ text selected.name ]
    , button
        [ onClick address (Increment selected.id) ]
        [ text "Add 5 Points" ]
    ]

app =
  StartApp.start
    { model = model
    , update = update
    , view = view
    }

main = app
