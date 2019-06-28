import Svg exposing (..)
import Svg.Attributes exposing (..)
import String
import List



boat_coords =
  [ [0, 0]
  , [-50, 10]
  , [-100, 20]
  , [-150, 80]
  , [-150, 150]
  , [150, 150]
  , [150, 80]
  , [100, 20]
  , [50, 10]
  , [0, 0]
  ]


coord_to_string_point : List Float -> String
coord_to_string_point coords = String.join "," (List.map String.fromFloat coords)

coords_to_string_points : List (List Float) -> String
coords_to_string_points coords = String.join " " (List.map coord_to_string_point coords)


translate : Float -> Float -> List Float -> List Float
translate x y coords = case coords of
  [] -> [0 + x, 0 + y]
  x1 :: y1 :: rest -> [x1 + x, y1 + y]
  x1 :: rest -> [x1 + x, 0 + y]

flip_vertical : List Float -> List Float
flip_vertical coords = case coords of
  [] -> [0 * 1, 0 * -1]
  x1 :: y1 :: rest -> [x1 * 1, y1 * -1]
  x1 :: rest -> [x1 * 1, 0 * -1]


boatTranslate = translate 250 340


type Msg
  = Tick
  | NewWeight Float
  | NewCog Float Float

type alias Model =
  { weight : Float
  , cog : {x : Float, y: Float}
  , pos : {x : Float, y: Float}
  }

init : Model
init =
  Model 200 {x=0, y=60} {x=250, y=340}



main = view init


view : Model -> Svg Msg
view model =
  svg
    [ width "500"
    , height "500"
    , viewBox "0 0 500 500"
    ]
    [ line
        [ x1 "0"
        , y1 "250"
        , x2 "500"
        , y2 "250"
        , stroke "blue"
        ][]
    , polyline
        [ points (coords_to_string_points (List.map (\c -> (translate model.pos.x model.pos.y (flip_vertical c))) boat_coords))
        , fill "green"
        , stroke "red"
        ][]
    ]
