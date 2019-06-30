import Svg exposing (..)
import Html exposing (Html, p, div, text)
import Svg.Attributes exposing (x1, y1, x2, y2, stroke, fill, width, height, viewBox, points)
import Browser
import Browser.Events
import String
import List
import List.Extra
import Debug
import Task
import Time

type alias Vector2D = 
  { x : Float
  , y : Float
  }

type alias Vector3D = 
  { x : Float
  , y : Float
  , z : Float
  }


translate2d : Float -> Float -> Vector2D -> Vector2D
translate2d x y pos = {x = pos.x + x, y = pos.y + y}

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

boat_shape = List.map list_to_vector2d boat_coords

list_to_vector2d : List Float -> Vector2D
list_to_vector2d floats = case floats of
  [] -> {x = 0, y = 0}
  x1 :: y1 :: rest -> {x = x1, y = y1}
  x1 :: rest -> {x = x1, y = 0}

underwater_polygon : List Vector2D -> List Vector2D
underwater_polygon points = List.map (\p -> {x = p.x, y = max p.y 250}) points

coord_to_string_point : Vector2D -> String
coord_to_string_point pos = String.join "," [String.fromFloat pos.x, String.fromFloat pos.y]

coords_to_string_points : List Vector2D -> String
coords_to_string_points coords = String.join " " (List.map coord_to_string_point coords)

flip_vertical : Vector2D -> Vector2D
flip_vertical pos = {x = pos.x * 1, y = pos.y * -1}

polygon_area : List Vector2D -> Float
polygon_area points = let
                          offsets = case points of
                            [] -> []
                            head :: tail -> List.Extra.zip points (List.append tail [head])
                          x_y = List.foldl (\(u, d) acc -> acc + (u.x * d.y)) 0 offsets
                          y_x = List.foldl (\(u, d) acc -> acc + (u.y * d.x)) 0 offsets
                      in
                         (x_y - y_x) / 2

type Msg
  = Tick
  | NewWeight Float
  | NewCog Float Float

type alias Model =
  { weight : Float
  , cog : {x : Float, y: Float}
  , pos : {x : Float, y: Float}
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model 20000 {x=0, y=60} {x=250, y=340}
  , Task.perform Tick
  )


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Task.perform Tick)


main = Browser.element
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }


view : Model -> Html Msg
view model =
  let
    boat = (List.map (\c -> (translate2d model.pos.x model.pos.y (flip_vertical c))) boat_shape)
    boat_area = polygon_area boat
    underwater_shape = underwater_polygon boat
    underwater_area = polygon_area underwater_shape
  in
    div
      [ ]
      [ svg
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
            [ points (coords_to_string_points boat)
            , fill "green"
            , stroke "red"
            ][]
        ]
      , div
        []
        [ p [] [ text ("Gravity " ++ String.fromFloat model.weight) ]
        , p [] [ text ("Bouyancy ") ]
        , p [] [ text ("Whole Area " ++ String.fromFloat boat_area) ]
        , p [] [ text ("Underwater Area " ++ String.fromFloat underwater_area) ]
        ]
      ]
