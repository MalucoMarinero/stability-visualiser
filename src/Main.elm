import Svg exposing (..)
import Html exposing (Html, p, div, text)
import Svg.Attributes exposing (cx, cy, r, x1, y1, x2, y2, stroke, fill, width, height, viewBox, points)
import Browser
import Browser.Events
import String
import List
import List.Extra
import Debug
import Task
import Time
import Platform.Cmd

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

vector_add : Vector2D -> Vector2D -> Vector2D
vector_add v1 v2 = {x = v1.x + v2.x, y = v1.y + v2.y}
vector_mult : Vector2D -> Float -> Vector2D
vector_mult v m = {x = v.x * m, y = v.y * m}
vector_div : Vector2D -> Float -> Vector2D
vector_div v m = {x = v.x / m, y = v.y / m}

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

polygon_center : List Vector2D -> Vector2D
polygon_center points = let
                          ps = Maybe.withDefault [] (List.tail points)
                          xs = List.map (\v -> v.x) ps |> List.foldl (+) 0
                          ys = List.map (\v -> v.y) ps |> List.foldl (+) 0
                          x = xs / toFloat (List.length ps)
                          y = ys / toFloat (List.length ps)
                        in
                          {x=x, y=y}


type Msg
  = TickDelta Float
  | NewWeight Float
  | NewCog Float Float

type alias Model =
  { weight : Float
  , rcog : Vector2D
  , pos : Vector2D
  , vel : Vector2D
  , acob : Vector2D
  , acof : Vector2D
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { weight = 20000
    , rcog = {x=0, y=60}
    , pos = {x=250, y=100}
    , vel = {x=0, y=0}
    , acob = {x=250, y=250}
    , acof = {x=250, y=250}
    }
  , Platform.Cmd.none
  )


subscriptions : Model -> Sub Msg
subscriptions model =
  Browser.Events.onAnimationFrameDelta TickDelta



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  TickDelta delta ->
    let
      boat = (List.map (\c -> (translate2d model.pos.x model.pos.y (flip_vertical c))) boat_shape)
      boat_area = polygon_area boat
      underwater_shape = underwater_polygon boat
      underwater_area = polygon_area underwater_shape
      acob = (polygon_center underwater_shape)
      down_force = {x = 0, y = model.weight}
      buoyancy_force = {x = 0, y = underwater_area * -1}
      d = delta / 50000
      accel = (vector_add buoyancy_force down_force)
      vel_mod = vector_mult (vector_add buoyancy_force down_force) d
      new_vel = vector_add model.vel vel_mod
      displacement = vector_add (vector_mult model.vel d) (vector_mult (vector_mult accel 0.5) d)
      new_pos = vector_add model.pos displacement
    in
      (
        {model | acob=acob, pos=new_pos, vel=new_vel},
        Platform.Cmd.none
      )
  _ -> (model, Platform.Cmd.none)


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
    acog = vector_add {x = model.pos.x, y = model.pos.y} (vector_mult model.rcog -1)
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
        [ polyline
            [ points (coords_to_string_points boat)
            , fill "white"
            , stroke "red"
            ][]
        , line
            [ x1 "0"
            , y1 "250"
            , x2 "500"
            , y2 "250"
            , stroke "blue"
            ][]
        , circle
            [ cx (String.fromFloat acog.x)
            , cy (String.fromFloat acog.y)
            , r "2"
            , fill "red"
            ][]
        , circle
            [ cx (String.fromFloat model.acob.x)
            , cy (String.fromFloat model.acob.y)
            , r "2"
            , fill "blue"
            ][]
        ]
      , div
        []
        [ p [] [ text ("Gravity " ++ String.fromFloat model.weight) ]
        , p [] [ text ("Bouyancy " ++ String.fromInt (round underwater_area)) ]
        ]
      ]
