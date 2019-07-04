import Svg exposing (..)
import Html exposing (Html, p, div, text, label, input, form, br)
import Html.Attributes exposing (type_, value, step)
import Html.Events exposing (onInput)
import Svg.Attributes exposing (style, x, y, cx, cy, r, x1, y1, x2, y2, stroke, fill, width, height, viewBox, points)
import Browser
import Browser.Events
import String
import List
import List.Extra
import Debug
import Tuple
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
  , [-100, 20]
  , [-150, 80]
  , [-150, 150]
  , [150, 150]
  , [150, 80]
  , [100, 20]
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
vector_sub : Vector2D -> Vector2D -> Vector2D
vector_sub v1 v2 = {x = v1.x - v2.x, y = v1.y - v2.y}
vector_mult : Vector2D -> Float -> Vector2D
vector_mult v m = {x = v.x * m, y = v.y * m}
vector_div : Vector2D -> Float -> Vector2D
vector_div v m = {x = v.x / m, y = v.y / m}


get_water_point : Vector2D -> Vector2D -> Vector2D
get_water_point v1 v2 = let
                            sides = vector_sub v1 v2
                            hypot = sqrt (sides.x^2 + sides.y^2)
                            vert_side = abs (v2.y - v1.y)
                            angle = asin (vert_side / hypot)
                            top_angle = (pi / 2) - angle
                            new_vert_side = abs (250 - v1.y)
                            horz = new_vert_side * (tan top_angle)
                        in
                           if horz == 0 then
                             {x = v2.x, y = max v2.y 250}
                           else if v1.x > v2.x then
                             {x = v1.x - horz, y = max v2.y 250}
                           else
                             {x = v1.x + horz, y = max v2.y 250}

underwater_polygon : List Vector2D -> List Vector2D
underwater_polygon points = Tuple.second
  ( List.foldl
      (\p shapes -> let
                      (hull, uw) = shapes
                      last_hull_point = case List.Extra.last hull of
                        Just lp -> lp
                        Nothing -> p
                    in
                      if last_hull_point.y < 250 && p.y < 250 then
                        (hull ++ [p], uw)
                      else if last_hull_point.y >= 250 && p.y >= 250 then
                        (hull ++ [p], uw ++ [p])
                      else if p.y < 250 && last_hull_point.y >= 250 then
                        let
                          water_point = get_water_point last_hull_point p
                        in
                          (hull ++ [p], uw ++ [water_point])
                      else
                        let
                          water_point = get_water_point p last_hull_point
                        in
                          (hull ++ [p], uw ++ [water_point, p])
      )
      ([], [])
      points
  )

coord_to_string_point : Vector2D -> String
coord_to_string_point pos = String.join "," [String.fromFloat pos.x, String.fromFloat pos.y]

coords_to_string_points : List Vector2D -> String
coords_to_string_points coords = String.join " " (List.map coord_to_string_point coords)

flip_vertical : Vector2D -> Vector2D
flip_vertical pos = {x = pos.x * 1, y = pos.y * -1}

rotate_point : Vector2D -> Vector2D -> Float -> Vector2D
rotate_point vec centroid angle = let
                                     vc = { x = vec.x - centroid.x , y = vec.y - centroid.y}
                                     vm = { x = (vc.x * cos (degrees angle)) + (vc.y * sin (degrees angle))
                                          , y = (vc.x * -1 * sin (degrees angle)) + (vc.y * cos (degrees angle))
                                          }
                                  in
                                     vector_add vm centroid

rotate_polygon : List Vector2D -> Vector2D -> Float -> List Vector2D
rotate_polygon points centroid angle = List.map (\p -> rotate_point p centroid angle) points


polygon_area : List Vector2D -> Float
polygon_area points = let
                          offsets = case points of
                            [] -> []
                            head :: tail -> List.Extra.zip points (List.append tail [head])
                          x_y = List.foldl (\(u, d) acc -> acc + (u.x * d.y)) 0 offsets
                          y_x = List.foldl (\(u, d) acc -> acc + (u.y * d.x)) 0 offsets
                      in
                         (x_y - y_x) / 2

get_polygon_triangles : List Vector2D -> List (Vector2D, Vector2D, Vector2D)
get_polygon_triangles points =
  let
    skip_range = List.range 0 ((List.length points) - 2)

    tri_sets = List.map (\skip ->
      let
        t1_s = points
        t2_s = (List.drop (1+skip) points) ++ (List.take (1+skip) points)
        t3_s = (List.drop (2+skip) points) ++ (List.take (2+skip) points)
      in
        (List.map3 (\a b c -> (a,b,c)) t1_s t2_s t3_s)
      ) skip_range
  in
    List.concat tri_sets

polygon_center : List Vector2D -> Vector2D
polygon_center points = case points of
  [] -> {x=0, y=0}
  p1 :: p2 :: ps ->
    let
      t1_s = List.take ((List.length points) - 3) points
      t2_s = List.take ((List.length points) - 3) (List.drop 1 points)
      t3_s = List.drop 3 points
      triangles = get_polygon_triangles points

      (area, centroid) = List.foldl
        (\(a, b, c) (area_total, centroid_total) ->
          let
            tri_area = ( (a.x * (b.y - c.y))
                       + (b.x * (c.y - a.y))
                       + (c.x * (a.y - b.y))
                       ) / 2.0
          in
            if tri_area == 0 then
               (area_total, centroid_total)
            else
              let
                tri_centroid = { x = (a.x + b.x + c.x) / 3.0
                               , y = (a.y + b.y + c.y) / 3.0
                               }
              in
                ( area_total + tri_area
                , { x = ((area_total * centroid_total.x) + (tri_area * tri_centroid.x)) / (area_total + tri_area)
                  , y = ((area_total * centroid_total.y) + (tri_area * tri_centroid.y)) / (area_total + tri_area)
                  }  
                )
        )
        (0, p1)
        triangles
    in
       centroid
  p1 :: _ -> p1


type Msg
  = TickDelta Float
  | NewWeight Float
  | NewHeelForce Float
  | NewWaterDensity Float
  | NewCogVertical Float
  | NewCogHorizontal Float

type alias Model =
  { weight : Float
  , time : Float
  , heel_force : Float
  , water_density : Float
  , rcog : Vector2D
  , pos : Vector2D
  , rot : Float
  , vel : Vector2D
  , rot_vel : Float
  , acog : Vector2D
  , acob : Vector2D
  , acof : Vector2D
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { weight = 20000
    , time = 0
    , water_density = 1.000
    , heel_force = 0
    , rcog = {x=0, y=60}
    , pos = {x=250, y=360}
    , rot = 20
    , vel = {x=0, y=0}
    , rot_vel = 0
    , acog = {x=250, y=250}
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
  NewWeight new_weight ->
    ( {model | weight=new_weight}
    , Platform.Cmd.none
    )
  NewWaterDensity new_water_density ->
    ( {model | water_density=new_water_density}
    , Platform.Cmd.none
    )
  NewHeelForce new_heel_force ->
    ( {model | heel_force=new_heel_force}
    , Platform.Cmd.none
    )
  NewCogVertical new_vertical_cog ->
    ( {model | rcog = { x = model.rcog.x , y = new_vertical_cog}}
    , Platform.Cmd.none
    )
  NewCogHorizontal new_horizontal_cog ->
    ( {model | rcog = { y = model.rcog.y , x = new_horizontal_cog}}
    , Platform.Cmd.none
    )
  TickDelta delta ->
    let
      boat = (List.map (\c -> (translate2d model.pos.x model.pos.y (flip_vertical c))) boat_shape)
      boat_area = polygon_area boat
      rotated_boat = rotate_polygon boat model.acof model.rot
      rotated_underwater_shape = underwater_polygon rotated_boat
      rotated_underwater_area = polygon_area rotated_underwater_shape
      buoyancy_power = rotated_underwater_area * model.water_density
      acob = (polygon_center rotated_underwater_shape)
      down_force = {x = 0, y = model.weight}
      buoyancy_force = {x = 0, y = buoyancy_power * -1}
      d = delta / 50000
      acog = rotate_point (vector_add {x = model.pos.x, y = model.pos.y} (vector_mult model.rcog -1)) model.acof model.rot

      heel = model.heel_force
      righting_arm = Debug.log "righting" <| ((buoyancy_power * acob.x) - (model.weight * acog.x)) / 1000

      rot_accel = heel + righting_arm
      rot_vel_mod = rot_accel * d * 100
      new_rot_vel = model.rot_vel + rot_vel_mod
      rot_displace = (model.rot_vel * d) + (rot_accel * 0.35 * d)
      new_rot = model.rot + rot_displace

      accel = (vector_add buoyancy_force down_force)
      vel_mod = vector_mult (vector_add buoyancy_force down_force) d
      new_vel = vector_add model.vel vel_mod
      displacement = vector_add (vector_mult model.vel d) (vector_mult (vector_mult accel 0.5) d)
      new_pos = vector_add model.pos displacement
    in
      ( {model | acog = acog
               , acob = acob
               , pos = new_pos
               , vel = new_vel
               , rot = new_rot
               , rot_vel = new_rot_vel
               , time = model.time + delta
               }
      , Platform.Cmd.none
      )


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
    rotated_boat = rotate_polygon boat model.acof model.rot
    boat_area = polygon_area boat
    underwater_shape = underwater_polygon boat
    rotated_underwater_shape = underwater_polygon rotated_boat
    rotated_underwater_area = polygon_area rotated_underwater_shape
    tris_rotated_boat = List.map (\(a,b,c) -> [a,b,c,a]) (get_polygon_triangles rotated_underwater_shape)
    rotated_acob = (polygon_center rotated_underwater_shape)
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
            [ points (coords_to_string_points rotated_boat)
            , fill "white"
            , stroke "red"
            ][]
        , polyline
            [ points (coords_to_string_points rotated_underwater_shape)
            , fill "none"
            , stroke "blue"
            ][]
        -- , polyline
        --     [ points (coords_to_string_points underwater_shape)
        --     , fill "none"
        --     , stroke "blue"
        --     ][]
        -- , g
        --     []
        --     (List.map (\tri ->
        --       polyline
        --         [ points (coords_to_string_points tri)
        --         -- , fill "red"
        --         , fill "red"
        --         , stroke "purple"
        --         ][]
        --     ) tris_rotated_boat)
        , line
            [ x1 "0"
            , y1 "250"
            , x2 "500"
            , y2 "250"
            , stroke "blue"
            ][]
        , line
            [ x1 (String.fromFloat model.acog.x)
            , y1 (String.fromFloat model.acog.y)
            , x2 (String.fromFloat model.acob.x)
            , y2 (String.fromFloat model.acog.y)
            , stroke "#bbb"
            ][]
        , line
            [ x1 (String.fromFloat model.acob.x)
            , y1 (String.fromFloat model.acob.y)
            , x2 (String.fromFloat model.acob.x)
            , y2 (String.fromFloat model.acog.y)
            , stroke "#bbb"
            ][]
        , circle
            [ cx (String.fromFloat model.acog.x)
            , cy (String.fromFloat model.acog.y)
            , r "2"
            , fill "red"
            ][]
        , text_
            [ x (String.fromFloat (model.acog.x + 4))
            , y (String.fromFloat (model.acog.y + 2))
            , style "font-family: sans-serif; font-size: 8px;"
            ][ text "G" ]
        , circle
            [ cx (String.fromFloat model.acob.x)
            , cy (String.fromFloat model.acob.y)
            , r "2"
            , fill "blue"
            ][]
        , text_
            [ x (String.fromFloat (model.acob.x + 4))
            , y (String.fromFloat (model.acob.y + 2))
            , style "font-family: sans-serif; font-size: 8px;"
            ][ text "B" ]
        , circle
            [ cx (String.fromFloat model.acob.x)
            , cy (String.fromFloat model.acog.y)
            , r "2"
            , fill "purple"
            ][]
        , text_
            [ x (String.fromFloat (model.acob.x + 4))
            , y (String.fromFloat (model.acog.y + 2))
            , style "font-family: sans-serif; font-size: 8px;"
            ][ text "Z" ]
        , circle
            [ cx (String.fromFloat model.acof.x)
            , cy (String.fromFloat model.acof.y)
            , r "2"
            , fill "green"
            ][]
        , text_
            [ x (String.fromFloat (model.acof.x - 8))
            , y (String.fromFloat (model.acof.y - 4))
            , style "font-family: sans-serif; font-size: 8px;"
            ][ text "COF" ]
        ]
      , form
        []
        [ label
          []
          [ text "Vessel Weight"
          , input
            [ type_ "number"
            , value (String.fromInt (round model.weight))
            , step "1000"
            , onInput (\s -> NewWeight (Maybe.withDefault 10 (String.toFloat s)))
            ][]
          ]
        , br [] []
        , label
          []
          [ text "Water Density (specific gravity)"
          , input
            [ type_ "number"
            , value (String.fromFloat model.water_density)
            , step "0.005"
            , onInput (\s -> NewWaterDensity (Maybe.withDefault 1 (String.toFloat s)))
            ][]
          ]
        , br [] []
        , label
          []
          [ text "Vertical Centre of Gravity"
          , input
            [ type_ "number"
            , value (String.fromInt (round model.rcog.y))
            , step "5"
            , onInput (\s -> NewCogVertical (Maybe.withDefault 60 (String.toFloat s)))
            ][]
          ]
        , br [] []
        , label
          []
          [ text "Tranverse Centre of Gravity"
          , input
            [ type_ "number"
            , value (String.fromInt (round model.rcog.x))
            , step "5"
            , onInput (\s -> NewCogHorizontal (Maybe.withDefault 0 (String.toFloat s)))
            ][]
          ]
        , br [] []
        , label
          []
          [ text "Heeling Force"
          , input
            [ type_ "number"
            , value (String.fromInt (round model.heel_force))
            , step "100"
            , onInput (\s -> NewHeelForce (Maybe.withDefault 0 (String.toFloat s)))
            ][]
          ]
        , p [] [ text ("Bouyancy Force " ++ String.fromInt (round (rotated_underwater_area * model.water_density))) ]
        ]
      ]
