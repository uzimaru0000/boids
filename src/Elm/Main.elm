module Main exposing (main)

import Browser
import Browser.Events as Browser
import Canvas exposing (..)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Random exposing (Generator)


type alias Pos =
    { x : Float
    , y : Float
    }


type alias Bird =
    { pos : Pos
    , vec : Pos
    , axel : Pos
    }


type alias Obj =
    { radius : Float
    , pos : Pos
    }


type alias Model =
    { birds : List Bird
    }


type Msg
    = NoOp
    | GenBirds (List Bird)
    | Tick Float


width : Float
width =
    640


height : Float
height =
    640


maxSpeed : Float
maxSpeed =
    2


minSpeed : Float
minSpeed =
    2


birdRadius : Float
birdRadius =
    5


posGenerator : ( Float, Float ) -> ( Float, Float ) -> Generator Pos
posGenerator ( xmin, xmax ) ( ymin, ymax ) =
    Random.map2 Pos
        (Random.float xmin xmax)
        (Random.float ymin ymax)


birdGenerator : Float -> Float -> Generator Bird
birdGenerator w h =
    Random.map3 Bird
        (posGenerator ( 50, w - 50 ) ( 50, h - 50 ))
        (posGenerator ( -1, 1 ) ( -1, 1 )
            |> Random.map normal
            |> Random.map2 mul (Random.float minSpeed maxSpeed)
        )
        (Random.constant <| Pos 0 0)



-- a + b


add : Pos -> Pos -> Pos
add a b =
    { x = a.x + b.x, y = a.y + b.y }



-- a - b


sub : Pos -> Pos -> Pos
sub a b =
    { x = a.x - b.x, y = a.y - b.y }



-- n * v


mul : Float -> Pos -> Pos
mul n pos =
    { x = pos.x * n, y = pos.y * n }


divide : Float -> Pos -> Pos
divide n pos =
    if n == 0 then
        pos

    else
        mul (1 / n) pos


length : Pos -> Float
length pos =
    (pos.x ^ 2 + pos.y ^ 2)
        |> sqrt


normal : Pos -> Pos
normal p =
    divide (length p) p


distance : Pos -> Pos -> Float
distance p1 p2 =
    sub p1 p2
        |> length


init : () -> ( Model, Cmd Msg )
init _ =
    ( { birds = []
      }
    , birdGenerator width height
        |> Random.list 100
        |> Random.generate GenBirds
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenBirds birds ->
            ( { model | birds = birds }
            , Cmd.none
            )

        Tick d ->
            ( { model
                | birds =
                    model.birds
                        |> List.map (birdUpdate (d / 50) model.birds)
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


birdUpdate : Float -> List Bird -> Bird -> Bird
birdUpdate d birds bird =
    let
        bs =
            birds
                ++ virtualBird bird

        f =
            [ separateForce
                (bs
                    |> List.filter
                        (\b ->
                            distance b.pos bird.pos
                                <= (5 * birdRadius)
                        )
                )
                bird
            , alignmentForce
                (bs
                    |> List.filter
                        (\b ->
                            let
                                dis =
                                    distance b.pos bird.pos
                            in
                            dis
                                > (5 * birdRadius)
                                && dis
                                <= (6 * birdRadius)
                        )
                )
                bird
                |> normal
            , cohesionForce
                (bs
                    |> List.filter
                        (\b ->
                            let
                                dis =
                                    distance b.pos bird.pos
                            in
                            dis <= (5 * birdRadius)
                        )
                )
                bird
                |> normal
            ]

        fix =
            [ 5
            , 2
            , 1
            ]

        a =
            List.map2 mul fix f
                |> List.foldl add (Pos 0 0)
    in
    { pos = add bird.pos <| move d bird.vec bird.axel
    , vec = addForce d bird.vec bird.axel
    , axel = a
    }


move : Float -> Pos -> Pos -> Pos
move d v0 f =
    add (mul d v0) (mul (0.5 * d ^ 2) f)


addForce : Float -> Pos -> Pos -> Pos
addForce t v0 f =
    let
        v =
            add (mul t f) v0
    in
    normal v
        |> mul (clamp minSpeed maxSpeed (length v))


virtualBird : Bird -> List Bird
virtualBird { pos, vec } =
    let
        up =
            Bird
                (Pos pos.x 0)
                vec
                (Pos 0 0)

        down =
            Bird
                (Pos pos.x height)
                vec
                (Pos 0 0)

        left =
            Bird
                (Pos 0 pos.y)
                vec
                (Pos 0 0)

        right =
            Bird
                (Pos width pos.y)
                vec
                (Pos 0 0)
    in
    [ up, down, left, right ]


distanceForce : Float -> Pos -> Pos -> Float
distanceForce m p1 p2 =
    distance p1 p2
        |> (+) -birdRadius
        |> (^) 2
        |> (\x -> m / x)


separateForce : List Bird -> Bird -> Pos
separateForce birdList bird =
    if List.length birdList == 0 then
        Pos 0 0

    else
        let
            fs =
                birdList
                    |> List.map .pos
                    |> List.map (distanceForce 1000 bird.pos)

            dirs =
                birdList
                    |> List.map .pos
                    |> List.map (\v -> sub bird.pos v)
                    |> List.map normal
        in
        List.map2 mul fs dirs
            |> List.foldl add (Pos 0 0)
            |> divide (birdList |> List.length |> toFloat)


alignmentForce : List Bird -> Bird -> Pos
alignmentForce birdList bird =
    if List.length birdList == 0 then
        Pos 0 0

    else
        sub
            (birdList
                |> List.map .vec
                |> List.foldl add (Pos 0 0)
                |> divide (List.length birdList |> toFloat)
            )
            bird.vec


cohesionForce : List Bird -> Bird -> Pos
cohesionForce birdList bird =
    if List.length birdList == 0 then
        Pos 0 0

    else
        sub
            (birdList
                |> List.map .pos
                |> List.foldl add (Pos 0 0)
                |> divide (List.length birdList |> toFloat)
            )
            bird.pos


view : Model -> Html Msg
view model =
    div []
        [ Canvas.toHtml ( round width, round height )
            [ style "border" "1px solid #eee" ]
            ([ clear <| rgba 1 1 1 0.1
             , model.birds
                |> List.map vecRender
                |> shapes [ stroke Color.blue ]
             ]
                ++ (model.birds
                        |> List.map (birdRender birdRadius)
                   )
            )
        ]


clear : Color -> Renderable
clear color =
    shapes
        [ fill color ]
        [ rect ( 0, 0 ) width height ]


objRender : Obj -> Shape
objRender { radius, pos } =
    circle ( pos.x, pos.y ) radius


birdRender : Float -> Bird -> Renderable
birdRender r { pos, axel } =
    shapes [ fill <| hsl (length axel) 0.5 0.5 ] [ circle ( pos.x, pos.y ) r ]


vecRender : Bird -> Shape
vecRender { pos, vec } =
    let
        dir =
            (normal >> mul 10) vec
    in
    path ( pos.x, pos.y ) [ lineTo ( pos.x + dir.x, pos.y + dir.y ) ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Browser.onAnimationFrameDelta Tick
        }
