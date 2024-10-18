module Main exposing (..)


import Browser
import Html exposing (Html)
import Svg exposing (Svg, rect, svg, line, circle, text_, path)
import Svg.Attributes exposing (
  x, y, width, height, rx, fill, stroke, x1, x2, y1, y2, cx, cy,
  r, dominantBaseline, fontFamily, textAnchor, d
  )


-- I hate Elm

getById : Int -> List { a | id : Int } -> Maybe { a | id : Int }
getById n = List.filter (\x -> x.id == n) >> List.head


-- Main

main =
  Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = always Sub.none
  }


-- Model

type alias Model =
  { nodes : List Node
  , inputs : List Input
  , nextId : Int
  }


type alias Node =
  { id : Int
  , x : Int
  , y : Int
  , width : Int
  , height : Int
  , inputs : List NodeInput
  , outputs : List String
  , name : String
  }


type alias NodeInput =
  { name : String
  , source : Maybe Source
  }


type Source
  = FromOutput Int Int
  | FromInput Int


type alias Input =
  { id : Int
  , x : Int
  , y : Int
  , width : Int
  , name : String
  }


defaultModel =
  { nodes =
    [ { id = 0
      , x = 200
      , y = 100
      , width = 200
      , height = 60
      , inputs =
        [ { name = "JanuszgjłW"
          , source = Nothing
          }
        , { name = "PlgjyłQ"
          , source = Just ( FromInput 2 )
          }
        ]
      , outputs = [ "JanuszgjłW", "PlgjyłQ" ]
      , name = "some-node"
      }
    , { id = 1
      , x = 400
      , y = 300
      , width = 200
      , height = 60
      , inputs =
        [ { name = "JanuszgjłW"
          , source = Just ( FromOutput 0 1 )
          }
        , { name = "PlgjyłQ"
          , source = Nothing
          }
        ]
      , outputs = [ "JanuszgjłW", "PlgjyłQ" ]
      , name = "some-node"
      }
    ] 
  , inputs =
    [ { id = 2
      , x = 10
      , y = 200
      , width = 50
      , name = "some-input"
      }
    ]
  , nextId = 3
  }


init : () -> ( Model, Cmd a )
init _ = ( defaultModel, Cmd.none )


-- Update

type Msg = YepMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )


-- View

view : Model -> Html Msg
view model = svg
  [ height "1000", width "1000" ]
  ( ( List.map ( renderNodeConnections model ) model.nodes
    |> List.concat
    )
  ++ ( List.map renderNode model.nodes |> List.concat )
  ++ ( List.map renderInput model.inputs |> List.concat )
  )


renderNode : Node -> List ( Svg Msg )
renderNode node =
  [ rect
    [ node.x |> String.fromInt |> x
    , node.y |> String.fromInt |> y
    , node.height |> String.fromInt |> height
    , node.width |> String.fromInt |> width
    , rx "10"
    , fill "#FFF0DD"
    , stroke "#000000"
    ]
    []
  , line
    [ node.x |> String.fromInt |> x1
    , node.y + 20 |> String.fromInt |> y1
    , node.x + node.width |> String.fromInt |> x2
    , node.y + 20 |> String.fromInt |> y2
    , stroke "#000000"
    ]
    []
  , text_
    [ node.x + node.width // 2 |> String.fromInt |> x
    , node.y + 10 |> String.fromInt |> y
    , fontFamily "monospace"
    , dominantBaseline "middle"
    , textAnchor "middle"
    ]
    [ Svg.text node.name ]
  ]
  ++ ( List.indexedMap ( renderNodeInput node ) node.inputs |> List.concat )
  ++ ( List.indexedMap ( renderNodeOutput node ) node.outputs |> List.concat )


renderNodeInput : Node -> Int -> NodeInput -> List ( Svg Msg )
renderNodeInput node index input =
  [ circle
    [ node.x |> String.fromInt |> cx
    , node.y + 20 * index + 30 |> String.fromInt |> cy
    , r "4"
    , fill "#555555"
    , stroke "#000000"
    ]
    []
  , text_
    [ dominantBaseline "middle"
    , node.x + 10 |> String.fromInt |> x
    , node.y + 20 * index + 30 |> String.fromInt |> y
    , fontFamily "monospace"
    ]
    [ Svg.text input.name ]
  ]


renderNodeOutput : Node -> Int -> String -> List ( Svg Msg )
renderNodeOutput node index name =
  [ circle
    [ node.x + node.width |> String.fromInt |> cx
    , node.y + 20 * index + 30 |> String.fromInt |> cy
    , r "4"
    , fill "#555555"
    , stroke "#000000"
    ]
    []
  , text_
    [ dominantBaseline "middle"
    , node.x + node.width - 10 |> String.fromInt |> x
    , node.y + 20 * index + 30 |> String.fromInt |> y
    , fontFamily "monospace"
    , textAnchor "end"
    ]
    [ Svg.text name ]
  ]


renderNodeConnections : Model -> Node -> List ( Svg Msg )
renderNodeConnections model node =
  let
    maybeRenderConnection index input =
      case input.source of
        Nothing -> []
        Just ( FromOutput sourceId sourceIndex ) ->
          case getById sourceId model.nodes of
            Nothing -> []
            Just sourceNode ->
              bezier node.x ( node.y + 20 * index + 30 )
                ( sourceNode.x + sourceNode.width )
                ( sourceNode.y + 20 * sourceIndex + 30 )
        Just ( FromInput source ) -> [] -- TODO
  in List.indexedMap maybeRenderConnection node.inputs |> List.concat


bezier : Int -> Int -> Int -> Int -> List ( Svg Msg )
bezier x1 y1 x2 y2 =
  [ path
    [ "M" ++ String.fromInt x1
    ++ "," ++ String.fromInt y1
    ++ "C" ++ String.fromInt ( x1 - 100 )
    ++ "," ++ String.fromInt y1
    ++ "," ++ String.fromInt ( x2 + 100 )
    ++ "," ++ String.fromInt y2 
    ++ "," ++ String.fromInt x2 
    ++ "," ++ String.fromInt y2 |> d
    , stroke "#000000"
    , fill "none"
    ]
    []
  ]


renderInput : Input -> List ( Svg Msg )
renderInput input =

