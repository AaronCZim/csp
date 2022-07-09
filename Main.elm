-- Start of ./src/Main.elm
-- module Main exposing(..)


import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Browser.Dom
import Browser.Events
import Dict
import Set
import Json.Decode
import Random
import Array


-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { w : Float
  , h : Float
  , logicGrid : LogicGridModel
  }


defaultW = 256
defaultH = 128


init : () -> (Model, Cmd Msg)
init _ =
  ( Model defaultW defaultH logicGridInit
  , Cmd.batch
    [ Task.attempt
      getSizeFromViewport
      Browser.Dom.getViewport
    , Cmd.map MsgLogicGrid cmdLogicGridInit
    ]
  )


getSizeFromViewport resultViewport =
  case resultViewport of
    Err err ->
      SetSizeFromViewport
        defaultW
        defaultH
        
    Ok viewport ->
      SetSizeFromViewport
        (viewport.viewport.width - 50)
        (viewport.viewport.height - 55)


-- UPDATE


type Msg
  = SetSizeFromViewport Float Float
  | MsgLogicGrid LogicGridMsg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MsgLogicGrid logicGridMsg ->
      let
        ( logicGrid, cmd ) =
          logicGridUpdate
            logicGridMsg
            model.logicGrid
      in
      ( { model | logicGrid = logicGrid }
      , Cmd.map MsgLogicGrid cmd
      )
    
    SetSizeFromViewport w h ->
      ( { model
          | w = w
          , h = h
        }
      , Cmd.none
      )


-- VIEW


view : Model -> Html Msg
view model =
  svg
    [ -- viewBox parameters are:
      --   minXValue
      --   minYValue
      --   width
      --   height
      viewBox ( "-25 -25 "
        ++ String.fromFloat (model.w + 50)
        ++ " "
        ++ String.fromFloat (model.h + 50)
        )
    , width (String.fromFloat (model.w + 50))
    , height (String.fromFloat (model.h + 50))
    ]
    [ rect
      [ x "-15"
      , y "-15"
      , width (String.fromFloat (model.w + 30))
      , height (String.fromFloat (model.h + 30))
      , rx "15"
      , ry "15"
      , fill "rgb(50,50,50)"
      ]
      []
    , rect
      [ x "-5"
      , y "-5"
      , width (String.fromFloat (model.w + 10))
      , height (String.fromFloat (model.h + 10))
      , rx "5"
      , ry "5"
      , fill "LightBlue"
      ]
      []
    , logicGridView 0 0 model.w model.h
      model.logicGrid
        |> Svg.map MsgLogicGrid
    ]


viewLine strokeC strokeW x1Float y1Float x2Float y2Float =
  line
    [ x1 <| String.fromFloat x1Float
    , y1 <| String.fromFloat y1Float
    , x2 <| String.fromFloat x2Float
    , y2 <| String.fromFloat y2Float
    , stroke strokeC
    , strokeWidth <| String.fromFloat strokeW
    ]
    []


viewRectFill fillC = viewRect fillC "none" 0
viewRectLine strokeC strokeW =
  viewRect "none" strokeC strokeW
viewRect fillC strokeC strokeW xFloat yFloat widthFloat heightFloat =
  rect
    [ x <| String.fromFloat xFloat
    , y <| String.fromFloat yFloat
    , width <| String.fromFloat widthFloat
    , height <| String.fromFloat heightFloat
    , fill fillC
    , stroke strokeC
    , strokeWidth <| String.fromFloat strokeW
    ]
    []


viewCircleFill fillC = viewCircle fillC "none" 0
viewCircleLine strokeC strokeW =
  viewCircle "none" strokeC strokeW
viewCircle fillC strokeC strokeW cxFloat cyFloat rFloat =
  circle
    [ cx <| String.fromFloat cxFloat
    , cy <| String.fromFloat cyFloat
    , r <| String.fromFloat rFloat
    , fill fillC
    , stroke strokeC
    , strokeWidth <| String.fromFloat strokeW
    ]
    []


viewText fontC fontSizeFloat xFloat yFloat str =
  text_
    [ x <| String.fromFloat xFloat
    , y <| String.fromFloat yFloat
    , fontSize
      <| String.fromFloat
        fontSizeFloat
    , fontFamily "tahoma"
    , fill fontC
    ]
    [ text str ]


lettersWithTails =
  "gjpyq;,/\\$()[]{}"
    |> String.toList
    |> Set.fromList


tallLetters =
  ( "ABCDEFTHIJKLMNOPQRSTUVWXYZ"
    ++ "df/lkjibht<>?/'\"{}[])(*&&^%$#@!~`"
    ++ "1234567890"
  )
    |> String.toList
    |> Set.fromList


extraTallLetters =
  ( "`{}[]()"
  )
    |> String.toList
    |> Set.fromList


fontWs =
  -- These measurements are for viewFillText
  [("!",0.3281767955801105),("\"",0.400390625),("#",0.7243902439024391),("$",0.5449541284403671),("%",0.981818181818182),("&",0.675),("'",0.21119505494505497),("(",0.3832258064516129),(")",0.38076923076923075),("*",0.5449541284403671),("+",0.7243902439024391),(",",0.300759493670886),("-",0.3632),(".",0.29849246231155774),("/",0.38151364764268),("0",0.5449541284403671),("1",0.5449541284403671),("2",0.5449541284403671),("3",0.5449541284403671),("4",0.5449541284403671),("5",0.5449541284403671),("6",0.5449541284403671),("7",0.5449541284403671),("8",0.5449541284403671),("9",0.5449541284403671),(":",0.3494318181818181),(";",0.3504424778761062),("<",0.7252358490566039),("=",0.724468085106383),(">",0.7252358490566039),("?",0.4730769230769231),("@",0.9068702290076335),("A",0.6026548672566371),("B",0.591044776119403),("C",0.6030456852791881),("D",0.6788571428571428),("E",0.5603773584905661),("F",0.5233480176211455),("G",0.6674157303370786),("H",0.675),("I",0.37124999999999997),("J",0.4139372822299651),("K",0.591044776119403),("L",0.4991596638655461),("M",0.7714285714285716),("N",0.6674157303370786),("O",0.7071428571428572),("P",0.55),("Q",0.7071428571428573),("R",0.6219895287958116),("S",0.5474654377880184),("T",0.5399999999999999),("U",0.656353591160221),("V",0.6),("W",0.9068702290076335),("X",0.5823529411764707),("Y",0.5766990291262137),("Z",0.5551401869158877),("[",0.38151364764268),("]",0.38056930693069296),("^",0.7243902439024391),("_",0.55),("`",0.5351351351351351),("a",0.5211864406779662),("b",0.5510752688172043),("c",0.458955223880597),("d",0.5510752688172043),("e",0.5247440273037541),("f",0.31441717791411045),("g",0.5510752688172043),("h",0.5570652173913043),("i",0.22643593519882177),("j",0.27903811252268607),("k",0.4991883116883115),("l",0.22811572700296737),("m",0.8401639344262294),("n",0.5550541516245486),("o",0.5413732394366196),("p",0.5510752688172043),("q",0.5491071428571429),("r",0.3609154929577465),("s",0.4456140350877192),("t",0.3278251599147121),("u",0.5550541516245486),("v",0.4975728155339806),("w",0.7427536231884058),("x",0.49596774193548393),("y",0.4991883116883115),("z",0.444364161849711),("{",0.47897196261682246),("}",0.48046874999999994),("~",0.7243902439024391)]

fontWAvg =
  ( fontWs
    |> List.map Tuple.second
    |> List.foldl (+) 0
  )
  / (fontWs |> List.length |> toFloat)
    
  
fontW =
  fontWs |> Dict.fromList


viewFillText fillC left top w h str =
  let
    ws =
      str
        |> String.toList
        |> List.map
          (\ch ->
            Dict.get
              (String.fromChar ch)
              fontW
              |> Maybe.withDefault
                fontWAvg
          )
        |> List.foldl (+) 0

    fontSizeByW =
      w / ws
    
    hasTails =
      str
        |> String.toList
        |> List.any
          (\ch ->
            Set.member ch lettersWithTails
          )
    
    hasTall =
      str
        |> String.toList
        |> List.any
          (\ch ->
            Set.member ch tallLetters
          )
    
    
    fontSizeByH =
      if hasTails then
        if hasTall then
          h * 1
        else
          h * 1.3
      else if hasTall then
        h * 1.27
      else
        h * 1.7
    
    fontS =
      Basics.min
        fontSizeByW
        fontSizeByH
    
    yOffset =
      if hasTails then
        if hasTall then
        fontS * 0.74
        else
        fontS * 0.53
      else if hasTall then
        fontS * 0.77
      else
        fontS * 0.57
    
    textH =
      if hasTails then
        if hasTall then
        fontS * 0.95
        else
        fontS * 0.7
      else if hasTall then
        fontS * 0.77
      else
        fontS * 0.57
        
    textW = fontS * ws
  in
  viewText fillC
    fontS
    (left + (w / 2) - (textW / 2))
    (top + ((h-textH)/2) + yOffset)
    str


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize
      setSizeFromViewport
    , Sub.map
      MsgLogicGrid
      (logicGridSubscriptions model.logicGrid)
    ]

setSizeFromViewport w h =
  SetSizeFromViewport
    (toFloat (w - 50))
    (toFloat (h - 55))
    
    
-- End of ./src/Main.elm


-- Start of ./src/LogicGrid.elm
-- module LogicGrid exposing(..)
-- Copy "import" lines from the top of Main.elm


-- Model


type alias LogicGridModel =
  { highlight : Highlight
  , gridDims : List Int
  , grid : Array.Array (Maybe Bool)
  , xLabels : String
  , yLabels : String
  , constraints : List Constraint
  }

logicGridInit =
  [ AllDiff 0 (Set.fromList [0,1,2])
  , AllDiff 0 (Set.fromList [3,4,5])
  , Fix 0 0
  , Fix 1 1
  , Fix 2 2
  , Fix 3 0
  , Fix 4 1
  , Eq 5 4 1
  ]
    |> LogicGridModel
      NoHighlight
      [ 3, 3 ]
      (Array.repeat 18 Nothing)
      "123123"
      "ABCXYZ"


logicGridEmpty =
  LogicGridModel
    NoHighlight
    [ 3, 3 ]
    (Array.repeat 18 Nothing)
    "123123"
    "ABCXYZ"
    []


type Highlight
  = NoHighlight
  | GridHighlight Int
  | ColHighlight Int Int
  | CellHighlight Int Int Int


-- Update


type LogicGridMsg
  = KeyDown String
  | SetConstraints (List Constraint)


cmdLogicGridInit = cmdLogicGridRandom


cmdLogicGridRandom =
  cmdRandomDoubleAllDiffCsp 3 3
    |> Cmd.map
      (\msg ->
        case msg of
          SetCsp answer constraints ->
            SetConstraints
              constraints

          _ ->
            KeyDown ""
      )


logicGridUpdate msg model =
  case msg of
    SetConstraints constraints ->
      ( { logicGridEmpty
        | constraints = constraints
        }
      , Cmd.none
      )

    KeyDown key ->
      case String.toInt key of
        Nothing ->
          if key == "." || key == "+" then
            case model.highlight of
              CellHighlight gridI colI rowI ->
                setCell
                  gridI
                  colI
                  rowI
                  Nothing
                  model

              _ ->
                ( model, Cmd.none )
          else
            ( model, Cmd.none )

        Just num ->
          if num == 0 then
            ( { model
              | highlight = NoHighlight
              }
            , Cmd.none
            )
            
          else
            case model.highlight of
              NoHighlight ->
                ( { model
                  | highlight =
                    GridHighlight (num - 1)
                  }
                , Cmd.none
                )
                
              GridHighlight gridI ->
                ( { model
                  | highlight =
                    ColHighlight gridI (num - 1)
                  }
                , Cmd.none
                )
                
              ColHighlight gridI colI ->
                ( { model
                  | highlight =
                    CellHighlight gridI colI (num - 1)
                  }
                , Cmd.none
                )
                
              CellHighlight gridI colI rowI ->
                setCell
                  gridI
                  colI
                  rowI
                  (Just (num == 1))
                  model


setCell gridI colI rowI toValue model =
  let
    gridNext =
      setGrid
        gridI
        colI
        rowI
        toValue
        model.grid
        model.gridDims
  in
    ( { model
      | highlight = NoHighlight
      , grid = gridNext
      }
    , if
        gridNext
          |> Array.toList
          |> List.all ((/=) Nothing)
      then
        let
          options =
            toOptionsGrid
              model.gridDims
              gridNext
        in
        if
          model.constraints
            |> List.all
              (isGuaranteed options)
        then
          cmdLogicGridRandom
        else
          Cmd.none
      else
        Cmd.none
    )
    
setGrid gridI colI rowI toValue grid gridDims =
  let
    dim =
      gridDims
        |> List.drop gridI
        |> List.head
        |> Maybe.withDefault 0
    startI =
      gridDims
        |> List.take gridI
        |> List.foldl
          (\d sum->
            (d*d) + sum
          )
          0
  in
    setGridCell
      startI
      dim
      dim
      colI
      rowI
      toValue
      grid


setGridCell startI cols rows colI rowI toValue grid =
  if
    Maybe.withDefault False toValue
      == True
  then
    grid
      |> Array.indexedMap
        (\i cell ->
          if
            (i < startI)
              || ( i >=
                  ( startI
                    + (rows * cols)
                  )
                )
          then
            cell
          else
            if
              modBy cols (i - startI)
                == colI
            then
              if
                (i - startI) // cols
                  == rowI
              then
                Just True
              else 
                Just False
            else if
              (i - startI) // cols
                == rowI
            then
              Just False
            else
              cell
        )
  else
    grid
      |> Array.set
        (startI + colI + (rowI * cols))
        toValue


-- View


black = "rgb(50,50,50)"


logicGridView left top w h model =
  let
    halfW = w / 2
    halfH = h / 2
    options =
      model.grid
          |> toOptionsGrid
            model.gridDims
    maxDim =
      model.gridDims
        |> List.foldl
          Basics.max
          0
    cellW =
      (halfW - 10) / toFloat (maxDim + 1)
    cellH =
      model.gridDims
        |> List.foldl (+) 0
        -- Label cells
        |> toFloat
        |> (+)
          ( (toFloat
            ( List.length
              model.gridDims
            )
            )
          )
        |> \cellHigh -> h / cellHigh
  in
  [ logicGridConstraintsView
    model.yLabels
    (left+halfW)
    top
    halfW
    h
    options
    model.constraints
  , logicGridGridsView
    model.xLabels
    model.yLabels
    left
    top
    cellW
    cellH
    model.gridDims
  , logicGridsContentsView
    (left + cellW)
    (top + cellH)
    cellW --(halfW - 10 - cellW)
    cellH --(halfH - 10 - cellH)
    model.gridDims
    model.grid
  , logicGridHighlightView
    left
    top
    cellW
    cellH
    model.gridDims
    model.highlight
  ]
    |> g []


logicGridConstraintsView labels left top w h options constraints =
  let
    rowH =
      h
        / toFloat
          ( List.length
            constraints
          )
  in
  constraints
    |> List.indexedMap
      (\i constraint ->
        let
          colour =
            if
              isGuaranteed
                options
                constraint
            then
              "Green"
            else if
              isPossible
                options
                constraint
            then
              black
            else
              "red"
        in
        viewFillText
          colour
          left
          (top + (rowH * toFloat i))
          w
          rowH
          ( toStringGridConstraint
            labels
            constraint
          )
      )
    |> g []


logicGridsContentsView left top cellW cellH gridDims grid =
  gridDims
    |> List.foldl
      (\dim (startI, t, svgs) ->
        ( startI + (dim * dim)
        , t + (toFloat (dim + 1) * cellH)
        , logicGridContentsView
          left
          t
          cellW
          cellH
          startI
          dim
          grid
          :: svgs
        )
      )
      (0, top, [])
    |> (\(_, _, svgs) -> svgs)
    |> g []


logicGridContentsView left top cellW cellH startI dim grid =
  let
    len = Array.length grid
    cellInnerW = cellW * 0.6
    cellInnerH = cellH * 0.6
    cellInnerX = cellW * 0.2
    cellInnerY = cellH * 0.2
  in
  grid
    |> Array.toList
    |> List.drop startI
    |> List.take (dim * dim)
    |> List.foldl
      (\maybeTruth ((prevX, prevY), svgs) ->
        let
          (thisX, thisY) =
            if prevX + 1 >= dim then
              (0, prevY + 1)
            else
              (prevX + 1, prevY)
        in
        ( (thisX, thisY)
        , case maybeTruth of
          Nothing ->
            svgs

          Just truth ->
            if truth then
              viewFillText
                "Green"
                (left
                  + (cellW * toFloat thisX)
                  + cellInnerX
                )
                (top
                  + (cellH * toFloat thisY)
                  + cellInnerY
                )
                cellInnerW
                cellInnerH
                "O"
                :: svgs

            else
              viewFillText
                black
                (left
                  + (cellW * toFloat thisX)
                  + cellInnerX
                )
                (top
                  + (cellH * toFloat thisY)
                  + cellInnerY
                )
                cellInnerW
                cellInnerH
                "X"
                :: svgs
        )
      )
      ((-1, 0), [])
    |> Tuple.second
    |> g []


logicGridHighlightView : Float -> Float -> Float -> Float -> List Int -> Highlight -> Svg.Svg LogicGridMsg
logicGridHighlightView left top cellW cellH gridDims highlight =
  let
    cellInnerX = cellW * 0.3
    cellInnerY = cellH * 0.3
    textW =
      cellW - cellInnerX - cellInnerX
    textH =
      cellH - cellInnerY - cellInnerY
    gridLeft = left + cellW
    getGridTop gridI =
      gridDims
        |> List.take gridI
        |> List.foldl (+) 0
        |> (+) gridI
        |> (+) 1
        |> toFloat
        |> (*) cellH
  in
  case highlight of
    NoHighlight ->
      gridDims
        |> List.indexedMap Tuple.pair
        |> List.foldl
          (\(i, dim) ( t, svgs ) ->
            ( t + (toFloat (dim + 1) * cellH)
            , viewCircle
                "white"
                black
                2
                (left + (cellW/2))
                (t + (cellH/2))
                ( Basics.min
                  textW
                  textH
                )
              :: viewFillText
                "gold"
                (left + cellInnerX)
                (t + cellInnerY)
                textW
                textH
                ( String.fromInt
                  ( i + 1 )
                )
              :: svgs
            )
          )
          ( top, [] )
        |> Tuple.second
        |> g []
    
    GridHighlight gridI ->
      let
        gridTop = getGridTop gridI
        dim =
          gridDims
            |> List.drop gridI
            |> List.head
            |> Maybe.withDefault 1
            |> toFloat
      in
      if gridI == 0 then
        viewRectLine
          "yellow"
          2
          gridLeft
          gridTop
          (cellW * dim)
          (cellH * dim)
      else
        viewRectLine
          "yellow"
          2
          gridLeft
          gridTop
          (cellW * dim)
          (cellH * dim)

    ColHighlight gridI colI ->
      let
        gridTop = getGridTop gridI
        dim =
          gridDims
            |> List.drop gridI
            |> List.head
            |> Maybe.withDefault 1
            |> toFloat
      in
      viewRectLine
        "yellow"
        2
        (gridLeft + (toFloat colI * cellW))
        gridTop
        cellW
        (cellH * dim)

    CellHighlight gridI colI rowI ->
      let
        gridTop = getGridTop gridI
      in
      viewRectLine
        "yellow"
        2
        (gridLeft + (toFloat colI * cellW))
        (gridTop + (toFloat rowI * cellH))
        cellW
        cellH


logicGridGridsView : String -> String -> Float -> Float -> Float -> Float -> List Int -> Svg.Svg LogicGridMsg
logicGridGridsView xLabels yLabels left top cellW cellH gridDims =
  gridDims
    |> List.foldl
      (\dim ((t, xl, yl), svgs) ->
        ( ( t + (toFloat (dim + 1) * cellH)
          , String.dropLeft dim xl
          , String.dropLeft dim yl
          )
        , logicGridGridView
          left
          t
          cellW
          cellH
          dim
          dim
          ( xl
            |> String.left dim
            |> String.toList
            |> List.map String.fromChar
          )
          ( yl
            |> String.left dim
            |> String.toList
            |> List.map String.fromChar
          )
          :: svgs
        )
      )
      ((top, xLabels, yLabels), [])
    |> Tuple.second
    |> g []


logicGridGridView left top cellW cellH cols rows xLabels yLabels =
  let
    cellInnerW = cellW * 0.7
    cellInnerX = cellW * 0.15
    cellInnerH = cellH * 0.7
    cellInnerY = cellH * 0.15
  in
  [ logicGridGridLinesView
    (left + cellW)
    (top + cellH)
    cellW
    cellH
    cols
    rows
  , logicGridYLabelView
    left
    top
    cellInnerW
    cellInnerH
    cellInnerX
    cellInnerY
    cellH
    yLabels
  , logicGridXLabelView
    left
    top
    cellInnerW
    cellInnerH
    cellInnerX
    cellInnerY
    cellW
    xLabels
  ]
    |> g []


logicGridYLabelView left top cellInnerW cellInnerH cellInnerX cellInnerY cellH yLabels =
  yLabels
    |> List.indexedMap
      (\i labelStr ->
        viewFillText
          black
          (left + cellInnerX)
          (top
            + (toFloat (i + 1) * cellH)
            + cellInnerY
          )
          cellInnerW
          cellInnerH
          labelStr
      )
    |> g []


logicGridXLabelView left top cellInnerW cellInnerH cellInnerX cellInnerY cellW xLabels =
  xLabels
    |> List.indexedMap
      (\i labelStr ->
        viewFillText
          black
          (left
            + (toFloat (i + 1) * cellW)
            + cellInnerX
          )
          (top + cellInnerY)
          cellInnerW
          cellInnerH
          labelStr
      )
    |> g []


logicGridGridLinesView left top cellW cellH cols rows =
  let
    w = cellW * toFloat cols
    h = cellH * toFloat rows
  in
  [ List.range 0 cols
    |> List.map toFloat
    |> List.map
      (\i ->
        viewLine black
          2
          (left + (i * cellW))
          top
          (left + (i * cellW))
          (top + h)
      )
    |> g []
  , List.range 0 cols
    |> List.map toFloat
    |> List.map
      (\i ->
        viewLine black
          2
          left
          (top + (i * cellH))
          (left + w)
          (top + (i * cellH))
      )
    |> g []
  , viewCircleFill black
    left
    top
    0.9
  , viewCircleFill black
    (left + w)
    top
    0.9
  , viewCircleFill black
    left
    (top + h)
    0.9
  , viewCircleFill black
    (left + w)
    (top + h)
    0.9
  ]
    |> g []


-- Subscriptions


logicGridSubscriptions model =
  Browser.Events.onKeyDown
    (Json.Decode.field "key"
      Json.Decode.string
      |> Json.Decode.map
        KeyDown
    )


-- Functions


toStringGridConstraint labels constraint =
  case constraint of
    AllDiff exceptions indexSet ->
      "Diff{"
        ++ ( indexSet
            |> Set.toList
            |> List.map
              (gridIndex labels)
            |> String.join ","
          )
        ++ "}"
        ++ ( if exceptions == 0 then
              ""
            else
              String.fromInt
                exceptions
          )

    Fix atI value ->
      gridIndex labels atI
        ++ " = "
        ++ String.fromInt (value + 1)

    Eq atI atJ offset ->
      gridIndex labels atI
        ++ " = "
          ++ ( if offset /= 0 then
              "( "
                ++ gridIndex labels atJ
                ++ " + "
                ++ String.fromInt offset
                ++ " )"
            else
              gridIndex labels atJ 
          )


gridIndex labels atI =
  labels
    |> String.toList
    |> List.map String.fromChar
    |> List.drop atI
    |> List.head
    |> Maybe.withDefault ""

toOptionsGrid gridDims grid =
  let
    firstCols =
      gridDims
        |> List.head
        |> Maybe.withDefault 0
  in
  grid
    |> Array.map (Maybe.withDefault True)
    |> Array.indexedMap Tuple.pair
    |> Array.toList
    |> List.foldr
      (\(i, isOpPossible) (opData, gridData)->
        let
          (opsAtI, options) = opData
          (dimsLeft, startI, cols) = gridData
          opsAtINext =
            if isOpPossible then
              (modBy cols (i - startI)) :: opsAtI
            else
              opsAtI
        in
        if modBy cols (i - startI) == 0 then
          ( ( []
            , opsAtINext :: options
            )
          , if
              (i - startI) // cols
                == (cols - 1)
            then
              ( List.drop 1 dimsLeft
              , startI + (cols * cols)
              , dimsLeft
                |> List.head
                |> Maybe.withDefault 0
              )
            else
              ( dimsLeft, startI, cols )
          )
        else
          ( (opsAtINext, options)
          , gridData
          )
      )
      (([], [])
      , ( List.drop 1 gridDims
        , 0
        , firstCols
        )
      )
    |> Tuple.first
    --|> \(opsAtI, options) -> opsAtI :: options
    |> Tuple.second


-- End of ./src/LogicGrid.elm


-- Start of ./src/Csp.elm
-- Module Csp exposing(..)
-- Copy the "import" lines at the top of ./src/Main.elm


-- Csp Model


type alias CspModel =
  { guesses : List Answer
  , guessesLen : Int
  , userInput : List Int
  , problem : Csp
  }


cspModelEmpty =
  CspModel
    []
    7
    []
    cspEmpty


type alias Csp =
  { template : Answer
  , options : Options
  , constraints : List Constraint
  }


type alias Answer = List Int


type alias Options = List (List Int)


cspEmpty =
  Csp [] [] []


type Constraint
  = Fix Int Int
  | Eq Int Int Int
  | AllDiff Int (Set.Set Int)


eq i j = Eq i j 0


cspInit =
  setCsp
    [2, 2, 2]
    [ Fix 0 0
    , Eq 1 2 -1
    , AllDiff 0 (Set.fromList [0, 1, 2])
    ]
    cspModelEmpty


setCsp template constraints solver =
  { solver
    | problem =
      Csp
        template
        ( templateToOptions template )
        constraints
    , userInput = []
  }


cmdRandomProblem =
  cmdRandomAllDiffCsp 4


cspInitCmd =
  cmdRandomProblem
  {-- cmdPrune
    cspInit.problem.template
    cspInit.problem.constraints
  --}


-- Csp Update


type CspMsg
  = RandomGuess Answer
  | MsgPruneStep PruneStep
  | SetCsp Answer (List Constraint)
  | InputValue Int
  | ClearInput


cmdRandomDoubleAllDiffCsp len1 len2 =
  let
    template =
      List.repeat len1 (len1 - 1)
       ++ List.repeat len2 (len2 - 1)
    answer1Value =
      List.range 0 (len1 - 1)
    answer2Value =
      List.range 0 (len2 - 1)
  in
  randShuffle
    answer1Value
    []
    |> Random.andThen
      (\answer1 ->
        randShuffle
          answer2Value
          []
          |> Random.andThen
            (\answer2 ->
              randConstraintsFrom
                template
                ( answer1 ++ answer2 )
                [ List.range 0 (len1 - 1)
                  -- [0, 1, 2, ...]
                  |> Set.fromList
                  |> AllDiff 0
                , List.range len1
                  ( len1 + len2 - 1 )
                  -- [0, 1, 2, ...]
                  |> Set.fromList
                  |> AllDiff 0
                ]
            )
      )
      |> Random.map
        ( filterConstraints template )
      |> Random.generate
        (SetCsp template)


cmdRandomAllDiffCsp len =
  let
    template =
      List.repeat len (len - 1)
    answerValue =
      List.range 0 (len - 1)
  in
  randShuffle
    answerValue
    []
    |> Random.andThen
      (\answer ->
        randConstraintsFrom
          template
          answer
          [ template
            |> List.length
            |> (\l -> l - 1 )
            |> List.range 0
            -- [0, 1, 2, ...]
            |> Set.fromList
            |> AllDiff 0
          ]
      )
    |> Random.map
      ( filterConstraints template )
    |> Random.generate
      (SetCsp template)


randConstraintsFrom template answer constraints =
  let
    options =
      templateToOptions template
        |> prune constraints
    nonSingleton =
      options
        |> List.indexedMap Tuple.pair
        |> List.foldl
          (\(i, optionsAtI) nons->
            if
              List.drop 1 optionsAtI == []
            then
              nons
            else
              i :: nons
          )
          []
  in
  case List.head nonSingleton of
    Nothing ->
      Random.constant constraints

    Just firstI ->
      Random.pair
        ( nonSingleton
          |> List.drop 1
          |> Random.uniform firstI
        )
        (Random.int 0 1)
        |> Random.andThen
          (\(randNonSingletonI, constraintType ) ->
            if constraintType == 0 then
              randFixConstraint
                answer
                randNonSingletonI
            else
              randEqConstraint
                answer
                randNonSingletonI
          )
        |> Random.map
          (\constraint ->
            constraint
              :: constraints
          )
        |> Random.andThen
          ( randConstraintsFrom template answer )


randFixConstraint answer randNonSingletonI =
  answer
    |> List.drop
      randNonSingletonI
    |> List.head
    |> Maybe.withDefault 0
    |> Fix randNonSingletonI
    |> Random.constant


randEqConstraint answer randNonSingletonI =
  let
    answerLen = List.length answer
  in
  Random.pair
    ( Random.int 0 (answerLen - 2) )
    ( Random.int 0 1 )
    |> Random.map
      (\(j, shouldFlip) ->
        if j >= randNonSingletonI then
          ( j + 1, shouldFlip )
        else
          ( j, shouldFlip )
      )
    |> Random.map
      (\(j, shouldFlip) ->
        if shouldFlip == 1 then
          (j, randNonSingletonI)
        else
          (randNonSingletonI, j)
      )
    |> Random.map
      (\(i, j) ->
        let
          answerAtI =
            answer
              |> List.drop i
              |> List.head
              |> Maybe.withDefault 0
          answerAtJ =
            answer
              |> List.drop j
              |> List.head
              |> Maybe.withDefault 0
        in
        Eq i j (answerAtI - answerAtJ)
      )
  

randShuffle remaining partial =
  case List.head remaining of
    Nothing ->
      Random.constant partial

    Just first ->
      Random.uniform first
        (List.drop 1 remaining)
        |> Random.andThen
          (\answerNext ->
            let
              remainingNext =
                remaining
                  |> List.filter
                    ((/=) answerNext)
            in
            randShuffle
              remainingNext
              (answerNext :: partial)
          )

cmdPrune template constraints =
  let
    options =
      templateToOptions
        template
  in
  case List.head constraints of
    Nothing ->
      Cmd.none

    Just firstConstraint ->
      ConstraintStep
        options
        []
        firstConstraint
        ( List.drop 1 constraints )
        options
        |> PruneStep
        |> MsgPruneStep
        |> Task.succeed
        |> Task.perform identity


cmdPruneFrom pruneStep =
  case pruneStep of
    PrunningDone _ ->
      Cmd.none

    PruneStep _ ->
      MsgPruneStep pruneStep
        |> Task.succeed
        |> Task.perform identity


cmdRandomGuess options partialGuess =
  randGuess options []
    |> Random.generate
      RandomGuess


randGuess : Options -> Answer -> Random.Generator (List Int)
randGuess options partialGuess =
  let
    lenMinus1 =
      List.length options - 1
    lastOptionsList =
      options
        |> List.drop lenMinus1
        |> List.head
        |> Maybe.withDefault []
  in
  case List.head lastOptionsList of
    Nothing ->
      Random.constant partialGuess

    Just firstOfTheLastOptionsList ->
      let
        optionsRemaining =
          List.take lenMinus1 options
      in
      Random.uniform
        firstOfTheLastOptionsList
        ( List.drop 1 lastOptionsList )
        |> Random.map
          (\answerAtI ->
            answerAtI
              :: partialGuess
          )
        |> Random.andThen
          ( randGuess
              optionsRemaining
          )


cspUpdate msg model =
  case msg of
    InputValue inputValue ->
      let
        userInput =
          model.userInput
            ++ [ inputValue ]
      in
      if
        getScore
          model.problem.template
          model.problem.constraints
          userInput
          == 1.0
      then
        ( model
        , cmdRandomProblem
        )
      else
        ( { model
          | userInput = userInput
          }
        , Cmd.none
        )
    
    ClearInput ->
      ( { model | userInput = [] }
      , Cmd.none
      )
    
    SetCsp template constraint ->
      ( setCsp template constraint model
      , cmdPrune template constraint
      )

    MsgPruneStep pruneStep ->
      let
        problem = model.problem
        pruneStepNext =
          pruneUpdate pruneStep
      in
      case pruneStepNext of
        PrunningDone options ->
          ( { model
            | problem =
              { problem
                | options = options
              }
            }
          , cmdRandomGuess options []
          )
        PruneStep step ->
          ( { model
            | problem =
              { problem
                | options =
                  step.options
              }
            }
          , cmdPruneFrom
            pruneStepNext
          )

    RandomGuess answer ->
      ( { model
        | guesses =
          insertScore
            model
            answer
            model.guesses
        }
      , if
          getScore
            model.problem.template
            model.problem.constraints
            answer
            == 1.0
        then
          Cmd.none
        else
          cmdRandomGuess
            model.problem.options
            []
      )


insertScore model guess guesses =
  (guess :: guesses)
    |> Set.fromList
    |> Set.toList
    |> List.sortBy (\guessI ->
        getScore
          model.problem.template
          model.problem.constraints
          guessI
          |> (*) -1
      )
    |> List.take model.guessesLen


-- Csp View


cspView left top w h model =
  ( ( ( model.problem.options
        |> Debug.toString
      )
      :: List.map
        toStringConstraint
          model.problem.constraints
    )
    ++ [ ( ( model.userInput
            |> List.map String.fromInt
          )
            ++ List.repeat
              ( List.length model.problem.template )
              "_"
        )
          |> List.take ( List.length model.problem.template )
          |> String.join " "
      ]
  )
    |> List.indexedMap (\i str ->
        text_
          [ x "10"
          , y
            <| String.fromInt
              ( ( i + 1 ) * 30 )
          , fontSize "30"
          , fontFamily "Tahoma"
          , fontWeight "bold"
          , fill "rgb(50,50,50)"
          ]
          [ str |> text ]
      )
    |> g []


-- Csp Subscriptions


cspSubscriptions model =
  Browser.Events.onKeyDown
    ( Json.Decode.field
      "key"
      Json.Decode.string
      |> Json.Decode.map
        (\str ->
          case String.toInt str of
            Nothing ->
              ClearInput
            
            Just inputValue ->
              InputValue inputValue
        )
    )


-- Csp Functions


toStringConstraint constraint =
  case constraint of
    AllDiff exceptions indexSet ->
      "AllDiff {"
        ++ ( indexSet
            |> Set.toList
            |> List.map String.fromInt
            |> String.join ","
          )
        ++ "} "
        ++ String.fromInt exceptions

    Fix atI value ->
      "at"
        ++ String.fromInt atI
        ++ " == "
        ++ String.fromInt value

    Eq atI atJ offset ->
      "at"
        ++ String.fromInt atI
        ++ " == "
          ++ ( if offset /= 0 then
              "( at"
                ++ String.fromInt atJ
                ++ " + "
                ++ String.fromInt offset
                ++ " )"
            else
              "at"
                ++ String.fromInt atJ 
          )


getScore template constraints guess =
  if
    List.length template
      /= List.length guess
  then
    0.0
  else if
    template
      |> List.indexedMap
        (\i templateValue ->
          let
            guessI =
              guess
                |> List.drop i
                |> List.head
                |> Maybe.withDefault 0
          in
          templateValue < guessI
        )
        -- list of isViolation:
        -- True if there is a violation
      |> List.any identity
    then
      0.0
    else
      constraints
        |> List.foldl
          (\constraint score ->
            if isConstrained guess constraint then
              score + 1
            else
              score
          )
          0
        |> (\score ->
            constraints
              |> List.length
              |> toFloat
              |> (/) (toFloat score)
          )
        

isConstrained guess constraint =
  let
    get i =
      guess
        |> List.drop i
        |> List.head
        |> Maybe.withDefault 0
  in
  case constraint of
    AllDiff exceptions diffSet ->
      diffSet
        |> Set.toList
        |> List.map get
        |> Set.fromList
        |> Set.size
        |> (==) ( Set.size diffSet )
    
    Fix i value ->
      get i == value
    
    Eq i j offset ->
      get i == (get j + offset)


isPossible options constraint =
  if
    options
      |> List.any
        (List.length >> (==) 0)
  then
    False
  else
    let
      at i =
        options
          |> List.drop i
          |> List.head
          |> Maybe.withDefault []
    in
    case constraint of
      AllDiff exceptions diffSet ->
        -- TODO: What is one index has all values.
        options
          |> List.indexedMap Tuple.pair
          |> List.filterMap
            (\(i, optionsAtI) ->
              if
                Set.member i diffSet
              then
                Set.fromList
                  optionsAtI
                  |> Just
              else
                Nothing
            )
          -- All values in the diffset
          -- turned from lists to set
          |> List.foldl
            Set.union
            Set.empty
          -- All unique values.
          |> Set.size
          -- Size is how many
          -- unique values there are
          -- in the diffset.
          |> (+) exceptions
          |> (\uniqueValues ->
            uniqueValues
              >= Set.size diffSet
            )

      Fix i value ->
        at i
          |> List.any ((==) value)

      Eq i j offset ->
        let
          opsAtISet =
            Set.fromList (at i)
        in
        at j
          |> List.map ((+) offset)
          |> List.any
            (\value ->
              Set.member value opsAtISet
            )


isGuaranteed options constraint =
  if
    options
      |> List.any
        (List.length >> (==) 0)
  then
    False
  else
    let
      at i =
        options
          |> List.drop i
          |> List.head
          |> Maybe.withDefault []
    in
    case constraint of
      AllDiff exceptions diffSet ->
        let
          diffSetOptions =
            options
              |> List.indexedMap Tuple.pair
              |> List.filterMap
                (\(i, optionsAtI) ->
                  if
                    Set.member i diffSet
                  then
                    Set.fromList
                      optionsAtI
                      |> Just
                  else
                    Nothing
                )
        in
        -- TODO: Remove duplicate comparisons.
        --   optionSetAtI == optionSetAtJ
        --   Never: optionSetAtJ == optionSetAtI
        diffSetOptions
          |> List.indexedMap Tuple.pair
          |> List.all
            (\(i, optionSetAtI) ->
              diffSetOptions
                |> List.indexedMap Tuple.pair
                |> List.all
                  (\(j, optionSetAtJ) ->
                    if i == j then
                      True
                    else
                      Set.intersect
                        optionSetAtI
                        optionSetAtJ
                        |> Set.size
                        |> (==) 0
                  )
            )

      Fix i value ->
        at i == [value]

      Eq i j offset ->
        let
          optionsAtI = at i
          optionsAtJ = at j
        in
        if
          (List.drop 1 optionsAtI == [])
            && (List.drop 1 optionsAtJ == [])
            && (optionsAtI /= [])
            && (optionsAtJ /= [])
        then
          (List.head optionsAtI)
            == ( optionsAtJ
              |> List.head
              |> Maybe.map ((+) offset)
              )
        else
          False
        


templateToOptions template =
  template
    |> List.map
      (List.range 0)


type PruneStep
  = PrunningDone Options
  | PruneStep ConstraintStep


type alias ConstraintStep =
  { optionsBefore : Options
  , constraintsPrev : List Constraint
  , constraint : Constraint
  , constraintsNext : List Constraint
  , options : Options
  }


pruneUpdate pruneStep =
  case pruneStep of
    PrunningDone _ ->
      pruneStep

    PruneStep step ->
      let
        optionsNext =
          pruneConstraint
            step.constraint
            step.options
      in
      case
        List.head
          step.constraintsNext
      of
      Nothing ->
        -- Either the start of the next loop
        -- Or the end of prunning.
        if
          step.optionsBefore
            == optionsNext
        then
          PrunningDone optionsNext
        else
          let
            allConstraints =
              step.constraintsPrev
                ++ [ step.constraint ]
            firstConstraint =
              allConstraints
                |> List.head
                |> Maybe.withDefault
                  step.constraint
          in
          ConstraintStep
            optionsNext
            []
            firstConstraint
            ( List.drop 1 allConstraints )
            optionsNext
            |> PruneStep
          
      
      Just laterConstraint ->
        -- Either first or middel constraint
        { step
        | options = optionsNext
        , constraintsPrev =
          step.constraintsPrev
            ++ ( case step.constraint of
              Fix _ _ ->
                []
              _ ->
                [ step.constraint ]
            )
        , constraint = laterConstraint
        , constraintsNext =
          step.constraintsNext
            |> List.drop 1
        }
          |> PruneStep


prune constraints options =
  let
    optionsNext =
      constraints
        |> List.foldl
          pruneConstraint
          options
  in
  if optionsNext == options then
    options
  else
    prune constraints optionsNext


pruneConstraint constraint options =
  case constraint of
    Fix i value ->
      let
        optionsAtI =
          options
            |> List.drop i
            |> List.head
            |> Maybe.withDefault []
      in
      ( List.take i options )
        ++ ( List.filter
            ((==) value)
            optionsAtI
            :: List.drop
              (i + 1)
               options
          )
    
    Eq i j offset ->
      let
        optionsAtI =
          options
            |> List.drop i
            |> List.head
            |> Maybe.withDefault []
        optionsAtJ =
          options
            |> List.drop j
            |> List.head
            |> Maybe.withDefault []
        acceptableIValues =
          optionsAtJ
            |> List.map
              ((+) offset)
            |> Set.fromList
        acceptableJValues =
          optionsAtI
            |> List.map
              (\o -> o - offset)
            |> Set.fromList
        setMember s member =
          Set.member member s
      in
      options
        |> List.indexedMap
          (\k optionsAtK ->
            if k == i then
              optionsAtK
                |> List.filter
                  (setMember acceptableIValues)
            else if k == j then
              optionsAtK
                |> List.filter
                  (setMember acceptableJValues)
            else
              optionsAtK
          )
    
    AllDiff exceptions diffSet ->
      if exceptions /= 0 then
        options
      else
        let
          singletons =
            options
              |> List.indexedMap Tuple.pair
              |> List.filterMap
                (\(i, optionsAtI) ->
                  if
                    (Set.member i diffSet)
                      && ( List.drop 1
                          optionsAtI
                          == []
                        )
                  then
                    optionsAtI
                      |> List.head
                      |> Maybe.map
                        (Tuple.pair i)
                  else
                    Nothing
                )
        in
        singletons
          |> List.foldl
            (\(singletonI, singleton) op ->
              op
                |> List.indexedMap
                  (\i optionsAtI ->
                    if
                      Set.member i diffSet
                        && i /= singletonI
                    then
                      optionsAtI
                        |> List.filter
                          ((/=) singleton)
                    else
                      optionsAtI
                  )
            )
            options


filterConstraints template constraints =
  let
    allOptions =
      templateToOptions template
  in
  constraints
    |> List.foldr
      (\constraint ( fc, options ) ->
        let
          constraintsNext =
            constraint :: fc
          optionsNext =
            prune
              constraintsNext
              options
        in
        case constraint of
          AllDiff _ _ ->
            ( constraintsNext, optionsNext )
        
          _ ->
            if optionsNext == options then
              ( fc, options )
            else
              ( constraintsNext, optionsNext )
      )
      ( [], allOptions )
    |> Tuple.first


-- End of ./src/Csp.elm
