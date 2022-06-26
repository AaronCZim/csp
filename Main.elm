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
  , sudoku : SudokuModel
  }


defaultW = 256
defaultH = 128


init : () -> (Model, Cmd Msg)
init _ =
  ( Model defaultW defaultH sudokuInit
  , Cmd.batch
    [ Task.attempt
      getSizeFromViewport
      Browser.Dom.getViewport
    , Cmd.map MsgSudoku cmdSudokuInit
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
  | MsgSudoku SudokuMsg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MsgSudoku sudokuMsg ->
      let
        ( sudoku, cmd ) =
          sudokuUpdate
            sudokuMsg
            model.sudoku
      in
      ( { model | sudoku = sudoku }
      , Cmd.map MsgSudoku cmd
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
    , sudokuView 0 0 model.w model.h
      model.sudoku
        |> Svg.map MsgSudoku
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
      MsgSudoku
      (sudokuSubscriptions model.sudoku)
    ]

setSizeFromViewport w h =
  SetSizeFromViewport
    (toFloat (w - 50))
    (toFloat (h - 55))
    
    
-- End of ./src/Main.elm


-- Start of ./src/Sudoku.elm
-- module Sudoku exposing(..)
-- Copy "import" lines from the top of Main.elm


-- Model


type alias SudokuModel =
  { highlight : Highlight
  , viewingLogic : Bool
  , sudoku : Sudoku
  }


sudokuEmpty =
  SudokuModel NoHighlight False emptySudoku


type alias Sudoku =
  Array.Array Cell

emptySudoku =
  Array.repeat 81 EmptyCell


type Cell
  = EmptyCell
  | ConstCell Int
  | GuessCell Int


type Highlight
  = NoHighlight
  | ColHighlight Int
  | CellHighlight Int Int


sudokuInit : SudokuModel
sudokuInit =
  sudokuEmpty


-- Update


type SudokuMsg
  = KeyDown String
  | RandomCell ( ( Int, Int ), Int )


cmdSudokuInit =
  cmdRandomCell


cmdRandomCell =
  Random.pair
    ( Random.pair
      (Random.int 0 8)
      (Random.int 0 8)
    )
    (Random.int 0 8)
    |> Random.generate
      RandomCell


sudokuUpdate msg model =
  case msg of
    RandomCell ( ( cX, cY ), value ) ->
      let
        sudokuNext =
          model.sudoku
            |> sudokuSet cX cY (GuessCell (value))
        sudokuCspOptions =
          toCspModelSudoku sudokuNext
            |> .problem
            |> .options
      in
      if
        sudokuCspOptions
          |> List.any ((==) [])
      then
        ( model
        , cmdRandomCell
        )
      else
        ( { model | sudoku = sudokuNext }
        , if
            -- If all singletons,
            -- then done.
            sudokuCspOptions
              |> List.map (List.drop 1)
              |> List.all ((==) [])
          then
            Cmd.none
          else
            cmdRandomCell
        )
      

    KeyDown key ->
      case String.toInt key of
        Nothing ->
          if key == " " then
            ( { model
              | viewingLogic =
                not model.viewingLogic
              }
            , Cmd.none
            )
          else
            case model.highlight of
              CellHighlight cellX cellY ->
                if key == "." || key == "+" then
                  ( { model
                    | sudoku =
                      model.sudoku
                        |> sudokuXyMap
                          (\cX cY cell ->
                            if
                              (cX == cellX)
                              && (cY == cellY)
                            then
                              EmptyCell
                            else
                              cell
                          )
                    , highlight = NoHighlight
                    }
                  , Cmd.none
                  )
                else
                  ( model, Cmd.none )
                
              _ ->
                ( { model
                  | highlight = NoHighlight
                  }
                , Cmd.none
                )
          
        Just value ->
          if value == 0 then
            ( { model
              | highlight = NoHighlight
              }
            , Cmd.none
            )
            
          else if value <= 9 && value > 0 then
            case model.highlight of
              ColHighlight colX ->
                ( { model
                  | highlight =
                    CellHighlight colX (value - 1)
                  }
                , Cmd.none
                )
                
              NoHighlight ->
                ( { model
                  | highlight =
                    ColHighlight (value - 1)
                  }
                , Cmd.none
                )
                
              CellHighlight cellX cellY ->
                let
                  sudokuNext =
                      model.sudoku
                        |> sudokuSet cellX cellY
                          (GuessCell (value - 1))
                in
                if
                  -- If the full board
                  -- with no contradictions,
                  -- then it is done.
                  ( sudokuNext
                    |> Array.toList
                    |> List.all ((/=) EmptyCell)
                  )
                    && ( sudokuNext
                        |> toCspModelSudoku
                        |> .problem
                        |> .options
                        |> List.all ((/=) [])
                      )
                then
                  ( { model | highlight = NoHighlight
                    , sudoku = emptySudoku
                    }
                  , cmdRandomCell
                  )
                else
                  ( { model | highlight = NoHighlight
                    , sudoku = sudokuNext
                    }
                  , Cmd.none
                  )
          else
            ( model, Cmd.none )



-- View


black = "rgb(50,50,50)"


sudokuView left top w h model =
  if model.viewingLogic then
    model.sudoku
      |> toCspModelSudoku
      |> cspView left top w h
  else
    sudokuGameView left top w h model


sudokuGameView left top w h model =
  let
    cellW = w / 9
    cellH = h / 9
    cellWMargin = cellW / 8
    cellHMargin = cellH / 8
    cellWInner = cellW * 3 / 4
    cellHInner = cellH * 3 / 4
  in
  [ case model.highlight of
    ColHighlight colX ->
      viewRectFill
        "rgba(255,255,255,0.5)"
        ( left
          + (toFloat colX * cellW)
        )
        top
        cellW
        (cellH * 9)
      
    CellHighlight cellX cellY ->
      viewRectFill
        "rgba(255,255,255,0.5)"
        ( left
          + (toFloat cellX * cellW)
        )
        ( top
          + (toFloat cellY * cellH)
        )
        cellW
        cellH
      
    NoHighlight ->
      g [] []
  , List.range 0 9
    -- [ 0, 1, 2, 3, 4 ]
    |> List.map
      (\i ->
        let
          l =
            (toFloat i * cellW)
              + left
        in
        viewLine
          black
          ( if modBy 3 i == 0 then
            4
            else
            1
          )
          l
          top
          l
          (top + h)
      )
    |> g []
  , List.range 0 9
    -- [ 0, 1, 2, 3, 4 ]
    |> List.map
      (\i ->
        let
          t =
            (toFloat i * cellH)
              + top
        in
        viewLine
          black
          ( if modBy 3 i == 0 then
            4
            else
            1
          )
          left
          t
          (left + w)
          t
      )
    |> g []
  , viewCircleFill black
    0
    0
    2
  , viewCircleFill black
    w
    0
    2
  , viewCircleFill black
    w
    h
    2
  , viewCircleFill black
    0
    h
    2
  , model.sudoku
    |> sudokuXyMap
      (\cXInt cYInt cell ->
        let
          cX = toFloat cXInt
          cY = toFloat cYInt
          l =
            ((cX * cellW)
              + left
              + cellWMargin
            )
          t =
            ( (cY * cellH)
              + top
              + cellHMargin
            )
        in
        case cell of
          EmptyCell ->
            g [] []

          ConstCell value ->
            viewFillText
              black
              l
              t
              cellWInner
              cellHInner
              ( String.fromInt
                (value + 1)
              )

          GuessCell value ->
            viewFillText
              "grey"
              l
              t
              cellWInner
              cellHInner
              ( String.fromInt
                (value + 1)
              )
      )
    |> Array.toList
    |> g []
  ]
    |> g []


-- Subscriptions


sudokuSubscriptions model =
  Browser.Events.onKeyDown
    (Json.Decode.field "key"
      Json.Decode.string
      |> Json.Decode.map
        KeyDown
    )


-- Functions


sudokuSet dX dY value sudoku =
  Array.set (dX + (dY * 9)) value sudoku


sudokuXyMap map sudoku =
  sudoku
    |> Array.indexedMap
      (\i cell ->
        map (modBy 9 i) (i // 9) cell
      )


toCspModelSudoku sudoku =
  Csp sudokuTemplate
    ( templateToOptions sudokuTemplate
      |> prune
        ( toConstraintsSudoku sudoku
          ++ sudokuRuleConstraints
        )
    )
    ( toConstraintsSudoku sudoku
      ++ sudokuRuleConstraints
    )
    |> CspModel [] 7 []

    
    
toConstraintsSudoku sudoku =
  sudoku
    |> Array.indexedMap
      (\tileNumber cell ->
        case cell of
          ConstCell value ->
            Just (Fix tileNumber value)

          GuessCell value ->
            Just (Fix tileNumber value)

          _ ->
            Nothing
      )
    |> Array.toList
    |> List.filterMap identity


sudokuTemplate =
  List.repeat 81 8


sudokuRuleConstraints =
  -- Row Constraints
  ( List.range 0 8
    |> List.map (\i ->
        List.range (i * 9) ((i * 9) + 8)
      )
    |> List.map Set.fromList
    |> List.map (AllDiff 0)
  )
  ++
  -- Column Constraints
  ( List.range 0 8
    |> List.map (\i ->
        List.range 0 8
          |> List.map ((*) 9)
          |> List.map ((+) i)
      )
    |> List.map Set.fromList
    |> List.map (AllDiff 0)
  )
  ++
  [ AllDiff 0 (Set.fromList [0,1,2,9,10,11,18,19,20])
  , AllDiff 0 (Set.fromList [27,28,29,36,37,38,45,46,47])
  , AllDiff 0 (Set.fromList [54,55,56,63,64,65,72,73,74])
  , AllDiff 0 (Set.fromList [3,4,5,12,13,14,21,22,23])
  , AllDiff 0 (Set.fromList [30,31,32,39,40,41,48,49,50])
  , AllDiff 0 (Set.fromList [57,58,59,66,67,68,75,76,77])
  , AllDiff 0 (Set.fromList [6,7,8,15,16,17,24,25,26])
  , AllDiff 0 (Set.fromList [33,34,35,42,43,44,51,52,53])
  , AllDiff 0 (Set.fromList [60,61,62,69,70,71,78,79,80])
  ]


-- End of ./src/Sudoku.elm


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
