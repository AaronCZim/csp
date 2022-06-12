-- To run this code,
--   1) copy it to the elm-lang.org/try text area; or
--   2) copy it to an initialized elm folder's src folder, and
--      uncomment the "module Main exposing(..)" line.
-- Start ./src/Main.elm
-- module Main exposing(..)


import Browser
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Browser.Dom
import Browser.Events
import Time
import Json.Decode
import Set
import Random


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
  , debugger : DebuggerModel
  }


defaultW = 256
defaultH = 128


init : () -> (Model, Cmd Msg)
init _ =
  ( Model defaultW defaultH debuggerInit
  , Cmd.batch
    [ Task.attempt
        getSizeFromViewport
        Browser.Dom.getViewport
    , Cmd.map MsgDebugger debuggerInitCmd
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
  | MsgDebugger DebuggerMsg



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetSizeFromViewport w h ->
      ( { model
          | w = w
          , h = h
        }
      , Cmd.none
      )
      
    MsgDebugger debuggerMsg ->
      let
        ( debugger, cmd ) =
          debuggerUpdate
            debuggerMsg
            model.debugger
      in
      ( { model | debugger = debugger }
      , Cmd.map MsgDebugger cmd
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
    , debuggerView
        0
        0
        model.w
        model.h
        model.debugger
    ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize
        setSizeFromViewport
    , Sub.map MsgDebugger
        ( debuggerSubscriptions
          model.debugger
        )
    ]

setSizeFromViewport w h =
  SetSizeFromViewport
    (toFloat (w - 50))
    (toFloat (h - 55))
    
    
-- End of ./src/Main.elm


-- Start of ./src/Debugger.elm
-- Module Debugger exposing(..)
-- Copy the "import" lines at the top of ./src/Main.elm


-- Debugger Model


type alias DebuggerModel =
  { msgs : List CspMsg
  , csp : CspModel
  }


debuggerInit =
  DebuggerModel [] cspInit


debuggerInitCmd =
  Cmd.map MsgCsp cspInitCmd


-- Debugger Update


type DebuggerMsg
  = Step
  | MsgCsp CspMsg


debuggerUpdate msg model =
  case msg of
    MsgCsp cspMsg ->
      ( { model
        | msgs =
          model.msgs
            ++ [cspMsg]
        }
      , Cmd.none
      )

    Step ->
      case List.head model.msgs of
        Nothing ->
          ( model, Cmd.none )
        Just cspMsg ->
          let
            ( csp, cmd ) =
              cspUpdate
                cspMsg
                model.csp
          in
          ( { model
            | csp = csp
            , msgs = List.drop 1 model.msgs
            }
          , Cmd.map MsgCsp cmd
          )


-- Debugger View


debuggerView left top w h model =
  cspView left top w h model.csp


-- Debugger Subscriptions


debuggerSubscriptions model =
  Sub.batch
    [ case List.head model.msgs of
        Nothing ->
          Sub.none

        Just msg ->
          Time.every
            (delayFor msg)
            (always Step)
    , Sub.map MsgCsp
      (cspSubscriptions model.csp)
    ]


-- Debugger Functions


delayFor msg =
  0


-- End of ./src/Debugger.elm


-- Start of ./src/Csp.elm
-- Module Csp exposing(..)
-- Copy the "import" lines at the top of ./src/Main.elm


-- Csp Model


type alias CspModel =
  { guesses : List Answer
  , guessesLen : Int
  , problem : Csp
  }


cspModelEmpty =
  CspModel
    []
    7
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
  }


cspInitCmd =
  cmdRandomAllDiffCsp 3
  {-- cmdPrune
    cspInit.problem.template
    cspInit.problem.constraints
  --}


-- Csp Update


type CspMsg
  = RandomGuess Answer
  | MsgPruneStep PruneStep
  | SetCsp Answer (List Constraint)


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
      (randConstraints template)
    |> Random.map
      ( filterConstraints template )
    |> Random.generate
      (SetCsp template)


randConstraints template answer =
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
      {-- ( model.problem.template
        |> List.map String.fromInt
        |> String.join ","
      )
      --}
        :: List.map
          toStringConstraint
            model.problem.constraints
    )
    ++ ( model.guesses
        |> List.map
          (\guess ->
            ( guess
              |> List.map String.fromInt
              |> String.join ","
            )
              ++ ": "
              ++ ( getScore
                    model.problem.template
                    model.problem.constraints
                    guess
                  |> String.fromFloat
                  |> String.left 5
                )
          )
      )
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
  Sub.none


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
      ( List.take i options )
        ++ ( [ value ]
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
        setMember set member =
          Set.member member set
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
                    List.head optionsAtI
                  else
                    Nothing
                )
        in
        singletons
          |> List.foldl
            (\singleton op ->
              op
                |> List.indexedMap
                  (\i optionsAtI ->
                    if
                      (Set.member i diffSet)
                        && List.drop 1 optionsAtI
                          /= []
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
          constraintsNow =
            constraint :: fc
          optionsNow =
            prune
              constraintsNow
              options
        in
        case constraint of
          AllDiff _ _ ->
            ( constraintsNow, options )
        
          _ ->
            if optionsNow == options then
              ( fc, options )
            else
              ( constraintsNow, options )
      )
      ( [], allOptions )
    |> Tuple.first


-- End of ./src/Csp.elm
