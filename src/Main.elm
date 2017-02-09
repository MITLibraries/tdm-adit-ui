{-| Adit UI is a web application for users to explore TDM collections
and create docsets, backed by the Adit API server.
-}

import Debug exposing (log)
import Dict exposing (Dict)
import Set exposing (Set)
import Html exposing (button, div, input, text)
import Html.Attributes exposing (placeholder, type_, style)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (..)
import Json.Encode as Encoder

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

-- Model

type alias Model =
  { partitions : List Partition
  , projection : Maybe Lens
  , lenses : List Lens
  , filters : List Filter
  }

type alias Partition =
  { spec : Spec
  , count : Int
  , selected : Bool
  }

type alias Lens =
  { name : String
  , agg : String
  }

type alias Filter =
  { name : String
  , field : String
  , value : String
  }

type alias Clause =
  { action : Action
  , field : String
  , values : Set String
  }

type alias Spec = Dict String Clause

type Action = Include | Exclude | None

{-| Ultimately, these lenses could be derived from an API call to the search
schema, but for now we hard-code a few
-}

byDept : Lens
byDept =
  { name = "Department"
  , agg = ""
  }

byDegree : Lens
byDegree =
  { name = "Degree"
  , agg = ""
  }

byDate : Lens
byDate =
  { name = "Date Issued"
  , agg = ""
  }

byKeyword : Filter
byKeyword =
  { name = "Keyword"
  , field = "_all"
  , value = ""
  }

emptyClause : Clause
emptyClause =
  { action = None
  , field = ""
  , values = Set.empty
  }

mockClause1 : Clause
mockClause1 =
  { action = Include
  , field = "department"
  , values = Set.singleton "Mechanical Engineering"
  }

mockClause2 : Clause
mockClause2 =
  { action = Include
  , field = "department"
  , values = Set.singleton "Electrical Engineering and Computer Science"
  }

emptySpec : Spec
emptySpec =
  Dict.empty

emptyPart : Partition
emptyPart =
  { spec = Dict.empty
  , count = 0
  , selected = False
  }

emptyModel : Model
emptyModel =
  { partitions = [ emptyPart ]
  , projection = Nothing
  , lenses = [ byDept, byDegree, byDate ]
  , filters = [ byKeyword ]
  }

init : ( Model, Cmd Msg )
init = emptyModel ! [ fullPartition ]

-- Update

type Msg
  = NoOp
  | Project Lens
  | NewFilter String
  | ApplyFilter Filter
  | Select Partition
  | Add
  | Remove
  | MakeDocSet
  | Clear
  | LensedPartitions (Result Http.Error Int)
  | FilteredPartition (Result Http.Error Int)
  | NewDocset (Result Http.Error String)


toggleSelect : Partition -> Partition
toggleSelect partition =
  { partition | selected = not partition.selected }

selected : Partition -> Bool
selected partition =
  partition.selected

mapButton: Model -> Bool -> Msg
mapButton model add =
  case model.projection of
    Just a ->
      if add then Add else Remove

    Nothing ->
      if add then MakeDocSet else Clear

mapButtonText: Model -> Bool -> String
mapButtonText model add =
    case model.projection of
      Just a ->
        if add then "Add" else "Remove"

      Nothing ->
        if add then "DocSet" else "Clear"

pickMock : String -> Int -> Clause
pickMock name idx =
  if name == "Department" && (idx == 1) then mockClause1 else
    if name == "Department" && (idx == 2) then mockClause2 else
      if idx == 1 then mockClause1 else mockClause2


-- Mock service for partitioning: Zeno just cuts each partition in half
zenoPart : Maybe Partition -> Lens -> List Partition
zenoPart part lens =
  case part of
    Just p ->
      [  { p | count = p.count // 2
             , spec = mergeSpecs p.spec ( Dict.singleton ( lens.name ) (pickMock lens.name 1) )
          }
      ,  { p | count = p.count // 2
             , spec = mergeSpecs p.spec ( Dict.singleton  ( lens.name ) (pickMock lens.name 2) )
         }
      ]

    Nothing ->
      []

mergeBoth : String -> Clause -> Clause -> Spec -> Spec
mergeBoth key leftCl rightCl spec =
  Dict.insert key { leftCl | values = Set.union leftCl.values rightCl.values } spec

mergeSpecs : Spec -> Spec -> Spec
mergeSpecs left right =
  Dict.merge Dict.insert mergeBoth Dict.insert left right Dict.empty

mergeParts : Partition -> Partition -> Partition
mergeParts part1 part2 =
  { part1 |
      count = part1.count + part2.count,
      selected = False,
      spec = mergeSpecs part1.spec part2.spec
  }

mergePartList : List Partition -> Action -> Partition
mergePartList partitions action =
  let
    seed = case List.head partitions of
      Just t ->
        Maybe.withDefault emptyPart ( List.head partitions )

      Nothing -> -- should never happen
        emptyPart

    rest = Maybe.withDefault [] ( List.tail partitions )
  in
    if List.length rest > 0 then
      List.foldl mergeParts seed rest
    else
      { seed | selected = False }

mergeFilter : Filter -> Partition -> Partition
mergeFilter flt part =
  { part | spec = mergeSpecs part.spec ( filterToSpec flt ) }

filterToSpec : Filter -> Spec
filterToSpec flt =
  Dict.singleton flt.name { emptyClause | action = Include,
                                          field = flt.field,
                                          values = ( Set.singleton flt.value ) }

partName : Spec -> String
partName spec =
  let
    words = List.map clauseWords ( Dict.toList spec )
  in
    String.join " " words

clauseWords : (String, Clause) -> String
clauseWords (name, clause) =
  case clause.action of
    Include ->
      String.join ", " ( Set.toList clause.values ) ++ " " ++ name

    Exclude ->
      "except" ++ String.join ", " ( Set.toList clause.values ) ++ " " ++ name

    None ->
      ""

recordCount : Int -> Partition -> Partition
recordCount rec part =
  { part | count = rec }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      NoOp ->
        model ! []

      Project lens ->
        { model |
            projection = Just lens,
            lenses = List.filter(\l -> l /= lens) model.lenses,
            -- Send query to API and parse response into partitions -TDB now use zeno mock
            partitions = zenoPart ( List.head model.partitions ) lens
        }
        ! [ fetchPartitions lens ]

      NewFilter fltText ->
        { model | filters = [ { byKeyword | value = fltText } ] }
        ! []

      ApplyFilter flt ->
        let
          newParts = List.map ( mergeFilter flt ) model.partitions
        in
          { model | partitions = newParts }
          ! [ filterPartition ( Maybe.withDefault emptyPart ( List.head newParts ) ) ]

      Select part ->
        { model | partitions = List.map(\p -> if p == part then toggleSelect(p) else p) model.partitions }
        ! []

      Add ->
        -- take selected and add to query
        let
          included = List.filter selected model.partitions
          isize = Debug.log "sel-size" ( List.length included )
        --  newSpecs = updateSpecs included model.specList Include
        in
          { model |
              projection = Nothing,
              -- should send revised query and put in a single partition (TBD) - now just fake it
              partitions = [ mergePartList included Include ]
          }
          ! []

      Remove ->
        -- take selected and exclude from query
        let
          excluded = List.filter selected model.partitions
    --      newSpecs = updateSpecs excluded model.specList Exclude
        in
          { model |
              projection = Nothing,
              -- should send revised query and put in a single partition (TBD) - now just fake it
              partitions = [ mergePartList excluded Exclude ]
          }
          ! []

      MakeDocSet ->
        model ! [ createDocset ( Maybe.withDefault emptyPart ( List.head model.partitions ) ) ]

      Clear ->
        emptyModel ! [ fullPartition ]

      LensedPartitions result ->
        case result of
          Ok parts ->
            { model | partitions = List.map ( recordCount parts ) model.partitions }
            ! []

          Err error ->
            model ! []

      FilteredPartition result ->
        case result of
          Ok parts ->
            { model | partitions = List.map ( recordCount parts ) model.partitions }
            ! []

          Err error ->
            model ! []

      NewDocset result ->
        case result of
          Ok ref ->
            model ! []

          Err error ->
            model ! []


-- View

view : Model -> Html.Html Msg
view model =
  let
    lensButton lens = button [ onClick ( Project lens ), buttonStyle ] [ text <| lens.name ]
    filterDiv flt = div [] [input [ type_ "text", placeholder "Keyword", onInput NewFilter ] [], button [ onClick ( ApplyFilter flt ) ] [ text "Filter" ]]
    partDiv part = div
      [ onClick ( Select part )
      , if part.selected then
         partSelStyle
        else
         partUnselStyle
      ]
      [ div [ numberStyle ]
        [ text <| ( toString part.count ) ]
      , div [ nameStyle ]
        [ text <| ( partName part.spec ) ]
      ]
    addButton = button [ onClick ( mapButton model True ), buttonStyle ] [ text <| ( mapButtonText model True ) ]
    removeButton = button [ onClick ( mapButton model False ), buttonStyle ] [ text <| ( mapButtonText model False ) ]
  in
    div
      [ style [ ( "width", "80%" )
              , ( "text-align", "center" )
              ]
      ]
      [ text "MIT Theses And Dissertations"
      , div [] ( List.map filterDiv model.filters )
      , div [] ( List.map lensButton model.lenses )
      , div [] ( List.map partDiv model.partitions )
      , div [] [ addButton, removeButton ]
      ]

-- HTTP/JSON Decoding

svcUri : String
svcUri =
  "http://35.185.15.48:4000/api/"

apiKey : String
apiKey =
  "heart-of-gold"

nullQuery =
  Encoder.object [ ("query",
    Encoder.object [ ("match_all",
      Encoder.object []
    )]
  )]

filterQuery clause =
  log "filterQuery" (
    Encoder.object [ ( "query",
      Encoder.object [ ( "constant_score",
        Encoder.object [ ( "filter", ( filterClause clause ) ) ]
      )]
    )]
  )

filterClause : Clause -> Encoder.Value
filterClause clause =
  Encoder.list ( List.map ( filterTerm clause.field ) ( Set.toList clause.values ) )

filterTerm field value =
  Encoder.object [ ( "term",
    Encoder.object [ ( field, Encoder.string value ) ]
  )]

buildQuery : Partition -> Encoder.Value
buildQuery part =
  let
    clause = Maybe.withDefault emptyClause ( List.head ( Dict.values part.spec ) )
  in
    filterQuery clause

buildDocset : Partition -> Encoder.Value
buildDocset part =
  Encoder.object []

hitsDecoder = field "hits" Json.Decode.int

docsetPost : Partition -> Http.Request String
docsetPost part =
  Http.request
    { method = "POST"
    , headers = [ Http.header "X-Api-Key" apiKey
                , Http.header "Accept" "application/json"
                ]
    , url = svcUri ++ "docsets"
    , body = Http.jsonBody ( buildQuery part )
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }

fullPartition : Cmd Msg
fullPartition =
  Http.send FilteredPartition
    ( Http.post ( svcUri ++ "collections/1/search" ) ( Http.jsonBody nullQuery ) hitsDecoder )

filterPartition : Partition -> Cmd Msg
filterPartition part =
  Http.send FilteredPartition
    ( Http.post ( svcUri ++ "collections/1/search" ) ( Http.jsonBody ( buildQuery part ) ) hitsDecoder )

fetchPartitions : Lens -> Cmd Msg
fetchPartitions lens =
  Http.send LensedPartitions
    ( Http.post ( svcUri ++ "collections/1/search" ) (Http.jsonBody nullQuery) hitsDecoder )

createDocset : Partition -> Cmd Msg
createDocset part =
  Http.send NewDocset ( docsetPost part )

partSelStyle =
    style
        [ ( "background-color", "rgba(52, 152, 219,1.0)" )
--        , ( "width", "400px" )
--        , ( "height", "200px" )
        , ( "color", "white" )
        , ( "font-family", "-apple-system, system, sans-serif" )
        , ( "margin", "20px 0px 0px 20px" )
        , ( "cursor", "pointer" )
        ]

partUnselStyle =
    style
        [ ( "background-color", "rgba(52, 152, 219,0.5)" )
        , ( "color", "white" )
        , ( "font-family", "-apple-system, system, sans-serif" )
        , ( "margin", "20px 0px 0px 20px" )
        , ( "cursor", "pointer" )
        ]

nameStyle =
    style
        [ ( "padding", "20px" )
        , ( "font-size", "18px" )
        , ( "text-align", "center" )
        ]

numberStyle =
    style
        [ ( "padding", "20px 20px 0px 20px" )
        , ( "font-size", "60px" )
        , ( "text-align", "center" )
        ]

buttonStyle =
  style
    [ ( "width", "120px")
    , ( "height", "40px")
    , ( "font-size", "18px" )
    , ( "font-family", "-apple-system, system, sans-serif" )
    ]
