module Quarter exposing (Quarter(..), buildPlanDays, fromName)


type Quarter
    = Q1
    | Q2
    | Q3
    | Q4


days : Quarter -> List Int
days quarter =
    case quarter of
        Q1 ->
            [ 28, 31, 30, 31, 30, 31, 31, 30, 31 ]

        Q2 ->
            [ 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

        Q3 ->
            [ 30, 31, 30, 31, 31, 28, 31, 30, 31 ]

        Q4 ->
            [ 30, 31, 31, 28, 31, 30, 31, 30, 31 ]


plan_builder : Int -> ( Int, List Int ) -> ( Int, List Int )
plan_builder next ( previous, result ) =
    let
        val =
            next + previous
    in
    ( val, val :: result )


buildPlanDays : Quarter -> Int -> List Int
buildPlanDays quarter n =
    days quarter
        |> List.take (n - 1)
        |> List.foldl plan_builder ( 0, [] )
        |> Tuple.second
        |> List.reverse


fromName : String -> Maybe Quarter
fromName name =
    let
        suffix =
            String.right 2 name
    in
    case suffix of
        "T1" ->
            Just Q1

        "T2" ->
            Just Q2

        "T3" ->
            Just Q3

        "T4" ->
            Just Q4

        _ ->
            Nothing
