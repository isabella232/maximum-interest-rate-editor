module Quarter exposing (Quarter(..), days, fromName)

import Days


type Quarter
    = Q1
    | Q2
    | Q3
    | Q4


days : Quarter -> List (List Int)
days quarter =
    let
        days_candidates =
            case quarter of
                Q1 ->
                    [ "2021-01-01", "2021-02-01", "2021-03-01", "2021-03-31" ]

                Q2 ->
                    [ "2021-04-01", "2021-05-01", "2021-06-01", "2021-06-30" ]

                Q3 ->
                    [ "2021-07-01", "2021-08-01", "2021-09-01", "2021-09-30" ]

                Q4 ->
                    [ "2021-10-01", "2021-11-01", "2021-12-01", "2021-12-31" ]

        days_candidates_posix =
            List.map Days.toPosix days_candidates
    in
    days_candidates_posix
        |> List.map Days.timeBetweenPayments


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
