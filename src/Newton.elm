module Newton exposing (optimize)

-- Gradient descent method (https://fr.wikipedia.org/wiki/M%C3%A9thode_de_Newton)
-- Code extracted from SciPy (use `scipy.optimize.newton??` inside IPython), then simplified


maxiter =
    50


rel_tol =
    0.0


abs_tol =
    0.0017


eps =
    1.0e-4


optimize : (Float -> Float) -> Float -> Maybe Float
optimize f x0 =
    let
        _ =
            f 0.2083
                |> Debug.log "Value for TAEG"

        p0 =
            x0

        p11 =
            x0 * (1 + eps)

        p1 =
            if p11 >= 0 then
                p11 + eps

            else
                p11 - eps

        q0 =
            f p0

        q1 =
            f p1
    in
    if abs q1 < abs q0 then
        optimizer f p1 p0 q1 q0 maxiter

    else
        optimizer f p0 p1 q0 q1 maxiter


isclose : Float -> Float -> Bool
isclose a b =
    if a == b then
        True

    else
        let
            diff =
                abs (b - a)
        in
        diff <= abs (rel_tol * b) || diff <= abs (rel_tol * a) || diff <= abs_tol


optimizer : (Float -> Float) -> Float -> Float -> Float -> Float -> Int -> Maybe Float
optimizer f p0 p1 q0 q1 step =
    if step == 0 then
        Nothing

    else if q1 == q0 then
        if p1 /= p0 then
            Nothing

        else
            Just <| (p1 + p0) / 2

    else
        let
            p =
                if abs q1 > abs q0 then
                    (-q0 / q1 * p1 + p0) / (1 - q0 / q1)

                else
                    (-q1 / q0 * p0 + p1) / (1 - q1 / q0)
        in
        if isclose p p1 then
            Just p

        else
            let
                q =
                    f p
            in
            optimizer f p1 q1 p q (step - 1)
