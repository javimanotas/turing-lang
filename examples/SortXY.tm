-- This Turing Machine sorts a sequence of Xs and Ys
-- Input: a sequence of Xs and Ys between 2 blanks e.g. BXXXYYYXXYYXB
-- Output: the same sequence between blanks but with all the Xs comming first and Ys last

Stop = ()

Place_a = (
    (q0, _) -> (q1, a, S)
)

Right = (
    (q0, _) -> (q1, _, R)
)

RightUntil_a = (
    Right
    | a -> Stop
    | _ -> RightUntil_a
)

Left = (
    (q0, _) -> (q1, _, L)
)

LeftUntil_a = (
    Left
    | a -> Stop
    | _ -> LeftUntil_a
)

XMisplaced = (
    Place_Y, LeftUntil_B, RightUntil_Y, Place_X, Main
)

YAppeared = (
    Right
    | Y -> YAppeared
    | B -> LeftUntil_B
    | X -> XMisplaced
)

Main = (
    Right
    | B -> LeftUntil_B
    | X -> Main
    | Y -> YAppeared
)
