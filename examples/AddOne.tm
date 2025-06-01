-- This Turing Machine adds 1 to a given binary number
-- Input: a binary number between 2 blanks e.g. B100101B
-- Output: the binary number plus one (destroys the input)

Main (
    (q0, B) -> (q1, B, R) |
    (q1, 0) -> (q1, 0, R) |
    (q1, 1) -> (q1, 1, R) |
    (q1, B) -> (q2, B, L) |
    (q2, 0) -> (q3, 1, L) |
    (q2, 1) -> (q4, 0, L) |
    (q3, 0) -> (q3, 0, L) |
    (q3, 1) -> (q3, 1, L) |
    (q3, B) -> (q5, B, S) |
    (q4, 1) -> (q4, 0, L) |
    (q4, 0) -> (q3, 1, L) |
    (q4, B) -> (q3, 1, L)
)
