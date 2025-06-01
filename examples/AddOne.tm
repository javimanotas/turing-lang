-- This Turing Machine adds 1 to a given binary number
-- Input: a binary number between 2 blanks e.g. B100101B
-- Output: the binary number plus one (destroys the input)

Main (
    (q0, B) -> (q1, _, R)
    
    (q1, B) -> (q2, _, L)
    (q1, _) -> (q1, _, R)
    
    (q2, 0) -> (q3, 1, L)
    (q2, 1) -> (q4, 0, L)
    
    (q3, B) -> (q5, _, S)
    (q3, _) -> (q3, _, L)
    
    (q4, 1) -> (q4, 0, L)
    (q4, _) -> (q3, 1, L)
)
