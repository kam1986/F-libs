module Token

open Position

type 'R RegToken =
    | ATOM of 'R
    | OR 
    | STAR | PLUS
    | HAT | LINE
    | LPAR | RPAR | LBRA | RBRA 
    | NOISE // for easy removal of whitespace, tab, newline and more
    

// this is the pattern of an atom
let atom symbol =
    [
        ''', NOISE // will be removed by the filter 
        symbol, ATOM symbol
        ''', NOISE // -||-
    ]

let Tokens =
    [
        '|', OR, Next
        '*', STAR, Next
        '+', PLUS, Next
        '-', LINE, Next
        '^', HAT, Next
        '(', LPAR, Next
        ')', RPAR, Next
        '{', LBRA, Next
        '}', RBRA, Next
        // whitespace, tab and newlines not enclosed with ' are all noise
        ' ', NOISE, Next
        '\t', NOISE, Next
        '\n', NOISE, Newline
        '\r', NOISE, Newline
    ]