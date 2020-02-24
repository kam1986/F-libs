module Position


// file Position type tracks line number, line offset and absolut position in the file 
type position =
    {
        line : int
        offset : int
        absolut : int
    }

let startPos =
    {
        line = 0
        offset = 0
        absolut = 0
    }
let Move n position =
    { position with
        offset = position.offset + n
        absolut = position.absolut + n
    }

let Next position = Move 1 position

let Newline position =
    { position with
        offset = 0
        absolut = position.absolut + 1
        line = position.line + 1
    }
