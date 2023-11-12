open Values

let fork v = (v, v)

let join_fn = function
| Bottom, w -> w
| v, Bottom -> v
| Top, _ -> Top
| _, Top -> Top
| True, False -> Top
| False, True -> Top
| True, True -> True
| False, False -> False

let and_fn = function
| False, _ -> False
| _, False -> False
| True, w -> w
| v, True -> v
| Bottom, Bottom -> Bottom
| Bottom, Top -> False
| Top, Bottom -> False
| Top, Top -> Top

let or_fn v w = match (v, w) with
| True, _ -> True
| _, True -> True
| False, w -> w
| v, False -> v
| Bottom, Bottom -> Bottom
| Bottom, Top -> True
| Top, Bottom -> True
| Top, Top -> Top

let not_fn = function
| Bottom -> Bottom
| True -> False
| False -> True
| Top -> Top

let pair (op1, op2) = fun (x, y) -> (op1 x, op2 y)

let de_morgan_and (a, b) = pair (not_fn, not_fn) (a, b) |> and_fn |> not_fn