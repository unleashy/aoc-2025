module Aoc.Etc

let (|Empty|Str|) (s: string) =
  if s.Length > 0 then
    Str(s[0], s[1..])
  else
    Empty

let lines (s: string) =
  s.Split("\n")

let count x xs =
  xs
  |> Seq.filter ((=) x)
  |> Seq.length

let floorRem a n =
  let div = (float a / float n) |> floor |> int
  a - n * div
