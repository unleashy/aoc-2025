module Aoc.Day1

let private maxDial = 100

let private parseRotation (s: string) =
  match s with
  | Etc.Str('L', num) -> maxDial - int num
  | Etc.Str('R', num) -> int num
  | s -> failwithf $"invalid rotation {s}"

let private rotate dial n =
  (dial + n) % maxDial

let part1 input =
  input
  |> Etc.lines
  |> Seq.map parseRotation
  |> Seq.scan rotate 50
  |> Etc.count 0
