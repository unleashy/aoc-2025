module Aoc.Day1

let private dialMax = 100
let private dialStart = 50

let private parseRotation (s: string) =
  match s with
  | Etc.Str('L', num) -> -int num
  | Etc.Str('R', num) -> +int num
  | s -> failwithf $"invalid rotation {s}"

let private spreadRotation n =
  Seq.replicate (abs n) (sign n)

let private rotate dial n =
  Etc.floorRem (dial + n) dialMax

let part1 input =
  input
  |> Etc.lines
  |> Seq.map parseRotation
  |> Seq.scan rotate dialStart
  |> Etc.count 0

let part2 input =
  input
  |> Etc.lines
  |> Seq.map parseRotation
  |> Seq.collect spreadRotation
  |> Seq.scan rotate dialStart
  |> Etc.count 0
