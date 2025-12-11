module Aoc.Day2

let private numberLength n =
  1.0 + floor (log10 n)

let private shiftHalf n =
  floor (n / 10.0**(numberLength n / 2.0))

let private intoDoublet n =
  n * 10.0**(numberLength n) + n

let private isDoublet n =
  let n = double n
  n = intoDoublet (shiftHalf n)

let private parse (input: string) =
  input
  |> _.Split(',')
  |> Array.map (fun it ->
    let xs = it.Split('-')
    int64 xs[0], int64 xs[1]
  )

let part1 input =
  parse input
  |> Seq.collect (fun (left, right) ->
    seq { left .. right }
    |> Seq.filter isDoublet
  )
  |> Seq.sum
