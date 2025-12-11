module Aoc.Day2

let inline private numberLength n =
  1 + int (log10 (double n))

let private shift k n =
  floor (n / 10.0**(int k))

// this forms a semigroup!
let private concatNumber a b =
  a * 10.0**(numberLength b) + b

let private replicateNumber k n =
  Seq.replicate k n
  |> Seq.reduce concatNumber

let private isReplication k n =
  let n = double n
  let l = numberLength n
  l % k = 0 && n = replicateNumber k (shift (l - l / k) n)

let private isDoublet =
  isReplication 2

let rec private isAnyReplication n =
  seq { 2 .. numberLength n }
  |> Seq.exists (fun k -> isReplication k n)

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

let part2 input =
  parse input
  |> Seq.collect (fun (left, right) ->
    seq { left .. right }
    |> Seq.filter isAnyReplication
  )
  |> Seq.sum
