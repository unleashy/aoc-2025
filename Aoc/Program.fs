open System
open System.IO
open System.Reflection
open System.Text

type Info = { Day: int; Part: int }

let inputsDir =
  Path.Join(AppContext.BaseDirectory, "..", "..", "..", "Inputs")
  |> Path.GetFullPath

Environment.CurrentDirectory <- inputsDir

let readInput day =
  let path = Path.Join(inputsDir, $"day{day}.txt")

  File.ReadAllText(path, Encoding.UTF8)
  |> _.ReplaceLineEndings("\n")
  |> _.Trim()

let getImpl info =
  let cls =
    Assembly
      .GetExecutingAssembly()
      .GetType($"Aoc.Day{info.Day}", throwOnError = true)

  let mutable mth =
    cls
      .GetMethod($"part{info.Part}", BindingFlags.Public ||| BindingFlags.Static)

  if mth.ContainsGenericParameters then
    mth <- mth.MakeGenericMethod(typeof<string>)

  fun input -> mth.Invoke(cls, [| input |])

let go info =
  let input = readInput info.Day
  let run = getImpl info

  let result = run input

  printfn $"%A{result}"

go { Day = 1; Part = 2; }
