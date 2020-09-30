module Prelude

open System
open System.Globalization

let flip f x y = f y x

let trim (s : string) = s.Trim()

let showEscaped (s : string) =
  s.Replace("\r", "\\r").Replace("\n", "\\n").Replace("\t", "\\t")


let (|InvariantEqual|_|) str arg =
  if String.Compare(str, arg, StringComparison.OrdinalIgnoreCase) = 0
    then Some () else None

let (|IsDecimal|_|) (str : string) =
  let styles =
    NumberStyles.Float
    ||| NumberStyles.Number
    ||| NumberStyles.Integer
    ||| NumberStyles.AllowExponent
    ||| NumberStyles.AllowDecimalPoint

  let culture = CultureInfo.InvariantCulture
  let out = ref 0m

  if Decimal.TryParse(str, styles, culture, out) then
    Some <| !out
  else
    None
