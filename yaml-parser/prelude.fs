module Prelude

let flip f x y = f y x

let trim (s : string) = s.Trim()

let showEscaped (s : string) =
  s.Replace("\r", "\\r").Replace("\n", "\\n").Replace("\t", "\\t")
