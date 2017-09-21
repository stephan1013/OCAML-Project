module type DATA =
sig

  exception Invalid_Data

  type train

  val station: (string * string * int) list
  val getType: train -> string
  val getStation: train -> string list
  val getSpeed: train -> int
  val tgv: train
  val thalys: train
  val eurostar: train

end

module Dt : DATA
