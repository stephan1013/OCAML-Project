open String

module type COMMAND =
sig
    val find: string -> char -> int
    val epur: string -> string
    val myStrToList: string -> string list -> char -> string list
    val commandToList: string -> string list
end

module Cmd : COMMAND =
struct

  let find cmd char =
    try index cmd char with
      | Not_found -> String.length cmd
  ;;

  let rec epur str =
    match str with
      | "" -> ""
      | str when str.[0] == ' ' -> ((String.sub str 0 1)^epur(String.trim str))
      | str ->
	((String.sub str 0 1)^epur(String.sub str 1 ((String.length str) - 1)))
  ;;

  let rec myStrToList cmd list char =
    match cmd with
      | cmd when (find cmd char) == String.length cmd -> List.append list (cmd::[])
      | _ -> myStrToList
	(sub cmd ((find cmd char) + 1) (String.length cmd - ((find cmd char) + 1)))
	(List.append list ((sub cmd 0 (find cmd char))::[])) char
  ;;

  let commandToList cmd = myStrToList cmd [] ' ';;
end
