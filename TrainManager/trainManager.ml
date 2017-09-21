open Command
open Trips
open List

let rec main list =
  let module Mode = Trip.MakeTrip (Data.Dt) (Command.Cmd) in
  let cmd =
    try read_line () with
      | End_of_file -> exit 0
  in
  let cmdList = Cmd.myStrToList cmd [] ' ' in
  if (hd cmdList) = "quit" then exit 0
  else
    let tripList =
      if (hd cmdList) = "create" then Trp.createTrip (tl cmdList) list
      else if (hd cmdList) = "delete" then Trp.deleteTrip list (hd (tl cmdList)) []
      else if (hd cmdList) = "list" then Trp.listTrip list list
      else list
    in main tripList
;;

main [];;
