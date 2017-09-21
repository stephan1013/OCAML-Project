open Data
open Command

module type TRIP =
sig
  exception Invalid_Data

  type trip
  val getId: trip -> string
  val getDate: trip -> string
  val getStation: trip -> string list
  val checkTrain: string -> string list -> int
  val checkStation: string list-> string list -> int
  val findStation: string list -> string list -> int
  val checkData: string list -> int
  val deleteTrip: trip list -> string -> trip list -> trip list
  val createTrip: string list -> trip list -> trip list

end

module type MAKETRIP =
  functor (Data: DATA) (Command: COMMAND) -> TRIP

module MakeTrip : MAKETRIP =
  functor (Data: DATA) (Command: COMMAND) ->
struct
  exception Invalid_Data

  type trip = (string * string * string list)


  let getId (id, _, _) = id ;;
  let getDate (_, date, _) = date ;;
  let getStation (_, _, station) = station ;;

  let rec checkTrain station listStation =
    match listStation with
      | [] -> 1
      | _ ->
	begin
	  if station == (List.hd listStation) then 0
	  else checkTrain station (List.tl listStation)
	end
  ;;

  let rec checkStation cmd refe =
    match cmd with
      | [] -> 0
      | _ ->
	begin
	  if checkTrain (List.hd cmd) refe == 1 then 1
	  else checkStation (List.tl cmd) refe
	end
  ;;

  let rec findStation cmd train =
    match cmd with
      | [] -> raise Invalid_Data
      | _ ->
	begin
	  if Command.find (List.hd cmd) ',' == String.length (List.hd cmd) then
	    findStation (List.tl cmd) train
	  else
	    checkStation (Command.myStrToList (List.hd cmd) [] ',') train
	end
  ;;

  let checkData cmd =
    begin
      if (List.hd cmd) == "TGV" then
	begin
	  try findStation cmd (Data.getStation Data.tgv) with
	    | Invalid_Data -> 1
	end
      else if (List.hd cmd) == "Thalys" then
	begin
	  try findStation cmd (Data.getStation Data.thalys) with
	    | Invalid_Data -> 1
	end
      else if (List.hd cmd) == "Eurostar" then
	begin
	  try findStation cmd (Data.getStation Data.eurostar) with
	    | Invalid_Data -> 1
	end
      else
	1
    end
  ;;

  let findId cmd = (List.hd cmd);;

  let findDate cmd = ((List.hd (List.tl cmd)) ^ " ") ^
    (List.hd (List.tl (List.tl cmd)));;

  let findListStation cmd = Command.myStrToList
    (List.hd (List.tl (List.tl (List.tl cmd)))) [] ',';;

  let createTrip cmd list =
    if checkData cmd == 1 then raise Invalid_Data
    else
      (List.append list ((findId cmd, findDate cmd, findListStation cmd)::[]))
  ;;

  let rec deleteTrip listTrip id newList =
    match listTrip with
      | [] -> raise Invalid_Data
      | listTrip when (getId (List.hd listTrip)) == id -> List.append newList (List.tl listTrip)
      | _ -> deleteTrip
	(List.tl listTrip) id (List.append newList ((List.hd listTrip)::[]))
end
