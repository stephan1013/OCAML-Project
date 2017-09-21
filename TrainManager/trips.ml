open Data
open Command

module type TRIPS =
sig
  exception Invalid_Data

  type trips = (string * string * string * string list)
  val getId: trips -> string
  val getDate: trips -> string
  val getStation: trips -> string list
  val checkTrain: string -> string list -> int
  val checkStation: string list-> string list -> int
  val findStation: string list -> string list -> int
  val checkData: string list -> int
  val deleteTrip: trips list -> string -> trips list -> trips list
  val createTrip: string list -> trips list -> trips list
  val listTrip: trips list -> trips list -> trips list

end

module Trp: TRIPS =
struct
  exception Invalid_Data

  type trips = (string * string * string * string list)

  let getType (typ, _, _, _) = typ;;
  let getId (_, id, _, _) = id ;;
  let getDate (_, _, date, _) = date ;;
  let getStation (_, _, _, station) = station ;;

  let rec checkTrain station listStation =
    match listStation with
      | [] -> 1
      | _ ->
	begin
	  if station = (List.hd listStation) then 0
	  else checkTrain station (List.tl listStation)
	end
  ;;

  let rec checkStation cmd refe =
    match cmd with
      | [] -> 0
      | _ ->
	begin
	  if checkTrain (List.hd cmd) refe = 1 then 1
	  else checkStation (List.tl cmd) refe
	end
  ;;

  let rec findStation cmd train =
    match cmd with
      | [] -> raise Invalid_Data
      | _ ->
	begin
	  if Cmd.find (List.hd cmd) ',' = String.length (List.hd cmd) then
	    findStation (List.tl cmd) train
	  else
	    checkStation (Cmd.myStrToList (List.hd cmd) [] ',') train
	end
  ;;

  let checkData cmd =
    begin
      if (List.hd cmd) = "TGV" then
	begin
	  try findStation cmd (Dt.getStation Dt.tgv) with
	    | Invalid_Data -> 1
	end
      else if (List.hd cmd) = "Thalys" then
	begin
	  try findStation cmd (Dt.getStation Dt.thalys) with
	    | Invalid_Data -> 1
	end
      else if (List.hd cmd) = "Eurostar" then
	begin
	  try findStation cmd (Dt.getStation Dt.eurostar) with
	    | Invalid_Data -> 1
	end
      else
	1
    end
  ;;

  let findType cmd = (List.hd cmd);;

  let findId cmd = (List.hd cmd) ^
    (Pervasives.string_of_int(Random.int(8999) + 1000));;

  let findDate cmd = ((List.hd (List.tl cmd)) ^ " ") ^
    (List.hd (List.tl (List.tl cmd)));;

  let findListStation cmd = Cmd.myStrToList
    (List.hd (List.tl (List.tl (List.tl cmd)))) [] ',';;

  let createTrip cmd list =
    if checkData cmd == 1 then
      begin
	let _ = Printf.printf ("Trip not created: conflic with %s %d")
	  (List.hd (List.tl cmd)) (Random.int(8999) + 1000)
	in
	list
      end
    else
      begin
	let newList = (findType cmd, findId cmd, findDate cmd, findListStation cmd)
	in
	let _ =
	    Printf.printf ("Trip created: %s %s\n") (getType newList)
	      (String.sub  (getId newList)
		 (String.length (getType newList))
		 ((String.length (getId newList)) -
		     (String.length (getType newList))))
	in
	(List.append list (newList::[]))
      end
  ;;

  let rec deleteTrip listTrip id newList =
    match listTrip with
      | [] -> raise Invalid_Data
      | listTrip when (getId (List.hd listTrip)) = id ->
	List.append newList (List.tl listTrip)
      | _ -> deleteTrip
	(List.tl listTrip) id (List.append newList ((List.hd listTrip)::[]))
  ;;

  let rec printStation stations pos arrive depart =
    match stations with
      | [] -> ()
      | _ ->
	begin
	  let _  = Printf.printf ("%s (%s) (%s)\n") (List.hd stations) (arrive) (depart)
	  in
	  if (List.tl pos) = [] then
	    printStation (List.tl stations) pos depart ","
	  else
	    printStation (List.tl stations) (List.tl pos) depart depart
	end
  ;;

  let printData trip =
    begin
      printStation (getStation trip) (List.tl (getStation trip)) "," (getDate trip),
      Printf.printf ("%s %s\n")
	(getType trip)
	(String.sub(getId trip)
	   (String.length (getType trip))
	   ((String.length (getId trip)) - (String.length (getType trip))))
    end
  ;;

  let rec listTrip trip list =
    match trip with
      | [] -> list
      | _ ->
	begin
	  let _ = printData (List.hd trip)
	  in
	  listTrip (List.tl trip) list
	end
  ;;
end
