open List
open String

module type CONTACT =
sig

  exception Invalid_Data
  type contact

  val sortContact: contact list -> contact list
  val create: (string * string * int * string * string) -> contact
  val getFirstName: contact -> string
  val getLastName: contact -> string
  val getAge: contact -> int
  val getEmail: contact -> string
  val getPhone: contact -> string
  val findFirstName: contact list -> string -> int -> int
  val findLastName: contact list -> string -> int -> int
  val findAge: contact list -> string -> int -> int
  val findEmail: contact list -> string -> int -> int
  val findPhone: contact list -> string -> int -> int
  val findAll: contact list -> string -> int -> int
  val printContact: contact -> int -> unit
end

module Contact : CONTACT =
struct

  exception Invalid_Data
  type contact = (string * string * int * string * string)

  let getFirstName (fistname, _, _, _, _) = fistname
  let getLastName (_, lastname, _, _, _) = lastname
  let getAge (_, _, age, _, _) = age
  let getEmail (_, _, _, email, _) = email
  let getPhone (_, _, _, _, phone) = phone

  let mycompar first second =
    match first with
      | first when compare (getFirstName first) (getFirstName second) > 0 -> 1
      | first when compare (getFirstName first) (getFirstName second) == 0 &&
	  compare (getLastName first) (getLastName second) > 0 -> 1
      | first when compare (getFirstName first) (getFirstName second) == 0 &&
	  compare (getLastName first) (getLastName second) < 0 -> -1
      | _ -> -1

  let sortContact list = sort mycompar list

  let checkName str =
    if str == "" then 1
    else 0

  let rec epur_str str check =
    match str with
      | "" -> ""
      | str when str.[0] == ' ' && check != 0 ->
	((sub str 0 1)^capitalize_ascii (epur_str (trim str) check))
      | str when str.[0] == ' ' && check == 0 ->
	capitalize_ascii (epur_str (trim str) check)
      | str when str.[0] == '-' ->
	((sub str 0 1)^capitalize_ascii(epur_str(sub str 1 (length str - 1)) check))
      | str -> ((sub str 0 1)^(epur_str(sub str 1 ((length str) - 1)) 1 ))

  let rec checkEmail str len i valid =
    match str with
      | str when str == "" -> 1
      | str when i == len -> 1
      | str when str.[i] == '@' &&
	  (i + 1) <= len && i != 0 -> checkEmail str len (i + 1) 1
      | str when valid == 1 &&
	  str.[i] == '.' && (i + 1) <= len && str.[i - 1] != '@' -> 0
      | str -> checkEmail str len (i + 1) valid

  let rec checkPhone str len i num =
    match str with
      | str when str == "" -> 1
      | str when str.[0] != '0' -> 1
      | str when num == 5 && i > len-> 0
      | str when i + 1 >= len -> 1
      | str when num != 0 && str.[i - 1] != ' ' -> 1
      | str when str.[i] >= '0' && str.[i] <= '9' &&
	  str.[i + 1] >= '0' && str.[i + 1] <= '9' ->
	checkPhone str len (i + 3) (num + 1)
      | str -> 1

  let create (fistname, lastname, age, email, phone) =
    begin
      if age < 0 || age > 120 then raise Invalid_Data
      else if checkName fistname != 0 then raise Invalid_Data
      else if checkName lastname != 0 then raise Invalid_Data
      else if checkEmail email (length email) 0 0 != 0 then raise Invalid_Data
      else if checkPhone phone (length phone) 0 0 != 0 then raise Invalid_Data
      else (capitalize_ascii(trim(epur_str fistname 0)),
	    uppercase_ascii(trim(epur_str lastname 0)), age, email, phone)
    end

  let rec myStrStr first second i j flen slen=
    match first with
      | first when i == flen -> 0
      | first when j == slen -> 1
      | first when first.[i] == second.[j] ->myStrStr first second (i + 1) (j + 1) flen slen
      | _ -> myStrStr first second 0 (j + 1) flen slen

  let rec findFirstName list fistname id =
    match list with
      | [] -> -1
      | _ ->	match fistname with
	  | fistname when myStrStr (lowercase_ascii fistname)
	      (lowercase_ascii(getFirstName (hd list))) 0 0 (length fistname)
	      (length (getFirstName (hd list))) == 0 -> id
	  | _ -> findFirstName (tl list) fistname (id + 1)

  let rec findLastName list lastname id =
    match list with
      | [] -> -1
      | _ ->
	match lastname with
	  | lastname when myStrStr (lowercase_ascii lastname)
	      (lowercase_ascii(getLastName (hd list))) 0 0 (length lastname)
	      (length (getLastName (hd list))) == 0 -> id
	  | _ -> findLastName (tl list) lastname (id + 1)

  let rec findAge list age id =
    match list with
      | [] -> -1
      | _ ->
	match age with
 	  | age when  int_of_string age == (getAge (hd list))-> id
	  | _ -> findAge (tl list) age (id + 1)

  let rec findEmail list email id =
    match list with
      | [] -> -1
      | _ ->
	match email with
	  | email when myStrStr (lowercase_ascii email)
	      (lowercase_ascii(getEmail (hd list))) 0 0 (length email)
	      (length (getEmail (hd list))) == 0 -> id
	  | _ -> findEmail (tl list) email (id + 1)

  let rec findPhone list phone id =
    match list with
      | [] -> -1
      | _ ->
	match phone with
	  | phone when myStrStr (lowercase_ascii phone)
	      (lowercase_ascii(getPhone (hd list))) 0 0 (length phone)
	      (length (getPhone (hd list))) == 0 -> id
	  | _ -> findPhone (tl list) phone (id + 1)

  let rec findAll list all id =
    match list with
      | [] -> -1
      | _ ->
	match all with
	  | all when myStrStr (lowercase_ascii all)
	      (lowercase_ascii(getFirstName (hd list))) 0 0 (length all)
	      (length (getFirstName (hd list))) == 0 ||
	      myStrStr (lowercase_ascii all)
	      (lowercase_ascii(getLastName (hd list))) 0 0 (length all)
	      (length (getLastName (hd list))) == 0 ||
	      int_of_string all == (getAge (hd list)) ||
	      myStrStr (lowercase_ascii all)
	      (lowercase_ascii(getEmail (hd list))) 0 0 (length all)
	      (length (getEmail (hd list))) == 0 ||
	      myStrStr (lowercase_ascii all)
	      (lowercase_ascii(getPhone (hd list))) 0 0 (length all)
	      (length (getPhone (hd list))) == 0 -> id
	| _ -> findAll (tl list) all (id + 1)

  let myPrintNbr nbr =
    match nbr with
      | nbr when nbr < 10 ->
	  begin
	    print_int nbr;
	    print_string "   ";
	  end
      | nbr when nbr < 100 ->
	begin
	    print_int nbr;
	    print_string "  ";
	  end
      | _ ->
	begin
	  print_int nbr;
	  print_string " ";
	end

  let rec myPrintStr str i len size =
    match str with
      | str when i == size -> ()
      | str when i >= len ->
	begin
	  print_char ' ';
	  myPrintStr str (i + 1) len size
	end
      | _ ->
	begin
	    print_char str.[i];
	  myPrintStr str (i + 1) len size
	end

  let printContact contact id =
    begin
      myPrintNbr id;
      myPrintStr (getFirstName contact) 0 (length (getFirstName contact)) 16;
      myPrintStr (getLastName contact) 0 (length (getLastName contact)) 16;
      myPrintNbr (getAge contact);
      myPrintStr (getEmail contact) 0 (length (getEmail contact)) 32;
      myPrintStr (getPhone contact) 0 (length (getPhone contact)) 14;
      print_char '\n';
    end
end
