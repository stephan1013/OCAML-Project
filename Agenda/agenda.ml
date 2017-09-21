open Contact
open List

exception Add_Contact_With_Invalid_Data
exception Remove_Impossible_On_An_Empty_List
exception Remove_Using_An_Invalid_Id
exception Replace_Using_An_Invalid_Id
exception Replace_Contact_With_Invalid_Data

type field = All | Id | FirstName | LastName | Age | Email | Phone

module type AGENDA =
sig
  val addContact    : Contact.contact list -> string * string * int * string * string -> Contact.contact list
  val getContactId   : Contact.contact list -> field -> string -> int
  val removeContact  : Contact.contact list -> int -> Contact.contact list
  val replaceContact : Contact.contact list -> int -> string * string * int * string * string -> Contact.contact list
  val printContacts  : Contact.contact list -> field -> string -> unit
end

module Agenda : AGENDA =
struct

  let addContact list elem =
    try Contact.sortContact (cons (Contact.create elem) list) with
      | Contact.Invalid_Data -> raise Add_Contact_With_Invalid_Data

  let rec getContactId list field data =
    match list with
      | [] -> -1
      | _ ->
	match field with
	  | FirstName -> Contact.findFirstName list data 0
	  | LastName -> Contact.findLastName list data 0
	  | Age -> Contact.findAge list data 0
	  | Email -> Contact.findEmail list data 0
	  | Phone -> Contact.findPhone list data 0
	  | Id -> int_of_string data
	  | All -> Contact.findAll list data 0

  let removeContact list id =
    let rec findContact list id new_list =
      match list with
	| [] when id > 0 -> raise Remove_Using_An_Invalid_Id
        | list when id == 0 -> append new_list (tl list)
	| _ -> findContact (tl list) (id - 1) (cons (hd list) new_list)
    in if list == [] then raise Remove_Impossible_On_An_Empty_List
      else findContact list id []

  let replaceContact list id data =
    let rec rplContact list id data new_list=
      match list with
        | [] -> raise Replace_Using_An_Invalid_Id
        | list when id == 0 ->
	  begin
	    try Contact.sortContact (append (tl new_list) (addContact list data))  with
	      | Add_Contact_With_Invalid_Data -> raise Replace_Contact_With_Invalid_Data
	  end
        | _ -> rplContact (tl list) (id - 1) data (cons (hd list) new_list)
    in rplContact list id data []

  let rec printAllContacts list id=
    match list with
      | [] -> ()
      | _ ->
	begin
	  Contact.printContact (hd list) id;
	  printAllContacts (tl list) (id +  1)
	end

  let rec printContactId list pos id=
    match list with
      | [] -> ()
      | list when pos == 0 -> Contact.printContact (hd list) id;
      | _ -> printContactId (tl list) (pos - 1) id

  let printContacts list field data =
      match list with
	| [] -> ()
	| _ ->
	  begin
	    if data != "" then printAllContacts list 0
	    else printContactId list (getContactId list field data)
	      (getContactId list field data)
	  end
end
