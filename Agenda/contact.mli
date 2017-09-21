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

module Contact : CONTACT
