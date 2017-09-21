type 'a my_list =
  | Item of ('a * ' a my_list )
  | Empty ;;

let cons elem my_list =
  match my_list with
    | Empty -> Item (elem, Empty)
    | Item (head, tail) -> Item (elem, Item (head, tail));;

let rec length my_list =
  match my_list with
    | Empty -> 0
    | Item (head, tail) -> (1 + length tail) ;;

let hd my_list =
  match my_list with
    | Empty -> raise (Failure "hd")
    | Item (head, tail) -> head ;;

let tl my_list =
  match my_list with
    | Empty -> raise (Failure "tl")
    | Item (head, tail) -> tail ;;

let rec nth my_list nb =
  match my_list with
    | Empty -> raise (Failure "nth")
    | Item (head, tail) ->
      match nb with
	| 0 -> head
	| nb when nb < 0 -> raise (Invalid_argument "List.nth")
	| nb -> nth tail (nb - 1) ;;

let rev my_list =
  let rec create_list new_list old_list =
    match old_list with
    | Empty -> new_list
    | Item (head, tail) -> create_list (Item(head, new_list)) tail
  in create_list Empty my_list ;;

let append my_list add_list =
  let rec fusion_list my_list add_list =
    match add_list with
      | Empty -> rev my_list
      | Item (head, tail) -> fusion_list (Item(head, my_list)) tail
  in fusion_list (rev my_list) add_list ;;

let rev_append my_list add_list =
  let rec fusion_rev_list my_list add_list =
    match add_list with
      | Empty -> rev my_list
      | Item (head, tail) -> fusion_rev_list (Item(head, my_list)) tail
  in fusion_rev_list my_list add_list ;;

let rec flatten my_list =
  match my_list with
    | Empty -> Empty
    | Item (Item(head, first), second) ->
      append (Item(head, first)) (flatten second)
    | Item (Empty, _) -> Empty ;;

let rec iter funct my_list =
  match my_list with
    | Empty -> ()
    | Item (head, tail) ->
      begin
	funct head;
	iter funct tail
      end ;;

let rec map funct my_list =
  match my_list with
    | Empty -> Empty
    | Item (head, tail) -> Item(funct head, (map funct tail)) ;;

let rec fold_left funct elem my_list =
  match my_list with
    | Empty -> elem
    | Item(head, tail) -> fold_left funct (funct elem head) tail ;;

let rec for_all funct my_list =
  match my_list with
    | Empty -> true
    | Item (head, tail) when funct head && for_all funct tail -> true
    | Item (head, tail) -> false ;;

let rec exists funct my_list =
  match my_list with
    | Empty -> false
    | Item (head, tail) when funct head || for_all funct tail -> true
    | Item (head, tail) -> false ;;

let rec mem value my_list =
  match my_list with
    | Empty -> false
    | Item (head, tail) when (value = head) || (mem value tail) -> true
    | Item (head, tail) -> false ;;

let rec memq value my_list =
  match my_list with
    | Empty -> false
    | Item (head, tail) when (value == head) || (memq value tail) -> true
    | Item (head, tail) -> false ;;

let rec filter funct my_list =
  match my_list with
    | Empty -> Empty
    | Item (head, tail) when funct head -> (Item(head, (filter funct tail)))
    | Item (head, tail) ->  filter funct tail ;;

let split my_list =
  let rec split_list my_list first_list second_list =
    match my_list with
      | Empty -> rev first_list, rev second_list
      | Item ((first, second), tail) ->
	split_list tail (Item(first, first_list)) (Item(second, second_list))
  in  split_list my_list Empty Empty ;;

let rec combine first_list second_list =
  if ((length first_list) != (length second_list)) then
    raise (Invalid_argument "List.combine")
  else
    match first_list with
      | Empty -> Empty
      | Item (first, first_tail) ->
	match second_list with
	  | Empty -> Empty
	  | Item (second, second_tail) ->
	    Item ((first, second), combine first_tail second_tail) ;;

let partition funct my_list =
  let rec  create_partition funct my_list true_list false_list =
    match my_list with
    | Empty -> rev true_list , rev false_list
    | Item (head, tail) when (funct head) ->
      create_partition funct tail (Item(head, true_list)) false_list
    | Item (head, tail) ->
      create_partition funct tail true_list (Item(head, false_list))
  in  create_partition funct my_list Empty Empty ;;

let check first second =
  if first < second then -1
  else if first > second then 1
  else 0 ;;

let sort check my_list =
  match my_list with
    | Empty -> Empty
    | Item (head, tail) when (check head head) == 0 -> my_list
    | Item (head,  tail) -> my_list ;;
