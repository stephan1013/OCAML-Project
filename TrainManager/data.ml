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

module Dt : DATA =
struct

  exception Invalid_Data
  type train = (string * string list * int)

  let getType (name, _, _) = name ;;
  let getStation (_, station, _) = station ;;
  let getSpeed (_, _, speed) = speed ;;

  let station =
    [("Paris", "Lyon", 427);
     ("Dijon", "Lyon", 192);
     ("Paris", "Lille", 225);
     ("Paris", "Nancy", 327);
     ("Dijon", "Nancy", 226);
     ("Brest", "Rennes", 248);
     ("Lille", "London", 269);
     ("Liege", "Cologne", 118);
     ("Le Mans", "Paris", 201);
     ("Cologne", "Essen", 81);
     ("Lyon", "Marseille", 325);
     ("Brussels", "Liege", 104);
     ("Paris", "Le Havre", 230);
     ("Rennes", "Le Mans", 163);
     ("Le Mans", "Nantes", 183);
     ("Paris", "Bordeaux", 568);
     ("Lille", "Brussels", 106);
     ("Nancy", "Strasbourg", 149);
     ("Paris", "Strasbourg", 449);
     ("Dijon", "Strasbourg", 309);
     ("Toulouse", "Bordeaux", 256);
     ("Brussels", "Amsterdam", 211);
     ("Montpellier", "Toulouse", 248);
     ("Marseille", "Montpellier", 176)];
  ;;

  let tgv = ("TGV", [("Brest");("Le Havre"); ("Lille"); ("Paris");
			("Strasbourg"); ("Nancy"); ("Dijon"); ("Lyon");
			("Nice"); ("Marseille"); ("Montpellier"); ("Perpignan");
			("Bordeaux"); ("Nantes"); ("Avignon"); ("Rennes");
			("Biarritz"); ("Toulouse"); ("Le Mans")], 230)
  ;;

  let thalys = ("Thalys", [("Paris");("Lille"); ("Liege"); ("Brussels");
		     ("Amsterdam"); ("Cologne"); ("Essen")], 210)
  ;;

  let eurostar = ("Eurostar", [("Paris");("London"); ("Brussels"); ("Lille")],
		  210)
  ;;

end
