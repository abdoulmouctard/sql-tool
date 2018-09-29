module R = struct

  (* Création des opérations de manipulation des relations à valeur dans Value *)
  include Relation.Make(Value)

  (* Fonctions d'agrégation (à compléter...) *)
  let sum dist =
    fun a r -> fold dist (fun acc v -> match acc with None -> Some v | Some v' -> Some (Value.add v' v)) None a r
end
