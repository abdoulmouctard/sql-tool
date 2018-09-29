(* Signature des modules de données, i.e., des valeurs atomiques acceptées dans une relation *)

module type DATA =
sig

  (* type des domaines de valeurs *)
  type domain

  (* type des valeurs *)
  type value

  (* fonction de conversion avec les chaînes de caractères (pour les entrées/sorties CSV) *)
  val string_of_value: value -> string

  val string_of_domain: domain -> string

  val value_of_string: domain -> string -> value

  val domain_of_string: string -> domain

end


(* Signature des modules de relations avec une partie des opérations de l'algèbre relationnelle *)

module type S =
sig

  (* type des domaines de valeurs *)
  type domain

  (* type des valeurs *)
  type value

  (* type des relations à valeurs de type values *)
  type relation

  (* type des tuples des relations *)
  type tuple

  (* type des attributs *)
  type attribute

  (* ''attribute a t'' retourne la valeur de l'attribut ''a'' du tuple ''t'' si elle existe *)
  val attribute: attribute -> tuple -> value option

  (* ''domain r a'' retourne le domaine de l'attribut ''a'' de la relation ''r'' *)
  val domain: relation -> attribute -> domain

  (* ''width r'' retourne le nombre d'attributs de la relation ''r'' *)
  val width: relation -> int

  (* ''cardinal r'' retourne le nombre de tuples de la relation ''r'' *)
  val cardinal: relation -> int

  (* ''distinct r'' retourne la relation ''r'' sans doublon *)
  val distinct: relation -> relation

  (* ''selection p r'' retourne la relation ''r'' privée des tuples ne vérifiant pas le prédicat ''p'' *)
  val selection: (tuple -> bool) -> relation -> relation

  (* ''projection [ (d_0, f_0) ; ... ; (d_n, f_n) ] r'' retourne une relation à ''n+1'' colonnes
   * calculée par projection de la relation ''r'' ; chaque fonction ''f_i'' exprime le calcul du ''i''ème
   * attribut de domaine ''d_i''.
   *)
  val projection: (domain * (tuple -> value option)) list -> relation -> relation

  (* ''union r1 r2'' retourne l'union des relations ''r1'' et ''r2'' en conservant les doublons ; ''r1'' et ''r2'' doivent avoir le même schéma *)
  val union: relation -> relation -> relation

  (* ''inter r1 r2'' retourne l'intersection des relations ''r1'' et ''r2'' en conservant les doublons ; ''r1'' et ''r2'' doivent avoir le même schéma *)
  val inter: relation -> relation -> relation

  (* ''diff r1 r2'' retourne la relation ''r1'' privée de la relation ''r2'' en conservant les doublons ; ''r1'' et ''r2'' doivent avoir le même schéma *)
  val diff: relation -> relation -> relation

  (* ''cossjoin r1 r2'' retourne le produit cartésien des relations ''r1'' et ''r2'' *)
  val crossjoin: relation -> relation -> relation

  (* ''innerjoin p r1 r2'' retourne la theta-jointure des relations ''r1'' et ''r2'' en respectant le prédicat ''p'',
     c'est-à-dire le produit cartésien de ''r1'' et ''r2'' où ne sont gardés que les paires des tuples ''t1'' de ''r1'' et
     ''t2'' de ''r2'' tels que ''p t1 t2'' est vrai
   *)
  val innerjoin: (tuple -> tuple -> bool) -> relation -> relation -> relation

  (* ''leftouterjoin p r1 r2'' retourne la theta-jointure des relations ''r1'' et ''r2'' en respectant le prédicat ''p'',
     tout en conservant les tuples défaillants de ''r1''
   *)
  val leftouterjoin: (tuple -> tuple -> bool) -> relation -> relation -> relation

  (* ''fullouterjoin p r1 r2'' retourne la theta-jointure des relations ''r1'' et ''r2'' en respectant le prédicat ''p'',
     tout en conservant les tuples défaillants de ''r1'' et les tuples défaillants de ''r2''
   *)
  val fullouterjoin: (tuple -> tuple -> bool) -> relation -> relation -> relation

  (* ''fold dist f z a r'' réduit l'ensemble si ''dist'' est vrai, le multi-ensemble sinon, des valeurs de l'attribut ''a''
     des tuples de la relation ''r'' ; autrement dit, cela équivaut au calcul

       ''f (... (f (f z (attribute a t_1)) (attribute a t_2)) ... (attribute a t_n)''

     où les ''t_i'' représentent les tuples de la relation ''r'' ayant un attribut ''a'' non-nul
   *)
  val fold: bool -> ('a -> value -> 'a) -> 'a -> attribute -> relation -> 'a

  (* ''aggregate [ a_0 ; ... ; a_{p-1} ] [ (d_p, f_p) ; (d_n, f_n) ] r'' retourne une relation à ''n+1'' colonnes par agrégation de ''r'' sur ses attributs ''a_i'' ;
   * les ''p'' premières colonnes correspondent aux attributs ''a_i'' et les ''n-p+1'' dernières à la réduction de chaque groupe par les fonctions ''f_i'' de domaine ''d_i''
   *)
  val aggregate: attribute list -> (domain * (relation -> value option)) list -> relation -> relation

  (* ''from_file fname sep'' retourne la relation extraite du fichier ''fname'' au format CSV avec ''sep'' comme séparateur ;
     retourne également la liste d'association triée entre les noms des attributs et leurs positions
   *)
  val from_file: string -> char -> (string * attribute) list * relation

  (* ''to_file fname sep l r'' enregistre dans le fichier ''fname'' la relation ''r'' au format CSV avec ''sep'' comme séparateur ;
     la liste ''l'' permet de spécifier l'association entre numéro de colonnes et nom d'attribut si besoin
   *)
  val to_file: string -> char -> (attribute * string) list -> relation -> unit

  (* Comme ''to_file'' mais sur la sortie standard *)
  val print: char -> (attribute * string) list -> relation -> unit

end


(* Foncteur permettant de créer un module de manipulation à valeur dans un module V *)

module Make:
  functor (V: DATA) -> S with type value = V.value
                         and  type domain = V.domain
                         and  type attribute = int
