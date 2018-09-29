open Env
open ModuleR

(*
  let r1 = R.selection (fun t -> match R.attribute (List.assoc "Région" vin_att) t with Some (VVChar "Bordeaux") -> true | _ -> false) vin in
  let r2 = R.selection (fun t -> match R.attribute 4 t with Some (VVChar "Alsace") -> true | _ -> false) vin in
  let r3 = R.projection [ DInt, R.attribute 0 ; DInt, R.attribute 2 ; DInt, R.attribute 4 ] commande in
  let r4 = R.projection [ DInt, R.attribute 3 ] vin in
  let r4' = R.distinct r4 in
  let r5 = R.union viticulteur client in
  let r5' = R.distinct r5 in
  let r6 = R.inter viticulteur client in
  let r7 = R.diff viticulteur client in
  let r8 = R.diff client viticulteur in
  let r9 = R.crossjoin client commande in
  let r10 = R.crossjoin viticulteur vin in
  let r11 = R.innerjoin (fun v c -> R.attribute 2 c = R.attribute 0 v && match R.attribute 4 c with Some (VInt q) when q >= 30 -> true | _ -> false) vin commande in
  let r12 = R.innerjoin (fun vit vin -> R.attribute 3 vit = R.attribute 4 vin) viticulteur vin in
  let r13 = R.innerjoin (fun c v -> R.attribute 3 c = R.attribute 4 v) client vin in
  let r14 = R.leftouterjoin (fun c v -> R.attribute 3 c = R.attribute 4 v) client vin in
  let r15 = R.fullouterjoin (fun c v -> R.attribute 3 c = R.attribute 4 v) client vin in
  let r16 = R.fullouterjoin (fun v c -> R.attribute 2 c = R.attribute 0 v) vin commande in
  let r17 = R.aggregate [ 0 ] [ DInt, R.sum false 9 ] r16 in () *)
  (* let _ = R.print '|' [] r17 in *)

    let _ =
      let vin_att,vin = R.from_file "vin.csv" '|' in
      let viticulteur_att, viticulteur = R.from_file "viticulteur.csv" '|' in
      let client_att, client = R.from_file "client.csv" '|' in
      let commande_att, commande = R.from_file "commande.csv" '|' in
      let env=empty in
      let env = (add "vin" (vin,(vin_att)) env) in
      let env = (add "viticulteur" (viticulteur,(viticulteur_att)) env) in
      let env = (add "client" (client,(client_att)) env) in
      let env = (add "commande" (commande,(commande_att)) env) in
      (* Ouverture un flot de caractère ; ici à partir de l'entrée standard *)
      let source = Lexing.from_channel stdin in
      (* Boucle infinie interompue par une exception correspondant à la fin de fichier *)
      let rec f () =
        try
          (* Récupération d'une expression à partir de la source puis affichage de l'évaluation *)
          let e = Parser.ansyn Lexer.anlex source in
           Printf.printf "\n-------------\n# %s # \n" (Ast.string_of_query e);
           Printf.printf "-------------\n\n";
           (R.print '|' [] (Ast.eval_query env e));
           Printf.printf "\n[sql-tool]=>";
           flush stdout;
          f ()
        with Lexer.Eof -> Printf.printf "Bye\n"
      in
      f ()
