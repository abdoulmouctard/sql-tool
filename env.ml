type 'a env = (string * 'a) list
 
let (empty: 'a env) = []
 
let rec find k env = match env with
  | [] -> None
  | (k',v) :: _ when k' = k -> Some v
  | _ :: env' -> find k env'
 
let rec add k v env = match env with
  | [] -> (k,v) :: env
  | (k',v') :: env' when k = k' -> (k,v) :: env'
  | (k',v') :: _ when k < k' -> (k,v) :: env
  | c :: env' -> c :: (add k v env')
 
let rec map f env = match env with
  | [] -> []
  | (k,v) :: env' -> (k,f v) :: (map f env')
 
let rec union env1 env2 = match env1, env2 with
  | [], _ -> env2
  | _, [] -> env1
  | (k1,v1) :: env1', (k2,v2) :: env2' ->
    if k1 = k2 then failwith "Env: union: ambiguity"
    else if k1 < k2 then (k1,v1) :: (union env1' env2)
    else (k2, v2) :: (union env1 env2')

