include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None


let free_vars =
  let rec go = function
    | TVar a -> VarSet.singleton a
    | TFun (t1, t2) -> VarSet.union (go t1) (go t2)
    | TList l -> go l
    | TOption o -> go o
    | TPair (t1, t2) -> VarSet.union (go t1) (go t2)
    | _ -> VarSet.empty
  in go

let ty_subst t a =
  let rec go = function
    | TVar b -> if a = b then t else TVar b
    | TFun (t1, t2) -> TFun (go t1, go t2)
    | TList l -> TList (go l)
    | TOption o -> TOption (go o)
    | TPair (t1, t2) -> TPair (go t1, go t2)
    | ty -> ty
  in go

let rec unify (sol : (string * ty) list) (constrs : constr list) : ((string * ty) list) option =
  let rec go = function
    | [] -> Some (List.rev sol)
    | (t1, t2) :: cs when t1 = t2 -> go cs
    | (TFun (s1, s2), TFun (t1, t2)) :: cs -> go ((s1, t1) :: (s2, t2) :: cs)
    | (TVar a, t) :: cs when not (VarSet.mem a (free_vars t)) ->
      let cs = List.map (fun (t1, t2) -> (ty_subst t a t1, ty_subst t a t2)) cs in
      unify ((a, t) :: sol) cs
    | (t, TVar a) :: cs -> go ((TVar a, t) :: cs)
    | (TList t1, TList t2) :: cs -> go ((t1, t2) :: cs)
    | (TOption t1, TOption t2) :: cs -> go ((t1, t2) :: cs)
    | (TPair (s1, s2), TPair (t1, t2)) :: cs -> go ((s1, t1) :: (s2, t2) :: cs)
    | _ -> None
  in go constrs 
let unify = unify []

let principle_type (ty : ty) (cs : constr list) : ty_scheme option = 
  match unify cs with
  | None -> None
  | Some sol ->
    let ty = List.fold_left (fun ty (a, t) -> ty_subst t a ty) ty sol in
    Some (Forall (free_vars ty, ty))


let env_add x ty = Env.add x (Forall (VarSet.empty, ty))

let instantiate (Forall (bvs, ty) : ty_scheme) : ty =
  VarSet.fold 
    (fun v acc -> ty_subst (TVar (gensym ())) v acc)
    bvs
    ty

let type_of (ctxt: stc_env) (e : expr) : ty_scheme option = 
  let rec type_of_helper (ctxt: stc_env) (e : expr) : ty * constr list =
    let rec go (e : expr) : ty * constr list =
      match e with
      | Unit -> TUnit, []
      | Bool _ -> TBool, []
      | Nil -> 
        let a = TVar (gensym ()) in
        (TList a), []
      | ENone -> 
        let a = (TVar (gensym ())) in
        (TOption a), []
      | Int _ -> TInt, []
      | Float _ -> TFloat, []
      | Var x ->
        (
          match Env.find_opt x ctxt with
          | Some ty -> (instantiate ty), []
          | None -> TInt, [TInt, TBool]
        )
      | Assert (Bool false) -> 
        let a = TVar (gensym ()) in
        a, []
      | Assert e -> 
        let t1, c1 = go e in
        TUnit, (t1, TBool) :: c1
      | ESome e -> 
        let t1, c1 = go e in
        (TOption t1), c1
      | If (e1, e2, e3) ->
        let t1, c1 = go e1 in
        let t2, c2 = go e2 in
        let t3, c3 = go e3 in
        t3, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3
      | ListMatch {matched=m; hd_name=h; tl_name=t_name; cons_case=con; nil_case=nil} ->
        let t, c = go m in
        let a = TVar (gensym ()) in
        let ctxt = env_add h a ctxt in
        let final_ctxt = env_add t_name (TList a) ctxt in
        let t1, c1 = type_of_helper final_ctxt con in
        let t2, c2 = go nil in
        t2, (t, TList a) :: (t1, t2) :: c @ c1 @ c2
      | OptMatch { matched=m; some_name=some_n; some_case=some_c; none_case=none_c} ->
        let t, c = go m in
        let a = TVar (gensym ()) in
        let t1, c1 = type_of_helper (env_add some_n a ctxt) some_c in
        let t2, c2 = go none_c in
        t2, (t, (TOption a)) :: (t1, t2) :: c @ c1 @ c2
      | PairMatch { matched=e; fst_name=x; snd_name=y; case=e'} -> 
        let t, c = go e in
        let a = TVar (gensym ()) in
        let b = TVar (gensym ()) in
        let ctxt = env_add x a ctxt in
        let final_ctxt = env_add y b ctxt in
        let t', c' = type_of_helper final_ctxt e' in
        t', (t, TPair (a, b)) :: c @ c'
      | Fun (x, t, e) ->
        (
          match t with
          | Some t -> 
            let ctxt = env_add x t ctxt in
            let t', c = type_of_helper ctxt e in
            (TFun (t, t')), c
          | None -> 
            let a = TVar (gensym ()) in
            let ctxt = env_add x a ctxt in
            let t', c = type_of_helper ctxt e in
            (TFun (a, t')), c
        )
      | App (e1, e2) -> 
        let t1, c1 = go e1 in
        let t2, c2 = go e2 in
        let a = TVar (gensym ()) in
        a, (t1, TFun (t2, a)) :: c1 @ c2
      | Annot (e, t) ->
        let t', c = go e in
        t, (t, t') :: c
      | Let { is_rec=is_rec; name=x; binding=e1; body=e2} -> 
        if is_rec then
          (* might have to confirm that e1 is an anon func *)
          let a = TVar (gensym ()) in
          let new_ctxt = env_add x a ctxt in
          let t1, c1 = type_of_helper new_ctxt e1 in
          let t2, c2 = type_of_helper new_ctxt e2 in
          t2, (t1, a) :: c1 @ c2
        else
          let t1, c1 = go e1 in
          let t2, c2 = type_of_helper (env_add x t1 ctxt) e2 in
          t2, c1 @ c2
      | Bop (op, e1, e2) -> 
        (
          match op with
          | Add -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2
          | Sub -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2
          | Mul -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2
          | Div -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2
          | Mod -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2
          | AddF -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TFloat, (t1, TFloat) :: (t2, TFloat) :: c1 @ c2
          | SubF -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TFloat, (t1, TFloat) :: (t2, TFloat) :: c1 @ c2
          | DivF -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TFloat, (t1, TFloat) :: (t2, TFloat) :: c1 @ c2
          | MulF -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TFloat, (t1, TFloat) :: (t2, TFloat) :: c1 @ c2
          | PowF -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TFloat, (t1, TFloat) :: (t2, TFloat) :: c1 @ c2
          | Lt -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TBool, (t1, t2) :: c1 @ c2
          | Lte -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TBool, (t1, t2) :: c1 @ c2
          | Gt -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TBool, (t1, t2) :: c1 @ c2
          | Gte -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TBool, (t1, t2) :: c1 @ c2
          | Eq -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TBool, (t1, t2) :: c1 @ c2
          | Neq -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TBool, (t1, t2) :: c1 @ c2
          | And -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TBool, (t1, TBool) :: (t2, TBool) :: c1 @ c2
          | Or -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            TBool, (t1, TBool) :: (t2, TBool) :: c1 @ c2
          | Comma -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            (TPair (t1, t2)), c1 @ c2
          | Cons -> 
            let t1, c1 = go e1 in
            let t2, c2 = go e2 in
            (TList t1), (t2, (TList t1)) :: c1 @ c2
        )
    in go e
  in let t1, c1 = type_of_helper ctxt e in
  principle_type t1 c1
  (* let rec go e = *)
    (* | Unit -> Some (Forall (VarSet.empty, TUnit)) *)
    (* | Unit -> 
      let t1, c1 = go_helper e in
      principle_type t1 c1
    | Bool _ -> principle_type TBool [] (* make sure it's supposed to be empty *)
    | Nil ->  
      let a = TVar (gensym ()) in
      principle_type (TList a) []
    | ENone -> 
      let a = (TVar (gensym ())) in
      principle_type (TOption a) []
      (* Some (Forall (VarSet.singleton (gensym ()), TOption _)) *)
    | Int _ -> principle_type TInt []
    | Float _ -> principle_type TFloat []
    | Var x -> 
      (
        match Env.find_opt x ctxt with
        (* | Some ty -> Some (Forall ((VarSet.empty), (instantiate ty))) *)
        | Some ty -> principle_type (instantiate ty) []
        (* | None -> Some (Forall ((VarSet.of_list [TInt, TBool]), TInt)) *)
        | None -> principle_type TInt [TInt, TBool]
      )
    | Assert (Bool false) -> 
      let a = TVar (gensym ()) in
      principle_type a []
    | Assert e ->
        (
          match go e with
          | Some (Forall (c1, t1)) -> 
            let c1 = VarSet.to_list c1 in
            principle_type TUnit ((t1, TBool) :: c1)
          | None -> None
        )
    | _ -> assert false  *)

  (* in go e *)

let is_well_typed (p : prog) : bool = 
  let rec type_check (ctxt : stc_env) (p : prog) : bool =
    match p with
    | [] -> true
    | {is_rec=is_rec;name=x;binding=e} :: p -> 
      (
        (* (
          match type_of ctxt e with
          | Some (Forall (c, t)) -> 
            (
              match unify c with
              | Some s ->
                (
                match principle_type t s with
                | Some ty -> type_check (Env.add x ty ctxt) p
                | None -> false
                )
              | _ -> false
            )
          | None -> false
        ) *)

         (* match type_of ctxt e with
         | Some t_s -> type_check (Env.add x t_s ctxt) p
         | None -> false *)
         
         if is_rec then
          let a = TVar (gensym ()) in
          let ctxt' = Env.add x (Forall (VarSet.empty, a)) ctxt in
          match type_of ctxt' e with
          | Some (Forall (_, t)) -> 
            (
              match unify [(t, a)] with
              | Some _ -> type_check (Env.add x (Forall (free_vars t, t)) ctxt) p
              | _ -> false
            )
          | None -> false
         else
          match type_of ctxt e with
         | Some (Forall(_, t)) -> type_check (Env.add x (Forall (free_vars t, t)) ctxt) p
         | None -> false
      )
  in type_check Env.empty p

exception AssertFail
exception DivByZero
exception CompareFunVals

let eval_expr (env : dyn_env) (e : expr) : value = 
  let rec eval (env : dyn_env) (e : expr) : value =
    let rec go e =
      match e with
      | Unit -> VUnit
      | Bool b -> VBool b
      | Nil -> VList []
      | ENone -> VNone
      | Int i -> VInt i
      | Float f -> VFloat f
      | Var x -> 
        (
          match Env.find_opt x env with
          | Some v -> v
          | _ -> assert false
        )
      | Assert e -> 
        (
          match go e with
          | VBool true -> VUnit
          | exception exc -> raise exc
          | _ -> raise AssertFail
        )
      | ESome e -> VSome (go e)
      | If (e1, e2, e3) -> 
        (
          match go e1 with
          | VBool b -> 
            if b
              then go e2
            else
              go e3
          | exception exc -> raise exc
          | _ -> assert false
        )
      | ListMatch {matched=e; hd_name=h; tl_name=t; cons_case=e1; nil_case=e2} -> 
        (
          match go e with
          | VList [] -> go e2
          | VList (v1 :: v2) -> 
            let env = Env.add h v1 env in
            let env' = Env.add t (VList v2) env in
            eval env' e1
          | exception e -> raise e 
          | _ -> assert false
        )
      | OptMatch { matched=e; some_name=x; some_case=e1; none_case=e2} -> 
        (
          match go e with
          | VSome v -> 
            let env = Env.add x v env in
            eval env e1
          | VNone -> go e2
          | exception exc -> raise exc
          | _ -> assert false
        )
      | PairMatch { matched=e; fst_name=x; snd_name=y; case=e'} -> 
        (
          match go e with
          | VPair (v1, v2) -> 
            let env = Env.add x v1 env in
            let env' = Env.add y v2 env in
            eval env' e'
          | exception exc -> raise exc
          | _ -> assert false
        )
      | Fun (x, _, e) -> VClos {name=None; arg=x; body=e; env=env}
      | App (e1, e2) -> 
        (
          match go e1 with
          | VClos {name=None; arg=x;body=e;env=env'} -> 
            (
              match go e2 with
              | v2 -> eval (Env.add x v2 env') e
            )
          | VClos {name=Some f; arg=x;body=e;env=env'} -> 
            (
              match go e2 with
              | v2 ->
                let env' = Env.add x v2 env' in
                let env' = Env.add f (VClos {name=Some f; arg=x;body=e;env=env'}) env' in
                eval env' e
            )
          | exception exc -> raise exc
          | _ -> assert false
        )
      | Annot (e, _) -> go e
      | Let { is_rec=is_rec; name=x; binding=e1; body=e2} -> 
        if is_rec then
          (* might need to instead match in go e1 and see if it matches with an exception *)
          (* let v_clos = go e1 in
          let env' = Env.add x v_clos env in
          eval env' e2 *)
          (* (
            match go e1 with
            | VClos {name=_; arg=arg;body=body;env=env} -> 
              let env' = Env.add x (Fun (arg, None, body)) env in
              eval env' e2
            | _ -> assert false
          ) *)
           (
            match eval env e1 with
            | VClos {name=_; arg=arg;body=body;env=env'} -> 
              let new_env = Env.add x (VClos {name=Some x; arg=arg;body=body; env=env'}) env' in
              let clos = VClos {name=Some x; arg=arg; body=body; env=new_env} in
              eval (Env.add x clos env) e2
            | _ -> assert false
           )
        else
          let v1 = go e1 in
          let env' = Env.add x v1 env in
          eval env' e2
      | Bop (op, e1, e2) -> 
        (
          match op with
          | Add -> 
            (
              match go e1 with
              | VInt m -> 
                (
                  match go e2 with
                  | VInt n -> VInt (m + n)
                  | exception exc -> raise exc
                  | _ -> assert false
                )
              | exception exc -> raise exc
              | _ -> assert false
            )
          | Sub -> 
            (
              match go e1 with
              | VInt m -> 
                (
                  match go e2 with
                  | VInt n -> VInt (m - n)
                  | exception exc -> raise exc
                  | _ -> assert false
                )
              | exception exc -> raise exc
              | _ -> assert false
            )
          | Mul -> 
            (
              match go e1 with
              | VInt m -> 
                (
                  match go e2 with
                  | VInt n -> VInt (m * n)
                  | exception exc -> raise exc
                  | _ -> assert false
                )
              | exception exc -> raise exc
              | _ -> assert false
            )
          | Div -> 
            (
              match go e1 with
              | VInt m -> 
                (
                  match go e2 with
                  | VInt n -> 
                    if n = 0 
                      then raise DivByZero
                    else
                      VInt (m / n)
                  | exception exc -> raise exc
                  | _ -> assert false
                )
              | exception exc -> raise exc
              | _ -> assert false
            )
          | Mod -> 
            (
              match go e1 with
              | VInt m -> 
                (
                  match go e2 with
                  | VInt n -> 
                    if n = 0 
                      then raise DivByZero
                    else
                      VInt (m mod n)
                  | exception exc -> raise exc
                  | _ -> assert false
                )
              | exception exc -> raise exc
              | _ -> assert false
            )
          | AddF -> 
            (
              match go e1 with
              | VFloat m -> 
                (
                  match go e2 with
                  | VFloat n -> VFloat (m +. n)
                  | exception exc -> raise exc
                  | _ -> assert false
                )
              | exception exc -> raise exc
              | _ -> assert false
            )
          | SubF -> 
            (
              match go e1 with
              | VFloat m -> 
                (
                  match go e2 with
                  | VFloat n -> VFloat (m -. n)
                  | exception exc -> raise exc
                  | _ -> assert false
                )
              | exception exc -> raise exc
              | _ -> assert false
            )
          | MulF -> 
            (
              match go e1 with
              | VFloat m -> 
                (
                  match go e2 with
                  | VFloat n -> VFloat (m *. n)
                  | exception exc -> raise exc
                  | _ -> assert false
                )
              | exception exc -> raise exc
              | _ -> assert false
            )
          | DivF -> 
            (
              match go e1 with
              | VFloat m -> 
                (
                  match go e2 with
                  | VFloat n -> 
                    if n = 0.0 
                      then raise DivByZero
                    else
                      VFloat (m /. n)
                  | exception exc -> raise exc
                  | _ -> assert false
                )
              | exception exc -> raise exc
              | _ -> assert false
            )
          | PowF -> 
            (
              match go e1 with
              | VFloat m -> 
                (
                  match go e2 with
                  | VFloat n -> VFloat (m ** n)
                  | exception exc -> raise exc
                  | _ -> assert false
                )
              | exception exc -> raise exc
              | _ -> assert false
            )
          | Lt -> 
            (
              (* match go e1 with
              | VInt n -> 
                (
                  match go e2 with
                  | VInt m -> VBool (n < m)
                  | VFloat m -> VBool (float n < m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VFloat n -> 
                (
                  match go e2 with
                  | VFloat m -> VBool (n < m)
                  | VInt m -> VBool (n < float m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VClos _ -> raise CompareFunVals
              | _ -> assert false *)
              match go e1 with
              | VClos _ -> raise CompareFunVals
              | v1 -> 
                (
                  match go e2 with
                  | VClos _ -> raise CompareFunVals
                  | v2 -> VBool (v1 < v2)
                )
            )
          | Lte -> 
            (* (
              match go e1 with
              | VInt n -> 
                (
                  match go e2 with
                  | VInt m -> VBool (n <= m)
                  | VFloat m -> VBool (float n <= m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VFloat n -> 
                (
                  match go e2 with
                  | VFloat m -> VBool (n <= m)
                  | VInt m -> VBool (n < float m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VClos _ -> raise CompareFunVals
              | _ -> assert false
            ) *)
             (
              match go e1 with
              | VClos _ -> raise CompareFunVals
              | v1 -> 
                (
                  match go e2 with
                  | VClos _ -> raise CompareFunVals
                  | v2 -> VBool (v1 <= v2)
                )
             )
          | Gt -> 
            (* (
              match go e1 with
              | VInt n -> 
                (
                  match go e2 with
                  | VInt m -> VBool (n > m)
                  | VFloat m -> VBool (float n > m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VFloat n -> 
                (
                  match go e2 with
                  | VFloat m -> VBool (n > m)
                  | VInt m -> VBool (n < float m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VClos _ -> raise CompareFunVals
              | _ -> assert false
            ) *)
             (
              match go e1 with
              | VClos _ -> raise CompareFunVals
              | v1 -> 
                (
                  match go e2 with
                  | VClos _ -> raise CompareFunVals
                  | v2 -> VBool (v1 > v2)
                )
             )
          | Gte -> 
            (* (
              match go e1 with
              | VInt n -> 
                (
                  match go e2 with
                  | VInt m -> VBool (n >= m)
                  | VFloat m -> VBool (float n >= m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VFloat n -> 
                (
                  match go e2 with
                  | VFloat m -> VBool (n >= m)
                  | VInt m -> VBool (n >= float m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VClos _ -> raise CompareFunVals
              | _ -> assert false
            ) *)
             (
              match go e1 with
              | VClos _ -> raise CompareFunVals
              | v1 -> 
                (
                  match go e2 with
                  | VClos _ -> raise CompareFunVals
                  | v2 -> VBool (v1 >= v2)
                )
             )
          | Eq -> 
            (* (
              match go e1 with
              | VInt n -> 
                (
                  match go e2 with
                  | VInt m -> VBool (n = m)
                  | VFloat m -> VBool (float n = m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VFloat n -> 
                (
                  match go e2 with
                  | VFloat m -> VBool (n = m)
                  | VInt m -> VBool (n = float m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VClos _ -> raise CompareFunVals
              | _ -> assert false
            ) *)
             (
              match go e1 with
              | VClos _ -> raise CompareFunVals
              | v1 -> 
                (
                  match go e2 with
                  | VClos _ -> raise CompareFunVals
                  | v2 -> VBool (v1 = v2)
                )
             )
          | Neq -> 
            (* (
              match go e1 with
              | VInt n -> 
                (
                  match go e2 with
                  | VInt m -> VBool (n <> m)
                  | VFloat m -> VBool (float n <> m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VFloat n -> 
                (
                  match go e2 with
                  | VFloat m -> VBool (n <> m)
                  | VInt m -> VBool (n <> float m)
                  | VClos _ -> raise CompareFunVals
                  | _ -> assert false
                )
              | VClos _ -> raise CompareFunVals
              | _ -> assert false
            ) *)
             (
              match go e1 with
              | VClos _ -> raise CompareFunVals
              | v1 -> 
                (
                  match go e2 with
                  | VClos _ -> raise CompareFunVals
                  | v2 -> VBool (v1 <> v2)
                )
             )
          | And -> 
            (
              match go e1 with
              | VBool false -> VBool false
              | VBool true -> 
                (
                  match go e2 with
                  | VClos _ -> raise CompareFunVals
                  | v -> v
                )
              | VClos _ -> raise CompareFunVals
              | _ -> assert false
            )
          | Or -> 
            (
              match go e1 with
              | VBool true -> VBool true
              | VBool false -> 
                (
                  match go e2 with
                  | VClos _ -> raise CompareFunVals
                  | v -> v
                )
              | VClos _ -> raise CompareFunVals
              | _ -> assert false
            )
          | Comma -> VPair (go e1, go e2)
          | Cons -> 
            (
              match go e1 with
              | v -> 
                (
                  match go e2 with
                  | VList rest -> VList (v :: rest)
                  (* | exception exc -> raise exc *)
                  | _ -> assert false
                )
            )
        )

    in go e
  in eval env e

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;binding}] -> Let {is_rec;name;binding;body = Var name}
    | {is_rec;name;binding} :: ls -> Let {is_rec;name;binding;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog ->
    if is_well_typed prog
    then Ok (eval prog)
    else Error TypeError
  | None -> Error ParseError
