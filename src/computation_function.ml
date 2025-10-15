(**************************************************************
 *                                                            *
 * This file is modified from ProVerif 2.04                   *
 *                                                            *
 * ProVerif 2.04 is by                                        *
 *  Bruno Blanchet, Vincent Cheval, and Marc Sylvestre        *
 *  Copyright (C) INRIA, CNRS 2000-2018                       *
 *                                                            *
 * The authors of the changes since ProVerif 2.04 are left    *
 * anonymous for submission to USENIX 2022                    *
 *                                                            *
 **************************************************************)

(*

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details (in file LICENSE).

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*)
open Parsing_helper
open Types
open Terms

(* Module that evaluates the computation function *)

(* Copy *)

(* Native computation function *)

let compfun_arity = { cf_id = "arity"; cf_ty = [Param.any_type],Param.int_type; cf_cat = CArity; cf_deterministic = true; cf_may_fail = false; cf_display = false }
let compfun_is_fun = { cf_id = "is_fun"; cf_ty = [Param.any_type],Param.bool_type; cf_cat = CIsFun; cf_deterministic = true; cf_may_fail = false; cf_display = false }
let compfun_is_var = { cf_id = "is_var"; cf_ty = [Param.any_type],Param.bool_type; cf_cat = CIsVar; cf_deterministic = true; cf_may_fail = false; cf_display = false }
let compfun_is_name = { cf_id = "is_name"; cf_ty = [Param.any_type],Param.bool_type; cf_cat = CIsName; cf_deterministic = true; cf_may_fail = false; cf_display = false }
let compfun_nth_arg = { cf_id = "nth_arg"; cf_ty = [Param.any_type;Param.int_type],Param.any_type; cf_cat = CNthArg; cf_deterministic = true; cf_may_fail = false; cf_display = false }

(* Subterm computation function *)

let compfun_subterm = { cf_id = "subterm"; cf_ty = [Param.any_type],Param.any_type; cf_cat = CUserDefined []; cf_deterministic = false; cf_may_fail = true; cf_display = false }
and compfun_subterm_args = { cf_id = "_subterm_args"; cf_ty = [Param.any_type;Param.int_type],Param.any_type; cf_cat = CUserDefined []; cf_deterministic = false; cf_may_fail = true; cf_display = false }

let rule_subterm1 =
  let x = Terms.new_var "x" Param.any_type in
  ([Var x],None,CVar x)

let rule_subterm2 =
  let x = Terms.new_var "x" Param.any_type in
  let is_fun = CCall(compfun_is_fun,[CVar x]) in
  let is_not_name = CNot(CCall(compfun_is_name,[CVar x])) in
  let cond = CBoolOp(BAnd,is_fun,is_not_name) in
  ([Var x],Some cond,CCall(compfun_subterm_args,[CVar x; CInt 0]))

let rule_subterm_args1 =
  let x = Terms.new_var "x" Param.any_type in
  let n = Terms.new_var "n" Param.int_type in
  let cond = CIntOp(ILeq,CIntOp(IPlus,CVar n,CInt 1),CCall(compfun_arity,[CVar x])) in
  let rhs = CCall(compfun_subterm,[CCall(compfun_nth_arg,[CVar x;CVar n])]) in
  ([Var x; Var n],Some cond,rhs)

let rule_subterm_args2 =
  let x = Terms.new_var "x" Param.any_type in
  let n = Terms.new_var "n" Param.int_type in
  let cond = CIntOp(ILeq,CIntOp(IPlus,CVar n,CInt 1),CCall(compfun_arity,[CVar x])) in
  let rhs = CCall(compfun_subterm_args,[CVar x;CIntOp(IPlus,CVar n,CInt 1)]) in
  ([Var x; Var n],Some cond,rhs)

let _ =
  compfun_subterm.cf_cat <- CUserDefined [rule_subterm1;rule_subterm2];
  compfun_subterm_args.cf_cat <- CUserDefined [rule_subterm_args1;rule_subterm_args2]

(* Evaluation of computation term *)

let int_of_res = function
  | CRInt n -> n
  | _ -> Parsing_helper.internal_error "[computation_functon.ml >> int_of_res] Incorrect type"

let term_of_res = function
  | CRTerm t -> t
  | _ -> Parsing_helper.internal_error "[computation_functon.ml >> term_of_res] Incorrect type"

let rec match_term_with_result pat res = match pat,res with
  | Var v, CRInt n ->
      begin match v.link with
        | NoLink -> link v (CLink res)
        | CLink (CRInt n') ->
            if n != n'
            then raise NoMatch
        | _ -> Parsing_helper.internal_error "[computation_function.ml >> match_term_with_result] Unexpected type."
      end
  | Var v, CRTerm t ->
      begin match v.link with
        | NoLink -> link v (CLink res)
        | CLink (CRTerm t') ->
            if not (Terms.equal_terms t t')
            then raise NoMatch
        | _ -> Parsing_helper.internal_error "[computation_function.ml >> match_term_with_result] Unexpected type (2)."
      end
  | _, CRInt _ -> Parsing_helper.internal_error "[computation_function.ml >> match_term_with_result] Unexpected type (3)."
  | _, CRTerm t -> match_term_with_term pat t

and match_term_with_term t1 t2 = match t1,t2 with
  | Var v, t ->
      begin match v.link with
        | NoLink -> link v (CLink (CRTerm t))
        | CLink (CRTerm t') -> if not (equal_terms t t') then raise NoMatch
        | _ -> Parsing_helper.internal_error "[computation_function.ml >> match_term_with_term] Bad link"
      end
  | FunApp(f1,args1), FunApp(f2,args2) ->
      if f1 != f2 then raise NoMatch;
      List.iter2 match_term_with_term args1 args2
  | _ -> raise NoMatch

let rec eval_determinist_compterm = function
  | CVar v ->
      if v.link <> NoLink
      then Parsing_helper.internal_error "[computation_function.ml >> eval_determinist_compterm] Unexpected link";
      CRTerm (Var v)
  | CFunApp(f,args) ->
      let args' = List.map (fun t -> term_of_res(eval_determinist_compterm t)) args in
      CRTerm(FunApp(f,args'))
  | CInt n -> CRInt n
  | CIntOp(op,t1,t2) ->
      let n1 = int_of_res (eval_determinist_compterm t1) in
      let n2 = int_of_res (eval_determinist_compterm t2) in
      begin match op with
        | IMinus -> CRInt(n1 - n2)
        | IPlus -> CRInt(n1 + n2)
        | IMult -> CRInt(n1 * n2)
        | IDiv -> CRInt(n1 / n2)
        | ILeq -> if n1 <= n2 then CRTerm(Terms.true_term) else CRTerm(Terms.false_term)
      end
  | CBoolOp((BEq|BDiff) as op,t1,t2) ->
      begin match eval_determinist_compterm t1, eval_determinist_compterm t2 with
        | CRInt(n1), CRInt(n2) ->
            if (n1 = n2 && op = BEq) || (n1 <> n2 && op = BDiff)
            then CRTerm(Terms.true_term)
            else CRTerm(Terms.false_term)
        | CRTerm t1', CRTerm t2' ->
            let eqt = Terms.equal_terms t1' t2' in
            if (eqt && op = BEq) || (not eqt && op = BDiff)
            then CRTerm(Terms.true_term)
            else CRTerm(Terms.false_term)
        | _ -> Parsing_helper.internal_error "[computation_function.ml >> eval_determinist_compterm] Type not matching"
      end
  | CBoolOp(BOr,t1,t2) ->
      let t1' = term_of_res (eval_determinist_compterm t1) in
      if Terms.equal_terms t1' Terms.true_term
      then CRTerm (Terms.true_term)
      else eval_determinist_compterm t2
  | CBoolOp(BAnd,t1,t2) ->
      let t1' = term_of_res (eval_determinist_compterm t1) in
      if Terms.equal_terms t1' Terms.true_term
      then eval_determinist_compterm t2
      else CRTerm (Terms.false_term)
  | CNot t ->
      let t' = term_of_res (eval_determinist_compterm t) in
      if Terms.equal_terms t' Terms.true_term
      then CRTerm (Terms.false_term)
      else CRTerm (Terms.true_term)
  | CTest(cond,tin,telse_op) ->
      let cond' = term_of_res (eval_determinist_compterm cond) in
      if Terms.equal_terms cond' Terms.true_term
      then eval_determinist_compterm tin
      else
        begin match telse_op with
          | None -> raise Terms.NoMatch
          | Some telse -> eval_determinist_compterm telse
        end
  | CLet(cpat,t,tin) ->
      let t' = eval_determinist_compterm t in
      let tin' =
        Terms.auto_cleanup (fun () ->
          match_term_with_result cpat t';
          copy_compterm3 tin
        )
      in
      eval_determinist_compterm tin'
  | CCall(compfun,args) ->
      if not compfun.cf_deterministic
      then Parsing_helper.internal_error "[computation_function.ml >> eval_determinist_compterm] Only deterministic function can be applied.";

      let args' = List.map eval_determinist_compterm args in
      if compfun.cf_may_fail
      then eval_and_display_deterministic_function compfun args'
      else
        try
          eval_and_display_deterministic_function compfun args'
        with NoMatch ->
          Display.Text.print_line (Printf.sprintf "The computation function %s is declared as unfailing but fails on the following call:" compfun.cf_id);
          Display.Text.print_string "   ";
          Display.Text.display_compfun_with_result_args compfun args';
          Display.Text.newline ();
          Display.Text.print_line (Printf.sprintf "Add the option [mayFail] to allow %s to fail." compfun.cf_id);
          Parsing_helper.user_error ("Failing function "^compfun.cf_id)

and eval_deterministic_function compfun args = match args, compfun.cf_cat with
  | [CRTerm(FunApp(_,l))], CArity -> CRInt(List.length l)
  | [CRTerm(FunApp _)], CIsFun -> CRTerm(Terms.true_term)
  | _, CIsFun -> CRTerm(Terms.false_term)
  | [CRTerm(Var _)], CIsVar -> CRTerm(Terms.true_term)
  | _, CIsVar -> CRTerm(Terms.false_term)
  | [CRTerm(FunApp({ f_cat = Name _;_},_))], CIsName -> CRTerm(Terms.true_term)
  | _, CIsName -> CRTerm(Terms.false_term)
  | [CRTerm(FunApp(_,l));CRInt n], CNthArg ->
      let ar = List.length l in
      if n < ar && n >= 0
      then CRTerm(List.nth l n)
      else raise Terms.NoMatch
  | args,CUserDefined rules -> eval_deterministic_user_defined args rules
  | _ -> raise Terms.NoMatch

and eval_deterministic_user_defined args = function
  | [] -> raise Terms.NoMatch
  | (cpat_l,cond_opt,res)::q ->
      let success_rule = ref false in
      try
        let (cpat_l1,cond_opt1,res1) =
          Terms.auto_cleanup (fun () ->
            List.map copy_term cpat_l,
            copy_compterm_opt cond_opt,
            copy_compterm res
          )
        in
        let (cond_opt2,res2) =
          Terms.auto_cleanup (fun () ->
            List.iter2 match_term_with_result cpat_l1 args;
            copy_compterm_opt3 cond_opt1,
            copy_compterm3 res1
          )
        in
        match cond_opt2 with
          | None ->
              success_rule := true;
              eval_determinist_compterm res2
          | Some cond ->
              let cond1 = term_of_res (eval_determinist_compterm cond) in
              if Terms.equal_terms cond1 Terms.true_term
              then
                begin
                  success_rule := true;
                  eval_determinist_compterm res2
                end
              else raise NoMatch
      with NoMatch ->
        if !success_rule
        then raise NoMatch
        else eval_deterministic_user_defined args q

and eval_and_display_deterministic_function compfun args =
  if compfun.cf_display
  then
    begin
      let res = eval_deterministic_function compfun args in
      Display.auto_cleanup_display (fun () ->
        Display.Text.print_string "- Evaluation of ";
        Display.Text.display_compfun_with_result_args compfun args;
        Display.Text.print_string "\n  Result: ";
        Display.Text.display_computation_result res;
        Display.Text.newline ();
        Display.Text.newline ()
      );
      res
    end
  else eval_deterministic_function compfun args

let rec eval_compterm f_next ct = match ct with
  | CVar _
  | CFunApp _
  | CInt _
  | CIntOp _
  | CBoolOp _
  | CNot _ ->
      let res = eval_determinist_compterm ct in
      f_next [] res
  | CTest(cond,tin,telse_op) ->
      let cond' = term_of_res (eval_determinist_compterm cond) in
      if Terms.equal_terms cond' Terms.true_term
      then eval_compterm f_next tin
      else
        begin match telse_op with
          | None -> raise NoMatch
          | Some telse -> eval_compterm f_next telse
        end
  | CLet(cpat,t,tin) ->
      let t' = eval_determinist_compterm t in
      let tin' =
        Terms.auto_cleanup (fun () ->
          match_term_with_result cpat t';
          copy_compterm3 tin
        )
      in
      eval_compterm f_next tin'
  | CCall(compfun,args) when compfun.cf_deterministic ->
      let res = eval_determinist_compterm ct in
      f_next [] res
  | CCall(compfun,args) ->
      let args' = List.map eval_determinist_compterm args in
      eval_non_deterministic_function f_next compfun args'

and eval_non_deterministic_function f_next compfun args = match args, compfun.cf_cat with
  | args,CUserDefined rules -> eval_user_defined f_next 0 args rules
  | _ -> Parsing_helper.internal_error "[computation_function.ml >> eval_non_deterministic_function] Unexpected case."

and eval_user_defined f_next i args = function
  | [] -> raise Terms.NoMatch
  | (cpat_l,cond_opt,res)::q ->
      try
        let (cpat_l1,cond_opt1,res1) =
          Terms.auto_cleanup (fun () ->
            List.map copy_term cpat_l,
            copy_compterm_opt cond_opt,
            copy_compterm res
          )
        in
        let (cond_opt2,res2) =
          Terms.auto_cleanup (fun () ->
            List.iter2 match_term_with_result cpat_l1 args;
            copy_compterm_opt3 cond_opt1,
            copy_compterm3 res1
          )
        in
        match cond_opt2 with
          | None -> eval_compterm (fun pos_l res -> f_next (i::pos_l) res) res2
          | Some cond ->
              let cond1 = term_of_res (eval_determinist_compterm cond) in
              if Terms.equal_terms cond1 Terms.true_term
              then eval_compterm (fun pos_l res -> f_next (i::pos_l) res) res2
              else raise NoMatch
      with NoMatch ->
        eval_user_defined f_next (i+1) args q

let eval_computation_term f_next ct =
  eval_compterm (fun pos_l res -> match res with
    | CRInt _ -> Parsing_helper.internal_error "[computation_function >> eval_computation_term] Should only return a term"
    | CRTerm t ->
        (* print_string "** Eval compterm : ";
        Display.Text.display_compterm ct;
        print_string "\n";
        print_string "Non deterministic result : ";
        Display.Text.display_term t;
        print_string "\n"; *)
        f_next pos_l t
  ) ct

(* Evaluation of computation term with position (For history reconstruction) *)

let rec eval_user_defined_at_position pos rest_pos i args = function
  | [] -> Parsing_helper.internal_error "[computation_function.ml >> eval_user_defined_at_position] Mismatch between the position and the number of rules."
  | _::q when i < pos -> eval_user_defined_at_position pos rest_pos (i+1) args q
  | (cpat_l,cond_opt,res)::q when i = pos ->
      begin
        try
          let (cpat_l1,cond_opt1,res1) =
            Terms.auto_cleanup (fun () ->
              List.map copy_term cpat_l,
              copy_compterm_opt cond_opt,
              copy_compterm res
            )
          in
          let (cond_opt2,res2) =
            Terms.auto_cleanup (fun () ->
              List.iter2 match_term_with_result cpat_l1 args;
              copy_compterm_opt3 cond_opt1,
              copy_compterm3 res1
            )
          in
          match cond_opt2 with
            | None -> eval_compterm_at_position rest_pos res2
            | Some cond ->
                let cond1 = term_of_res (eval_determinist_compterm cond) in
                if Terms.equal_terms cond1 Terms.true_term
                then eval_compterm_at_position rest_pos res2
                else raise NoMatch
        with NoMatch -> Parsing_helper.internal_error "Evaluation failure in history rebuilding"
      end
  | _ -> Parsing_helper.internal_error "[computation_function.ml >> eval_user_defined_at_position] Mismatch between the position and the number of rules (2)."

and eval_compterm_at_position posl ct = match ct with
  | CVar _
  | CFunApp _
  | CInt _
  | CIntOp _
  | CBoolOp _
  | CNot _ ->
      if posl <> []
      then Parsing_helper.internal_error "[computation_function.ml >> eval_compterm_at_position] When ct is determinist, the list of positition should be empty.";

      eval_determinist_compterm ct
  | CTest(cond,tin,telse_op) ->
      let cond' = term_of_res (eval_determinist_compterm cond) in
      if Terms.equal_terms cond' Terms.true_term
      then eval_compterm_at_position posl tin
      else
        begin match telse_op with
          | None -> Parsing_helper.internal_error "[computation_function.ml >> eval_compterm_at_position] The evalutation of the computation term should not fail.";
          | Some telse -> eval_compterm_at_position posl telse
        end
  | CLet(cpat,t,tin) ->
      let t' = eval_determinist_compterm t in
      let tin' =
        Terms.auto_cleanup (fun () ->
          match_term_with_result cpat t';
          copy_compterm3 tin
        )
      in
      eval_compterm_at_position posl tin'
  | CCall(compfun,args) when compfun.cf_deterministic ->
      if posl <> []
      then Parsing_helper.internal_error "[computation_function.ml >> eval_compterm_at_position] When ct is determinist, the list of positition should be empty (2).";

      eval_determinist_compterm ct
  | CCall(compfun,args) ->
      let args' = List.map eval_determinist_compterm args in
      eval_non_deterministic_function_at_position posl compfun args'

and eval_non_deterministic_function_at_position posl compfun args = match args, compfun.cf_cat with
  | args,CUserDefined rules ->
      if posl = []
      then Parsing_helper.internal_error "[computation_function.ml >> eval_non_deterministic_function_at_position] When ct is not determinist, the list of positition should not be empty.";

      eval_user_defined_at_position (List.hd posl) (List.tl posl) 0 args rules
  | _ -> Parsing_helper.internal_error "[computation_function.ml >> eval_non_deterministic_function] Unexpected case."

let eval_computation_term_at_position posl ct =
  try
    match eval_compterm_at_position posl ct with
      | CRInt _ -> Parsing_helper.internal_error "[computation_function.ml >> eval_computation_term_at_position] The result should not be an interger."
      | CRTerm t -> t
  with NoMatch ->
    Parsing_helper.internal_error "[computation_function.ml >> eval_computation_term_at_position] The evalutation of the computation term should not fail."

let eval_determinist_computation_function compfun args =
  let args' = List.map (fun t -> CRTerm t) args in
  try
    match eval_and_display_deterministic_function compfun args' with
      | CRTerm t -> Some t
      | CRInt _ -> Parsing_helper.internal_error "[Computation_function.ml >> eval_determinist_computation_function] Expecting a term"
  with NoMatch -> None
