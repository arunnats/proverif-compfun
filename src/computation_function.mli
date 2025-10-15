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

(* Native computation function *)

val compfun_arity : computation_function
val compfun_is_fun : computation_function
val compfun_is_var : computation_function
val compfun_is_name : computation_function
val compfun_nth_arg : computation_function
val compfun_subterm : computation_function

(* Evaluation of computation term *)

val eval_computation_term : (int list -> term -> 'a) -> compterm -> 'a

(* Evaluation of computation term with position (for history reconstruction) *)

val eval_computation_term_at_position : int list -> compterm -> term

(* Evaluation of ground term *)

val eval_determinist_computation_function : computation_function -> term list -> term option
