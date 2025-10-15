type token =
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | SEMI
  | COLON
  | IDENT of (Ptree.ident)
  | INT of (int)
  | RED
  | EQUIV
  | EQUIVEQ
  | EQUAL
  | FUN
  | EQUATION
  | QUERY
  | NOUNIF
  | SLASH
  | STAR
  | DOT
  | WEDGE
  | EOF
  | NOT
  | ELIMTRUE
  | DIFF
  | PREDICATE
  | REDUCTION
  | DATA
  | PARAM
  | CLAUSES
  | CONST
  | SET
  | NAME
  | TYPE
  | FORALL
  | SELECT
  | MINUS

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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
# 34 "parser.mly"

open Parsing_helper
open Types
open Ptree
exception Syntax

# 81 "parser.ml"
let yytransl_const = [|
  257 (* COMMA *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACKET *);
  261 (* RBRACKET *);
  262 (* SEMI *);
  263 (* COLON *);
  266 (* RED *);
  267 (* EQUIV *);
  268 (* EQUIVEQ *);
  269 (* EQUAL *);
  270 (* FUN *);
  271 (* EQUATION *);
  272 (* QUERY *);
  273 (* NOUNIF *);
  274 (* SLASH *);
  275 (* STAR *);
  276 (* DOT *);
  277 (* WEDGE *);
    0 (* EOF *);
  278 (* NOT *);
  279 (* ELIMTRUE *);
  280 (* DIFF *);
  281 (* PREDICATE *);
  282 (* REDUCTION *);
  283 (* DATA *);
  284 (* PARAM *);
  285 (* CLAUSES *);
  286 (* CONST *);
  287 (* SET *);
  288 (* NAME *);
  289 (* TYPE *);
  290 (* FORALL *);
  291 (* SELECT *);
  292 (* MINUS *);
    0|]

let yytransl_block = [|
  264 (* IDENT *);
  265 (* INT *);
    0|]

let yylhs = "\255\255\
\003\000\003\000\003\000\003\000\005\000\005\000\004\000\004\000\
\006\000\006\000\007\000\007\000\007\000\007\000\007\000\009\000\
\009\000\008\000\008\000\010\000\010\000\011\000\011\000\012\000\
\012\000\013\000\013\000\013\000\013\000\014\000\014\000\015\000\
\015\000\016\000\017\000\017\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\018\000\
\018\000\018\000\019\000\019\000\019\000\020\000\020\000\020\000\
\021\000\021\000\021\000\021\000\022\000\022\000\023\000\024\000\
\024\000\025\000\025\000\026\000\026\000\027\000\027\000\028\000\
\028\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\000\000\000\000"

let yylen = "\002\000\
\004\000\003\000\004\000\001\000\003\000\001\000\001\000\000\000\
\003\000\001\000\004\000\003\000\004\000\001\000\002\000\003\000\
\001\000\001\000\000\000\002\000\000\000\001\000\000\000\003\000\
\003\000\003\000\001\000\003\000\003\000\003\000\001\000\003\000\
\002\000\003\000\003\000\005\000\006\000\006\000\004\000\004\000\
\005\000\004\000\004\000\007\000\006\000\006\000\003\000\002\000\
\003\000\000\000\002\000\003\000\000\000\004\000\001\000\003\000\
\003\000\001\000\003\000\003\000\003\000\001\000\004\000\005\000\
\003\000\003\000\000\000\004\000\003\000\003\000\000\000\004\000\
\006\000\004\000\006\000\010\000\007\000\005\000\006\000\004\000\
\007\000\005\000\007\000\005\000\006\000\004\000\006\000\004\000\
\008\000\005\000\006\000\006\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\094\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\095\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\033\000\000\000\000\000\000\000\047\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\093\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
\000\000\039\000\024\000\025\000\040\000\000\000\000\000\000\000\
\000\000\034\000\018\000\020\000\000\000\042\000\043\000\000\000\
\000\000\030\000\032\000\026\000\028\000\029\000\000\000\000\000\
\000\000\022\000\000\000\000\000\070\000\000\000\000\000\000\000\
\000\000\056\000\080\000\000\000\000\000\048\000\000\000\000\000\
\000\000\000\000\086\000\000\000\088\000\000\000\000\000\000\000\
\066\000\000\000\000\000\069\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\074\000\051\000\000\000\000\000\000\000\
\000\000\005\000\001\000\003\000\000\000\000\000\000\000\000\000\
\015\000\000\000\041\000\000\000\000\000\000\000\000\000\000\000\
\009\000\000\000\078\000\000\000\000\000\000\000\063\000\049\000\
\082\000\000\000\000\000\000\000\000\000\090\000\000\000\061\000\
\068\000\057\000\059\000\060\000\000\000\000\000\000\000\000\000\
\052\000\084\000\000\000\037\000\036\000\012\000\000\000\000\000\
\016\000\000\000\038\000\045\000\046\000\000\000\000\000\064\000\
\079\000\000\000\085\000\087\000\000\000\000\000\091\000\092\000\
\075\000\000\000\011\000\013\000\044\000\000\000\000\000\081\000\
\000\000\077\000\083\000\000\000\073\000\089\000\000\000\076\000"

let yydgoto = "\003\000\
\014\000\028\000\052\000\074\000\075\000\162\000\145\000\146\000\
\147\000\085\000\163\000\042\000\043\000\044\000\045\000\038\000\
\033\000\110\000\130\000\053\000\122\000\123\000\056\000\054\000\
\064\000\065\000\050\000\102\000"

let yysindex = "\105\000\
\177\255\193\255\000\000\000\255\096\255\101\255\021\255\101\255\
\101\255\040\255\101\255\052\255\067\255\000\000\076\255\041\255\
\103\255\085\255\103\255\103\255\112\255\089\255\127\255\141\255\
\160\255\165\255\085\255\000\000\158\255\096\255\130\255\167\255\
\170\255\143\255\159\255\200\255\180\255\188\255\210\255\225\255\
\195\255\222\255\061\255\148\255\254\000\242\255\248\255\004\000\
\002\000\089\255\162\255\240\255\250\255\009\000\030\255\254\255\
\011\000\255\255\012\000\003\000\014\000\144\255\016\000\122\255\
\022\001\018\000\013\000\020\000\008\000\015\000\023\000\021\000\
\030\000\029\000\000\000\096\255\096\255\096\255\177\255\096\255\
\096\255\177\255\063\255\026\000\017\000\177\255\177\255\027\000\
\101\255\101\255\000\000\101\255\101\255\101\255\000\000\031\000\
\133\255\002\000\033\000\034\000\096\255\022\000\096\255\035\000\
\096\255\193\255\122\255\063\255\010\255\024\000\037\000\193\255\
\122\255\193\255\122\255\002\000\028\000\039\000\032\000\163\255\
\036\000\074\255\186\255\000\000\041\000\244\255\042\000\193\255\
\014\255\038\000\037\000\040\000\096\255\000\000\044\000\046\000\
\047\000\000\000\000\000\000\000\000\000\063\255\175\255\048\000\
\051\000\000\000\000\000\000\000\177\255\000\000\000\000\002\000\
\222\255\000\000\000\000\000\000\000\000\000\000\043\000\045\000\
\049\000\000\000\052\000\002\000\000\000\053\000\193\255\056\000\
\060\000\000\000\000\000\050\000\059\000\000\000\055\000\193\255\
\065\000\254\255\000\000\054\000\000\000\057\000\068\000\193\255\
\000\000\122\255\089\255\000\000\122\255\122\255\122\255\041\255\
\058\000\061\000\062\000\000\000\000\000\063\000\193\255\015\000\
\177\255\000\000\000\000\000\000\096\255\070\000\063\255\063\255\
\000\000\063\255\000\000\064\000\177\255\177\255\177\255\069\000\
\000\000\096\255\000\000\000\000\016\000\193\255\000\000\000\000\
\000\000\066\000\193\255\193\255\041\255\000\000\036\000\000\000\
\000\000\000\000\000\000\000\000\067\000\193\255\193\255\193\255\
\000\000\000\000\071\000\000\000\000\000\000\000\072\000\074\000\
\000\000\177\255\000\000\000\000\000\000\075\000\079\000\000\000\
\000\000\193\255\000\000\000\000\073\000\193\255\000\000\000\000\
\000\000\193\255\000\000\000\000\000\000\041\255\089\255\000\000\
\193\255\000\000\000\000\076\000\000\000\000\000\193\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\019\255\
\000\000\000\000\000\000\000\000\000\000\123\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\077\000\226\255\000\000\
\000\000\078\000\000\000\000\000\000\000\080\000\000\000\000\000\
\000\000\245\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\123\255\071\255\000\000\000\000\000\000\000\000\081\000\
\000\000\000\000\000\000\000\000\000\000\083\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\084\000\000\000\000\000\
\058\255\000\000\000\000\077\000\085\000\000\000\000\000\238\255\
\000\000\000\000\199\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\086\000\046\255\000\000\000\000\000\000\077\000\000\000\
\000\000\000\000\000\000\089\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\086\000\000\000\000\000\000\000\116\255\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\087\000\000\000\000\000\000\000\000\000\089\000\037\255\000\000\
\038\255\000\000\000\000\000\000\000\000\000\000\000\000\088\000\
\223\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\082\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\081\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\123\255\000\000\000\000\000\000\000\000\083\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\084\000\
\000\000\000\000\000\000\000\000\000\000\000\000\089\000\090\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\151\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\083\000\000\000\230\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\091\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\083\000\123\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\194\255\172\255\252\255\215\255\164\000\213\255\000\000\161\255\
\100\000\000\000\156\255\025\000\000\000\235\000\004\001\000\000\
\148\000\176\000\155\000\251\255\000\000\175\000\241\255\247\255\
\206\255\178\000\197\255\095\000"

let yytablesize = 367
let yytable = "\101\000\
\032\000\035\000\117\000\035\000\035\000\100\000\035\000\029\000\
\057\000\059\000\061\000\070\000\173\000\058\000\060\000\183\000\
\138\000\071\000\174\000\141\000\071\000\171\000\197\000\150\000\
\151\000\073\000\071\000\179\000\037\000\181\000\036\000\108\000\
\039\000\040\000\135\000\136\000\104\000\014\000\139\000\014\000\
\017\000\014\000\017\000\196\000\049\000\175\000\206\000\041\000\
\010\000\198\000\010\000\212\000\071\000\119\000\014\000\017\000\
\014\000\017\000\121\000\046\000\006\000\168\000\006\000\006\000\
\142\000\010\000\090\000\006\000\006\000\006\000\143\000\073\000\
\073\000\137\000\047\000\073\000\140\000\006\000\006\000\187\000\
\091\000\144\000\219\000\048\000\035\000\035\000\211\000\035\000\
\035\000\035\000\055\000\225\000\055\000\188\000\004\000\178\000\
\166\000\030\000\073\000\230\000\170\000\172\000\030\000\031\000\
\030\000\001\000\002\000\180\000\034\000\182\000\051\000\247\000\
\248\000\153\000\242\000\200\000\156\000\157\000\158\000\062\000\
\217\000\055\000\063\000\030\000\067\000\055\000\055\000\055\000\
\073\000\120\000\067\000\076\000\237\000\077\000\066\000\055\000\
\055\000\001\001\244\000\004\000\160\000\161\000\003\001\004\001\
\076\000\116\000\077\000\049\000\067\000\080\000\251\000\252\000\
\253\000\007\001\008\001\009\001\054\000\092\000\093\000\094\000\
\054\000\054\000\054\000\103\000\103\000\077\000\077\000\068\000\
\104\000\005\001\054\000\054\000\069\000\016\001\001\000\072\000\
\207\000\018\001\208\000\078\000\231\000\019\001\081\000\234\000\
\235\000\236\000\083\000\013\001\022\001\079\000\004\000\005\000\
\006\000\007\000\024\001\189\000\190\000\191\000\008\000\009\000\
\032\000\010\000\011\000\012\000\013\000\084\000\015\000\016\000\
\017\000\018\000\020\001\000\001\088\000\255\000\019\000\020\000\
\019\000\021\000\019\000\082\000\101\000\022\000\023\000\024\000\
\025\000\026\000\004\000\027\000\004\000\086\000\004\000\004\000\
\031\000\031\000\031\000\004\000\004\000\004\000\004\000\062\000\
\062\000\062\000\089\000\008\000\087\000\004\000\004\000\008\000\
\008\000\008\000\027\000\193\000\194\000\095\000\031\000\031\000\
\031\000\008\000\008\000\096\000\097\000\098\000\058\000\105\000\
\027\000\099\000\062\000\062\000\062\000\106\000\107\000\109\000\
\111\000\113\000\112\000\115\000\058\000\124\000\114\000\118\000\
\125\000\126\000\127\000\128\000\131\000\132\000\133\000\134\000\
\129\000\164\000\148\000\152\000\149\000\185\000\165\000\159\000\
\202\000\167\000\169\000\176\000\177\000\104\000\203\000\184\000\
\192\000\195\000\204\000\210\000\205\000\249\000\216\000\209\000\
\186\000\199\000\220\000\201\000\221\000\223\000\213\000\224\000\
\214\000\218\000\108\000\154\000\215\000\222\000\229\000\241\000\
\246\000\227\000\011\001\254\000\228\000\238\000\012\001\008\000\
\239\000\240\000\014\001\250\000\015\001\002\001\006\001\065\000\
\023\000\008\000\010\001\019\000\017\001\155\000\019\000\023\001\
\245\000\226\000\243\000\021\000\050\000\004\000\071\000\053\000\
\232\000\000\000\035\000\023\000\233\000\021\001\072\000"

let yycheck = "\050\000\
\005\000\006\000\062\000\008\000\009\000\049\000\011\000\008\001\
\018\000\019\000\020\000\027\000\108\000\019\000\020\000\116\000\
\079\000\027\000\009\001\082\000\002\001\106\000\009\001\086\000\
\087\000\030\000\008\001\112\000\008\001\114\000\006\000\002\001\
\008\000\009\000\076\000\077\000\007\001\001\001\080\000\003\001\
\003\001\005\001\005\001\128\000\004\001\036\001\142\000\008\001\
\003\001\036\001\005\001\152\000\034\001\063\000\018\001\018\001\
\020\001\020\001\064\000\008\001\003\001\103\000\005\001\006\001\
\002\001\020\001\006\001\010\001\011\001\012\001\008\001\076\000\
\077\000\078\000\008\001\080\000\081\000\020\001\021\001\006\001\
\020\001\019\001\167\000\008\001\089\000\090\000\149\000\092\000\
\093\000\094\000\020\001\176\000\008\001\020\001\024\001\111\000\
\101\000\002\001\103\000\184\000\105\000\107\000\002\001\008\001\
\002\001\001\000\002\000\113\000\008\001\115\000\008\001\207\000\
\208\000\089\000\199\000\131\000\092\000\093\000\094\000\008\001\
\164\000\006\001\034\001\002\001\002\001\010\001\011\001\012\001\
\133\000\008\001\008\001\002\001\192\000\004\001\008\001\020\001\
\021\001\222\000\201\000\024\001\008\001\009\001\227\000\228\000\
\002\001\002\001\004\001\004\001\008\001\007\001\213\000\214\000\
\215\000\238\000\239\000\240\000\006\001\010\001\011\001\012\001\
\010\001\011\001\012\001\002\001\002\001\004\001\004\001\008\001\
\007\001\229\000\020\001\021\001\008\001\002\001\024\001\018\001\
\002\001\006\001\004\001\013\001\186\000\010\001\024\001\189\000\
\190\000\191\000\007\001\250\000\017\001\020\001\014\001\015\001\
\016\001\017\001\023\001\010\001\011\001\012\001\022\001\023\001\
\205\000\025\001\026\001\027\001\028\001\018\001\014\001\015\001\
\016\001\017\001\014\001\221\000\018\001\218\000\022\001\023\001\
\018\001\025\001\020\001\020\001\015\001\029\001\030\001\031\001\
\032\001\033\001\001\001\035\001\003\001\020\001\005\001\006\001\
\010\001\011\001\012\001\010\001\011\001\012\001\013\001\010\001\
\011\001\012\001\021\001\006\001\020\001\020\001\021\001\010\001\
\011\001\012\001\006\001\008\001\009\001\000\000\010\001\011\001\
\012\001\020\001\021\001\018\001\013\001\002\001\006\001\024\001\
\020\001\008\001\010\001\011\001\012\001\020\001\006\001\018\001\
\006\001\006\001\020\001\006\001\020\001\000\000\020\001\008\001\
\007\001\013\001\007\001\020\001\006\001\009\001\001\001\003\001\
\018\001\001\001\009\001\009\001\020\001\006\001\005\001\009\001\
\133\000\020\001\008\001\020\001\008\001\007\001\003\001\020\001\
\008\001\008\001\005\001\001\001\006\001\210\000\003\001\008\001\
\021\001\020\001\003\001\020\001\001\001\003\001\020\001\009\001\
\020\001\013\001\002\001\089\000\020\001\020\001\003\001\009\001\
\003\001\020\001\003\001\007\001\020\001\020\001\005\001\003\001\
\020\001\020\001\008\001\020\001\006\001\020\001\020\001\006\001\
\003\001\005\001\020\001\003\001\020\001\090\000\005\001\020\001\
\205\000\178\000\200\000\020\001\020\001\024\001\020\001\020\001\
\186\000\255\255\020\001\020\001\187\000\015\001\020\001"

let yynames_const = "\
  COMMA\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
  SEMI\000\
  COLON\000\
  RED\000\
  EQUIV\000\
  EQUIVEQ\000\
  EQUAL\000\
  FUN\000\
  EQUATION\000\
  QUERY\000\
  NOUNIF\000\
  SLASH\000\
  STAR\000\
  DOT\000\
  WEDGE\000\
  EOF\000\
  NOT\000\
  ELIMTRUE\000\
  DIFF\000\
  PREDICATE\000\
  REDUCTION\000\
  DATA\000\
  PARAM\000\
  CLAUSES\000\
  CONST\000\
  SET\000\
  NAME\000\
  TYPE\000\
  FORALL\000\
  SELECT\000\
  MINUS\000\
  "

let yynames_block = "\
  IDENT\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'termseq) in
    Obj.repr(
# 99 "parser.mly"
 ( PFunApp (_1, _3) )
# 424 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'termseq) in
    Obj.repr(
# 101 "parser.mly"
 ( PTuple _2 )
# 431 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'termseq) in
    Obj.repr(
# 103 "parser.mly"
 ( PName(_1, _3) )
# 439 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ptree.ident) in
    Obj.repr(
# 105 "parser.mly"
 ( PIdent (_1) )
# 446 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'netermseq) in
    Obj.repr(
# 109 "parser.mly"
 ( _1 :: _3 )
# 454 "parser.ml"
               : 'netermseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 111 "parser.mly"
 ( [_1] )
# 461 "parser.ml"
               : 'netermseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'netermseq) in
    Obj.repr(
# 115 "parser.mly"
        ( _1 )
# 468 "parser.ml"
               : 'termseq))
; (fun __caml_parser_env ->
    Obj.repr(
# 117 "parser.mly"
 ( [] )
# 474 "parser.ml"
               : 'termseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'neidentseq) in
    Obj.repr(
# 121 "parser.mly"
        ( _1 :: _3 )
# 482 "parser.ml"
               : 'neidentseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ptree.ident) in
    Obj.repr(
# 123 "parser.mly"
 ( [_1] )
# 489 "parser.ml"
               : 'neidentseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'formatseq) in
    Obj.repr(
# 127 "parser.mly"
 ( PFFunApp (_1, _3) )
# 497 "parser.ml"
               : 'format))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formatseq) in
    Obj.repr(
# 129 "parser.mly"
 ( PFTuple _2 )
# 504 "parser.ml"
               : 'format))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'formatseq) in
    Obj.repr(
# 131 "parser.mly"
 ( PFName(_1, _3) )
# 512 "parser.ml"
               : 'format))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ptree.ident) in
    Obj.repr(
# 133 "parser.mly"
 ( PFIdent (_1) )
# 519 "parser.ml"
               : 'format))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ptree.ident) in
    Obj.repr(
# 135 "parser.mly"
        ( PFAny (_2) )
# 526 "parser.ml"
               : 'format))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'format) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'neformatseq) in
    Obj.repr(
# 139 "parser.mly"
 ( _1 :: _3 )
# 534 "parser.ml"
               : 'neformatseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'format) in
    Obj.repr(
# 141 "parser.mly"
 ( [_1] )
# 541 "parser.ml"
               : 'neformatseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'neformatseq) in
    Obj.repr(
# 145 "parser.mly"
        ( _1 )
# 548 "parser.ml"
               : 'formatseq))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
 ( [] )
# 554 "parser.ml"
               : 'formatseq))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 151 "parser.mly"
        ( NoUnifValue(-_2) )
# 561 "parser.ml"
               : 'optint))
; (fun __caml_parser_env ->
    Obj.repr(
# 153 "parser.mly"
        ( NoUnifNegDefault )
# 567 "parser.ml"
               : 'optint))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'neidentseq) in
    Obj.repr(
# 157 "parser.mly"
        ( _1 )
# 574 "parser.ml"
               : 'identseq))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "parser.mly"
        ( [] )
# 580 "parser.ml"
               : 'identseq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'termseq) in
    Obj.repr(
# 165 "parser.mly"
 ( PSimpleFact(_1,_3), parse_extent() )
# 588 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 167 "parser.mly"
        ( PSNeq(_1,_3), parse_extent() )
# 596 "parser.ml"
               : 'fact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'termand) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 171 "parser.mly"
        ( Clause(_1,_3) )
# 604 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 173 "parser.mly"
        ( Clause([],_1) )
# 611 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'termand) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 175 "parser.mly"
        ( Equiv(_1,_3,true) )
# 619 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'termand) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 177 "parser.mly"
        ( Equiv(_1,_3,false) )
# 627 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fact) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'termand) in
    Obj.repr(
# 181 "parser.mly"
 ( _1 :: _3 )
# 635 "parser.ml"
               : 'termand))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fact) in
    Obj.repr(
# 183 "parser.mly"
 ( [_1] )
# 642 "parser.ml"
               : 'termand))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rule) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'reduc) in
    Obj.repr(
# 187 "parser.mly"
 ( _1 :: _3 )
# 650 "parser.ml"
               : 'reduc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rule) in
    Obj.repr(
# 189 "parser.mly"
 ( [_1] )
# 657 "parser.ml"
               : 'reduc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formatseq) in
    Obj.repr(
# 193 "parser.mly"
 ( (_1,_3) )
# 665 "parser.ml"
               : 'factformat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 199 "parser.mly"
        ( [(_1, _3)] )
# 673 "parser.ml"
               : 'eqlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'eqlist) in
    Obj.repr(
# 201 "parser.mly"
 ( (_1, _3) :: _5 )
# 682 "parser.ml"
               : 'eqlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ptree.decl list) in
    Obj.repr(
# 205 "parser.mly"
        ( (FunDecl(_2, _4)) :: _6 )
# 691 "parser.ml"
               : Ptree.decl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ptree.decl list) in
    Obj.repr(
# 207 "parser.mly"
        ( (DataFunDecl(_2, _4)) :: _6 )
# 700 "parser.ml"
               : Ptree.decl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'eqlist) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ptree.decl list) in
    Obj.repr(
# 209 "parser.mly"
        ( (Equation(_2)) :: _4 )
# 708 "parser.ml"
               : Ptree.decl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'fact) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ptree.decl list) in
    Obj.repr(
# 211 "parser.mly"
        ( (Query _2) :: _4 )
# 716 "parser.ml"
               : Ptree.decl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'factformat) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'optint) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ptree.decl list) in
    Obj.repr(
# 213 "parser.mly"
        ( (NoUnif (_2,_3)) :: _5 )
# 725 "parser.ml"
               : Ptree.decl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'fact) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ptree.decl list) in
    Obj.repr(
# 215 "parser.mly"
        ( (Not _2) :: _4 )
# 733 "parser.ml"
               : Ptree.decl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'fact) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ptree.decl list) in
    Obj.repr(
# 217 "parser.mly"
        ( (Elimtrue _2) :: _4 )
# 741 "parser.ml"
               : Ptree.decl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'identseq) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ptree.decl list) in
    Obj.repr(
# 219 "parser.mly"
        ( (PredDecl(_2, _4, _5)) :: _7 )
# 751 "parser.ml"
               : Ptree.decl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ptree.ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ptree.decl list) in
    Obj.repr(
# 221 "parser.mly"
        ( (Param(_2,S _4)) :: _6 )
# 760 "parser.ml"
               : Ptree.decl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ptree.decl list) in
    Obj.repr(
# 223 "parser.mly"
        ( (Param(_2,I _4)) :: _6 )
# 769 "parser.ml"
               : Ptree.decl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'reduc) in
    Obj.repr(
# 225 "parser.mly"
 ( [Reduc(_2)] )
# 776 "parser.ml"
               : Ptree.decl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 231 "parser.mly"
    ( NoUnifValue(-_2) )
# 783 "parser.ml"
               : 'nounif_value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 233 "parser.mly"
    ( NoUnifValue _3 )
# 790 "parser.ml"
               : 'nounif_value))
; (fun __caml_parser_env ->
    Obj.repr(
# 235 "parser.mly"
    ( NoUnifNegDefault )
# 796 "parser.ml"
               : 'nounif_value))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 239 "parser.mly"
    ( NoUnifValue(_2) )
# 803 "parser.ml"
               : 'select_value))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 241 "parser.mly"
    ( NoUnifValue(-_3) )
# 810 "parser.ml"
               : 'select_value))
; (fun __caml_parser_env ->
    Obj.repr(
# 243 "parser.mly"
    ( NoUnifPosDefault )
# 816 "parser.ml"
               : 'select_value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'termseq) in
    Obj.repr(
# 247 "parser.mly"
 ( PSimpleFact(_1,_3), parse_extent() )
# 824 "parser.ml"
               : 'tfact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ptree.ident) in
    Obj.repr(
# 249 "parser.mly"
        ( PSimpleFact(_1,[]), parse_extent() )
# 831 "parser.ml"
               : 'tfact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 251 "parser.mly"
        ( PSNeq(_1,_3), parse_extent() )
# 839 "parser.ml"
               : 'tfact))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ttermand) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tfact) in
    Obj.repr(
# 255 "parser.mly"
        ( Clause(_1,_3) )
# 847 "parser.ml"
               : 'trule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tfact) in
    Obj.repr(
# 257 "parser.mly"
        ( Clause([],_1) )
# 854 "parser.ml"
               : 'trule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ttermand) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tfact) in
    Obj.repr(
# 259 "parser.mly"
        ( Equiv(_1,_3,true) )
# 862 "parser.ml"
               : 'trule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ttermand) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tfact) in
    Obj.repr(
# 261 "parser.mly"
        ( Equiv(_1,_3,false) )
# 870 "parser.ml"
               : 'trule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tfact) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ttermand) in
    Obj.repr(
# 265 "parser.mly"
 ( _1 :: _3 )
# 878 "parser.ml"
               : 'ttermand))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tfact) in
    Obj.repr(
# 267 "parser.mly"
 ( [_1] )
# 885 "parser.ml"
               : 'ttermand))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'formatseq) in
    Obj.repr(
# 271 "parser.mly"
 ( (_1,_3) )
# 893 "parser.ml"
               : 'tfactformat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ptree.ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'nevartype) in
    Obj.repr(
# 275 "parser.mly"
        ( (_1,_3)::_5 )
# 902 "parser.ml"
               : 'nevartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ptree.ident) in
    Obj.repr(
# 278 "parser.mly"
        ( [(_1,_3)] )
# 910 "parser.ml"
               : 'nevartype))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'nevartype) in
    Obj.repr(
# 282 "parser.mly"
        ( _2 )
# 917 "parser.ml"
               : 'forallvartype))
; (fun __caml_parser_env ->
    Obj.repr(
# 284 "parser.mly"
        ( [] )
# 923 "parser.ml"
               : 'forallvartype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'forallvartype) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'trule) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'treduc) in
    Obj.repr(
# 288 "parser.mly"
 ( (_1,_2) :: _4 )
# 932 "parser.ml"
               : 'treduc))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'forallvartype) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'trule) in
    Obj.repr(
# 290 "parser.mly"
 ( [_1,_2] )
# 940 "parser.ml"
               : 'treduc))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'neidentseq) in
    Obj.repr(
# 294 "parser.mly"
        ( _2 )
# 947 "parser.ml"
               : 'options))
; (fun __caml_parser_env ->
    Obj.repr(
# 296 "parser.mly"
        ( [] )
# 953 "parser.ml"
               : 'options))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'forallvartype) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 302 "parser.mly"
    ( [(_1, _2, _4)] )
# 962 "parser.ml"
               : 'teqlist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'forallvartype) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'teqlist) in
    Obj.repr(
# 304 "parser.mly"
    ( (_1, _2, _4)::_6 )
# 972 "parser.ml"
               : 'teqlist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 308 "parser.mly"
        ( TTypeDecl(_2) :: _4 )
# 980 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ptree.ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 310 "parser.mly"
        ( TNameDecl(_2,_4) :: _6 )
# 989 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'identseq) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : Ptree.ident) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'options) in
    let _10 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 312 "parser.mly"
        ( (TFunDecl(_2, _4, _7, _8)) :: _10 )
# 1000 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : Ptree.ident) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'options) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 314 "parser.mly"
        ( (TConstDecl(_2, _4, _5)) :: _7 )
# 1010 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'options) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'teqlist) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 319 "parser.mly"
        ( (TEquation(_3, _2)) :: _5 )
# 1019 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'nevartype) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'tfact) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 321 "parser.mly"
        ( (TQuery(_2, _4)) :: _6 )
# 1028 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'tfact) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 323 "parser.mly"
        ( (TQuery([], _2)) :: _4 )
# 1036 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'nevartype) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'tfactformat) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'nounif_value) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 325 "parser.mly"
        ( (TNoUnif (_2, _4,_5)) :: _7 )
# 1046 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'tfactformat) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'nounif_value) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 327 "parser.mly"
        ( (TNoUnif ([],_2,_3)) :: _5 )
# 1055 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'nevartype) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'tfactformat) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'select_value) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 329 "parser.mly"
        ( (TNoUnif (_2, _4,_5)) :: _7 )
# 1065 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'tfactformat) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'select_value) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 331 "parser.mly"
        ( (TNoUnif ([],_2,_3)) :: _5 )
# 1074 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'nevartype) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'tfact) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 333 "parser.mly"
        ( (TNot(_2,_4)) :: _6 )
# 1083 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'tfact) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 335 "parser.mly"
        ( (TNot([],_2)) :: _4 )
# 1091 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'nevartype) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'tfact) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 337 "parser.mly"
        ( (TElimtrue(_2,_4)) :: _6 )
# 1100 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'tfact) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 339 "parser.mly"
        ( (TElimtrue([],_2)) :: _4 )
# 1108 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'identseq) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'options) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 341 "parser.mly"
        ( (TPredDecl(_2, _4, _6)) :: _8 )
# 1118 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ptree.ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'options) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 343 "parser.mly"
        ( (TPredDecl(_2, [], _3)) :: _5 )
# 1127 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ptree.ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 345 "parser.mly"
        ( (TSet(_2,S _4)) :: _6 )
# 1136 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Ptree.ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ptree.tdecl list) in
    Obj.repr(
# 347 "parser.mly"
        ( (TSet(_2,I _4)) :: _6 )
# 1145 "parser.ml"
               : Ptree.tdecl list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'treduc) in
    Obj.repr(
# 349 "parser.mly"
 ( [TReduc(_2)] )
# 1152 "parser.ml"
               : Ptree.tdecl list))
(* Entry all *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry tall *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let all (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ptree.decl list)
let tall (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : Ptree.tdecl list)
