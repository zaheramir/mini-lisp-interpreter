BeginPackage["MiniLisp`"];

parse::usage = "parse[code]  ⟹  syntax tree for a Mini‑Lisp S‑expression.";
eval::usage  = "eval[tree, env] ⟹ evaluates the parsed tree (default env <||>).";
rep::usage   = "rep[]          ⟹ tiny REPL (type quit / exit).";

Begin["`Private`"];

ClearAll[$miniLispTag, condEval];
$miniLispTag = Unique["miniTag$"];
condEval     = Unique["condTag$"];

(* TOKENISER *)
ClearAll[tokenise];
tokenise[str_String] := StringSplit[
  StringReplace[
    str,
    {"(" -> " ( ", ")" -> " ) ", "." -> " . ", "\"" :> " \" ", "'" -> " ' "}
  ],
  WhitespaceCharacter
] // DeleteCases[""]

(* PARSER *)
ClearAll[parse];
parse[str_String?StringQ] := Catch[
  Module[{tok = tokenise[str], pos = 1, peek, next, expect, rdExpr, rdList, rdRest, toAtom, res},
    peek[]  := tok[[pos]] /; pos <= Length[tok];
    next[]  := tok[[pos++]] /; pos <= Length[tok];
    expect[s_] := If[next[] =!= s, Throw["syntax error: expected " <> s, $miniLispTag]];

    toAtom[t_] := If[
      StringMatchQ[t, DigitCharacter ..],
      ToExpression[t],
      t
    ];

    rdExpr[] := Which[
      peek[] === "(", rdList[],
      peek[] === "'", next[]; {"quote", rdExpr[]},
      peek[] === ")", Throw["syntax error: unexpected ')'", $miniLispTag],
      True, toAtom[next[]]
    ];

    rdList[] := Module[{first},
      expect["("];
      If[peek[] === ")", next[]; Return[{}]];
      first = rdExpr[];
      Which[
        peek[] === ".", next[]; With[{tail = rdExpr[]}, expect[")"]; {"pair", first, tail}],
        peek[] === ")", next[]; {first},
        True, Prepend[rdRest[], first]
      ]
    ];

    rdRest[] := If[peek[] === ")", next[]; {}, Module[{e = rdExpr[]}, Prepend[rdRest[], e]]];
    res = rdExpr[];
    If[pos === Length[tok] + 1, res, Throw["syntax error: extra tokens", $miniLispTag]]
  ],
  $miniLispTag
];

(* EVALUATOR *)
ClearAll[eval];
cAccessorQ[s_String] := StringMatchQ[s, "c" ~~ ("a" | "d") .. ~~ "r"] && 3 <= StringLength[s] <= 5
applyAccessor[str_String, expr_] := Fold[
  If[#2 === "a", First[#], Rest[#]] &,
  expr,
  Reverse@Characters@StringTake[str, {2, -2}]
]
ClearAll[isAtom];
isAtom[x_] := x === {} || AtomQ[x]




eval[expr_, env_: <||>] := Module[{op, args, opStr},
  Which[
    StringQ[expr], Lookup[env, expr, expr],
    NumberQ[expr], expr,

    (* Top-level label definition, returns updated environment *)
    ListQ[expr] && First[expr] === "label",
      Module[{name = expr[[2]], fun = expr[[3]]},
        Append[env, name -> fun]
      ],

    (* Immediate label application: ((label f (lambda ...)) arg) *)
    ListQ[expr] && ListQ[First[expr]] && First[First[expr]] === "label",
      Module[{name = First[expr][[2]], fun = First[expr][[3]], call = Rest[expr]},
        eval[Prepend[call, name], Append[env, name -> fun]]
      ],

    ListQ[expr] && First[expr] === "quote", expr[[2]],

    ListQ[expr] && First[expr] === "cond",
      Catch[
        Do[
          If[eval[cl[[1]], env], Throw[eval[cl[[2]], env], condEval]],
          {cl, Rest[expr]}
        ];
        Throw["error: no true clause in COND", $miniLispTag],
        condEval
      ],

    (* Anonymous lambda application *)
    ListQ[expr] && MatchQ[First[expr], {"lambda", _, _}],
      Module[{fn = First[expr], params, body, vals},
        params = fn[[2]];
        body   = fn[[3]];
        vals   = eval[#, env] & /@ Rest[expr];
        eval[body, Join[AssociationThread[params -> vals], env]]
      ],

    (* Regular function call *)
    ListQ[expr],
      op = First[expr]; args = Rest[expr]; opStr = ToString[op];

      If[StringQ[op] && cAccessorQ[op],
        Return[applyAccessor[op, eval[args[[1]], env]]]
      ];

      If[KeyExistsQ[env, op] && ListQ[env[op]] && First[env[op]] === "lambda",
        Module[{fn = env[op], params, body, vals},
          params = fn[[2]]; body = fn[[3]];
          vals = eval[#, env] & /@ args;
          Return[eval[body, Join[AssociationThread[params -> vals], env]]]
        ]
      ];

      Switch[opStr,
        "cons", Pair[eval[args[[1]], env], eval[args[[2]], env]],

        "car", With[{p = eval[args[[1]], env]},
          If[Head[p] === Pair, p[[1]],
            Throw["error: CAR on non-pair", $miniLispTag]]
        ],

        "cdr", With[{p = eval[args[[1]], env]},
          If[Head[p] === Pair, p[[2]],
            Throw["error: CDR on non-pair", $miniLispTag]]
        ],
        "assoc",
        Module[{key = eval[args[[1]], env], alist = eval[args[[2]], env]},
          Which[
            alist === {}, {},
            MatchQ[alist[[1]], {"pair", _, _}] && alist[[1, 2]] === key, alist[[1]],
            True, eval[{"assoc", key, {"quote", Rest[alist]}}, env]
          ]
        ],
        "pairup",
        Module[{u = eval[args[[1]], env], v = eval[args[[2]], env]},
          Which[
            u === {} || v === {}, {},
            True,
              Prepend[
                eval[{"pairup", {"quote", Rest[u]}, {"quote", Rest[v]}}, env],
                {"pair", First[u], First[v]}
              ]
          ]
        ],

        "ffappend",
        Module[{u = eval[args[[1]], env], v = eval[args[[2]], env]},
          If[u === {}, v,
            Prepend[
              eval[{"ffappend", {"quote", Rest[u]}, {"quote", v}}, env],
              First[u]
            ]
          ]
        ],
        "atom", isAtom@eval[args[[1]], env],
        "null", eval[args[[1]], env] === {},
        "eq", With[{v1 = eval[args[[1]], env], v2 = eval[args[[2]], env]},
          If[isAtom[v1] && isAtom[v2] && v1 === v2, True, False]
        ],
        "list", Map[eval[#, env] &, args],
        "error", Throw["error: " <> ToString[args[[1]]], $miniLispTag],
        _, Throw["error: unknown op " <> opStr, $miniLispTag]
      ],

    True, Throw["error: invalid expression", $miniLispTag]
  ]
];

(* REPL *)
ClearAll[rep];
SetAttributes[rep, HoldAll];
rep[] := Module[{env = <||>, line, parsed, result},
  Print["Mini‑Lisp (type quit / exit)"];
  While[True,
    line = InputString["mini> "];
    If[line === "quit" || line === "exit" || line === $Canceled, Break[]];
    If[StringTrim[line] === "", Continue[]];
    Quiet[
      Check[
        parsed = parse[line];

        (* Handle top-level label *)
        If[ListQ[parsed] && First[parsed] === "label",
          env[parsed[[2]]] = parsed[[3]];
          Print["Defined ", parsed[[2]]];
          Continue[];
        ];

        result = eval[parsed, env];
        Print[result],
        Print["!! ", #] &
      ]
    ]
  ]
];

Format[Pair[a_, b_]] := Row[{"(", a, " . ", b, ")"}]
MakeBoxes[expr : {"pair", a_, b_}, fmt_] := 
  RowBox[{"(", MakeBoxes[a, fmt], " . ", MakeBoxes[b, fmt], ")"}]
End[];
EndPackage[];
