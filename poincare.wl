translate[a_, b_] := (a + b)/(1 + a * Conjugate[b])

rotate[a_, b_, theta_] := translate[translate[a, -b] * Exp[theta * I], b]

center[a_, b_] := (a * b * Conjugate[a - b] + b - a)/(2 * I * Im[Conjugate[a] * b])

geodesic[a_, b_] := If[
  Abs[Arg[b] - Arg[a]] <= 10^(-Min[Accuracy[Arg[{a, b}]]]),
  Line[ReIm[{a, b}]],
  With[
    {c = center[a, b]},
    Circle[ReIm[c], Sqrt[Abs[c] ^ 2 - 1],
    Arg[(c - {a, b}) * Exp[-Arg[c] * I]] + Arg[c] + Pi
    ]
  ]
]

polygon[vertices_] := Apply[
  geodesic,
  Table[{vertices[[i]], vertices[[Mod[i, p] + 1]]}, {i, p}],
  {1}
]
