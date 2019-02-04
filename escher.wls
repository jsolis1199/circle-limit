#!/usr/bin/env wolframscript

If[
  Length[$ScriptCommandLine] < 3,
  WriteString["stdout", "Not enough arguments. Aborting...\n"];
  Quit[1]
]
p = ToExpression[$ScriptCommandLine[[2]]]
q = ToExpression[$ScriptCommandLine[[3]]]
symbol = "{" <> ToString[p] <> ", " <> ToString[q] <> "}" 
If[
  IntegerQ[p] && IntegerQ[q] && 1/p + 1/q < 1/2,
  WriteString[
    "stdout",
    "Identified the Schlafi symbol " <> symbol <> ".\nGenerating vertices..."
  ],
  WriteString[
    "stdout",
    symbol <> " is not a valid Schlafi symbol. Aborting...\n"
  ];
  Quit[1]
]

<<poincare.wl

threshold = 500000
depth = Floor[Log[q - 1, 1 + threshold * (q - 2) / p]]

vertices = <||>
Do[
  AssociateTo[
    vertices,
    {1, m} ->
    N[
      Cos[Pi (1/p + 1/q)]/Sqrt[Cos[Pi/q]^2-Sin[Pi/p]^2]*Exp[I (2 Pi*m/p - Pi/p)]
    ]
  ],
  {m, p}
]
Do[
  AssociateTo[
    vertices,
    {2, (m - 1) (q - 1) + n} ->
    rotate[
      vertices[{1, Mod[m - 2, p] + 1}],
      vertices[{1, m}],
      2 Pi*n/q
    ]
  ],
  {m, p},
  {n, q - 1}
]
Do[
  AssociateTo[
    vertices,
    {l, (m - 1) (q - 1) + n} ->
    rotate[
      vertices[{l - 2, Quotient[m - 1, q - 1] + 1}],
      vertices[{l - 1, m}],
      2 Pi*n/q
    ]
  ],
  {l, 3, depth},
  {m, ((q - 1)^(l - 2)) p},
  {n, q - 1}
]
epsilon = .001
vertices = Round[vertices, epsilon]
WriteString["stdout", " done.\n"]

WriteString["stdout", "Generating edges..."]
edges = Reap[
  Do[
    Sow[
      UndirectedEdge[
        vertices[{1, m}],
        vertices[{1, Mod[m - 2, p] + 1}]
      ]
    ],
    {m, p}
  ];
  Do[
    Sow[
      UndirectedEdge[
        vertices[{l - 1, m}],
        vertices[{l, (m - 1) (q - 1) + n}]
      ]
    ],
    {l, 2, depth},
    {m, ((q - 1)^(l - 2)) p},
    {n, q - 1}
  ]
][[2]][[1]]
WriteString["stdout", " done.\n"]

file = symbol <> ".pdf"
WriteString["stdout", "Exporting image..."]
Export[
  file,
  Graphics[
    Style[
      Append[
        Apply[geodesic, EdgeList[SimpleGraph[Graph[edges]]], {1}],
        Circle[ReIm[0]]
      ],
      Thin
    ]
  ]
]
WriteString["stdout", " Image written to " <> file <> "\n"]
