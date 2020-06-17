(* tokens and aliases *)
%token FUN "fun"
%token Z "z"
%token S0 "s0"
%token S1 "s1"
%token <string> ID
%token CASE "case"
%token REC "rec"
%token WITH "with"
%token ARROW "->"
%token LEFT_BRACE "{"
%token RIGHT_BRACE "}"
%token COLON ":"
%token LEFT_PAREN "("
%token RIGHT_PAREN ")"
%token BAR "|"
%token NAT "nat"
%token BOX "[]"
%token EOF

%right "->"

%start <Ir.Expr.t> top_expr
%%

top_expr:
| e = expr EOF { e }

ty:
| "nat" { Ir.Ty.Nat }
| tau1 = ty "->" tau2 = ty { Ir.Ty.Arr (tau1, tau2) }
| "[]" "(" tau1 = ty "->" tau2 = ty ")" { Ir.Ty.Boxarr (tau1, tau2) }
| "(" tau = ty ")" { tau }

expr:
| x = ID { Var(x) }
| "z" { Z }
| "s0" "(" e = expr ")" { S0(e) }
| "s1" "(" e = expr ")" { S1(e) }
| "(" e1 = expr ")" "(" e2 = expr ")" { App (e1, e2) }
| "case" e = expr "{"
  "z" "->" e0 = expr
  "|" "s0" "(" x = ID ")" "->" e1 = expr
  "|" "s1" "(" y = ID ")" "->" e2 = expr
  "}" { Case (e, (e0, (x, e1), (y, e2))) }
| "rec" e = expr "{"
  "z" "->" e0 = expr
  "|" "s0" "(" x1 = ID ")" "with" y1 = ID "->" e1 = expr
  "|" "s1" "(" x2 = ID ")" "with" y2 = ID "->" e2 = expr
  "}" { Rec (e, (e0, (x1, y1, e1), (x2, y2, e2))) }
| "fun" "(" x = ID ":" tau = ty ")" e = expr { Fun (tau, x, e) }
| "(" e = expr ")" { e }
