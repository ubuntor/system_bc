%token FUN
%token Z
%token S0
%token S1
%token <string> ID
%token CASE
%token REC
%token WITH
%token ARROW
%token LEFT_BRACE
%token RIGHT_BRACE
%token COLON
%token LEFT_PAREN
%token RIGHT_PAREN
%token BAR
%token NAT
%token BOX
%token EOF

%right ARROW

%start <Ir.Expr.t option> top_expr
%%

top_expr:
| EOF { None }
| e = expr EOF { Some e }

ty:
| NAT { Ir.Ty.Nat }
| tau1 = ty; ARROW; tau2 = ty { Ir.Ty.Arr (tau1, tau2) }
| BOX; LEFT_PAREN; tau1 = ty; ARROW; tau2 = ty; RIGHT_PAREN { Ir.Ty.Boxarr (tau1, tau2) }
| LEFT_PAREN; tau = ty; RIGHT_PAREN { tau }

expr:
| x = ID { Var(x) }
| Z { Z }
| S0; LEFT_PAREN; e = expr; RIGHT_PAREN { S0(e) }
| S1; LEFT_PAREN; e = expr; RIGHT_PAREN { S1(e) }
| LEFT_PAREN; e1 = expr; RIGHT_PAREN; LEFT_PAREN; e2 = expr; RIGHT_PAREN { App (e1, e2) }
| CASE; e = expr; LEFT_BRACE;
  Z; ARROW; e0 = expr;
  BAR; S0; LEFT_PAREN; x = ID; RIGHT_PAREN; ARROW; e1 = expr;
  BAR; S1; LEFT_PAREN; y = ID; RIGHT_PAREN; ARROW; e2 = expr;
  RIGHT_BRACE { Case (e, (e0, (x, e1), (y, e2))) }
| REC; e = expr; LEFT_BRACE;
  Z; ARROW; e0 = expr;
  BAR; S0; LEFT_PAREN; x1 = ID; RIGHT_PAREN; WITH; y1 = ID; ARROW; e1 = expr;
  BAR; S1; LEFT_PAREN; x2 = ID; RIGHT_PAREN; WITH; y2 = ID; ARROW; e2 = expr;
  RIGHT_BRACE { Rec (e, (e0, (x1, y1, e1), (x2, y2, e2))) }
| FUN; LEFT_PAREN; x = ID; COLON; tau = ty; RIGHT_PAREN; e = expr { Fun (tau, x, e) }
| LEFT_PAREN; e = expr; RIGHT_PAREN { e }
