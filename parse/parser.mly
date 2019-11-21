%token FUN
%token Z
%token S0
%token S1
%token <string> ID
%token CASE
%token REC
%token ARROW
%token LEFT_BRACE
%token RIGHT_BRACE
%token COLON
%token LEFT_PAREN
%token RIGHT_PAREN
%token BAR
%token BOX
%token EOF

%start <Expr.t option> top_expr
%%

top_expr:
| EOF { None }
| e = expr EOF { Some e }

expr:
| Z { Z }
| S0; LEFT_PAREN; e = expr; RIGHT_PAREN { S0(e) }
| S1; LEFT_PAREN; e = expr; RIGHT_PAREN { S1(e) }
| e1 = expr; LEFT_PAREN; e2 = expr; RIGHT_PAREN { App (e1, e2) }
| CASE; e = expr; LEFT_BRACE;
  Z; ARROW; e0 = expr;
  BAR; S0; LEFT_PAREN; x = ID; RIGHT_PAREN; ARROW; e1 = expr;
  BAR; S1; LEFT_PAREN; y = ID; RIGHT_PAREN; ARROW; e2 = expr;
  RIGHT_BRACE { Case (e, (e0, (x, e1), (y, e2))) }
| LEFT_PAREN; e = expr; RIGHT_PAREN {e}
