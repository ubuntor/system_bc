%token FUN
%token Z
%token S0
%token S1
%token CASE
%token REC
%token ARROW
%token LEFT_BRACE
%token RIGHT_BRACE
%token COLON
%token LEFT_PAREN
%token RIGHT_PAREN
%token BOX
%token EOF

%start <Expr.t> expr
%%

expr:
| Z { Z }
| S0; LEFT_PAREN; e = expr; RIGHT_PAREN { S0(e) }
| S1; LEFT_PAREN; e = expr; RIGHT_PAREN { S1(e) }
