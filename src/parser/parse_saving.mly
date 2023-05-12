%{
    open Syntax
    open Parser
%}

%token<int> INT
%token LBRACKET
%token RBRACKET
%token LSBRACKET
%token RSBRACKET
%token EQ
%token DOT
%token SEMICOLON
%token EOF
%token VERSION
%token <string> IDENT
%token LAM PI APP
%token LPAREN RPAREN
%token COMMA
%token<string> STRING
%token TERM_DECL IDENT_DECL
%token RULES


%start file


%type <(term_decl list * int * associativity * ISet.t * term) list> file

%%

file:
    | LBRACKET version SEMICOLON c = content RBRACKET EOF
    {
        c
    };

version:
    | VERSION EQ x1 = INT DOT x2 = INT DOT x3 = INT {
        let v = (x1, x2, x3) in
        let (v1, v2, v3) = Config.version in
        if v <> Config.version
        then
            begin 
                Format.printf "Got saving of version %d.%d.%d instead of %d.%d.%d\n"
                    x1 x2 x3
                    v1 v2 v3
                    ;
                exit 1;
            end
        ;
    };

content:
    | RULES EQ LSBRACKET l = separated_nonempty_list(SEMICOLON, parsing_rule) RSBRACKET { l };

parsing_rule:
    | LPAREN d = decls COMMA prio = INT COMMA a = assoc COMMA ids = ids COMMA tm = expr RPAREN
        { ((d : term_decl list), prio, a, ids, tm) }
;

decls:
    | LSBRACKET l = separated_list(SEMICOLON, decl) RSBRACKET
        { l }
;
    
decl:
    | s = STRING { Symbol s }
    | IDENT_DECL i = IDENT { Ident i}
    | TERM_DECL t = IDENT { Term t }
;

assoc:
    | i = INT { Saving.assoc_of_int i }
;

ids:
    | LSBRACKET l = list(IDENT) RSBRACKET
        { ISet.of_list l }
;

expr:
    | LAM LPAREN i = IDENT COMMA e1 = expr COMMA e2 = expr RPAREN
        { {term = Lam (i, e1, e2); loc = ("no-name", 0, 0)}}
    | PI LPAREN i = IDENT COMMA e1 = expr COMMA e2 = expr RPAREN
        { {term = Pi (i, e1, e2); loc = ("no-name", 0, 0)}}
    | APP LPAREN e1 = expr COMMA e2 = expr RPAREN
        { {term = App (e1, e2); loc = ("no-name", 0, 0)} }
    | i = IDENT
        { {term = Var (i, None); loc = ("no-name", 0, 0)}}
;