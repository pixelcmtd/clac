%{
#include "λ.l.h"
extern int yyerror(char *);
%}
%token END 0
%token LAMBDA 1
%token DOT 2
%token ID 3
%%

func: LAMBDA ids DOT ids

ids: ID | ID ids
