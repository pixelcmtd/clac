%{
#include "λ.y.h"
%}
%option noyywrap
%%
λ return 1;
\. return 2;
[a-zA-Z] { yylval = *yytext; return 3; }
.|\n ;
