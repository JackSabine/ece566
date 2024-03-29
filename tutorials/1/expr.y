%{
#include <cstdio>
#include <list>
#include <iostream>
#include <string>
#include <memory>
#include <stdexcept>

using namespace std;

extern FILE *yyin;         // defined in expr.lex
int yylex();               // defined in expr.lex
void yyerror(const char*); // defined in expr.lex

extern "C" {
  // this function is generated by bison from the rules in this file.
  int yyparse();
}

// helper code for string formatting.
template<typename ... Args>
std::string format( const std::string& format, Args ... args )
{
    size_t size = snprintf( nullptr, 0, format.c_str(), args ... ) + 1; // Extra space for '\0'
    if( size <= 0 ){ throw std::runtime_error( "Error during formatting." ); }
    std::unique_ptr<char[]> buf( new char[ size ] ); 
    snprintf( buf.get(), size, format.c_str(), args ... );
    return std::string( buf.get(), buf.get() + size - 1 ); // We don't want the '\0' inside
}

int getReg() {
  static int cnt = 8;
  return cnt++;
}

   
%}

// Make the output verbose
%verbose
%define parse.trace

%union {
  int reg;
  int imm;
}

%token <reg> REG
%token <imm> IMMEDIATE
%type <reg> expr

%token ASSIGN SEMI PLUS MINUS LPAREN RPAREN LBRACKET RBRACKET

%left  PLUS MINUS

%%

program:   REG ASSIGN expr SEMI
{
  //printf("program: REG ASSIGN expr SEMI\n");
  printf("ADD R%d, R%d, 0\n",$1,$3);
  return 0; // if we get here, we succeeded!
}
;

expr: IMMEDIATE
{
  int reg = getReg();
  //printf("expr: IMMEDIATE (%d)\n", $1);
  printf("AND R%d, R%d, 0\n",reg,reg);
  printf("ADD R%d, R%d, %d\n", reg, reg, $1);
  $$ = reg;
}
| REG
{
  printf("expr: REG (%d)\n", $1);
  $$ = $1;
}
| expr PLUS expr
{
  int reg = getReg();
  //printf("expr: IMMEDIATE (%d)\n", $1);
  printf("ADD R%d, R%d, R%d\n", reg, $1, $3);
  $$ = reg;
}
| expr MINUS expr
{
  //printf("expr: expr MINUS expr\n");
  int reg = getReg();
  //printf("expr: IMMEDIATE (%d)\n", $1);
  printf("SUB R%d, R%d, R%d\n", reg, $1, $3);
  $$ = reg;

}
| LPAREN expr RPAREN
{
  //printf("expr: LPAREN expr RPAREN\n");
  $$ = $2;
}
| MINUS expr
{
  //printf("expr: MINUS expr\n");
  int reg = getReg();
  //printf("expr: IMMEDIATE (%d)\n", $1);
  printf("NOT R%d, R%d\n", reg, $2);
  printf("ADD R%d, R%d, 1\n", reg, reg);
  $$ = reg;
}
| LBRACKET expr RBRACKET
{
  //  printf("expr: LBRACKET expr RBRACKET\n");
  //printf("expr: expr MINUS expr\n");
  int reg = getReg();
  //printf("expr: IMMEDIATE (%d)\n", $1);
  printf("LDR R%d, R%d, 0\n", reg, $2);
  $$ = reg;
}
;

%%

void yyerror(const char* msg)
{
  printf("%s",msg);
}

int main(int argc, char *argv[])
{
  yydebug = 0;
  yyin = stdin;
  yyparse();
  return 0;
}
