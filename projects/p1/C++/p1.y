%{
#include <cstdio>
#include <list>
#include <vector>
#include <map>
#include <iostream>
#include <string>
#include <memory>
#include <stdexcept>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Support/SystemUtils.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Support/FileSystem.h"

using namespace llvm;
using namespace std;

// Need for parser and scanner
extern FILE *yyin;
int yylex();
void yyerror(const char*);
int yyparse();

// Needed for LLVM
string funName;
Module *M;
LLVMContext TheContext;
IRBuilder<> Builder(TheContext);

class VariableSpace {
private:
  map<string, Value *> variable_map;
public:
  bool id_is_undeclared(string *id) {
    return variable_map.find(*id) == variable_map.end();
  }

  Value *read(string *id) {
    if (id_is_undeclared(id)) {
      fprintf(stderr,"%s is used without first being defined. Assuming 0.\n", id->c_str());
      variable_map[*id] = Builder.getInt32(0);
    }

    return variable_map[*id];
  }

  void write(string *id, Value *value) {
    variable_map[*id] = value;
  }

  void add_argument(string *id, Argument *argument) {
    // Argument is a child of Value
    variable_map[*id] = argument;
  }
};

VariableSpace variable_space;

enum ReductionType {REDUCE_AND = 0, REDUCE_OR, REDUCE_XOR, REDUCE_ADD};

Value *reduction_atom(Value *LHS, Value *RHS, ReductionType reduction_type) {
  switch(reduction_type) {
    case REDUCE_AND:
      return Builder.CreateAnd(LHS, RHS);
    case REDUCE_OR:
      return Builder.CreateOr(LHS, RHS);
    case REDUCE_XOR:
      return Builder.CreateXor(LHS, RHS);
    case REDUCE_ADD:
      return Builder.CreateAdd(LHS, RHS);
    default:
      fprintf(stderr, "reduction_type (%0d) not implemented\n", reduction_type);
      return Builder.getInt32(0);
  }
}

Value *tree_reduction(Value *ensemble_value, ReductionType reduction_type) {
  Value *bit_array[32];

  for (int i = 0; i < 32; i++) {
    bit_array[i] = Builder.CreateAnd(
      Builder.CreateLShr(
        ensemble_value,
        Builder.getInt32(i)
      ),
      Builder.getInt32(1)
    );
  }

  // bit_array is now the ensemble split into 32 numbers whose values are in the set {0, 1}
  for (int inc = 1; inc < 32; inc *= 2) {
    for (int i = 0; i < 32; i = i + (2*inc)) {
      bit_array[i] = reduction_atom(
        bit_array[i],
        bit_array[i + inc],
        reduction_type
      );
    }
  }

  return bit_array[0];
}

void indexed_write_to_variable(string *id, Value *index, Value *value_to_write) {
  Value *local_id_value;

  Value *masked;
  Value *shifted_lsb;
  Value *bit_inserted_id_value;

  local_id_value = variable_space.read(id);

  // masked = local_id_value & ~(1 << index);
  masked = Builder.CreateAnd(
    Builder.CreateNot(
      Builder.CreateShl(
        Builder.getInt32(1),
        index
      )
    ),
    local_id_value
  );

  // shifted_lsb = (ensemble & 1) << index;
  shifted_lsb = Builder.CreateShl(
    Builder.CreateAnd(
      value_to_write,
      Builder.getInt32(1)
    ),
    index
  );

  // bit_inserted_id_value = masked | shifted_lsb;
  bit_inserted_id_value = Builder.CreateOr(
    masked,
    shifted_lsb
  );

  variable_space.write(id, bit_inserted_id_value);
}

%}

%union {
  vector<string> *params_list;
  Value *val;
  int number;
  string *id;
}

/*%define parse.trace*/

%type <params_list> params_list
%type <val> expr
%type <val> ensemble

%token IN FINAL
%token ERROR
%token <number> NUMBER
%token <id> ID
%token BINV INV PLUS MINUS XOR AND OR MUL DIV MOD
%token COMMA ENDLINE ASSIGN LBRACKET RBRACKET LPAREN RPAREN NONE COLON
%token REDUCE EXPAND

%precedence BINV
%precedence INV
%left PLUS MINUS OR
%left MUL DIV AND XOR MOD

%start program

%%

program: inputs statements_opt final
{
  YYACCEPT;
}
;

inputs:   IN params_list ENDLINE
{
  std::vector<Type*> param_types;
  for(auto s: *$2)
    {
      param_types.push_back(Builder.getInt32Ty());
    }
  ArrayRef<Type*> Params (param_types);

  // Create int function type with no arguments
  FunctionType *FunType =
    FunctionType::get(Builder.getInt32Ty(),Params,false);

  // Create a main function
  Function *Function = Function::Create(FunType,GlobalValue::ExternalLinkage,funName,M);

  for(Argument &a: Function->args()) {
    // iterate over arguments of function
    // match name to position
    variable_space.add_argument(
      &$2->at(a.getArgNo()),
      &a
    );
  }

  //Add a basic block to main to hold instructions, and set Builder
  //to insert there
  Builder.SetInsertPoint(BasicBlock::Create(TheContext, "entry", Function));

}
| IN NONE ENDLINE
{
  // Create int function type with no arguments
  FunctionType *FunType =
    FunctionType::get(Builder.getInt32Ty(),false);

  // Create a main function
  Function *Function = Function::Create(FunType,
         GlobalValue::ExternalLinkage,funName,M);

  //Add a basic block to main to hold instructions, and set Builder
  //to insert there
  Builder.SetInsertPoint(BasicBlock::Create(TheContext, "entry", Function));
}
;

params_list: ID
{
  $$ = new vector<string>;
  $$->push_back(*$1);
}
| params_list COMMA ID
{
  $1->push_back(*$3);
  $$ = $1;
}
;

final: FINAL ensemble endline_opt
{
  Builder.CreateRet($2);
}
;

endline_opt: %empty | ENDLINE;


statements_opt: %empty
            | statements;

statements:   statement
            | statements statement
;

statement: ID ASSIGN ensemble ENDLINE {
  // fprintf(stdout, "Assigning to %s\n", $1->c_str());
  variable_space.write($1, $3);
}
| ID NUMBER ASSIGN ensemble ENDLINE {
  if (variable_space.id_is_undeclared($1)) {
    YYABORT;
  }
  indexed_write_to_variable($1, Builder.getInt32($2), $4);
}
| ID LBRACKET ensemble RBRACKET ASSIGN ensemble ENDLINE {
  if (variable_space.id_is_undeclared($1)) {
    YYABORT;
  }
  indexed_write_to_variable($1, $3, $6);
}
;

ensemble:  expr {
  $$ = $1;
}
| expr COLON NUMBER {                  // 566 only
  $$ = Builder.CreateShl($1, Builder.getInt32($3));
}
| ensemble COMMA expr {
  $$ = Builder.CreateOr(Builder.CreateShl($1, Builder.getInt32(1)), $3);
}
| ensemble COMMA expr COLON NUMBER {  // 566 only
  $$ = Builder.CreateShl(
    Builder.CreateOr(Builder.CreateShl($1, Builder.getInt32(1)), $3),
    Builder.getInt32($5)
  );
}
;

expr:   ID {
  // fprintf(stdout, "Reading from %s\n", $1->c_str());
  $$ = variable_space.read($1);
}
| ID NUMBER {
  // (ID (logical)>> NUMBER) & 1
  $$ = Builder.CreateAnd(
    Builder.CreateLShr(
      variable_space.read($1),
      $2
    ),
    Builder.getInt32(1)
  );
}
| NUMBER {
  $$ = Builder.getInt32($1);
}
| expr PLUS expr {
  $$ = Builder.CreateAdd($1, $3);
}
| expr MINUS expr {
  $$ = Builder.CreateSub($1, $3);
}
| expr XOR expr {
  $$ = Builder.CreateXor($1, $3);
}
| expr AND expr {
  $$ = Builder.CreateAnd($1, $3);
}
| expr OR expr {
  $$ = Builder.CreateOr($1, $3);
}
| INV expr {
  $$ = Builder.CreateNot($2);
}
| BINV expr {
  $$ = Builder.CreateXor($2, Builder.getInt32(1));
}
| expr MUL expr {
  $$ = Builder.CreateMul($1, $3);
}
| expr DIV expr {
  $$ = Builder.CreateSDiv($1, $3); // Signed division per https://piazza.com/class/lr5jf250zob9a/post/40
}
| expr MOD expr {
  $$ = Builder.CreateSRem($1, $3);
}
| ID LBRACKET ensemble RBRACKET {
  $$ = Builder.CreateAnd(
    Builder.CreateLShr(
      variable_space.read($1),
      $3
    ),
    Builder.getInt32(1)
  );
}
| LPAREN ensemble RPAREN {
  $$ = $2;
}
/* 566 only */
| LPAREN ensemble RPAREN LBRACKET ensemble RBRACKET {
  $$ = Builder.CreateAnd(
    Builder.CreateLShr(
      $2,
      $5
    ),
    Builder.getInt32(1)
  );
}
| REDUCE AND LPAREN ensemble RPAREN {
  $$ = tree_reduction($4, REDUCE_AND);
}
| REDUCE OR LPAREN ensemble RPAREN {
  $$ = tree_reduction($4, REDUCE_OR);
}
| REDUCE XOR LPAREN ensemble RPAREN {
  $$ = tree_reduction($4, REDUCE_XOR);
}
| REDUCE PLUS LPAREN ensemble RPAREN {
  $$ = tree_reduction($4, REDUCE_ADD);
}
| EXPAND  LPAREN ensemble RPAREN {
  // Duplicate the lsb 32 times
  // 0 - ($3 & 1)
  //
  // ($3 & 1) | 0 - ($3 & 1)
  // ---------+-------------
  //        0 | 0 - 0 = 0
  //        1 | 0 - 1 = FFFF_FFFF
  $$ = Builder.CreateSub(
    Builder.getInt32(0),
    Builder.CreateAnd(
      $3,
      Builder.getInt32(1)
    )
  );
}
;

%%

unique_ptr<Module> parseP1File(const string &InputFilename)
{
  funName = InputFilename;
  if (funName.find_last_of('/') != string::npos)
    funName = funName.substr(funName.find_last_of('/')+1);
  if (funName.find_last_of('.') != string::npos)
    funName.resize(funName.find_last_of('.'));

  //errs() << "Function will be called " << funName << ".\n";

  // unique_ptr will clean up after us, call destructor, etc.
  unique_ptr<Module> Mptr(new Module(funName.c_str(), TheContext));

  // set global module
  M = Mptr.get();

  /* this is the name of the file to generate, you can also use
     this string to figure out the name of the generated function */
  yyin = fopen(InputFilename.c_str(),"r");

  //yydebug = 1;
  if (yyparse() != 0)
    // errors, so discard module
    Mptr.reset();
  else
    // Dump LLVM IR to the screen for debugging
    M->print(errs(),nullptr,false,true);

  return Mptr;
}

void yyerror(const char* msg)
{
  printf("%s\n",msg);
}
