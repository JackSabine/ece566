%{
#include <cstdio>
#include <list>
#include <vector>
#include <map>
#include <iostream>
#include <string>
#include <memory>
#include <stdexcept>
#include <tuple>

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

// Forward declarations
Value *indexed_variable_read(string *id, int index);

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

Value *ensemble_tree_reduction(Value *ensemble_value, ReductionType reduction_type) {
  Value *bit_array[32];

  for (int i = 0; i < 32; i++) {
    if (i == 0) bit_array[i] = Builder.CreateAnd(ensemble_value, Builder.getInt32(1));
    if (i == 31) bit_array[i] = Builder.CreateLShr(ensemble_value, Builder.getInt32(31));
    else {
      bit_array[i] = Builder.CreateAnd(
        Builder.CreateLShr(
          ensemble_value,
          Builder.getInt32(i)
        ),
        Builder.getInt32(1)
      );
    }
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

Value *ensemble_reduce_add(Value *ensemble_value) {
  Value *one_sum, *two_sum, *four_sum, *eight_sum, *thirtytwo_sum, *reduced;

  one_sum = ensemble_value;

  two_sum = Builder.CreateSub(
    one_sum,
    Builder.CreateAnd(
      Builder.CreateLShr(
        one_sum,
        Builder.getInt32(1)
      ),
      Builder.getInt32(0x55555555)
    )
  );

  four_sum = Builder.CreateAdd(
    Builder.CreateAnd(two_sum, Builder.getInt32(0x33333333)),
    Builder.CreateAnd(
      Builder.CreateLShr(two_sum, Builder.getInt32(2)),
      Builder.getInt32(0x33333333)
    )
  );

  eight_sum = Builder.CreateAnd(
    Builder.CreateAdd(
      four_sum,
      Builder.CreateLShr(
        four_sum,
        Builder.getInt32(4)
      )
    ),
    Builder.getInt32(0x0F0F0F0F)
  );

  thirtytwo_sum = Builder.CreateMul(
    eight_sum,
    Builder.getInt32(0x01010101)
  );

  reduced = Builder.CreateLShr(thirtytwo_sum, Builder.getInt32(24));

  return reduced;
}

Value *ensemble_reduce_xor(Value *ensemble_value) {
  Value *full, *half, *quar, *mult, *reduced;

  full = ensemble_value;

  half = Builder.CreateXor(full, Builder.CreateLShr(full, Builder.getInt32(1)));
  quar = Builder.CreateXor(half, Builder.CreateLShr(half, Builder.getInt32(2)));

  mult = Builder.CreateMul(
    Builder.CreateAnd(
      quar,
      Builder.getInt32(0x11111111)
    ),
    Builder.getInt32(0x11111111)
  );

  reduced = Builder.CreateAnd(
    Builder.CreateLShr(
      mult,
      Builder.getInt32(28)
    ),
    Builder.getInt32(1)
  );

  return reduced;
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

map<string, Value *> variable_index_cache;

Value *indexed_variable_read(string *id, int index) {
  Value *variable_read;
  Value *shifted;
  Value *masked;

  string formatted_map_index;

  formatted_map_index = string(*id) + to_string(index);

  if (variable_index_cache.find(formatted_map_index) == variable_index_cache.end()) {
    // Missed in cache, make new instructions
    variable_read = variable_space.read(id);

    // (ID (logical)>> NUMBER) & 1
    shifted = (index != 0) ? Builder.CreateLShr(variable_read, index) : variable_read;
    masked = (index != 31) ? Builder.CreateAnd(shifted, Builder.getInt32(1)) : shifted;
    variable_index_cache[formatted_map_index] = masked;
  }

  return variable_index_cache.at(formatted_map_index);
}

// Clear cache for specific index of a variable
void clear_specific_index_of_variable(string *id, int index) {
  string formatted_map_index;

  formatted_map_index = string(*id) + to_string(index);

  if (variable_index_cache.find(formatted_map_index) == variable_index_cache.end()) {
    variable_index_cache.erase(formatted_map_index);
  }
}

// Clear cache for all indices for variable
void clear_all_indices_of_variable(string *id) {
  for (int i = 0; i < 32; i++) {
    clear_specific_index_of_variable(id, i);
  }
}

class AggregatedEnsembleElement {
public:
  bool is_an_aggregate;

  Value *value; // used by non-aggregates
  int postshift;

  string id;
  int aggregate_width;
  int aggregate_min_index;

  AggregatedEnsembleElement(Value *value, int postshift) {
    this->is_an_aggregate = false;
    this->value = value;
    this->postshift = postshift;
    this->id = "";
    this->aggregate_width = 0;
    this->aggregate_min_index = 0;
  }

  AggregatedEnsembleElement(string id, int aggregate_width, int aggregate_min_index) {
    this->is_an_aggregate = true;
    this->value = nullptr;
    this->postshift = 0;
    this->id = id;
    this->aggregate_width = aggregate_width;
    this->aggregate_min_index = aggregate_min_index;
  }
};

class BetterExpr {
public:
  Value *value;
  bool is_a_single_bit_slice;
  string *id;
  int index;

  BetterExpr(Value *value) {
    this->value = value;
    this->is_a_single_bit_slice = false;
    this->id = nullptr;
    this->index = 0;
  }

  BetterExpr(Value *value, string *id, int index) {
    this->value = value;
    this->is_a_single_bit_slice = true;
    this->id = id;
    this->index = index;
  }
};

Value *elaborate_ensemble(vector<tuple<BetterExpr *, int>> *ensemble) {
  BetterExpr *curr_bexpr;
  int curr_postshift;

  bool active_streak = false;
  string active_variable;
  int highest_index;
  int lowest_index;

  Value *result = nullptr;

  vector<AggregatedEnsembleElement> aggregated_ensemble;

  Value *shifted;
  Value *masked;
  Value *read;

  for (tuple<BetterExpr *, int> curr : *ensemble) {
    curr_bexpr = get<0>(curr);
    curr_postshift = get<1>(curr);

    if (active_streak) {
      if (
        curr_bexpr->is_a_single_bit_slice &&
        string(*curr_bexpr->id) == active_variable &&
        curr_bexpr->index == (lowest_index - 1) &&
        curr_postshift == 0
      ) {
        // New streak member!
        lowest_index = curr_bexpr->index;
      } else {
        // Streak ended, push streak to aggregated_ensemble
        active_streak = false;
        aggregated_ensemble.push_back(
          AggregatedEnsembleElement(
            active_variable,
            highest_index - lowest_index + 1,
            lowest_index
          )
        );
      }
    }

    // The above logic may have ended a streak because of the current element
    // Check to see if this element is eligible to start a new streak
    if (!active_streak) {
      if (curr_bexpr->is_a_single_bit_slice && curr_bexpr->index != 0) {
        // Begin a new streak
        active_streak = true;
        active_variable = string(*curr_bexpr->id);
        highest_index = curr_bexpr->index;
        lowest_index = curr_bexpr->index;
      } else {
        // Not eligible for a streak, push expr and postshift to aggregated_ensemble
        aggregated_ensemble.push_back(
          AggregatedEnsembleElement(
            curr_bexpr->value,
            curr_postshift
          )
        );
      }
    }
  }

  if (active_streak) {
    // The end of the ensemble is reached, end the streak and push to aggregated_ensemble
    active_streak = false;
    aggregated_ensemble.push_back(
      AggregatedEnsembleElement(
        active_variable,
        highest_index - lowest_index + 1,
        lowest_index
      )
    );
  }

  for (AggregatedEnsembleElement curr : aggregated_ensemble) {
    // Iterate through this layer and create instructions
    // It doesn't matter if you know the final shift of the first element
    // left shifting new elements in affects old elements, too -> same instruction count
    if (curr.is_an_aggregate) {
      // Computing the value to shift in
      if (curr.aggregate_width == 0) {
        read = indexed_variable_read(&curr.id, curr.aggregate_min_index);
        if (result == nullptr) {
          result = read;
        } else {
          result = Builder.CreateOr(
            Builder.CreateShl(result, curr.aggregate_width),
            read
          );
        }
      } else {
        if (curr.aggregate_min_index == 0) {
          shifted = variable_space.read(&curr.id);
        } else {
          shifted = Builder.CreateLShr(variable_space.read(&curr.id), curr.aggregate_min_index);
        }

        if (curr.aggregate_width == 32) {
          masked = shifted;
        } else {
          masked = Builder.CreateAnd(
            shifted,
            Builder.getInt32((1 << curr.aggregate_width) - 1) // Gets an aggregate_width-wide bitmask
          );
        }

        if (result == nullptr) {
          result = masked;
        } else {
          result = Builder.CreateOr(
            Builder.CreateShl(result, curr.aggregate_width),
            masked
          );
        }
      }
    } else {
      if (result == nullptr) {
        result = curr.value;
      } else {
        result = Builder.CreateOr(Builder.CreateShl(result, Builder.getInt32(1)), curr.value);
      }
      if (curr.postshift != 0) {
        result = Builder.CreateShl(result, Builder.getInt32(curr.postshift));
      }
    }
  }

  return result;
}

Function *F;

%}

%union {
  vector<string> *params_list;
  Value *val;
  int number;
  string *id;
  BetterExpr *better_expr;
  vector<tuple<BetterExpr *, int>> *ens;
}

/*%define parse.trace*/

%type <params_list> params_list
%type <better_expr> expr
%type <ens> ensemble

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
  F = Function::Create(FunType,GlobalValue::ExternalLinkage,funName,M);

  for(Argument &a: F->args()) {
    // iterate over arguments of function
    // match name to position
    variable_space.add_argument(
      &$2->at(a.getArgNo()),
      &a
    );
  }

  //Add a basic block to main to hold instructions, and set Builder
  //to insert there
  Builder.SetInsertPoint(BasicBlock::Create(TheContext, "entry", F));

}
| IN NONE ENDLINE
{
  // Create int function type with no arguments
  FunctionType *FunType =
    FunctionType::get(Builder.getInt32Ty(),false);

  // Create a main function
  F = Function::Create(FunType,
         GlobalValue::ExternalLinkage,funName,M);

  //Add a basic block to main to hold instructions, and set Builder
  //to insert there
  Builder.SetInsertPoint(BasicBlock::Create(TheContext, "entry", F));
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
  vector<Instruction *> instructions_to_remove;
  bool any_instructions_removed;
  Builder.CreateRet(elaborate_ensemble($2));

  do {
    any_instructions_removed = false;

    for(BasicBlock &BB: *F) {
      for (Instruction &I: BB) {
        if (I.isSafeToRemove() && I.use_empty()) {
          instructions_to_remove.push_back(&I);
        }
      }
    }

    for (Instruction *I: instructions_to_remove) {
      I->eraseFromParent();
      any_instructions_removed = true;
    }
    instructions_to_remove.clear();
  } while (any_instructions_removed);
}
;

endline_opt: %empty | ENDLINE;


statements_opt: %empty
            | statements;

statements:   statement
            | statements statement
;

statement: ID ASSIGN ensemble ENDLINE {
  fprintf(stdout, "Assigning to %s\n", $1->c_str());
  variable_space.write($1, elaborate_ensemble($3));
}
| ID NUMBER ASSIGN ensemble ENDLINE {
  if (variable_space.id_is_undeclared($1)) {
    YYABORT;
  }
  indexed_write_to_variable($1, Builder.getInt32($2), elaborate_ensemble($4));
  clear_specific_index_of_variable($1, $2);
}
| ID LBRACKET ensemble RBRACKET ASSIGN ensemble ENDLINE {
  if (variable_space.id_is_undeclared($1)) {
    YYABORT;
  }
  indexed_write_to_variable($1, elaborate_ensemble($3), elaborate_ensemble($6));
  clear_all_indices_of_variable($1);
}
;

ensemble:  expr {
  $$ = new vector<tuple<BetterExpr *, int>>;
  $$->push_back(make_tuple($1, 0));
}
| expr COLON NUMBER {                  // 566 only
  $$ = new vector<tuple<BetterExpr *, int>>;
  $$->push_back(make_tuple($1, $3));
}
| ensemble COMMA expr {
  $1->push_back(make_tuple($3, 0));
  $$ = $1;
}
| ensemble COMMA expr COLON NUMBER {  // 566 only
  $1->push_back(make_tuple($3, $5));
  $$ = $1;
}
;

expr:   ID {
  // fprintf(stdout, "Reading from %s\n", $1->c_str());
  $$ = new BetterExpr(variable_space.read($1));
}
| ID NUMBER {
  $$ = new BetterExpr(indexed_variable_read($1, $2), $1, $2);
}
| NUMBER {
  $$ = new BetterExpr(Builder.getInt32($1));
}
| expr PLUS expr {
  $$ = new BetterExpr(Builder.CreateAdd($1->value, $3->value));
}
| expr MINUS expr {
  $$ = new BetterExpr(Builder.CreateSub($1->value, $3->value));
}
| expr XOR expr {
  $$ = new BetterExpr(Builder.CreateXor($1->value, $3->value));
}
| expr AND expr {
  $$ = new BetterExpr(Builder.CreateAnd($1->value, $3->value));
}
| expr OR expr {
  $$ = new BetterExpr(Builder.CreateOr($1->value, $3->value));
}
| INV expr {
  $$ = new BetterExpr(Builder.CreateNot($2->value));
}
| BINV expr {
  $$ = new BetterExpr(Builder.CreateXor($2->value, Builder.getInt32(1)));
}
| expr MUL expr {
  $$ = new BetterExpr(Builder.CreateMul($1->value, $3->value));
}
| expr DIV expr {
  // Signed division per https://piazza.com/class/lr5jf250zob9a/post/40
  $$ = new BetterExpr(Builder.CreateSDiv($1->value, $3->value));
}
| expr MOD expr {
  $$ = new BetterExpr(Builder.CreateSRem($1->value, $3->value));
}
| ID LBRACKET ensemble RBRACKET {
  $$ = new BetterExpr(
    Builder.CreateAnd(
      Builder.CreateLShr(
        variable_space.read($1),
        elaborate_ensemble($3)
      ),
      Builder.getInt32(1)
    )
  );
}
| LPAREN ensemble RPAREN {
  $$ = new BetterExpr(elaborate_ensemble($2));
}
/* 566 only */
| LPAREN ensemble RPAREN LBRACKET ensemble RBRACKET {
  $$ = new BetterExpr(
      Builder.CreateAnd(
      Builder.CreateLShr(
        elaborate_ensemble($2),
        elaborate_ensemble($5)
      ),
      Builder.getInt32(1)
    )
  );
}
| REDUCE AND LPAREN ensemble RPAREN {
  $$ = new BetterExpr(ensemble_tree_reduction(elaborate_ensemble($4), REDUCE_AND));
}
| REDUCE OR LPAREN ensemble RPAREN {
  $$ = new BetterExpr(ensemble_tree_reduction(elaborate_ensemble($4), REDUCE_OR));
}
| REDUCE XOR LPAREN ensemble RPAREN {
  $$ = new BetterExpr(ensemble_reduce_xor(elaborate_ensemble($4)));
}
| REDUCE PLUS LPAREN ensemble RPAREN {
  $$ = new BetterExpr(ensemble_reduce_add(elaborate_ensemble($4)));
}
| EXPAND  LPAREN ensemble RPAREN {
  // Duplicate the lsb 32 times
  // 0 - ($3 & 1)
  //
  // ($3 & 1) | 0 - ($3 & 1)
  // ---------+-------------
  //        0 | 0 - 0 = 0
  //        1 | 0 - 1 = FFFF_FFFF
  $$ = new BetterExpr(
    Builder.CreateSub(
      Builder.getInt32(0),
      Builder.CreateAnd(
        elaborate_ensemble($3),
        Builder.getInt32(1)
      )
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
