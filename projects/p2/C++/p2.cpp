#include <fstream>
#include <memory>
#include <algorithm>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "llvm-c/Core.h"

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/Bitcode/BitcodeReader.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/LinkAllPasses.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Analysis/InstructionSimplify.h"

using namespace llvm;

static void CommonSubexpressionElimination(Module *);

static void summarize(Module *M);
static void print_csv_file(std::string outputfile);

static cl::opt<std::string>
        InputFilename(cl::Positional, cl::desc("<input bitcode>"), cl::Required, cl::init("-"));

static cl::opt<std::string>
        OutputFilename(cl::Positional, cl::desc("<output bitcode>"), cl::Required, cl::init("out.bc"));

static cl::opt<bool>
        Mem2Reg("mem2reg",
                cl::desc("Perform memory to register promotion before CSE."),
                cl::init(false));

static cl::opt<bool>
        NoCSE("no-cse",
              cl::desc("Do not perform CSE Optimization."),
              cl::init(false));

static cl::opt<bool>
        Verbose("verbose",
                    cl::desc("Verbose stats."),
                    cl::init(false));

static cl::opt<bool>
        NoCheck("no",
                cl::desc("Do not check for valid IR."),
                cl::init(false));

int main(int argc, char **argv) {
    // Parse command line arguments
    cl::ParseCommandLineOptions(argc, argv, "llvm system compiler\n");

    // Handle creating output files and shutting down properly
    llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.
    LLVMContext Context;

    // LLVM idiom for constructing output file.
    std::unique_ptr<ToolOutputFile> Out;
    std::string ErrorInfo;
    std::error_code EC;
    Out.reset(new ToolOutputFile(OutputFilename.c_str(), EC,
                                 sys::fs::OF_None));

    EnableStatistics();

    // Read in module
    SMDiagnostic Err;
    std::unique_ptr<Module> M;
    M = parseIRFile(InputFilename, Err, Context);

    // If errors, fail
    if (M.get() == 0)
    {
        Err.print(argv[0], errs());
        return 1;
    }

    // If requested, do some early optimizations
    if (Mem2Reg)
    {
        legacy::PassManager Passes;
        Passes.add(createPromoteMemoryToRegisterPass());
        Passes.run(*M.get());
    }

    if (!NoCSE) {
        CommonSubexpressionElimination(M.get());
    }

    // Collect statistics on Module
    summarize(M.get());
    print_csv_file(OutputFilename);

    if (Verbose)
        PrintStatistics(errs());

    // Verify integrity of Module, do this by default
    if (!NoCheck)
    {
        legacy::PassManager Passes;
        Passes.add(createVerifierPass());
        Passes.run(*M.get());
    }

    // Write final bitcode
    WriteBitcodeToFile(*M.get(), Out->os());
    Out->keep();

    return 0;
}

static llvm::Statistic nFunctions = {"", "Functions", "number of functions"};
static llvm::Statistic nInstructions = {"", "Instructions", "number of instructions"};
static llvm::Statistic nLoads = {"", "Loads", "number of loads"};
static llvm::Statistic nStores = {"", "Stores", "number of stores"};

static void summarize(Module *M) {
    for (auto i = M->begin(); i != M->end(); i++) {
        if (i->begin() != i->end()) {
            nFunctions++;
        }

        for (auto j = i->begin(); j != i->end(); j++) {
            for (auto k = j->begin(); k != j->end(); k++) {
                Instruction &I = *k;
                nInstructions++;
                if (isa<LoadInst>(&I)) {
                    nLoads++;
                } else if (isa<StoreInst>(&I)) {
                    nStores++;
                }
            }
        }
    }
}

static void print_csv_file(std::string outputfile)
{
    std::ofstream stats(outputfile + ".stats");
    auto a = GetStatistics();
    for (auto p : a) {
        stats << p.first.str() << "," << p.second << std::endl;
    }
    stats.close();
}

static llvm::Statistic CSEDead = {"", "CSEDead", "CSE found dead instructions"};
static llvm::Statistic CSEElim = {"", "CSEElim", "CSE redundant instructions"};
static llvm::Statistic CSESimplify = {"", "CSESimplify", "CSE simplified instructions"};
static llvm::Statistic CSELdElim = {"", "CSELdElim", "CSE redundant loads"};
static llvm::Statistic CSEStore2Load = {"", "CSEStore2Load", "CSE forwarded store to load"};
static llvm::Statistic CSEStElim = {"", "CSEStElim", "CSE redundant stores"};

bool isDead(Instruction &I) {
    int32_t opcode;
    LoadInst *li;

    opcode = I.getOpcode();

    switch(opcode) {
        case Instruction::FNeg:
        case Instruction::FAdd:
        case Instruction::Sub:
        case Instruction::FSub:
        case Instruction::Mul:
        case Instruction::FMul:
        case Instruction::UDiv:
        case Instruction::SDiv:
        case Instruction::FDiv:
        case Instruction::URem:
        case Instruction::SRem:
        case Instruction::FRem:
        case Instruction::Shl:
        case Instruction::LShr:
        case Instruction::AShr:
        case Instruction::And:
        case Instruction::Or:
        case Instruction::Xor:
        case Instruction::GetElementPtr:
        case Instruction::Trunc:
        case Instruction::ZExt:
        case Instruction::SExt:
        case Instruction::FPToUI:
        case Instruction::FPToSI:
        case Instruction::UIToFP:
        case Instruction::SIToFP:
        case Instruction::FPTrunc:
        case Instruction::FPExt:
        case Instruction::PtrToInt:
        case Instruction::IntToPtr:
        case Instruction::BitCast:
        case Instruction::AddrSpaceCast:
        case Instruction::ICmp:
        case Instruction::FCmp:
        case Instruction::ExtractElement:
        case Instruction::InsertElement:
        case Instruction::ShuffleVector:
        case Instruction::ExtractValue:
        case Instruction::InsertValue:
        case Instruction::Alloca:
        case Instruction::PHI:
        case Instruction::Select:
            if (I.use_begin() == I.use_end()) {
                return true;
            }
            break;

        case Instruction::Load:
            li = dyn_cast<LoadInst>(&I);
            if (li && li->isVolatile()) {
                return false;
            } else if (I.use_begin() == I.use_end()) {
                return true;
            }
            break;

        default:
            return false;
            break;
    }

    return false;
}

class CSE_Bank {
private:

    class CSE_Bank_Entry {
    public:
        unsigned int opcode;
        Type *type;
        unsigned int num_operands;
        std::vector<Value *> operands;

        Instruction *inst;

        std::string owner;

        CSE_Bank_Entry(Instruction &I, std::string owner_bb) {
            opcode = I.getOpcode();
            type = I.getType();
            num_operands = I.getNumOperands();
            for (unsigned i = 0; i < num_operands; i++) {
                operands.push_back(I.getOperand(i));
            }

            inst = &I;

            owner = owner_bb;
        }

        ~CSE_Bank_Entry() {}
    };

    std::vector<CSE_Bank_Entry> bank;

    bool opcode_can_be_redundant(unsigned opcode) {
        switch(opcode) {
            case Instruction::FNeg:
            case Instruction::FAdd:
            case Instruction::Sub:
            case Instruction::FSub:
            case Instruction::Mul:
            case Instruction::FMul:
            case Instruction::UDiv:
            case Instruction::SDiv:
            case Instruction::FDiv:
            case Instruction::URem:
            case Instruction::SRem:
            case Instruction::FRem:
            case Instruction::Shl:
            case Instruction::LShr:
            case Instruction::AShr:
            case Instruction::And:
            case Instruction::Or:
            case Instruction::Xor:
            case Instruction::GetElementPtr:
            case Instruction::Trunc:
            case Instruction::ZExt:
            case Instruction::SExt:
            case Instruction::FPToUI:
            case Instruction::FPToSI:
            case Instruction::UIToFP:
            case Instruction::SIToFP:
            case Instruction::FPTrunc:
            case Instruction::FPExt:
            case Instruction::PtrToInt:
            case Instruction::IntToPtr:
            case Instruction::BitCast:
            case Instruction::AddrSpaceCast:
            case Instruction::ICmp:
            case Instruction::FCmp:
            case Instruction::ExtractElement:
            case Instruction::InsertElement:
            case Instruction::ShuffleVector:
            case Instruction::ExtractValue:
            case Instruction::InsertValue:
            // case Instruction::Alloca:
            case Instruction::PHI:
            case Instruction::Select:
                return true;
                break;

            default:
                return false;
                break;
        }

        return false;
    }

public:
    void clear_all() {
        bank.clear();
    }

    void clear_matching(std::string owner_to_erase) {
        bank.erase(
            std::remove_if(
                bank.begin(),
                bank.end(),
                [owner_to_erase](CSE_Bank_Entry &entry) {
                    return entry.owner == owner_to_erase;
                }),
            bank.end()
        );
    }

    Instruction *is_instruction_in_bank(Instruction &I) {
        bool operands_differ;
        unsigned opcode;
        ExtractValueInst *entry_ev, *inst_ev;
        CmpInst *entry_cmp, *inst_cmp;

        for (CSE_Bank_Entry &entry : bank) {
            if (entry.opcode       != I.getOpcode()     ) continue;
            if (entry.type         != I.getType()       ) continue;
            if (entry.num_operands != I.getNumOperands()) continue;

            operands_differ = false;
            for (unsigned i = 0; i < entry.num_operands; i++) {
                if (entry.operands[i] != I.getOperand(i)) {
                    operands_differ = true;
                    break;
                }
            }

            if (operands_differ) continue;

            opcode = I.getOpcode();

            switch (opcode) {
                case Instruction::ExtractValue:
                    entry_ev = dyn_cast<ExtractValueInst>(entry.inst);
                    inst_ev = dyn_cast<ExtractValueInst>(&I);

                    if (entry_ev->getNumIndices() != inst_ev->getNumIndices()) continue;

                    if (!std::equal(entry_ev->idx_begin(), entry_ev->idx_end(), inst_ev->idx_begin(), inst_ev->idx_end())) {
                        continue;
                    }
                    break;

                case Instruction::ICmp:
                case Instruction::FCmp:
                    entry_cmp = dyn_cast<CmpInst>(entry.inst);
                    inst_cmp = dyn_cast<CmpInst>(&I);

                    if (entry_cmp->getPredicate() != inst_cmp->getPredicate()) continue;
                    break;

                default:
                    break;
            }

            return entry.inst;
        }

        return nullptr;
    }

    void add_to_bank_if_legal(Instruction &I, std::string owner_bb) {
        if (this->opcode_can_be_redundant(I.getOpcode())) {
            bank.push_back(CSE_Bank_Entry(I, owner_bb));
        } else {
            // errs() << "Blocked adding a " << std::string(I.getOpcodeName()) << " to the bank\n";
        }
    }
};

void SimpleDeadCodeElimination(Module *M) {
    for (Function &F : M->functions()) {
        for (BasicBlock &BB : F) {
            auto I = BB.begin();

            while (I != BB.end()) {
                if (isDead(*I)) { // Simple DCE
                    I = I->eraseFromParent();
                    CSEDead++;
                } else {
                    I++;
                }
            }
        }
    }
}

void SimplifyInstructions(Module *M) {
    Value *simplified_instruction;

    for (Function &F : M->functions()) {
        for (BasicBlock &BB : F) {
            auto I = BB.begin();

            while (I != BB.end()) {
                simplified_instruction = simplifyInstruction(&*I, M->getDataLayout());

                if (simplified_instruction != nullptr && !PoisonValue::classof(simplified_instruction))  { // Simple constant folding
                    I->replaceAllUsesWith(simplified_instruction);
                    I = I->eraseFromParent();
                    CSESimplify++;
                } else {
                    I++;
                }
            }
        }
    }
}

void Recursive_MCSE_Helper(DomTreeNode *node, CSE_Bank &cse_bank) {
    BasicBlock *BB;
    std::string BB_Name;
    Instruction *cse_older_instruction;

    BB = node->getBlock();
    BB_Name = BB->getName().str();

    // Parse THIS basic block's code
    auto I = BB->begin();
    while(I != BB->end()) {
        cse_older_instruction = cse_bank.is_instruction_in_bank(*I);

        if (cse_older_instruction != nullptr) {
            I->replaceAllUsesWith(cse_older_instruction);
            I = I->eraseFromParent();
            CSEElim++;
        } else {
            cse_bank.add_to_bank_if_legal(*I, BB_Name);
            I++;
        }
    }

    // Iterate over children recursively
    for (DomTreeNodeBase<BasicBlock> **child = node->begin(); child != node->end(); child++) {
        Recursive_MCSE_Helper(*child, cse_bank);
    }

    // Once finished with children, clear this block from the cse_bank
    cse_bank.clear_matching(BB_Name);
}

void MatchingCommonSubexpressionElimination(Module *M) {
    CSE_Bank cse_bank;
    DominatorTreeBase<BasicBlock, false> *dominator_tree;
    DomTreeNode *tree_root;

    dominator_tree = new DominatorTree();

    for (Function &F : M->functions()) {
        if (F.begin() == F.end()) continue; // Function has no basic blocks, skip

        dominator_tree->recalculate(F);

        tree_root = dominator_tree->getRootNode();
        Recursive_MCSE_Helper(tree_root, cse_bank);
    }

    delete dominator_tree;
}

static void CommonSubexpressionElimination(Module *M) {
    SimpleDeadCodeElimination(M);
    SimplifyInstructions(M);
    MatchingCommonSubexpressionElimination(M);
}

