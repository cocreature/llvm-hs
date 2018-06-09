#include <llvm/IR/PassManager.h>
#include <llvm/Transforms/Utils/Mem2Reg.h>

#define DEFINE_DISPOSE(name) void dispose##name(llvm::name*

#define DEFINE_MANAGER_BINDINGS(name)                                          \
    llvm::name *create##name(LLVMBool debugLogging) {                          \
        return new llvm::name(debugLogging);                                   \
    }                                                                          \
    void dispose##name(llvm::name *v) { delete v; }

llvm::PreservedAnalyses *
modulePassManagerRun(llvm::FunctionPassManager *passManager,
                     llvm::Function *module,
                     llvm::FunctionAnalysisManager *analysisManager) {
    return new llvm::PreservedAnalyses(
        passManager->run(*module, *analysisManager));
}

DEFINE_MANAGER_BINDINGS(FunctionPassManager)
DEFINE_MANAGER_BINDINGS(FunctionAnalysisManager)
DEFINE_MANAGER_BINDINGS(ModulePassManager)
DEFINE_MANAGER_BINDINGS(ModuleAnalysisManager)

class FunctionPass {
  public:
    virtual llvm::PreservedAnalyses
    run(llvm::Function &module,
        llvm::FunctionAnalysisManager &analysisManager) = 0;
    static llvm::StringRef name() { return "fuck you LLVM"; }
};

template <typename T> struct FunctionPassT : FunctionPass {

    FunctionPassT(T *data_) : data(data_) {}

    llvm::PreservedAnalyses
    run(llvm::Function &module,
        llvm::FunctionAnalysisManager &analysisManager) override {
        return data->run(module, analysisManager);
    }

  private:
    T *data;
};

void functionPassManagerAddPass(llvm::PassManager<llvm::Function> *passManager,
                                FunctionPass *pass) {
    passManager->addPass(FunctionPassT<FunctionPass>(pass));
}

class ModulePass {
  public:
    virtual llvm::PreservedAnalyses
    run(llvm::Module &module, llvm::ModuleAnalysisManager &analysisManager) = 0;
    static llvm::StringRef name() { return "fuck you LLVM"; }
};

template <typename T> struct ModulePassT : ModulePass {

    ModulePassT(T *data_) : data(data_) {}

    llvm::PreservedAnalyses
    run(llvm::Module &module,
        llvm::ModuleAnalysisManager &analysisManager) override {
        return data->run(module, analysisManager);
    }

  private:
    T *data;
};

void modulePassManagerAddPass(llvm::PassManager<llvm::Module> *passManager,
                              ModulePass *pass) {
    passManager->addPass(ModulePassT<ModulePass>(pass));
}

#define GEN_PASS_BINDINGS(name)                                                \
    void dispose##name(llvm::name *pass) { delete pass; }                      \
    FunctionPass *wrap##name(llvm::name *pass) {                               \
        return new FunctionPassT<llvm::name>(pass);                            \
    }

#define GEN_ALL_PASS_BINDINGS(name)                                            \
    llvm::name *create##name() { return new llvm::name(); }                    \
    void dispose##name(llvm::name *pass) { delete pass; }                      \
    FunctionPass *wrap##name(llvm::name *pass) {                               \
        return new FunctionPassT<llvm::name>(pass);                            \
    }

GEN_ALL_PASS_BINDINGS(PromotePass)

typedef llvm::ModuleToFunctionPassAdaptor<FunctionPassT<FunctionPass>>
    ModuleToFunctionPassAdaptor;

ModuleToFunctionPassAdaptor *
createModuleToFunctionPassAdaptor(FunctionPass *pass) {
    return new ModuleToFunctionPassAdaptor(FunctionPassT<FunctionPass>(pass));
}

void disposeModuleToFunctionPassAdaptor(ModuleToFunctionPassAdaptor *adaptor) {
    delete adaptor;
}

ModulePass *
wrapModuleToFunctionPassAdaptor(ModuleToFunctionPassAdaptor *adaptor) {
    return new ModulePassT<ModuleToFunctionPassAdaptor>(adaptor);
}
