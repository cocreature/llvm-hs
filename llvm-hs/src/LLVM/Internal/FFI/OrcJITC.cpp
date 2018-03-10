#include "llvm/Support/Error.h"

#include "LLVM/Internal/FFI/OrcJIT.h"
#include "LLVM/Internal/FFI/Target.hpp"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileOnDemandLayer.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#include "llvm/ExecutionEngine/Orc/IRTransformLayer.h"
#include "llvm/ExecutionEngine/Orc/IndirectionUtils.h"
#include "llvm/ExecutionEngine/Orc/LambdaResolver.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/Mangler.h"

#include <type_traits>
#include <unordered_map>

#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm-c/Object.h"

using namespace llvm;
using namespace orc;

typedef unsigned LLVMModuleHandle;
typedef unsigned LLVMObjectHandle;
typedef llvm::orc::LambdaResolver<
    std::function<JITSymbol(const std::string &name)>,
    std::function<JITSymbol(const std::string &name)>>
    LLVMLambdaResolver;
typedef std::shared_ptr<LLVMLambdaResolver> *LLVMLambdaResolverRef;

// We want to allow users to choose themselves which layers they want to use.
// However, the LLVM API requires that this is selected statically via template
// arguments. We convert this static polymorphism to runtime polymorphism by
// creating an LinkingLayer and a CompileLayer class which use virtual dispatch
// to select the concrete layer.

template <typename T> class HandleSet {
  public:
    unsigned insert(T t) {
        unsigned handle = handles.insert({nextFree, t}).first->first;
        ++nextFree;
        return handle;
    }
    T &lookup(unsigned i) { return handles.at(i); }

    void remove(unsigned i) { handles.erase(i); }

  private:
    std::unordered_map<unsigned, T> handles;
    unsigned nextFree = 0;
};

class LinkingLayer {
  public:
    virtual ~LinkingLayer(){};
    typedef unsigned ObjHandleT;
    virtual Expected<ObjHandleT>
    addObject(std::shared_ptr<object::OwningBinary<object::ObjectFile>> object,
              std::shared_ptr<JITSymbolResolver> resolver) = 0;
    virtual Error removeObject(ObjHandleT H) = 0;
    virtual JITSymbol findSymbol(StringRef name, bool exportedSymbolsOnly) = 0;
    virtual JITSymbol findSymbolIn(ObjHandleT h, StringRef name,
                                   bool exportedSymbolsOnly) = 0;
    virtual void emitAndFinalize(ObjHandleT h) = 0;
};

template <typename T> class LinkingLayerT : public LinkingLayer {
  public:
    LinkingLayerT(T data_) : data(std::move(data_)) {}
    Expected<ObjHandleT>
    addObject(std::shared_ptr<object::OwningBinary<object::ObjectFile>> object,
              std::shared_ptr<JITSymbolResolver> resolver) override {
        if (auto handleOrErr =
                data.addObject(std::move(object), std::move(resolver))) {
            return handles.insert(*handleOrErr);
        } else {
            return handleOrErr.takeError();
        }
    }
    Error removeObject(ObjHandleT h) override {
        if (auto err = data.removeObject(handles.lookup(h))) {
            return err;
        } else {
            handles.remove(h);
            return err;
        }
    }
    JITSymbol findSymbol(StringRef name, bool exportedSymbolsOnly) override {
        return data.findSymbol(name, exportedSymbolsOnly);
    }
    JITSymbol findSymbolIn(ObjHandleT h, StringRef name,
                           bool exportedSymbolsOnly) override {
        return data.findSymbolIn(handles.lookup(h), name, exportedSymbolsOnly);
    }
    void emitAndFinalize(ObjHandleT h) override {
        data.emitAndFinalize(handles.lookup(h));
    }

  private:
    T data;
    HandleSet<typename T::ObjHandleT> handles;
};

class CompileLayer {
  public:
    typedef LLVMModuleHandle ModuleHandleT;
    virtual ~CompileLayer(){};
    virtual JITSymbol findSymbol(StringRef Name, bool ExportedSymbolsOnly) = 0;
    virtual JITSymbol findSymbolIn(ModuleHandleT H, StringRef Name,
                                   bool ExportedSymbolsOnly) = 0;
    virtual Expected<ModuleHandleT>
    addModule(std::shared_ptr<Module> Modules,
              std::shared_ptr<JITSymbolResolver> Resolver) = 0;
    virtual Error removeModule(ModuleHandleT H) = 0;
};

template <typename T> class CompileLayerT : public CompileLayer {
  public:
    template <typename... Arg>
    CompileLayerT(Arg &&... arg) : data{std::forward<Arg>(arg)...} {}
    JITSymbol findSymbol(StringRef Name, bool ExportedSymbolsOnly) override {
        return data.findSymbol(Name, ExportedSymbolsOnly);
    }
    JITSymbol findSymbolIn(ModuleHandleT H, StringRef Name,
                           bool ExportedSymbolsOnly) override {
        return data.findSymbolIn(handles.lookup(H), Name, ExportedSymbolsOnly);
    }
    Expected<ModuleHandleT>
    addModule(std::shared_ptr<Module> Module,
              std::shared_ptr<JITSymbolResolver> Resolver) override {
        if (auto handleOrErr =
                data.addModule(std::move(Module), std::move(Resolver))) {
            return handles.insert(*handleOrErr);
        } else {
            return handleOrErr.takeError();
        }
    }
    Error removeModule(ModuleHandleT H) override {
        auto handle = handles.lookup(H);
        handles.remove(H);
        return data.removeModule(handle);
    }

  private:
    T data;
    HandleSet<typename T::ModuleHandleT> handles;
};

typedef llvm::orc::CompileOnDemandLayer<CompileLayer> LLVMCompileOnDemandLayer;
typedef LLVMCompileOnDemandLayer *LLVMCompileOnDemandLayerRef;

typedef llvm::orc::IRTransformLayer<
    CompileLayer,
    std::function<std::shared_ptr<Module>(std::shared_ptr<Module>)>>
    LLVMIRTransformLayer;

typedef llvm::orc::JITCompileCallbackManager *LLVMJITCompileCallbackManagerRef;

typedef llvm::JITSymbol *LLVMJITSymbolRef;

typedef llvm::orc::IndirectStubsManager *LLVMIndirectStubsManagerRef;
typedef std::function<std::unique_ptr<llvm::orc::IndirectStubsManager>()>
    *LLVMIndirectStubsManagerBuilderRef;

static std::string mangle(StringRef name, LLVMTargetDataRef dataLayout) {
    std::string mangledName;
    {
        raw_string_ostream mangledNameStream(mangledName);
        Mangler::getNameWithPrefix(mangledNameStream, name,
                                   *unwrap(dataLayout));
    }
    return mangledName;
}

extern "C" {

/* Constructor functions for the different compile layers */

CompileLayer *LLVM_Hs_createIRCompileLayer(LinkingLayer *linkingLayer,
                                           LLVMTargetMachineRef tm) {
    TargetMachine *tmm = unwrap(tm);
    return new CompileLayerT<IRCompileLayer<LinkingLayer, SimpleCompiler>>(
        IRCompileLayer<LinkingLayer, SimpleCompiler>(*linkingLayer,
                                                     SimpleCompiler(*tmm)));
}

CompileLayer *LLVM_Hs_createCompileOnDemandLayer(
    CompileLayer *compileLayer,
    void (*partitioningFtor)(llvm::Function *, std::set<llvm::Function *> *set),
    LLVMJITCompileCallbackManagerRef callbackManager,
    LLVMIndirectStubsManagerBuilderRef stubsManager,
    LLVMBool cloneStubsIntoPartitions) {
    return new CompileLayerT<LLVMCompileOnDemandLayer>(
        *compileLayer,
        [partitioningFtor](llvm::Function &f) -> std::set<llvm::Function *> {
            std::set<llvm::Function *> result;
            partitioningFtor(&f, &result);
            return result;
        },
        *callbackManager, *stubsManager,
        static_cast<bool>(cloneStubsIntoPartitions));
}

CompileLayer *LLVM_Hs_createIRTransformLayer(CompileLayer *compileLayer,
                                             Module *(*transform)(Module *)) {
    std::function<std::shared_ptr<Module>(std::shared_ptr<Module>)> transform_ =
        [transform](std::shared_ptr<Module> module) {
            return std::shared_ptr<Module>(transform(module.get()));
        };
    return new CompileLayerT<LLVMIRTransformLayer>(*compileLayer, transform_);
}

/* Functions that work on all compile layers */

void LLVM_Hs_CompileLayer_dispose(CompileLayer *compileLayer) {
    delete compileLayer;
}

LLVMJITSymbolRef LLVM_Hs_CompileLayer_findSymbol(CompileLayer *compileLayer,
                                                 const char *name,
                                                 LLVMBool exportedSymbolsOnly) {
    JITSymbol symbol = compileLayer->findSymbol(name, exportedSymbolsOnly);
    return new JITSymbol(std::move(symbol));
}

LLVMJITSymbolRef
LLVM_Hs_CompileLayer_findSymbolIn(CompileLayer *compileLayer,
                                  LLVMModuleHandle handle, const char *name,
                                  LLVMBool exportedSymbolsOnly) {
    JITSymbol symbol =
        compileLayer->findSymbolIn(handle, name, exportedSymbolsOnly);
    return new JITSymbol(std::move(symbol));
}

LLVMModuleHandle LLVM_Hs_CompileLayer_addModule(CompileLayer *compileLayer,
                                                LLVMTargetDataRef dataLayout,
                                                LLVMModuleRef module,
                                                LLVMLambdaResolverRef resolver,
                                                char **errorMessage) {
    std::shared_ptr<Module> mod{unwrap(module), [](Module *) {}};
    if (mod->getDataLayout().isDefault()) {
        mod->setDataLayout(*unwrap(dataLayout));
    }
    if (auto handleOrErr = compileLayer->addModule(std::move(mod), *resolver)) {
        *errorMessage = nullptr;
        return *handleOrErr;
    } else {
        std::string errString = toString(handleOrErr.takeError());
        *errorMessage = strdup(errString.c_str());
        return 0;
    }
}

void LLVM_Hs_CompileLayer_removeModule(CompileLayer *compileLayer,
                                       LLVMModuleHandle moduleSetHandle) {
    if (compileLayer->removeModule(moduleSetHandle)) {
        // TODO handle failure
    }
}

/* Constructor functions for the different object layers */

LinkingLayer *LLVM_Hs_createObjectLinkingLayer() {
    return new LinkingLayerT<RTDyldObjectLinkingLayer>(RTDyldObjectLinkingLayer(
        []() { return std::make_shared<SectionMemoryManager>(); }));
}

/* Fuctions that work on all object layers */

void LLVM_Hs_LinkingLayer_dispose(LinkingLayer *linkingLayer) {
    delete linkingLayer;
}

void LLVM_Hs_disposeJITSymbol(LLVMJITSymbolRef symbol) { delete symbol; }

LLVMLambdaResolverRef LLVM_Hs_createLambdaResolver(
    void (*dylibResolver)(const char *, LLVMJITSymbolRef),
    void (*externalResolver)(const char *, LLVMJITSymbolRef)) {
    std::function<JITSymbol(const std::string &name)> dylibResolverFun =
        [dylibResolver](const std::string &name) -> JITSymbol {
        JITSymbol symbol(nullptr);
        dylibResolver(name.c_str(), &symbol);
        return symbol;
    };
    std::function<JITSymbol(const std::string &name)> externalResolverFun =
        [externalResolver](const std::string &name) -> JITSymbol {
        JITSymbol symbol(nullptr);
        externalResolver(name.c_str(), &symbol);
        return symbol;
    };
    return new std::shared_ptr<LLVMLambdaResolver>(
        createLambdaResolver(dylibResolverFun, externalResolverFun));
}

void LLVM_Hs_disposeLambdaResolver(LLVMLambdaResolverRef resolver) {
    delete resolver;
}

LLVMObjectHandle LLVM_Hs_LinkingLayer_addObject(LinkingLayer *linkLayer,
                                                LLVMObjectFileRef  obj,
                                                LLVMLambdaResolverRef resolver,
                                                char **errorMessage) {
    auto obj2 = reinterpret_cast<object::OwningBinary<object::ObjectFile> *>(obj);
    auto obj3 = std::move(*obj2);
    auto obj4 = std::make_shared<object::OwningBinary<object::ObjectFile>>(std::move(obj3));
    if (auto handleOrErr = linkLayer->addObject(std::move(obj4), *resolver)) {
        *errorMessage = nullptr;
        return *handleOrErr;
    } else {
        std::string errString = toString(handleOrErr.takeError());
        *errorMessage = strdup(errString.c_str());
        return 0;
    }
}

static JITSymbolFlags unwrap(LLVMJITSymbolFlags f) {
    JITSymbolFlags flags = JITSymbolFlags::None;
#define ENUM_CASE(x)                                                           \
    if (f & LLVMJITSymbolFlag##x)                                              \
        flags |= JITSymbolFlags::x;
    LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(ENUM_CASE)
#undef ENUM_CASE
    return flags;
}

static LLVMJITSymbolFlags wrap(JITSymbolFlags f) {
    unsigned r = 0;
#define ENUM_CASE(x)                                                           \
    if ((char)(f & JITSymbolFlags::x))                                         \
        r |= (unsigned)LLVMJITSymbolFlag##x;
    LLVM_HS_FOR_EACH_JIT_SYMBOL_FLAG(ENUM_CASE)
#undef ENUM_CASE
    return LLVMJITSymbolFlags(r);
}

JITTargetAddress LLVM_Hs_JITSymbol_getAddress(LLVMJITSymbolRef symbol,
                                              char **errorMessage) {
    *errorMessage = nullptr;
    if (auto addrOrErr = symbol->getAddress()) {
        return *addrOrErr;
    } else {
        std::string error = toString(addrOrErr.takeError());
        *errorMessage = strdup(error.c_str());
        return 0;
    }
}

LLVMJITSymbolFlags LLVM_Hs_JITSymbol_getFlags(LLVMJITSymbolRef symbol) {
    return wrap(symbol->getFlags());
}

void LLVM_Hs_setJITSymbol(LLVMJITSymbolRef symbol, JITTargetAddress addr,
                          LLVMJITSymbolFlags flags) {
    *symbol = JITSymbol(addr, unwrap(flags));
}

void LLVM_Hs_getMangledSymbol(char **mangledSymbol, const char *symbol,
                              LLVMTargetDataRef dataLayout) {
    std::string mangled = mangle(symbol, dataLayout);
    *mangledSymbol = new char[mangled.size() + 1];
    strcpy(*mangledSymbol, mangled.c_str());
}

void LLVM_Hs_disposeMangledSymbol(char *mangledSymbol) {
    delete[] mangledSymbol;
}

LLVMJITCompileCallbackManagerRef
LLVM_Hs_createLocalCompileCallbackManager(const char *triple,
                                          JITTargetAddress errorHandler) {
    // We copy the string so that it can be freed on the Haskell side.
    std::string tripleStr(triple);
    return llvm::orc::createLocalCompileCallbackManager(
               Triple(std::move(tripleStr)), errorHandler)
        .release();
}

void LLVM_Hs_disposeCallbackManager(
    LLVMJITCompileCallbackManagerRef callbackManager) {
    delete callbackManager;
}

LLVMIndirectStubsManagerBuilderRef
LLVM_Hs_createLocalIndirectStubsManagerBuilder(const char *triple) {
    // We copy the string so that it can be freed on the Haskell side.
    std::string tripleStr(triple);
    return new std::function<std::unique_ptr<IndirectStubsManager>()>(
        llvm::orc::createLocalIndirectStubsManagerBuilder(
            Triple(std::move(tripleStr))));
}

void LLVM_Hs_disposeIndirectStubsManagerBuilder(
    LLVMIndirectStubsManagerBuilderRef stubsManager) {
    delete stubsManager;
}

void LLVM_Hs_insertFun(std::set<llvm::Function *> *set, llvm::Function *f) {
    set->insert(f);
}
}
