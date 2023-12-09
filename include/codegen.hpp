#ifndef CC_PROJECT_CODEGEN_H
#define CC_PROJECT_CODEGEN_H

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
#include "AST.hpp"
#include "semanticAnalyser.hpp"

namespace Codegen {

void InitCodegenContext(std::string moduleName, FILE *output);
void DestroyCodegenContext();
void RunCodegenPass(Program *program);
std::map<TypeID, llvm::Type *> &GetTypesMap();
std::map<TypeID, size_t> &GetTypesSizeMap();

llvm::Value *ValueOfExpression(Expression *expression);

int InsertLLVMValues(llvm::IRBuilder<> *builder, Semantics::Scope *scope, Expression *expression);
} // namespace Codegen

#endif // CC_PROJECT_CODEGEN_H
