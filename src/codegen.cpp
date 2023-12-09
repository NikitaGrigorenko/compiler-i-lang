#include "codegen.hpp"
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include "semanticAnalyser.hpp"

using namespace Codegen;

const auto objectHeaderSize = 4;

FILE *moduleOutputFile;
llvm::LLVMContext *llvmContext = nullptr;
llvm::Module *llvmModule = nullptr;
llvm::IRBuilder<> *llvmBuilder = nullptr;
llvm::Function *currentLlvmFunction = nullptr;

// Map from defined types to llvm-declared types
std::map<TypeID, llvm::Type *> llvmTypesMap;
// Map from defined types to llvm-declared size
std::map<TypeID, size_t> llvmTypeSizeMap;
// Map from function to sys function
std::map<std::string, llvm::FunctionCallee> llvmSystemFunctions;

int GenerateScopeBody(Body *body, std::string currentInsertPoint);

/**
 * The function initializes the code generation context by setting the module
 * name and output file, and creating the LLVM context, module, and IR builder.
 *
 * @param moduleName The name of the module being generated. This is typically a
 * string that represents the name of the file or program being compiled.
 * @param output The "output" parameter is a pointer to a FILE object, which
 * represents the output file where the generated code will be written. If the
 * "output" parameter is NULL, the code will be written to the standard output
 * (stdout).
 */
void Codegen::InitCodegenContext(std::string moduleName, FILE *output) {
    if (!output) {
        output = stdout;
    }
    moduleOutputFile = output;
    llvmContext = new llvm::LLVMContext();
    llvmModule = new llvm::Module(moduleName, *llvmContext);
    llvmBuilder = new llvm::IRBuilder<>(*llvmContext);
}

/**
 * The function "DestroyCodegenContext" clears maps and deletes objects related
 * to LLVM code generation.
 */
void Codegen::DestroyCodegenContext() {
    llvmTypeSizeMap.clear();
    llvmTypesMap.clear();
    delete llvmBuilder;
    delete llvmModule;
    delete llvmContext;
}

/**
 * The function GetCurrentLlvmFunction returns the current LLVM function.
 *
 * @return a pointer to an llvm::Function object.
 */
llvm::Function *GetCurrentLlvmFunction() {
    return currentLlvmFunction;
}

/**
 * The function returns a reference to a map that maps TypeID to llvm::Type*.
 *
 * @return A reference to a `std::map` object with keys of type `TypeID` and
 * values of type `llvm::Type*`.
 */
std::map<TypeID, llvm::Type *> &Codegen::GetTypesMap() {
    return llvmTypesMap;
}

/**
 * The function returns a reference to a map that maps TypeIDs to their
 * corresponding sizes.
 *
 * @return A reference to a `std::map` object with keys of type `TypeID` and
 * values of type `size_t`.
 */
std::map<TypeID, size_t> &Codegen::GetTypesSizeMap() {
    return llvmTypeSizeMap;
}

/**
 * The function "AddBuiltinFunctionDefinitions" defines a built-in function
 * called "print" that allows printing something in the terminal.
 *
 * @return an integer value of 0.
 */
int AddBuiltinFunctionDefinitions() {
    { // Function that allows print something in terminal (print)
        auto printSignature = "print";
        auto printParams = std::vector<llvm::Type *>();
        printParams.push_back(llvmBuilder->getInt32Ty());
        auto printRetType = llvm::Type::getVoidTy(*llvmContext);
        auto printFuncLinkage = llvm::Function::ExternalLinkage;
        auto printFuncType = llvm::FunctionType::get(printRetType, printParams, false);
        auto printFunc
            = llvm::Function::Create(printFuncType, printFuncLinkage, printSignature, llvmModule);
        auto printEntry = llvm::BasicBlock::Create(*llvmContext, "entrypoint", printFunc);
        llvmBuilder->SetInsertPoint(printEntry);
        auto formatStringRef = llvm::StringRef("%d\n");
        auto formatString = llvmBuilder->CreateGlobalStringPtr(formatStringRef, "$_print_int");

        auto argsIterator = printFunc->arg_begin();
        auto arg1Value = static_cast<llvm::Value *>(argsIterator++);
        arg1Value->setName("_int_print");

        auto printfArgs = std::vector<llvm::Value *>();
        printfArgs.push_back(formatString);
        printfArgs.push_back(arg1Value);
        auto printfFunction = llvmSystemFunctions["printf"];
        llvmBuilder->CreateCall(printfFunction, printfArgs, "printf.ret");
        llvmBuilder->CreateRetVoid();
        Semantics::GetGlobalScope()->funcs()->lookupFunc("print")->setLLVMFunction(printFunc);
    }
    return 0;
}

int GenerateType(Semantics::Scope *scope, Semantics::Type *typeToGen);

/**
 * The function generates a variable and assigns an initial value if
 * provided.
 *
 * @param scope A pointer to an object of type Semantics::Scope, which
 * represents the scope in which the variable declaration is being made.
 * @param varDeclaration A pointer to a VariableDeclaration object, which
 * contains information about a variable declaration in the code.
 *
 * @return an integer value of 0.
 */
int GenerateVariable(Semantics::Scope *scope, VariableDeclaration *varDeclaration) {
    // TODO: test this case with !varDeclaration->type->isArray.
    // May be some errors (because of workaround here)
    if (varDeclaration->initialValue && !varDeclaration->type->isArray) {
        // For all native types and their offsprings we set initial value
        InsertLLVMValues(llvmBuilder, scope, varDeclaration->initialValue);
        auto rLlvmValue = ValueOfExpression(varDeclaration->initialValue);
        Semantics::Name::ofVariableDeclaration(varDeclaration)->setLLVMValue(scope, rLlvmValue);
    } else if (varDeclaration->type->isArray) {
        auto elementType = varDeclaration->type->getTypeId();
        auto llvmElementType = llvmTypesMap[elementType];
        auto arraySize = 12;

        auto llvmArrayType = llvm::ArrayType::get(llvmElementType, arraySize);
        auto llvmArrayPtr = llvmBuilder->CreateAlloca(llvmArrayType, nullptr, "array_ptr");

        Semantics::Name::ofVariableDeclaration(varDeclaration)->setLLVMValue(scope, llvmArrayPtr);
    }
    return 0;
};

/**
 * The function "GenerateType" takes a scope and a type declaration, converts
 * the type declaration into a type, and then calls another function with the
 * scope and the generated type.
 *
 * @param scope The "scope" parameter is a pointer to an object of type
 * "Semantics::Scope". It represents the scope in which the type declaration is
 * being generated.
 * @param typeDeclaration TypeDeclaration is a pointer to an object of type
 * TypeDeclaration.
 *
 * @return an integer value.
 */
int GenerateType(Semantics::Scope *scope, TypeDeclaration *typeDeclaration) {
    auto typeToGen = Semantics::Type::ofTypeDeclaration(typeDeclaration);
    return GenerateType(scope, typeToGen);
}

/**
 * The function `GenerateType` generates LLVM types for primitive
 *
 * @param scope A pointer to an object of type `Semantics::Scope`, which
 * represents the current scope in the program.
 * @param typeToGen A pointer to an object of type `Semantics::Type` that
 * represents the type to be generated.
 *
 * @return an integer value.
 */
int GenerateType(Semantics::Scope *scope, Semantics::Type *typeToGen) {
    return 0;
}

/**
 * The function `GenerateExpressionStatement` inserts LLVM values for binary
 * expressions or callable primary expressions in a given scope.
 *
 * @param scope A pointer to an object of type Semantics::Scope, which
 * represents the current scope in the program.
 * @param statement A pointer to an object of type ExpressionStatement, which
 * represents an expression statement in the code.
 *
 * @return an integer value.
 */
int GenerateExpressionStatement(Semantics::Scope *scope, ExpressionStatement *statement) {
    if (statement->expression->isBinary) {
        // Only binary expression which can be contained is assignment operation
        return InsertLLVMValues(llvmBuilder, scope, statement->expression);
    }
    if (statement->expression->isPrimary) {
        // Only callable primary are allowed to be written
        return InsertLLVMValues(llvmBuilder, scope,
                                static_cast<CallablePrimary *>(statement->expression));
    }
    return -1;
}

/**
 * The function generates a return statement in LLVM code for a given
 * expression.
 *
 * @param scope The "scope" parameter is a pointer to an object of type
 * "Semantics::Scope". This object represents the current scope or context in
 * which the return statement is being executed. It contains information about
 * the variables, functions, and other symbols that are accessible in that
 * scope.
 * @param statement The "statement" parameter is a pointer to a ReturnStatement
 * object.
 *
 * @return an integer value of 0.
 */
int GenerateReturnStatement(Semantics::Scope *scope, ReturnStatement *statement) {
    InsertLLVMValues(llvmBuilder, scope, statement->expressionToReturn);
    llvmBuilder->CreateRet(ValueOfExpression(statement->expressionToReturn));
    return 0;
}

/**
 * The function generates LLVM code for an if statement
 *
 * @param scope The "scope" parameter is a pointer to an object of type
 * "Semantics::Scope". It represents the scope in which the if statement is
 * defined. A scope is a container for variables and their values, and it
 * provides a way to look up and manipulate variables within a specific context.
 * @param statement The "statement" parameter is of type "IfStatement" and
 * represents an if statement in the code. It contains the expression to be
 * evaluated and the bodies of the true and false branches of the if statement.
 *
 * @return an integer value of 0.
 */
int GenerateIfStatement(Semantics::Scope *scope, IfStatement *statement) {
    auto ifBeginningBlock = llvmBuilder->GetInsertBlock();
    InsertLLVMValues(llvmBuilder, scope, statement->expression);
    auto expressionValue = ValueOfExpression(statement->expression);
    auto trueName = scope->getScopeName() + "_true_branch_"
                    + std::to_string(GetAndIncrementUniqueCounter(SCOPE_COUNT));
    auto trueBlock = llvm::BasicBlock::Create(*llvmContext, trueName, GetCurrentLlvmFunction());
    llvm::BasicBlock *endBlock, *endTrueBlock = nullptr, *endFalseBlock = nullptr;
    llvm::BasicBlock *falseBlock = nullptr;
    std::string falseName;
    if (statement->falseBody) {
        auto falseCounter = GetAndIncrementUniqueCounter(SCOPE_COUNT);
        auto endCounter = GetAndIncrementUniqueCounter(SCOPE_COUNT);
        auto endName = scope->getScopeName() + "_end_" + std::to_string(endCounter);
        endBlock = llvm::BasicBlock::Create(*llvmContext, endName, GetCurrentLlvmFunction());
        falseName = scope->getScopeName() + "_false_branch_" + std::to_string(falseCounter);
        falseBlock
            = llvm::BasicBlock::Create(*llvmContext, falseName, GetCurrentLlvmFunction(), endBlock);
        llvmBuilder->CreateCondBr(expressionValue, trueBlock, falseBlock);
    } else {
        auto endName = scope->getScopeName() + "_end_"
                       + std::to_string(GetAndIncrementUniqueCounter(SCOPE_COUNT));
        endBlock = llvm::BasicBlock::Create(*llvmContext, endName, GetCurrentLlvmFunction());
        llvmBuilder->CreateCondBr(expressionValue, trueBlock, endBlock);
    }
    llvmBuilder->SetInsertPoint(trueBlock);
    GenerateScopeBody(statement->trueBody, trueName);
    llvmBuilder->CreateBr(endBlock);
    endTrueBlock = llvmBuilder->GetInsertBlock();
    if (statement->falseBody) {
        llvmBuilder->SetInsertPoint(falseBlock);
        GenerateScopeBody(statement->falseBody, falseName);
        llvmBuilder->CreateBr(endBlock);
        endFalseBlock = llvmBuilder->GetInsertBlock();
    }
    llvmBuilder->SetInsertPoint(endBlock);
    auto valueUpdateFunc = [=](llvm::PHINode *phi, Semantics::Name *fallthroughName) {
        bool fromThisScope = false;
        auto nameToFall = scope->names()->lookupName(fallthroughName->getName(), &fromThisScope);
        if (nameToFall != fallthroughName) {
        }
        nameToFall->setLLVMValue(scope, static_cast<llvm::Value *>(phi));
        if (!fromThisScope) {
            // Parent scope fallthrough values append by value
            scope->insertNameFallthrough(fallthroughName);
        }
    };
    auto trueScope = Semantics::Scope::ofBody(statement->trueBody);
    auto trueCallbacksList = trueScope->getAllFallthroughNames();
    if (statement->falseBody) {
        auto falseScope = Semantics::Scope::ofBody(statement->falseBody);
        auto falseCallbacksList = falseScope->getAllFallthroughNames();
        std::map<Semantics::Name *, bool> trueCallbackBitmap;
        std::map<Semantics::Name *, bool> falseCallbackBitmap;
        for (auto name : trueCallbacksList) {
            trueCallbackBitmap[name] = true;
        }
        for (auto name : falseCallbacksList) {
            falseCallbackBitmap[name] = true;
        }
        for (auto fallthroughName : trueCallbacksList) {
            auto nameType = GetTypesMap()[fallthroughName->getType()->getType()];
            auto trueValue = fallthroughName->getLLVMValue(trueScope);
            auto phi = llvmBuilder->CreatePHI(nameType, 2);
            if (falseCallbackBitmap.count(fallthroughName) > 0) {
                auto falseValue = fallthroughName->getLLVMValue(falseScope);
                phi->addIncoming(trueValue, endTrueBlock);
                phi->addIncoming(falseValue, falseBlock);
            } else {
                auto parentValue = fallthroughName->getLLVMValue(scope);
                phi->addIncoming(parentValue, ifBeginningBlock);
                phi->addIncoming(trueValue, endTrueBlock);
            }
            valueUpdateFunc(phi, fallthroughName);
        }
        for (auto fallthroughName : trueCallbacksList) {
            if (trueCallbackBitmap.count(fallthroughName) < 1) {
                auto nameType = GetTypesMap()[fallthroughName->getType()->getType()];
                auto falseValue = fallthroughName->getLLVMValue(falseScope);
                auto parentValue = fallthroughName->getLLVMValue(scope);
                auto phi = llvmBuilder->CreatePHI(nameType, 2);
                phi->addIncoming(parentValue, ifBeginningBlock);
                phi->addIncoming(falseValue, endFalseBlock);
                valueUpdateFunc(phi, fallthroughName);
            }
        }
    } else {
        for (auto fallthroughName : trueCallbacksList) {
            auto nameType = GetTypesMap()[fallthroughName->getType()->getType()];
            auto trueValue = fallthroughName->getLLVMValue(trueScope);
            auto parentValue = fallthroughName->getLLVMValue(scope);
            auto phi = llvmBuilder->CreatePHI(nameType, 2);
            phi->addIncoming(parentValue, ifBeginningBlock);
            phi->addIncoming(trueValue, endTrueBlock);
            valueUpdateFunc(phi, fallthroughName);
        }
    }
    return 0;
}

/**
 * The function generates LLVM code for a while loop statement, including the
 * expression block, body block, and end block.
 *
 * @param scope The "scope" parameter is a pointer to an object of type
 * Semantics::Scope. It represents the current scope in which the while loop
 * statement is being generated. A scope is a container for variables and their
 * values, and it provides a way to look up and manipulate variables within a
 * specific scope.
 * @param statement The "statement" parameter in the code represents a While
 * Loop statement. It is of type "WhileLoopStatement" and contains information
 * about the condition expression and the body of the loop.
 *
 * @return an integer value of 0.
 */
int GenerateWhileLoop(Semantics::Scope *scope, WhileLoopStatement *statement) {
    auto beginningBlock = llvmBuilder->GetInsertBlock();
    auto expressionBlockName = scope->getScopeName() + "_exp_"
                               + std::to_string(GetAndIncrementUniqueCounter(SCOPE_COUNT));
    auto expressionBlock
        = llvm::BasicBlock::Create(*llvmContext, expressionBlockName, GetCurrentLlvmFunction());
    auto bodyBlockName = scope->getScopeName() + "_while_"
                         + std::to_string(GetAndIncrementUniqueCounter(SCOPE_COUNT));
    auto bodyBlock
        = llvm::BasicBlock::Create(*llvmContext, bodyBlockName, GetCurrentLlvmFunction());
    auto endBlockName = scope->getScopeName() + "_end_"
                        + std::to_string(GetAndIncrementUniqueCounter(SCOPE_COUNT));
    auto endBlock = llvm::BasicBlock::Create(*llvmContext, endBlockName, GetCurrentLlvmFunction());

    llvmBuilder->SetInsertPoint(beginningBlock);
    llvmBuilder->CreateBr(expressionBlock);
    llvmBuilder->SetInsertPoint(bodyBlock);
    llvmBuilder->SetInsertPoint(endBlock);

    // Body genration to found out which outer scope variables are affected inside
    // the loop
    GenerateScopeBody(statement->body, bodyBlockName);
    llvmBuilder->ClearInsertionPoint();

    auto bodyScope = Semantics::Scope::ofBody(statement->body);
    auto bodyCallbacksList = bodyScope->getAllFallthroughNames();

    std::map<Semantics::Name *, llvm::PHINode *> fallthroughValues;
    llvmBuilder->SetInsertPoint(expressionBlock);
    for (auto fallthroughName : bodyCallbacksList) {
        auto nameType = GetTypesMap()[fallthroughName->getType()->getType()];
        auto parentValue = fallthroughName->getLLVMValue(scope);
        auto phi = llvmBuilder->CreatePHI(nameType, 2);
        phi->addIncoming(parentValue, beginningBlock);
        fallthroughValues[fallthroughName] = phi;

        bool fromThisScope = false;
        auto nameToFall
            = bodyScope->names()->lookupName(fallthroughName->getName(), &fromThisScope);
        nameToFall->setLLVMValue(bodyScope, static_cast<llvm::Value *>(phi));
    }
    InsertLLVMValues(llvmBuilder, bodyScope, statement->expression);
    auto expressionValue = ValueOfExpression(statement->expression);
    llvmBuilder->CreateCondBr(expressionValue, bodyBlock, endBlock);

    // Body generation
    llvmBuilder->SetInsertPoint(bodyBlock);
    GenerateScopeBody(statement->body, bodyBlockName);
    for (auto fallthroughName : bodyCallbacksList) {
        auto bodyValue = fallthroughName->getLLVMValue(bodyScope);
        fallthroughValues[fallthroughName]->addIncoming(bodyValue, bodyBlock);
        bool fromThisScope = false;
        auto nameToFall
            = bodyScope->names()->lookupName(fallthroughName->getName(), &fromThisScope);
        nameToFall->setLLVMValue(bodyScope, bodyValue);
        if (!fromThisScope) {
            // Parent scope fallthrough values appending by value
            bodyScope->insertNameFallthrough(fallthroughName);
        }
    }
    llvmBuilder->CreateBr(expressionBlock);

    // Variable fallthrough finalizing
    llvmBuilder->SetInsertPoint(endBlock);
    for (auto fallthroughName : bodyCallbacksList) {
        auto phiValue = fallthroughValues[fallthroughName];
        bool fromThisScope = false;
        auto nameToFall = scope->names()->lookupName(fallthroughName->getName(), &fromThisScope);
        nameToFall->setLLVMValue(scope, phiValue);
        if (!fromThisScope) {
            scope->insertNameFallthrough(fallthroughName);
        }
    }
    return 0;
}

/**
 * The function generates LLVM code for for loop statement.
 *
 * @param scope The "scope" parameter is a pointer to an object of type
 * Semantics::Scope. It represents the scope in which the for loop statement is
 * defined. A scope is a container for variables and other symbols that are
 * accessible within a certain block of code.
 * @param statement The "statement" parameter is of type "ForLoopStatement",
 * which represents a for loop statement in the code. It contains information
 * about the loop range, loop body, and loop identifier.
 *
 * @return an integer value of 0.
 */
int GenerateForLoop(Semantics::Scope *scope, ForLoopStatement *statement) {
    auto beginningBlock = llvmBuilder->GetInsertBlock();

    auto bodyBlockName = scope->getScopeName() + "_for_body_"
                         + std::to_string(GetAndIncrementUniqueCounter(SCOPE_COUNT));
    auto bodyBlock
        = llvm::BasicBlock::Create(*llvmContext, bodyBlockName, GetCurrentLlvmFunction());
    auto endBlockName = scope->getScopeName() + "_end_"
                        + std::to_string(GetAndIncrementUniqueCounter(SCOPE_COUNT));
    auto endBlock = llvm::BasicBlock::Create(*llvmContext, endBlockName, GetCurrentLlvmFunction());

    InsertLLVMValues(llvmBuilder, scope, statement->range->lo);
    auto loValue = ValueOfExpression(statement->range->lo);
    InsertLLVMValues(llvmBuilder, scope, statement->range->hi);
    auto hiValue = ValueOfExpression(statement->range->hi);

    llvmBuilder->CreateBr(bodyBlock);

    auto bodyScope = Semantics::Scope::ofBody(statement->body);
    llvmBuilder->SetInsertPoint(bodyBlock);
    llvmBuilder->SetInsertPoint(endBlock);
    auto counterScopeName = bodyScope->names()->lookupName(statement->identifier);
    counterScopeName->setLLVMValue(bodyScope, llvmBuilder->getInt32(1));
    GenerateScopeBody(statement->body, bodyBlockName);

    llvmBuilder->SetInsertPoint(bodyBlock);
    auto bodyCallbacksList = bodyScope->getAllFallthroughNames();
    std::map<Semantics::Name *, llvm::PHINode *> fallthroughValues;
    for (auto fallthroughName : bodyCallbacksList) {
        auto nameType = GetTypesMap()[fallthroughName->getType()->getType()];
        auto bodyValue = fallthroughName->getLLVMValue(bodyScope);
        auto parentValue = fallthroughName->getLLVMValue(scope);
        auto phi = llvmBuilder->CreatePHI(nameType, 2);
        phi->addIncoming(parentValue, beginningBlock);
        phi->addIncoming(bodyValue, bodyBlock);
        fallthroughValues[fallthroughName] = phi;

        bool fromThisScope = false;
        auto nameToFall
            = bodyScope->names()->lookupName(fallthroughName->getName(), &fromThisScope);
        nameToFall->setLLVMValue(bodyScope, bodyValue);
        if (!fromThisScope) {
            bodyScope->insertNameFallthrough(fallthroughName);
        }
    }
    auto counterPhi = llvmBuilder->CreatePHI(llvmTypesMap[Semantics::TYPE_ID_INT], 2);
    counterPhi->addIncoming(loValue, beginningBlock);
    counterScopeName->setLLVMValue(bodyScope, counterPhi);
    GenerateScopeBody(statement->body, bodyBlockName);
    llvm::Value *newCounterValue = nullptr;

    if (!statement->reverse) {
        newCounterValue = llvmBuilder->CreateAdd(counterPhi, llvmBuilder->getInt32(1));
    } else {
        newCounterValue = llvmBuilder->CreateSub(counterPhi, llvmBuilder->getInt32(1));
    }
    counterPhi->addIncoming(newCounterValue, bodyBlock);
    auto exitCond = llvmBuilder->CreateICmpEQ(newCounterValue, hiValue);
    llvmBuilder->CreateCondBr(exitCond, endBlock, bodyBlock);

    llvmBuilder->SetInsertPoint(endBlock);
    for (auto fallthroughName : bodyCallbacksList) {
        auto phiValue = fallthroughValues[fallthroughName];
        bool fromThisScope = false;
        auto nameToFall = scope->names()->lookupName(fallthroughName->getName(), &fromThisScope);
        nameToFall->setLLVMValue(scope, phiValue);
        if (!fromThisScope) {
            scope->insertNameFallthrough(fallthroughName);
        }
    }
    return 0;
}

/**
 * The function `GenerateScopeBody` iterates through the nodes in a body and
 * generates code based on the type of each node.
 *
 * @param body A pointer to a Body object, which represents a block of code or a
 * function body.
 * @param currentInsertPoint The parameter "currentInsertPoint" is a string that
 * represents the current insert point in the code. It is used to keep track of
 * the current position in the code where new code should be inserted.
 *
 * @return an integer value of 0.
 */
int GenerateScopeBody(Body *body, std::string currentInsertPoint) {
    auto bodyScope = Semantics::Scope::ofBody(body);
    for (auto node : body->nodeList) {
        if (!node->isSemanticsValid) {
            Semantics::ReportSemanticError("node in " + bodyScope->getScopeName()
                                           + " not be skipped due to error");
            continue;
        }
        if (node->type == AstNodeType::DECLARATION) {
            auto declaration = node->toDeclaration();
            switch (declaration->declarationType) {
                case DeclarationType::VARIABLE_DECLARATION: {
                    GenerateVariable(bodyScope, declaration->toVariableDeclaration());
                    break;
                }
            }
        }
        if (node->type == AstNodeType::STATEMENT) {
            auto statement = node->toStatement();
            switch (statement->statementType) {
                case StatementType::EXPRESSION_STATEMENT: {
                    GenerateExpressionStatement(bodyScope,
                                                static_cast<ExpressionStatement *>(statement));
                    break;
                }
                case StatementType::RETURN_STATEMENT: {
                    GenerateReturnStatement(bodyScope, static_cast<ReturnStatement *>(statement));
                    break;
                }
                case StatementType::IF_STATEMENT: {
                    GenerateIfStatement(bodyScope, static_cast<IfStatement *>(statement));
                    break;
                }
                case StatementType ::WHILE_STATEMENT: {
                    GenerateWhileLoop(bodyScope, static_cast<WhileLoopStatement *>(statement));
                    break;
                }
                case StatementType ::FOR_STATEMENT: {
                    GenerateForLoop(bodyScope, static_cast<ForLoopStatement *>(statement));
                    break;
                }
            }
        }
    }
    return 0;
}

/**
 * The function "GenerateFunctionDeclaration" generates the LLVM function
 * declaration for a given routine declaration.
 *
 * @param routineDeclaration A pointer to a RoutineDeclaration object, which
 * represents a routine (function or procedure) declaration in the code.
 *
 * @return the result of calling the GenerateScopeBody function.
 */
int GenerateFunctionDeclaration(RoutineDeclaration *routineDeclaration) {
    auto globalFunction = Semantics::Function::ofRoutineDeclaration(routineDeclaration);
    auto llvmFuncName = globalFunction->getName();

    auto llvmFuncParams = std::vector<llvm::Type *>();
    for (auto &param : globalFunction->getAllParameters()) {
        auto primitive = true;
        auto rootParamType = Semantics::FindNativeBaseType(param.type->getType());
        if (rootParamType == -1) {
            rootParamType = param.type->getType();
            primitive = false;
        }
        auto llvmParamType = llvmTypesMap[rootParamType];
        if (primitive) {
            llvmFuncParams.push_back(llvmParamType);
        } else {
            llvmFuncParams.push_back(llvmParamType->getPointerTo());
        }
    }
    auto llvmParamsRef = llvm::ArrayRef<llvm::Type *>(llvmFuncParams);

    auto returnPrimitive = true;
    auto returnTypeId = Semantics::FindNativeBaseType(globalFunction->getReturnTypeId());
    if (returnTypeId == -1) {
        returnTypeId = globalFunction->getReturnTypeId();
        returnPrimitive = false;
    }
    auto llvmRetType = llvmTypesMap[returnTypeId];
    if (!returnPrimitive) {
        llvmRetType = llvmRetType->getPointerTo();
    }
    auto llvmFuncType = llvm::FunctionType::get(llvmRetType, llvmParamsRef, false);
    auto llvmFuncLinkage = llvm::Function::ExternalLinkage;
    auto llvmFunc = llvm::Function::Create(llvmFuncType, llvmFuncLinkage, llvmFuncName, llvmModule);
    Semantics::Function::ofRoutineDeclaration(routineDeclaration)->setLLVMFunction(llvmFunc);
    auto funcEntryPoint = llvm::BasicBlock::Create(*llvmContext, "entrypoint", llvmFunc);
    llvmBuilder->SetInsertPoint(funcEntryPoint);
    currentLlvmFunction = llvmFunc;
    auto bodyGenRes = GenerateScopeBody(routineDeclaration->body, "entrypoint");
    if (globalFunction->getReturnTypeId() == Semantics::TYPE_ID_VOID) {
        llvmBuilder->CreateRetVoid();
    }
    currentLlvmFunction = nullptr;
    return bodyGenRes;
}

/**
 * The function `RunCodegenPass` generates LLVM code for a given program,
 * including system and user-defined types, variables, and function
 * declarations.
 *
 * @param program The "program" parameter is a pointer to an object of type
 * "Program". It represents the program that needs to be processed and code
 * generated for.
 */
void Codegen::RunCodegenPass(Program *program) {
    llvmTypesMap[Semantics::TYPE_ID_VOID] = static_cast<llvm::Type *>(llvmBuilder->getVoidTy());
    llvmTypesMap[Semantics::TYPE_ID_INT] = static_cast<llvm::Type *>(llvmBuilder->getInt32Ty());
    llvmTypesMap[Semantics::TYPE_ID_FLOAT] = static_cast<llvm::Type *>(llvmBuilder->getFloatTy());
    llvmTypesMap[Semantics::TYPE_ID_BOOL] = static_cast<llvm::Type *>(llvmBuilder->getInt1Ty());

    llvmTypeSizeMap[Semantics::TYPE_ID_VOID] = 0;
    llvmTypeSizeMap[Semantics::TYPE_ID_INT] = 4;
    llvmTypeSizeMap[Semantics::TYPE_ID_FLOAT] = 4;
    llvmTypeSizeMap[Semantics::TYPE_ID_BOOL] = 1;

    // Print defenition which used as builtin function
    auto printfParams = std::vector<llvm::Type *>();
    printfParams.push_back(llvmBuilder->getInt8Ty()->getPointerTo());
    auto printfParamsRef = llvm::ArrayRef<llvm::Type *>(printfParams);
    auto printfType = llvm::FunctionType::get(llvmBuilder->getInt32Ty(), printfParamsRef, true);
    llvmSystemFunctions["printf"] = llvmModule->getOrInsertFunction("printf", printfType);

    AddBuiltinFunctionDefinitions();
    auto areSystemFunctionsDefined = false;
    auto userTypes = Semantics::GetUserTypesList();
    for (auto userType : userTypes) {
        GenerateType(Semantics::GetGlobalScope(), userType);
    }
    for (auto node : program->declarations) {
        if (node->declarationType == DeclarationType::VARIABLE_DECLARATION) {
            GenerateVariable(Semantics::GetGlobalScope(), node->toVariableDeclaration());
        }
        if (node->declarationType == DeclarationType::ROUTINE_DECLARATION) {
            GenerateFunctionDeclaration(node->toRoutineDeclaration());
        }
    }

    llvmModule->print(llvm::outs(), nullptr);

    int fd = fileno(moduleOutputFile);
    auto moduleOStream = llvm::raw_fd_ostream(fd, true, false);
    const auto &moduleRef = *llvmModule;
    llvm::WriteBitcodeToFile(moduleRef, moduleOStream);
    llvmSystemFunctions.clear();
    Codegen::DestroyCodegenContext();
    Semantics::DestroySemanticContext();
}
