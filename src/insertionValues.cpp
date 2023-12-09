#include "AST.hpp"
#include "Parser.tab.hpp"
#include "codegen.hpp"
#include "semanticAnalyser.hpp"

using namespace Codegen;

// ValueOfExpression takes an expression and returns its LLVM Value pointer,
// which is used for further manipulation in LLVM.
llvm::Value *Codegen::ValueOfExpression(Expression *expression) {
    return static_cast<llvm::Value *>(expression->llvmValuePtr);
}

// GenerateAccessVector computes the address (pointer) for a member or an array
// element within a structure or a top-level variable, using LLVM IRBuilder to
// create GEP (GetElementPointer) instructions to calculate the addresses.
llvm::Value *GenerateAccessVector(llvm::IRBuilder<> *builder, Semantics::Scope *scope,
                                  ModifiablePrimary *primary) {
    auto topLevelName = scope->names()->lookupName(primary->identifier);
    auto topLevelType = topLevelName->getType();
    auto topNameScopeOfVar = scope;
    if (topLevelName->isParameter()) {
        auto funcOwner = topLevelName->getParameterOwner();
        auto llvmFuncOwner = funcOwner->getLLVMFunction();
        auto argIterator = llvmFuncOwner->arg_begin();
        for (int i = 0; i < funcOwner->getAllParameters().size(); i++) {
            auto llvmArg = static_cast<llvm::Value *>(argIterator++);
            auto funcArg = funcOwner->getAllParameters()[i];
            if (funcArg.name->getName() == topLevelName->getName()) {
                primary->llvmValuePtr = llvmArg;
                break;
            }
        }
    } else {
        do {
            primary->llvmValuePtr = topLevelName->getLLVMValue(topNameScopeOfVar);
            topNameScopeOfVar = topNameScopeOfVar->parent();
        } while (primary->llvmValuePtr == nullptr && topNameScopeOfVar != nullptr);
    }
    auto topLevelNamePtr = ValueOfExpression(primary);
    auto currentLlvmValPtr = topLevelNamePtr;
    auto currentType = topLevelType;
    auto currentPrimary = primary;
    for (;;) {
        if (currentPrimary->index) {
            InsertLLVMValues(builder, scope, currentPrimary->index);
            std::vector<llvm::Value *> gepIndices
                = {builder->getInt32(0), ValueOfExpression(currentPrimary->index)};

            auto *allocaInst = static_cast<llvm::AllocaInst *>(
                static_cast<llvm::Value *>(primary->llvmValuePtr));
            currentLlvmValPtr = builder->CreateInBoundsGEP(allocaInst->getAllocatedType(),
                                                           allocaInst, gepIndices);
        }

        auto nextPrimary = currentPrimary->member;
        if (nextPrimary == nullptr) {
            return currentLlvmValPtr;
        }
        if ((nextPrimary->index == nullptr) && (nextPrimary->member == nullptr)) {
            return currentLlvmValPtr;
        } else {
            currentLlvmValPtr
                = builder->CreateLoad(currentLlvmValPtr->getType(), currentLlvmValPtr);
            currentPrimary = nextPrimary;
            continue;
        }
    }
}

// Generate an access vector using the given modifiable primary
// This function returns the pointer to the target element in the modifiable
// primary
int GenerateMemberWrite(llvm::IRBuilder<> *builder, Semantics::Scope *scope,
                        ModifiablePrimary *primary, llvm::Value *value) {
    auto targetPtr = GenerateAccessVector(builder, scope, primary);
    if (targetPtr == nullptr) return -1;
    builder->CreateStore(value, targetPtr, true);
    return 0;
}

// Generate the loading of an LLVM value using the given modifiable primary
// This function returns the value at the target element of the modifiable
// primary
llvm::Value *GenerateMemberRead(llvm::IRBuilder<> *builder, Semantics::Scope *scope,
                                ModifiablePrimary *primary) {
    auto targetPtr = GenerateAccessVector(builder, scope, primary);
    auto topLevelName = scope->names()->lookupName(primary->identifier);
    auto topLevelType = topLevelName->getType()->getType();
    auto topLllvmTypeValues = GetTypesMap();
    return builder->CreateLoad(topLllvmTypeValues[topLevelType], targetPtr);
}

// Function to perform insertion of assigment into llvm builder
int InsertLLVMAssignment(llvm::IRBuilder<> *builder, Semantics::Scope *scope,
                         BinaryExpression *expression) {
    InsertLLVMValues(builder, scope, expression->operand2);
    auto rValue = ValueOfExpression(expression->operand2);
    auto modifiableLValue = static_cast<ModifiablePrimary *>(expression->operand1);
    if (!modifiableLValue->isAccessMember && modifiableLValue->index == nullptr) {
        // Storing of variable
        auto isFromThis = true;
        auto name = scope->names()->lookupName(modifiableLValue->identifier, &isFromThis);
        name->setLLVMValue(scope, rValue);
        if (!isFromThis) {
            scope->insertNameFallthrough(name);
        }
        return 0;
    }
    return GenerateMemberWrite(builder, scope, modifiableLValue, rValue);
}

// Function to perform assignment of the LLVM r-value in a binary expression
int InsertLLVMValues(llvm::IRBuilder<> *builder, Semantics::Scope *scope,
                     BinaryExpression *expression) {
    if (expression->oper == OP_ASSIGNMENT) {
        return InsertLLVMAssignment(builder, scope, expression);
    }
    auto nativeTypesMap = GetTypesMap();
    InsertLLVMValues(builder, scope, expression->operand1);
    InsertLLVMValues(builder, scope, expression->operand2);
    auto lValue = ValueOfExpression(expression->operand1);
    auto rValue = ValueOfExpression(expression->operand2);
    llvm::Value *resultValue = nullptr;
    if (expression->expressionType == Semantics::TYPE_ID_INT) {
        switch (expression->oper) {
            case OP_PLUS: {
                resultValue = builder->CreateAdd(lValue, rValue);
                break;
            }
            case OP_MINUS: {
                resultValue = builder->CreateSub(lValue, rValue);
                break;
            }
            case OP_MULTIPLY: {
                resultValue = builder->CreateMul(lValue, rValue);
                break;
            }
            case OP_DIVIDE: {
                resultValue = builder->CreateSDiv(lValue, rValue);
                break;
            }
            case OP_MODULE: {
                resultValue = builder->CreateSRem(lValue, rValue);
                break;
            }
        }
    } else if (expression->expressionType == Semantics::TYPE_ID_FLOAT) {
        switch (expression->oper) {
            case OP_PLUS: {
                resultValue = builder->CreateFAdd(lValue, rValue);
                break;
            }
            case OP_MINUS: {
                resultValue = builder->CreateFSub(lValue, rValue);
                break;
            }
            case OP_MULTIPLY: {
                resultValue = builder->CreateFMul(lValue, rValue);
                break;
            }
            case OP_DIVIDE: {
                resultValue = builder->CreateFDiv(lValue, rValue);
                break;
            }
            case OP_MODULE: {
                resultValue = builder->CreateFRem(lValue, rValue);
                break;
            }
        }
    } else if (expression->expressionType == Semantics::TYPE_ID_BOOL) {
        switch (expression->oper) {
            case OP_OR:
            case OP_PLUS: {
                resultValue = builder->CreateOr(lValue, rValue);
                break;
            }
            case OP_AND:
            case OP_MULTIPLY: {
                resultValue = builder->CreateAnd(lValue, rValue);
                break;
            }
            case OP_XOR: {
                resultValue = builder->CreateXor(lValue, rValue);
                break;
            }
            case OP_GREATER: {
                switch (expression->operand1->expressionType) {
                    case Semantics::TYPE_ID_INT: {
                        resultValue = builder->CreateICmpSGT(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_FLOAT: {
                        resultValue = builder->CreateFCmpOGT(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_BOOL: {
                        resultValue = builder->CreateICmpSGT(lValue, rValue);
                        break;
                    }
                }
                break;
            }
            case OP_GREATER_OR_EQUAL: {
                switch (expression->operand1->expressionType) {
                    case Semantics::TYPE_ID_INT: {
                        resultValue = builder->CreateICmpSGE(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_FLOAT: {
                        resultValue = builder->CreateFCmpOGE(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_BOOL: {
                        resultValue = builder->CreateICmpSGE(lValue, rValue);
                        break;
                    }
                }
                break;
            }
            case OP_LESS: {
                switch (expression->operand1->expressionType) {
                    case Semantics::TYPE_ID_INT: {
                        resultValue = builder->CreateICmpSLT(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_FLOAT: {
                        resultValue = builder->CreateFCmpOLT(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_BOOL: {
                        resultValue = builder->CreateICmpSLT(lValue, rValue);
                        break;
                    }
                }
                break;
            };
            case OP_LESS_OR_EQUAL: {
                switch (expression->operand1->expressionType) {
                    case Semantics::TYPE_ID_INT: {
                        resultValue = builder->CreateICmpSLE(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_FLOAT: {
                        resultValue = builder->CreateFCmpOLE(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_BOOL: {
                        resultValue = builder->CreateICmpSLE(lValue, rValue);
                        break;
                    }
                }
                break;
            }
            case OP_EQUAL: {
                switch (expression->operand1->expressionType) {
                    case Semantics::TYPE_ID_INT: {
                        resultValue = builder->CreateICmpEQ(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_FLOAT: {
                        resultValue = builder->CreateFCmpOEQ(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_BOOL: {
                        resultValue = builder->CreateICmpEQ(lValue, rValue);
                        break;
                    }
                }
                break;
            }
            case OP_NOT_EQUAL: {
                switch (expression->operand1->expressionType) {
                    case Semantics::TYPE_ID_INT: {
                        resultValue = builder->CreateICmpNE(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_FLOAT: {
                        resultValue = builder->CreateFCmpONE(lValue, rValue);
                        break;
                    }
                    case Semantics::TYPE_ID_BOOL: {
                        resultValue = builder->CreateICmpNE(lValue, rValue);
                        break;
                    }
                }
                break;
            }
        }
    }
    expression->llvmValuePtr = resultValue;
    return 0;
}

// Function to insert and generate the LLVM values for given binary expressions
int InsertLLVMValues(llvm::IRBuilder<> *builder, Semantics::Scope *scope,
                     UnaryExpression *expression) {
    InsertLLVMValues(builder, scope, expression->operand);
    auto operandValue = ValueOfExpression(expression->operand);
    auto nativeTypesMap = GetTypesMap();
    switch (expression->oper) {
        case Semantics::OP_CAST_INT_TO_FLOAT: {
            expression->llvmValuePtr
                = builder->CreateCast(llvm::Instruction::CastOps::SIToFP, operandValue,
                                      nativeTypesMap[Semantics::TYPE_ID_FLOAT]);
            break;
        }
        case Semantics::OP_CAST_FLOAT_TO_INT: {
            expression->llvmValuePtr
                = builder->CreateCast(llvm::Instruction::CastOps::FPToSI, operandValue,
                                      nativeTypesMap[Semantics::TYPE_ID_INT]);
            break;
        }
        case Semantics::OP_CAST_INT_TO_BOOL: {
            expression->llvmValuePtr
                = builder->CreateCast(llvm::Instruction::CastOps::Trunc, operandValue,
                                      nativeTypesMap[Semantics::TYPE_ID_BOOL]);
            break;
        }
        case Semantics::OP_CAST_FLOAT_TO_BOOL: {
            auto intermediateIntValue
                = builder->CreateCast(llvm::Instruction::CastOps::FPToSI, operandValue,
                                      nativeTypesMap[Semantics::TYPE_ID_BOOL]);
            expression->llvmValuePtr
                = builder->CreateCast(llvm::Instruction::CastOps::Trunc, intermediateIntValue,
                                      nativeTypesMap[Semantics::TYPE_ID_BOOL]);
            break;
        }
    }
    return 0;
}
// Function to insert and generate the LLVM values for given ConstPrimary
int InsertLLVMValues(llvm::IRBuilder<> *builder, Semantics::Scope *scope,
                     ConstPrimary *constPrimary) {
    auto constType = GetTypesMap()[constPrimary->expressionType];
    switch (constPrimary->expressionType) {
        case Semantics::TYPE_ID_INT: {
            constPrimary->llvmValuePtr = llvm::ConstantInt::get(constType, constPrimary->intValue);
            break;
        }
        case Semantics::TYPE_ID_FLOAT: {
            constPrimary->llvmValuePtr = llvm::ConstantFP::get(constType, constPrimary->realValue);
            break;
        }
        case Semantics::TYPE_ID_BOOL: {
            constPrimary->llvmValuePtr = llvm::ConstantInt::get(constType, constPrimary->boolValue);
            break;
        }
    }
    return 0;
}
// Function to insert and generate the LLVM values for given Modifiable Primary
int InsertLLVMValues(llvm::IRBuilder<> *builder, Semantics::Scope *scope,
                     ModifiablePrimary *primary) {
    auto name = scope->names()->lookupName(primary->identifier);

    // If name is not a param
    if (primary->member == nullptr && primary->index == nullptr) {
        // Simple variable
        if (name->isParameter()) {
            auto funcOwner = name->getParameterOwner();
            auto llvmFuncOwner = funcOwner->getLLVMFunction();
            auto argIterator = llvmFuncOwner->arg_begin();
            for (int i = 0; i < funcOwner->getAllParameters().size(); i++) {
                auto llvmArg = static_cast<llvm::Value *>(argIterator++);
                auto funcArg = funcOwner->getAllParameters()[i];
                if (funcArg.name->getName() == name->getName()) {
                    primary->llvmValuePtr = llvmArg;
                    break;
                }
            }
            if (primary->llvmValuePtr != nullptr) {
                return 0;
            }
        } else {
            auto scopeOfVar = scope;
            do {
                primary->llvmValuePtr = name->getLLVMValue(scopeOfVar);
                scopeOfVar = scopeOfVar->parent();
            } while (primary->llvmValuePtr == nullptr && scopeOfVar != nullptr);
            return 0;
        }
    } else {
        // For Non-Primitive type
        primary->llvmValuePtr = GenerateMemberRead(builder, scope, primary);
        return 0;
    }
    return -1;
}

// Function to insert and generate the LLVM values for given Callable Primary
int InsertLLVMValues(llvm::IRBuilder<> *builder, Semantics::Scope *scope,
                     CallablePrimary *primary) {
    auto func = scope->funcs()->lookupFunc(primary->identifier);
    auto llvmFuncToCall = func->getLLVMFunction();
    // Parameter values fetching
    auto llvmFuncArgs = std::vector<llvm::Value *>();
    for (auto expression : primary->argumentList->expressionList) {
        Codegen::InsertLLVMValues(builder, scope, expression);
        llvmFuncArgs.push_back(ValueOfExpression(expression));
    }
    auto callInstr = builder->CreateCall(llvmFuncToCall, llvmFuncArgs);
    primary->llvmValuePtr = static_cast<llvm::Value *>(callInstr);
    return 0;
}

// Function to insert and generate the LLVM values for given expression
int Codegen::InsertLLVMValues(llvm::IRBuilder<> *builder, Semantics::Scope *scope,
                              Expression *expression) {
    if (!expression->isPrimary) {
        if (expression->isBinary) {
            return InsertLLVMValues(builder, scope, static_cast<BinaryExpression *>(expression));
        } else {
            return InsertLLVMValues(builder, scope, static_cast<UnaryExpression *>(expression));
        }
    } else {
        auto primaryExpression = static_cast<Primary *>(expression);
        if (primaryExpression->isConst) {
            return InsertLLVMValues(builder, scope, static_cast<ConstPrimary *>(expression));
        } else if (primaryExpression->isCall) {
            return InsertLLVMValues(builder, scope, static_cast<CallablePrimary *>(expression));
        } else {
            return InsertLLVMValues(builder, scope, static_cast<ModifiablePrimary *>(expression));
        }
    }
}
