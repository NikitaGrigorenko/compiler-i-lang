#include "AST.hpp"
#include "Parser.tab.hpp"
#include "semanticAnalyser.hpp"

using namespace Semantics;

bool isSingleCallStatement = false;

/**
 * The function SetCallStatementStatus sets the status of a call statement to indicate whether it is
 * a single call or not.
 *
 * @param isSingleCall A boolean value indicating whether the call statement is a single call or
 * not.
 */
void Semantics::SetCallStatementStatus(bool isSingleCall) {
    isSingleCallStatement = isSingleCall;
}

/**
 * The function increments a static counter and returns the updated value.
 *
 * @return the incremented value of the static variable counter.
 */
TypeID Semantics::IncrementAndGetTypeID() {
    static TypeID counter = 10;
    return ++counter;
}

/**
 * The function `FindNativeBaseType` searches for the native base type of a given type.
 *
 * @param typeId The `typeId` parameter is of type `TypeID` and represents the identifier of a type.
 *
 * @return the native base type of the given TypeID. If the native base type is found, it is
 * returned. If the native base type is not found, -1 is returned. If there is an error in the logic
 * of the function, -2 is returned.
 */
TypeID Semantics::FindNativeBaseType(TypeID typeId) {
    auto typeTable = GetCurrentScope()->types();
    auto type = typeTable->lookupType(typeId);
    for (;;) {
        if (type->isNative()) {
            return type->getType();
        }
        type = typeTable->lookupType(type->getBaseType());
    }
}

/**
 * The function `RunPrimaryTypeTrace` is used to determine the type of a primary expression
 *
 * @param primary A pointer to an object of type Primary.
 *
 * @return a value of type "TypeID".
 */
TypeID RunPrimaryTypeTrace(Primary *primary) {
    if (primary->isConst) {
        auto constPrimary = static_cast<ConstPrimary *>(primary);
        switch (constPrimary->type) {
            case INTEGER_PRIMARY_TYPE: return TYPE_ID_INT;
            case REAL_PRIMARY_TYPE: return TYPE_ID_FLOAT;
            case BOOLEAN_PRIMARY_TYPE: return TYPE_ID_BOOL;
        }
        Semantics::ReportSemanticError(std::string("type tracing unabled for primary type ")
                                       + std::to_string(constPrimary->type));
        return -1;
    }
    if (primary->isCall) {
        auto callablePrimary = static_cast<CallablePrimary *>(primary);
        auto func = GetCurrentScope()->funcs()->lookupFunc(callablePrimary->identifier);
        if (func == nullptr) {
            Semantics::ReportSemanticError(std::string("function ") + callablePrimary->identifier
                                           + " is not defined");
        }
        auto paramsList = func->getAllParameters();
        auto argList = callablePrimary->argumentList->expressionList;
        auto argIterator = callablePrimary->argumentList->expressionList.begin();
        auto argIteratorEnd = callablePrimary->argumentList->expressionList.end();
        int counter = 1;
        while (argIterator != argIteratorEnd) {
            auto argExpressionRef = &(*argIterator);
            auto argExpression = *argIterator;
            auto argType = RunTypeTrace(argExpression);
            if (argType == -1) {
                Semantics::ReportSemanticError(
                    std::string("function call ") + callablePrimary->identifier
                    + " can not be performed with invalid type " + std::to_string(counter));
            }
            auto paramType = paramsList[counter - 1].type->getType();
            if (paramType != argType) {
                auto res
                    = InsertTypeCastExpression(argExpressionRef, argExpression, paramType, false);
                if (res == -1) {
                    Semantics::ReportSemanticError(
                        std::string("function call ") + callablePrimary->identifier
                        + " can not be performed with " + std::to_string(counter)
                        + " because of non-conforming type");
                } else {
                    Semantics::ReportSemanticWarning(
                        std::string("applying implicit casting when call function: ")
                        + func->getName());
                }
            }
            ++argIterator;
            counter++;
        }
        if (func->getReturnTypeId() == TYPE_ID_VOID) {
            if (isSingleCallStatement) {
                return TYPE_ID_VOID;
            }
            Semantics::ReportSemanticError("function " + func->getName()
                                           + " cannot be used as operand because of void type");
        }
        return func->getReturnTypeId();
    }
    auto nameTable = GetCurrentScope()->names();
    auto modifiablePrimary = static_cast<ModifiablePrimary *>(primary);
    if (modifiablePrimary->index || modifiablePrimary->member) {
        auto name = nameTable->lookupName(modifiablePrimary->identifier);
        auto currentPrimary = modifiablePrimary;
        auto currentType = name->getType();
        while (currentPrimary && (currentPrimary->member || currentPrimary->index)) {
            if (currentPrimary->index) {
                auto indexType = RunTypeTrace(currentPrimary->index);
                if (indexType == -1) {
                    Semantics::ReportSemanticError("unable to trace type for array size");
                    return -1;
                }
                if (indexType != TYPE_ID_INT) {
                    Semantics::ReportSemanticWarning("array index should be of type integer");
                    InsertTypeCastExpression(&modifiablePrimary->index, modifiablePrimary->index,
                                             TYPE_ID_INT, false);
                }
                currentType = GetCurrentScope()->types()->lookupType(currentType->getType());
            }
            currentPrimary = currentPrimary->member;
        }
        return currentType->getType();
    } else {
        auto name = nameTable->lookupName(modifiablePrimary->identifier);
        if (name == nullptr) {
            Semantics::ReportSemanticError(std::string("no name \"") + modifiablePrimary->identifier
                                           + "\" in the scope "
                                           + GetCurrentScope()->getScopeName());
        }
        return name->getType()->getType();
    }
    return -1;
}

/**
 * The function RunUnaryTypeTrace assigns the expression type of the operand in a unary expression
 * by calling the RunTypeTrace function.
 *
 * @param unaryExpression A pointer to a UnaryExpression object.
 *
 * @return an integer value of 0.
 */
TypeID RunUnaryTypeTrace(UnaryExpression *unaryExpression) {
    unaryExpression->operand->expressionType = RunTypeTrace(unaryExpression->operand);
    return 0;
}

/**
 * The function `RunBinaryTypeTrace` determines the type of a binary expression and performs type
 * casting if necessary.
 *
 * @param binaryExpression A pointer to a BinaryExpression struct, which represents a binary
 * expression in a programming language. The struct contains the following members:
 *
 * @return a TypeID.
 */
TypeID RunBinaryTypeTrace(BinaryExpression *binaryExpression) {
    RunTypeTrace(binaryExpression->operand1);
    RunTypeTrace(binaryExpression->operand2);
    if (binaryExpression->oper == OP_ASSIGNMENT) {
        if (binaryExpression->operand1->expressionType
            != binaryExpression->operand2->expressionType) {
            return InsertTypeCastExpression(&binaryExpression->operand2, binaryExpression->operand2,
                                            binaryExpression->operand2->expressionType,
                                            binaryExpression->operand1->expressionType, false);
        } else {
            return binaryExpression->operand1->expressionType;
        }
    }
    TypeID commonTypeId = -1;
    if (binaryExpression->operand1->expressionType == binaryExpression->operand2->expressionType) {
        commonTypeId = binaryExpression->operand1->expressionType;
    } else {
        commonTypeId = ApplyTypeWidening(binaryExpression);
    }
    switch (binaryExpression->oper) {
        case OP_AND:
        case OP_OR:
        case OP_XOR:
        case OP_EQUAL:
        case OP_NOT_EQUAL:
        case OP_GREATER:
        case OP_GREATER_OR_EQUAL:
        case OP_LESS:
        case OP_LESS_OR_EQUAL: return TYPE_ID_BOOL;
        default: return commonTypeId;
    }
}

/**
 * The function `RunTypeTrace` determines the type of an expression node by recursively tracing its
 * type through binary, primary, and unary expressions.
 *
 * @param expressionNode A pointer to an Expression object.
 *
 * @return the expression type of the given expression node.
 */
TypeID Semantics::RunTypeTrace(Expression *expressionNode) {
    if (expressionNode->expressionType != -1) {
        return expressionNode->expressionType;
    }
    if (expressionNode->isBinary) {
        auto binaryExpression = static_cast<BinaryExpression *>(expressionNode);
        binaryExpression->expressionType = RunBinaryTypeTrace(binaryExpression);
        return binaryExpression->expressionType;
    }
    if (expressionNode->isPrimary) {
        auto primaryExpression = static_cast<Primary *>(expressionNode);
        primaryExpression->expressionType = RunPrimaryTypeTrace(primaryExpression);
        return primaryExpression->expressionType;
    } else {
        auto unaryExpression = static_cast<UnaryExpression *>(expressionNode);
        unaryExpression->expressionType = RunUnaryTypeTrace(unaryExpression);
        return unaryExpression->expressionType;
    }
}

/**
 * The function inserts a type cast expression into the parent expression, converting the child
 * expression to the target type.
 *
 * @param refFromParent A pointer to a pointer to an Expression object. This is used to update the
 * parent's reference to the child expression if a type cast is inserted.
 * @param child The "child" parameter is a pointer to an Expression object that represents the
 * expression to be type casted.
 * @param targetType The `targetType` parameter represents the type to which the expression `child`
 * is being casted.
 * @param isExplicit The "isExplicit" parameter is a boolean flag that indicates whether the type
 * cast expression is explicit or implicit. If "isExplicit" is true, it means that the type cast is
 * explicitly specified in the code. If it is false, it means that the type cast is implicit and the
 * compiler will
 *
 * @return the result of the call to `Semantics::InsertTypeCastExpression`.
 */
TypeID Semantics::InsertTypeCastExpression(Expression **refFromParent, Expression *child,
                                           TypeID targetType, bool isExplicit) {
    auto childSourceType = RunTypeTrace(child);
    if (childSourceType == -1) {
        return -1;
    }
    auto nativeChildType = FindNativeBaseType(childSourceType);
    if (nativeChildType == -1) {
        Semantics::ReportSemanticError("non-primitive types can not used in type casting");
    }
    return Semantics::InsertTypeCastExpression(refFromParent, child, nativeChildType, targetType,
                                               isExplicit);
}

/**
 * The function `InsertTypeCastExpression` inserts a type cast expression into the AST (Abstract
 * Syntax Tree) based on the source and target types, and whether the cast is explicit or not.
 *
 * @param refFromParent A pointer to a pointer to an Expression object. This is used to update the
 * parent expression with the newly created cast expression.
 * @param child The "child" parameter is a pointer to an Expression object that represents the
 * expression to be type casted.
 * @param sourceType The sourceType parameter represents the type of the expression that needs to be
 * casted.
 * @param targetType The targetType parameter in the code represents the type to which the
 * expression is being cast. It is of type TypeID, which is likely an enumeration or a numeric
 * identifier representing different types in the code.
 * @param isExplicit The "isExplicit" parameter is a boolean value that indicates whether the type
 * cast is explicit or implicit. An explicit type cast is when the programmer explicitly specifies
 * the type conversion using a cast operator, while an implicit type cast is when the type
 * conversion is done automatically by the compiler.
 *
 * @return the TypeID of the targetType.
 */
TypeID Semantics::InsertTypeCastExpression(Expression **refFromParent, Expression *child,
                                           TypeID sourceType, TypeID targetType, bool isExplicit) {
    int castOperator = 0;
    if (sourceType == TYPE_ID_BOOL && targetType == TYPE_ID_INT) {
        castOperator = OP_CAST_BOOL_TO_INT;
    } else if (sourceType == TYPE_ID_BOOL && targetType == TYPE_ID_FLOAT) {
        castOperator = OP_CAST_BOOL_TO_FLOAT;
    } else if (sourceType == TYPE_ID_INT && targetType == TYPE_ID_FLOAT) {
        castOperator = OP_CAST_INT_TO_FLOAT;
    }
    if (castOperator == 0) {
        if (!isExplicit) {
            Semantics::ReportSemanticWarning("beware of bugs, type narrowing was in action");
        }
        if (sourceType == TYPE_ID_FLOAT && targetType == TYPE_ID_INT) {
            castOperator = OP_CAST_FLOAT_TO_INT;
        } else if (sourceType == TYPE_ID_FLOAT && targetType == TYPE_ID_BOOL) {
            castOperator = OP_CAST_FLOAT_TO_BOOL;
        } else if (sourceType == TYPE_ID_INT && targetType == TYPE_ID_BOOL) {
            castOperator = OP_CAST_INT_TO_BOOL;
        }
    }
    auto castExpression = new UnaryExpression(castOperator, child);
    castExpression->expressionType = targetType;
    *refFromParent = castExpression;
    return targetType;
}

/**
 * The function `ApplyTypeWidening` in the `Semantics` class performs type widening for binary
 * expressions in C++.
 *
 * @param binaryExpression A pointer to a BinaryExpression object, which represents a binary
 * operation (e.g., addition, subtraction) in the code.
 *
 * @return the type ID of the result of the type widening operation.
 */
TypeID Semantics::ApplyTypeWidening(BinaryExpression *binaryExpression) {
    auto left = binaryExpression->operand1;
    auto right = binaryExpression->operand2;
    auto leftType = RunTypeTrace(left);
    auto rightType = RunTypeTrace(right);
    leftType = FindNativeBaseType(leftType);
    rightType = FindNativeBaseType(rightType);
    if (leftType == -1 || rightType == -1) {
        auto type1 = GetCurrentScope()->types()->lookupType(left->expressionType);
        auto type2 = GetCurrentScope()->types()->lookupType(right->expressionType);
        Semantics::ReportSemanticError(
            std::string("implicit type casting is not allowed for non-native derived types ")
            + type1->getName() + " and " + type2->getName());
    }
    if (leftType == rightType) {
        return leftType;
    }
    auto lRef = &binaryExpression->operand1;
    auto rRef = &binaryExpression->operand2;
    if (leftType == TYPE_ID_INT) {
        if (rightType == TYPE_ID_FLOAT) {
            return InsertTypeCastExpression(lRef, left, TYPE_ID_INT, TYPE_ID_FLOAT, false);
        }
        if (rightType == TYPE_ID_BOOL) {
            return InsertTypeCastExpression(rRef, right, TYPE_ID_BOOL, TYPE_ID_INT, false);
        }
    }
    if (leftType == TYPE_ID_FLOAT) {
        if (rightType == TYPE_ID_INT) {
            return InsertTypeCastExpression(rRef, right, TYPE_ID_INT, TYPE_ID_FLOAT, false);
        }
        if (rightType == TYPE_ID_BOOL) {
            return InsertTypeCastExpression(rRef, right, TYPE_ID_BOOL, TYPE_ID_FLOAT, false);
        }
    }
    if (leftType == TYPE_ID_BOOL) {
        if (rightType == TYPE_ID_INT) {
            return InsertTypeCastExpression(lRef, left, TYPE_ID_BOOL, TYPE_ID_INT, false);
        }
        if (rightType == TYPE_ID_FLOAT) {
            return InsertTypeCastExpression(lRef, left, TYPE_ID_BOOL, TYPE_ID_FLOAT, false);
        }
    }
}

/**
 * The function ApplyTypeWidening applies type widening to a unary expression.
 *
 * @param unaryExpression A pointer to a UnaryExpression object.
 */
TypeID Semantics::ApplyTypeWidening(UnaryExpression *unaryExpression) {}