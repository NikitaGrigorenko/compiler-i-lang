#include "semanticAnalyser.hpp"
#include "AST.hpp"
#include "Parser.tab.hpp"

using namespace Semantics;

// Function to process body elements of an AST.
int ProcessBodyElements(Body *body);

// Current function being processed.
Function *currentFunction;

// Function to process simple declarations (type/variable declarations) in semantic analysis.
int ProcessSimpleDeclaration(Declaration *declaration) {
    if (declaration->declarationType == DeclarationType::TYPE_DECLARATION) {
        auto type
            = Semantics::GetCurrentScope()->types()->defineType(declaration->toTypeDeclaration());
        declaration->isSemanticsValid = type != nullptr;
        return declaration->isSemanticsValid ? 0 : -1;
    } else {
        VariableDeclaration *varDeclaration = declaration->toVariableDeclaration();
        auto name = Semantics::GetCurrentScope()->names()->defineName(varDeclaration);
        if (varDeclaration->initialValue != nullptr) {
            RunTypeTrace(varDeclaration->initialValue);
        }
        declaration->isSemanticsValid = name != nullptr;
        return declaration->isSemanticsValid ? 0 : -1;
    }
}

// Function to process expression statements in semantic analysis.
int ProcessExpressionStatement(ExpressionStatement *statement) {
    auto expression = statement->expression;
    if (expression->isBinary) {
        auto binaryExpression = static_cast<BinaryExpression *>(expression);
        return RunTypeTrace(binaryExpression->operand2) != -1 ? 0 : -1;
    } else if (expression->isPrimary) {
        auto primaryExpression = static_cast<Primary *>(expression);
        if (primaryExpression->isCall) {
            SetCallStatementStatus(true);
            auto typeTraceResult = RunTypeTrace(primaryExpression) != -1 ? 0 : -1;
            SetCallStatementStatus(false);
            return typeTraceResult;
        }
    } else {
        Semantics::ReportSemanticWarning("ignoring of unused expression result");
        return -1;
    }
}

// Function to process return statements in semantic analysis.
int ProcessReturnStatement(ReturnStatement *statement) {
    int typeTraceResult = RunTypeTrace(statement->expressionToReturn);
    if (currentFunction->getReturnTypeId() == TYPE_ID_VOID) {
        Semantics::ReportSemanticError("function with void return type should not return values");
        return -1;
    }
    auto source = FindNativeBaseType(typeTraceResult);
    auto target = FindNativeBaseType(currentFunction->getReturnTypeId());
    if (source != target) {
        auto castType = InsertTypeCastExpression(
            &statement->expressionToReturn, statement->expressionToReturn, source, target, false);
        if (castType != currentFunction->getReturnTypeId()) {
            Semantics::ReportSemanticError("Casting of return statament was failed");
            return -1;
        }
    }
    return typeTraceResult != -1 ? 0 : -1;
}

// Function to process if statements in semantic analysis.
int ProcessIfStatement(IfStatement *statement) {
    int expressionTypeId = RunTypeTrace(statement->expression);
    if (expressionTypeId != TYPE_ID_BOOL) {
        Semantics::ReportSemanticWarning(
            "applying implicit casting to if expression non-boolean type");
        if (InsertTypeCastExpression(&statement->expression, statement->expression, TYPE_ID_BOOL)
            == -1) {
            Semantics::ReportSemanticError("Casting of expression of if was failed");
            return -1;
        }
        expressionTypeId = TYPE_ID_BOOL;
    }
    auto trueScope = Scope::createInnerScope(GetCurrentScope(), "true");
    statement->trueBody->scopePtr = trueScope;
    Semantics::EnterScope(trueScope);
    ProcessBodyElements(statement->trueBody);
    Semantics::ExitCurrentScope();
    if (statement->falseBody) {
        auto falseScope = Scope::createInnerScope(GetCurrentScope(), "false");
        statement->falseBody->scopePtr = falseScope;
        Semantics::EnterScope(falseScope);
        ProcessBodyElements(statement->falseBody);
        Semantics::ExitCurrentScope();
    }
    return 0;
}

// Process and validate a for loop statement.
int ProcessForStatement(ForLoopStatement *statement) {
    int rangeLoTypeId = RunTypeTrace(statement->range->lo);
    if (rangeLoTypeId != TYPE_ID_INT) {
        Semantics::ReportSemanticWarning("applying implicit casting to LO range of for loop");
        if (InsertTypeCastExpression(&statement->range->lo, statement->range->lo, TYPE_ID_INT)
            == -1) {
            Semantics::ReportSemanticError("Casting of LO in for loop was failed");
            return -1;
        }
        rangeLoTypeId = TYPE_ID_INT;
    }
    int rangeHiTypeId = RunTypeTrace(statement->range->hi);
    if (rangeHiTypeId != TYPE_ID_INT) {
        Semantics::ReportSemanticWarning("applying implicit casting to HI range of for loop");
        if (InsertTypeCastExpression(&statement->range->hi, statement->range->hi, TYPE_ID_INT)
            == -1) {
            Semantics::ReportSemanticError("Casting of HI in for loop was failed");
            return -1;
        }
        rangeHiTypeId = TYPE_ID_INT;
    }
    auto bodyScope = Scope::createInnerScope(GetCurrentScope(), "body");
    auto counterType = bodyScope->types()->lookupType(TYPE_ID_INT);
    auto counterName = new Name(statement->identifier, counterType);
    bodyScope->names()->defineName(counterName);
    statement->body->scopePtr = bodyScope;
    Semantics::EnterScope(bodyScope);
    ProcessBodyElements(statement->body);
    Semantics::ExitCurrentScope();
    return 0;
}

// Process and validate a while loop statement.
int ProcessWhileStatement(WhileLoopStatement *statement) {
    int expressionTypeId = RunTypeTrace(statement->expression);
    if (expressionTypeId != TYPE_ID_BOOL) {
        Semantics::ReportSemanticWarning(
            "applying implicit casting because of not boolean type in while expression");
        if (InsertTypeCastExpression(&statement->expression, statement->expression, TYPE_ID_BOOL)
            == -1) {
            Semantics::ReportSemanticError("Unable to cast condition expression of while loop");
            return -1;
        }
        expressionTypeId = TYPE_ID_BOOL;
    }
    auto bodyScope = Scope::createInnerScope(GetCurrentScope(), "body");
    statement->body->scopePtr = bodyScope;
    Semantics::EnterScope(bodyScope);
    ProcessBodyElements(statement->body);
    Semantics::ExitCurrentScope();
    return 0;
}

// Process an individual body element (either a declaration or a statement) and validate its
// semantics.
int ProcessBodyElement(AstNode *node) {
    if (node->type == AstNodeType::DECLARATION) {
        auto declaration = static_cast<Declaration *>(node);
        return ProcessSimpleDeclaration(declaration);
    }
    auto statementNode = static_cast<Statement *>(node);
    switch (statementNode->statementType) {
        case StatementType::EXPRESSION_STATEMENT: {
            return ProcessExpressionStatement(static_cast<ExpressionStatement *>(statementNode));
        }
        case StatementType::RETURN_STATEMENT: {
            return ProcessReturnStatement(static_cast<ReturnStatement *>(statementNode));
        }
        case StatementType::IF_STATEMENT: {
            return ProcessIfStatement(static_cast<IfStatement *>(statementNode));
        }
        case StatementType::FOR_STATEMENT: {
            return ProcessForStatement(static_cast<ForLoopStatement *>(statementNode));
        }
        case StatementType::WHILE_STATEMENT: {
            return ProcessWhileStatement(static_cast<WhileLoopStatement *>(statementNode));
        }
    }
    return -1;
}

// Process all elements in a body, calling the appropriate function for each element.
int ProcessBodyElements(Body *body) {
    for (AstNode *node : body->nodeList) {
        auto result = ProcessBodyElement(node);
        if (result != 0) {
            Semantics::ReportSemanticError(std::string("processing body element for ")
                                           + GetCurrentScope()->getScopeName() + " is unable");
            node->isSemanticsValid = false;
        }
    }
    return 0;
}

// Process the RoutineDeclaration node in the semantic analysis process.
int ProcessRoutineDeclaration(Declaration *declaration) {
    RoutineDeclaration *routineDeclaration = static_cast<RoutineDeclaration *>(declaration);
    auto routine = Semantics::GetGlobalScope()->funcs()->defineFunc(routineDeclaration);
    currentFunction = routine;
    Semantics::EnterScope(routine->getScope());
    ProcessBodyElements(routineDeclaration->body);
    Semantics::ExitCurrentScope();
    currentFunction = routine;
    return 0;
}

// RunSemanticPass function applies the semantic analysis to the entire Program AST.
void Semantics::RunSemanticPass(Program *program) {
    // Obtain the declarations from the program.
    auto declarations = program->declarations;

    // Iterate over each declaration in the program.
    for (auto declaration : declarations) {
        // Process different types of declarations: TypeDeclaration, VariableDeclaration, and
        // RoutineDeclaration.
        if (declaration->declarationType == DeclarationType::TYPE_DECLARATION) {
            ProcessSimpleDeclaration(declaration);
            continue;
        }
        if (declaration->declarationType == DeclarationType::VARIABLE_DECLARATION) {
            ProcessSimpleDeclaration(declaration);
            continue;
        }
        if (declaration->declarationType == DeclarationType::ROUTINE_DECLARATION) {
            ProcessRoutineDeclaration(declaration);
            continue;
        }
    }
}