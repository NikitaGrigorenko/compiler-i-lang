#include "AST.hpp"
#include <iostream>
#include "Parser.tab.hpp"
#include "semanticAnalyser.hpp"

using namespace Semantics;

// Helper functions to downcast AstNode pointers to derived classes.
Statement *AstNode::toStatement() {
    return static_cast<Statement *>(this);
}

Declaration *AstNode::toDeclaration() {
    return static_cast<Declaration *>(this);
}

// Destructor for Program struct, responsible for cleaning up declaration
// pointers.
Program::~Program() {
    for (auto declaration : declarations) {
        delete declaration;
    }
}

Declaration::~Declaration() = default;

// Function to downcast Declaration pointers to derived classes.
VariableDeclaration *Declaration::toVariableDeclaration() {
    if (declarationType == VARIABLE_DECLARATION) {
        return static_cast<VariableDeclaration *>(this);
    } else {
        return nullptr;
    }
}

// Function to downcast Declaration pointers to derived classes.
TypeDeclaration *Declaration::toTypeDeclaration() {
    if (declarationType == TYPE_DECLARATION) {
        return static_cast<TypeDeclaration *>(this);
    } else {
        return nullptr;
    }
}

// Function to downcast Declaration pointers to derived classes.
RoutineDeclaration *Declaration::toRoutineDeclaration() {
    if (declarationType == ROUTINE_DECLARATION) {
        return static_cast<RoutineDeclaration *>(this);
    } else {
        return nullptr;
    }
}
// Destructor for VariableDeclaration struct, responsible for cleaning up declaration
// pointers.
VariableDeclaration::~VariableDeclaration() {
    std::free(varName);
    delete type;
    delete initialValue;
}

// Destructor for VariableDeclarationList struct, responsible for cleaning up declaration
// pointers.
VariableDeclarationList::~VariableDeclarationList() {
    for (auto declaration : variableDeclarationList) {
        delete declaration;
    }
}

// Destructor for ParameterDeclaration struct, responsible for cleaning up declaration
// pointers.
ParameterDeclaration::~ParameterDeclaration() {
    std::free(identifier);
    std::free(typeName);
}

// Destructor for ParameterList struct, responsible for cleaning up declaration
// pointers.
ParameterList::~ParameterList() {
    for (auto declaration : parameterList) {
        delete declaration;
    }
}

// Destructor for RoutineDeclaration struct, responsible for cleaning up declaration
// pointers.
RoutineDeclaration::~RoutineDeclaration() {
    std::free(routineName);
    delete parameters;
    delete returnType;
    delete body;
}

// Destructor for TypeDeclaration struct, responsible for cleaning up declaration
// pointers.
TypeDeclaration::~TypeDeclaration() {
    std::free(typeName);
    delete typeDefinition;
}

// Destructor for TypeDefinition struct, responsible for cleaning up declaration
// pointers.
TypeDefinition::~TypeDefinition() = default;

// Destructor for NamedTypeDefinition struct, responsible for cleaning up declaration
// pointers.
NamedTypeDefinition::~NamedTypeDefinition() {
    std::free(name);
}

// Destructor for ArrayTypeDefinition struct, responsible for cleaning up declaration
// pointers.
ArrayTypeDefinition::~ArrayTypeDefinition() {
    delete sizeExpression;
    delete arrayElementType;
}

// Destructor for Statement struct, responsible for cleaning up declaration
// pointers.
Statement::~Statement() = default;

// Destructor for ExpressionStatement struct, responsible for cleaning up declaration
// pointers.
ExpressionStatement::~ExpressionStatement() {
    delete expression;
}

// Destructor for ReturnStatement struct, responsible for cleaning up declaration
// pointers.
ReturnStatement::~ReturnStatement() {
    delete expressionToReturn;
}

// Destructor for ArgumentList struct, responsible for cleaning up declaration
// pointers.
ArgumentList::~ArgumentList() {
    for (auto expression : expressionList) {
        delete expression;
    }
}

// Destructor for Body struct, responsible for cleaning up declaration
// pointers.
Body::~Body() {
    for (auto node : nodeList) {
        delete node;
    }
}

// Destructor for WhileLoopStatement struct, responsible for cleaning up declaration
// pointers.
WhileLoopStatement::~WhileLoopStatement() {
    delete expression;
    delete body;
}

// Destructor for Range struct, responsible for cleaning up declaration
// pointers.
Range::~Range() {
    delete lo;
    delete hi;
}

// Destructor for ForLoopStatement struct, responsible for cleaning up declaration
// pointers.
ForLoopStatement::~ForLoopStatement() {
    std::free(identifier);
    delete range;
    delete body;
}

// Destructor for IfStatement struct, responsible for cleaning up declaration
// pointers.
IfStatement::~IfStatement() {
    delete expression;
    delete trueBody;
    delete falseBody;
}

// Destructor for Expression struct, responsible for cleaning up declaration
// pointers.
Expression::~Expression() = default;

// Destructor for UnaryExpression struct, responsible for cleaning up declaration
// pointers.
UnaryExpression::~UnaryExpression() {
    delete operand;
}

// Destructor for BinaryExpression struct, responsible for cleaning up declaration
// pointers.
BinaryExpression::~BinaryExpression() {
    delete operand1;
    delete operand2;
}

// Destructor for Primary struct, responsible for cleaning up declaration
// pointers.
Primary::~Primary() = default;

// Destructor for ConstPrimary struct, responsible for cleaning up declaration
// pointers.
ConstPrimary::~ConstPrimary() = default;

// Destructor for ModifiablePrimary struct, responsible for cleaning up declaration
// pointers.
ModifiablePrimary::~ModifiablePrimary() {
    std::free(identifier);
    if (isAccessMember) {
        delete member;
    } else {
        delete index;
    }
}

// Destructor for CallablePrimary struct, responsible for cleaning up declaration
// pointers.
CallablePrimary::~CallablePrimary() {
    delete modifiablePrimary;
    delete argumentList;
}

// Global variable for storing the program tree.
Program *programModule = nullptr;

// Function to set the program tree.
void SetProgramTree(Program *program) {
    programModule = program;
}

// Function to get the program tree.
Program *GetProgramTree() {
    return programModule;
}

// NamedTypeDefinition::obtainTypeId gets the TypeID of a NamedTypeDefinition
// object in the current scope. It returns -1 if the base type cannot be found.
TypeID NamedTypeDefinition::obtainTypeId() {
    // this type should be either native or loaded already loaded
    auto baseType = GetCurrentScope()->types()->lookupType(this->name);
    if (baseType == nullptr) {
        ReportSemanticError(std::string("unable to create base type ") + this->name);
        return -1;
    }
    return baseType->getType();
}

// Constructor for NamedTypeDefinition class, takes a yaccKeyword and
// initializes the 'name' member accordingly.
NamedTypeDefinition::NamedTypeDefinition(int yaccKeyword) : TypeDefinition(true) {
    switch (yaccKeyword) {
        case BOOLEAN: {
            STRDUP("bool", &name)
            break;
        }
        case INTEGER: {
            STRDUP("int", &name)
            break;
        }
        case REAL: {
            STRDUP("float", &name)
            break;
        }
    }
}

// ArrayTypeDefinition::obtainTypeId returns TypeID for Array Type Definitions.
TypeID ArrayTypeDefinition::obtainTypeId() {
    return 0;
}

// These methods are called when corresponding nodes are added to the body.
void Body::onTypeDeclared(TypeDeclaration *declaration) {
    // use lazy type loading via getter
    if (declaration->typeDefinition->getTypeId() == -1) {
    }
}

void Body::onVariableDeclared(VariableDeclaration *declaration) {}

void Body::onStatementAdded(Statement *statement) {}
