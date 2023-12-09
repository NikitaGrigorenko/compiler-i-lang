#ifndef CC_PROJECT_TREE_H
#define CC_PROJECT_TREE_H

#include <list>
#include <string>

typedef long long TypeID;

enum AstNodeType {
    PROGRAM,
    DECLARATION,
    DEFINITION,
    STATEMENT,
    EXPRESSION,
    LIST,
};

enum DeclarationType {
    VARIABLE_DECLARATION = 1,
    TYPE_DECLARATION = 2,
    PARAMETER_DECLARATION = 3,
    ROUTINE_DECLARATION = 4,
};

enum StatementType {
    EXPRESSION_STATEMENT,
    RETURN_STATEMENT,
    IF_STATEMENT,
    WHILE_STATEMENT,
    FOR_STATEMENT,
};

struct Declaration;
struct Statement;
struct Expression;
struct Primary;

struct Program;
struct Declaration;
struct TypeDeclaration;
struct VariableDeclaration;
struct RoutineDeclaration;
struct TypeDefinition;
struct Body;
struct TypeDefinition;
struct ModifiablePrimary;

// Defines a struct called `AstNode`. It has two member variables: `type` and `isSemanticsValid`.
struct AstNode {
    int type : 7;
    bool isSemanticsValid : 1;

    explicit AstNode(int type) : type(type), isSemanticsValid(true) {} //, line(0), column(0) {}
    virtual ~AstNode() = default;

    Statement *toStatement();
    Declaration *toDeclaration();
};

void SetProgramTree(Program *program);

Program *GetProgramTree();

// Defines a struct called "Program" which is a subclass of "AstNode". It has a member variable
// called "declarations" which is a list of pointers to "Declaration" objects.
struct Program : AstNode {
    std::list<Declaration *> declarations;

    Program() : AstNode(PROGRAM) { SetProgramTree(this); }

    virtual ~Program();

    Program *addSimpleDeclaration(Declaration *declaration) {
        declarations.push_back(declaration);
        return this;
    }

    Program *addRoutineDeclaration(Declaration *declaration) {
        declarations.push_back(declaration);
        return this;
    }
};

// Defines a struct called "Declaration" that inherits from the "AstNode" class. It has a member
// variable called "declarationType" of type int, which is initialized to -1.
struct Declaration : AstNode {
    int declarationType = -1;

    explicit Declaration(int declarationType) :
        AstNode(DECLARATION), declarationType(declarationType) {}

    virtual ~Declaration();

    VariableDeclaration *toVariableDeclaration();

    TypeDeclaration *toTypeDeclaration();

    RoutineDeclaration *toRoutineDeclaration();
};

// Defines a struct called `VariableDeclaration` which is a subclass of `Declaration`.
struct VariableDeclaration : Declaration {
    char *varName;
    TypeDefinition *type;
    Expression *initialValue;
    void *namePtr{nullptr};

    VariableDeclaration(char *varName, TypeDefinition *type, Expression *initialValue) :
        Declaration(VARIABLE_DECLARATION), varName(varName), type(type),
        initialValue(initialValue) {}

    virtual ~VariableDeclaration();
};

// Defines a struct called VariableDeclarationList that inherits from the AstNode struct. It has a
// member variable called variableDeclarationList which is a list of pointers to VariableDeclaration
// objects.
struct VariableDeclarationList : AstNode {
    std::list<VariableDeclaration *> variableDeclarationList;

    explicit VariableDeclarationList(VariableDeclaration *variableDeclaration) : AstNode(LIST) {
        variableDeclarationList.push_back(variableDeclaration);
    }

    VariableDeclarationList *addVariableDeclaration(VariableDeclaration *variableDeclaration) {
        variableDeclarationList.push_back(variableDeclaration);
        return this;
    }

    virtual ~VariableDeclarationList();
};

// Defines a struct called `ParameterDeclaration` which is a subclass of `Declaration`. It has two
// member variables `identifier` and `typeName`, both of type `char*`. The struct also has a
// constructor that takes two `char*` arguments and initializes the member variables. The
// constructor also calls the constructor of the base class `Declaration` with the argument
// `PARAMETER_DECLARATION`. There is also a virtual destructor defined for the struct.
struct ParameterDeclaration : Declaration {
    char *identifier;
    char *typeName;

    ParameterDeclaration(char *identifier, char *typeName) :
        Declaration(PARAMETER_DECLARATION), identifier(identifier), typeName(typeName) {}

    virtual ~ParameterDeclaration();
};

// ParameterList is a container for a list of parameter declarations in a routine (e.g., (int x, int
// y, int z))
struct ParameterList : AstNode {
    ParameterList() : AstNode(LIST) {}

    std::list<ParameterDeclaration *> parameterList;

    explicit ParameterList(ParameterDeclaration *parameterDeclaration) : AstNode(LIST) {
        parameterList.push_back(parameterDeclaration);
    }

    ParameterList *addParameter(ParameterDeclaration *parameterDeclaration) {
        parameterList.push_back(parameterDeclaration);
        return this;
    }

    virtual ~ParameterList();
};

// RoutineDeclaration represents a function or procedure declaration in the language
struct RoutineDeclaration : Declaration {
    char *routineName;
    ParameterList *parameters;
    TypeDefinition *returnType;
    Body *body;
    void *functionPtr{nullptr};

    RoutineDeclaration(char *routineName, ParameterList *parameters, TypeDefinition *returnType,
                       Body *body) :
        Declaration(ROUTINE_DECLARATION),
        routineName(routineName), parameters(parameters), returnType(returnType), body(body) {}

    virtual ~RoutineDeclaration();
};

// Defining a struct called `TypeDeclaration` that inherits from a class called
// `Declaration`.
struct TypeDeclaration : Declaration {
    char *typeName;
    TypeDefinition *typeDefinition;
    void *typePtr{nullptr};

    TypeDeclaration(char *typeName, TypeDefinition *typeDefinition) :
        Declaration(TYPE_DECLARATION), typeName(typeName), typeDefinition(typeDefinition) {}

    virtual ~TypeDeclaration();
};

// Defines a struct called `TypeDefinition` which is a subclass of `AstNode`. It has several member
// variables including `isReady`, `isNamed`, `isArray`, and `typeId`.
struct TypeDefinition : AstNode {
    bool isReady;
    bool isNamed;
    bool isArray;
    TypeID typeId;

    TypeDefinition(bool isNamed, bool isArray = false, TypeID typeId = -1) :
        AstNode(DEFINITION), isReady(false), isNamed(isNamed), isArray(isArray), typeId(typeId) {}

    TypeID getTypeId() {
        if (isArray) {
            isReady = true;
            return typeId;
        }
        if (!isReady) {
            typeId = obtainTypeId();
            isReady = true;
        }
        return typeId;
    }

    virtual ~TypeDefinition();

  protected:
    virtual TypeID obtainTypeId() = 0;
};

// Defines a struct named `NamedTypeDefinition` that inherits from `TypeDefinition`. It has a member
// variable `name` of type `char*`.
struct NamedTypeDefinition : TypeDefinition {
    char *name;

    explicit NamedTypeDefinition(int yaccKeyword);

    explicit NamedTypeDefinition(char *typeName) : TypeDefinition(true) { this->name = typeName; }

    TypeID obtainTypeId() override;

    virtual ~NamedTypeDefinition();
};

// Defines a struct called `ArrayTypeDefinition` which is a subclass of `TypeDefinition`. It has
// three member variables: `sizeExpression` which is a pointer to an `Expression` object,
// `arrayElementType` which is a pointer to a `TypeDefinition` object, and `typeId` which is
// inherited from `TypeDefinition`.
struct ArrayTypeDefinition : TypeDefinition {
    Expression *sizeExpression;
    TypeDefinition *arrayElementType;

    explicit ArrayTypeDefinition(Expression *constSizeExpression, TypeDefinition *type) :
        TypeDefinition(false, true, type->getTypeId()), sizeExpression(constSizeExpression),
        arrayElementType(type) {}

    TypeID obtainTypeId() override;

    virtual ~ArrayTypeDefinition();
};

// Defines a struct called "Statement" that inherits from the "AstNode" struct. It has a member
// variable called "statementType" of type "StatementType". The constructor of the "Statement"
// struct takes a "StatementType" parameter and initializes the "statementType" member variable. The
// "Statement" struct also has a virtual destructor.
struct Statement : AstNode {
    StatementType statementType;

    explicit Statement(StatementType statementType) :
        AstNode(STATEMENT), statementType(statementType) {}

    virtual ~Statement();
};

// Defines a struct called ExpressionStatement that inherits from the Statement struct. It has a
// member variable called expression, which is a pointer to an Expression object. The constructor
// initializes the expression member variable, and the destructor is declared but not defined.
struct ExpressionStatement : Statement {
    Expression *expression;
    ExpressionStatement(Expression *expression) :
        Statement(EXPRESSION_STATEMENT), expression(expression) {}

    virtual ~ExpressionStatement();
};

// Defines a struct called ReturnStatement that inherits from the Statement struct. It has a member
// variable called expressionToReturn, which is a pointer to an Expression object. The constructor
// takes an Expression pointer as a parameter and initializes the expressionToReturn member
// variable. The code also declares a virtual destructor for the ReturnStatement struct.
struct ReturnStatement : Statement {
    Expression *expressionToReturn;
    ReturnStatement(Expression *expression) :
        Statement(RETURN_STATEMENT), expressionToReturn(expression) {}

    virtual ~ReturnStatement();
};

// Defines a struct called `ArgumentList` which is a subclass of `AstNode`. It represents a list of
// arguments in an abstract syntax tree (AST).
struct ArgumentList : AstNode {
    ArgumentList() : AstNode(LIST) {}

    std::list<Expression *> expressionList;

    explicit ArgumentList(Expression *expression) : AstNode(LIST) {
        expressionList.push_back(expression);
    }

    ArgumentList *addArgument(Expression *expression) {
        expressionList.push_back(expression);
        return this;
    }

    virtual ~ArgumentList();
};

// Defines a struct called "Body" which is a subclass of "AstNode". It has a constructor that
// initializes the type of the node to "STATEMENT".
struct Body : AstNode {
    Body() : AstNode(STATEMENT) {}

    void *scopePtr{nullptr};
    std::list<AstNode *> nodeList;

    void onTypeDeclared(TypeDeclaration *declaration);

    void onVariableDeclared(VariableDeclaration *declaration);

    void onStatementAdded(Statement *statement);

    Body *addNode(AstNode *node) {
        nodeList.push_back(node);
        if (node->type == AstNodeType::DECLARATION) {
            auto declaration = static_cast<Declaration *>(node);
            if (declaration->declarationType == DeclarationType::TYPE_DECLARATION) {
                onTypeDeclared(static_cast<TypeDeclaration *>(node));
            } else {
                onVariableDeclared(static_cast<VariableDeclaration *>(node));
            }
        } else {
            onStatementAdded(static_cast<Statement *>(node));
        }
        return this;
    }

    virtual ~Body();
};

// Defines a struct called "WhileLoopStatement" which is a subclass of the "Statement" class. It has
// two member variables: "expression" which is a pointer to an Expression object, and "body" which
// is a pointer to a Body object.
struct WhileLoopStatement : Statement {
    Expression *expression;
    Body *body;

    WhileLoopStatement(Expression *expression, Body *body) :
        Statement(WHILE_STATEMENT), expression(expression), body(body) {}
    virtual ~WhileLoopStatement();
};

// Defines a struct called "Range" that inherits from the "AstNode" class. The "Range" struct has
// two member variables: "lo" and "hi", both of which are pointers to Expression objects. The
// constructor of the "Range" struct takes two Expression pointers as arguments and initializes the
// "lo" and "hi" member variables. The constructor also calls the constructor of the base class
// "AstNode" with the argument "LIST". The code also declares a virtual destructor for the "Range"
// struct.
struct Range : AstNode {
    Expression *lo;
    Expression *hi;

    Range(Expression *lo, Expression *hi) : AstNode(LIST), lo(lo), hi(hi) {}

    virtual ~Range();
};

// Defines a struct called `ForLoopStatement` which is a subclass of `Statement`. It has several
// member variables including `identifier` (a character pointer), `range` (a pointer to a `Range`
// object), `reverse` (a boolean value), and `body` (a pointer to a `Body` object).
struct ForLoopStatement : Statement {
    char *identifier;
    Range *range;
    bool reverse;
    Body *body;

    ForLoopStatement(char *identifier, Range *range, bool reverse, Body *body) :
        Statement(FOR_STATEMENT), identifier(identifier), range(range), reverse(reverse),
        body(body) {
        if (reverse) {
            auto rangeTemp = range->hi;
            range->hi = range->lo;
            range->lo = rangeTemp;
        }
    }
    virtual ~ForLoopStatement();
};

// The above code is defining a struct called IfStatement that inherits from the Statement struct.
// It has three member variables: expression, trueBody, and falseBody. The expression variable is a
// pointer to an Expression object, while trueBody and falseBody are pointers to Body objects.
struct IfStatement : Statement {
    Expression *expression;
    Body *trueBody;
    Body *falseBody;

    IfStatement(Expression *expression, Body *trueBody, Body *falseBody) :
        Statement(IF_STATEMENT), expression(expression), trueBody(trueBody), falseBody(falseBody) {}

    virtual ~IfStatement();
};

// Defines a struct called "Expression" that inherits from a base class called "AstNode". The struct
// has several member variables including "isPrimary" and "isBinary" which are boolean flags
// indicating whether the expression is a primary or binary expression. It also has a member
// variable "expressionType" which is of type "TypeID" and represents the type of the expression.
// The struct also has a member variable "llvmValuePtr" which is a void pointer to an LLVM value.
struct Expression : AstNode {
    bool isPrimary;
    bool isBinary;
    TypeID expressionType{-1};
    void *llvmValuePtr;

    explicit Expression(bool isPrimary, bool isBinary) :
        AstNode(EXPRESSION), isPrimary(isPrimary), isBinary(isBinary) {}

    explicit Expression(bool isPrimary, bool isBinary, TypeID expressionType) :
        AstNode(EXPRESSION), isPrimary(isPrimary), isBinary(isBinary),
        expressionType(expressionType) {}

    virtual ~Expression();
};

// Defines a struct called `UnaryExpression` which is a subclass of `Expression`. It has three
// member variables: `oper`, `nativeOper`, and `operand`.
struct UnaryExpression : Expression {
    int oper;
    int nativeOper{-1};
    Expression *operand;

    UnaryExpression(int oper, Expression *operand) :
        Expression(false, false), oper(oper), operand(operand) {}

    virtual ~UnaryExpression();
};

// Defines a struct called BinaryExpression that inherits from the Expression struct. It represents
// a binary expression, which is an expression that operates on two operands.
struct BinaryExpression : Expression {
    int oper;
    int nativeOper{-1};
    Expression *operand1;
    Expression *operand2;

    BinaryExpression(int oper, Expression *operand1, Expression *operand2) :
        Expression(false, true), oper(oper), operand1(operand1), operand2(operand2) {}

    virtual ~BinaryExpression();
};

// Defines a struct named "Primary" that inherits from the "Expression" struct. The "Primary" struct
// has two boolean member variables: "isConst" and "isCall". It also has two explicit constructors
// that take in values for "isConst", "isCall", and "expressionType". The constructors initialize
// the "Expression" base class with the values true and false, and also initialize the "isConst" and
// "isCall" member variables with the provided values. The struct also declares a virtual
// destructor.
struct Primary : Expression {
    bool isConst;
    bool isCall;

    explicit Primary(bool isConst, bool isCall) :
        Expression(true, false), isConst(isConst), isCall(isCall) {}

    explicit Primary(bool isConst, bool isCall, TypeID expressionType) :
        Expression(true, false, expressionType), isConst(isConst), isCall(isCall) {}

    virtual ~Primary();
};

// Defining an enumeration called "ConstPrimaryType" with three possible values:
// INTEGER_PRIMARY_TYPE, REAL_PRIMARY_TYPE, and BOOLEAN_PRIMARY_TYPE. This enumeration is used to
// represent different types of primary data types.
enum ConstPrimaryType {
    INTEGER_PRIMARY_TYPE,
    REAL_PRIMARY_TYPE,
    BOOLEAN_PRIMARY_TYPE,
};

// Defines a struct called `ConstPrimary` which is a subclass of `Primary`. It has a member variable
// `type` of type `ConstPrimaryType` and a union that can hold an `int`, `float`, or `bool` value.
struct ConstPrimary : Primary {
    ConstPrimaryType type{};
    union {
        int intValue;
        float realValue;
        bool boolValue;
    };

    explicit ConstPrimary(int intValue) :
        Primary(true, false, 2), type(INTEGER_PRIMARY_TYPE), intValue(intValue) {}

    explicit ConstPrimary(bool boolValue) :
        Primary(true, false, 1), type(BOOLEAN_PRIMARY_TYPE), boolValue(boolValue) {}

    explicit ConstPrimary(float realValue) :
        Primary(true, false, 3), type(REAL_PRIMARY_TYPE), realValue(realValue) {}

    virtual ~ConstPrimary();
};

// Defines a struct called `ModifiablePrimary` which is derived from another struct called
// `Primary`.
struct ModifiablePrimary : Primary {
    char *identifier{};
    bool isAccessMember;
    ModifiablePrimary *member = nullptr;
    Expression *index = nullptr;

    explicit ModifiablePrimary(char *identifier) :
        Primary(false, false), identifier(identifier), isAccessMember(false) {}

    ModifiablePrimary(char *identifier, ModifiablePrimary *member) :
        Primary(false, false), identifier(identifier), isAccessMember(true), member(member) {}

    ModifiablePrimary(char *identifier, Expression *index) :
        Primary(false, false), identifier(identifier), isAccessMember(false), index(index) {}

    ModifiablePrimary(char *identifier, Expression *index, ModifiablePrimary *member) :
        Primary(false, false), identifier(identifier), isAccessMember(true), member(member),
        index(index) {}

    virtual ~ModifiablePrimary();
};

// Defines a struct called CallablePrimary which is a subclass of Primary. It has three member
// variables: modifiablePrimary, identifier, and argumentList.
struct CallablePrimary : Primary {
    ModifiablePrimary *modifiablePrimary;
    char *identifier;
    ArgumentList *argumentList;

    CallablePrimary(ModifiablePrimary *identifier, ArgumentList *argumentList) :
        Primary(false, true), modifiablePrimary(identifier), identifier(identifier->identifier),
        argumentList(argumentList) {}

    virtual ~CallablePrimary();
};

#endif // CC_PROJECT_TREE_H
