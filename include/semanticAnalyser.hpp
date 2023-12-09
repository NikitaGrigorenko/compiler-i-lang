#ifndef CC_PROJECT_SEMANTICS_H
#define CC_PROJECT_SEMANTICS_H

#include <cstring>
#include <iostream>
#include <llvm/IR/Function.h>
#include <map>
#include <ostream>
#include <string>
#include <vector>
#include "AST.hpp"

#define STRDUP(constCharPtr, outCharPtrPtr)                  \
    {                                                        \
        auto size = std::strlen(constCharPtr) + 1;           \
        *outCharPtrPtr = new char[size];                     \
        std::memcpy(*outCharPtrPtr, constCharPtr, size + 1); \
    }

// Unique counters for scopes, variables and types
enum Counter {
    SCOPE_COUNT = 0,
    VAR_COUNT = 1,
    TYPE_COUNT = 2,
};

// Get and increment unique counter based on the provided Counter enumeration
long long GetAndIncrementUniqueCounter(Counter c);

namespace Semantics {

const int TYPE_ID_VOID = 0;
const int TYPE_ID_BOOL = 1;
const int TYPE_ID_INT = 2;
const int TYPE_ID_FLOAT = 3;

const int OP_CAST_BOOL_TO_FLOAT = 10;
const int OP_CAST_BOOL_TO_INT = 11;
const int OP_CAST_INT_TO_FLOAT = 12;

const int OP_CAST_FLOAT_TO_INT = 13;
const int OP_CAST_INT_TO_BOOL = 14;
const int OP_CAST_FLOAT_TO_BOOL = 15;

enum SpecialFunctionType {
    CONSTRUCTOR_FUNC,
    DESTRUCTOR_FUNC,
};

struct Type;
struct Name;
struct Function;
class Scope;

TypeID FindNativeBaseType(TypeID typeId);

// The `struct Type` defines the properties and methods of a type in the programming language.
struct Type {
    friend class TypeTable;

  protected:
    bool primitive{false};

    bool array{false};

    bool native{false};

    std::string name;

    int size;

    TypeID typeId{-1};

    TypeID baseTypeId{-1};
    union {
        struct {
            TypeID nativeTypeID{-2};
        };
        struct {
            Expression *arraySize;
        };
    };

  public:
    Type(bool primitive, bool array, std::string name, TypeID typeId, TypeID baseTypeId, int size) :
        primitive(primitive), array(array), native(baseTypeId == -1), name(name), size(size),
        typeId(typeId), baseTypeId(baseTypeId) {}

    Type(std::string name, TypeID typeId, TypeID elementId, Expression *size) :
        array(true), name(name), typeId(typeId), baseTypeId(elementId), arraySize(size) {}

    ~Type() {}

    bool isNative() const { return native; }

    bool isPrimitive() const { return primitive; }

    bool isArray() const { return array; }

    const std::string getName() const { return std::string(name); }

    const int getSize() const { return size; }

    Expression *getArraySize() { return arraySize; }

    const TypeID getType() const { return typeId; }

    const TypeID getBaseType() const { return baseTypeId; }

    TypeID getNativeType() {
        if (nativeTypeID == -2) {
            // to cache value
            nativeTypeID = FindNativeBaseType(getType());
        }
        return nativeTypeID;
    }

  public:
    static Type *ofTypeDeclaration(TypeDeclaration *typeDeclaration) {
        return static_cast<Type *>(typeDeclaration->typePtr);
    }
};

// The `struct Name` represents a name (identifier) in the programming language.
struct Name {
    friend class NameTable;

  protected:
    std::string name;
    Type *type;
    bool parameter{false};
    Function *parameterOwner{nullptr};
    std::map<Scope *, llvm::Value *> llvmValueMap;

  public:
    Name(std::string name, Type *type) : Name(name, type, false, nullptr) {}
    Name(std::string name, Type *type, bool isParameter, Function *owner) :
        name(name), type(type), parameter(isParameter), parameterOwner(owner) {}

    virtual ~Name() = default;
    void setLLVMValue(Scope *referrer, llvm::Value *value) { llvmValueMap[referrer] = value; }
    void resetLLVMValue(Scope *referrer) { llvmValueMap.erase(referrer); }
    bool isParameter() { return parameter; }
    llvm::Value *getLLVMValue(Scope *referrer) { return llvmValueMap[referrer]; }
    Function *getParameterOwner() { return parameterOwner; }
    virtual std::string &getName() { return name; }
    virtual Type *getType() { return type; }

  public:
    static Name *ofVariableDeclaration(VariableDeclaration *variableDeclaration) {
        return static_cast<Name *>(variableDeclaration->namePtr);
    }
};

// The `struct FunctionParameter` is defining a structure that represents a parameter of a function.
// It contains three members: `index`, `name`, and `type`.
struct FunctionParameter {
    int index;
    Name *name;
    Type *type;
};

// The `struct Function` defines the properties and methods of a function in the programming
// language.
struct Function {
    friend class FuncTable;

  protected:
    Scope *innerScope;
    std::string name;
    std::vector<FunctionParameter> parameters;
    TypeID returnType{0};
    Body *functionBody;
    llvm::Function *llvmFunction{nullptr};

  public:
    Function(Scope *visibilityScope, std::string functionName, Body *body) :
        Function(visibilityScope, functionName, body, TypeID(0)) {}

    Function(Scope *visibilityScope, std::string functionName, Body *body, TypeID returnType);

    Function(Scope *visibilityScope, std::string functionName, TypeID returnType);

    virtual ~Function();

    void setLLVMFunction(llvm::Function *function) { llvmFunction = function; }
    void addParameter(FunctionParameter parameter) { parameters.push_back(parameter); }
    void onFunctionDefined();

    Scope *getScope() { return innerScope; }
    TypeID getReturnTypeId() { return returnType; }
    llvm::Function *getLLVMFunction() { return llvmFunction; }
    std::vector<FunctionParameter> &getAllParameters() { return parameters; }
    virtual std::string &getName() { return name; }

    static Function *ofRoutineDeclaration(RoutineDeclaration *declaration) {
        return static_cast<Function *>(declaration->functionPtr);
    }
};

// The `NameTable` class is responsible for managing the names (variables) within a scope.
// It provides methods for defining and looking up names, as well as retrieving all names within the
// table.
class NameTable {
    friend Scope;

  public:
    virtual ~NameTable() = default;

  public:
    Name *defineName(VariableDeclaration *varDeclaration);
    virtual Name *lookupName(const std::string varName, bool *fromThis) = 0;
    virtual Name *lookupName(const std::string varName) {
        bool unused = true;
        return this->lookupName(varName, &unused);
    };

    virtual Name *defineName(Name *src) = 0;
    virtual std::vector<Name *> getAllNames() = 0;

  protected:
    virtual void setOwnerScope(Scope *scope) = 0;
    virtual NameTable *getParentTable() = 0;
    virtual Scope *getOwnerScope() = 0;
};

// The `TypeTable` class is responsible for managing the types within a scope.
// It provides methods for defining and looking up types, as well as retrieving all types within the
// table.
class TypeTable {
    friend Scope;

  public:
    virtual ~TypeTable() = default;

  public:
    Type *defineType(TypeDeclaration *typeDeclaration);

    Type *defineType(const std::string typeName, TypeDefinition *src);

    virtual Type *lookupType(const std::string typeName) = 0;

    virtual Type *lookupType(const TypeID typeId) = 0;
    virtual Type *defineType(Type *src) = 0;
    virtual std::vector<Type *> getAllTypes() = 0;

  protected:
    virtual void setOwnerScope(Scope *scope) = 0;
    virtual TypeTable *getParentTable() = 0;
};

// The `FuncTable` class is responsible for managing the functions within a scope.
// It provides methods for defining and looking up functions, as well as retrieving all functions
// within the table.
class FuncTable {
    friend Scope;

  public:
    virtual ~FuncTable() = default;

  public:
    Function *defineFunc(RoutineDeclaration *funcDeclaration);
    virtual Function *lookupFunc(const std::string funcName) = 0;
    virtual Function *defineFunc(Function *src) = 0;
    virtual std::vector<Function *> getAllFuncs() = 0;

  protected:
    virtual void setOwnerScope(Scope *scope) = 0;
};

// The `Scope` class represents a scope in the programming language.
// It contains a parent scope, as well as tables for names, types, and functions within the scope.
// It also has a flag indicating whether it is a global scope, a name for the scope, and lists of
// child scopes and fallthrough names.
class Scope {
    Scope *parentScope;
    NameTable *nameTable;
    TypeTable *typeTable;
    FuncTable *funcTable;
    bool isGlobalScope;
    std::string scopeName;
    std::vector<Scope *> children;
    std::vector<Name *> namesFallthroughList;

  public:
    Scope(Scope *parentScope, NameTable *nameTable, TypeTable *typeTable, std::string name);

    ~Scope();

    std::string getScopeName() { return scopeName; }
    bool isGlobal() { return isGlobalScope; }
    Scope *parent() { return parentScope; }
    NameTable *names() { return nameTable; }
    TypeTable *types() { return typeTable; }
    FuncTable *funcs() { return funcTable; }

    void insertNameFallthrough(Name *name) { namesFallthroughList.push_back(name); }

    std::vector<Name *> &getAllFallthroughNames() { return namesFallthroughList; }

  private:
    void onChildScopeAttached(Scope *child);

  public:
    static Scope *ofBody(Body *body) { return static_cast<Scope *>(body->scopePtr); }
    static Scope *createInnerScope(Scope *parent, std::string name);
};

// Next we can see the headers for functions
int InitSemanticContext();

int DestroySemanticContext();

Scope *GetGlobalScope();

Scope *GetCurrentScope();

bool EnterScope(Scope *scope);

bool ExitCurrentScope();

void SetCallStatementStatus(bool isSingleCall);

FuncTable *GetFunctionTable();

void AddUserType(Type *type);

std::vector<Type *> &GetUserTypesList();

void ReportSemanticWarning(std::string message);

void ReportSemanticError(std::string message);

void RunSemanticPass(Program *program);

TypeID RunTypeTrace(Expression *expressionNode);

TypeID InsertTypeCastExpression(Expression **refFromParent, Expression *child, TypeID targetType,
                                bool isExplicit = false);

TypeID InsertTypeCastExpression(Expression **refFromParent, Expression *child, TypeID sourceType,
                                TypeID targetType, bool isExplicit = false);

TypeID ApplyTypeWidening(BinaryExpression *binaryExpression);

TypeID ApplyTypeWidening(UnaryExpression *unaryExpression);

TypeID IncrementAndGetTypeID();

TypeID FindNativeBaseType(TypeID typeId);

} // namespace Semantics

#endif // CC_PROJECT_SEMANTICS_H
