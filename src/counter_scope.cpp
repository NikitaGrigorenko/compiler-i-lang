#include <algorithm>
#include <map>
#include "semanticAnalyser.hpp"

using namespace Semantics;

FuncTable *globalFunctionTable;
NameTable *globalNameTable;
TypeTable *globalTypeTable;

Scope *globalScope;
Scope *currentScope;

/* The ScopeNameTable class is a subclass of NameTable that represents a table of names within a
specific scope, allowing for name lookup, definition, and retrieval. */
class ScopeNameTable : public NameTable {
    Scope *owner;

    std::map<std::string, Name *> nameMap;

    // Destructor for the ScopeNameTable class, deallocates memory for all Name objects in the
    // nameMap.
    ~ScopeNameTable() {
        for (auto pair : nameMap) {
            delete pair.second;
        }
    };

    // Looks up a name in the current scope and its parent scopes, returns the corresponding Name
    // object if found, and updates the 'fromThis' flag.
    Name *lookupName(const std::string varName, bool *fromThis) override {
        if (nameMap.count(varName) != 0) {
            *fromThis = true;
            return nameMap[varName];
        } else {
            *fromThis = false;
            auto parentTable = getParentTable();
            if (parentTable == nullptr) {
                return nullptr;
            } else {
                return parentTable->lookupName(varName);
            }
        }
    }

    // Adds a new name to the current ScopeNameTable, returns a pointer to the added Name object.
    Name *defineName(Name *name) override {
        nameMap[name->getName()] = name;
        return name;
    }

    // Returns a pointer to the parent scope's NameTable, or nullptr if the current scope is global.
    NameTable *getParentTable() override {
        if (this->owner->isGlobal()) {
            return nullptr;
        } else {
            return this->owner->parent()->names();
        }
    }

    // Returns a pointer to the owner Scope of the current table, or nullptr if the current scope is
    // global.
    Scope *getOwnerScope() override {
        if (this->owner->isGlobal()) {
            return nullptr;
        } else {
            return this->owner;
        }
    }

    // Sets the owner Scope of the current ScopeNameTable to the given scope pointer.
    void setOwnerScope(Semantics::Scope *scope) override { this->owner = scope; }

    // Returns a vector containing all Name objects in the current ScopeNameTable.
    std::vector<Name *> getAllNames() override {
        std::vector<Name *> nameList;
        for (auto it = nameMap.begin(); it != nameMap.end(); ++it) {
            nameList.push_back(it->second);
        }
        return nameList;
    }
};

/* The ScopeTypeTable class is a subclass of TypeTable that provides functionality for looking up,
defining, and retrieving types within a specific scope. */
class ScopeTypeTable : public TypeTable {
    Scope *owner;
    std::map<std::string, Type *> typeNameMap;
    std::map<TypeID, Type *> typeIDMap;

    // Destructor for the ScopeTypeTable class, deallocates memory for all Type objects in the
    // typeIDMap.
    ~ScopeTypeTable() {
        for (auto pair : typeIDMap) {
            delete pair.second;
        }
    }

    // Looks up a type by name in the current scope and its parent scopes, returns the corresponding
    // Type object if found.
    Type *lookupType(const std::string typeName) override {
        if (typeNameMap.count(typeName) != 0) {
            return typeNameMap[typeName];
        } else {
            auto parentTable = getParentTable();
            if (parentTable == nullptr) {
                return nullptr;
            } else {
                return parentTable->lookupType(typeName);
            }
        }
    }

    // Looks up a type by TypeID in the current scope and its parent scopes, returns the
    // corresponding Type object if found.
    Type *lookupType(const TypeID typeID) override {
        if (typeIDMap.count(typeID) != 0) {
            return typeIDMap[typeID];
        } else {
            auto parentTable = getParentTable();
            if (parentTable == nullptr) {
                return nullptr;
            } else {
                return parentTable->lookupType(typeID);
            }
        }
    }

    // Adds a new type to the current ScopeTypeTable, returns a pointer to the added Type object.
    Type *defineType(Type *type) override {
        typeNameMap[type->getName()] = type;
        typeIDMap[type->getType()] = type;
        return type;
    }

    // Returns a pointer to the parent scope's TypeTable, or nullptr if the current scope is global.
    TypeTable *getParentTable() override {
        if (this->owner->isGlobal()) {
            return nullptr;
        } else {
            return this->owner->parent()->types();
        }
    }

    // Sets the owner Scope of the current ScopeTypeTable to the given scope pointer.
    void setOwnerScope(Semantics::Scope *scope) override { this->owner = scope; }

    // Returns a vector containing all Type objects in the current ScopeTypeTable.
    std::vector<Type *> getAllTypes() override {
        std::vector<Type *> typeList;
        for (auto it = typeIDMap.begin(); it != typeIDMap.end(); ++it) {
            typeList.push_back(it->second);
        }
        return typeList;
    }
};

/* The GlobalFunctionTable class is a subclass of FuncTable that stores and manages a collection of
Function objects. */
class GlobalFunctionTable : public FuncTable {
    std::map<std::string, Function *> funcMap;

    // Destructor for the GlobalFunctionTable class, deallocates memory for all Function objects in
    // the funcMap.
    ~GlobalFunctionTable() {
        for (auto pair : funcMap) {
            delete pair.second;
        }
    }

    // Looks up a function by name in the GlobalFunctionTable, returns the corresponding Function
    // object if found.
    Function *lookupFunc(const std::string funcName) override {
        if (funcMap.count(funcName) == 0) {
            return nullptr;
        }

        return funcMap[funcName];
    }

    // Adds a new function to the GlobalFunctionTable, returns a pointer to the added Function
    // object. Calls the onFunctionDefined method on the Function.
    Function *defineFunc(Function *func) override {
        funcMap[func->getName()] = func;
        func->onFunctionDefined();
        return func;
    }

    void setOwnerScope(Semantics::Scope *scope) override {}

    std::vector<Function *> getAllFuncs() override {
        std::vector<Function *> funcList;
        for (auto it = funcMap.begin(); it != funcMap.end(); ++it) {
            funcList.push_back(it->second);
        }
        return funcList;
    }
};

/* The `FunctionTableWrap` class is a wrapper class that extends `FuncTable` and provides additional
functionality for looking up and defining functions. */
class FunctionTableWrap : public FuncTable {
    Scope *owner{};
    FuncTable *wrapTable;

  public:
    FunctionTableWrap(FuncTable *wrap) : wrapTable(wrap) {}

    virtual ~FunctionTableWrap() = default;

    Function *lookupFunc(const std::string funcName) override {
        return wrapTable->lookupFunc(funcName);
    }

    Function *defineFunc(Function *func) override {
        TypeTable *typeTable = owner->types();
        if (typeTable->lookupType(func->getName())) {
            return nullptr;
        }
        NameTable *nameTable = owner->names();
        if (nameTable->lookupName(func->getName())) {
            return nullptr;
        }
        return wrapTable->defineFunc(func);
    }

    void setOwnerScope(Semantics::Scope *scope) override { owner = scope; }

    std::vector<Function *> getAllFuncs() override { return wrapTable->getAllFuncs(); }
};

/**
 * The Function constructor initializes a new function object with a given visibility scope,
 * function name, body, and return type.
 *
 * @param visibilityScope The visibilityScope parameter is a pointer to the scope in which the
 * function is defined. It represents the scope in which the function is visible and can be
 * accessed.
 * @param functionName The name of the function being defined.
 * @param body A pointer to an object of type Body, which represents the body of the function.
 * @param returnType The `returnType` parameter is the type of the value that the function will
 * return. It specifies the data type of the value that will be returned when the function is
 * called.
 */
Function::Function(Scope *visibilityScope, std::string functionName, Body *body,
                   TypeID returnType) :
    name(functionName),
    returnType(returnType), functionBody(body) {
    auto typeTable = new ScopeTypeTable();
    auto nameTable = new ScopeNameTable();
    innerScope = new Scope(visibilityScope, nameTable, typeTable, "func$" + functionName);
}

/**
 * The Function constructor initializes a Function object with a visibility scope, function name,
 * return type, and a null function body.
 *
 * @param visibilityScope The visibility scope is a pointer to the scope in which the function is
 * defined. It determines the visibility of the function and the variables that can be accessed
 * within the function.
 * @param functionName A string representing the name of the function.
 * @param returnType The returnType parameter is the type of value that the function will return
 * when it is called. It specifies the data type of the value that will be returned by the function.
 */
Function::Function(Scope *visibilityScope, std::string functionName, TypeID returnType) :
    name(functionName), returnType(returnType), functionBody(nullptr) {
    innerScope = nullptr;
}

Function::~Function() {}

void Function::onFunctionDefined() {}

/**
 * The function defines a name for a variable declaration, determines its type, and assigns an
 * initial value if necessary.
 *
 * @param varDeclaration The parameter `varDeclaration` is of type `VariableDeclaration*`.
 *
 * @return a pointer to a Name object.
 */
Name *NameTable::defineName(VariableDeclaration *varDeclaration) {
    std::string varName(varDeclaration->varName);
    Type *type;
    if (varDeclaration->type != nullptr) {
        auto typeId = varDeclaration->type->getTypeId();
        auto typeTable = this->getOwnerScope()->types();
        type = typeTable->lookupType(typeId);
        if (type == nullptr) {
            ReportSemanticError(std::string("no such type for variable declaration"));
        }
    } else {
        if (varDeclaration->initialValue == nullptr) {
            ReportSemanticError(std::string("no initial value of ") + varDeclaration->varName
                                + ". Can not deduce type");
        }
        auto typeId = RunTypeTrace(varDeclaration->initialValue);
        type = this->getOwnerScope()->types()->lookupType(typeId);
    }
    if (type == nullptr) {
        return nullptr;
    }
    if (varDeclaration->initialValue == nullptr) {
        auto nativeInitialTypeId = type->getNativeType();
        switch (nativeInitialTypeId) {
            case TYPE_ID_INT: {
                varDeclaration->initialValue = new ConstPrimary(0);
                break;
            }
            case TYPE_ID_BOOL: {
                varDeclaration->initialValue = new ConstPrimary(false);
                break;
            }
            case TYPE_ID_FLOAT: {
                varDeclaration->initialValue = new ConstPrimary(0.0F);
                break;
            }
        }
    } else {
    }
    Name *name = new Name(varName, type);
    varDeclaration->namePtr = name;
    return this->defineName(name);
}

/**
 * The function defines a type in the type table and assigns the type pointer to the type
 * declaration.
 *
 * @param typeDeclaration TypeDeclaration object that contains information about the type being
 * defined, such as the type name and type definition.
 *
 * @return a pointer to the defined type.
 */
Type *TypeTable::defineType(TypeDeclaration *typeDeclaration) {
    auto type = this->defineType(typeDeclaration->typeName, typeDeclaration->typeDefinition);
    typeDeclaration->typePtr = type;
    return type;
}

/**
 * The function defines a new type in a type table based on the given type definition.
 *
 * @param typeName The `typeName` parameter is a string that represents the name of the type being
 * defined.
 * @param src The parameter `src` is a pointer to a `TypeDefinition` object. It is used to define a
 * new type based on the information provided in `src`. The type can be either a named type or an
 * array type.
 *
 * @return a pointer to a TypeTable object.
 */
Type *TypeTable::defineType(const std::string typeName, TypeDefinition *src) {
    TypeID baseTypeId = -1;
    if (src->isNamed) {
        auto namedSrc = static_cast<NamedTypeDefinition *>(src);
        auto nameType = this->lookupType(namedSrc->name);
        if (nameType == nullptr) {
            ReportSemanticError(std::string("no such base type: ") + namedSrc->name
                                + ": unable to define: " + typeName);
            return nullptr;
        }
        baseTypeId = nameType->getType();
        auto newType = new Type(true, false, typeName, IncrementAndGetTypeID(), baseTypeId,
                                nameType->getSize());
        auto definedType = this->defineType(newType);
        src->typeId = definedType->getType();
        return definedType;
    }
    if (src->isArray) {
        auto arraySrc = static_cast<ArrayTypeDefinition *>(src);
        auto elementSrc = arraySrc->arrayElementType;
        Type *elementType = nullptr;
        if (elementSrc->isNamed) {
            auto fieldTypeName = static_cast<NamedTypeDefinition *>(elementSrc)->name;
            elementType = lookupType(fieldTypeName);
        }

        auto indexType = RunTypeTrace(arraySrc->sizeExpression);
        if (indexType == -1) {
            Semantics::ReportSemanticError("unable to get array size type");
            return nullptr;
        }
        if (indexType != TYPE_ID_INT) {
            Semantics::ReportSemanticWarning("only integer type can be used as an array index");
            InsertTypeCastExpression(&arraySrc->sizeExpression, arraySrc->sizeExpression,
                                     TYPE_ID_INT, false);
        }
        auto newType = new Type(typeName, IncrementAndGetTypeID(), elementType->getType(),
                                arraySrc->sizeExpression);
        auto definedType = this->defineType(newType);
        src->typeId = definedType->getType();
        AddUserType(definedType);
        return definedType;
    }
}

/**
 * The defineFunc function defines a new function in a function table, including its name, return
 * type, parameters, and body.
 *
 * @param routineDeclaration A pointer to a RoutineDeclaration object, which contains information
 * about a function declaration, such as the function name, return type, parameters, and body.
 *
 * @return a pointer to a Function object.
 */
Function *FuncTable::defineFunc(RoutineDeclaration *routineDeclaration) {
    std::string name(routineDeclaration->routineName);
    TypeID returnValueTypeId = 0;
    if (routineDeclaration->returnType != nullptr) {
        returnValueTypeId = routineDeclaration->returnType->getTypeId();
    } else {
        returnValueTypeId = TYPE_ID_VOID;
    }
    auto *func = new Function(GetCurrentScope(), name, routineDeclaration->body, returnValueTypeId);
    int index = 0;
    for (auto paramNode : routineDeclaration->parameters->parameterList) {
        auto paramType = GetCurrentScope()->types()->lookupType(paramNode->typeName);
        if (!paramType) {
            Semantics::ReportSemanticError(paramNode->typeName + std::string("is unknown type name")
                                           + " for function " + name);
            return nullptr;
        }
        auto paramName = new Name(paramNode->identifier, paramType, true, func);
        func->getScope()->names()->defineName(paramName);
        func->addParameter(
            FunctionParameter{.index = index++, .name = paramName, .type = paramType});
    }
    routineDeclaration->functionPtr = func;
    routineDeclaration->body->scopePtr = func->getScope();
    return this->defineFunc(func);
}

int scopeNameCounter = 0;

/**
 * The Scope constructor initializes a new scope with a parent scope, name table, type table, and
 * name.
 *
 * @param parentScope A pointer to the parent scope of the current scope. If the current scope is a
 * global scope, then the parent scope is nullptr.
 * @param nameTable A pointer to an instance of the NameTable class.
 * @param typeTable The `typeTable` parameter is a pointer to an instance of the `TypeTable` class.
 * It is used to store and manage information about types in the scope.
 * @param name The name of the scope.
 */
Scope::Scope(Scope *parentScope, NameTable *nameTable, TypeTable *typeTable, std::string name) :
    parentScope(parentScope), nameTable(nameTable), typeTable(typeTable),
    isGlobalScope(parentScope == nullptr), scopeName(name) {
    this->funcTable = new FunctionTableWrap(globalFunctionTable);
    this->funcTable->setOwnerScope(this);
    if (this->typeTable) {
        this->typeTable->setOwnerScope(this);
    }
    if (this->nameTable) {
        this->nameTable->setOwnerScope(this);
    }
    if (this->parentScope != nullptr) {
        this->parentScope->onChildScopeAttached(this);
    }
}

/**
 * The destructor for the Scope class deletes all the child scopes and tables associated with the
 * scope.
 */
Scope::~Scope() {
    for (auto child : children) {
        delete child;
    }
    delete funcTable;
    delete nameTable;
    delete typeTable;
}

/**
 * The function adds a child scope to the list of children scopes.
 *
 * @param child The "child" parameter is a pointer to an object of type "Scope".
 */
void Scope::onChildScopeAttached(Scope *child) {
    children.push_back(child);
}

/**
 * The function creates a new inner scope with a given name and returns it.
 *
 * @param parent The parent scope that the inner scope will be created within.
 * @param name The "name" parameter is a string that represents the name of the inner scope that is
 * being created.
 *
 * @return a pointer to a newly created inner scope.
 */
Scope *Scope::createInnerScope(Scope *parent, std::string name) {
    auto typeTable = new ScopeTypeTable();
    auto nameTable = new ScopeNameTable();
    auto innerScope = new Scope(parent, nameTable, typeTable, parent->getScopeName() + "_" + name);
    return innerScope;
}

std::vector<Type *> userTypes;

/**
 * The function adds a user-defined type to a list of user types.
 *
 * @param type A pointer to an object of type Semantics::Type.
 */
void Semantics::AddUserType(Semantics::Type *type) {
    userTypes.push_back(type);
}

/**
 * The function returns a reference to a vector of Type pointers.
 *
 * @return A reference to a vector of pointers to objects of type "Type".
 */
std::vector<Type *> &Semantics::GetUserTypesList() {
    return userTypes;
}

/**
 * The EnterScope function sets the current scope to the given scope and returns true.
 *
 * @param scope The "scope" parameter is a pointer to an object of type "Scope".
 *
 * @return a boolean value of true.
 */
bool Semantics::EnterScope(Scope *scope) {
    currentScope = scope;
    return true;
}

/**
 * The function ExitCurrentScope() checks if the current scope is the global scope and if not, sets
 * the current scope to its parent and returns false.
 *
 * @return a boolean value of false.
 */
bool Semantics::ExitCurrentScope() {
    if (currentScope == globalScope) {
        return false;
    }
    currentScope = currentScope->parent();
    return false;
}

/**
 * The function returns the current scope.
 *
 * @return the value of the variable "currentScope", which is of type "Scope*".
 */
Scope *Semantics::GetCurrentScope() {
    return currentScope;
}

/**
 * The function GetGlobalScope() returns the global scope.
 *
 * @return a pointer to the global scope.
 */
Scope *Semantics::GetGlobalScope() {
    return globalScope;
}

/**
 * The function `InitBuiltinTypes` initializes the built-in types in a C++ program.
 */
void InitBuiltinTypes() {
    TypeTable *typeTable = globalScope->types();
    typeTable->defineType(new Type(true, false, "void", TYPE_ID_VOID, -1, 0));
    typeTable->defineType(new Type(true, false, "bool", TYPE_ID_BOOL, -1, 1));
    typeTable->defineType(new Type(true, false, "int", TYPE_ID_INT, -1, 4));
    typeTable->defineType(new Type(true, false, "float", TYPE_ID_FLOAT, -1, 4));
}

/**
 * The function initializes built-in functions in C++ by defining a print function for integers.
 */
void InitBuiltinFunctions() {
    auto typeTable = globalScope->types();
    auto funcTable = globalScope->funcs();
    auto intType = typeTable->lookupType(TYPE_ID_INT);
    char *printIntName = nullptr;
    STRDUP("print", &printIntName)
    auto printIntFunc = new Function(globalScope, printIntName, TYPE_ID_VOID);
    printIntFunc->addParameter(FunctionParameter{.index = 0, .name = nullptr, .type = intType});
    funcTable->defineFunc(printIntFunc);
}

/**
 * The function initializes the semantic context by creating tables for function, type, and name
 * information, initializing built-in types and functions, and setting the current scope to the
 * global scope.
 *
 * @return an integer value of 0.
 */
int Semantics::InitSemanticContext() {
    globalFunctionTable = new GlobalFunctionTable();
    auto moduleTypeTable = new ScopeTypeTable();
    auto moduleNameTable = new ScopeNameTable();
    globalScope = new Scope(nullptr, moduleNameTable, moduleTypeTable, "global");
    InitBuiltinTypes();
    InitBuiltinFunctions();
    currentScope = globalScope;
    return 0;
}

/**
 * The function destroys the semantic context by deleting various objects and clearing a container.
 *
 * @return an integer value of 0.
 */
int Semantics::DestroySemanticContext() {
    delete globalScope;
    delete globalFunctionTable;
    delete GetProgramTree();
    userTypes.clear();
    SetProgramTree(nullptr);
    return 0;
}

/**
 * The function "ReportSemanticWarning" prints a warning message to the standard error stream.
 *
 * @param message The parameter "message" is a string that represents the warning message that you
 * want to report.
 */
void Semantics::ReportSemanticWarning(std::string message) {
    std::cerr << "\033[35mWarning: \033[0m" << message << std::endl;
}

/**
 * The function "ReportSemanticError" prints an error message to the standard error stream and exits
 * the program.
 *
 * @param message The "message" parameter is a string that represents the error message that you
 * want to display when a semantic error occurs.
 */
void Semantics::ReportSemanticError(std::string message) {
    std::cerr << "\033[91mError: \033[0m" << message << std::endl;
    exit(1);
}
