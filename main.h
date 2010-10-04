// main.h  --  Header File
//
// KPL Compiler
//
// Copyright 2002-2007, Harry H. Porter III
//
// This file may be freely copied, modified and compiled, on the sole
// condition that if you modify it...
//   (1) Your name and the date of modification is added to this comment
//       under "Modifications by", and
//   (2) Your name and the date of modification is added to the printHelp()
//       routine in file "main.cc" under "Modifications by".
//
// Original Author:
//   06/15/02 - Harry H. Porter III
//
// Modifcations by:
//   03/15/06 - Harry H. Porter III
//   09/27/10 - Add inttypes to allow correct casting of pointers
//




#include <inttypes.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <float.h>
#include <math.h>
#include <ctype.h>
#include <stdlib.h>

#include "ast.h"

/* SWAP_BYTES (int)  -->  int
**
** This macro is used to swap the bytes in a 32-bit int from Big Endian order
** to Little Endian order, or vice-versa.
**
** For example:
**     i = SWAP_BYTES (i);
**
** This program was originally written for a Big Endian architecture so swapping
** bytes was never necessary.  When compiled on a Big Endian computer, this macro
** is a no-op; when compiled on a Little Endian machine, it will swap the bytes.
**
*/
#ifdef BLITZ_HOST_IS_LITTLE_ENDIAN
#define SWAP_BYTES(x) \
    ((int)((((int)(x) & 0xff000000) >> 24) | \
           (((int)(x) & 0x00ff0000) >>  8) | \
           (((int)(x) & 0x0000ff00) <<  8) | \
           (((int)(x) & 0x000000ff) << 24)))
#else
#define SWAP_BYTES(x) (x)
#endif

class IR;

//----------  Routines in main.cc  ----------

void initializeConstants ();
void printAllData ();
void dump (const char * message);
void testLexer ();
void printToken (Token token);
void programLogicError (const char * msg);
void terminateCompiler ();
void fatalError (const char * msg);
void syntaxError (const char * msg);
void syntaxErrorWithToken (Token token, const char * msg);
void error (AstNode * node, const char * msg);
void error2 (AstNode * node, const char * msg);
void doMessage (Token token, const char * prefix, const char * msg);
void errorWithType (const char * msg, Type * type);
void checkTokenSkipping (int count);
void processCommandLine (int argc, char ** argv);
void printHelp ();
void checkHostCompatibility ();
char * appendStrings (const char * str1, const char * str2, const char * str3);
void divide (int a, int b);
int truncateToInt (double d);


//----------  String Table  ----------

/* class String { */
/*   public: */
/*     String       * next;           // Link list of Strings */
/*     int          type;             // E.g., ID or BOOL, BREAK, ... */
/*     int          primitiveSymbol;  // 0=not a primitive; e.g., PLUS, DOUBLE_TO_INT, ... */
/*     int          length;           // The length and characters */
/*     char         chars []; */
/* }; */

#define MAX_STR_LEN 2000           // Strings, IDs are limited to 2000 chars.
#define STRING_TABLE_HASH_SIZE 211 // Size of hash table for string table.
extern String * stringTableIndex [STRING_TABLE_HASH_SIZE];



/* //----------  Token  ---------- */

/* union TokenValue { */
/*   String * svalue; */
/*   int      ivalue; */
/*   double   rvalue; */
/* }; */

/* struct Token { */
/*   int        type; */
/*   TokenValue value; */
/*   int        tokenPos; */
/* //  char *     fileName; */
/* //  int        lineNumber; */
/* }; */



//----------  Global Variables  ----------

#define MAX_INPUT_FILES 255            /* Do not change; related to 8 bits */

extern TokenValue currentTokenValue;   // Used in lexer only
extern Token tokenMinusOne, token, token2, token3, token4, token5;
extern int currentInputFileIndex;      // These describe the current position in the file
extern int currentLineOfToken;         // .
extern int currentCharPosOfToken;      // .
extern int posOfNextToken;             // Position of the next token
extern int eofCount;                   // Used to check for looping on EOF
extern char * inputFileNames [MAX_INPUT_FILES+1];    // Array of ptrs to file names
extern int errorsDetected;             // Count of errors detected so far
extern int tokenPosOfLastError;        // Used to suppress extraneous syntax errors
extern int hashVal;                    // The running hash code for this file
extern int hashCount;                  // Used in comuting the hashVal
extern char * commandPackageName;      // The package name, NULL = missing
extern char * commandDirectoryName;    // The search directory name, NULL = missing
extern char * headerFileName;          // The header file name, NULL = missing
extern char * codeFileName;            // The code file name, NULL = missing
extern char * outputFileName;          // The .s filename, NULL = missing
extern FILE * inputFile;               // The input file, e.g., stdin
extern FILE * outputFile;              // The output file, e.g., stdout
extern int commandOptionS;             // True: print the symbol table
extern int commandOptionP;             // True: pretty-print the AST
extern int commandOptionAST;           // True: print the full AST
extern int commandOptionTestLexer;     // True: stop after lexer & print tokens
extern int commandOptionTestParser;    // True: stop after parser & print AST
extern int safe;                       // True: only allow safe constructs
extern Header * headerList;            // List of all headers
extern Header * headerListLast;        // .
extern Mapping<String, Header> *       // Strings --> Headers
  headerMapping;                       // .
extern Code * code;                    // The thing being compiled
extern Header * mainHeader;            // The corresponding header
extern Interface * tempInterList;      // Used in topoProcessInterfaces
extern ClassDef * tempClassList;       // Used in topoProcessClasses
extern int changed;                    // Used in assignOffsets
extern int quo;                        // Used in integer division
extern int rem;                        // .
extern ClassDef * currentClass;        // The class we are currently processing
extern Header * currentHeader;         // The header we are currently processing
extern int recursionCounter;           // Used to detect recursive types
extern IntConst * memoryStart;         // Used to compute memory usage by compiler
extern IR * firstInstruction;          // List of IR instructions
extern IR * lastInstruction;           // .
extern int maxArgBytesSoFar;           // Used in setting fun/meth->maxArgBytes
extern DoubleConst * floatList;        // Used during code gen
extern StringConst * stringList;       // Used during code gen
extern MethOrFunction * currentFunOrMeth;  // Used during code gen
extern Offset * firstDispatchOffset;   // Ptr to linked list: 4,8,12,16,...



#define BUFF_LEN 300
extern char buffer [BUFF_LEN];     // Misc. use, e.g., "_Person__Constructor"

// During parsing errors, tokens may be skipped in an attempt to recover
// and continue parsing.  Do we print a message saying that tokens were
// skipped?  This constant determines the threshhold for printing that msg.
#define TOKEN_SKIP_COUNT 1

// The compiler will automatically terminate after this many error messages
#define MAX_NUMBER_OF_ERRORS 100



//----------  Lexical Routines  ----------

const char * initScanner (const char * filename);
void scan ();
int getToken (void);
void lexError (const char *msg);
void initKeywords ();
String * lookupAndAdd (const char * givenStr, int newType);
String * lookupAndAdd2 (const char * givenStr, int length, int newType);
int bytesEqual (const char * p, const char * q, int length);
void printStringTable ();
void printString (FILE * file, String *);
void printChar (FILE * file, int c);
const char * symbolName (int i);
void printSymbol (int sym);
int hexCharToInt (char ch);
char intToHexChar (int c);
double raisedToThePower (int i);
int isEscape (char c);
int scanEscape ();
int isOpChar (char);
void addToHash (int i);
void addTokenToHash (Token tok);
int createTokenPos ();
int extractLineNumber (Token token);
int extractCharPos (Token token);
char * extractFilename (Token token);
void incrLineNumber ();
int getNextChar ();
void unGetChar (char ch);
void addToInputFilenames (const char * filename);


#include "ast.h"



//---------- Global Data  ----------

extern String * stringUnaryBang;
extern String * stringUnaryStar;
extern String * stringUnaryAmp;
extern String * stringUnaryMinus;

extern String * stringPlus;
extern String * stringMinus;
extern String * stringStar;
extern String * stringSlash;
extern String * stringPercent;
extern String * stringBar;
extern String * stringCaret;
extern String * stringAmp;
extern String * stringBarBar;
extern String * stringAmpAmp;
extern String * stringEqualEqual;
extern String * stringNotEqual;
extern String * stringLess;
extern String * stringLessEqual;
extern String * stringGreater;
extern String * stringGreaterEqual;
extern String * stringLessLess;
extern String * stringGreaterGreater;
extern String * stringGreaterGreaterGreater;
extern String * stringIntToDouble;
extern String * stringDoubleToInt;
extern String * stringIntToChar;
extern String * stringCharToInt;
extern String * stringPtrToBool;
extern String * stringPosInf;
extern String * stringNegInf;
extern String * stringNegZero;
extern String * stringIIsZero;
extern String * stringINotZero;
extern String * stringObject;
extern String * stringMain;

extern CharType * basicCharType;
extern IntType  * basicIntType;
extern DoubleType * basicDoubleType;
extern BoolType * basicBoolType;
extern VoidType * basicVoidType;
extern TypeOfNullType * basicTypeOfNullType;
extern AnyType * basicAnyType;
extern PtrType * basicCharArrayPtrType;
extern PtrType * basicVoidPtrType;
extern PtrType * basicAnyPtrType;

extern IntConst * constantIntZero;
extern IntConst * constantIntOne;
extern IntConst * constantIntMinusOne;
extern BoolConst * constantFalse;
extern BoolConst * constantTrue;

/*****
extern DoubleExpr * constantDoubleZero;
extern DoubleExpr * constantDoubleOne;
extern CharExpr * constantCharNull;
extern NullExpr * constantNull;
extern String * stringGenericDestructor;
extern String * stringThis;


*****/

//----------  Parsing Routines  ----------

int nextTokenIsID (const char * msg);
String * mustHaveID (const char * msg);
void mustHave (int tok, const char * msg);
int mustHaveOrScanUntilInScanSet (int tok, const char * msg);
void scanToFollowType ();
int inScanSet ();
int inFirstStmt ();
int inFollowStmt ();
int inFirstExpr (Token tok);
int inFollowExpr ();
int inFirstType (Token tok);
int inFollowType ();
int inHeaderSet ();
int inCodeSet ();
Statement * appendStmtLists (Statement * stmtList1,
                           Statement * stmtList2);
Statement * parseStmtList (Statement * enclosingStmtForBreak,
                           Statement * enclosingStmtForContinue,
                           MethOrFunction * enclosingMethOrFunction,
                           int endsWithSemiColon);
Statement * parseStmt (Statement * enclosingStmtForBreak,
                       Statement * enclosingStmtForContinue,
                       MethOrFunction * enclosingMethOrFunction);
int gotVoidType (Type * type);
Parameter * parseParmList ();
Function * parseFunction (int expectingID);
FunctionProto * parseFunctionProtos ();
Method * parseMethod ();
MethodProto * parseMethodProto ();
Header * parseHeader ();
Code * parseCode ();
Behavior * parseBehavior ();
Uses * parseUses ();
Renaming * parseRenamings ();
String * pickupKeywordSelector ();
int colonCount (char * str);
Interface * parseInterface ();
TypeParm * parseTypeParms ();
ClassDef * parseClass();

Type * parseType (const char * errorMsg);
RecordField * parseRecordFieldList ();
FunctionType * parseFunctionType ();
NamedType * parseNamedType (const char * errorMsg);
ArrayType * parseArrayType (Token tokenForPos);
Expression * parseExpr (const char * errorMsg);
Expression * parseExpr0 (const char * errorMsg);
Expression * parseExpr1 (const char * errorMsg);
Expression * parseExpr2 (const char * errorMsg);
Expression * parseExpr3 (const char * errorMsg);
// Expression * parseExpr4 (const char * errorMsg);
Expression * parseExpr5 (const char * errorMsg);
Expression * parseExpr6 (const char * errorMsg);
Expression * parseExpr7 (const char * errorMsg);
Expression * parseExpr8 (const char * errorMsg);
Expression * parseExpr9 (const char * errorMsg);
Expression * parseExpr10 (const char * errorMsg);
Expression * parseExpr11 (const char * errorMsg);
Expression * parseExpr12 (const char * errorMsg);
Expression * parseExpr13 (const char * errorMsg);
Expression * parseExpr14 (const char * errorMsg);
Expression * parseExpr15 (const char * errorMsg);
Expression * parseExpr16 (const char * errorMsg);
Expression * parseExpr17 (const char * errorMsg);
ArrayAccess * parseArrayAccess (Token tokenForPos, Expression * soFar);
Argument * parseArgList ();
Constructor * parseConstructor ();
Local * parseLocalVarDecls ();
Global * parseGlobalVarDecls ();
ClassField * parseClassFields ();
ErrorDecl * parseErrorDecls ();
ConstDecl * parseConstDecls ();
ConstDecl * parseEnums ();
TypeDef * parseTypeDefs ();




//----------  Routines in check.cc  ----------

void topoProcessAllPackages ();
void topoProcessOnePackage (Header * hdr);
void buildPackageMapping (Header * hdr);
void addToPackage (Header * hdr, String * id, AstNode * item);
void topoProcessInterfaces (Header * hdr);
void topoProcessOneInterface (Interface * inter);
void topoProcessClasses (Header * hdr);
void topoProcessOneClass (ClassDef * cl);
void bindTypeNames (AstNode * node,
                    Mapping <String, AstNode> * typeParmMap);
void checkTypeDefCircularity (Header * hdr);
Type * findCoreType (Type * typ);
Type * copyTypeWithSubst (Type * type, Mapping<TypeParm,Type> * subst);
TypeArg * copyArgListWithSubst (TypeArg * argList,
                                Mapping<TypeParm,Type> * subst);
RecordField * copyRecordFieldsWithSubst (RecordField * fieldList,
                                         Mapping <TypeParm, Type> * subst);
void inheritFields (Header * hdr);
void inheritMethodProtos (Header * hdr);
String * addSuperTo (String * str);
void inheritMessages (Header * hdr);
//----------
void bindVarNames (AstNode * node,
                   Mapping <String, AstNode> * varMap);

void setFourByteRestricted (Type * t);
//----------
void assignOffsetsAndEvalExprs (Header * hdr);
void assignOffsets2 (AstNode * node, int wantPrinting);
void assignOffsetsInClass (ClassDef * cl, int wantPrinting);
void assignOffsetsInRecord (RecordType * recordType, int wantPrinting);
int assignOffsetsInParmList (Parameter * parmList,
                              int wantPrinting,
                              int startingOffset);
void setParmSizeInFunctionType (FunctionType * fType);
int needsAlignment (Type * type);
AstNode * getTypeDef (Type * t);
int sizeInBytesOfWhole (Type * type, AstNode * errNode, int wantPrinting);
AstNode * evalExprsIn (AstNode * node);
int isIntConst (AstNode * node);
int isCharConst (AstNode * node);
int isBoolConst (AstNode * node);
int isDoubleConst (AstNode * node);
//----------
void testSubType (Header * hdr);
int typesEqual (Type * t1, Type * t2);
int isSubType (Type * t1, Type * t2);
int assignable (Type * t1, Type * t2);
Mapping <TypeParm, Type> * buildSubstitution (TypeParm * typeParm,
                                              TypeArg * typeArg);
Parameter * copyParmListWithSubst (Parameter * parmList,
                                   Mapping <TypeParm, Type> * subst);
//----------
void checkImplements (Header * hdr);
void checkMethodProtos (Header * hdr);
void checkProtos (MethodProto * protoSuper, MethodProto * protoSub);
void checkProtos2 (MethodProto * protoSuper, MethodProto * protoSub);
void checkProtos3 (MethodProto * proto1, MethodProto * proto2);
void checkExtends (Header * hdr);
//----------
Type * checkTypes (AstNode * hdr);
Type * checkConstructor (Constructor * constructor);
void linkDouble (DoubleConst * doubleConst);
void checkStaticData (Expression * expr, AstNode * node);
AstNode * isStaticData (Expression * expr);
void checkArgList (Argument * arg,
                   Parameter * parm,
                   AstNode * proto,
                   AstNode * invocation);
void checkArgList2 (Argument * arg,
                   TypeArg * parm,
                   AstNode * proto,
                   AstNode * invocation);
void checkTypeInstantiation (TypeArg * typeArg,
                             TypeParm * typeParm,
                             AstNode * proto,
                             AstNode * invocation);
Type * checkForPrimitive (SendExpr * sendExpr,
                          Type * recvrType,
                          int PRIMITIVE_I_OP,
                          int PRIMITIVE_D_OP);
Type * checkForPrimitive2 (SendExpr * sendExpr,
                           Type * recvrType,
                           int PRIMITIVE_I_OP);
Type * checkForPrimitive3 (SendExpr * sendExpr,
                           Type * recvrType,
                           int PRIMITIVE_I_OP,
                           int PRIMITIVE_D_OP);
Type * checkForPrimitive4 (SendExpr * sendExpr,
                           Type * recvrType,
                           int PRIMITIVE_B_OP);
Type * checkForPrimitive5 (SendExpr * sendExpr,
                           Type * recvrType,
                           int PRIMITIVE_I_OP,
                           int PRIMITIVE_D_OP,
                           int PRIMITIVE_B_OP,
                           int PRIMITIVE_OBJ_OP);
Type * checkMessageSend (MethodProto * methodProto,
                         SendExpr * sendExpr,
                         Type * recvrType,
                         Mapping<TypeParm,Type> * subst);
Expression * checkAssignment (Expression * expr,
                              Type * expectedType,
                              const char * msg,
                              AssignStmt * assignStmt);
Expression * insertIntToDouble (Expression * expr);
Expression * insertIntIsZero (Expression * expr);
Expression * insertIntNotZero (Expression * expr);
Expression * insertCharToInt (Expression * expr);
Expression * insertPtrToBool (Expression * expr);
Expression * insertDeref (Expression * expr);
int isDeref (AstNode * p);
int isAddressOf (AstNode * p);
Expression * insertFalse (Expression * expr);
Expression * insertTrue (Expression * expr);
Expression * insertZero (Expression * expr);
int argCount (Argument * argList);
int isCharType (Type * type);
int isIntType (Type * type);
int isDoubleType (Type * type);
int isBoolType (Type * type);
int isVoidType (Type * type);
int isTypeOfNullType (Type * type);
int isAnyType (Type * type);
int isPtrType (Type * type);
int isPtrToVoidType (Type * type);
PtrType * getPtrType (Type * type);
int isRecordType (Type * type);
int isArrayType (Type * type);
ArrayType * getArrayType (Type * type);
FunctionType * getFunctionType (Type * type);
int isObjectType (Type * type);
int isExactClass (Type * type);
ClassDef * getClassDef (Type * type);
Interface * getInterface (Type * type);
Type * resolveNamedType (Type * type);
Type * resolveNamedType2 (Type * type);
void checkConcreteClass (Type * type,
                         AstNode * errorNode,
                         const char * errorMsg);
void checkConcreteClassOrInterface (Type * type,
                                    AstNode * errorNode,
                                    const char * errorMsg);
void checkConcreteType (Type * type,
                        AstNode * errorNode,
                        const char * errorMsg);
int isLValue (Expression * expr);
void updateMaxArgBytes (int i);
//----------
int fallsThru (AstNode * node);
int fallsThruStmtList (Statement * stmtList);
//----------
void assignLocalOffsets (Header * hdr);
void assignOffsetsInMethOrFunction (MethOrFunction * methOrFunction);
int assignOffsetToLocal (int lastOffset,
                         VarDecl * varDecl,
                         int commandOptionS,
                         int doingBytes);
void printParmOffsets (Parameter * parmList);
//----------
void assignDispatchTableOffsets (Header * hdr);
int isThisOffsetOK (String * sel,
                    Offset * offset,
                    Mapping<Abstract,Abstract> * setToCheck);
void assignOffsetToSelector (String * sel,
                             Offset * offset,
                             Mapping <Abstract, Abstract> * relatedSet,
                             Mapping <Abstract, Abstract> * affectedSet);
void addAllSuperAbstracts (Abstract * abs,
                           Header * hdr,
                           Mapping <Abstract, Abstract> * supersInThisPackage,
                           Mapping <Abstract, Abstract> * supersInOtherPackages);
Offset * nextDispatchOffset (Offset * off);
void stackPush (AbstractStack * st, Abstract * elt);
Abstract * stackPop (AbstractStack * st);
int stackEmpty (AbstractStack * st);
AbstractStack * newStack ();




//----------  Routines in ir.cc  ----------

#include "ir.h"

int within16Bits (int);
void getIntoReg4 (AstNode * src, const char * reg);
void getIntoReg1 (AstNode * src, const char * reg1, const char * reg2);
void getIntoReg8 (AstNode * src, const char * freg1, const char * reg2);
void getAddrOfVarIntoReg (AstNode * src, const char * reg1);
void storeFromReg4 (VarDecl * dest, const char * reg1, const char * reg2);
void storeFromReg1 (VarDecl * dest, const char * reg1, const char * reg2);
void storeFromReg8 (VarDecl * dest, const char * freg1, const char * reg2);
void printIR ();
void printANode (AstNode * node);



//----------  Routines in gen.cc  ----------

void generateIR ();
void genInterface (Interface * inter);
void genClass (ClassDef * cl);
void addAllSupers (Abstract * abs, Mapping<Abstract, Abstract > * setOfSupers);
void genMethOrFunction (MethOrFunction * methOrFunction);
void genVarDescriptor (VarDecl * varDecl);
void genStaticData (Expression * expr);
FieldInit * sortFieldInits (FieldInit * f);
void genStmts (Statement * stmtList);
void genIfStmt (IfStmt * stmt);
void genAssignStmt (AssignStmt * stmt);
void genCallStmt (CallStmt * stmt);
void genSendStmt (SendStmt * stmt);
void genCallExpr (VarDecl * target,
                  CallExpr * callExpr,
                  char * trueLabel,
                  char * falseLabel);
void genSendExpr (VarDecl * target,
                  SendExpr * sendExpr,
                  char * trueLabel,
                  char * falseLabel);
void genWhileStmt (WhileStmt * stmt);
void genDoStmt (DoStmt * stmt);
void genBreakStmt (BreakStmt * stmt);
void genContinueStmt (ContinueStmt * stmt);
void genReturnStmt (ReturnStmt * stmt);
void genForStmt (ForStmt * stmt);
void genSwitchStmt (SwitchStmt * stmt);
int roundUpToPrime (int i);
int hashForSwitchStmt (int i, int hashMax);
void genTryStmt (TryStmt * stmt);
void genThrowStmt (ThrowStmt * stmt);
void genFreeStmt (FreeStmt * stmt);
void genDebugStmt (DebugStmt * stmt);
AstNode * genExpr (Expression * p, int targetSize);
void genExprInto (VarDecl * target,
                  Expression * node,
                  char * trueLabel,
                  char * falseLabel);
VarDecl * genAddressOf (Expression * node);
VarDecl * genConstructor (Constructor * constructor, VarDecl * target);
char * newLabel ();
char * newName (const char * str);
char * sanitizedPackageName (String * packageName);
char * newMethodName (char * sanitizedClassName, int i);
void genLineNumber (AstNode * node, const char * stmtCode);
Local * newTemp (int size);

//qqqqq





/***************


//  //----------  Routines in check.cc  ----------
//  
//  void addGlobalDecls ();
//  void addFunctions ();
//  void checkSuperclassCycles ();
//  void bindAllTypeNames ();
//  void bindTypeNamesIn (AstNode * node,
//                        Mapping<String,TypeParm> * typeParmMap);
//  void bindTypeNamesInStmtList (Statement * stmtList,
//                                Mapping<String,TypeParm> * typeParmMap);
//  void topoProcessAllClasses ();
//  void topoProcess (Class * cl);
//  void buildFieldMapping ();
//  void buildMethodMapping ();
//  Header * copyHeaderWithSubst (Header * subst,
//                                Mapping<TypeParm,Type> * subst);
//  void checkConstructor (Method * meth, Class * cl);
//  void bindAllNames ();
//  void bindNamesIn (AstNode * node,
//                    Mapping<String,AstNode> * varMap,
//                    Mapping<String,AstNode> * methMap,
//                    Class * currentClass,
//                    AstNode * enclosingMethodOrFunction);
//  void bindNamesInStmtList (Statement * stmtList,
//                            Mapping<String,AstNode> * varMap,
//                            Mapping<String,AstNode> * methMap,
//                            Class * currentClass,
//                            AstNode * enclosingMethodOrFunction);
//  
//  int typesAreEqual (Type * type1, Type * type2);
//  int subTypeOf (Type * type1,
//                 Mapping<TypeParm,Type> * subst1,
//                 Type * type2,
//                 Mapping<TypeParm,Type> * subst2);
//  int subClassOf (Type * type1,
//                  Mapping<TypeParm,Type> * subst1,
//                  Type * type2,
//                  Mapping<TypeParm,Type> * subst2);
//  int typeArgListsEqual (TypeArg * typeArgList1,
//                         TypeArg * typeArgList2);
//  Type * copyTypeWithSubst (Type * type,
//                            Mapping<TypeParm,Type> * subst);
//  TypeArg * copyArgListWithSubst (TypeArg * argList,
//                                  Mapping<TypeParm,Type> * subst);
//  Parameter * copyParmListWithSubst (Parameter * parmList,
//                                     Mapping<TypeParm,Type> * subst);
//  
//  void evaluateExpressions ();
//  Expression * evalExprsIn (AstNode * node);
//  void evalExprsInStmtList (Statement * stmtList);
//  Expression * evalExprsInUnaryExpr (AstNode * node);
//  Expression * evalExprsInBinaryExpr (AstNode * node);
//  BinaryExpr * newMultiplyExpr (Expression * expr1, Expression * expr2);
//  IntExpr * newIntConst (int value, AstNode * otherNode);
//  DoubleExpr * newDoubleConst (double value, AstNode * otherNode);
//  BoolExpr * newBoolConst (int value, AstNode * otherNode);
//  CharExpr * newCharConst (int value, AstNode * otherNode);
//  
//  void printDispatchTable (Class * cl);
//  
//  #define FIRST_DISPATCH_TABLE_OFFSET 0
//  // The following constant must match a similar constant in the emulator.
//  #define MAX_NUMBER_OF_DISPATCH_TABLE_ENTRIES 100
//  Statement * createInitializers (Decl * declList);
//  Statement * createDestructors (Decl * declList);
//  void addConstructorDestructorCalls (Method * meth);
//  void setFourByteRestriction ();
//  void setRestrictionOnType (Type * type);
//  void assignOffsetsAndEvalExprs ();
//  int assignOffsets2 (int wantPrinting);
//  int needsAlignment (Type * type);
//  int sizeInBytesOfWhole (Type * type);
//  void checkAllTypes ();
//  Type* checkTypesIn (AstNode * node,
//                     Class * currentClass,
//                     AstNode * enclosingMethodOrFunction);
//  void checkTypesInStmtList (Statement * stmtList,
//                             Class * currentClass,
//                             AstNode * enclosingMethodOrFunction);
//  Type * checkTypesInUnaryExpr (AstNode * node,
//                                Class * currentClass,
//                                AstNode * enclosingMethodOrFunction);
//  Type * checkTypesInBinaryExpr (AstNode * node,
//                                 Class * currentClass,
//                                 AstNode * enclosingMethodOrFunction);
//  void rewriteAssignmentEquals (BinaryExpr * binExpr,
//                                int mode,
//                                AstNode * methodOrFunction);
//  int isBool (Type * type);
//  int isInt (Type * type);
//  int isDouble (Type * type);
//  int isChar (Type * type);
//  int isVoid (Type * type);
//  int isNull (Type * type);
//  int isFunction (Type * type);
//  int isArrayPtrType (Type * type);
//  int isVoidPointer (Type * type);
//  int isClassType (Type * type);
//  int isVoidable (Expression * expr);
//  int isLValue (Expression * expr);
//  String * newName (char * str);
//  Decl * newPtrTemp (AstNode * methodOrFunction);
//  Decl * newIntTemp (AstNode * methodOrFunction);
//  Decl * newCharTemp (AstNode * methodOrFunction);
//  Decl * newDoubleTemp (AstNode * methodOrFunction);
//  Decl * newTempWithType (AstNode * methodOrFunction, Type * type);
//  int implicitCastToFrom (Type * toType, Type* fromType);
//  Expression * insertImplicitCast (Type * toType,
//                                   Type* fromType,
//                                   Expression * expr);
//  int explicitCastToFrom (Type * toType, Type* fromType);
//  Expression * insertExplicitCast (Type * toType,
//                                   Type* fromType,
//                                   Expression * expr);
//  UnaryExpr * insertUnary (Expression * expr, int op);
//  void setModeAndLength (Expression * expr, Type * type);
//  TypeArg * createTypeArgListFromTemplateParmList (TypeParm * typeParmList);
//  Type * checkTypesInNew (AstNode * node,
//                          Class * currentCl,
//                          AstNode * enclosingMethOrFun);
//  Type * checkTypesInCall (AstNode * node,
//                           Class * currentCl,
//                           AstNode * enclosingMethOrFun);
//  void checkFallsThru ();
//  int fallsThru (AstNode * node);
//  int fallsThruStmtList (Statement * stmtList);
//  
//  
//  
//  //----------  Data related to IR code generation  ----------
//  
//  
//  
//  // These are the op-code for the IR instructions.
//  
//  typedef enum iropcode {
//    IRcomment,        // result is a string
//    IRlabel,          // result is a string such as "Label_43"
//    IRgoto,           // result is a string such as "Label_43"
//    IRbyte1,          // result is a string. Arg1 is an integer
//    IRbyte2,          // result is not used. Arg1 is an integer
//    IRword1,          // result is a string. Arg1 is an integer
//    IRword2,          // result is a string. Arg1 is a string
//    IRword3,          // result is not used. Arg1 is a string
//    IRdouble,         // result is a string. Arg1 points to a double
//    IRimport,         // result is a string
//    IRexport,         // result is a string
//    IRalign,          // no args
//    IRdata,           // no args
//    IRtext,           // no args
//    IRskip,           // result is a string, Arg1 is an integer
//    IRskip2,          // result is not used, Arg1 is an integer
//    IRascii,          // result is a string label; Arg1 points to a "String"
//    IRassignb,        // Move arg1 to result.  Arg2 is ignored.
//    IRassignw,        // Move arg1 to result.  Arg2 is ignored.
//    IRassignf,        // Move arg1 to result.  Arg2 is ignored.
//    IRmoveBytes,      // Move *arg1 to *result.  Arg2 is is int count.
//    IRloadAddr,       // result := &arg1
//    IRloadWordFromByteIndirect,  // result := *arg1, zero-filling high 3 bytes
//    IRloadByteIndirect,  // result := *arg1
//    IRloadWordIndirect,  // result := *arg1
//    IRloadFloatIndirect, // result := *arg1
//    IRloadStringAddr, // result := addr; arg1 is StringExpr
//    IRloadLabel,      // result := value; arg1 is a label, e.g., "foo"
//    IRloadFloat,      // result is 8 byte var, arg1 is DoubleExpr
//    IRstoreb,         // *result := arg1, byte length
//    IRstorew,         // *result := arg1, word length
//    IRstoref,         // *result := arg1, float length
//    IRgotoiEQ,        // if arg1 == arg2 then goto result (integer arith)
//    IRgotoiNE,        // if arg1 != arg2 then goto result (integer arith)
//    IRgotoiLT,        // if arg1 <  arg2 then goto result (integer arith)
//    IRgotoiLE,        // if arg1 <= arg2 then goto result (integer arith)
//    IRgotoiGT,        // if arg1 >  arg2 then goto result (integer arith)
//    IRgotoiGE,        // if arg1 >= arg2 then goto result (integer arith)
//    IRgotofEQ,        // if arg1 == arg2 then goto result (float arith)
//    IRgotofNE,        // if arg1 != arg2 then goto result (float arith)
//    IRgotofLT,        // if arg1 <  arg2 then goto result (float arith)
//    IRgotofLE,        // if arg1 <= arg2 then goto result (float arith)
//    IRgotofGT,        // if arg1 >  arg2 then goto result (float arith)
//    IRgotofGE,        // if arg1 >= arg2 then goto result (float arith)
//    IRintToDouble,    // result := intToDouble (arg1) 
//    IRintToBool,      // result := intToBool (arg1)   
//    IRintToChar,      // result := intToChar (arg1); truncate to 8 bits
//    IRdoubleToInt,    // result := doubleToInt (arg1) 
//    IRcharToInt,      // result := charToInt (arg1); sign-extend
//    IRbyteToWord,     // result := arg1; result is 32 bits; zero-fill
//    IRcharToDouble,   // result := charToDouble (arg1)
//    IRcharToBool,     // result := charToBool (arg1)  
//                      // (There is no ptrToBool; we use intToBool instead)
//                      // (There is no boolToInt; we use charToInt instead)
//    IRiadd,           // result := arg1 + arg2, using integer arithmetic.
//    IRisub,           // result := arg1 - arg2, using integer arithmetic.
//    IRimul,           // result := arg1 * arg2, using integer arithmetic.
//    IRidiv,           // result := arg1 DIV arg2, using integer arithmetic.
//    IRirem,           // result := arg1 REM arg2, using integer arithmetic.
//    IRineg,           // result := - arg1, using integer arithmetic.
//  
//    IRsll,            // result := arg1 << arg2, using integer arithmetic.
//    IRsra,            // result := arg1 >> arg2, using integer arithmetic.
//  
//    IRfadd,           // result := arg1 + arg2, using floating arithmetic.
//    IRfsub,           // result := arg1 - arg2, using floating arithmetic.
//    IRfmul,           // result := arg1 * arg2, using floating arithmetic.
//    IRfdiv,           // result := arg1 / arg2, using floating arithmetic.
//    IRfneg,           // result := - arg1, using floating arithmetic.
//  
//    IRand,            // result := arg1 AND arg2, bitwise, word length.
//    IRor,             // result := arg1 OR arg2, bitwise, word length.
//    IRxor,            // result := arg1 XOR arg2, bitwise, word length.
//  
//  
//  // temp...
//    IRreturnExpr,     // Return with result as the returned value, arg1=lexLev.
//    IRreturnVoid,     // Return with no value, arg1=lexLev.
//  
//  // The following are leftover from PCAT...
//    IRitof,           // result := intToFloat(arg1).
//    IRcall,           // Invokes a routine.  Result points to PROC_DECL. 
//    IRparam,          // arg2 contains a value, arg1 is the arg number.
//    IRresultTo,       // result :=  value returned from function.
//    IRmainEntry,      // Result points to a BODY
//    IRmainExit,       // No args
//    IRprocEntry,      // result points to the PROC_DECL, arg1 is lex. level.
//    IRformal,         // result points to a FORMAL; arg1 is the position.
//    IRallocate,       // result := allocate (arg1).
//    IRreadInt,        // readInt (result)
//    IRreadFloat,      // readFloat (result)
//    IRwriteInt,       // writeInt (result)
//    IRwriteFloat,     // writeFloat (result)
//    IRwriteString,    // writeString (result)
//    IRwriteBoolean,   // writeBoolean (result)
//    IRwriteNewline    // writeNewline ()
//  } IRopcode;
//  
//  
//  
//  // Each IR instruction is called a "Quad" and is represented by
//  // one of these structures.
//  
//  typedef struct irquad Quad;
//  
//  struct irquad {
//    IRopcode	op;
//    AstNode *	result;
//    AstNode *	arg1;
//    AstNode *	arg2;
//    Quad *	next;
//  };
//  
//  
//  
//  // There is a linked-list of all the IR quads.
//  
//  extern Quad * firstQuad;
//  extern Quad * lastQuad;
//  
//  
//  
//  extern AstNode * currentMethOrFun;
//  extern Class * currentClass;
//  extern StringExpr * stringList;
//  extern DoubleExpr * floatList;
//  
//  
//  
//  // Constants relating to assigning offsets.
//  
//  // #define INITIAL_VARIABLE_OFFSET  -4
//  // #define VARIABLE_OFFSET_INCR  -4
//  // #define INITIAL_FORMAL_OFFSET +68
//  // #define FORMAL_OFFSET_INCR +4
//  // #define REGISTER_SAVE_AREA_SIZE +64
//  // #define DISPLAY_REG_SAVE_AREA_OFFSET +64
//  
//  
//  
//  //----------  Routines in generate.cc  ----------
//  
//  char * newLabel (char * str);
//  Decl * newTemp ();
//  void genComment (char * commentString);
//  void gen (IRopcode op, AstNode * result, AstNode * arg1, AstNode * arg2);
//  void printIR ();
//  void printOffsets (AstNode * p);
//  void generateIR ();
//  void genFunction (Function * fun);
//  void genMethod (Method * meth);
//  void genStmts (Statement * stmtList);
//  void genIfStmt (IfStmt * t);
//  void genWhileStmt (WhileStmt * t);
//  void genDoStmt (DoStmt * t);
//  void genForStmt (ForStmt * t);
//  void genSwitchStmt (SwitchStmt * t);
//  void genBreakContinueStmt (BreakContinueStmt * t);
//  void genReturnStmt (ReturnStmt * t);
//  void genExprStmt (ExprStmt * t);
//  AstNode * genExpr (AstNode * node,
//                     char * trueLabel,
//                     char * falseLabel,
//                     AstNode * dest);
//  AstNode * genUnaryExpr (UnaryExpr * node,
//                          char * trueLabel,
//                          char * falseLabel,
//                          AstNode * dest);
//  AstNode * genBinaryExpr (BinaryExpr * node,
//                           char * trueLabel,
//                           char * falseLabel,
//                           AstNode * dest);
//  AstNode * genLValue (AstNode * node, AstNode * dest);
//  Type * typeOfVariable (AstNode * declOrParameter);
//  int modeOf (Type * type);
//  void printDoubleValue (double d);
//  void printIntInHex (int i);
//  void printByte (int c);
//  void genCompareBytes (AstNode * x,
//                        AstNode * y,
//                        int count,
//                        char * trueLabel,
//                        char * falseLabel);
//  int isType (AstNode * type);
//  
//  
//  
//  
//  //----------  Routines in misc .c files.  ----------
//  
//  // void peephole ();                        // peephole.c
//  // void emitAll ();                         // emit.c

***************/
