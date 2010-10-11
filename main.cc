// main.cc  --  Driver routine for the compiler; basic control routines
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
//       routine under "Modifications by".
//
// Original Author:
//   06/15/02 - Harry H. Porter III
//
// Modifcations by:
//   03/15/06 - Harry H. Porter III
//

#include <signal.h>  

#include "main.

//---------------  Global Variables  ---------------

char * commandPackageName = NULL;      // The package name, NULL = missing
char * headerFileName = NULL;          // The header file name, NULL = missing
char * codeFileName = NULL;            // The code file name, NULL = missing
int commandOptionS = 0;                // True: print the symbol table
int commandOptionP = 0;                // True: pretty-print the AST
int commandOptionAST = 0;              // True: print the full AST
int commandOptionTestLexer = 0;        // True: stop after lexer & print tokens
int commandOptionTestParser = 0;       // True: stop after parser & print AST
int safe = 1;                          // True: only allow safe constructs
Header * headerList = NULL;            // List of all headers
Header * headerListLast = NULL;        // .
Mapping <String, Header> *             // Strings --> Headers
  headerMapping = NULL;                // .
Code * code = NULL;                    // The thing being compiled
Header * mainHeader = NULL;            // The corresponding header
Interface * tempInterList = NULL;      // Used in topoProcessInterfaces
ClassDef * tempClassList = NULL;       // Used in topoProcessClasses
int changed = 0;                       // Used in assignOffsets
int quo = 0;                           // Used in integer division
int rem = 0;                           // .
ClassDef * currentClass = NULL;        // The class we are currently processing
Header * currentHeader = NULL;         // The header we are currently processing
int recursionCounter = 0;              // Used to detect recursive types
IntConst * memoryStart;                // Used to compute memory usage by compiler

int maxArgBytesSoFar = -1;             // Used in setting fun/meth->maxArgBytes
DoubleConst * floatList = NULL;        // Used during code gen
StringConst * stringList = NULL;       // Used during code gen
MethOrFunction * currentFunOrMeth = NULL;     // Used during code gen
Offset * firstDispatchOffset = NULL;   // Ptr to linked list: 4,8,12,16,...



// // The String Table, for all Strings and IDs.
// String * stringTableIndex [STRING_TABLE_HASH_SIZE];


char buffer [BUFF_LEN];     // Misc. use, e.g., "_Person__Constructor"


// These are initialized in "initializeConstants"...
String * stringUnaryBang;
String * stringUnaryStar;
String * stringUnaryAmp;
String * stringUnaryMinus;

String * stringPlus;
String * stringMinus;
String * stringStar;
String * stringSlash;
String * stringPercent;
String * stringBar;
String * stringCaret;
String * stringAmp;
String * stringBarBar;
String * stringAmpAmp;
String * stringEqualEqual;
String * stringNotEqual;
String * stringLess;
String * stringLessEqual;
String * stringGreater;
String * stringGreaterEqual;
String * stringLessLess;
String * stringGreaterGreater;
String * stringGreaterGreaterGreater;
String * stringIntToDouble;
String * stringDoubleToInt;
String * stringIntToChar;
String * stringCharToInt;
String * stringPtrToBool;
String * stringPosInf;
String * stringNegInf;
String * stringNegZero;
String * stringIIsZero;
String * stringINotZero;
String * stringObject;
String * stringMain;

CharType * basicCharType;
IntType  * basicIntType;
DoubleType * basicDoubleType;
BoolType * basicBoolType;
VoidType * basicVoidType;
TypeOfNullType * basicTypeOfNullType;
AnyType * basicAnyType;
PtrType * basicCharArrayPtrType;
PtrType * basicVoidPtrType;
PtrType * basicAnyPtrType;

IntConst * constantIntZero;
IntConst * constantIntOne;
IntConst * constantIntMinusOne;
BoolConst * constantFalse;
BoolConst * constantTrue;


// main (argc, argv)
//
// The compiler main.
//
int main (int argc, char ** argv) {
  Header * hdr, * newHeader;
  Uses * uses;
  String * newPackName;
  AstNode * ast;
  int len;
  char * fileName;
  Type * t;
  int saveSafe;
  int wantProgress = 0;

  memoryStart = new IntConst ();
  // printf ("Starting memory address = 0x%08x\n", (int) memoryStart);

  errorsDetected = 0;
  tokenPosOfLastError = -1;  // FF=255, LLLL=65535, PP=255
  initKeywords ();
  checkHostCompatibility ();

  processCommandLine (argc, argv);

  initializeConstants ();

  // If -testLexer command line option present...
  if (commandOptionTestLexer) {
    printf("file-name\tline\tchar\ttoken-type\tother-info\n");
    printf("=========\t====\t====\t==========\t==========\n");
    if (initScanner (headerFileName) != NULL) {
      testLexer ();
    }

    terminateCompiler ();
  }

  headerMapping = new Mapping<String, Header> (15, NULL);

  // If -testParser command line option present...
  if (commandOptionTestParser) {
    if (initScanner (headerFileName)) {
      if (token.type == FUNCTION) {
        ast = parseFunction (1);  // 1=expecting ID
      } else if (token.type == METHOD) {
        ast = parseMethod ();
      } else if (token.type == HEADER) {
        ast = parseHeader ();
      } else if (token.type == CODE) {
        ast = parseCode ();
      } else if (token.type == DO) {
        scan ();
        ast = parseExpr ("Expecting test expression after 'do'");
      }

      if (commandOptionAST) {
        printAst (6, ast);
      }

      if (commandOptionP) {
        printf ("========  PRETTY PRINT  ========\n");
        pretty (ast);
        printf ("\n================================\n");
      }
    }

    terminateCompiler ();
  }

  // Make sure the user specified a package name on the command line.
  if (commandPackageName == NULL) {
    fprintf (stderr, "*****  ERROR: Missing package name on command line\n");
    errorsDetected++;
    terminateCompiler ();
  }
  if ((codeFileName == NULL) || (headerFileName == NULL)) {
    programLogicError ("File names should have been created from package name");
  }

  if (wantProgress) printf ("Parsing...\n");

  // Parse the code file...
  if (initScanner (codeFileName)) {
    code = parseCode ();
  } else {
    code = NULL;
  }

  // Parse the main header file...
  fileName = (char *)initScanner (headerFileName);
  if (fileName != NULL) {
      //mainHeader = parseHeader (commandPackageName);
    mainHeader = parseHeader ();
    headerList = mainHeader;
    headerListLast = mainHeader;
    if (mainHeader) {
//      headerMapping->enter (mainHeader->packageName, mainHeader);
      headerMapping->enter (lookupAndAdd (commandPackageName, ID), mainHeader);
    }
  } else {
    mainHeader = NULL;
    headerList = NULL;
    headerListLast = NULL;
  }

  // Run through the headerList.  For every header on it, see which
  // packages it uses.  For each, if it has not already been parsed
  // then parse it and add it to the headerMapping.
  hdr = headerList;
  while (hdr != NULL) {
    // Look at the next "hdr"...
    // Run through the list of packages this "hdr" uses...
    uses = hdr->uses;
    while (uses != NULL) {
      newPackName = uses->id;

      // If this package has not been seen before...
      if (! headerMapping->alreadyDefined (newPackName)) {

        // Figure out the name of the .h header file...
        len = strlen (newPackName->chars);
        fileName = (char *) calloc (1, len + 4);
        strcpy (fileName, newPackName->chars);
        fileName [len] = '.';
        fileName [len+1] = 'h';
        fileName [len+2] = '\0';
        fileName = (char *)initScanner (fileName);
        if (fileName != NULL) {
        
          // Parse the new package...
          //      newHeader = parseHeader (newPackName->chars);
          newHeader = parseHeader ();
          if (newHeader != NULL) {

            // Add the new package to headerMapping...
            // (NOTE: the keys will not contain any "-d" directory prefixes.)
            // printf ("ADDING TO HEADERMAPPING: filename=%s, newHeader=%s\n",
            //          newPackName->chars, newHeader->packageName->chars);
            headerMapping->enter (newPackName, newHeader);

            // Add the new package to the end of headerList...
            headerListLast->next = newHeader;
            headerListLast = newHeader;
          }
        }
      }
      uses = uses->next;
    }

    // Move to next header on the headerList
    hdr = hdr->next;
  }

  // printAllData ();
  // dump ("After parsing...");

  // Check for package-uses circularity and re-order all packages...
  if (wantProgress) printf ("Finding a topological-sort order for packages...\n");
  topoProcessAllPackages ();

  // Must stop here if errors, since one error is cyclic package-use.  From
  // here on out, we're assuming our super-packages have already been done first.
  if (errorsDetected) {
    terminateCompiler ();
  }

  saveSafe = safe;

  // Run through all headers and process each in turn...
  for (hdr=headerList; hdr!=NULL; hdr=hdr->next) {
    if (wantProgress) printf ("Processing package \"%s\"...\n", hdr->packageName->chars);
    currentHeader = hdr;

    if (currentHeader == mainHeader) {
      safe = saveSafe;
    } else {
      safe = 0;    // Allow unsafe constructs in any header we are using.
    }

    // Build the symbol tables...
    if (wantProgress) printf ("  Building symbol tables...\n");
    buildPackageMapping (hdr);

    // Topo-process the interfaces...
    if (wantProgress) printf ("  Topologically sorting interfaces...\n");
    topoProcessInterfaces (hdr);

    // Topo-process the classes...
    if (wantProgress) printf ("  Topologically sorting classes...\n");
    topoProcessClasses (hdr);

    // Bind Types...
    if (wantProgress) printf ("  Binding types...\n");
    bindTypeNames (hdr, NULL);

    // Check TypeDef circularity...
    if (wantProgress) printf ("  Checking type def circularity...\n");
    checkTypeDefCircularity (hdr);

    // Inherit Class Fields...
    if (wantProgress) printf ("  Inheriting class fields...\n");
    inheritFields (hdr);

    // Inherit MethodProtos...
    if (wantProgress) printf ("  Inheriting method prototypes...\n");
    inheritMethodProtos (hdr);

    // Inherit messages for interface "extends" hierarchy...
    if (wantProgress) printf ("  Inheriting messages in interfaces...\n");
    inheritMessages (hdr);

    // Bind variable names...
    if (wantProgress) printf ("  Binding variable names...\n");
    bindVarNames (hdr, NULL);

    // Assign offsets and evaluate expressions...
    if (wantProgress) printf ("  Assigning offsets and evaluating static expressions...\n");
    assignOffsetsAndEvalExprs (hdr);

    // The subtype test is operative after this point.
    // testSubType (hdr);
    // break;

    // Check class-interface "implements" hierarchy...
    if (wantProgress) printf ("  Checking class/interface implements...\n");
    checkImplements (hdr);

    // Check method prototypes...
    if (wantProgress) printf ("  Checking method prototypes...\n");
    checkMethodProtos (hdr);

    // Check interface extends...
    if (wantProgress) printf ("  Checking interface extends...\n");
    checkExtends (hdr);

    // printf ("=================== Before checkTypes =========================\n");
    // printAst (6, hdr);
    // printf ("===============================================================\n");

    // Check types...
    if (wantProgress) printf ("  Checking types...\n");
    t = checkTypes (hdr);

    // Evaluate expressions again; to handle anything introduced by checkTypes...
    if (wantProgress) printf ("  Evaluating static expressions...\n");
    changed = 1;
    while (changed) {
      changed = 0;
      evalExprsIn (hdr);
      // The test called "bizarre" will print this message...
      //   if (changed) {
      //     printf ("LOOKS LIKE WE NEEDED A SECOND PASS!!!\n");
      //   }
    }

    // Make sure the recursionCounter is OK...
    if (recursionCounter != 0) {
      printf ("recursionCounter = %d\n", recursionCounter);
      programLogicError ("recursionCounter not incremented and decremented equally");
    }

    // Check flow of control...
    if (wantProgress) printf ("  Checking flow of control...\n");
    fallsThru (hdr);

    // Assign Dispatch Table Offsets...
    if (wantProgress) printf ("  Assigning Dispatch Table Offsets...\n");
    assignDispatchTableOffsets (hdr);

    // Assign offsets to locals and parms in functions and closures...
    //   printf ("  Asigning offsets to local variables and parameters...\n");
    //   assignLocalOffsets (hdr);

    // printf ("\n==========  HERE IS THE PACKAGE WE JUST FINISHED  ==========\n");
    // hdr->prettyPrint (4);
    // printf ("\n============================================================\n");

  }

  safe = saveSafe;

  // Make sure the recursionCounter is OK...
  if (recursionCounter != 0) {
    printf ("recursionCounter = %d\n", recursionCounter);
    programLogicError ("recursionCounter not incremented and decremented equally");
  }

  // Stop here if errors...
  if (commandOptionAST) {     //       && errorsDetected) {
    printAllData ();
  }
  if (commandOptionP) {     //       && errorsDetected) {
    dump ("After semantic processing...");
  }
  if (errorsDetected) {
    terminateCompiler ();
  }

  // Generating IR code...
  if (wantProgress) printf ("  Generating IR code...\n");
  generateIR ();

  // Assign offsets to locals and parms in functions and closures...
  if (wantProgress) printf ("  Asigning offsets to local variables and parameters...\n");
  assignLocalOffsets (mainHeader);

  // Print the IR code...
  if (wantProgress) printf ("  Writing .s file...\n");
  printIR ();

  // Print the full AST in all detail, if requested...
  if (commandOptionAST) {
    printAllData ();
  }

  // Pretty-print the AST, if requested...
  if (commandOptionP) {
    dump ("After code generation...");
  }

  terminateCompiler ();

  return 0;
}



// initializeConstants ()
//
// This routine initializes various constants that will be used during the
// compilation. 
//
void initializeConstants () {
  ArrayType * arrayType;

  // The nodes created here will be positioned at "(null):0:"
  token.value.svalue = NULL;
  token.tokenPos = 0;

  stringUnaryBang =             lookupAndAdd ("_prefix_!", OPERATOR);
  stringUnaryStar =             lookupAndAdd ("_prefix_*", OPERATOR);
  stringUnaryAmp =              lookupAndAdd ("_prefix_&", OPERATOR);
  stringUnaryMinus =            lookupAndAdd ("_prefix_-", OPERATOR);

  stringPlus =                  lookupAndAdd ("+", OPERATOR);
  stringMinus =                 lookupAndAdd ("-", OPERATOR);
  stringStar =                  lookupAndAdd ("*", OPERATOR);
  stringSlash =                 lookupAndAdd ("/", OPERATOR);
  stringPercent =               lookupAndAdd ("%", OPERATOR);
  stringBar =                   lookupAndAdd ("|", OPERATOR);
  stringCaret =                 lookupAndAdd ("^", OPERATOR);
  stringAmp =                   lookupAndAdd ("&", OPERATOR);
  stringBarBar =                lookupAndAdd ("||", OPERATOR);
  stringAmpAmp =                lookupAndAdd ("&&", OPERATOR);
  stringEqualEqual =            lookupAndAdd ("==", OPERATOR);
  stringNotEqual =              lookupAndAdd ("!=", OPERATOR);
  stringLess =                  lookupAndAdd ("<", OPERATOR);
  stringLessEqual =             lookupAndAdd ("<=", OPERATOR);
  stringGreater =               lookupAndAdd (">", OPERATOR);
  stringGreaterEqual =          lookupAndAdd (">=", OPERATOR);
  stringLessLess =              lookupAndAdd ("<<", OPERATOR);
  stringGreaterGreater =        lookupAndAdd (">>", OPERATOR);
  stringGreaterGreaterGreater = lookupAndAdd (">>>", OPERATOR);

  stringIntToDouble =           lookupAndAdd ("intToDouble", ID);
  stringDoubleToInt =           lookupAndAdd ("doubleToInt", ID);
  stringIntToChar =             lookupAndAdd ("intToChar", ID);
  stringCharToInt =             lookupAndAdd ("charToInt", ID);
  stringPtrToBool =             lookupAndAdd ("ptrToBool", ID);
  stringPosInf =                lookupAndAdd ("posInf", ID);
  stringNegInf =                lookupAndAdd ("negInf", ID);
  stringNegZero =               lookupAndAdd ("negZero", ID);
  stringIIsZero =               lookupAndAdd ("iIsZero", ID);   // hidden function
  stringINotZero =              lookupAndAdd ("iNotZero", ID);  // hidden function
  stringObject =                lookupAndAdd ("Object", ID);
  stringMain =                  lookupAndAdd ("main", ID);

  // Initialize nodes corresponding to basic types...

  token.type = CHAR;
  basicCharType = new CharType ();

  token.type = INT;
  basicIntType = new IntType ();

  token.type = DOUBLE;
  basicDoubleType = new DoubleType ();

  token.type = BOOL;
  basicBoolType = new BoolType ();

  token.type = VOID;
  basicVoidType = new VoidType ();

  token.type = TYPE_OF_NULL;
  basicTypeOfNullType = new TypeOfNullType ();

  token.type = ANY_TYPE;
  basicAnyType = new AnyType ();

  token.type = ARRAY;                         // array [*] of char
  arrayType = new ArrayType ();
  arrayType->baseType = basicCharType;
  arrayType->sizeOfElements = 1;
  
  token.type = PTR;                           // ptr to array [*] of char
  basicCharArrayPtrType = new PtrType ();
  basicCharArrayPtrType->baseType = arrayType;

  token.type = PTR;                           // ptr to void
  basicVoidPtrType = new PtrType ();
  basicVoidPtrType->baseType = basicVoidType;

  token.type = PTR;                           // ptr to anyType
  basicAnyPtrType = new PtrType ();
  basicAnyPtrType->baseType = basicAnyType;

/*****
  printf ("==========  Basic Type Nodes  ==========\n");
  printf ("basicCharType = ");          pretty (basicCharType);
  printf ("basicIntType = ");           pretty (basicIntType);
  printf ("basicDoubleType = ");        pretty (basicDoubleType);
  printf ("basicBoolType = ");          pretty (basicBoolType);
  printf ("basicVoidType = ");          pretty (basicVoidType);
  printf ("basicTypeOfNullType = ");    pretty (basicTypeOfNullType);
  printf ("basicCharArrayPtrType = ");  pretty (basicCharArrayPtrType);
  printf ("basicVoidPtrType = ");       pretty (basicVoidPtrType);
  printf ("basicAnyType = ");           pretty (basicAnyType);
  printf ("========================================\n");
  error (basicVoidPtrType, "Testing...");
*****/

  // Some functions and messages are "primitives".  Examples include "+" and
  // "doubleToInt".  Mark these symbols so that they can be easily tested later.
  stringUnaryBang->primitiveSymbol  = UNARY_BANG;
  stringUnaryStar->primitiveSymbol  = UNARY_STAR;
  stringUnaryAmp->primitiveSymbol   = UNARY_AMP;
  stringUnaryMinus->primitiveSymbol = UNARY_MINUS;

  stringPlus->primitiveSymbol = PLUS;
  stringMinus->primitiveSymbol = MINUS;
  stringStar->primitiveSymbol = STAR;
  stringSlash->primitiveSymbol = SLASH;
  stringPercent->primitiveSymbol = PERCENT;
  stringBar->primitiveSymbol = BAR;
  stringCaret->primitiveSymbol = CARET;
  stringAmp->primitiveSymbol = AMP;
  stringBarBar->primitiveSymbol = BAR_BAR;
  stringAmpAmp->primitiveSymbol = AMP_AMP;
  stringEqualEqual->primitiveSymbol = EQUAL_EQUAL;
  stringNotEqual->primitiveSymbol = NOT_EQUAL;
  stringLess->primitiveSymbol = LESS;
  stringLessEqual->primitiveSymbol = LESS_EQUAL;
  stringGreater->primitiveSymbol = GREATER;
  stringGreaterEqual->primitiveSymbol = GREATER_EQUAL;
  stringLessLess->primitiveSymbol = LESS_LESS;
  stringGreaterGreater->primitiveSymbol = GREATER_GREATER;
  stringGreaterGreaterGreater->primitiveSymbol = GREATER_GREATER_GREATER;
  stringIntToDouble->primitiveSymbol = INT_TO_DOUBLE;
  stringDoubleToInt->primitiveSymbol = DOUBLE_TO_INT;
  stringIntToChar->primitiveSymbol = INT_TO_CHAR;
  stringCharToInt->primitiveSymbol = CHAR_TO_INT;
  stringPtrToBool->primitiveSymbol = PTR_TO_BOOL;
  stringPosInf->primitiveSymbol = POS_INF;
  stringNegInf->primitiveSymbol = NEG_INF;
  stringNegZero->primitiveSymbol = NEG_ZERO;
  stringIIsZero->primitiveSymbol = I_IS_ZERO;
  stringINotZero->primitiveSymbol = I_NOT_ZERO;

  // Set up some constants...
  constantIntZero = new IntConst ();
  constantIntOne = new IntConst ();
  constantIntOne->ivalue = 1;
  constantIntMinusOne = new IntConst ();
  constantIntMinusOne->ivalue = -1;
  constantFalse = new BoolConst (0);
  constantTrue = new BoolConst (1);


/*****
  constantDoubleZero = new DoubleExpr ();
  constantDoubleOne = new DoubleExpr ();
  constantDoubleOne->rvalue = 1.0;
  constantCharNull = new CharExpr ();
  constantNull = new NullExpr ();
  stringUninitialized = lookupAndAdd ("<uninitialized string>", ID);
  stringGenericDestructor = lookupAndAdd ("_Generic_Destructor", ID);
  stringThis = lookupAndAdd ("_this", ID);
*****/

}



// printAllData ()
//
// This routine prints out all the data structures using printAst().
// in full and gory detail.
//
void printAllData () {

  fflush (stdout);
  printf ("\n================  HEADER LIST  ================\n");
  printAst (6, headerList);
  printf ("================================\n");

  // printf ("\n================  CODE  ================\n");
  // printAst (6, code);
  // printf ("================================\n");

/*****
  printf ("constantIntZero:\n");
  printAst (6, constantIntZero);
  printf ("constantIntOne:\n");
  printAst (6, constantIntOne);
  printf ("constantIntMinusOne:\n");
  printAst (6, constantIntMinusOne);
  printf ("constantDoubleZero:\n");
  printAst (6, constantDoubleZero);
  printf ("constantDoubleOne:\n");
  printAst (6, constantDoubleOne);
  printf ("constantCharNull:\n");
  printAst (6, constantCharNull);
  printf ("constantFalse:\n");
  printAst (6, constantFalse);
  printf ("constantTrue:\n");
  printAst (6, constantTrue);
  printf ("constantNull:\n");
  printAst (6, constantNull);
  printf ("basicTypeInt:\n");
  printAst (6, basicTypeInt);
  printf ("basicTypeDouble:\n");
  printAst (6, basicTypeDouble);
  printf ("basicTypeChar:\n");
  printAst (6, basicTypeChar);
  printf ("basicTypeBool:\n");
  printAst (6, basicTypeBool);
  printf ("basicTypeVoid:\n");
  printAst (6, basicTypeVoid);
  printf ("basicTypeNull:\n");
  printAst (6, basicTypeNull);
  printf ("basicTypeCharPtr:\n");
  printAst (6, basicTypeCharPtr);
  printf ("basicTypeVoidPtr:\n");
  printAst (6, basicTypeVoidPtr);
*****/

  fflush (stdout);
}



// dump (message)
//
// This routine prints out various data structures, using "prettyPrinting".
//
void dump (const char * message) {
  Header * hdr;

  fflush (stdout);
  printf ("\n***************\n");
  printf ("**           **\n");
  printf ("**  DUMPING  **  %s\n", message);
  printf ("**           **\n");
  printf ("***************\n\n");


  // Print out the headerList...
  hdr = headerList;
  printf ("\n================  HEADER LIST  ================\n");
  while (hdr) {
    hdr->prettyPrint (4);
    // if (hdr->packageMapping) {
    //   hdr->packageMapping->print (4);
    // }
    printf ("================================\n");
    hdr = hdr->next;
  }

/*****
  if (code) {
    printf ("\n================  CODE  ================\n");
    code->prettyPrint (4);
    printf ("================================\n");
  }
*****/

  // Print out the headerMapping...
  // printf ("\n\n");
  // headerMapping->print (0);

  fflush (stdout);

}



// testLexer ()
//
// This routine can be used to test the lexer portion of the compiler.
//
void testLexer () {
  while (1) {
    printToken (token);
/*****
    printf ("\t\t\t");
    printToken (token2);
    printf ("\t\t\t\t");
    printToken (token3);
    printf ("\t\t\t\t\t");
    printToken (token4);
    printf ("\t\t\t\t\t\t");
    printToken (token5);
*****/
    if (token.type == EOF) {
      break;
    }
    scan ();
  }
//  printStringTable ();
}



// printToken (token)
//
// This routine prints a token in a form such as...
//      7    ID            abc
//      8    INT           1234
//      9    STRING_CONST  "abc"
//      10   CHAR_CONST    0x61     97      'a'
//      11   WHILE
//
void printToken (Token token) {
  int i;
  printf("%s\t%d\t%d\t%s",
         extractFilename (token),
         extractLineNumber (token),
         extractCharPos (token),
         symbolName (token.type));
  switch (token.type) {
    case ID:
      printf("\t");
      printString (stdout, token.value.svalue);
      break;
    case OPERATOR:
      printf("\t");
      printString (stdout, token.value.svalue);
      break;
    case STRING_CONST:
      printf("\t\"");
      printString (stdout, token.value.svalue);
      printf("\"");
      break;
    case CHAR_CONST:
      i = token.value.ivalue;
      printf("\t%02x\t%d", i, i);
      if ((i >= ' ') && (i <= '~')) {
        printf("\t\'%c\'", i);
      }
      break;
    case INT_CONST:
      printf("\t0x%08x\t%d", token.value.ivalue, token.value.ivalue);
      break;
    case DOUBLE_CONST:
      printf("\t%.16g", token.value.rvalue);
      break;
  }
  printf("\n");
}





// checkTokenSkipping (count)
//
// "count" is the number of tokens we just skipped over.  If it exceeds a
// threshhold, then print a message, using the current token as the position
// of the message.  Also, watch out for hitting EOF.
//
void checkTokenSkipping (int count) {
  if (count > TOKEN_SKIP_COUNT) {
    fprintf (stderr, "%s:%d:        Skipping %d tokens...\n",
                     extractFilename (token),
                     extractLineNumber (token),
                     count);
  }
  if (token.type == EOF) {
    fprintf (stderr, "%s:%d: *****  SYNTAX ERROR: Unexpected EOF... aborting\n",
                     extractFilename (token),
                     extractLineNumber (token));
    terminateCompiler ();
  }
}



// processCommandLine (argc, argv)
//
// This routine processes the command line options.
//
void processCommandLine (int argc, char ** argv) {
  int argCount;
  int badArgs = 0;
  int len;
  for (argc--, argv++; argc > 0; argc -= argCount, argv += argCount) {
    argCount = 1;
    // Scan the -h option
    if (!strcmp (*argv, "-h")) {
      printHelp ();
      exit (1);

    // Check for the -s option
    } else if (!strcmp (*argv, "-s")) {
      commandOptionS = 1;

    // Check for the -p option
    } else if (!strcmp (*argv, "-p")) {
      commandOptionP = 1;

    // Check for the -ast option
    } else if (!strcmp (*argv, "-ast")) {
      commandOptionAST = 1;

    // Check for the -testLexer option
    } else if (!strcmp (*argv, "-testLexer")) {
      commandOptionTestLexer = 1;

    // Check for the -testParser option
    } else if (!strcmp (*argv, "-testParser")) {
      commandOptionTestParser = 1;

    // Check for the -unsafe option
    } else if (!strcmp (*argv, "-unsafe")) {
      safe = 0;

    // Check for the -o option, which should be followed by a file name
    } else if (!strcmp (*argv, "-o")) {
      if (argc <= 1) {
        fprintf (stderr,
          "Expecting filename after -o option.  Use -h for help display.\n");
        badArgs = 1;
      } else {
        argCount++;
        if (outputFileName == NULL) {
          outputFileName = *(argv+1);
        } else {
          fprintf (stderr,
            "Invalid command line:  Multiple output files.  Use -h for help display.\n");
          badArgs = 1;
        }
      }

    // Check for the search directory name
    } else if (!strcmp (*argv, "-d")) {
      if (argc <= 1) {
        fprintf (stderr,
          "Expecting search directory prefix after -d option.  Use -h for help display.\n");
        badArgs = 1;
      } else {
        argCount++;
        if (commandDirectoryName == NULL) {
          commandDirectoryName = *(argv+1);
        } else {
          fprintf (stderr,
            "Invalid command line:  Multiple search directories.  Use -h for help display.\n");
          badArgs = 1;
        }
      }

    // Check for the package file name
    } else if ((*argv)[0] != '-') {
      if (commandPackageName == NULL) {
        commandPackageName = *argv;
      } else {
        fprintf (stderr,
          "Invalid command line:  Multiple package names.  Use -h for help display.\n");
        badArgs = 1;
      }
    } else {
      fprintf (stderr,
        "Invalid command line option (%s).  Use -h for help display.\n", *argv);
      badArgs = 1;
    }
  }

  // If command line problems, then abort now.
  if (badArgs) {
    exit (1);
  }

  // Figure out the name of the .h header file.
  if (commandPackageName != NULL) {
    len = strlen (commandPackageName);
    headerFileName = (char *) calloc (1, len + 4);
    strcpy (headerFileName, commandPackageName);
    headerFileName [len] = '.';
    headerFileName [len+1] = 'h';
    headerFileName [len+2] = '\0';
  }

  // Figure out the name of the .c code file.
  if (commandPackageName != NULL) {
    codeFileName = (char *) calloc (1, len + 4);
    strcpy (codeFileName, commandPackageName);
    codeFileName [len] = '.';
    codeFileName [len+1] = 'k';
    codeFileName [len+2] = '\0';
  }

  // Figure out the name of the .s output file.
  if (outputFileName == NULL) {
    if (commandPackageName != NULL) {
      outputFileName = (char *) calloc (1, len + 4);
      strcpy (outputFileName, commandPackageName);
      outputFileName [len] = '.';
      outputFileName [len+1] = 's';
      outputFileName [len+2] = '\0';
    }
  }

  // Open the output (.s) file.
  if (outputFileName == NULL) {
    outputFile = stdout;
  } else {
    outputFile = fopen (outputFileName, "w");
    if (outputFile == NULL) {
      fprintf (stderr, "File \"%s\" could not be opened for writing\n", outputFileName);
      exit (1);
    }
  }

}



// printHelp ()
//
// This routine prints some documentation.  It is invoked whenever
// the -h option is used on the command line.
//
void printHelp () {
  printf (
"==============================\n"
"=====                    =====\n"
"=====  The KPL Compiler  =====\n"
"=====                    =====\n"
"==============================\n"
"\n"
"Copyright 2002-2007, Harry H. Porter III\n"
"========================================\n"
"  Original Author:\n"
"    06/15/02 - Harry H. Porter III\n"
"  Modifcations by:\n"
"    03/15/06 - Harry H. Porter III\n"
"  Modifcations by:\n"
"    10/01/10 - Dan Colish\n"
"\n"
"Command Line Options\n"
"====================\n"
"  Command line options may be given in any order.\n"
"    -h\n"
"      Print this help info.  All other options are ignored.\n"
"    packageName\n"
"      Compile the package with this name.  The input will come from the files\n"
"      called \"packageName.h\" and \"packageName.c\".  No extension should be\n"
"      given on the command line.  Only one package may be compiled at once.\n"
"      The packageName is required.\n"
"    -d directoryPrefix\n"
"      When looking for header and code files, the default is to look in the\n"
"      current directory.  With this option, the current directory is first\n"
"      searched.  If that fails, then the directoryPrefix is prepended to the\n"
"      file name and the resulting file name is used.  For example:\n"
"          kpl myPack -d ~harry/BlitzLib/\n"
"      will first try to open \"myPack.h\" and, if that fails, will try to open\n"
"      \"~harry/BlitzLib/myPack.h\".\n"
"    -unsafe\n"
"      Allow unsafe language constructs.\n"
"    -o filename\n"
"      If there are no errors, an assembly code file will be created.  This \n"
"      option can be used to give the output file a specific name.  If \n"
"      missing, the name of the output file will be computed from the name of\n"
"      the package and appending \".s\".  For example:\n"
"           myPackage  -->  myPackage.s\n"
"      COMPILER DEBUGGING: If packageName and output filename are missing,\n"
"      stdout will be used.\n"
"    -testLexer\n"
"      COMPILER DEBUGGING: Scan tokens only, and print tokens out.  Input may\n"
"      come from stdin.\n"
"    -testParser\n"
"      COMPILER DEBUGGING: Parse program only, and print data structures out.\n"
"      Input may come from stdin.\n"
"    -s\n"
"      COMPILER DEBUGGING: Print the symbol table on stdout.\n"
"    -p\n"
"      COMPILER DEBUGGING: Pretty-print the AST.\n"
"    -ast\n"
"      COMPILER DEBUGGING: Dump the full AST.\n"
  );
}



// checkHostCompatibility ()
//
// This routine checks that the host implementation of C++ meets certain
// requirements.
//
// (1) This routine checks that integers are represented using exactly 4
// bytes.
//
// (2) This routine checks that integers are stored in the expected
// Big or Little Endian order.
//
// (3) This routine checks that integer overflow behavior is as expected
// with two's complement arithmetic.
//
// (4) This routine checks that doubles are implemented using 8-bytes in
// the IEEE standard, with the bytes in correct Big/Little Endian order.
//
// (5) This routine checks that the double->int conversion works as
// expected.  If this is not the case, then truncateToInt() will need to
// be changed.
//
void checkHostCompatibility () {
  union fourBytes {
    char chars [4];
    unsigned int i;
  } fourBytes;
  double d;
  char * p, * q;
  int i, i1, i2, i3;

  // Check that ints are in the expected Big/Little Endian order.
  fourBytes.chars[0] = 0x12;
  fourBytes.chars[1] = 0x34;
  fourBytes.chars[2] = 0x56;
  fourBytes.chars[3] = 0x78;
  if (SWAP_BYTES(fourBytes.i) != 0x12345678) {
    fatalError ("There is a big/little endian byte ordering problem.");
  }

  // Check that we have at least 4 bytes of precision.
  i = 0x00000001;
  i <<= 20;
  i <<= 10;
  i >>= 20;
  i >>= 10;
  if (i != 0x00000001) {
    fatalError ("This program only runs on computers with 4 byte integers - 1");
  }

  // Check that we have no more than 4 bytes of precision.
  i = 0x00000001;
  i <<= 20;
  i <<= 13;   // Some compilers treat <<33 as a nop!
  i >>= 20;
  i >>= 13;
  if (i != 0x00000000) {
    fatalError ("This program only runs on computers with 4 byte integers - 2");
  }

  // Check that we have the expected overflow behavior for ints.
  i = -2147483647;
  i = i - 2;
  if (i != 2147483647) {
    fatalError ("This program only runs on computers with 4 byte integers - 3");
  }

  // Check that doubles are represented as we expect.
  d = 123.456e37;
  p = (char *) &d;
  q = p;
  // If doubles are stored in Big Endian byte order....
  if ((*p++ == '\x48') &&
      (*p++ == '\x0d') &&
      (*p++ == '\x06') &&
      (*p++ == '\x3c') &&
      (*p++ == '\xdb') &&
      (*p++ == '\x93') &&
      (*p++ == '\x27') &&
      (*p++ == '\xcf')) {
#ifdef BLITZ_HOST_IS_LITTLE_ENDIAN
    fatalError ("There is a big/little endian byte ordering problem with doubles - 1.");
#endif

  // Else, if doubles are stored in Little Endian byte order...
  } else if ((*q++ == '\xcf') &&
             (*q++ == '\x27') &&
             (*q++ == '\x93') &&
             (*q++ == '\xdb') &&
             (*q++ == '\x3c') &&
             (*q++ == '\x06') &&
             (*q++ == '\x0d') &&
             (*q++ == '\x48')) {
#ifdef BLITZ_HOST_IS_LITTLE_ENDIAN

#else
    fatalError ("There is a big/little endian byte ordering problem with doubles - 2.");
#endif

  // Else, if doubles are stored in some other way...
  } else {
    fatalError ("The host implementation of 'double' is not what I expect.");
  }

  // There is variation in the way different hosts handle double->int conversion
  // when the double is too large to represent as an integer.  When checking
  // we must do the conversion in two steps, since some compilers perform the
  // conversion at compile time, and will do the conversion differently than
  // the host machine.  Truly appalling, isn't it!
  //   On PPC,   (int) 9e99 is 0x7fffffff
  //   On PPC,   (int) d    is 0x7fffffff
  //   On Intel, (int) 9e99 is 0x7fffffff
  //   On Intel, (int) d    is 0x80000000
  //
  i = (int) 9e99;
  // printf ("(int) 9e99 is 0x%08x\n", i);
  d = 9e99;
  i = (int) d;  // Note: ((int) 9e99 == 0 while ((int) d) == 2147483647)!!!
  // printf ("(int) d is 0x%08x\n", i);

  // Check that double->int conversion works as expected.
  d = 4.9;
  i1 = (int) d;
  d = -4.9;
  i2 = (int) d;
  d = -9e99;
  i3 = (int) d;
  if ((i1 !=  4) ||
      (i2 != -4) ||
      (i3 != (int) 0x80000000)) {
    printf ("%d %d %d\n", i1, i2, i3);
    fatalError ("The host implementation of double->int casting is not what I expect.");
  }

}

