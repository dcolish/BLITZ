#include <float.h>

#include "ast.h"
#include "printAst.h"
#include "errors.h"

// During parsing errors, tokens may be skipped in an attempt to recover
// and continue parsing.  Do we print a message saying that tokens were
// skipped?  This constant determines the threshhold for printing that msg.


extern int errorsDetected;                    // Count of errors detected so far

char * commandDirectoryName;    // The search directory name, NULL = missing
TokenValue currentTokenValue;   // Used in lexer only
Token tokenMinusOne, token, token2, token3, token4, token5;
int hashVal;                    // The running hash code for this file
int hashCount;                  // Used in comuting the hashVal

int currentInputFileIndex;      // These describe the current position in the file
int currentLineOfToken;         // .
int currentCharPosOfToken;      // .
int posOfNextToken;             // Position of the next token
int eofCount;                   // Used to check for looping on EOF
FILE * inputFile;               // The input file, e.g., stdin
char * outputFileName;          // The .s filename, NULL = missing
FILE * outputFile;              // The output file, e.g., stdout
char * inputFileNames [MAX_INPUT_FILES+1];   // Array of ptrs to file 
int tokenPosOfLastError;
int errorsDetected;

String * stringTableIndex [STRING_TABLE_HASH_SIZE];

//----------  Lexical Routines  ----------

char * appendStrings (const char * str1, const char * str2, const char * str3);
const char * initScanner (const char * filename);
void scan ();
int getToken (void);
void lexError (const char *msg);
void initKeywords ();
String * lookupAndAdd (const char * givenStr, int newType);
String * lookupAndAdd2 (const char * givenStr, int length, int newType);
int bytesEqual (const char * p, const char * q, int length);


double raisedToThePower (int i);
int isEscape (char c);
int scanEscape ();
int isOpChar (char);
void addToHash (int i);
void addTokenToHash (Token tok);
int createTokenPos ();
void incrLineNumber ();
int getNextChar ();
void unGetChar (char ch);
void addToInputFilenames (const char * filename);
