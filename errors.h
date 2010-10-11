#include <stdio.h>

#include "util.h"
#include "printAst.h"

extern int tokenPosOfLastError;             // Used to suppress extraneous syntax errors
extern int errorsDetected;                    // Count of errors detected so far


/* Error handling code */
void programLogicError (const char * msg);
void terminateCompiler ();
void fatalError (const char * msg);
void syntaxError (const char * msg);
void syntaxErrorWithToken (Token token, const char * msg);
void error (AstNode * node, const char * msg);
void error2 (AstNode * node, const char * msg);
void doMessage (Token token, const char * prefix, const char * msg);
void errorWithType (const char * msg, Type * type);
