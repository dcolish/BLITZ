#include "errors.h"

// programLogicError (msg)
//
// This routine prints the message and terminates the compiler.
//
void programLogicError (const char * msg) {
  fprintf (stderr,
"********************************************************************\n"
"*****\n"
"*****  PROGRAM LOGIC ERROR\n"
"*****\n"
"*****  It appears that this compiler or you code contains a software bug.\n"
"*****  I apologize for the inconvenience it causes you.\n"
"*****\n"
"*****  Error message: \"%s\"\n"
"*****\n"
"********************************************************************\n", msg);
  errorsDetected++;
  terminateCompiler ();
}



// terminateCompiler ()
//
// Print out the number of errors (if any) and terminate of the compiler.  If errors, then
// remove the output file (if any).  If no errors, the close the output file normally.
//
void terminateCompiler () {

  if (errorsDetected == 0) {
    if (outputFileName != NULL) {
      fclose (outputFile);
    }
    return;
  } else if (errorsDetected == 1) {
    fprintf (stderr, "\n**********  1 error detected!  **********\n");
  } else {
    fprintf (stderr, "\n**********  %d errors detected!  **********\n",
             errorsDetected);
  }

  if (outputFileName != NULL) {
    fclose (outputFile);
    remove (outputFileName);
  }
}

 

// fatalError (msg)
//
// This routine is called to print an error message and the current line
// number of the curent token.  It aborts the compiler.
//
void fatalError (const char *msg) {
  errorsDetected++;
  doMessage (token, "*****  FATAL ERROR", msg);
  terminateCompiler ();
}



// error (node, msg)
//
// This routine is called to print an error message.  It returns; it
// does not terminate the program after printing.  The "node" parameter
// is used to print additional information about the position of the error.
//
void error (AstNode * node, const char * msg) {
  errorsDetected++;
  doMessage (node->tokn, "*****  ERROR", msg);
}



// error2 (node, msg)
//
// This routine is called to print an error message.  It returns; it
// does not terminate the program after printing.  The "node" parameter
// is used to print additional information about the position of the error.
// It differs from "error()" in that it does not print "*****  ERRROR"; it is
// used to print additional info after the initial error message.
//
void error2 (AstNode * node, const char * msg) {
  // errorsDetected++;
  doMessage (node->tokn, "            ", msg);
}



// syntaxError (msg)
//
// This routine is called to print a syntax error message.
//
// This routine returns; it does not terminate the compiler after printing.
//
// It uses the current token to print additional information about the
// position of the error.
//
void syntaxError (const char * msg) {
  syntaxErrorWithToken (token, msg);
}



// syntaxErrorWithToken (tok, msg)
//
// This routine is called to do the work of printing a syntax error message,
// position on 'tok'.
//
void syntaxErrorWithToken (Token tok, const char * msg) {
  // If the last message was on this token, then suppress this message.
  if (tok.tokenPos != tokenPosOfLastError) {
    errorsDetected++;
    doMessage (tok, "*****  SYNTAX ERROR", msg);
  }
  tokenPosOfLastError = tok.tokenPos;
}



// doMessage (tok, prefix, msg)
//
// Print info about the current token and the given "msg".
//
void doMessage (Token tok, const char * prefix, const char * msg) {
  fprintf (stderr, "%s:%d: %s at ",
                     extractFilename (tok),
                     extractLineNumber (tok),
                     prefix);
  switch (tok.type) {
    case ID:
      fprintf (stderr, "\'");
      printString (stderr, tok.value.svalue);
      fprintf (stderr, "\'");
      break;
    case STRING_CONST:
      fprintf (stderr, "\"");
      printString (stderr, tok.value.svalue);
      fprintf (stderr, "\"");
      break;
    case CHAR_CONST:
      fprintf (stderr, "\'");
      printChar (stderr, tok.value.ivalue);
      fprintf (stderr, "\'");
      break;
    case INT_CONST:
      fprintf (stderr, "\'%d\'", tok.value.ivalue);
      break;
    case DOUBLE_CONST:
      fprintf (stderr, "%.16g", tok.value.rvalue);
      break;
    case OPERATOR:
      fprintf (stderr, "\"");
      printString (stderr, tok.value.svalue);
      fprintf (stderr, "\"");
      break;
    default:
      fprintf (stderr, "%s", symbolName (tok.type));
  }
  fprintf (stderr, ": %s\n", msg);
  fflush (stderr);
  if (errorsDetected >= MAX_NUMBER_OF_ERRORS) {
    fprintf (stderr, "%s:%d: *****  Too many errors - I'm giving up\n",
                     extractFilename (tok),
                     extractLineNumber (tok));
    terminateCompiler ();
  }
}



// errorWithType (msg, type)
//
// This routine is called to print an error message.  It returns; it does not
// terminate the program unless we've had too many errors.
//
// The "type" parameter is printed after the message.  For example, if msg is
//    "The expected type is"
// the following might get printed:
//    test.c:26:         The expected type is: ptr to array [*] of char
//
// This routine calls "resolveNamedType" so it prints out the underlying
// type, getting rid of aliases.
//
void errorWithType (const char * msg, Type * type) {
  Token tok;
  if (type == NULL) {
    tok.tokenPos = 0;
  } else {
    tok = type->tokn;
  }
  fprintf (stderr, "%s:%d:              %s: ",
                     extractFilename (tok),
                     extractLineNumber (tok),
                     msg);
  fpretty (type);
  fprintf (stderr, "\n");
  fflush (stderr);
  // errorsDetected++;
  // if (errorsDetected >= MAX_NUMBER_OF_ERRORS) {
  //   fprintf (stderr, "%s:%d: *****  Too many errors - I'm giving up\n",
  //                    extractFilename (tok),
  //                    extractLineNumber (tok));
  //   terminateCompiler ();
  // }
}

