// lexer.cc  --  Routines for lexical analysis
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
//   09/27/10 - Dan Colish - Correct pointer casting
//
// The primary routines provided in this file are...
//
//   initScanner (filename)
//   scan ()
//
// Information about the current token will be found in...
//
//   token        - the type of the token
//     token.type        - the type of the token
//     token.value       - additional info about the token
//     token.tokenPos    - the position of the token (file, line no, char pos)
//
// Look-ahead tokens can also be found in...
//
//   token2
//   token3
//   token4
//   token5
//
// The input is obtained from the operating system character-by-character,
// using the following interface:
//
//   getc (inputFile)
//   ungetc (ch, inputFile)
//
// After all tokens have been scanned, the pseudo-random integer "hashVal" will have been
// computed.  Any change to the token stream will (with high probability) result in
// a different value being stored in "hashVal".



#include "lexer.h"



// initScanner (filename)  -->  actualFileName
//
// This routine initializes the 5 token look-ahead buffer.  The tokens will come from the
// file with the given name.  We try to open the file (for read-only) and if that fails,
// we prepend the directory path prefix to the name and try again.
//
// We return the actual file name if we succeed and NULL if problems occurred
// while opening the file.
//
// If 'filename' is NULL, we will open stdin and return "<stdin>".
//
const char * initScanner (const char * filename) {
  const char * currentInputFileName = "<filename missing>";
  currentLineOfToken = 1;
  currentCharPosOfToken = 0;
  // newlinePreceedsThisToken = 0;
  // newlinePreceedsToken2 = 0;
  posOfNextToken = createTokenPos ();
  token.type = COLON;
  token.value.ivalue = 0;
  token.tokenPos = posOfNextToken;
  token2 = token;
  token3 = token;
  token4 = token;
  token5 = token;
  eofCount = 0;
  hashVal = 0x12345678;
  hashCount = 20;

  if (filename == NULL) {
    currentInputFileName = lookupAndAdd ("<stdin>", ID) -> chars;
    inputFile = stdin;
  }
  else {
    currentInputFileName = filename;
    // Open the input file.
    inputFile = fopen (currentInputFileName, "r");
    // If that fails, prepend the directory path prefix and try again.
    if ((inputFile == NULL) && (commandDirectoryName != NULL)) {
      currentInputFileName = appendStrings (commandDirectoryName, filename, "");
      inputFile = fopen (currentInputFileName, "r");
    }
    // If that fails, then print an error, set things to the EOF state, and return.
    if (inputFile == NULL) {
      if (commandDirectoryName == NULL) {
        fprintf (stderr,
                 "%s:0: *****  LEXICAL ERROR: File could not be opened for reading\n",
                 filename);
      } else {
        fprintf (stderr,
                 "%s:0: *****  LEXICAL ERROR: File could not be opened for reading (also tried %s)\n",
                 filename,
                 currentInputFileName);
      }
      errorsDetected++;
      token.type = EOF;
      token.tokenPos = posOfNextToken;
      tokenMinusOne = token;
      token2 = token;
      token3 = token;
      token4 = token;
      token5 = token;
      return NULL;
    }   
  }
  addToInputFilenames (currentInputFileName);
  token5.type = getToken ();
  token5.value = currentTokenValue;
  token5.tokenPos = posOfNextToken;
  addTokenToHash (token5);
  token4 = token5;
  scan ();
  scan ();
  scan ();
  scan ();
  return currentInputFileName;
}



// scan ()
//
// This routine advances to the next token.  It shifts the 5 token look-ahead
// buffer ahead by one token.  At EOF, the token types will all be EOF.
//
void scan () {
  tokenMinusOne  = token;
  token  = token2;
  token2 = token3;
  token3 = token4;
  token4 = token5;
  if (token5.type != EOF) {
    token5.type = getToken ();
    token5.value = currentTokenValue;
    token5.tokenPos = posOfNextToken;
    addTokenToHash (token5);
  } else {
    eofCount++;
    if (eofCount > 10) {
      programLogicError ("Parser is looping on EOF");
    }
  }
}



// getToken ()
//
// Scan the next token and return it's type.  Side-effects the following:
//   currentTokenValue
//   currentInputFileIndex
//   currentLineOfToken
//   currentCharPosOfToken
// Returns EOF repeatedly after end-of-file is reached.
//
int getToken (void) {
  int ch, ch2, lengthError, numDigits;
  int intVal, t, intOverflow, realOverflow, sign;
  double realValue, exp, power;
  char lexError2 [200];                 // buffer for varying error messages
  char buffer [MAX_STR_LEN+1];          // buffer for saving a string
  int next, i;

  while (1) {
    ch = getNextChar ();

    // Process end-of-file...
    if (ch == EOF) {
      unGetChar (ch);
      posOfNextToken = createTokenPos ();
      return EOF;

    // Process newline...
    } else if (ch == '\n') {
      incrLineNumber ();
      continue;

    // Process other white space...
    } else if (ch == ' ' || ch == '\t') {
      // do nothing
      continue;
    }

    posOfNextToken = createTokenPos ();

    // Process identifiers...
    if (isalpha (ch)) {
      lengthError = 0;
      next = 0;
      while (isalpha (ch) || isdigit (ch) || (ch=='_')) {
        if (next >= MAX_STR_LEN) {
          lengthError = 1;
        } else {
          buffer [next++] = ch;
        }
        ch = getNextChar ();
      }
      unGetChar (ch);
      currentTokenValue.svalue = lookupAndAdd2 (buffer, next, ID);
      // If already there, then its type may be BOOL, ..., IF, ..., or ID
      if (lengthError) {
        lexError ("Identifier exceeds maximum allowable length");
      }
      i = currentTokenValue.svalue->type;
      return i;

    // Process strings...
    } else if (ch == '"') {
      next = 0;
      lengthError = 0;
      while (1) {
        ch2 = getNextChar ();
        if (ch2 == '"') {
          break;
        } else if (ch2 == '\n') {
          incrLineNumber ();
          if (next >= MAX_STR_LEN) {
            lengthError = 1;
          } else {
            buffer [next++] = ch2;
          }
        } else if (ch2 == EOF) {
          lexError ("End-of-file encountered within a string");
          unGetChar (ch2);
          break;
        } else if ((ch2 < 32) || (ch2 > 126)) {
          sprintf (lexError2,
              "Illegal character 0x%02x in string ignored", ch2);
          lexError (lexError2);
        } else {
          if (ch2 == '\\') {
            ch2 = scanEscape ();
          }
          if (next >= MAX_STR_LEN) {
            lengthError = 1;
          } else {
            buffer [next++] = ch2;
          }
        }
      }
      currentTokenValue.svalue = lookupAndAdd2 (buffer, next, ID);
      if (lengthError) {
        lexError ("Maximum string length exceeded");
      }
      return STRING_CONST;

    // Process operators...
    } else if (isOpChar (ch)) {
      lengthError = 0;
      next = 0;
      buffer [next++] = ch;
      ch2 = getNextChar ();
      // Check for -- comments...
      if ((ch == '-') && (ch2 == '-')) {
        while (1) {
          ch = getNextChar ();
          if (ch == EOF) {
            lexError ("End-of-file encountered within a -- comment");
            unGetChar (ch);
            return EOF;
          } else if (ch == '\n') {
            incrLineNumber ();
            break;
          }
        }
        continue;
      // Check for /* comments...
      } else if ((ch == '/') && (ch2 == '*')) {
        ch2 = ' ';
        i = 1;
        while (1) {
          ch = ch2;
          ch2 = getNextChar ();
          if (ch2 == EOF) {
            lexError ("End-of-file encountered within a /* comment");
            unGetChar (ch2);
            return EOF;
          } else if (ch2 == '\n') {
            incrLineNumber ();
          }
          if (ch == '/' && ch2 == '*') {
            i++;
            ch = ' ';
            ch2 = ' ';
          }
          if (ch == '*' && ch2 == '/') {
            if (i <= 1) {
              break;
            } else {
              i--;
              ch = ' ';
              ch2 = ' ';
            }
          }
        }
        continue;
      } else if (isOpChar (ch2)) {
        buffer [next++] = ch2;
        ch = getNextChar ();
        while (isOpChar (ch)) {
          if (next >= MAX_STR_LEN) {
            lengthError = 1;
          } else {
            buffer [next++] = ch;
          }
          ch = getNextChar ();
        }
        unGetChar (ch);
      } else {
        unGetChar (ch2);
      }
      currentTokenValue.svalue = lookupAndAdd2 (buffer, next, OPERATOR);
      if (lengthError) {
        lexError ("Operator exceeds maximum allowable length");
      }
      return currentTokenValue.svalue->type;

    // Process character constants...
    } else if (ch == '\'') {
      currentTokenValue.ivalue = '?';
      ch2 = getNextChar ();
      if (ch2 == EOF) {
        lexError ("End-of-file encountered within a char constant");
        unGetChar (ch2);
        return EOF;
      } else if (ch2 == '\n') {
        lexError ("End-of-line encountered within a char constant");
        incrLineNumber ();
        return CHAR_CONST;
      }
      if (ch2 == '\\') {
        ch2 = scanEscape ();
      }
      currentTokenValue.ivalue = ch2;
      ch2 = getNextChar ();
      if (ch2 != '\'') {
        unGetChar (ch2);
        lexError ("Expecting closing quote in character constant");
      }
      return CHAR_CONST;

    // Process integer and double constants...
    } else if (isdigit (ch)) {

      // See if we have 0x...*/
      if (ch == '0') {
        ch2 = getNextChar ();
        if (ch2 == 'x') {
          numDigits = 0;
          ch = getNextChar ();
          if (hexCharToInt (ch) < 0) {
            lexError ("Must have a hex digit after 0x");
          }
          next = intVal = intOverflow = 0;
          while (hexCharToInt (ch) >= 0) {
            intVal = (intVal << 4) + hexCharToInt (ch);
            numDigits++;
            ch = getNextChar ();
          }
          unGetChar (ch);
          if (numDigits > 8) {
            lexError ("Hex constants must be 8 or fewer digits");
            intVal = 0;
          }
          currentTokenValue.ivalue = intVal;
          return INT_CONST;
        }
        unGetChar (ch2);
      }

      // Otherwise we have a string of decimal numerals.
      intVal = intOverflow = realOverflow = 0;
      exp = 1.0;
      realValue = 0.0;
      while (isdigit (ch)) {
        t = intVal * 10 + (ch - '0');
        if (t < intVal) {
          intOverflow = 1;
        }
        intVal = t;
        realValue = (realValue * 10.0) + (double) (ch - '0');
        if (realValue > DBL_MAX) {
          realOverflow = 1;
        }
        ch = getNextChar ();
      }
      // If we have a real number...
      if ((ch == '.') || (ch == 'e') || (ch == 'E')) {
        // Read in the fractional part.
        if (ch == '.') {
          ch = getNextChar ();
          if (!isdigit (ch)) {
            lexError ("At least one digit is required after decimal point");
          }
          while (isdigit (ch)) {
            exp *= 10.0;
            realValue = realValue + ((double) (ch - '0') / exp);
            ch = getNextChar ();
          }
        }
        intVal = 0;
        sign = 1;
        if ((ch == 'e') || (ch == 'E')) {
          ch = getNextChar ();
          // Process the exponent sign, if there is one.
          if (ch == '+') {
            ch = getNextChar ();
          } else if (ch == '-') {
            ch = getNextChar ();
            sign = -1;
          }
          // Read in the exponent integer into intVal.
          if (!isdigit (ch)) {
            lexError ("Expecting exponent numerals");
          } else {
            intVal = intOverflow = 0;
            while (isdigit (ch)) {
              t = intVal * 10 + (ch - '0');
              if (t < intVal) {
                intOverflow = 1;
              }
              intVal = t;
              ch = getNextChar ();
            }
            if (intOverflow) {
              lexError ("Exponent is out of range");
              intVal = 0;
            }
          }
        }
        unGetChar (ch);
        currentTokenValue.rvalue = 999.888;
        // power =  pow ((double) 10.0, (double) (sign * intVal));
        power =  raisedToThePower (sign * intVal);
        realValue = realValue * power;
        if (realValue > DBL_MAX) {
          realOverflow = 1;
        }
        currentTokenValue.rvalue = realValue;
        if (realOverflow) {
          lexError ("Real number is out of range");
          currentTokenValue.rvalue = 0.0;
        }
        return DOUBLE_CONST;
      } else {  // If we have an integer...
        unGetChar (ch);
        if (intOverflow) {
          lexError ("Integer out of range (0..2147483647); use 0x80000000 for -2147483648");
          intVal = 0;
        }
        currentTokenValue.ivalue = intVal;
        return INT_CONST;
      }

    // Check for one character symbols.
    } else if (ch == '(') {
      return L_PAREN;
    } else if (ch == ')') {
      return R_PAREN;
    } else if (ch == '{') {
      return L_BRACE;
    } else if (ch == '}') {
      return R_BRACE;
    } else if (ch == '[') {
      return L_BRACK;
    } else if (ch == ']') {
      return R_BRACK;
    } else if (ch == ',') {
      return COMMA;
    } else if (ch == '.') {
      return PERIOD;
    } else if (ch == ':') {
      return COLON;
    } else if (ch == ';') {
      return SEMI_COLON;

    // // Check for semi-colon and print special message.
    // } else if (ch == ';') {
    //   lexError ("The semi-colon is not used in the KPL programming language");

    // Otherwise, we have an invalid character; ignore it.
    } else {
      if ((ch>=' ') && (ch<='~')) {
        sprintf (lexError2, "Illegal character '%c' ignored", ch);
      } else {
        sprintf (lexError2, "Illegal character 0x%02x ignored", ch);
      }
      lexError (lexError2);
    }
  }
}



// lexError (msg)
//
// This routine is called to print an error message and the current line
// number.  It returns.
//
void lexError (const char *msg) {
    fprintf (stderr, "%s", inputFileNames [currentInputFileIndex]);
    if (currentLineOfToken >= 65535) {
      fprintf (stderr, ":<line number not available>: ");
    } else {
      fprintf (stderr, ":%d: ", currentLineOfToken);
    }
    fprintf (stderr, "*****  LEXICAL ERROR: %s\n", msg);
    errorsDetected++;
}



// initKeywords ()
//
// This routine adds each keyword to the symbol table with the corresponding
// type code.
// This routine initializes the "keywords" and the "keywordTokens" arrays.
//
void initKeywords () {

  lookupAndAdd ("=", EQUAL);
  lookupAndAdd ("alloc", ALLOC);
  lookupAndAdd ("anyType", ANY_TYPE);
  lookupAndAdd ("array", ARRAY);
  lookupAndAdd ("arraySize", ARRAY_SIZE);
  lookupAndAdd ("asInteger", AS_INTEGER);
  lookupAndAdd ("asPtrTo", AS_PTR_TO);
  lookupAndAdd ("behavior", BEHAVIOR);
  lookupAndAdd ("bool", BOOL);
  lookupAndAdd ("break", BREAK);
  lookupAndAdd ("by", BY);
  lookupAndAdd ("case", CASE);
  lookupAndAdd ("catch", CATCH);
  lookupAndAdd ("char", CHAR);
  lookupAndAdd ("class", CLASS);
  lookupAndAdd ("code", CODE);
  lookupAndAdd ("const", CONST);
  lookupAndAdd ("continue", CONTINUE);
  lookupAndAdd ("debug", DEBUG);
  lookupAndAdd ("default", DEFAULT);
  lookupAndAdd ("do", DO);
  lookupAndAdd ("double", DOUBLE);
  lookupAndAdd ("else", ELSE);
  lookupAndAdd ("elseIf", ELSE_IF);
  lookupAndAdd ("endBehavior", END_BEHAVIOR);
  lookupAndAdd ("endClass", END_CLASS);
  lookupAndAdd ("endCode", END_CODE);
  lookupAndAdd ("endFor", END_FOR);
  lookupAndAdd ("endFunction", END_FUNCTION);
  lookupAndAdd ("endHeader", END_HEADER);
  lookupAndAdd ("endIf", END_IF);
  lookupAndAdd ("endInterface", END_INTERFACE);
  lookupAndAdd ("endMethod", END_METHOD);
  lookupAndAdd ("endRecord", END_RECORD);
  lookupAndAdd ("endSwitch", END_SWITCH);
  lookupAndAdd ("endTry", END_TRY);
  lookupAndAdd ("endWhile", END_WHILE);
  lookupAndAdd ("enum", ENUM);
  lookupAndAdd ("errors", ERRORS);
  lookupAndAdd ("extends", EXTENDS);
  lookupAndAdd ("external", EXTERNAL);
  lookupAndAdd ("false", FALSE);
  lookupAndAdd ("fields", FIELDS);
  lookupAndAdd ("for", FOR);
  lookupAndAdd ("free", FREE);
  lookupAndAdd ("function", FUNCTION);
  lookupAndAdd ("functions", FUNCTIONS);
  lookupAndAdd ("header", HEADER);
  lookupAndAdd ("if", IF);
  lookupAndAdd ("implements", IMPLEMENTS);
  lookupAndAdd ("infix", INFIX);
  lookupAndAdd ("int", INT);
  lookupAndAdd ("interface", INTERFACE);
  lookupAndAdd ("isInstanceOf", IS_INSTANCE_OF);
  lookupAndAdd ("isKindOf", IS_KIND_OF);
  lookupAndAdd ("messages", MESSAGES);
  lookupAndAdd ("method", METHOD);
  lookupAndAdd ("methods", METHODS);
  lookupAndAdd ("new", NEW);
  lookupAndAdd ("null", NULL_KEYWORD);
  lookupAndAdd ("of", OF);
  lookupAndAdd ("prefix", PREFIX);
  lookupAndAdd ("ptr", PTR);
  lookupAndAdd ("record", RECORD);
  lookupAndAdd ("renaming", RENAMING);
  lookupAndAdd ("return", RETURN);
  lookupAndAdd ("returns", RETURNS);
  lookupAndAdd ("self", SELF);
  lookupAndAdd ("sizeOf", SIZE_OF);
  lookupAndAdd ("super", SUPER);
  lookupAndAdd ("superclass", SUPER_CLASS);
  lookupAndAdd ("switch", SWITCH);
  lookupAndAdd ("throw", THROW);
  lookupAndAdd ("to", TO);
  lookupAndAdd ("true", TRUE);
  lookupAndAdd ("try", TRY);
  lookupAndAdd ("type", TYPE);
  lookupAndAdd ("typeOfNull", TYPE_OF_NULL);
  lookupAndAdd ("until", UNTIL);
  lookupAndAdd ("uses", USES);
  lookupAndAdd ("var", VAR);
  lookupAndAdd ("void", VOID);
  lookupAndAdd ("while", WHILE);

}


// lookupAndAdd (givenStr, type)
//
// This routine is passed a pointer to a string of characters, terminated
// by '\0'.  It looks it up in the string table.  If there is already an entry
// in the table, it returns a pointer to the previously stored entry.
// If not found, it allocates a new table entry, copies the new string into
// the table, and returns a pointer to the new String.
// If the string is new, then its 'type' is set according to newType.
//
String * lookupAndAdd (const char * givenStr, int newType) {
  return lookupAndAdd2 (givenStr, strlen(givenStr), newType);
}



// lookupAndAdd2 (givenStr, length, type)
//
// This routine is passed a pointer to a sequence of any characters (possibly
// containing \0, of length "length".  It looks this string up in the
// string table.  If there is already an entry in the table, it returns
// a pointer to the previously created entry.  If not found, it allocates
// a new String, copies the new string into the entry, and returns
// a pointer to the new String.
//
// After calling this routine, you can use pointer comparisons to check
// for equality, rather than the more expensive test for character equality.
// Furthermore, each different string will only be stored once, saving space.
//
// If the string is new, then its 'type' is set according to newType.
//
//
String * lookupAndAdd2 (const char * givenStr, int length, int newType) {
  unsigned hashVal = 0, g;
  char * p, * q;
  int i;
  String * stringPtr;

   // Compute the hash value for the givenStr and set hashVal to it.
  for ( p = (char*)givenStr, i=0;
        i < length;
        p++, i++ ) {
    hashVal = (hashVal << 4) + (*p);
    if ((g = hashVal) & 0xf0000000) {
      hashVal = hashVal ^ (g >> 24);
      hashVal = hashVal ^ g;
    }
  }
  hashVal %= STRING_TABLE_HASH_SIZE;

  // Search the linked list and return if we find it.
  for (stringPtr = stringTableIndex [hashVal];
                      stringPtr;
                      stringPtr = stringPtr->next) {
    if ((length == stringPtr->length) &&
        (bytesEqual (stringPtr->chars, givenStr, length))) {
      return stringPtr;
    }
  }

  // Create an entry and initialize it.
  stringPtr = (String *)
                calloc (1, sizeof (String) + length + 1);
  if (stringPtr == 0) {
    fatalError ("Calloc failed in lookupAndAdd!");
  }
  for (p=(char *)givenStr, q=stringPtr->chars, i=length;
       i>0;
       p++, q++, i--) {
    *q = *p;
  }
  *q = '\0';
  stringPtr->length = length;
  stringPtr->type = newType;
  stringPtr->primitiveSymbol = 0;

  // Add the new entry to the appropriate linked list and return.
  stringPtr->next = stringTableIndex [hashVal];
  stringTableIndex [hashVal] = stringPtr;
  return stringPtr;
}



// bytesEqual (p, q, length)
//
// This function is passed two pointers to blocks of characters, and a
// length.  It compares the two sequences of bytes and returns true iff
// they are both equal.
//
int bytesEqual (const char * p, const char * q, int length) {
  for (; length>0; length--, p++, q++) {
    if (*p != *q) return 0;
  }
  return 1;
}








// raisedToThePower (int i)
//
// This routine returns 10**i.  "i" may be positive or negative.
// It uses a brute force approach.  There must be a better way...
//
double raisedToThePower (int i) {
  double d;
  if (i==0) {
    return 1.0;
  } else if (i > 0) {
    for (d=1.0; i>0; i--) {
      d = d * 10.0;
    }
    return d;
  } else {
    for (d=1.0; i<0; i++) {
      d = d / 10.0;
    }
    return d;
  }
}



// isEscapeChar (char)  -->  ASCII Value
//
// This routine is passed a char, such as 'n'.  If this char is one
// of the escape characters, (e.g., \n), then this routine returns the
// ASCII value (e.g., 10).  Otherwise, it returns -1.
//
int isEscape (char ch) {
  if (ch == '0') {
    return 0;
  } else if (ch == 'a') {
    return '\a';
  } else if (ch == 'b') {
    return '\b';
  } else if (ch == 't') {
    return '\t';
  } else if (ch == 'n') {
    return '\n';
  } else if (ch == 'v') {
    return '\v';
  } else if (ch == 'f') {
    return '\f';
  } else if (ch == 'r') {
    return '\r';
  } else if (ch == '\"') {
    return '\"';
  } else if (ch == '\'') {
    return '\'';
  } else if (ch == '\\') {
    return '\\';
  } else {
    return -1;
  }
}



// scanEscape ()
//
// This routine is called after we have gotten a back-slash.  It
// reads whatever characters follow and returns the character.  If
// problems arise it prints a message and returns '?'.  If EOF is
// encountered, it prints a message, calls unGetChar(EOF) and returns '?'.
//
int scanEscape () {
  int ch, ch2, i, j;
  ch2 = getNextChar ();
  if (ch2 == '\n') {
    lexError ("End-of-line encountered after a \\ escape");
    incrLineNumber ();
    return '?';
  }
  if (ch2 == EOF) {
    lexError ("End-of-file encountered after a \\ escape");
    unGetChar (ch2);
    return '?';
  }
  i = isEscape(ch2);
  if (i != -1) {
    return i;
  } else if (ch2 == 'x') {
    ch = getNextChar ();  // Get 1st hex digit
    if (ch == '\n') {
      lexError ("End-of-line encountered after a \\x escape");
      incrLineNumber ();
      return '?';
    }
    if (ch == EOF) {
      lexError ("End-of-file encountered after a \\x escape");
      unGetChar (ch);
      return '?';
    }
    i = hexCharToInt (ch);
    if (i < 0) {
      lexError ("Must have a hex digit after \\x");
      return '?';
    } else {
      ch2 = getNextChar ();
      if (ch2 == '\n') {
        lexError ("End-of-line encountered after a \\x escape");
        incrLineNumber ();
        return '?';
      }
      if (ch2 == EOF) {
        lexError ("End-of-file encountered after a \\x escape");
        unGetChar (ch2);
        return '?';
      }
      j = hexCharToInt (ch2);
      if (j < 0) {
        lexError ("Must have two hex digits after \\x");
        return '?';
      }
      return (i<<4) + j;
    }
  } else {
    lexError ("Illegal escape (only \\0, \\a, \\b, \\t, \\n, \\v, \\f, \\r, \\\", \\\', \\\\, and \\xHH allowed)");
    return '?';
  }
}
    
    
    
// isOpChar (char)
//
// Return true if this char is one of...
//     +  -  *  \  /  !  @  #  $  %  ^  &  ~  `  |  ?  <  >  =
// 
int isOpChar (char ch) {
  if (ch == '+') {
    return 1;
  } else if (ch == '-') { 
    return 1;
  } else if (ch == '*') { 
    return 1;
  } else if (ch == '/') { 
    return 1;
  } else if (ch == '\\') { 
    return 1;
  } else if (ch == '!') { 
    return 1;
  } else if (ch == '@') { 
    return 1;
  } else if (ch == '#') { 
    return 1;
  } else if (ch == '$') { 
    return 1;
  } else if (ch == '%') { 
    return 1;
  } else if (ch == '^') { 
    return 1;
  } else if (ch == '&') { 
    return 1;
  } else if (ch == '~') { 
    return 1;
  } else if (ch == '`') { 
    return 1;
  } else if (ch == '|') { 
    return 1;
  } else if (ch == '?') { 
    return 1;
  } else if (ch == '<') { 
    return 1;
  } else if (ch == '>') { 
    return 1;
  } else if (ch == '=') { 
    return 1;
  } else {
    return 0;
  }
}



// addToHash (int i)
//
// This routine is passed an integer.  It adds it into the running "hashVal".
// The "hashVal" is initialized before each file is processed.  Every token is used to
// modify this value (by calling this routine) so that, after completely processing a file,
// the resulting "hashVal" will be a psuedo-random number, dependent on exactly what tokens
// were in the file and what order they were in.
//
// The idea is that any change to the tokens will almost certainly alter the computed
// "hashVal".  Comments and formatting changes, however, will not change the value.  This
// will be used later to check to make sure that compiled versions of files are consistent
// and that the header files have not been changed.  This technique is not fool-proof
// and should not be relied on; the programmer should make sure that header files are
// not changed between the several compilations that go into building an executable.
// Nevertheless, with this technique there is a reasonably good chance that a change
// to a header file will be detected and a runtime error can be signaled during
// program initialization.
//
void addToHash (int i) {
                // printf ("     Starting:     0x%08x\n", hashVal);
                // printf ("       i:            0x%08x   (%d)\n", i, i);
  // Rotate the current hashVal 1 bit to the left.
  if (hashVal < 0) {
    hashVal = (hashVal << 1) + 1;
  } else {
    hashVal = hashVal << 1;
  }
                // printf ("       after <<:     0x%08x\n", hashVal);
  // Add the current integer in by xor-ing it.
  hashVal = hashVal ^ i;
                // printf ("       After xor:    0x%08x\n", hashVal);
  // Next use the sequence number to add more info into the hashVal.
  hashCount = hashCount + 1;
  hashVal = hashVal ^ (hashCount * i);
                // printf ("       hashCount:    0x%08x\n", hashCount);
                // printf ("       hashCount*i:  0x%08x\n", hashCount*i);
                // printf ("       Ending:       0x%08x\n", hashVal);
}



// addTokenToHash (token)
//
// This routine is passed a token.  It adds info about that token into the running
// hashVal computation.
//
void addTokenToHash (Token token) {
  int i;
  int * p;
  String * str;
  addToHash (token.type);
  switch (token.type) {
    case ID:
    case OPERATOR:
    case STRING_CONST:
      str = token.value.svalue;
      for (i=0; i<str->length; i++) {
        addToHash (str->chars [i]);
      }
      return;
    case CHAR_CONST:
    case INT_CONST:
      addToHash(token.value.ivalue);
      return;
    case DOUBLE_CONST:
      p = (int *) (& token.value.rvalue);
      addToHash(*p);
      p++;
      addToHash(*p);
      return;
  }
}



// createTokenPos () --> int
//
// This routine takes the following:
//   currentInputFileIndex  (FF = 0..255)
//   currentLineOfToken     (LLLL = 0..65535)
//   currentCharPosOfToken  (PP = 0..255)
// and combines them into one 32-bit integer, as follows:
//   FFLLLLPP
//
int createTokenPos () {
  return
    ((currentInputFileIndex & 0x000000ff) << 24) |
    ((currentLineOfToken & 0x0000ffff) << 8) |
    (currentCharPosOfToken & 0x000000ff);
}





// incrLineNumber ()
//
// This routine increments "currentLineOfToken".  The largest value (65535) is sticky.
//
void incrLineNumber () {
  if (currentLineOfToken >= 65535) {
    currentLineOfToken = 65535;
  } else {
    currentLineOfToken++;
  } 
  currentCharPosOfToken = 0;
  return;
}



// getNextChar () --> char
//
// This routine reads and returns the next character from the input file.
// It also increments "currentCharPosOfToken".  The largest value (255) is sticky.
//
int getNextChar () {
  if (currentCharPosOfToken >= 255) {
    currentCharPosOfToken = 255;
  } else {
    currentCharPosOfToken++;
  }
  return getc (inputFile);
}



// unGetChar (ch)
//
// This routine returns the given character to the input buffer.  It also
// decrements "currentCharPosOfToken".  The largest value (255) is sticky.
//   
void unGetChar (char ch) {
  if (currentCharPosOfToken >= 255) {
    currentCharPosOfToken = 255;
  } else {        
    currentCharPosOfToken--;
  }    
  ungetc (ch, inputFile);
  return;
}   



// addToInputFileNames (filename)
//
// This routine is passed a filename.  It addes it to the array "inputFileNames".
// The global variable "currentInputIndex" is:
//    -1 = initial value
//     0 = first file
//     1 = second file
//     ...
//     255 = max value, indicating too many files
// We keep an array of pointers to the file names.  Within the token, we store only
// an 8 bit value, which is used to index into this array.  This value can range from
// 0 to 255, but 255 is used to represent problems, and will be <filename not available>.
// The value 255 is sticky; note that MAX_INPUT_FILES = 255.
//
void addToInputFilenames (const char * filename) {
  // printf ("addToInputFilenames: %s\n", filename);
  // printf ("currentInputFileIndex, before: %d\n", currentInputFileIndex);
  if (currentInputFileIndex >= MAX_INPUT_FILES-1) {
    fprintf (stderr, "%s:0: *****  LEXICAL ERROR: Maximum number of input files (%d) exceeded\n", filename, MAX_INPUT_FILES);
    errorsDetected++;
    currentInputFileIndex = MAX_INPUT_FILES;
    inputFileNames [currentInputFileIndex] = (char *) "<filename NOT available>";
  } else {
    currentInputFileIndex++;
    inputFileNames [currentInputFileIndex] = (char *) filename;
  }
  // printf ("currentInputFileIndex, after: %d\n", currentInputFileIndex);
}
