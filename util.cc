#include "util.h"

// appendStrings (char *, char *, char *)
//
// Allocate and a new char array and fill it in from the
// characters in the strings.  Return a pointer to it.
//
char * appendStrings (const char * str1, const char * str2, const char * str3) {
  int len = strlen (str1) + strlen (str2) + strlen (str3);
  char * newStr, * to, * from ;
  newStr = (char *) calloc (1, len+1);
  to = newStr;
  for (from=(char *)str1; *from != 0; to++, from++) {
    *to = *from;
  }
  for (from=(char *)str2; *from != 0; to++, from++) {
    *to = *from;
  }
  for (from=(char *)str3; *from != 0; to++, from++) {
    *to = *from;
  }
  *to = 0;
  return newStr;
}



// hexCharToInt (char)
//
// This routine is passed a character. If it is a hex digit, i.e.,
//    0, 1, 2, ... 9, a, b, ... f, A, B, ... F
// then it returns its value (0..15).  Otherwise, it returns -1.
//
int hexCharToInt (char ch) {
  if (('0'<=ch) && (ch<='9')) {
    return ch-'0';
  } else if (('a'<=ch) && (ch<='f')) {
    return ch-'a' + 10;
  } else if (('A'<=ch) && (ch<='F')) {
    return ch-'A' + 10;
  } else {
    return -1;
  }
}



// divide (a, b)
//
// This routine is passed two integers ("a" and "b").  It divides a by b
// to get a quotient ("q") and remainder ("r"), such that
//
//       a = b*q + r
//
// Furthermore, the remainder follows the mathematical definition of the
// "modulo" operator, namely that the remainder will have the same sign
// as b and that
//
//       0 <= abs(r) < abs(b)
//
// Another way to look at this is that the quotient is the real quotient,
// rounded down to the nearest integer.
//
// For example:
//
//       a   b     q   r     a =  b *  q +  r     a/b   rounded
//      ==  ==    ==  ==    =================    ====   =======
//       7   3     2   1     7 =  3 *  2 +  1     2.3      2
//      -7   3    -3   2    -7 =  3 * -3 +  2    -2.3     -3
//       7  -3    -3  -2     7 = -3 * -3 + -2    -2.3     -3
//      -7  -3     2  -1    -7 = -3 *  2 + -1     2.3      2
//
// This routine modifies global variables "qqo" and "rem".  If b=0 it
// sets q and r to zero and returns immediately.
//
// With this definition of "q" and "r", overflow can and will occur in only
// one situation.  Assuming that we are using 32-bit signed integers, the
// following inputs cause a problem...
//      a = -2147483648
//      b = -1
// The mathematically correct answer is...
//      q = +2147483648
//      r = 0
// Unfortunately, this value of q is not representable.  The underlying
// implementation of the C operators / and % will normally fail, and will
// quietly return the wrong answer...
//      q = -2147483648
//      r = 0
// This routine will simply return these incorrect values.
//
// The C language does not define the / and % operators precisely, but
// only requires that a = b*q + r be true.  This routine is designed to
// return consistent, "correct" answers, regardless of the underlying
// implementation of / and %.
//
// Typical variations in integer division are...
//
// (1) "r" is always non-negative.  0 <= r < abs(b)
//     "q" will be negative when either a or b (but not both) are negative.
//         a   b     q   r     a =  b *  q +  r
//        ==  ==    ==  ==    =================
//         7   3     2   1     7 =  3 *  2 +  1
//        -7   3    -3   2    -7 =  3 * -3 +  2
//         7  -3    -2   1     7 = -3 * -2 +  1
//        -7  -3     3   2    -7 = -3 *  3 +  2
//
// (2) Real division, rounded toward zero.
//     "q" = a/b, rounded toward zero.
//     "q" will be negative when either a or b (but not both) are negative.
//     The sign of "r" will be the same as the sign of "a".
//         a   b     q   r     a =  b *  q +  r     a/b   rounded
//        ==  ==    ==  ==    =================    ====   =======
//         7   3     2   1     7 =  3 *  2 +  1     2.3      2
//        -7   3    -2  -1    -7 =  3 * -2 + -1    -2.3     -2
//         7  -3    -2   1     7 = -3 * -2 +  1    -2.3     -2
//        -7  -3     2  -1    -7 = -3 *  2 + -1     2.3      2
//
// (3) Real division, rounded toward negative infinity.
//     "q" = a/b, rounded toward negative infinity.
//     This results in "r" being the mathematically correct "modulo".
//     "q" will be negative when either a or b (but not both) are negative.
//     "r" will be negative whenever "b" is negative.
//
// This routine implements option number (3).  It works assuming that
// the underlying C implementation uses options (1), (2), or (3).
//
// Overflow cannot occur in this routine, assuming 2's complement
// representation of integers.
//
void divide (int a, int b) {
    int quo, rem;
  if (b==0) {
    quo = rem = 0;
    return;
  }
  quo = a/b;
  rem = a%b;
  if (b>0) {
    if (rem<0) {
      quo--;          // Overflow iff q=MIN; but then b=1 and r=0... can't be.
      rem = rem + b;  // r is neg, b is pos; cannot overflow.
    }
  } else {
    if (rem>0) {
      quo--;          // Overflow iff q=MIN; but then b=1 and r=0... can't be.
      rem = rem + b;  // r is pos, b is neg; cannot overflow.
    }
  }
}

// extractFilename (token) --> char *
//
// Given a token, this routine returns the name of the file.
//
char * extractFilename (Token token) {
  int i = (token.tokenPos >> 24) & 0x000000ff;
  return inputFileNames [i];
}




// intToHexChar (int)
//
// This routine is passed an integer 0..15.  It returns a char
// from 0 1 2 3 4 5 6 7 8 9 A B C D E F
//
char intToHexChar (int i) {
  if (i<10) {
    return '0' + i;
  } else {
    return 'A' + (i-10);
  }
}



// printStringTable ()
//
// This routine runs through the string table and prints each entry.
//
void printStringTable () {
  int hashVal;
  String * stringPtr;
  printf ("LIST OF ALL STRINGS AND IDS\n");
  printf ("===========================\n");
  for (hashVal = 0; hashVal<STRING_TABLE_HASH_SIZE; hashVal++) {
    for (stringPtr = stringTableIndex [hashVal];
                       stringPtr;
                       stringPtr = stringPtr->next) {
      printString (stdout, stringPtr);
      printf ("\n");
    }
  }
}



// printString (file, str)
//
// This routine is passed a pointer to a String.  It prints the
// string of the given file, translating non-printable characters into
// escape sequences.  This routine will not print a terminating newline.
//
void printString (FILE * file, String * str) {
  int i = 0;
  if (str == NULL) {
    fprintf (file, "*****NULL-STRING*****");
    return;
  }
  for (i=0; i<str->length; i++) {
    printChar (file, str->chars [i]);
  }
}
    


// printChar (file, int)
//
// This routine is passed a char, which could be any value 0..255.
// It prints it on the given file (e.g. stdout) in a form such as
//     a   \n   \xEF   ...etc...
//
void printChar (FILE * file, int c) {
  int i, j;
  if (c == '\"') {
    fprintf (file, "\\\"");
  } else if (c == '\'') {
    fprintf (file, "\\\'");
  } else if (c == '\\') {
    fprintf (file, "\\\\");
  } else if ((c>=32) && (c<127)) {
    fprintf (file, "%c", c);
  } else if (c == '\0') {
    fprintf (file, "\\0");
  } else if (c == '\a') {
    fprintf (file, "\\a");
  } else if (c == '\b') {
    fprintf (file, "\\b");
  } else if (c == '\t') {
    fprintf (file, "\\t");
  } else if (c == '\n') {
    fprintf (file, "\\n");
  } else if (c == '\v') {
    fprintf (file, "\\v");
  } else if (c == '\f') {
    fprintf (file, "\\f");
  } else if (c == '\r') {
    fprintf (file, "\\r");
  } else {
    i = intToHexChar ((c>>4) & 0x0000000f);
    j = intToHexChar (c & 0x0000000f);
    fprintf (file, "\\x%c%c", i, j);
  }
}
