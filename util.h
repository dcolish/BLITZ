#ifndef UTIL_H
#define UTIL_H

#include <stdio.h>

#include "ast.h"
#include "globals.h"

extern String* stringTableIndex [STRING_TABLE_HASH_SIZE];

char * appendStrings (const char * str1, const char * str2, const char * str3);
void divide (int a, int b);
char * extractFilename (Token token);
int hexCharToInt (char ch);
char intToHexChar (int c);
void printStringTable ();
void printString (FILE * file, String *);
void printChar (FILE * file, int c);

#endif
