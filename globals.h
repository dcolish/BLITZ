#ifndef GLOBALS_H
#define GLOBALS_H

#include<stdio.h>
#include "tokens.h"

#define TOKEN_SKIP_COUNT 1
#define MAX_STR_LEN 2000 // Strings, IDs are limited to 2000 chars.
#define MAX_NUMBER_OF_ERRORS 100 // terminate after this many error messages

#define MAX_INPUT_FILES 255 // Do not change; related to 8 bits
#define STRING_TABLE_HASH_SIZE 211 // Size of hash table for string table.

extern char * outputFileName; // The .s filename, NULL = missing
extern FILE * outputFile; // The output file, e.g., stdout
extern char * inputFileNames [MAX_INPUT_FILES+1]; // Array of ptrs to file names

extern String * stringTableIndex [STRING_TABLE_HASH_SIZE];

#endif
