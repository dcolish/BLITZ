#ifndef PRINTAST_H
#define PRINTAST_H

void printAst (int indent, AstNode *t);
void printHeader (int indent, const char * str, AstNode * p);
void printFooter (int indent);
void printIndent (int indent);
void printLine (int indent, const char * str);
void printPtrField (int indent, const char * str, AstNode * id);
void printIntField (int indent, const char * str, int i);
void printBoolField (int indent, const char * str, int i);
void printStringField (int indent, const char * str1, String * str2);
void printSymbolField (int indent, const char * str, int sym);
void printCharPtrField (int indent, const char * str, char * charPtr);
void printId (int indent, String * id);
void printFieldName (int indent, const char * str);
void printItem (int indent, const char * s, AstNode * t);
void printOperator (int indent, int op);
int printPtr (AstNode * p);
void fpretty (Type * p);
void fpretty2 (Type * p, int wantRecursion);

#endif
