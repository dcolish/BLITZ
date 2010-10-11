#include "tokens.h"


// extractLineNumber (token) --> int
//
// Given a token, this routine returns the line number.
//
// This routine is not reliable for files of size >= 65535.
//

int extractLineNumber (Token token) {
  int i = (token.tokenPos & 0x00ffff00) >> 8;
  if (i >= 65535) {
    return -1;
  } else {
    return i;
  }
}



// extractCharPos (token) --> int
//
// Given a token, this routine returns the character position within the line.
//
int extractCharPos (Token token) {
  int i = token.tokenPos & 0x000000ff;
  if (i >= 255) {
    return -1;
  } else {
    return i;
  }
}


// symbolName (s)
//
// This routine is passed a symbol such as BOOL.  It returns a pointer
// to a string of characters, such as "BOOL".
//
const char * symbolName (int i) {
  switch (i) {
    case EOF:			return "EOF";
    case ID:			return "ID";
    case INT_CONST:		return "INT_CONST";
    case DOUBLE_CONST:		return "DOUBLE_CONST";
    case CHAR_CONST:		return "CHAR_CONST";
    case STRING_CONST:		return "STRING_CONST";
    case OPERATOR:		return "OPERATOR";

    // keywords
    case ALLOC:			return "ALLOC";
    case ANY_TYPE:		return "ANY_TYPE";
    case ARRAY:			return "ARRAY";
    case ARRAY_SIZE:		return "ARRAY_SIZE";
    case AS_INTEGER:		return "AS_INTEGER";
    case AS_PTR_TO:		return "AS_PTR_TO";
    case BEHAVIOR:		return "BEHAVIOR";
    case BOOL:			return "BOOL";
    case BREAK:			return "BREAK";
    case BY:			return "BY";
    case CASE:			return "CASE";
    case CATCH:			return "CATCH";
    case CHAR:			return "CHAR";
    case CLASS:			return "CLASS";
    case CODE:			return "CODE";
    case CONST:			return "CONST";
    case CONTINUE:		return "CONTINUE";
    case DEBUG:			return "DEBUG";
    case DEFAULT:		return "DEFAULT";
    case DO:			return "DO";
    case DOUBLE:		return "DOUBLE";
    case ELSE:			return "ELSE";
    case ELSE_IF:		return "ELSE_IF";
    case END_BEHAVIOR:		return "END_BEHAVIOR";
    case END_CLASS:		return "END_CLASS";
    case END_CODE:		return "END_CODE";
    case END_FOR:		return "END_FOR";
    case END_FUNCTION:		return "END_FUNCTION";
    case END_HEADER:		return "END_HEADER";
    case END_IF:		return "END_IF";
    case END_INTERFACE:		return "END_INTERFACE";
    case END_METHOD:		return "END_METHOD";
    case END_RECORD:		return "END_RECORD";
    case END_SWITCH:		return "END_SWITCH";
    case END_TRY:		return "END_TRY";
    case END_WHILE:		return "END_WHILE";
    case ENUM:			return "ENUM";
    case ERRORS:		return "ERRORS";
    case EXTENDS:		return "EXTENDS";
    case EXTERNAL:		return "EXTERNAL";
    case FALSE:			return "FALSE";
    case FIELDS:		return "FIELDS";
    case FOR:			return "FOR";
    case FREE:			return "FREE";
    case FUNCTION:		return "FUNCTION";
    case FUNCTIONS:		return "FUNCTIONS";
    case HEADER:		return "HEADER";
    case IF:			return "IF";
    case IMPLEMENTS:		return "IMPLEMENTS";
    case INFIX:			return "INFIX";
    case INT:			return "INT";
    case INTERFACE:		return "INTERFACE";
    case IS_INSTANCE_OF:	return "IS_INSTANCE_OF";
    case IS_KIND_OF:		return "IS_KIND_OF";
    case MESSAGES:		return "MESSAGES";
    case METHOD:		return "METHOD";
    case METHODS:		return "METHODS";
    case NEW:			return "NEW";
    case NULL_KEYWORD:		return "NULL";
    case OF:			return "OF";
    case PREFIX:		return "PREFIX";
    case PTR:			return "PTR";
    case RECORD:		return "RECORD";
    case RENAMING:		return "RENAMING";
    case RETURN:		return "RETURN";
    case RETURNS:		return "RETURNS";
    case SELF:			return "SELF";
    case SIZE_OF:		return "SIZE_OF";
    case SUPER:			return "SUPER";
    case SUPER_CLASS:		return "SUPER_CLASS";
    case SWITCH:		return "SWITCH";
    case THROW:			return "THROW";
    case TO:			return "TO";
    case TRUE:			return "TRUE";
    case TRY:			return "TRY";
    case TYPE:			return "TYPE";
    case TYPE_OF_NULL:		return "TYPE_OF_NULL";
    case UNTIL:			return "UNTIL";
    case USES:			return "USES";
    case VAR:			return "VAR";
    case VOID:			return "VOID";
    case WHILE:			return "WHILE";

    // Punctuation tokens
    case L_PAREN:		return "L_PAREN";
    case R_PAREN:		return "R_PAREN";
    case L_BRACE:		return "L_BRACE";
    case R_BRACE:		return "R_BRACE";
    case L_BRACK:		return "L_BRACK";
    case R_BRACK:		return "R_BRACK";
    case COLON:			return "COLON";
    case SEMI_COLON:		return "SEMI_COLON";
    case COMMA:			return "COMMA";
    case PERIOD:		return "PERIOD";
    case EQUAL:			return "EQUAL";

    // Symbols describing nodes in the Abstract Syntax Tree
    case ALLOC_EXPR:		return "ALLOC_EXPR";
//  case ANY_TYPE:		return "ANY_TYPE";
    case ARGUMENT:		return "ARGUMENT";
    case ARRAY_ACCESS:		return "ARRAY_ACCESS";
    case ARRAY_SIZE_EXPR:	return "ARRAY_SIZE_EXPR";
    case ARRAY_TYPE:		return "ARRAY_TYPE";
    case AS_INTEGER_EXPR:	return "AS_INTEGER_EXPR";
    case AS_PTR_TO_EXPR:	return "AS_PTR_TO_EXPR";
    case ASSIGN_STMT:		return "ASSIGN_STMT";
//  case BEHAVIOR:		return "BEHAVIOR";
    case BOOL_CONST:		return "BOOL_CONST";
    case BOOL_TYPE:		return "BOOL_TYPE";
    case BREAK_STMT:		return "BREAK_STMT";
    case CALL_EXPR:		return "CALL_EXPR";
    case CALL_STMT:		return "CALL_STMT";
//  case CASE:			return "CASE";
//  case CATCH:			return "CATCH";
//  case CHAR_CONST:		return "CHAR_CONST";
    case CHAR_TYPE:		return "CHAR_TYPE";
    case CLASS_DEF:		return "CLASS_DEF";
    case CLASS_FIELD:		return "CLASS_FIELD";
    case CLOSURE_EXPR:		return "CLOSURE_EXPR";
//  case CODE:			return "CODE";
    case CONST_DECL:		return "CONST_DECL";
    case CONSTRUCTOR:		return "CONSTRUCTOR";
    case CONTINUE_STMT:		return "CONTINUE_STMT";
    case COUNT_VALUE:		return "COUNT_VALUE";
    case DO_STMT:		return "DO_STMT";
//  case DOUBLE_CONST:		return "DOUBLE_CONST";
    case DOUBLE_TYPE:		return "DOUBLE_TYPE";
    case DYNAMIC_CHECK:		return "DYNAMIC_CHECK";
    case ERROR_DECL:		return "ERROR_DECL";
    case FIELD_ACCESS:		return "FIELD_ACCESS";
    case FIELD_INIT:		return "FIELD_INIT";
    case FOR_STMT:		return "FOR_STMT";
//  case FUNCTION:		return "FUNCTION";
    case FUNCTION_PROTO:	return "FUNCTION_PROTO";
    case FUNCTION_TYPE:		return "FUNCTION_TYPE";
    case GLOBAL:		return "GLOBAL";
//  case HEADER:		return "HEADER";
    case IF_STMT:		return "IF_STMT";
//  case INT_CONST:		return "INT_CONST";
    case INT_TYPE:		return "INT_TYPE";
//  case INTERFACE:		return "INTERFACE";
    case IS_INSTANCE_OF_EXPR:	return "IS_INSTANCE_OF_EXPR";
    case IS_KIND_OF_EXPR:	return "IS_KIND_OF_EXPR";
    case LOCAL:			return "LOCAL";
//  case METHOD:		return "METHOD";
    case METHOD_PROTO:		return "METHOD_PROTO";
    case NAMED_TYPE:		return "NAMED_TYPE";
    case NEW_EXPR:		return "NEW_EXPR";
    case NULL_CONST:		return "NULL_CONST";
    case PARAMETER:		return "PARAMETER";
    case PTR_TYPE:		return "PTR_TYPE";
    case RECORD_FIELD:		return "RECORD_FIELD";
    case RECORD_TYPE:		return "RECORD_TYPE";
    case RETURN_STMT:		return "RETURN_STMT";
    case SELF_EXPR:		return "SELF_EXPR";
    case SEND_EXPR:		return "SEND_EXPR";
    case SEND_STMT:		return "SEND_STMT";
    case SIZE_OF_EXPR:		return "SIZE_OF_EXPR";
//  case STRING_CONST:		return "STRING_CONST";
    case SUPER_EXPR:		return "SUPER_EXPR";
    case SWITCH_STMT:		return "SWITCH_STMT";
    case THROW_STMT:		return "THROW_STMT";
    case TRY_STMT:		return "TRY_STMT";
    case TYPE_ARG:		return "TYPE_ARG";
    case TYPE_DEF:		return "TYPE_DEF";
    case TYPE_OF_NULL_TYPE:	return "TYPE_OF_NULL_TYPE";
    case TYPE_PARM:		return "TYPE_PARM";
//  case USES:			return "USES";
//  case RENAMING:		return "RENAMING";
    case VARIABLE_EXPR:		return "VARIABLE_EXPR";
    case VOID_TYPE:		return "VOID_TYPE";
    case WHILE_STMT:		return "WHILE_STMT";
    case FREE_STMT:		return "FREE_STMT";
    case DEBUG_STMT:		return "DEBUG_STMT";

    //  Symbols used to identify build-in function ids, and mess selectors
    case DOUBLE_TO_INT:		return "DOUBLE_TO_INT";
    case INT_TO_DOUBLE:		return "INT_TO_DOUBLE";
    case INT_TO_CHAR:		return "INT_TO_CHAR";
    case CHAR_TO_INT:		return "CHAR_TO_INT";
    case PTR_TO_BOOL:		return "PTR_TO_BOOL";
    case POS_INF:		return "POS_INF";
    case NEG_INF:		return "NEG_INF";
    case NEG_ZERO:		return "NEG_ZERO";
    case I_IS_ZERO:		return "I_IS_ZERO";
    case I_NOT_ZERO:		return "I_NOT_ZERO";
    case BAR:			return "BAR";
    case CARET:			return "CARET";
    case AMP:			return "AMP";
    case BAR_BAR:		return "BAR_BAR";
    case AMP_AMP:		return "AMP_AMP";
    case EQUAL_EQUAL:		return "EQUAL_EQUAL";
    case NOT_EQUAL:		return "NOT_EQUAL";
    case LESS:			return "LESS";
    case LESS_EQUAL:		return "LESS_EQUAL";
    case GREATER:		return "GREATER";
    case GREATER_EQUAL:		return "GREATER_EQUAL";
    case LESS_LESS:		return "LESS_LESS";
    case GREATER_GREATER:	return "GREATER_GREATER";
    case GREATER_GREATER_GREATER:	return "GREATER_GREATER_GREATER";
    case PLUS:			return "PLUS";
    case MINUS:			return "MINUS";
    case STAR:			return "STAR";
    case SLASH:			return "SLASH";
    case PERCENT:		return "PERCENT";

    case UNARY_BANG:		return "UNARY_BANG";
    case UNARY_STAR:		return "UNARY_STAR";
    case UNARY_AMP:		return "UNARY_AMP";
    case UNARY_MINUS:		return "UNARY_MINUS";

    // Symbols identifying primitive functions
    case PRIMITIVE_I_ADD:		return "PRIMITIVE_I_ADD";
    case PRIMITIVE_I_SUB:		return "PRIMITIVE_I_SUB";
    case PRIMITIVE_I_MUL:		return "PRIMITIVE_I_MUL";
    case PRIMITIVE_I_DIV:		return "PRIMITIVE_I_DIV";
    case PRIMITIVE_I_REM:		return "PRIMITIVE_I_REM";
    case PRIMITIVE_I_SLL:		return "PRIMITIVE_I_SLL";
    case PRIMITIVE_I_SRA:		return "PRIMITIVE_I_SRA";
    case PRIMITIVE_I_SRL:		return "PRIMITIVE_I_SRL";
    case PRIMITIVE_I_LT:		return "PRIMITIVE_I_LT";
    case PRIMITIVE_I_LE:		return "PRIMITIVE_I_LE";
    case PRIMITIVE_I_GT:		return "PRIMITIVE_I_GT";
    case PRIMITIVE_I_GE:		return "PRIMITIVE_I_GE";
    case PRIMITIVE_I_EQ:		return "PRIMITIVE_I_EQ";
    case PRIMITIVE_I_NE:		return "PRIMITIVE_I_NE";
    case PRIMITIVE_I_AND:		return "PRIMITIVE_I_AND";
    case PRIMITIVE_I_OR:		return "PRIMITIVE_I_OR";
    case PRIMITIVE_I_XOR:		return "PRIMITIVE_I_XOR";
    case PRIMITIVE_I_NOT:		return "PRIMITIVE_I_NOT";
    case PRIMITIVE_I_NEG:		return "PRIMITIVE_I_NEG";
    case PRIMITIVE_I_IS_ZERO:		return "PRIMITIVE_I_IS_ZERO";
    case PRIMITIVE_I_NOT_ZERO:		return "PRIMITIVE_I_NOT_ZERO";
    case PRIMITIVE_D_ADD:		return "PRIMITIVE_D_ADD";
    case PRIMITIVE_D_SUB:		return "PRIMITIVE_D_SUB";
    case PRIMITIVE_D_MUL:		return "PRIMITIVE_D_MUL";
    case PRIMITIVE_D_DIV:		return "PRIMITIVE_D_DIV";
    case PRIMITIVE_D_LT:		return "PRIMITIVE_D_LT";
    case PRIMITIVE_D_LE:		return "PRIMITIVE_D_LE";
    case PRIMITIVE_D_GT:		return "PRIMITIVE_D_GT";
    case PRIMITIVE_D_GE:		return "PRIMITIVE_D_GE";
    case PRIMITIVE_D_EQ:		return "PRIMITIVE_D_EQ";
    case PRIMITIVE_D_NE:		return "PRIMITIVE_D_NE";
    case PRIMITIVE_D_NEG:		return "PRIMITIVE_D_NEG";
    case PRIMITIVE_B_AND:		return "PRIMITIVE_B_AND";
    case PRIMITIVE_B_OR:		return "PRIMITIVE_B_OR";
    case PRIMITIVE_B_NOT:		return "PRIMITIVE_B_NOT";
    case PRIMITIVE_B_EQ:		return "PRIMITIVE_B_EQ";
    case PRIMITIVE_B_NE:		return "PRIMITIVE_B_NE";
    case PRIMITIVE_OBJECT_EQ:		return "PRIMITIVE_OBJECT_EQ";
    case PRIMITIVE_OBJECT_NE:		return "PRIMITIVE_OBJECT_NE";
    case PRIMITIVE_DEREF:		return "PRIMITIVE_DEREF";
    case PRIMITIVE_ADDRESS_OF:		return "PRIMITIVE_ADDRESS_OF";

    // Symbols identifying primitive functions
    case PRIMITIVE_INT_TO_DOUBLE:	return "PRIMITIVE_INT_TO_DOUBLE";
    case PRIMITIVE_DOUBLE_TO_INT:	return "PRIMITIVE_DOUBLE_TO_INT";
    case PRIMITIVE_INT_TO_CHAR:		return "PRIMITIVE_INT_TO_CHAR";
    case PRIMITIVE_CHAR_TO_INT:		return "PRIMITIVE_CHAR_TO_INT";
    case PRIMITIVE_PTR_TO_BOOL:		return "PRIMITIVE_PTR_TO_BOOL";
    case PRIMITIVE_POS_INF:		return "PRIMITIVE_POS_INF";
    case PRIMITIVE_NEG_INF:		return "PRIMITIVE_NEG_INF";
    case PRIMITIVE_NEG_ZERO:		return "PRIMITIVE_NEG_ZERO";

//    case PRIMITIVE_xxx:	return "yyy";

    // Misc Symbols
    case KEYWORD:		return "KEYWORD";
    case NORMAL:		return "NORMAL";

    default:
      printf ("\nSYMBOL = %d\n", i);
      return "*****  unknown symbol  *****";
  }
}



// printSymbol (int)
//
// Print this symbol out.  Used for debugging.
//
void printSymbol (int sym) {
  printf ("*****  Symbol = %s  *****\n", symbolName (sym));
}
