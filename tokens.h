#ifndef TOKENS_H
#define TOKENS_H

enum {
       // Misc token types
       ID=257, INT_CONST, DOUBLE_CONST, CHAR_CONST, STRING_CONST, OPERATOR,

       // Keywords
       ALLOC, ANY_TYPE, ARRAY, ARRAY_SIZE, AS_INTEGER, AS_PTR_TO, BEHAVIOR, BOOL,
       BREAK, BY, CASE, CATCH, CHAR, CLASS, CODE,
       CONST, CONTINUE, DEBUG, DEFAULT, DO, DOUBLE, ELSE, ELSE_IF,
       END_BEHAVIOR, END_CLASS, END_CODE, END_FOR, END_FUNCTION, END_HEADER, END_IF,
       END_INTERFACE, END_METHOD, END_RECORD, END_SWITCH, END_TRY, END_WHILE,
       ENUM, ERRORS, EXTENDS,
       EXTERNAL, FALSE, FIELDS, FOR, FREE, FUNCTION, FUNCTIONS,
       HEADER, IF, IMPLEMENTS, INFIX, INT,
       INTERFACE, IS_INSTANCE_OF, IS_KIND_OF,
       MESSAGES, METHOD, METHODS, NEW, NULL_KEYWORD,
       OF, PREFIX, PTR, RECORD, RENAMING, RETURN, RETURNS, SELF, SIZE_OF, SUPER, SUPER_CLASS,
       SWITCH, THROW, TO, TRUE, TRY, TYPE, TYPE_OF_NULL, UNTIL, USES, VAR, VOID, WHILE,

       // Punctuation tokens
       L_PAREN, R_PAREN, L_BRACE, R_BRACE, L_BRACK, R_BRACK,
       COLON, SEMI_COLON, COMMA, PERIOD, EQUAL,

       // Symbols describing nodes in the Abstract Syntax Tree
       ALLOC_EXPR, ARGUMENT, ARRAY_ACCESS,
       ARRAY_SIZE_EXPR, ARRAY_TYPE, AS_INTEGER_EXPR,
       AS_PTR_TO_EXPR, ASSIGN_STMT, BOOL_CONST,
       BOOL_TYPE, BREAK_STMT, CALL_EXPR, CALL_STMT,
       CHAR_TYPE, CLASS_DEF, CLASS_FIELD,
       CLOSURE_EXPR, CONST_DECL, CONSTRUCTOR,
       CONTINUE_STMT, COUNT_VALUE, DO_STMT, DOUBLE_TYPE, DYNAMIC_CHECK,
       ERROR_DECL, FIELD_ACCESS, FIELD_INIT, FOR_STMT,
       FUNCTION_PROTO, FUNCTION_TYPE, GLOBAL, IF_STMT,
       INT_TYPE, IS_INSTANCE_OF_EXPR, IS_KIND_OF_EXPR, LOCAL,
       METHOD_PROTO, NAMED_TYPE, NEW_EXPR, NULL_CONST, PARAMETER,
       PTR_TYPE, RECORD_FIELD, RECORD_TYPE, RETURN_STMT,
       SELF_EXPR, SEND_EXPR, SEND_STMT, SIZE_OF_EXPR,
       SUPER_EXPR, SWITCH_STMT, THROW_STMT,
       TRY_STMT, TYPE_ARG, TYPE_DEF, TYPE_OF_NULL_TYPE,
       TYPE_PARM, VARIABLE_EXPR, VOID_TYPE, WHILE_STMT, FREE_STMT, DEBUG_STMT,

       // AstNode types, which are also keywords
       // BEHAVIOR, CASE, CATCH, CHAR_CONST, CODE, DOUBLE_CONST,
       // FUNCTION, HEADER, INT_CONST, INTERFACE, METHOD, STRING_CONST,
       // USES, RENAMING,

       // Symbols used to identify build-in function ids, and message selectors
       INT_TO_DOUBLE, DOUBLE_TO_INT, INT_TO_CHAR, CHAR_TO_INT,
       PTR_TO_BOOL, POS_INF, NEG_INF, NEG_ZERO,
       I_IS_ZERO, I_NOT_ZERO,

       PLUS, MINUS, STAR, SLASH, PERCENT,
       BAR, CARET, AMP, BAR_BAR, AMP_AMP,
       EQUAL_EQUAL, NOT_EQUAL, LESS, LESS_EQUAL, GREATER, GREATER_EQUAL,
       LESS_LESS, GREATER_GREATER, GREATER_GREATER_GREATER,

       UNARY_BANG, UNARY_STAR, UNARY_AMP, UNARY_MINUS,

       // Symbols identifying primitive messages
       PRIMITIVE_I_ADD,
       PRIMITIVE_I_SUB,
       PRIMITIVE_I_MUL,
       PRIMITIVE_I_DIV,
       PRIMITIVE_I_REM,
       PRIMITIVE_I_SLL,
       PRIMITIVE_I_SRA,
       PRIMITIVE_I_SRL,
       PRIMITIVE_I_LT,
       PRIMITIVE_I_LE,
       PRIMITIVE_I_GT,
       PRIMITIVE_I_GE,
       PRIMITIVE_I_EQ,
       PRIMITIVE_I_NE,
       PRIMITIVE_I_AND,
       PRIMITIVE_I_OR,
       PRIMITIVE_I_XOR,
       PRIMITIVE_I_NOT,
       PRIMITIVE_I_NEG,
       PRIMITIVE_I_IS_ZERO,
       PRIMITIVE_I_NOT_ZERO,

       PRIMITIVE_D_ADD,
       PRIMITIVE_D_SUB,
       PRIMITIVE_D_MUL,
       PRIMITIVE_D_DIV,
       PRIMITIVE_D_LT,
       PRIMITIVE_D_LE,
       PRIMITIVE_D_GT,
       PRIMITIVE_D_GE,
       PRIMITIVE_D_EQ,
       PRIMITIVE_D_NE,
       PRIMITIVE_D_NEG,

       PRIMITIVE_B_AND,
       PRIMITIVE_B_OR,
       PRIMITIVE_B_NOT,
       PRIMITIVE_B_EQ,
       PRIMITIVE_B_NE,

       PRIMITIVE_OBJECT_EQ,
       PRIMITIVE_OBJECT_NE,
       PRIMITIVE_DEREF,
       PRIMITIVE_ADDRESS_OF,

       // Symbols identifying primitive functions
       PRIMITIVE_INT_TO_DOUBLE,
       PRIMITIVE_DOUBLE_TO_INT,
       PRIMITIVE_INT_TO_CHAR,
       PRIMITIVE_CHAR_TO_INT,
       PRIMITIVE_PTR_TO_BOOL,
       PRIMITIVE_POS_INF,
       PRIMITIVE_NEG_INF,
       PRIMITIVE_NEG_ZERO,

       // Misc symbols
       KEYWORD, NORMAL,

};

//----------  String Table  ----------

class String {
  public:
    String       * next;           // Link list of Strings
    int          type;             // E.g., ID or BOOL, BREAK, ...
    int          primitiveSymbol;  // 0=not a primitive; e.g., PLUS, DOUBLE_TO_INT, ...
    int          length;           // The length and characters
    char         chars [];
};



//----------  Token  ----------

union TokenValue {
  String * svalue;
  int      ivalue;
  double   rvalue;
};

struct Token {
  int        type;
  TokenValue value;
  int        tokenPos;
};

#endif
