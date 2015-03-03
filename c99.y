// Taken from <http://www.quut.com/c/ANSI-C-grammar-y-1999.html>
%token IDENTIFIER TYPE_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER INLINE RESTRICT
%token CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE CONST VOLATILE VOID
%token BOOL COMPLEX IMAGINARY
%token STRUCT UNION ENUM

%start parameter_list
%%

declaration_specifiers
	: storage_class_specifier
	| storage_class_specifier declaration_specifiers
	| type_specifier
	| type_specifier declaration_specifiers
	| type_qualifier
	| type_qualifier declaration_specifiers
	| function_specifier
	| function_specifier declaration_specifiers
	;

storage_class_specifier
	: TYPEDEF
	| EXTERN
	| STATIC
	| AUTO
	| REGISTER
	;

type_specifier
	: VOID
	| CHAR
	| SHORT
	| INT
	| LONG
	| FLOAT
	| DOUBLE
	| SIGNED
	| UNSIGNED
	| BOOL
	| COMPLEX
	| IMAGINARY
        | STRUCT IDENTIFIER
        | UNION IDENTIFIER
        | ENUM IDENTIFIER
	| TYPE_NAME
	;

type_qualifier
	: CONST
	| RESTRICT
	| VOLATILE
	;

function_specifier
	: INLINE
	;

declarator
	: pointer direct_declarator
	| direct_declarator
	;

direct_declarator
	: IDENTIFIER
	| '(' declarator ')'
	// | direct_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_declarator '[' type_qualifier_list ']'
	// | direct_declarator '[' assignment_expression ']'
	// | direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	// | direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list '*' ']'
	| direct_declarator '[' '*' ']'
	| direct_declarator '[' ']'
	| direct_declarator '(' parameter_list ')'
	| direct_declarator '(' ')'
	;

pointer
	: '*'
	| '*' type_qualifier_list
	| '*' pointer
	| '*' type_qualifier_list pointer
	;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;

parameter_list
	: parameter_declaration
	| parameter_list ',' parameter_declaration
	;

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers abstract_declarator
	| declaration_specifiers
	;

abstract_declarator
	: pointer
	| direct_abstract_declarator
	| pointer direct_abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| '[' ']'
	// | '[' assignment_expression ']'
	| direct_abstract_declarator '[' ']'
	// | direct_abstract_declarator '[' assignment_expression ']'
	| '[' '*' ']'
	| direct_abstract_declarator '[' '*' ']'
	| '(' ')'
	| '(' parameter_list ')'
	| direct_abstract_declarator '(' ')'
	| direct_abstract_declarator '(' parameter_list ')'
	;


%%
#include <stdio.h>

extern char yytext[];
extern int column;

void yyerror(char const *s)
{
	fflush(stdout);
	printf("\n%*s\n%*s\n", column, "^", column, s);
}
