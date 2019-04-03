/*	Definition section */
%{
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define MAXYM 64

extern int yylineno;
extern int yylex();
void yyerror(char *);
/* Symbol table function - you can add new function if need. */
int lookup_symbol(char* id);
void create_symbol();
void insert_symbol(char* id, char* type, float data);
void dump_symbol();
//void assign_symbol(char* id, float data);
void pushstack(int floflag);
int popstack();
int symcnt = 0;
int floflag = 0;
int error = 0;
struct symbol {
	char id[12];
	char type[12];
	int idata;
	float fdata;
};
struct symbol table[MAXYM];
int stackcnt = 0;
int exprstack[12] = {0};
int label = 0;
int ifs = 0;
int iflag = 0;
FILE *file;
%}

/* Using union to define nonterminal and token type */
%union {
    int i_val;
    float f_val;
    char* string;
}

/* Token without return */
%token PRINT PRINTLN
%token IF ELSE FOR
%token VAR NEWLINE
%token INC DEC
%token GREEQU LESEQU EQUAL NOTEQU
%token ADDASI SUBASI MULASI DIVASI MODASI
%token CONAND CONOR

/* Token with return, which need to sepcify type */
%token <i_val> I_CONST
%token <f_val> F_CONST
%token <string> STRING ID VOID INT FLOAT

/* Nonterminal with return, which need to sepcify type */
%type <f_val> expr term constant initializer assignment 
%type <string> type

/* Priority */
%left CONOR
%left CONAND
%left EQUAL NOTEQU '<' LESEQU '>' GREEQU 
%left '+' '-'
%left '*' '/' '%'
%left INC DEC

/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%

program
    : program stat {floflag = 0; stackcnt = 0;}
    | program NEWLINE {floflag = 0; stackcnt = 0;}
    |
;

stat
    : declaration {floflag = 0; stackcnt = 0;}
    | compound_stat {floflag = 0; stackcnt = 0;}
    | expression_stat {floflag = 0; stackcnt = 0;}
    | print_func {floflag = 0; stackcnt = 0;}
;

declaration
    : VAR ID type NEWLINE
    	{
		    if(symcnt == 0)
		       create_symbol();
		    fprintf(file, "\tldc 0\n");
		    insert_symbol($2, $3, 0.0);
		    
		}
	| VAR ID type '=' initializer NEWLINE
		{
		    if(symcnt == 0)
		       create_symbol();
		    insert_symbol($2, $3, $5);
		}
;

type
    : INT { $$ = $1; }
    | FLOAT { $$ = $1; }
    | VOID { $$ = $1; }
;

initializer
	: constant {$$ = $1;}
;

compound_stat
	: for_stat
    | if_stat
;

assignment
	: ID '=' expr
		{
			int found = lookup_symbol($1);
			if(found == 0) {
				char errmsg[64] = {0};
				strcat(errmsg, "can’t find variable ");
				strcat(errmsg, $1);
				yyerror(errmsg);
			}
			else {
				if(strcmp(table[found].type, "int") == 0) {
					table[found].idata = (int)$3;
					if(popstack() == 1)
						fprintf(file, "\tf2i\n");
					fprintf(file, "\tistore %d\n", found-1);
				}
				else {
					table[found].fdata = $3;
					if(popstack() == 0)
						fprintf(file, "\ti2f\n");
					fprintf(file, "\tfstore %d\n", found-1);
				}
			}
		}
	| ID ADDASI expr
		{
			int found = lookup_symbol($1);
			if(found == 0) {
				char errmsg[64] = {0};
				strcat(errmsg, "can’t find variable ");
				strcat(errmsg, $1);
				yyerror(errmsg);
			}
			else {
				if(strcmp(table[found].type, "int") == 0) {
					table[found].idata += (int)$3;
					fprintf(file, "\tiload %d\n", found-1);
					if(popstack() == 0)
						fprintf(file, "\tiadd\n");
					else {
						fprintf(file, "\tfstore %d\n", symcnt);
						fprintf(file, "\ti2f\n");
						fprintf(file, "\tfload %d\n", symcnt);
						fprintf(file, "\tfadd\n");
						fprintf(file, "\tf2i\n");
					}
					fprintf(file, "\tistore %d\n", found-1);
				}
				else {
					table[found].fdata += $3;
					fprintf(file, "\tfload %d\n", found-1);
					if(popstack() == 0)
						fprintf(file, "\ti2f\n");
					fprintf(file, "\tfadd\n");
					fprintf(file, "\tfstore %d\n", found-1);
				}
			}
		}
	| ID SUBASI expr
		{
			int found = lookup_symbol($1);
			if(found == 0) {
				char errmsg[64] = {0};
				strcat(errmsg, "can’t find variable ");
				strcat(errmsg, $1);
				yyerror(errmsg);
			}
			else {
				if(strcmp(table[found].type, "int") == 0) {
					table[found].idata -= (int)$3;
					fprintf(file, "\tiload %d\n", found-1);
					if(popstack() == 0)
						fprintf(file, "\tisub\n");
					else {
						fprintf(file, "\tfstore %d\n", symcnt);
						fprintf(file, "\ti2f\n");
						fprintf(file, "\tfload %d\n", symcnt);
						fprintf(file, "\tfsub\n");
						fprintf(file, "\tf2i\n");
					}
					fprintf(file, "\tistore %d\n", found-1);
				}
				else {
					table[found].fdata -= $3;
					fprintf(file, "\tfload %d\n", found-1);
					if(popstack() == 0)
						fprintf(file, "\ti2f\n");
					fprintf(file, "\tfsub\n");
					fprintf(file, "\tfstore %d\n", found-1);
				}
			}
		}
	| ID MULASI expr
		{
			int found = lookup_symbol($1);
			if(found == 0) {
				char errmsg[64] = {0};
				strcat(errmsg, "can’t find variable ");
				strcat(errmsg, $1);
				yyerror(errmsg);
			}
			else {
				if(strcmp(table[found].type, "int") == 0) {
					table[found].idata *= (int)$3;
					fprintf(file, "\tiload %d\n", found-1);
					if(popstack() == 0)
						fprintf(file, "\timul\n");
					else {
						fprintf(file, "\tfstore %d\n", symcnt);
						fprintf(file, "\ti2f\n");
						fprintf(file, "\tfload %d\n", symcnt);
						fprintf(file, "\tfmul\n");
						fprintf(file, "\tf2i\n");
					}
					fprintf(file, "\tistore %d\n", found-1);
				}
				else {
					table[found].fdata *= $3;
					fprintf(file, "\tfload %d\n", found-1);
					if(popstack() == 0)
						fprintf(file, "\ti2f\n");
					fprintf(file, "\tfmul\n");
					fprintf(file, "\tfstore %d\n", found-1);
				}
			}
		}
	| ID DIVASI expr
		{
			int found = lookup_symbol($1);
			if(found == 0) {
				char errmsg[64] = {0};
				strcat(errmsg, "can’t find variable ");
				strcat(errmsg, $1);
				yyerror(errmsg);
			}
			else {
				if($3 == 0) {
					char errmsg[64] = "The divisor can’t be 0";
					yyerror(errmsg);
				}
				else {
					if(strcmp(table[found].type, "int") == 0) {
						table[found].idata /= (int)$3;
						fprintf(file, "\tiload %d\n", found-1);
						if(popstack() == 0)
							fprintf(file, "\tidiv\n");
						else {
							fprintf(file, "\tfstore %d\n", symcnt);
							fprintf(file, "\ti2f\n");
							fprintf(file, "\tfload %d\n", symcnt);
							fprintf(file, "\tfdiv\n");
							fprintf(file, "\tf2i\n");
						}
						fprintf(file, "\tistore %d\n", found-1);
					}
					else {
						table[found].fdata /= $3;
						fprintf(file, "\tfload %d\n", found-1);
						if(popstack() == 0)
							fprintf(file, "\ti2f\n");
						fprintf(file, "\tfdiv\n");
						fprintf(file, "\tfstore %d\n", found-1);
					}
				}
			}
		}
	| ID MODASI expr
		{
			int found = lookup_symbol($1);
			if(found == 0) {
				char errmsg[64] = {0};
				strcat(errmsg, "can’t find variable ");
				strcat(errmsg, $1);
				yyerror(errmsg);
			}
			else {
				if(strcmp(table[found].type, "int") == 0 && popstack() == 0) {
					table[found].idata %= (int)$3;
					fprintf(file, "\tiload %d\n", found-1);
					fprintf(file, "\tirem\n");
					fprintf(file, "\tistore %d\n", found-1);
				}
				else {
					char errmsg[64] = "can't Mod float numbers";
        			yyerror(errmsg);
        		}
			}
		}
;

expression_stat
	: expr
	| assignment
;

expr
	: term {$$ = $1;}
	| expr CONOR expr {$$ = ($1 || $3)? 1:0 ;}
	| expr CONAND expr {$$ = ($1 && $3)? 1:0 ;}
	| expr EQUAL expr
		{
		    $$ = ($1 == $3)? 1:0 ;
		    fprintf(file, "\tisub\n");
			fprintf(file, "\tifne Label_%d\n", label);
			label++;
		}
	| expr NOTEQU expr
		{
		    $$ = ($1 != $3)? 1:0 ;
		    fprintf(file, "\tisub\n");
			fprintf(file, "\tifeq Label_%d\n", label);
			label++;
		}
	| expr '<' expr
		{
		    $$ = ($1 < $3)? 1:0 ;
			fprintf(file, "\tisub\n");
			fprintf(file, "\tifge Label_%d\n", label);		
			label++;				
		}
	| expr LESEQU expr
		{
		    $$ = ($1 <= $3)? 1:0 ;
		    fprintf(file, "\tisub\n");
			fprintf(file, "\tifgt Label_%d\n", label);
			label++;
		}
	| expr '>' expr
		{
		    $$ = ($1 > $3)? 1:0 ;
		    fprintf(file, "\tisub\n");
			fprintf(file, "\tifle Label_%d\n", label);
			label++;
		}
	| expr GREEQU expr
		{
		    $$ = ($1 >= $3)? 1:0 ;
		    fprintf(file, "\tisub\n");
			fprintf(file, "\tiflt Label_%d\n", label);
			label++;
		}
	| expr '+' expr
		{
			$$ = $1 + $3;
			int right = popstack();
			int left = popstack();
			if(right == 0 && left == 0) {
				fprintf(file, "\tiadd\n");
				pushstack(0);
			}
			else if(right == 1 && left == 1) {
				fprintf(file, "\tfadd\n");
				pushstack(1);
			}
			else if(right == 0 && left == 1) {
				fprintf(file, "\ti2f\n");
				fprintf(file, "\tfadd\n");
				pushstack(1);
			}
			else {
				fprintf(file, "\tfstore %d\n", symcnt);
				fprintf(file, "\ti2f\n");
				fprintf(file, "\tfload %d\n", symcnt);
				fprintf(file, "\tfadd\n");
				pushstack(1);
			}
		}
	| expr '-' expr
		{
		    $$ = $1 - $3;
			int right = popstack();
			int left = popstack();
			if(right == 0 && left == 0) {
				fprintf(file, "\tisub\n");
				pushstack(0);
			}
			else if(right == 1 && left == 1) {
				fprintf(file, "\tfsub\n");
				pushstack(1);
			}
			else if(right == 0 && left == 1) {
				fprintf(file, "\ti2f\n");
				fprintf(file, "\tfsub\n");
				pushstack(1);
			}
			else {
				fprintf(file, "\tfstore %d\n", symcnt);
				fprintf(file, "\ti2f\n");
				fprintf(file, "\tfload %d\n", symcnt);
				fprintf(file, "\tfsub\n");
				pushstack(1);
			}
		}
	| expr '*' expr
		{
		    $$ = $1 * $3;
		    int right = popstack();
			int left = popstack();
			if(right == 0 && left == 0) {
				fprintf(file, "\timul\n");
				pushstack(0);
			}
			else if(right == 1 && left == 1) {
				fprintf(file, "\tfmul\n");
				pushstack(1);
			}
			else if(right == 0 && left == 1) {
				fprintf(file, "\ti2f\n");
				fprintf(file, "\tfmul\n");
				pushstack(1);
			}
			else {
				fprintf(file, "\tfstore %d\n", symcnt);
				fprintf(file, "\ti2f\n");
				fprintf(file, "\tfload %d\n", symcnt);
				fprintf(file, "\tfmul\n");
				pushstack(1);
			}
		}
    | expr '/' expr
		{
		    if($3 == 0) {
		        char errmsg[64] = "The divisor can’t be 0";
		        yyerror(errmsg);
		    	$$ = $1;
		    }
		    else {
		    	$$ = $1 / $3;
				int right = popstack();
				int left = popstack();
				if(right == 0 && left == 0) {
					fprintf(file, "\tidiv\n");
					pushstack(0);
				}
				else if(right == 1 && left == 1) {
					fprintf(file, "\tfdiv\n");
					pushstack(1);
				}
				else if(right == 0 && left == 1) {
					fprintf(file, "\ti2f\n");
					fprintf(file, "\tfdiv\n");
					pushstack(1);
				}
				else {
					fprintf(file, "\tfstore %d\n", symcnt);
					fprintf(file, "\ti2f\n");
					fprintf(file, "\tfload %d\n", symcnt);
					fprintf(file, "\tfdiv\n");
					pushstack(1);
				}
		    }
		}
	| expr '%' expr
		{
			int right = popstack();
			int left = popstack();
			if(right == 1 || left == 1) {
		        char errmsg[64] = "can't Mod float numbers";
		        yyerror(errmsg);
		    	$$ = $1;
		    }
			else {
				$$ = (int)$1 % (int)$3;
				fprintf(file, "\tirem\n");
				pushstack(0);
			}
		}
;

term
	: constant {$$ = $1;}
	| ID
		{
			int found = lookup_symbol($1);
			if(found == 0) {
			    char errmsg[64] = {0};
			    strcat(errmsg, "can’t find variable ");
			    strcat(errmsg, $1);
			    yyerror(errmsg);
			}
			else {
				if(strcmp(table[found].type, "int") == 0) {
					$$ = table[found].idata;
					fprintf(file, "\tiload %d\n", found-1);
					pushstack(0);
				}
				else {
					$$ = table[found].fdata;
					fprintf(file, "\tfload %d\n", found-1);
					floflag = 1;
					pushstack(1);
				}
			}
		}
	| ID INC 
		{
			int found = lookup_symbol($1);
			if(found == 0) {
			    char errmsg[64] = {0};
			    strcat(errmsg, "can’t find variable ");
			    strcat(errmsg, $1);
			    yyerror(errmsg);
			}
			else {
				if(strcmp(table[found].type, "int") == 0) {
					table[found].idata++;
					fprintf(file, "\tiload %d\n", found-1);
					fprintf(file, "\tldc 1\n");
					fprintf(file, "\tiadd\n");
					fprintf(file, "\tistore %d\n", found-1);
				}
				else {
					table[found].fdata++;
					fprintf(file, "\tfload %d\n", found-1);
					fprintf(file, "\tldc 1.000000\n");
					fprintf(file, "\tfadd\n");
					fprintf(file, "\tfstore %d\n", found-1);
				}
			}
    	}
	| ID DEC
		{
			int found = lookup_symbol($1);
			if(found == 0) {
			    char errmsg[64] = {0};
			    strcat(errmsg, "can’t find variable ");
			    strcat(errmsg, $1);
			    yyerror(errmsg);
			}
			else {
				if(strcmp(table[found].type, "int") == 0) {
					table[found].idata--;
					fprintf(file, "\tiload %d\n", found-1);
					fprintf(file, "\tldc 1\n");
					fprintf(file, "\tisub\n");
					fprintf(file, "\tistore %d\n", found-1);
				}
				else {
					table[found].fdata--;
					fprintf(file, "\tfload %d\n", found-1);
					fprintf(file, "\tldc 1\n");
					fprintf(file, "\ti2f\n");
					fprintf(file, "\tfsub\n");
					fprintf(file, "\tfstore %d\n", found-1);
				}
			}
    	}
    | '(' expr ')' {$$ = $2;}
;

constant
	: I_CONST 
		{
			$$ = $1;
			fprintf(file, "\tldc %d\n", $1);
			pushstack(0);
		}
	| F_CONST
		{
			$$ = $1;
			fprintf(file, "\tldc %f\n", $1);
			floflag = 1;
			pushstack(1);
		}
;

print_func
	: print_op '(' STRING ')' NEWLINE
		{
        	fprintf(file, "\tldc \"%s\"\n",$3);
        	fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
        	fprintf(file, "\tswap\n");
			fprintf(file, "\tinvokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n"); 
		}
	| print_op '(' expr ')' NEWLINE
		{
			if(floflag == 1)
			{
                fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                fprintf(file, "\tswap\n");
				fprintf(file, "\tinvokevirtual java/io/PrintStream/println(F)V\n");
			}
			else
			{
				fprintf(file, "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n");
                fprintf(file, "\tswap\n");
				fprintf(file, "\tinvokevirtual java/io/PrintStream/println(I)V\n");
			}
		}
;

print_op
	: PRINT
	| PRINTLN
;

for_stat
	: for_op expr block {label--; fprintf(file, "\tgoto Label_%d\n", label-1);}
	| for_op stat {fprintf(file, "Label_%d:\n", label); label += 3;} ';' stat {label--; fprintf(file, "\tgoto Label_%d\n", label-1); fprintf(file, "Label_%d:\n", label-2);} ';' stat {fprintf(file, "\tgoto Label_%d\n", label-3); fprintf(file, "Label_%d:\n", label-1);} block {fprintf(file, "\tgoto Label_%d\n", label-2); fprintf(file, "Label_%d:\n", label); label++;}
;

for_op	//FOR stat ';' expr ';' stat ';' stat block
	: FOR {fprintf(file, "Label_%d:\n", label), label++;}
;

if_stat
	: if_op {ifs--; fprintf(file, "Label_%d:\n", label-1); fprintf(file, "EXIT_%d:\n", ifs);}
	| if_op else_stat {ifs--; fprintf(file, "EXIT_%d:\n", ifs);}
;

if_op
	: IF {ifs++;} expr block {fprintf(file, "\tgoto EXIT_%d\n", ifs-1);}
;

else_stat
	: else_if {fprintf(file, "\tgoto EXIT_%d\n", ifs-1);} else_stat
	| ELSE {fprintf(file, "Label_%d:\n", label-1);} block {label--;}
;

else_if
	: ELSE IF {fprintf(file, "Label_%d:\n", label-1);} expr block
;

block
	: '{' program '}'
;

%%

/* C code section */
int main(int argc, char** argv)
{
	file = fopen("output.j", "w");
    fprintf(file, ".class public main\n.super java/lang/Object\n");
    fprintf(file, ".method public static main([Ljava/lang/String;)V\n");
	fprintf(file, ".limit stack %d\n.limit locals %d\n", 10, 10);
	
    yylineno = 0;
    yyparse();
    
    fprintf(file,"\treturn\n.end method\n");
	fclose(file);
	
	printf("parse over.\n");
    if(error == 1) {
    	printf("failed to generate .j file.\n");
		remove("output.j");    	
    }
    else
    	printf(".j file generated successfully.\n");
	//dump_symbol();
    return 0;
}

void create_symbol()
{
	int i;
	for (i=0; i<MAXYM; ++i) {
        memset(table[i].id, 0, sizeof(table[i].id));
        memset(table[i].type, 0, sizeof(table[i].type));
        table[i].idata = 0;
        table[i].fdata = 0.0;
	}
	return;
}

void insert_symbol(char* id, char* type, float data)
{
	if(lookup_symbol(id) < 0) {
		char errmsg[64] = {0};
		strcat(errmsg, "re-declaration for variable ");
		strcat(errmsg, id);
		yyerror(errmsg);
		return;
	}
	symcnt++;
	strcpy(table[symcnt].id, id);
	strcpy(table[symcnt].type, type);
	if(strcmp(table[symcnt].type, "int") == 0) {
		table[symcnt].idata = (int)data;
		if(popstack() == 1)
			fprintf(file, "\tf2i\n");
		fprintf(file, "\tistore %d\n", symcnt-1);
	}
	else {
		table[symcnt].fdata = data;
		if(popstack() == 0)
			fprintf(file, "\ti2f\n");
		fprintf(file, "\tfstore %d\n", symcnt-1);
	}
	return;
}

int lookup_symbol(char* id) 
{
	int i;
	if(symcnt == 0)
		return 0;
	for(i=1; i<=symcnt; i++) {
		if(strcmp(id, table[i].id) == 0)
			return i;
	}
    return 0;
}

void dump_symbol()
{
	printf("\nTotal lines : %d\n\n", yylineno);
    printf("The symbol table: \n");
    if(symcnt == 0) {
        printf("<symbol table empty>\n");
        return;
    }
    printf("Index\tID\tType\tData\n");
    int i;
    for(i=1; i<=symcnt; ++i) {
		printf("%d\t%s\t%s\t", i, table[i].id, table[i].type);
		if(strcmp(table[i].type, "int") == 0)
			printf("%d\n", table[i].idata);
		else
			printf("%f\n", table[i].fdata);
    }
    return;
}

void yyerror(char *s)
{
    printf("<ERROR> %s (line %d)\n", s, yylineno);
    error = 1;
    return;
}

void pushstack(int floflag)
{
	stackcnt++;
	exprstack[stackcnt] = floflag;
	return;
}

int popstack()
{
	int floflag;
	floflag = exprstack[stackcnt];
	stackcnt--;
	return floflag; 
}
