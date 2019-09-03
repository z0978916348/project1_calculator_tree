#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>


/*
Something like Python
>> y = 2
>> z = 2
>> x = 3*y + 4/(2*z)

*/

/*
the only type: integer
everything is an expression
  statement   := END | expr END
  expr        := term expr_tail
  expr_tail   := ADDSUB term expr_tail | NIL
  term        := factor term_tail
  term_tail := MULDIV factor term_tail | NIL
  factor      := INT | ADDSUB INT | ADDSUB ID | ID ASSIGN expr | ID | LPAREN expr RPAREN
*/
#define TBLSIZE 65535
#define MAXLEN 256
typedef enum {UNKNOWN, END, INT, ID, ADDSUB, MULDIV, ASSIGN,
LPAREN, RPAREN, AND, OR, XOR, end} TokenSet;

typedef struct {
	char name[MAXLEN];
	int val;
} Symbol;
static TokenSet getToken(void);
static TokenSet lookahead = UNKNOWN;
static char lexeme[MAXLEN];

TokenSet getToken(void)
{
    int i;
    char c;

    while ( (c = fgetc(stdin)) == ' ' || c== '\t' );  // ©¿²¤ªÅ¥Õ¦r¤¸
    if(c==EOF){
        return end;
    }
    if (isdigit(c)) {
        lexeme[0] = c;
        c = fgetc(stdin);
        i = 1;
        while (isdigit(c) && i<MAXLEN) {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        return INT;
    } else if (c == '+' || c == '-') {
        lexeme[0] = c;
        lexeme[1] = '\0';
        return ADDSUB;
    } else if (c == '*' || c == '/') {
        lexeme[0] = c;
        lexeme[1] = '\0';
        return MULDIV;
    } else if (c == '\n') {
        lexeme[0] = '\0';
        return END;
    } else if (c == '=') {
        strcpy(lexeme, "=");
        return ASSIGN;
    } else if (c == '&') {
        strcpy(lexeme, "&");
        return AND;
    } else if (c == '|') {
        strcpy(lexeme, "|");
        return OR;
    } else if (c == '^') {
        strcpy(lexeme, "^");
        return XOR;
    } else if (c == '(') {
        strcpy(lexeme, "(");
        return LPAREN;
    } else if (c == ')') {
        strcpy(lexeme, ")");
        return RPAREN;
    } else if (isalpha(c) || c == '_') {
        lexeme[0] = c;
        c = fgetc(stdin);
        i = 1;
        while (isalpha(c) || isdigit(c) || c == '_') {
            lexeme[i] = c;
            ++i;
            c = fgetc(stdin);
        }
        ungetc(c, stdin);
        lexeme[i] = '\0';
        return ID;
    } else {
        return UNKNOWN;
    }
}

void advance(void)
{
    lookahead = getToken();
}

int match(TokenSet token)
{
    if (lookahead == UNKNOWN) advance();
    return token == lookahead;
}

char* getLexeme(void)
{
    return lexeme;
}


Symbol table[TBLSIZE];
int sbcount = 0;

int position;
int r[8];
int r_t[8];
typedef struct _Node {
	char lexeme[MAXLEN];
	TokenSet token;
	int val;
	struct _Node *left, *right;
} BTNode;

void statement(void);
BTNode* expr(void);
BTNode* term(void);
BTNode* factor(void);
int getval(void);
int setval(char*, int);

typedef enum {MISPAREN, NOTNUMID, NOTFOUND, RUNOUT, NAN} ErrorType;
void error(ErrorType errorNum);

/* clean a tree */
void freeTree(BTNode *root)
{
	if (root!=NULL) {
		freeTree(root->left);
		freeTree(root->right);
		free(root);
	}
}

/* print a tree by pre-order */
void printPrefix(BTNode *root)
{
	if (root != NULL) {
		printf("%s ", root->lexeme);
		printPrefix(root->left);
		printPrefix(root->right);
	}
}
int empty;
/* traverse the syntax tree by pre-order
   and evaluate the underlying expression */
int ever[2000];
int ever1[2000];
int equal_left, equal_right;
int left1, right1;
int f = 3;
int t = 3;
int rvp1;
char table_[65535][256];
char tab[65535][256];
int q = 1;
int m;
int testequal = 0;
int testtree(BTNode *root)
{
	int retval = 0, lv, rv, i, valid = 0;
	if(q==1 && root->token!=ASSIGN){
        error(NAN);
    }
    q++;
	if (root != NULL) {
		switch (root->token) {
		case ID:
		    for(i=0;i<t;i++){
                if(strcmp(root->lexeme, tab[i])==0 ){
                    if(left1==1)break;
                    else{
                        valid = 1;
                        break;
                    }
                }
		    }
		    if( valid==0 && right1==1){
                printf("%s\n", root->lexeme);
                error(NAN);
		    }
		    if( valid==0 ){
                if(strcmp(root->lexeme, tab[i])==0){
                    break;
                }else{
                    strcpy(tab[t], root->lexeme);
                    t++;
                    break;
                }
		    }
		    if( valid && root->val==0) return root->val;
		    else if(valid) return 789;
		    break;

		case INT:
			retval = root->val;
			break;
		case ASSIGN:
		case ADDSUB:
		case MULDIV:
        case XOR:
        case OR:
        case AND:

			if (strcmp(root->lexeme, "+") == 0){
                lv = testtree(root->left);
                rv = testtree(root->right);
                retval = lv + rv;
			}
			else if (strcmp(root->lexeme, "-") == 0){
                lv = testtree(root->left);
                rv = testtree(root->right);
                retval = lv - rv;
			}

			else if (strcmp(root->lexeme, "*") == 0){
                lv = testtree(root->left);
                rv = testtree(root->right);
                retval = lv * rv;
			}

			else if (strcmp(root->lexeme, "/") == 0) {
                lv = testtree(root->left);
                rv = testtree(root->right);
				if (rv==0)
					error(NAN);
				else
					retval = lv / rv;
			} else if (strcmp(root->lexeme, "=") == 0){
                right1 = 1;
                left1 = 0;
                rv = testtree(root->right);
                right1 = 0;
                left1 = 1;
                lv = testtree(root->left);
                retval = setval(root->left->lexeme, rv);
                testequal++;
                if(testequal == 2){
                    error(NAN);
                }
			} else if (strcmp(root->lexeme, "&") == 0){
                lv = testtree(root->left);
                rv = testtree(root->right);
                retval = lv & rv;
			} else if (strcmp(root->lexeme, "|") == 0){
                lv = testtree(root->left);
                rv = testtree(root->right);
                retval = lv | rv;
			} else if (strcmp(root->lexeme, "^") == 0){
                lv = testtree(root->left);
                rv = testtree(root->right);
                retval = lv ^ rv;
			}
			break;
		default:
			retval = 0;
		}
	}
	return retval;
}


int evaluateTree(BTNode *root, int lvp, int rvp)
{
    int i, valid = 0;
	if (root != NULL) {
		switch (root->token) {
		case ID:
		    for(i=0;i<f;i++){
                if(strcmp(root->lexeme, table_[i])==0 ){
                    if(equal_left==1)break;
                    else{
                        //printf("%d %s\n", i, table_[i]);
                        valid = 1;
                        break;
                    }
                }
		    }
		    /*if( valid==0 && equal_right==1){
                error(NAN);
		    }*/
		    if( valid==0 ){
                if(strcmp(root->lexeme, table_[i])==0){
                    printf("MOV [%d] r%d", 4*i, rvp1);
                    position = rvp1;
                    //r[rvp1] = 1;
                    break;
                }else{
                    strcpy(table_[f], root->lexeme);
                    //printf("%d\n", f);
                    printf("MOV [%d] r%d", 4*f, rvp1);
                    position = rvp1;
                    //r[rvp1] = 1;
                    f++;
                    break;
                }
		    }
		    int record_i;
		    if(valid)record_i = i;
		    else record_i = f;
            int j, p = 0;
		    if(valid){
                if(ever[record_i]==1){
                        for(j=0;j<8;j++){
                            if(r[j]==0){
                                r[j] = 1;
                                printf("MOV r%d [%d]\n", j, 4*record_i);
                                position = j;
                                p = 1;
                                break;
                            }
                        }
                    if(p)break;
                }else{
                    for(i=0;i<8;i++){
                        if( r[i]==0 ){
                            printf("MOV r%d [%d]\n", i, 4*record_i);
                            ever[record_i] = 1;
                            r[i] = 1;
                            position = i;
                            break;
                        }
                    }
                }
            }
		case INT:
			for(i=0;i<8;i++){
                if( r[i]==0&&isdigit(root->lexeme[0]) ){
                    printf("MOV r%d %d\n", i, root->val);
                    r[i] = 1;
                    position = i;
                    break;
                }
			}
			//printf("1231\n");
		case ASSIGN:
		case ADDSUB:
		case MULDIV:
        case XOR:
        case OR:
        case AND:
			if (strcmp(root->lexeme, "+") == 0){
                lvp = evaluateTree(root->left, lvp, rvp);
                rvp = evaluateTree(root->right, lvp, rvp);
                printf("ADD r%d r%d\n", lvp, rvp);
                position = lvp;
                r[lvp] = 1;
                r[rvp] = 0;
			}
			else if (strcmp(root->lexeme, "-") == 0){
                lvp = evaluateTree(root->left, lvp, rvp);
                rvp = evaluateTree(root->right, lvp, rvp);

                printf("SUB r%d r%d\n", lvp, rvp);
                position = lvp;
                r[lvp] = 1;
                r[rvp] = 0;
			}
			else if (strcmp(root->lexeme, "*") == 0){
                lvp = evaluateTree(root->left, lvp, rvp);
                rvp = evaluateTree(root->right, lvp, rvp);
                printf("MUL r%d r%d\n", lvp, rvp);
                position = lvp;
                r[lvp] = 1;
                r[rvp] = 0;
			}
			else if (strcmp(root->lexeme, "/") == 0) {
                lvp = evaluateTree(root->left, lvp, rvp);
                rvp = evaluateTree(root->right, lvp, rvp);
                printf("DIV r%d r%d\n", lvp, rvp);
                position = lvp;
                r[lvp] = 1;
                r[rvp] = 0;
				/*if (rv==0)
					error(NAN);
				else
					retval = lv / rv;*/
			} else if (strcmp(root->lexeme, "=") == 0){
                    equal_left = 0;
                    equal_right = 1;
                    rvp1 = rvp = evaluateTree(root->right, lvp, rvp);
                    equal_left = 1;
                    equal_right = 0;
                    lvp = evaluateTree(root->left, lvp, rvp);
                    if(equal_left != 1 && equal_right!=0)
                        printf("MOV r%d r%d\n", lvp, rvp);
                    equal_left = 0;
                    equal_right = 0;
                    position = lvp;
                    r[lvp] = 0;
                    //r[rvp] = 1;
                    break;
            } else if (strcmp(root->lexeme, "&") == 0){
                lvp = evaluateTree(root->left, lvp, rvp);
                rvp = evaluateTree(root->right, lvp, rvp);
                printf("AND r%d r%d\n", lvp, rvp);
                position = lvp;
                r[lvp] = 1;
                r[rvp] = 0;
			} else if (strcmp(root->lexeme, "|") == 0){
                lvp = evaluateTree(root->left, lvp, rvp);
                rvp = evaluateTree(root->right, lvp, rvp);
                printf("OR r%d r%d\n", lvp, rvp);
                position = lvp;
                r[lvp] = 1;
                r[rvp] = 0;
			} else if (strcmp(root->lexeme, "^") == 0){
                lvp = evaluateTree(root->left, lvp, rvp);
                rvp = evaluateTree(root->right, lvp, rvp);
                printf("XOR r%d r%d\n", lvp, rvp);
                position = lvp;
                r[lvp] = 1;
                r[rvp] = 0;
			}
			break;
		default:
            printf("EXIT 1\n");
		}
	}
	return position;
}

int getval(void)
{
	int i, retval, found;
	if (match(INT)) {
		retval = atoi(getLexeme());
	} else if (match(ID)) {
		i = 0;
		found = 0;
		retval = 0;
		while (i<sbcount && !found) {
			if (strcmp(getLexeme(), table[i].name)==0) {
				retval = table[i].val;
				found = 1;
				break;
			} else {
				i++;
			}
		}
		if (!found) {
			if (sbcount < TBLSIZE) {
				strcpy(table[sbcount].name, getLexeme());
				table[sbcount].val = 0;
				sbcount++;
			} else {
				error(RUNOUT);
			}
		}
	}
	return retval;
}

int setval(char *str, int val)
{
	int i, retval = 0;
	i = 0;
	while (i<sbcount) {
		if (strcmp(str, table[i].name)==0) {
			table[i].val = val;
			retval = val;
			break;
		} else {
			i++;
		}
	}
	return retval;
}
/* create a node without any child */
BTNode* makeNode(TokenSet tok, const char *lexe)
{
	BTNode *node = (BTNode *) malloc(sizeof(BTNode));
	strcpy(node->lexeme, lexe);
	node->token= tok;
	node->val = 0;
	node->left = NULL;
	node->right = NULL;
	return node;
}
//  expr        := term expr_tail
//  expr_tail   := ADDSUB term expr_tail | NIL
BTNode* expr(void)
{
	BTNode *retp, *left;
	retp = left = term();
	while (match(ADDSUB)) { // tail recursion => while
		retp = makeNode(ADDSUB, getLexeme());
		advance();
		retp->right = term();
		retp->left = left;
		left = retp;
	}
	return retp;
}

//  term        := factor term_tail
//  term_tail := MULDIV factor term_tail | NIL
BTNode* term(void)
{
	BTNode *retp, *left;
	retp = left = factor();
	while (match(MULDIV)) { // tail recursion => while
		retp = makeNode(MULDIV, getLexeme());
		advance();
		retp->right = factor();
		retp->left = left;
		left = retp;
	}
	return retp;

}
BTNode* and(void)
{
	BTNode *retp, *left;
	retp = left = expr();
	while (match(AND)) { // tail recursion => while
		retp = makeNode(AND, getLexeme());
		advance();
		retp->right = expr();
		retp->left = left;
		left = retp;
	}
	return retp;
}
BTNode* or(void)
{
	BTNode *retp, *left;
	retp = left = and();
	while (match(OR)) { // tail recursion => while
		retp = makeNode(OR, getLexeme());
		advance();
		retp->right = and();
		retp->left = left;
		left = retp;
	}
	return retp;
}
BTNode* xor(void)
{
	BTNode *retp, *left;
	retp = left = or();
	while (match(XOR)) { // tail recursion => while
		retp = makeNode(XOR, getLexeme());
		advance();
		retp->right = or();
		retp->left = left;
		left = retp;
	}
	return retp;
}
BTNode* factor(void)
{
	BTNode* retp = NULL;
	char tmpstr[MAXLEN];
	if( (match(end)) )error(end);
	if (match(INT)) {
		retp =  makeNode(INT, getLexeme());
		retp->val = getval();
		advance();
	} else if (match(ID)) {
		BTNode* left = makeNode(ID, getLexeme());
		left->val = getval();
		strcpy(tmpstr, getLexeme());
		advance();
		if (match(ASSIGN)) {
            retp = makeNode(ASSIGN, getLexeme());
            advance();
            retp->right = xor();
            retp->left = left;
        } else if (match(INT)){
            error(MISPAREN);
        } else {
			retp = left;
		}
	} else if (match(ADDSUB)) {
		strcpy(tmpstr, getLexeme());
		advance();
		if (match(ID) || match(INT)) {
			retp = makeNode(ADDSUB, tmpstr);
			if (match(ID)){
                retp->right = makeNode(ID, getLexeme());
			} else if (match(INT)){
                retp->right = makeNode(INT, getLexeme());
			}
			retp->right->val = getval();
			retp->left = makeNode(INT, "0");
			retp->left->val = 0;
			advance();
		} else {
			error(NOTNUMID);
		}
	} else if (match(LPAREN)) {
		advance();
		retp = xor();
		if (match(RPAREN)) {
			advance();
		} else {
			error(MISPAREN);
		}
	} else {
		error(NOTNUMID);
	}
	return retp;
}
void error(ErrorType errorNum)
{
	switch (errorNum) {
    case end:
        printf("MOV r0 [0]\n");
        printf("MOV r1 [4]\n");
        printf("MOV r2 [8]\n");
        printf("EXIT 0\n");
		break;
	case MISPAREN:
		printf("EXIT 1\n");
		break;
	case NOTNUMID:
		printf("EXIT 1\n");
		break;
	case NOTFOUND:
		printf("EXIT 1\n");
		break;
	case RUNOUT:
		printf("EXIT 1\n");
		break;
	case NAN:
		printf("EXIT 1\n");
	}
	exit(0);
}

void statement(void)
{
	BTNode* retp;
    int m, b;
	if (match(END)) {
		//printf(">> ");
		advance();
	} else {
		retp = expr();
		if (match(END)) {
            b = testtree(retp);
            q = 1;
            testequal = 0;
            left1 = 0;
            right1 = 0;
            m = evaluateTree(retp, 0, 0);
            equal_left = 0;
            equal_right = 0;
			//printf("%d\n", evaluateTree(retp, 0, 0));
			//printPrefix(retp);
			printf("\n");
			freeTree(retp);

			//printf(">> ");
			advance();
		}
	}
}

int main()
{
    table_[0][0] = 'x';
    table_[1][0] = 'y';
    table_[2][0] = 'z';
    tab[0][0] = 'x';
    tab[1][0] = 'y';
    tab[2][0] = 'z';
	//printf(">> ");
	while (1) {
		statement();
	}
    /*printf("MOV r0 [0]\n");
    printf("MOV r1 [4]\n");
    printf("MOV r2 [8]\n");*/
	return 0;
}

/*
int main(void)
{
    TokenSet tok;
    tok = getToken();
    while (tok != END) {
        printf("%d: %s\n", tok, getLexeme());
        tok = getToken();
    }
    return 0;
}
*/
