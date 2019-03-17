/* C compiler: Fabrice Frances' 16 bits 65802 code generator */
char *version="16bit code V2.0 by F.Frances";
#include "c.h"
#define NULL ((char *)0)

static int offset;			/* current local size */
static int localsize;		/* max size of locals */
static int argoffset;		/* current stack position */
static int argbuildsize;	/* max size of arguments */
static int params;			/* size of formal parameters */
static int tmpsize;			/* max size of temporary variables */
static int nbregs;			/* number of used registers */
static unsigned busy;		/* busy&(1<<t) == 1 if tmp t is used */
static char *fname;		/* current function name */
static char *callname;		/* current function called name */
static char *NamePrefix;	/* Prefix for all local names */
static int omit_frame;		/* if no params and no locals */
static int return_int;		/* if function returns something of int size */
static int verbose=0;
static int optimizelevel=3;	/* set by command line option -On */
static char *temp[32];		/* 32 temporary variables (first 8 ones in ZP and 8 last ones for floating point) */
static char *regname[8];	/* 8 register variables names */
static Node a,b,r;


/* very small peephole optimizer */
static void output(char *format, ...)
{
	static char last[128]="\t";
	char buffer[128];
	va_list ap;

	va_init(ap, format);
	vsprintf(buffer,format, ap);
	va_end(ap);
	if (strncmp(buffer,"\tLOAD_",6)==0 
		&& strncmp(last,"\tSTORE_",7)==0
		&& strcmp(buffer+6,last+7)==0) 
	{
		return; /* removes unneeded LOAD */
	}
	if (strcmp(buffer,"\tCMP_C(0)\n")==0
		&& strncmp(last,"\tLOAD_",6)==0)
	{
		return; /* removes unneeded CMP #0 */
	}
	outs(buffer);
	strcpy(last,buffer);
}

static char *opnames[]={
	0,
	"CNST",
	"ARG",
	"ASGN",
	"INDIR",
	"CVC",
	"CVD",
	"CVF",
	"CVI",
	"CVP",
	"CVS",
	"CVU",
	"NEG",
	"CALL",
	"LOAD",
	"RET",
	"ADDRG",
	"ADDRF",
	"ADDRL",
	"ADD",
	"SUB",
	"LSH",
	"MOD",
	"RSH",
	"BAND",
	"BCOM",
	"BOR",
	"BXOR",
	"DIV",
	"MUL",
	"EQ",
	"GE",
	"GT",
	"LE",
	"LT",
	"NE",
	"JUMP",
	"LABEL",
	"AND",
	"NOT",
	"OR",
	"COND",
	"RIGHT",
	"FIELD"
};

static void printnode(Node p)
{
	char typenames[]=" FDCSIUPVB";
	if (p) {
		printf("%08X %s%c   "
			,p
			,opnames[(p->op)>>4]
			,typenames[(p->op)&0x0F]);
/*
		if (p->kids[0]) 
			printf("%s,%c  ",p->kids[0]->x.result.name,p->kids[0]->x.result.adrmode);
		if (p->kids[1]) 
			printf(";  %s,%c",p->kids[1]->x.result.name,p->kids[1]->x.result.adrmode);
*/
		printf("%08X,%08X ",p->kids[0],p->kids[1]);
		printf(" -> %s,%c",p->x.result.name,p->x.result.adrmode);
		printf("   count=%d\n",p->count);
	}
}

static char indirectmode(char *name,char mode)
{
	int i;
	char indmode;
	switch (mode) {
	case 'A': indmode='S'; break;
	case 'C': 
		for (i=0;i<8;i++)
			if (name==temp[i] || name==regname[i])
				break;
		if (i<8) indmode='Z';
		else indmode='D';
		break;
	case 'Z': indmode='I'; break;
	case 'S': indmode='P'; break;
	default: printf("Addressing mode %c cannot be dereferenced\n",mode); assert(0); break;
	}
	return indmode;
}


static Node *linearize(Node p, Node *last, Node next) {
	Node head0=0, head1=0, *last0, *last1;
	if (p && !p->x.visited) {
		Node k0=p->kids[0], k1=p->kids[1];
		p->x.optimized=0;
		p->x.result.type=0;
		p->x.result.name="*******";
		p->x.result.adrmode='*';
		last0 = linearize(k0, &head0, 0);
		last1 = linearize(k1, &head1, 0);
		
		if (verbose) printnode(p);
		p->x.visited = 1;
		if (p->op==INDIRB) { /* fix frontend bug: remove all INDIRB nodes */
			p->x.optimized=1;
			p->x.result=k0->x.result;
			return last;
		}
		if (head0) { *last = head0;	last = last0; }
		if (head1) { *last = head1;	last = last1; }
		*last = p;
		last = &p->x.next;
	}
	*last = next;
	return last;
}

void progbeg(int argc,char *argv[]) {
	int i;
	for(i=1;i<argc;i++) {
		if (strncmp(argv[i],"-N",2)==0) {
			NamePrefix=argv[i]+2;
		} else if (strcmp(argv[i],"-V")==0) {
			verbose=1;
		} else if (strcmp(argv[i],"-O")==0) {
			optimizelevel=3;
		} else if (strcmp(argv[i],"-O0")==0) {
			optimizelevel=0;	/* no optimization */
		} else if (strcmp(argv[i],"-O1")==0) {
			optimizelevel=1;	/* remove ADDR and CNST leaves */
		} else if (strcmp(argv[i],"-O2")==0) {
			optimizelevel=2;	/* allocate register variables */
						/* and do some easy opt. (INC...) */
		} else if (strcmp(argv[i],"-O3")==0) {
			optimizelevel=3;	/* optimizes INDIR, ASGN ... */
		} else {
			printf("Unknown option %s\n",argv[i]);
			exit(1);
		}
	}
	for (i=0;i<8;i++) {
		temp[i]=stringf("tmp%d",i);
		regname[i]=stringf("reg%d",i);
	}
	for (i=8;i<32;i++) {
		temp[i]=stringf("tmp%d",i);
	}
}

void progend(void) { output("; %s\n",version); }

void defsymbol(Symbol p) {
	p->x.type = GLOBAL;
	p->x.adrmode = 'C';
	if (p->scope == CONSTANTS) {
		p->x.name = p->name;
		if (p->x.name[0]=='0' && p->x.name[1]=='x') {
			p->x.name[0]=' '; p->x.name[1]='$';
		}
	} else if (p->sclass == STATIC)
		p->x.name = stringf("L%s%d", NamePrefix, genlabel(1));
	else if (p->generated)
		p->x.name = stringf("L%s%s", NamePrefix, p->name);
	else
		p->x.name = stringf("_%s", p->name);
}

void export(Symbol p) {}
void import(Symbol p) {}
void segment(int s) {}
void global(Symbol p) { output("%s\n", p->x.name); }

void printfloat(double val)
{
	int i,exp=32,negative=0;
	double two_pow31,two_pow32;
	unsigned long mantissa;
	if (val==0.0) {
		output("\tBYTE 0,0,0,0,0\n");
		return;
	}
	if (val<0.0) { negative=1; val= -val; }
	for (two_pow31=1.0,i=0;i<31;i++) two_pow31*=2;
	two_pow32=two_pow31*2;
	while (val>=two_pow32) {
		val/=2;
		exp++;
	}
	while (val<two_pow31) {
		val*=2;
		exp--;
	}
	if (!negative) val-=two_pow31;
	mantissa=val;
	output("\tBYTE $%x",(exp+128)&0xFF);
	output(",$%x",(mantissa>>24)&0xFF);
	output(",$%x",(mantissa>>16)&0xFF);
	output(",$%x",(mantissa>>8)&0xFF);
	output(",$%x\n",mantissa&0xFF);
}


void defconst(int ty, Value v) {
	switch (ty) {
	case C: output("\tDB(%d)\n",   v.uc); break;
	case S: output("\tDB(%d)\n",   v.us); break;
	case I: output("\tDW(%d)\n",   v.i ); break;
	case U: output("\tDW($%x)\n",  v.u ); break;
	case P: output("\tDW($%x)\n",  v.p ); break;
	case F: printfloat(v.f); break;
	case D: printfloat(v.d); break;
	default: assert(0);
	}
}

void defstring(int len, char *s) {
	while (len > 0) {
		if (s[0]==';' || s[0]<32 || s[0]==127) {
			output("\tBYTE $%x",*s++);
			len--;
			while (len>0 && (s[0]==';' || s[0]<32 || s[0]==127)) {
				output(",$%x",*s++);
				len--;
			}
			output("\n");
		} else {
			output("\tSTRING \"");
			while (len>0 && s[0]!=';' && s[0]>=32 && s[0]!=127) {
				len--;
				if (s[0]=='"') output("\\\"");
				else if (s[0]=='\\') output("\\\\");
				else output("%c",s[0]);
				s++;
			}
			output("\"\n");
		}
	}
}

void defaddress(Symbol p) { output("\tDW(%s)\n",p->x.name); }
void space(int n) { output("\tZERO(%d)\n",n); }

int allocreg(Symbol p) {
	if (nbregs==8 || p->type->size>2) return 0;
	p->x.type=GLOBAL;
	p->x.adrmode='C';
	p->x.name=regname[nbregs];
	nbregs++;
	return 1;
}

void function(Symbol f, Symbol caller[], Symbol callee[], int ncalls) {
	int i;

	localsize=tmpsize=nbregs=0; 
	fname=f->x.name;
	params=0;
	for (i = 0; caller[i] && callee[i]; i++) {
		caller[i]->x.type=PARAM;
		caller[i]->x.adrmode='A';
		caller[i]->x.name=NULL;
		caller[i]->x.offset=params;
		params+=caller[i]->type->size;

		if (optimizelevel<2 || callee[i]->sclass!=REGISTER || !allocreg(callee[i])){
			callee[i]->sclass=AUTO;
			callee[i]->x=caller[i]->x;
		}
	}
	if (verbose) printf("\nGenerating function %s :\n\n",fname);
	busy=localsize=argbuildsize=offset=argoffset=0;
	gencode(caller,callee);
	omit_frame=(argbuildsize+localsize==0);
	if (verbose) printf("Function %s generated.\n\n",fname);

	output("%s\n",fname);
	for (i=0;i<nbregs;i++) output("\tPUSH_Z(%s)\n",regname[i]);
	if (!omit_frame)
		output("\tENTER(%d)\n",localsize+argbuildsize);
/*
	if (isstruct(freturn(f->type))) {
		output("\tLOAD_D(op1)\n");
		output("\tSTORE_Y((fp),6)\n");
	}
*/
	argoffset=0; return_int=0;
	emitcode();
	if (!omit_frame || nbregs>0) {
		if (return_int) output("\tSTORE_Z(tmp)\n");
		if (!omit_frame) output("\tLEAVE(%d)\n",argbuildsize+localsize);
		for (i=nbregs-1;i>=0;i--) {
			output("\tPULL\n");
			output("\tSTORE_Z(%s)\n",regname[i]);
		}
		if (return_int) output("\tLOAD_Z(tmp)\n");
	}
	output("\tRET\n");
}

void local(Symbol p) {
	if (verbose)  {
		printf("asked local for %s:\n",p->name);
		if (p->temporary) printf("\ttemporary\n");
		if (p->sclass==AUTO) printf("\tAUTO\n");
		if (p->sclass==REGISTER) printf("\tREGISTER\n");
	}
	if (optimizelevel>=2 && p->sclass==REGISTER && allocreg(p))
		return; /* allocreg ok */
	p->sclass=AUTO;
	p->x.type=LOCAL;
	p->x.adrmode = 'A';
	p->x.name=NULL;
	p->x.offset = offset;
	if (verbose) 
		printf("\tallocated at offset %d\n",offset);
	offset+=p->type->size;
}

void address(Symbol q, Symbol p, int n) {
	q->x=p->x;
	if (p->x.adrmode=='A') {
		q->x.offset = p->x.offset+n;
	} else
		q->x.name = stringf("%s%s%d", p->x.name, n >= 0 ? "+" : "", n);
}

void blockbeg(Env *e) { e->offset = offset; }
void blockend(Env *e) {
	if (offset > localsize) localsize = offset;
	offset = e->offset;
}

static void gettmp(Node p) {
	int t;
	if ( optype(p->op)!=F && optype(p->op)!=D ) {
		for (t=0;t<24;t++)
			if ((busy&(1<<t))==0) {
				busy|=1<<t;
				p->x.result.type=GLOBAL;
				p->x.result.name=temp[t];
				if (t<8) p->x.result.adrmode='Z';
				else p->x.result.adrmode='D';
				return;
			}
	} else
		for (t=24;t<32;t++)
			if ((busy&(1<<t))==0) {
				busy|=1<<t;
				p->x.result.type=GLOBAL;
				p->x.result.name=temp[t];
				p->x.result.adrmode='D';
				return;
			}
	perror("Too complex expression"); exit(1);
}

static void releasetmp(Node p) {
	if (p) {
		assert(p->count!=0);
		p->count--;
		if (p->count==0 && !p->x.optimized) {
			int i;
			for (i=0;i<32;i++)
				if (p->x.result.name==temp[i]) break;
			busy&= ~(1<<i);
		}
	}
}

static int optimize_indirection(Node p)
{
	Node k=p->kids[0];

	if ( optimizelevel>=1 )	{
		switch (generic(k->op)) {
		case CNST: case ADDRG: case ADDRL: case ADDRF:
			p->x.optimized=1;
			p->x.result=k->x.result;
			p->x.result.adrmode=indirectmode(p->x.result.name,p->x.result.adrmode);
			if (verbose) 
				printf("INDIR optimization level 1 : %s,%c (kid : %s,%c)\n"
					,p->x.result.name,p->x.result.adrmode,k->x.result.name,k->x.result.adrmode);
			return 1;
			}
	}

	if ( optimizelevel>=2 ) {
		if (k->x.optimized && (k->x.result.adrmode=='Z' || k->x.result.adrmode=='S'))
		{

			p->x.optimized=1;
			p->x.result=k->x.result;
			p->x.result.adrmode=indirectmode(k->x.result.name,k->x.result.adrmode);
			if (verbose) 
				printf("INDIR optimization level 2 : %s,%c (kid : %s,%c)\n"
					,p->x.result.name,p->x.result.adrmode,k->x.result.name,k->x.result.adrmode);
			return 1;
		}
	}

	return 0;
}

static void optimize(Node p)
{
	Node k0=p->kids[0], k1=p->kids[1];
	if (verbose) printnode(p);
	switch (p->op) {
	case ARGI:
	case ARGP:
		if (optimizelevel>=3) {
			if (!k0->x.optimized) {
				p->x.optimized=1;
				k0->x.optimized=1;
				k0->x.result.type=ARG;
				k0->x.result.adrmode='S';
				k0->x.result.offset=argoffset;
				if (verbose) 
					printf("ARG optimization level 3 : %s,%c (kid : %s,%c)\n"
						,p->x.result.name,p->x.result.adrmode,k0->x.result.name,k0->x.result.adrmode);
			}
		}
		break;
	case ASGNI:
	case ASGNP:
		if (optimizelevel>=3) {	/* kids[1] : expression droite */
			if (	( generic(k0->op)==ADDRF
					|| generic(k0->op)==ADDRG
					|| generic(k0->op)==ADDRL
					|| generic(k0->op)==CNST)
			    && k1->count==0 && !k1->x.optimized)
			{
				p->x.optimized=1;
				k1->x.optimized=1;
				k1->x.result=k0->x.result;
				k1->x.result.adrmode=indirectmode(k0->x.result.name,k0->x.result.adrmode);
				if (verbose) 
					printf("ASGN optimization level 3 : %s,%c (kid : %s,%c)\n"
						,p->x.result.name,p->x.result.adrmode,k1->x.result.name,k1->x.result.adrmode);
			}
		}
		break;
	}
}

static int needtmp(Node p) {
	Node k=p->kids[0];
	switch (generic(p->op)) {
		case ADDRF:
		case ADDRG:
		case ADDRL:
		case CNST:
			if ( optimizelevel ) {
				p->x.optimized=1;
				p->x.result=p->syms[0]->x;
				if (verbose) 
					printf("Optimized CNST/ADDR node: %s,%c\n",p->x.result.name,p->x.result.adrmode);
				return 0;
		    }
			return 1;
		case INDIR:
			if (optimize_indirection(p)) return 0;
			return 1;
		case ASGN:
		case ARG:
		case EQ: case GE: case GT: case LE: case LT: case NE:
		case RET:
		case JUMP: case LABEL:
			return 0;
		case CALL: 
			if (optype(p->op)==B) return 0;
			if (p->count==0) p->op=CALLV;
			if (optype(p->op)==V) return 0;
			break;
	}
	return 1;
}

static void tmpalloc(Node p) {
	int i;
	releasetmp(p->kids[0]); releasetmp(p->kids[1]);
	switch (generic(p->op)) {
	case ARG:
		argoffset += p->syms[0]->u.c.v.i;
		if (argoffset > argbuildsize)
			argbuildsize = argoffset;
		break;
	case CALL:
		argoffset = 0;
		break;
	}
	if (needtmp(p)) gettmp(p);
	optimize(p);
}


void asmcode(char *str, Symbol argv[]) {
	for ( ; *str; str++)
		if (*str == '%' && str[1] >= 0 && str[1] <= 9)
			output("%s", argv[*++str]->x.name);
		else
			output("%c", *str);
	output("\n");
}

static char addr(char mode)
{
	if (mode=='D') return 'C';
	else if (mode=='I') return 'Z';
	else if (mode=='S') return 'A';
	else if (mode=='P') return 'S';
	else printf("Error: taking effective address of %c mode\n",mode);
	return 0;
}

static char *namefromoffset(Symbol p)
{
	if (p->x.type==ARG)
		p->x.name=stringf("%d",1+p->x.offset);
	if (p->x.type==LOCAL)
		p->x.name=stringf("%d",1+argbuildsize+p->x.offset);
	if (p->x.type==PARAM)
		p->x.name=stringf("%d",1+argbuildsize+localsize+nbregs*2+2+p->x.offset);
	return p->x.name;
}

static char *finalname(Node p)
{
	if (p->x.result.type==ARG)
		p->x.result.name=stringf("%d",1+p->x.result.offset);
	if (p->x.result.type==LOCAL)
		p->x.result.name=stringf("%d",1+argbuildsize+p->x.result.offset);
	if (p->x.result.type==PARAM)
		p->x.result.name=stringf("%d",1+argbuildsize+localsize+nbregs*2+2+p->x.result.offset);
	return p->x.result.name;
}

static void binary_integer_op(char *inst) {
	output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
	output("\t%s_%c(%s)\n",inst,b->x.result.adrmode,b->x.result.name);
	output("\tSTORE_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
}

static void binary_float_op(char *inst) {
	output("\tLOAD_%c(%s)\n",addr(a->x.result.adrmode),a->x.result.name);
	output("\tFLOAD2\n");
	output("\tLOAD_%c(%s)\n",addr(b->x.result.adrmode),b->x.result.name);
	output("\tFLOAD1\n");
	output("\t%s\n",inst);
	output("\tLOAD_%c(%s)\n",addr(r->x.result.adrmode),r->x.result.name);
	output("\tFSTORE\n");
}

static void integer_muldiv(char *inst) {
	output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
	output("\tSTORE_Z(op1)\n");
	output("\tLOAD_%c(%s)\n",b->x.result.adrmode,b->x.result.name);
	output("\tSTORE_Z(op2)\n");
	output("\t%s\n",inst);
	output("\tSTORE_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
}

static void binary(char *inst) {
	output("\t%s_%c%c%c(", inst
		,a->x.result.adrmode
		,b->x.result.adrmode
		,r->x.result.adrmode);
	output("%s,%s,%s)\n", a->x.result.name, b->x.result.name, r->x.result.name);
}

static void unary_integer_op(char *inst) {
	output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
	if (inst) output("\t%s\n",inst);
	output("\tSTORE_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
}

static void unary_float_op(char *inst) {
	output("\tLOAD_%c(%s)\n",addr(a->x.result.adrmode),a->x.result.name);
	output("\tFLOAD1\n");
	if (inst) output("\t%s\n",inst);
	output("\tLOAD_%c(%s)\n",addr(r->x.result.adrmode),r->x.result.name);
	output("\tFSTORE\n");
}

static void unary(char *inst) {
	output("\t%s_%c%c(", inst
		,a->x.result.adrmode
		,r->x.result.adrmode);
	output("%s,%s)\n", a->x.result.name, r->x.result.name);
}

static void compare0(char *inst) {
	output("\t%s_%c(%s,%s)\n"
		,inst
		,a->x.result.adrmode
		,a->x.result.name
		,r->syms[0]->x.name);
}

static void integer_compare(char *inst, Node op1, Node op2)
{
	output("\tLOAD_%c(%s)\n",op1->x.result.adrmode,op1->x.result.name);
	if (optimizelevel>=2 && strcmp(op2->x.result.name,"0")==0) {
		if (strcmp(inst,"BGE")==0) inst="BPL";
		else if (strcmp(inst,"BLT")==0) inst="BMI";
		else if (strcmp(inst,"BCS")==0) inst="BRA";
		else if (strcmp(inst,"BCC")==0) return;
	}
	output("\tCMP_%c(%s)\n",op2->x.result.adrmode,op2->x.result.name);
	output("\t%s(%s)\n",inst,r->syms[0]->x.name);
}

static void float_compare(char *inst, Node op1, Node op2)
{
	output("\tLOAD_%c(%s)\n",addr(op1->x.result.adrmode),op1->x.result.name);
	output("\tFLOAD1\n");
	output("\tLOAD_%c(%s)\n",addr(op2->x.result.adrmode),op2->x.result.name);
	output("\tFCOMP\n");
	output("\t%s(%s)\n",inst,r->syms[0]->x.name);
}

static void compare(char *inst) {
	output("\t%s_%c%c(", inst
		,a->x.result.adrmode
		,b->x.result.adrmode);
	output("%s,%s,%s)\n", a->x.result.name, b->x.result.name, r->syms[0]->x.name);
}

void emitdag(Node p) {
	int i;
	a = p->kids[0]; b = p->kids[1]; r=p;
	if (a) a->x.result.name=finalname(a);
	if (b) b->x.result.name=finalname(b);
	p->x.result.name=finalname(p);

	switch (p->op) {
		case BANDU:	binary_integer_op("AND");   break;
		case BORU:	binary_integer_op("OR");   break;
		case BXORU:	binary_integer_op("XOR");   break;
		case ADDD:  
		case ADDF:	binary_float_op("FADD");  break;
		case ADDI:
		case ADDP:
		case ADDU:
			if (optimizelevel>=2 && strcmp(a->x.result.name,p->x.result.name)==0
					&& (p->x.result.adrmode=='D' || p->x.result.adrmode=='Z')
					&& strcmp(b->x.result.name,"1")==0)
				output("\tINCW_%c(%s)\n",p->x.result.adrmode,p->x.result.name);
			else
				binary_integer_op("ADD");
			break;
		case SUBD:  
		case SUBF:	binary_float_op("FSUB");  break;
		case SUBI:  
		case SUBP:  
		case SUBU:
			if (optimizelevel>=2 && strcmp(a->x.result.name,p->x.result.name)==0
					&& (p->x.result.adrmode=='D' || p->x.result.adrmode=='Z')
					&& strcmp(b->x.result.name,"1")==0)
				output("\tDECW_%c(%s)\n",p->x.result.adrmode,p->x.result.name);
			else
				binary_integer_op("SUB");
			break;
		case MULD:  
		case MULF:	binary_float_op("FMUL");	 break;
		case MULI:	integer_muldiv("MULI");   break;
		case MULU:	integer_muldiv("MULU");   break;
		case DIVD:  
		case DIVF:	binary_float_op("FDIV");	 break;
		case DIVI:	integer_muldiv("DIVI");   break;
		case DIVU:	integer_muldiv("DIVU");   break;
		case MODI:	integer_muldiv("MODI");   break;
		case MODU:	integer_muldiv("MODU");   break;
		case RSHU:  
		case RSHI:
			if (optimizelevel>=2) {
				if (strcmp(b->x.result.name,"0")==0) unary_integer_op(NULL);
				else if (strcmp(b->x.result.name,"1")==0) unary_integer_op("RSH1");
				else if (strcmp(b->x.result.name,"2")==0) unary_integer_op("RSH2");
				else if (strcmp(b->x.result.name,"3")==0) unary_integer_op("RSH3");
				else if (strcmp(b->x.result.name,"4")==0) unary_integer_op("RSH4");
				else if (strcmp(b->x.result.name,"5")==0) unary_integer_op("RSH5");
				else if (strcmp(b->x.result.name,"6")==0) unary_integer_op("RSH6");
				else if (strcmp(b->x.result.name,"7")==0) unary_integer_op("RSH7");
				else if (strcmp(b->x.result.name,"8")==0) unary_integer_op("RSH8");
				else binary_integer_op("RSH");
			} else binary_integer_op("RSH");
			break;
		case LSHI:  
		case LSHU:
			if (optimizelevel>=2) {
				if (strcmp(b->x.result.name,"0")==0) unary_integer_op(NULL);
				else if (strcmp(b->x.result.name,"1")==0) unary_integer_op("LSH1");
				else if (strcmp(b->x.result.name,"2")==0) unary_integer_op("LSH2");
				else if (strcmp(b->x.result.name,"3")==0) unary_integer_op("LSH3");
				else if (strcmp(b->x.result.name,"4")==0) unary_integer_op("LSH4");
				else if (strcmp(b->x.result.name,"5")==0) unary_integer_op("LSH5");
				else if (strcmp(b->x.result.name,"6")==0) unary_integer_op("LSH6");
				else if (strcmp(b->x.result.name,"7")==0) unary_integer_op("LSH7");
				else if (strcmp(b->x.result.name,"8")==0) unary_integer_op("LSH8");
				else binary_integer_op("LSH");
			} else binary_integer_op("LSH");
			break;
		case INDIRC: 
			if (p->x.optimized) break;
			if (optimizelevel>=2) output("; INDIRC not optimized\n");
			switch (a->x.result.adrmode) {
			case 'C':
			case 'Z':
			case 'A':
				output("\tLOADB_%c(%s)\n"
					,indirectmode(a->x.result.name,'A')
					,a->x.result.name);	
				break;
			case 'D': output("\tLOAD_D(%s)\n",a->x.result.name);
					  output("\tSTORE_Z(tmp)\n");
					  output("\tLOADB_I(tmp)\n");
					  break;
			case 'S': output("\tLOADB_P(%s)\n",a->x.result.name);
					  break;
			}
			output("\tSTORE_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
			break;
		case INDIRS:
		case INDIRI: 
		case INDIRP:
			if (p->x.optimized) break;
			if (optimizelevel>=2) output("; INDIRI/P not optimized\n");
			switch (a->x.result.adrmode) {
			case 'C': 
			case 'Z': 
			case 'A':
				output("\tLOAD_%c(%s)\n"
					,indirectmode(a->x.result.name,'A')
					,a->x.result.name);
				break;;
			case 'D': output("\tLOAD_D(%s)\n",a->x.result.name);
					  output("\tSTORE_Z(tmp)\n");
					  output("\tLOAD_I(tmp)\n");
					  break;
			case 'S': output("\tLOAD_P(%s)\n",a->x.result.name);
					  break;
			}
			output("\tSTORE_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
			break;
		case INDIRD: 
		case INDIRF:
			output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tFLOAD1\n");
			output("\tLOAD_%c(%s)\n",addr(r->x.result.adrmode),r->x.result.name);
			output("\tFSTORE\n");
			break;
		case INDIRB:
			output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tSTORE_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
			break;
		case BCOMU:	unary_integer_op("COMPL" );   break;
		case NEGD:  
		case NEGF:	unary_float_op("FNEG"); break;
		case NEGI:
			output("\tLOAD_C(0)\n");
			output("\tSUB_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tSTORE_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
			break;
		case CVCI: 
			output("\tLOADSX_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tSTORE_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
			break;
		case CVCU: 
		case CVSU:
			output("\tLOADZX_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tSTORE_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
		case CVUC: 
		case CVUS: 
		case CVIC: 
			if (optimizelevel<=1 || strcmp(a->x.result.name,p->x.result.name)!=0) {
				output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
				output("\tSTOREB_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
			}
			break;
		case CVPU: 
		case CVUP: 
		case CVIU: 
		case CVUI:
		case CVSI:
		case CVIS:
			if (optimizelevel<=1 || strcmp(a->x.result.name,p->x.result.name)!=0)
				unary_integer_op(NULL);
			break;
		case CVID:
			output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tCIF\n");
			output("\tLOAD_%c(%s)\n",addr(r->x.result.adrmode),r->x.result.name);
			output("\tFSTORE\n");
			break;
		case CVDF: 
		case CVFD:
			if (optimizelevel<=1 || strcmp(a->x.result.name,p->x.result.name)!=0)
				unary_float_op(NULL);
			break;
		case CVDI:
			output("\tLOAD_%c(%s)\n",addr(a->x.result.adrmode),a->x.result.name);
			output("\tFLOAD1\n");
			output("\tCFI\n");
			output("\tSTORE_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
			break;
		case RETD: 
		case RETF:
			output("\tLOAD_%c(%s)\n",addr(a->x.result.adrmode),a->x.result.name);
			output("\tFLOAD1\n");
			break;
		case RETI:
			output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			return_int=1;
			break;
		case RETV:
			break;
		case ADDRGP: 
			if (p->x.optimized) break;
			output("\tLOAD_C(%s)\n",p->syms[0]->x.name);
			output("\tSTORE_%c(%s)\n",p->x.result.adrmode,p->x.result.name);
			break;
		case ADDRFP: 
		case ADDRLP:
			if (p->x.optimized) break;
			output("\tLOAD_A(%s)\n",namefromoffset(p->syms[0]));
			output("\tSTORE_%c(%s)\n",p->x.result.adrmode,p->x.result.name);
			break;
		case CNSTC: 
		case CNSTS:
		case CNSTI: 
		case CNSTU:
		case CNSTP:
			if (p->x.optimized) break;
			output("\tLOAD_C(%s)\n",p->syms[0]->x.name);
			output("\tSTORE_%c(%s)\n",p->x.result.adrmode,p->x.result.name);
			break;
		case JUMPV:
			output("\tJUMP_%c(%s)\n",a->x.result.adrmode, a->x.result.name);
			break;
		case ASGNB:
			output("\tLOAD_%c(%s)\n",b->x.result.adrmode,b->x.result.name);
			output("\tSTORE_Z(op1)\n");
			output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tSTORE_Z(op2)\n");
			output("\tSCOPY(%s)\n",p->syms[0]->x.name);
			break;
		case ASGNC: 
			switch (a->x.result.adrmode) {
			case 'C': break;
			case 'Z': break;
			case 'A': break;
			case 'S': break;
			case 'D': output("\tLOAD_D(%s)\n",a->x.result.name);
					  output("\tSTORE_Z(tmp)\n");
					  break;
			}
			output("\tLOAD_%c(%s)\n",b->x.result.adrmode,b->x.result.name);
			switch (a->x.result.adrmode) {
			case 'C': output("\tSTOREB_D(%s)\n",a->x.result.name); break;
			case 'Z': output("\tSTOREB_I(%s)\n",a->x.result.name); break;
			case 'A': output("\tSTOREB_S(%s)\n",a->x.result.name); break;
			case 'S': output("\tSTOREB_P(%s)\n",a->x.result.name); break;
			case 'D': output("\tSTOREB_I(tmp)\n");		  break;
			}
			break;
		case ASGND: 
		case ASGNF:
			output("\tLOAD_%c(%s)\n",addr(b->x.result.adrmode),b->x.result.name);
			output("\tFLOAD1\n");
			output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tFSTORE\n");
			break;
		case ASGNS:
		case ASGNI: 
		case ASGNP:
			if (p->x.optimized) break;
			if (a->x.result.adrmode=='D') {
				output("\tLOAD_D(%s)\n",a->x.result.name);
				output("\tSTORE_Z(tmp)\n");
			}
			output("\tLOAD_%c(%s)\n",b->x.result.adrmode,b->x.result.name);
			switch (a->x.result.adrmode) {
			case 'C': output("\tSTORE_%c(%s)\n"
						  ,indirectmode(a->x.result.name,'C')
						  ,a->x.result.name); 
				break;
			case 'Z': output("\tSTORE_I(%s)\n",a->x.result.name); break;
			case 'A': output("\tSTORE_S(%s)\n",a->x.result.name); break;
			case 'S': output("\tSTORE_P(%s)\n",a->x.result.name); break;
			case 'D': output("\tSTORE_I(tmp)\n");		  break;
			}
			break;
		case ARGB:
			if (p->x.optimized) break;
			output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tSTORE_Z(op1)\n");
			output("\tLOAD_A(%d)\n",1+argoffset);
			output("\tSTORE_Z(op2)\n");
			output("\tSCOPY(%s)\n",p->syms[0]->x.name);
			argoffset += p->syms[0]->u.c.v.i;
			break;
		case ARGD: 
		case ARGF:
			if (p->x.optimized) break;
			output("\tLOAD_%c(%s)\n",addr(a->x.result.adrmode),a->x.result.name);
			output("\tFLOAD1\n");
			output("\tLOAD_A(%d)\n",1+argoffset);
			output("\tFSTORE\n");
			argoffset += p->syms[0]->u.c.v.i;
			break;
		case ARGI: 
		case ARGP:
			if (p->x.optimized) break;
			output("\tLOAD_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tSTORE_S(%d)\n",1+argoffset);
			argoffset += p->syms[0]->u.c.v.i;
			break;
		case CALLB: 
			argoffset=0;
			output("\tCALL_%c(%s)\n"
				,a->x.result.adrmode
				,a->x.result.name);
			output("\tSTORE_Z(op1)\n");
			output("\tLOAD_%c(%s)\n",b->x.result.adrmode,b->x.result.name);
			output("\tSTORE_Z(op2)\n");
/* où se trouve la longueur de la structure ?? */
			output("\tSCOPY(%s)\n",r->x.result.name);
			break;
		case CALLV:
			argoffset=0;
			output("\tCALL_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			break;
		case CALLD: 
		case CALLF:
			argoffset=0;
			output("\tCALL_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tLOAD_%c(%s)\n",addr(p->x.result.adrmode),p->x.result.name);
			output("\tFSTORE\n");
			break;
		case CALLI:
			argoffset=0;
			output("\tCALL_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output("\tSTORE_%c(%s)\n",p->x.result.adrmode,p->x.result.name);
			break;
		case EQI:	integer_compare("BEQ",a,b);	break;
		case NEI:	integer_compare("BNE",a,b);	break;
		case GEI:	integer_compare("BGE",a,b); break;
		case GEU:	integer_compare("BCS",a,b); break;
	 	case GTI:	integer_compare("BLT",b,a); break;
		case GTU:	integer_compare("BCC",b,a); break;
		case LEI:	integer_compare("BGE",b,a); break;
		case LEU:	integer_compare("BCS",b,a); break;
		case LTI:	integer_compare("BLT",a,b); break;
		case LTU:	integer_compare("BCC",a,b); break;
		case EQD:   
		case EQF:	float_compare("BEQ",a,b); break;
		case NED:   
		case NEF:	float_compare("BNE",a,b); break;
		case GED:   
		case GEF:	float_compare("BPL",a,b); break;
		case GTD:   
		case GTF:	float_compare("BMI",b,a); break;
		case LED:   
		case LEF:	float_compare("BPL",b,a); break;
		case LTD:   
		case LTF:	float_compare("BMI",a,b); break;
		case LABELV:
			output("%s\n", p->syms[0]->x.name); break;
		default: assert(0);
	}
}

Node gen(Node p) {
	Node head, *last;
	for (last = &head; p; p = p->link)
		last = linearize(p, last, 0);
	if (verbose) printf("Dags forest linearized...\n");
	for (p = head; p; p = p->x.next) tmpalloc(p);
	if (verbose) printf("Temporaries allocated...\n");
	return head;
}

void emit(Node head) {
	Node p;
	for (p = head; p; p = p->x.next) emitdag(p);
}

