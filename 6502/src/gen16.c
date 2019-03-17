/* C compiler: Fabrice Frances' 16 bits 65802 code generator */
char *version="16bit code V3.0";
#include "c.h"
#include <string.h>
#include <stdio.h>
#define FLOATSIZE 5
extern void exit(int);

static int offset;			/* current local size */
static int localsize;		/* max size of locals */
static int argoffset;		/* current stack position */
static int argbuildsize;	/* max size of arguments */
static int params;			/* size of formal parameters */
static int nbtmp,nbftmp;	/* number of temporary variables */
static unsigned busy;		/* busy&(1<<t) == 1 if tmp t is used */
static char *callname;		/* current function called name */
static char *NamePrefix;	/* Prefix for all local names */
static int leaf;			/* tells if current function is a leaf function */
static int omit_frame;		/* if no params and no locals */
static int verbose=0;
static int optimizelevel=3;	/* set by command line option -On */
static char *temp="";		/* name of temporary variables */
static Node a,b,r;
static Node lastregnode;	/* last node that used the register */
static Node lastaccunode;	/* last node that used the floating point accumulator */
static int nodenumber;		/* current node number for dag listing */



/* very small peephole optimizer */
static void output(char *format, ...)
{
	static char last[128]="\t";
	char buffer[128];
	va_list ap;

	va_init(ap, format);
	vsprintf(buffer,format, ap);
	va_end(ap);
	if (optimizelevel>=2) {
		if (strncmp(buffer,"\tLOAD_",6)==0 
			&& strncmp(last,"\tSTORE_",7)==0
			&& strcmp(buffer+6,last+7)==0) 
		{
			if (verbose) outs("; peephole optimizer removed LOAD instruction\n");
			return; /* removes unneeded LOAD */
		}
		if (strcmp(buffer,"\tCMP_C(0)\n")==0
			&& strncmp(last,"\tLOAD_",6)==0)
		{
			if (verbose) outs("; peephole optimizer removed CMP_C(0) instruction\n");
			return; /* removes unneeded CMP #0 */
		}
		if (strcmp(buffer,"\tADD_C(1)\n")==0)
		{
			if (verbose) outs("; peephole optimizer replaced ADD_C(1)\n");
			sprintf(buffer,"\tINC\n");
		}
		if (strcmp(buffer,"\tSUB_C(1)\n")==0)
		{
			if (verbose) outs("; peephole optimizer replaced SUB_C(1)\n");
			sprintf(buffer,"\tDEC\n");
		}
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
static char typenames[]=" FDCSIUPVB";

static void printnode(Node p)
{
	if (p) {
		p->x.number=nodenumber++;
		printf("%d. %s%c "
			,p->x.number
			,opnames[(p->op)>>4]
			,typenames[(p->op)&0x0F]);
		switch (generic(p->op)) {
		case ADDRF:
		case ADDRG:
		case ADDRL:
		case CNST:
		case LABEL:
			printf("%s",p->syms[0]->name);
			break;
		case BCOM:
		case CVC:
		case CVD:
		case CVF:
		case CVI:
		case CVP:
		case CVS:
		case CVU:
		case INDIR:
		case NEG:
		case JUMP:
		case ARG:
		case CALL:
			printf("[%d]",p->kids[0]->x.number);
			break;
		case EQ:
		case GE:
		case GT:
		case LE:
		case LT:
		case NE:
			printf("%s ",p->syms[0]->name);
		case ADD:
		case BAND:
		case BOR:
		case BXOR:
		case DIV:
		case LSH:
		case MOD:
		case MUL:
		case RSH:
		case SUB:
		case ASGN:
			printf("[%d,%d]",p->kids[0]->x.number,p->kids[1]->x.number);
			break;
		case RET:
			if (p->kids[0]) printf("[%d]",p->kids[0]->x.number);
			break;
		default: assert(0);
		}
/*
		printf(" -> %s,%c",p->x.result.name,p->x.result.adrmode);
		printf("   count=%d\n",p->count);
*/
		printf("\n");
	}
}

static char indirectmode(char mode)
{
	switch (mode) {
	case 'L': return 'D';
	case 'C': return 'A';
	case 'D': return 'I';
	}
	printf("Addressing mode %c cannot be dereferenced\n",mode);
	assert(0);
	return -1;
}


static Node *linearize(Node p, Node *last, Node next) {
	Node head0=0, head1=0, *last0, *last1;
	if (p && !p->x.visited) {
		Node k0=p->kids[0], k1=p->kids[1];
		p->x.optimized=0;
		p->x.result.type=UNKNOWN;
		p->x.result.name="*******";
		p->x.result.adrmode='*';
		last0 = linearize(k0, &head0, 0);
		last1 = linearize(k1, &head1, 0);
		
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
}

void progend(void) { if (verbose) output("; %s\n",version); }

void defsymbol(Symbol p) {
	p->x.type = GLOBALVAR;
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

void function(Symbol f, Symbol caller[], Symbol callee[], int ncalls) {
	int i;

	fname=f->x.name;
	params=busy=nbtmp=nbftmp=localsize=argbuildsize=offset=argoffset=0;
	for (i = 0; caller[i] && callee[i]; i++) {
		caller[i]->x.type=PARAMETER;
		caller[i]->x.adrmode='L';
		caller[i]->x.name=NULL;
		caller[i]->x.offset=params;
		params+=caller[i]->type->size;
		callee[i]->sclass=AUTO;
		callee[i]->x=caller[i]->x;
	}
	if (verbose) printf("\nGenerating function %s :\n\n",fname);
	leaf=1;
	gencode(caller,callee);
	omit_frame=0;
	if (optimizelevel >= 2
		&& (leaf || argbuildsize+localsize==0)
		&& localsize+nbtmp*2+nbftmp*FLOATSIZE<32-4)
			omit_frame=1;
	if (verbose) printf("Function %s generated.\n\n",fname);

	output("%s\n",fname);
	if (!omit_frame)
		output("\tENTER(%d)\n",argbuildsize+nbftmp*FLOATSIZE+localsize);
/*
	if (isstruct(freturn(f->type))) {
		output("\tLOAD_D(op1)\n");
		output("\tSTORE_Y((fp),6)\n");
	}
*/
	argoffset=0;
	emitcode();
}

void local(Symbol p) {
	if (verbose)  {
		printf("asked local for %s:\n",p->name);
		if (p->temporary) printf("\ttemporary\n");
		if (p->sclass==AUTO) printf("\tAUTO\n");
		if (p->sclass==REGISTER) printf("\tREGISTER\n");
	}
	p->sclass=AUTO;
	p->x.type=LOCALVAR;
	p->x.adrmode = 'L';
	p->x.name=NULL;
	p->x.offset = offset;
	if (verbose) 
		printf("\tallocated at offset %d\n",offset);
	offset+=p->type->size;
}

void address(Symbol q, Symbol p, int n) {
	q->x=p->x;
	if (p->x.adrmode=='L') {
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
	if (verbose) printf("\tneed tmp.\n");
	if ( optype(p->op)!=F && optype(p->op)!=D ) {
		for (t=0;t<16;t++)
			if ((busy&(1<<t))==0) {
				busy|=1<<t;
				if (t>=nbtmp) nbtmp=t+1;
				p->x.result.type=TEMPORARY;
				p->x.result.name=temp;
				p->x.result.offset=t;
				if (optimizelevel>=2 && p->count==1) {
					if (verbose) printf("\tproposed for register storage\n");
					p->x.result.adrmode='R';
					lastregnode=p;
				} else
					p->x.result.adrmode='D';
				return;
			}
	} else
		for (t=0;t<8;t++)
			if ((busy&(1<<(t+16)))==0) {
				busy|=1<<(t+16);
				if (t>=nbftmp) nbftmp=t+1;
				p->x.result.type=TEMPORARY;
				p->x.result.name=temp;
				p->x.result.offset=t+16;
				if (optimizelevel>=2 && p->count==1) {
					if (verbose) printf("\tproposed for accu float storage\n");
					p->x.result.adrmode='R';
					lastaccunode=p;
				} else
					p->x.result.adrmode='D';
				return;
			}
	perror("Too complex expression"); exit(1);
}

static void releasetmp(Node p) {
	if (p) {
		assert(p->count!=0);
		p->count--;
		if (!p->x.optimized) {
			int t = p->x.result.offset;
			if (p->count==0)
				busy&= ~(1<<t);
			if (p->x.result.adrmode=='R') {
				if (t<16 && p!=lastregnode || t>=16 && p!=lastaccunode)
					p->x.result.adrmode='D';
			}
		}
	}
}

static int optimize_indirection(Node p)
{
	Node k=p->kids[0];
	if (optype(p->op)==B || optype(p->op)==C)
		return 0;

	if ( optimizelevel>=1 && p->count==1)	{
		switch (generic(k->op)) {
		case CNST: case ADDRG: case ADDRL: case ADDRF:
			p->x.optimized=1;
			p->x.result=k->x.result;
			p->x.result.adrmode=indirectmode(p->x.result.adrmode);
			if (verbose) 
				printf("\tINDIR optimization level 1 : adrmode %c (%s)\n"
					,p->x.result.adrmode,k->syms[0]->name);
			return 1;
			}
	}
/*
	if ( optimizelevel>=2 ) {
		if (k->count==0 && k->x.optimized && k->x.result.adrmode=='D')
		{

			p->x.optimized=1;
			p->x.result=k->x.result;
			p->x.result.adrmode='I';
			if (verbose) 
				printf("\tINDIR optimization level 2 : adrmode %c\n"
					,p->x.result.adrmode);
			return 1;
		}
	}
*/
	return 0;
}

static void optimize(Node p)
{
	Node k0=p->kids[0], k1=p->kids[1], next=p->x.next;
	switch (p->op) {
	case ARGI:
	case ARGP:
		if (optimizelevel>=3) {
/* Pb par exemple dans int sum(int n) { return sum(n-1)*n;}
			if (!k0->x.optimized) {
				p->x.optimized=1;
				k0->x.optimized=1;
				k0->x.result.type=ARGBUILD;
				k0->x.result.adrmode='D';
				k0->x.result.offset=argoffset;
				if (verbose) 
					printf("ARG optimization level 3 : %s,%c (kid : %s,%c)\n"
						,p->x.result.name,p->x.result.adrmode,k0->x.result.name,k0->x.result.adrmode);
			}
*/
		}
		break;
	case ADDI:
	case ADDU:
	case ADDP:
		if (optimizelevel>=3) {
			if  (  p->count==1
				&& next
				&& ( next->op==ASGNI || next->op==ASGNS || next->op==ASGNP )
				&& next->kids[1]==p
				&& ( k0->op==INDIRI || k0->op==INDIRS || k0->op==INDIRP )
				&& k1->op==CNSTI
				&& strcmp(k1->x.result.name,"1")==0
				&& k0->kids[0]==next->kids[0]
				&& (   k0->kids[0]->op==ADDRLP
					|| k0->kids[0]->op==ADDRFP
					|| k0->kids[0]->op==ADDRGP )
				)
			{
				p->x.optimized=1;
				next->x.optimized=1;
				p->x.result=k0->kids[0]->x.result;
				p->x.result.adrmode=indirectmode(p->x.result.adrmode);
				if (verbose)
					printf("\tOptimization level 3 : INC %s\n",k0->kids[0]->syms[0]->name);
				if (k0->count==1) {
					k0->x.result.adrmode='R';
					lastregnode=k0;
					if (verbose)
						printf("\tOptimization level 3 : old INDIR node proposed for register\n");
				}
			}
		}
		break;
	case SUBI:
	case SUBU:
	case SUBP:
		if (optimizelevel>=3) {
			if  (  p->count==1
				&& next
				&& ( next->op==ASGNI || next->op==ASGNS || next->op==ASGNP )
				&& next->kids[1]==p
				&& ( k0->op==INDIRI || k0->op==INDIRS || k0->op==INDIRP )
				&& k1->op==CNSTI
				&& strcmp(k1->x.result.name,"1")==0
				&& k0->kids[0]==next->kids[0]
				&& (   k0->kids[0]->op==ADDRLP
					|| k0->kids[0]->op==ADDRFP
					|| k0->kids[0]->op==ADDRGP )
				)
			{
				p->x.optimized=1;
				next->x.optimized=1;
				p->x.result=k0->kids[0]->x.result;
				p->x.result.adrmode=indirectmode(p->x.result.adrmode);
				if (verbose)
					printf("\tOptimization level 3 : DEC %s\n",k0->kids[0]->syms[0]->name);
				if (k0->count==1) {
					k0->x.result.adrmode='R';
					lastregnode=k0;
					if (verbose)
						printf("\tOptimization level 3 : old INDIR node proposed for register\n");
				}
			}
		}
		break;
	case ASGNS:
	case ASGNI:
	case ASGNP:
		if (p->x.optimized && verbose)
			printf("\tOptimization level 3 : ASGN previously removed\n");
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
					printf("\tOptimized %s node: %s,%c\n"
						,opnames[(p->op)>>4],p->syms[0]->name,p->x.result.adrmode);
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
	case LSH:
	case RSH:
	case DIV:
	case MOD:
	case MUL:
	case SUB:
	case GE:
	case LT:
		if (p->kids[1]->x.result.adrmode=='R') {
			p->kids[1]->x.result.adrmode='D';
			if (verbose) printf("\tkid 1's register allocation cancelled\n");
		}
		break;
	case INDIR:
	case ASGN:
	case GT:
	case LE:
		if (p->kids[0]->x.result.adrmode=='R') {
			p->kids[0]->x.result.adrmode='D';
			if (verbose) printf("\tkid 0's register allocation cancelled\n");
		}
		break;
	case ARG:
		argoffset += p->syms[0]->u.c.v.i;
		if (argoffset > argbuildsize)
			argbuildsize = argoffset;
		break;
	case CALL:
		leaf = 0;
		argoffset = 0;
		break;
	}
	if (verbose) printnode(p);
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
	if (mode=='A') return 'C';
	else if (mode=='D') return 'L';
	else if (mode=='I') return 'D';
	else printf("Error: taking effective address of %c mode\n",mode);
	return 0;
}

static char *finalname(Xsymbol *px)
{
	if (omit_frame) {
		int base=0;
		if (px->type==ARGBUILD) assert(0);
		if (px->type==TEMPORARY && px->offset<16)
			px->name=stringf("%d",px->offset*2);
		base+=2*nbtmp;
		if (px->type==TEMPORARY && px->offset>=16)
			px->name=stringf("%d",base+(px->offset-16)*FLOATSIZE);
		base+=FLOATSIZE*nbftmp;
		if (px->type==LOCALVAR)
			px->name=stringf("%d",base+px->offset);
		base=32;
		if (px->type==PARAMETER)
			px->name=stringf("%d",base+px->offset);
		return px->name;
	} else {
		int base=32;
		if (px->type==TEMPORARY && px->offset<16)
			px->name=stringf("%d",px->offset*2);
		if (px->type==ARGBUILD)
			px->name=stringf("%d",base+px->offset);
		base+=argbuildsize;
		if (px->type==TEMPORARY && px->offset>=16)
			px->name=stringf("%d",base+(px->offset-16)*FLOATSIZE);
		base+=FLOATSIZE*nbftmp;
		if (px->type==LOCALVAR)
			px->name=stringf("%d",base+px->offset);
		base+=localsize;
		if (px->type==PARAMETER)
			px->name=stringf("%d",base+px->offset);
		return px->name;
	}
}

static void output_load(char adrmode, char *name)
{
	if (adrmode!='R')
		output("\tLOAD_%c(%s)\n",adrmode,name);
}

static void output_fload(Node a)
{
	if (a->x.result.adrmode!='R') {
		output_load(addr(a->x.result.adrmode),a->x.result.name);
		output("\tFLOAD1\n");
	}
}

static void output_fload2(Node a)
{
	if (a->x.result.adrmode=='R') {
		output("\tFSHIFT\n");
	} else {
		output_load(addr(a->x.result.adrmode),a->x.result.name);
		output("\tFLOAD2\n");
	}
}

static void output_store(char adrmode, char *name)
{
	if (adrmode!='R')
		output("\tSTORE_%c(%s)\n",adrmode,name);
}

static void output_fstore(Node r)
{
	if (r->x.result.adrmode!='R') {
		output_load(addr(r->x.result.adrmode),r->x.result.name);
		output("\tFSTORE\n");
	}
}

static void binary_integer_op(char *inst) {
	if (a->x.result.adrmode=='R')
		output("\t%s_%c(%s)\n",inst,b->x.result.adrmode,b->x.result.name);
	else if (b->x.result.adrmode=='R')
		output("\t%s_%c(%s)\n",inst,a->x.result.adrmode,a->x.result.name);
	else {
		output_load(a->x.result.adrmode,a->x.result.name);
		output("\t%s_%c(%s)\n",inst,b->x.result.adrmode,b->x.result.name);
	}
	output_store(r->x.result.adrmode,r->x.result.name);
}

static void binary_float_op(char *inst) {
	output_fload2(a);
	output_fload(b);
	output("\t%s\n",inst);
	output_fstore(r);
}

static void integer_muldiv(char *inst) {
	switch (a->x.result.adrmode) {
	case 'D': output("\tPUSH_D(%s)\n",a->x.result.name); break;
	case 'C': output("\tPUSH_C(%s)\n",a->x.result.name); break;
	case 'A': output("\tLOAD_A(%s)\n",a->x.result.name);
	case 'R': output("\tPUSH\n"); break;
	default: assert(0);
	}
	switch (b->x.result.adrmode) {
	case 'D': output("\tPUSH_D(%s)\n",b->x.result.name); break;
	case 'C': output("\tPUSH_C(%s)\n",b->x.result.name); break;
	case 'A': 
		output("\tLOAD_A(%s)\n",b->x.result.name);
		output("\tPUSH\n"); 
		break;
	default: assert(0);
	}
	output("\t%s\n",inst);
	output_store(r->x.result.adrmode,r->x.result.name);
}

static void unary_integer_op(char *inst) {
	output_load(a->x.result.adrmode,a->x.result.name);
	if (inst) output("\t%s\n",inst);
	output_store(r->x.result.adrmode,r->x.result.name);
}

static void unary_float_op(char *inst) {
	output_fload(a);
	if (inst) output("\t%s\n",inst);
	output_fload(r);
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
	output_load(op1->x.result.adrmode,op1->x.result.name);
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
	output_fload(op1);
	output_fload2(op2);
	output("\tFCOMP\n");
	output("\t%s(%s)\n",inst,r->syms[0]->x.name);
}

void emitdag(Node p) {
	int i;
	a = p->kids[0]; b = p->kids[1]; r=p;
	if (a) a->x.result.name=finalname(&a->x.result);
	if (b) b->x.result.name=finalname(&b->x.result);
	p->x.result.name=finalname(&p->x.result);
	if (verbose) 
		outs(stringf("; %s%c\n",opnames[(p->op)>>4],typenames[(p->op)&0x0F]));

	switch (p->op) {
		case BANDU:	binary_integer_op("AND");   break;
		case BORU:	binary_integer_op("OR");   break;
		case BXORU:	binary_integer_op("XOR");   break;
		case ADDD:  
		case ADDF:	binary_float_op("FADD");  break;
		case ADDI:
		case ADDP:
		case ADDU:
			if (p->x.optimized)
				output("\tINC_%c(%s)\n",p->x.result.adrmode,p->x.result.name);
			else
				binary_integer_op("ADD");
			break;
		case SUBD:  
		case SUBF:	binary_float_op("FSUB");  break;
		case SUBI:  
		case SUBP:  
		case SUBU:
			if (p->x.optimized)
				output("\tDEC_%c(%s)\n",p->x.result.adrmode,p->x.result.name);
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
			if (optimizelevel>=2 && b->x.result.adrmode=='C') {
				if (strcmp(b->x.result.name,"0")==0) unary_integer_op(NULL);
				else if (strcmp(b->x.result.name,"1")==0) unary_integer_op("RSH1");
				else if (strcmp(b->x.result.name,"2")==0) unary_integer_op("RSH2");
				else if (strcmp(b->x.result.name,"3")==0) unary_integer_op("RSH3");
				else if (strcmp(b->x.result.name,"4")==0) unary_integer_op("RSH4");
				else if (strcmp(b->x.result.name,"5")==0) unary_integer_op("RSH5");
				else if (strcmp(b->x.result.name,"6")==0) unary_integer_op("RSH6");
				else if (strcmp(b->x.result.name,"7")==0) unary_integer_op("RSH7");
				else if (strcmp(b->x.result.name,"8")==0) unary_integer_op("RSH8");
				else if (strcmp(b->x.result.name,"9")==0) unary_integer_op("RSH9");
				else if (strcmp(b->x.result.name,"10")==0) unary_integer_op("RSH10");
				else if (strcmp(b->x.result.name,"11")==0) unary_integer_op("RSH11");
				else if (strcmp(b->x.result.name,"12")==0) unary_integer_op("RSH12");
				else if (strcmp(b->x.result.name,"13")==0) unary_integer_op("RSH13");
				else if (strcmp(b->x.result.name,"14")==0) unary_integer_op("RSH14");
				else if (strcmp(b->x.result.name,"15")==0) unary_integer_op("RSH15");
				else {
					output("\tLOAD_C(0)\n");
					output_store(r->x.result.adrmode,r->x.result.name);
				}
			} else binary_integer_op("RSH");
			break;
		case LSHI:  
		case LSHU:
			if (optimizelevel>=2 && b->x.result.adrmode=='C') {
				if (strcmp(b->x.result.name,"0")==0) unary_integer_op(NULL);
				else if (strcmp(b->x.result.name,"1")==0) unary_integer_op("LSH1");
				else if (strcmp(b->x.result.name,"2")==0) unary_integer_op("LSH2");
				else if (strcmp(b->x.result.name,"3")==0) unary_integer_op("LSH3");
				else if (strcmp(b->x.result.name,"4")==0) unary_integer_op("LSH4");
				else if (strcmp(b->x.result.name,"5")==0) unary_integer_op("LSH5");
				else if (strcmp(b->x.result.name,"6")==0) unary_integer_op("LSH6");
				else if (strcmp(b->x.result.name,"7")==0) unary_integer_op("LSH7");
				else if (strcmp(b->x.result.name,"8")==0) unary_integer_op("LSH8");
				else if (strcmp(b->x.result.name,"9")==0) unary_integer_op("LSH9");
				else if (strcmp(b->x.result.name,"10")==0) unary_integer_op("LSH10");
				else if (strcmp(b->x.result.name,"11")==0) unary_integer_op("LSH11");
				else if (strcmp(b->x.result.name,"12")==0) unary_integer_op("LSH12");
				else if (strcmp(b->x.result.name,"13")==0) unary_integer_op("LSH13");
				else if (strcmp(b->x.result.name,"14")==0) unary_integer_op("LSH14");
				else if (strcmp(b->x.result.name,"15")==0) unary_integer_op("LSH15");
				else {
					output("\tLOAD_C(0)\n");
					output_store(r->x.result.adrmode,r->x.result.name);
				}
			} else binary_integer_op("LSH");
			break;
		case INDIRC: 
			if (p->x.optimized) break;
			switch (a->x.result.adrmode) {
			case 'C': output("\tLOADB_A(%s)\n",a->x.result.name); break;
			case 'L': output("\tLOADB_D(%s)\n",a->x.result.name); break;
			case 'D': output("\tLOADB_I(%s)\n",a->x.result.name); break;
			case 'A': output("\tLOAD_A(%s)\n",a->x.result.name);
					  output("\tSTORE_D(tmp)\n");
					  output("\tLOADB_I(tmp)\n");
					  break;
			default: assert(0);
			}
			output_store(r->x.result.adrmode,r->x.result.name);
			break;
		case INDIRS:
		case INDIRI: 
		case INDIRP:
			if (p->x.optimized) break;
			switch (a->x.result.adrmode) {
			case 'C': output_load('A',a->x.result.name); break;
			case 'L': output_load('D',a->x.result.name); break;
			case 'D': output_load('I',a->x.result.name); break;
			case 'A': output_load('A',a->x.result.name);
					  output("\tSTORE_D(tmp)\n");
					  output("\tLOAD_I(tmp)\n");
					  break;
			default: assert(0);
			}
			output_store(r->x.result.adrmode,r->x.result.name);
			break;
		case INDIRD: 
		case INDIRF:
			if (p->x.optimized) break;
			output_load(a->x.result.adrmode,a->x.result.name);
			output("\tFLOAD1\n");
			output_fstore(r);
			break;
		case INDIRB:
			output_load(a->x.result.adrmode,a->x.result.name);
			output_store(r->x.result.adrmode,r->x.result.name);
			break;
		case BCOMU:	unary_integer_op("COMPL" );   break;
		case NEGD:  
		case NEGF:	unary_float_op("FNEG"); break;
		case NEGI:
			output_load(a->x.result.adrmode,a->x.result.name);
			output("\tXOR_C($FFFF)\n");
			output("\tINC\n");
			output_store(r->x.result.adrmode,r->x.result.name);
			break;
		case CVCI:
			output_load(a->x.result.adrmode,a->x.result.name);
			output("\tCVCI\n");
			output_store(r->x.result.adrmode,r->x.result.name);
			break;
		case CVCU: 
		case CVUC: 
		case CVIC: 
/*
			if (optimizelevel<=1 || strcmp(a->x.result.name,p->x.result.name)!=0) {
				output_load(a->x.result.adrmode,a->x.result.name);
				output("\tSTOREB_%c(%s)\n",r->x.result.adrmode,r->x.result.name);
			}
*/
			break;
		case CVPU: 
		case CVUP: 
		case CVUI:
		case CVUS: 
		case CVIU: 
		case CVIS:
		case CVSI:
		case CVSU:
			if (optimizelevel<=1 || strcmp(a->x.result.name,p->x.result.name)!=0)
				unary_integer_op(NULL);
			break;
		case CVID:
			output_load(a->x.result.adrmode,a->x.result.name);
			output("\tCIF\n");
			output_fstore(r);
			break;
		case CVDF: 
		case CVFD:
			if (optimizelevel<=1 || strcmp(a->x.result.name,p->x.result.name)!=0)
				unary_float_op(NULL);
			break;
		case CVDI:
			output_fload(a);
			output("\tCFI\n");
			output_store(r->x.result.adrmode,r->x.result.name);
			break;
		case RETD: 
		case RETF:
			output_fload(a);
			if (!omit_frame) output("\tLEAVE\n");
			output("\tRET\n");
			break;
		case RETI:
			output_load(a->x.result.adrmode,a->x.result.name);
			if (!omit_frame) output("\tLEAVE\n");
			output("\tRET\n");
			break;
		case RETV:
			if (!omit_frame) output("\tLEAVE\n");
			output("\tRET\n");
			break;
		case ADDRGP: 
			if (p->x.optimized) break;
			output("\tLOAD_C(%s)\n",p->syms[0]->x.name);
			output_store(p->x.result.adrmode,p->x.result.name);
			break;
		case ADDRFP: 
		case ADDRLP:
			if (p->x.optimized) break;
			output("\tLOAD_L(%s)\n",finalname(&p->syms[0]->x));
			output_store(p->x.result.adrmode,p->x.result.name);
			break;
		case CNSTC: 
		case CNSTS:
		case CNSTI: 
		case CNSTU:
		case CNSTP:
			if (p->x.optimized) break;
			output("\tLOAD_C(%s)\n",p->syms[0]->x.name);
			output_store(p->x.result.adrmode,p->x.result.name);
			break;
		case JUMPV:
			output("\tJUMP_%c(%s)\n",a->x.result.adrmode, a->x.result.name);
			break;
		case ASGNB:
			output_load(b->x.result.adrmode,b->x.result.name);
			output("\tSTORE_D(op1)\n");
			output_load(a->x.result.adrmode,a->x.result.name);
			output("\tSTORE_D(op2)\n");
			output("\tSCOPY(%s)\n",p->syms[0]->x.name);
			break;
		case ASGNC: 
			if (a->x.result.adrmode=='A') {
				output("\tLOAD_A(%s)\n",a->x.result.name);
				output("\tSTORE_D(tmp)\n");
			}
			output_load(b->x.result.adrmode,b->x.result.name);
			switch (a->x.result.adrmode) {
			case 'C': output("\tSTOREB_A(%s)\n",a->x.result.name); break;
			case 'L': output("\tSTOREB_D(%s)\n",a->x.result.name); break;
			case 'D': output("\tSTOREB_I(%s)\n",a->x.result.name); break;
			case 'A': output("\tSTOREB_I(tmp)\n",a->x.result.name); break;
			}
			break;
		case ASGND: 
		case ASGNF:
			output_fload(b);
			output_load(a->x.result.adrmode,a->x.result.name);
			output("\tFSTORE\n");
			break;
		case ASGNS:
		case ASGNI: 
		case ASGNP:
			if (p->x.optimized) break;
			if (a->x.result.adrmode=='A') {
				output("\tLOAD_A(%s)\n",a->x.result.name);
				output("\tSTORE_D(tmp)\n");
			}
			output_load(b->x.result.adrmode,b->x.result.name);
			switch (a->x.result.adrmode) {
			case 'C': output("\tSTORE_A(%s)\n",a->x.result.name); break;
			case 'L': output("\tSTORE_D(%s)\n",a->x.result.name); break;
			case 'D': output("\tSTORE_I(%s)\n",a->x.result.name); break;
			case 'A': output("\tSTORE_I(tmp)\n",a->x.result.name); break;
			}
			break;
		case ARGB:
			if (p->x.optimized) break;
			output_load(a->x.result.adrmode,a->x.result.name);
			output("\tSTORE_D(op1)\n");
			output("\tLOAD_L(%d)\n",32+argoffset);
			output("\tSTORE_D(op2)\n");
			output("\tSCOPY(%s)\n",p->syms[0]->x.name);
			argoffset += p->syms[0]->u.c.v.i;
			break;
		case ARGD: 
		case ARGF:
			if (p->x.optimized) break;
			output_fload(a);
			output("\tLOAD_L(%d)\n",32+argoffset);
			output("\tFSTORE\n");
			argoffset += p->syms[0]->u.c.v.i;
			break;
		case ARGI: 
		case ARGP:
			if (p->x.optimized) break;
			output_load(a->x.result.adrmode,a->x.result.name);
			output("\tSTORE_D(%d)\n",32+argoffset);
			argoffset += p->syms[0]->u.c.v.i;
			break;
		case CALLB: 
			argoffset=0;
			output("\tCALL_%c(%s)\n"
				,a->x.result.adrmode
				,a->x.result.name);
			output("\tSTORE_D(op1)\n");
			output_load(b->x.result.adrmode,b->x.result.name);
			output("\tSTORE_D(op2)\n");
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
			output_load(addr(r->x.result.adrmode),r->x.result.name);
			output("\tFSTORE\n");
			break;
		case CALLI:
			argoffset=0;
			output("\tCALL_%c(%s)\n",a->x.result.adrmode,a->x.result.name);
			output_store(r->x.result.adrmode,r->x.result.name);
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
	if (verbose) printf("Dags forest linearized:\n");
	nodenumber=0;
	for (p = head; p; p = p->x.next) tmpalloc(p);
	if (verbose) printf("Temporaries allocated...\n\n");
	return head;
}

void emit(Node head) {
	Node p;
	for (p = head; p; p = p->x.next) emitdag(p);
}

