
%$L-,D-,C-,T-\

(*&Rl   for PFORM, make reserved words lower case *)
(*&Nu   make non-reserved words upper case *)
(*&Xu   make procedure names upper case *)
(*&I2   indent 2 spaces per level *)
(*&G2   indent contents of BEGIN/END 2 extra spaces *)

%$V:001400000276B\

program PASCMP;

  INCLUDE 'pasprm.pas';   (* set up tops10 and tops20 *)

{I hereby make a public apology for the lousy documentation in this
compiler.  My only justification is that it was essentially undocumented
when I got it.  I have added comments as I came to understand certain
pieces of code.  However I have been a bit hesitant to do so, since a
number of these routines have somewhat obscure sideeffects.  Thus I
have only documented things when I was fairly sure I really knew what
they were doing.
    - C. Hedrick}

  %************************************************************
   *  Credits:						      *
   *                                                          *
   * 	    Original Portable Pascal System:                  *
   *        AUTHOR:   URS AMMANN                              *
   *                  FACHGRUPPE COMPUTERWISSENSCHAFTEN       *
   *                  EIDG. TECHNISCHE HOCHSCHULE             *
   *          CH-8006 ZUERICH                                 *
   *                                                          *
   *        CODE GENERATION FOR DECSYSTEM 10 BY               *
   *        C.-O. GROSSE-LINDEMANN, F.-W. LORENZ,             *
   *        H.-H. NAGEL, P.J. STIRL                           *
   *                                                          *
   *        MODIFICATIONS TO GENERATE RELOCATABLE OBJECT CODE *
   *        BY E. KISICKI (DEC 74)                            *
   *                                                          *
   *        DEBUG SYSTEM BY P. PUTFARKEN (DEC 74)             *
   *                                                          *
   *        INSTITUT FUER INFORMATIK, D-2 HAMBURG 13,         *
   *        SCHLUETERSTRASSE 70 / GERMANY                     *
   *                                                          *
   *        Maintenance and new runtimes:  Charles Hedrick    *
   *	    Laboratory for Computer Science Research	      *
   *	    Rutgers University				      *
   *	    New Brunswick, NJ  08903			      *
   *							      *
   *        Extended addressing:  Norm Samuelson	      *
   *	    Sandia National Laboratory			      *
   *        Division 2644				      *
   *        Albuquerque, NM  87185			      *
   *                                                          *
   ************************************************************\

  (* The info on "HOW TO GENERATE A NEW PASCAL COMPILER" was removed,
   * since it was obsolete anyway.  refer to the .CTL files supplied
   * by Hedrick for this info. *)

  %!    HINTS FOR INTERPRETING ABBREVIATED IDENTIFIERS
   !     BRACK  : BRACKET "[ ]"      IX  : INDEX
   !     C  : CURRENT                L  : LOCAL
   !     C  : COUNTER                L  : LEFT
   !     CST : CONSTANT              PARENT  : "( )"
   !     CTP : IDENTIFIER POINTER    P/PTR  : POINTER
   !     EL : ELEMENT                P/PROC  : PROCEDURE
   !     F  : FORMAL                 R  : RIGHT
   !     F  : FIRST                  S  : STRING
   !     F  : FILE                   SY  : SYMBOL
   !     F/FUNC : FUNCTION           V  : VARIABLE
   !     G  : GLOBAL                 V  : VALUE
   !     ID  : IDENTIFIER
   !     REL  : RELATIVE             REL  : RELOCATION\

{The edit history is now EDIT-HISTORY.TXT}

const
  HEADER = 'PASCAL %14(276)';
  PAS_VERSION = 14B;        {sets %PASVR in .rel files, used to
			    prevent mixing incompatible changes}

{Warning to modifiers: code at WRTWORD depends upon the fact that
 NIL_VALUE < PROGRST, and code in SELECTOR depends upon the fact that
 NIL_VALUE = 0.  These are document in situ.}
  NIL_VALUE = 0;
  FIX_UP = 0;               {flag for value that will be fixed up later}
  IFIW = 400000B;           {Instruction Format Indirect Word}
  MAX_SECTION = 37B;        {Limit for the KL10}
  PREV_BASIS = -1;          {for non-extended: dynamic sup,,lexical sup}
			    {for extended, lexical sup}
  DYNAMIC_SUP = -2;	    {for extended: dynamic sup}
  CURR_TOPP = -3;	    {for extended: current P, only need in proc's that
				have nonlocal goto's into them}
  CREF_BUG_LIMIT=5;         {until the bug in cref is fixed, limit PROC IDs
			    to 5 characters, so names will be meaningful}

  MAXNAMELEN = 200;
  DISPLIMIT = 20; MAXLEVEL = 8;
  STRGLGTH = 120; BITMAX = 36;
  SIZEOFFILEBLOCK=44B;      {plus size of component}
  CHCNTMAX = 132;           %maximum of characters in one line\
  LEFT = 2; RIGHT = 1; BOTH = 3; NO = 0;
(* 257 - fix ambiguous references *)
  LOWSEG = 4; XSEG = 5;

{At the moment we have a mixture of traditional LINK methods and new
 PSECT'ed ones.  Normally we use 0 .. 3 to indicate relocation.  In
 that case, addresses < 400000 are lowseg, and those above are
 high-seg.  Those above 1,,0 are xseg.  However when we are generating
 code, we sometimes use LOWSEG and XSEG, to make things unambiguous.
 Those values will only be put into ATTR's, and thus we hope will only
 end up being used as arguments to MACRO.  At the moment only MACRO
 knows how to handle them, and it turns them into more traditional
 kinds of relocation}

  %KONSTANTEN VON BODY: \
  %*********************\

  HWCSTMAX = 377777B;  LABMAX = 20;
  MAXERR = 4;  BASEMAX = 71;  CHARMAX = 177B;

  %ADDRESSES:
   **********\

  TAC0=0;         %SCRATCH REGISTER\
  TAC1=1;         %SCRATCH REGISTER\
  TAC2=2;         %AC2 when used as temporary\
  REGIN=1;        %INITIALIZE REGC\
  PARREGCMAX=6;   %HIGHEST REGISTER USED FOR PARAMETERS\
  WITHIN=13B;     %FIRST REGISTER FOR WITHSTACK\
  BIXREG=14B;     %REGISTER USED AS INDEX FOR BYTE POINTERS\
  DYNSUP=14B;     % ALSO USED IN ROUTINE CALLS TO SAVE DYNAMIC SUPERIOR\
  NEWREG=15B;	{not used for Tops-20}
  BASIS=16B;      %BASIS ADDRESS STACK ie: frame pointer\
  TOPP=17B;       %FIRST FREE PLACE IN DATASTACK\
  PROGRST = 145B; %LOC 140B..144B RESERVED FOR DEBUG-PROGR.\
  MAXADDR = 777777B;
  MAXXADDR = 7777777777B; {Maximum extended address}
  (* include mnemonics for all the opcodes used by the compiler *)
  %OP-CODES\
  OP_CALLI =  47B;        OP_OPEN  =  50B;
  OP_TTCALL=  51B;        OP_IN    =  56B;
  OP_OUT   =  57B;        OP_CLOSE =  70B;   OP_LOOKUP=  76B;
  OP_ENTER =  77B;        OP_JSYS  = 104B;   OP_ADJSP = 105B;
  OP_DMOVE = 120B;        OP_FIX   = 122B;
  OP_XTND  = 123B;        OP_DMOVEM= 124B;
  OP_FIXR  = 126B;        OP_FLTR  = 127B;
  OP_IBP   = 133B;        OP_ADJBP = 133B;
  OP_ILDB  = 134B;        OP_LDB   = 135B;
  OP_IDPB  = 136B;        OP_DPB   = 137B;
  OP_FADR  = 144B;        OP_FSBR  = 154B;
  OP_FMPR  = 164B;        OP_FDVR  = 174B;
  OP_MOVE  = 200B;        OP_MOVEI = 201B;
  OP_MOVEM = 202B;        OP_MOVSI = 205B;
  OP_MOVN  = 210B;        OP_MOVNI = 211B;   OP_MOVM  = 214B;
  OP_IMUL  = 220B;        OP_IMULI = 221B;
  OP_IDIV  = 230B;        OP_IDIVI = 231B;
  OP_ASH   = 240B;        OP_ROT   = 241B;
  OP_LSH   = 242B;        OP_LSHC  = 246B;
  OP_EXCH  = 250B;        OP_BLT   = 251B;   OP_AOBJN = 253B;
  OP_JRST  = 254B;        OP_HALT  = OP_JRST;
  OP_JFCL  = 255B;        OP_XCT   = 256B;
  OP_PUSHJ = 260B;        OP_PUSH  = 261B;   OP_POP   = 262B;
  OP_POPJ  = 263B;        OP_JSP   = 265B;   OP_JSA   = 266B;
  OP_ADD   = 270B;        OP_ADDI  = 271B;
  OP_SUB   = 274B;        OP_SUBI  = 275B;
  OP_CAI   = 300B;        OP_CAIL  = 301B;   OP_CAIE  = 302B;
  OP_CAILE = 303B;        OP_CAIA  = 304B;   OP_CAIGE = 305B;
  OP_CAIN  = 306B;        OP_CAIG  = 307B;
  OP_CAML  = 311B;        OP_CAME  = 312B;   OP_CAMLE = 313B;
  OP_CAMGE = 315B;        OP_CAMN  = 316B;   OP_CAMG  = 317B;
  OP_JUMP  = 320B;        OP_JUMPE = 322B;
  OP_JUMPA = 324B;        OP_JUMPN = 326B;
  OP_SKIPE = 332B;        OP_SKIPA = 334B;   OP_SKIPN = 336B;
  OP_AOJ   = 340B;        OP_AOJA  = 344B;   OP_AOS   = 350B;
  OP_SOJ   = 360B;        OP_SOJLE = 363B;   OP_SOJGE = 365B;
  OP_SOJG  = 367B;        OP_SOS   = 370B;   OP_SOSGE = 375B;
  OP_SETZ  = 400B;        OP_SETZM = 402B;   OP_SETZB = 403B;
  OP_AND   = 404B;        OP_ANDI  = 405B;   OP_ANDB  = 407B;
  OP_ANDCA = 410B;        OP_ANDCAI= 411B;   OP_SETM  = 414B;
  OP_XMOVEI= 415B;        OP_ANDCM = 420B;   
  OP_SETA  = 424B;	  OP_XOR   = 430B;
  OP_IOR   = 434B;        OP_IORM  = 436B;   OP_EQV   = 444B;
  OP_SETCA = 450B;        OP_SETO  = 474B;
  OP_SETOM = 476B;        OP_SETOB = 477B;
  OP_HLL   = 500B;        OP_XHLLI = 501B;
  OP_HRL   = 504B;        OP_HRLI  = 505B;
  OP_HRLM  = 506B;        OP_HRLS  = 507B;
  OP_HLLZ  = 510B;        OP_HLLZM = 512B;   OP_HLLZS = 513B;
  OP_HRLZ  = 514B;        OP_HRLZI = 515B;
  OP_HRR   = 540B;        OP_HRRI  = 541B;   OP_HRRM  = 542B;
  OP_HLR   = 544B;        OP_HLRS  = 547B;
  OP_HRRZ  = 550B;        OP_HRRZM = 552B;   OP_HRRZS = 553B;
  OP_HLRZ  = 554B;        OP_HLRZM = 556B;
  OP_HRROI = 561B;        OP_HRREI = 571B;   OP_TRN   = 600B;
  OP_TRNE  = 602B;        OP_TLNE  = 603B;
  OP_TRZ   = 620B;        OP_TLZ = 621B;     OP_TDZA  = 634B;
  OP_TLC   = 641B;	  OP_TLO = 661B;
  %extended instruction set\
  XOP_XBLT = 020000000000B;   {for EXTEND ac,[XBLT]}
  %CALLI AND JSYS CALLS\
  CALLI_CORE   =  11B;    CALLI_RESET  =   0B;    CALLI_APRENB =  16B;
  CALLI_RUNTIM =  27B;    CALLI_MSTIME =  23B;    CALLI_SETUWP =  36B;
  CALLI_EXIT   =  12B;
  JSYS_HALTF   = 170B;    JSYS_RUNTM   =  15B;
  JSYS_TIME    =  14B;    JSYS_RESET   = 147B;

  MAXCCLSW = 4;  {maximum offset from %CCLSW used by this code}
type

  %BASIC SYMBOLS\
  %*************\

  SYMBOL = (IDENT,INTCONST,REALCONST,STRINGCONST,NOTSY,MULOP,ADDOP,RELOP,
	    LPARENT,RPARENT,LBRACK,RBRACK,COMMA,SEMICOLON,PERIOD,ARROW,
	    COLON,BECOMES,LABELSY,CONSTSY,TYPESY,VARSY,VALUESY,FUNCTIONSY,
	    PROCEDURESY,SETSY,PACKEDSY,ARRAYSY,RECORDSY,FILESY,
	    FORWARDSY,PROGRAMSY,INCLUDESY,
	    BEGINSY,IFSY,CASESY,REPEATSY,WHILESY,FORSY,WITHSY,LOOPSY,
	    GOTOSY,EXITSY,ENDSY,ELSESY,UNTILSY,OFSY,DOSY,TOSY,DOWNTOSY,
	    EXTERNSY,PASCALSY,FORTRANSY,ALGOLSY,COBOLSY,
	    THENSY,OTHERSY,INITPROCSY,OTHERSSY);

  OPERATOR = (MUL,RDIV,ANDOP,IDIV,IMOD,PLUS,MINUS,OROP,LTOP,LEOP,GEOP,GTOP,
	      NEOP,EQOP,INOP,NOOP);

  SETOFSYS = set of SYMBOL;

  SUPPORTS = (FIRSTSUPPORT,STACKOVERFLOW,DEBSTACK,BADPOINT,ALLOCATE,
	      CLEARALLOC,DEALLOCATE,LOCALALLOC,WITHFILEDEALLOCATE,EXITGOTO,
	      EXITPROGRAM,GETLINE,GETFILE,PUTLINE,PUTFILE,PUTXFILE,
	      RESETFILE,REWRITEFILE,RESETSTRING,REWRITESTRING,
	      GETCHARACTER,PUTPAGE,ERRORINASSIGNMENT,FILEUNINITIALIZED,
	      INITFILEBLOCK,WRITEPACKEDSTRING,WRITESTRING,WRITEBOOLEAN,
	      READCHARACTER,READINTEGER,READREAL,READRECORD,WRITERECORD,
	      WRITESCALAR,BREAKOUTPUT,OPENTTY,INITIALIZEDEBUG,ENTERDEBUG,
	      INDEXERROR,WRITEOCTAL,WRITEINTEGER,WRITEREAL,
	      WRITEHEXADECIMAL,WRITECHARACTER,CONVERTINTEGERTOREAL,
	      CONVERTREALTOINTEGER,CLOSEFILE,READSTRING,READPACKEDSTRING,
	      READFILENAME,NAMEFILE,DISFILE,UPFILE,APFILE,READDUMP,
	      WRITEDUMP,SETIN,SETOUT,BREAKINPUT,SETPOSF,CURPOSF,
	      NEXTBLOCKF,SPACELEFTF,GETXF,DELFILE,RELFILE,INITMEM,
	      INITFILES,GETDAYTIME,DEBUGCALL,LASTSUPPORT);

  %CONSTANTS\
  %*********\

  CSTCLASS = (INT,REEL,PSET,CPSET,STRD,STRG,XINT,CARR,QSET1,QSETD1,QSET2,
		QSETD2);
{An XINT is an integer value that needs to be relocated by XADDRSYM}
{CARR is a dummy for use below.}
{QSETxx are for 4-word sets.  They are not actual contant types, but rather
 instructions to DEPCST.  QSET1 is used after a pair of instructions
 refering to the 1st two words of such a set.  QSET2 is used after a pair
 refering to the 2nd two words.  QSETDx is used after a single DMOVE or
 DMOVEM. CPSET is the real type for 4-word sets.}

  CSP = ^ CONSTNT;
  STRGARR = packed array [1..STRGLGTH] of CHAR;
  CONSTNT = record
	      SELFCSP: CSP;
	      NOCODE: BOOLEAN;
	      case CCLASS: CSTCLASS of
		   XINT,
		   INT : (INTVAL: INTEGER;
			  INTVAL1:INTEGER; %TO ACCESS SECOND WORD OF PVAL\
			  INTVAL2:INTEGER;
			  INTVAL3:INTEGER);
		   CARR: (CARVAL: array[0..3] of
				    packed array [0..35] of BOOLEAN);
		   REEL: (RVAL: REAL);
		   PSET: (PVAL: set of 0..71);
{DUMVAL is used only for bootstrapping, as set of CHAR is too small
 in the old compiler}
		   CPSET: (CPVAL: set of CHAR;
			   DUMVAL: array[1..2]of integer);
		   STRD,
		   STRG: (SLGTH: 0..STRGLGTH;
			  SVAL: STRGARR)
	    end;

  VALU = record
	   case BOOLEAN of
		TRUE:   (IVAL: INTEGER);
		FALSE:  (VALP: CSP)
	 end;

  %DATA STRUCTURES\
  %***************\

  LEVRANGE = 0..MAXLEVEL; ADDRRANGE = 0..MAXADDR; XADDRRANGE = 0..MAXXADDR;
  INSTRANGE = 0..677B;
  RADIXRANGE = 0..37777777777B; FLAGRANGE = 0..17B;
  BITRANGE = 0..BITMAX; ACRANGE = 0..15;
  IBRANGE = 0..1; CODERANGE = 0..CIXMAX;
  BITS5 = 0..37B; BITS6 = 0..77B;  BITS7 = 0..177B;
(* 235 - set of char *)
(* 237 - conformant *)
{QPOWER is a quad-word set - it can't be loaded, so it is like an array
 in many ways}
  STRUCTFORM = (SCALAR,CONFORMANT,SUBRANGE,POINTER,POWER,QPOWER,ARRAYS,
		RECORDS,FILES,TAGFWITHID,TAGFWITHOUTID,VARIANT);
  DECLKIND = (STANDARD,DECLARED);
  STP = ^ STRUCTURE; CTP = ^ IDENTIFIER; BTP = ^BYTEPOINT;
  FTP = ^FILBLCK;
  GTP = ^GLOBPTR;

{A STRUCTURE is used whenever it is necessary to say what something is.
 I.e. each variable entry in the symbol table points to a STRUCTURE to
 say what kind of thing it is, and expressions and other structured
 objects use a STRUCTURE to say what kind of object is involved.
 SELFSTP is used for dumping out the symbol table into the object file
 for PASDDT.}

  STRUCTURE = packed record
		       SELFSTP: STP;
		       SIZE: XADDRRANGE;
		       NOCODE: BOOLEAN;
		       BITSIZE: BITRANGE;
		       HASFILE: BOOLEAN;
		       case FORM: STRUCTFORM of
			    SCALAR:   (case SCALKIND: DECLKIND of
					    DECLARED: (FCONST: CTP));
			    SUBRANGE: (RANGETYPE: STP;
				       MIN, MAX: VALU);
(* 240 - added CONFORMANT *)
			    CONFORMANT: (BOUNDTYPE: STP;
					 UPPER,LOWER: CTP);
			    POINTER:  (ELTYPE: STP);
(* 235 - added CHARSET *)
			    POWER,QPOWER: (ELSET: STP);
			    ARRAYS:   (ARRAYCONF: BOOLEAN; {conformant array}
				       ARRAYPF: BOOLEAN;
				       ARRAYBPADDR: ADDRRANGE;
				       AELTYPE, INXTYPE: STP);
			    RECORDS:  (RECORDPF: BOOLEAN;
				       FSTFLD: CTP;
				       RECVAR: STP);
			    FILES:    (FILTYPE: STP;
				       FILEPF: BOOLEAN);
			    TAGFWITHID,
			    TAGFWITHOUTID: (FSTVAR: STP;
					    case BOOLEAN of
						 TRUE : (TAGFIELDP: CTP);
						 FALSE  : (TAGFIELDTYPE: STP));
			    VARIANT:  (NXTVAR, SUBVAR: STP;
				       FIRSTFIELD: CTP;
				       VARVAL: VALU;
				       QXLYPRTWRR: BOOLEAN)
		     end;

  BPOINTER = packed record
		      SBITS,PBITS: BITRANGE;
		      XBIT: IBRANGE;
		      IBIT: IBRANGE;
		      IREG: ACRANGE;
		      RELADDR: ADDRRANGE;
{We use INTEGER instead of XADDRRANGE in order to force it right
 justified in a new word}
		      XXBIT: IBRANGE;
		      XIBIT: IBRANGE;
		      XIREG: ACRANGE;
		      XRELADDR: XADDRRANGE;
		    end;

  BPKIND = (RECORDD,ARRAYY);

  BYTEPOINT = packed record
		       BYTE: BPOINTER;
		       LAST: BTP;
		       case BKIND:BPKIND of
			    RECORDD: (FIELDCP: CTP);
			    ARRAYY : (ARRAYSP: STP)
		     end;
  GLOBPTR = record
	      NEXTGLOBPTR: GTP;
	      FIRSTGLOB,
	      LASTGLOB   : ADDRRANGE;
	      FCIX         : CODERANGE
	    end;

  FILBLCK = packed record
		     NEXTFTP : FTP;
		     FILEIDENT : CTP
		   end;

  %NAMES\
  %*****\

  (* PARAMS is a special kind of TYPES.  It is used only for
   * predeclared identifiers describing kludgey types that are
   * valid only in procedure parameter lists. *)
  IDCLASS = (TYPES,KONST,VARS,FIELD,PROC,FUNC,LABELT,PARAMS);
  SETOFIDS = set of IDCLASS;
  IDKIND = (ACTUAL,FORMAL);
  PACKKIND = (NOTPACK,PACKK,HWORDR,HWORDL);
  CHARWORD = packed array [1..5] of CHAR;
  %ALFA = PACKED ARRAY [1..ALFALENG] OF CHAR;\
  BIGNAME = packed array [11..MAXNAMELEN] of CHAR;
  BIGNAMEFORMS = (CHARFORM,WORDFORM);
  XBIGNAME = record case BIGNAMEFORMS of
	CHARFORM:(CHARS: BIGNAME);
	WORDFORM:(WORDS: array[1..38] of INTEGER)
	end;
  XBIGNAMEPTR = ^ XBIGNAME;

{An IDENTIFIER record is the permanent symbol table record for a
 simple identifier.  It doesn't specify what kind of object the
 identifier is. (IDTYPE points to a STRUCTURE, which does that.)
 However it does have the address, in VADDR. The symbol table is
 a binary tree.  LLINK and RLINK point to subtrees that are
 alphabetically less than or greater than this symbol.  NEXT is
 used in constructing linked lists of identifiers, such as args
 to procedures, or fields in a record.  SELFCTP is used for
 dumping out the symbol table into the object file for PASDDT.}

  IDENTIFIER = packed record
			NAME: ALFA;
			NAMELEN: 0..777777B; NAMEEXT: XBIGNAMEPTR;
			LLINK, RLINK: CTP;
			IDTYPE: STP;
			NEXT: CTP;
			SELFCTP: CTP;
			NOCODE: BOOLEAN;
			case KLASS: IDCLASS of
(* 231 add CREF_TEMP *)
			     TYPES: (CREF_TEMP: INTEGER);
			     KONST: (VALUES: VALU);
			     VARS:   (VKIND: IDKIND;
				      VLEV: LEVRANGE;
				      CHANNEL: ACRANGE;
				      VDUMMY: 0..31;
				      VADDR: XADDRRANGE;
{VNOASSIGN is a flag that this variable temporarily can't be assigned
to.  At the moment it is set temporarily while the variable is the
controlled variable of a FOR loop.  CONFARG indicates that this variable
is one of the bounds on a conformant array.  This permanently implies
that the variable can't be assigned to.}
				      VNOASSIGN: BOOLEAN;
				      case CONFARG: BOOLEAN of
					   TRUE: (SOURCE: INTEGER;
						  ISUPPER: BOOLEAN));
			     FIELD: (PACKF: PACKKIND;
				     FDUMMY: 0..7777B;
				     FLDADDR: XADDRRANGE);
			     %* IF PACKF=PACKK THEN FLDADDR CONTAINS THE
			      * ABSOLUTE ADDRESS OF THE CORRESPONDING BYTEPOINTER
			      * -----> ENTERBODY\
			     PROC,
			     FUNC:   (PFCHAIN: CTP;
				      case PFDECKIND: DECLKIND of
					   STANDARD: (KEY: 1..45);
					   DECLARED: (PFLEV: LEVRANGE;
						      PFADDR: ADDRRANGE;
						      POFFSET:XADDRRANGE;
(* 225 fix LC for forward proc's *)
						      PLC:XADDRRANGE;
						      case PFKIND: IDKIND of
							   ACTUAL: (FORWDECL: BOOLEAN;
								    TESTFWDPTR: CTP;
								    EXTERNDECL: BOOLEAN;
								    LANGUAGE: SYMBOL;
								    EXTERNALNAME: ALFA;
								    LINKCHAIN: packed array [LEVRANGE] of ADDRRANGE)));
			     LABELT: (SCOPE:LEVRANGE;
				      NONLOCGOTO:BOOLEAN;
				      GOTOCHAIN:ADDRRANGE;
				      LABELADDRESS:ADDRRANGE)
		      end;


  DISPRANGE = 0..DISPLIMIT;
  WHERE = (BLCK,CREC);
  MACHINE = (OKNAME,T10NAME,T20NAME);

  %RELOCATION\
  %**********\

  RELBYTE = 0..5B %(NO,RIGHT,LEFT,BOTH,LOWSEG,XSEG)\;
  RELWORD = packed array [0..17] of 0..3B;

  %EXPRESSIONS\
  %***********\

  ATTRKIND = (CST,VARBL,EXPR);

{An ATTR contains the current status of an object that is being used by
 the code generator.  I.e. its lexical level, where it currently is
 (can be the permanent location, or an AC), etc.  This differs from
 an identifier record both because it exists for things other than
 identifiers, e.g. constants and expressions, and because it is current
 state in code generation, rather than permanent symbol table value.
 The non-obvious fields are:
   DPLMT - displacement, i.e. address, possibly indexed by INDEXR,
	or modified by byte pointer fields, etc.
   EXTERNCTP - this is non-NIL only for if the expression represented
	is a reference to an external variable.  External variables
	are the only case where we have to have a handle on the
	identifier record in the symbol table.  We need this because
	the way externals are referred to involves changing the
	address for them in the symbol table every time they are
	refered to.  Currently the only external variables allowed
	are files.  You should be able to test EXTERNCTP <> NIL to
	see whether a file is external.
   NOASSIGN - This is a variable that shouldn't be assigned to
 NB: Code generation does not allow for A[I] where A is external.
 We sometimes adjust A by the fixed part of the indexing expression.
 If A is in the middle of an external chain, this will cause the
 obvious problem.}

  ATTR = record
	   TYPTR: STP;
	   case KIND: ATTRKIND of
		CST:   (CVAL: VALU);
		VARBL: (PACKFG: PACKKIND; NOASSIGN: BOOLEAN;
			INDEXR: ACRANGE; INDBIT: IBRANGE;
			VLEVEL: LEVRANGE; BPADDR: ADDRRANGE; DPLMT:INTEGER;
			VRELBYTE: RELBYTE; SUBKIND: STP; EXTERNCTP: CTP);
		EXPR:  (REG:ACRANGE)
	 end;

  TESTP = ^ TESTPOINTER;
  TESTPOINTER = packed record
			 ELT1,ELT2: STP;
			 LASTTESTP: TESTP
		       end;


  %TYPES FROM BODY \
  %****************\

  WRITEFORM = (WRITEENTRY,WRITENAME,WRITEHISEG,WRITEGLOBALS,WRITECODE,
	       WRITEINTERNALS,WRITEPOLISH,WRITELIBRARY,
	       WRITESYMBOLS,WRITEBLK,WRITESTART,WRITEEND);

  UPDATEFORM = (C,D);
(* 237 - get rid of ERRMPTR *)
{This thing is used to provide the text for error messages that need it.
 The NUMBER is the error number, and provides the match between the
 text here and the error message.}
  ERRORUPDATE = record
		  NUMBER: INTEGER;
		  case FORM: UPDATEFORM of
		       C:  (STRING: ALFA);
		       D: (INTVAL: INTEGER)
		end;

  KSP = ^ KONSTREC;
  KONSTREC = packed record
		      (* two fixup chains for 2 word consts *)
		      ADDR, ADDR1, KADDR: ADDRRANGE;
		      CONSTPTR: CSP;
		      NEXTKONST: KSP
		    end;
  POLPT = ^ POLREC;  (* Polish fixups for CASE *)
  {This record indicates a Polish fixup to be done at address WHERE in
  the code.  The RH of WHERE is to get the BASE (assumed relocatable),
  adjusted by OFFSET (a constant).  This is needed because the loader
  assumes that any address < 400000B is in the lowseg.  So to get the
  virtual start of the CASE statement branch table we need to use
  this to adjust the physical start of the table by the first case
  index}
  
  POLCODE = (ARRAYPOL,XPOL);

{ARRAYPOL replaces the right half with the sum of a half-word and a full-word
   quantity.  It is used for array addressing.
 XPOL replaces a whole word with the sum of XADDRSYM and a full-word
   value.  It is used in extended sections.}

  POLREC = packed record
		    POLTYPE: POLCODE;
		    WHERE: ADDRRANGE;
		    BASE:  ADDRRANGE;
		    OFFSET: INTEGER;
		    NEXTPOL: POLPT
		  end;


  PDP10INSTR = packed record
			INSTR   : INSTRANGE;
			AC          : ACRANGE;
			INDBIT  : IBRANGE;
			INXREG  : ACRANGE;
			ADDRESS : ADDRRANGE
		      end;

  HALFS = packed record
		   LEFTHALF: ADDRRANGE;
		   RIGHTHALF: ADDRRANGE
		 end;

  PAGEELEM = packed record
		      WORD1: PDP10INSTR;
		      LHALF: ADDRRANGE; RHALF: ADDRRANGE
		    end;
  DEBENTRY = record
	       NEXTDEB: INTEGER;  %WILL BE PTR TO NEXT ENTRY\
	       LASTPAGEELEM: PAGEELEM;
	       GLOBALIDTREE: CTP;
	       STANDARDIDTREE: CTP;
	       INTPOINT:  STP;
	       REALPOINT: STP;
	       CHARPOINT: STP;
	       MODNAME: ALFA;
	       SOURCE: packed array [1..167] of CHAR;
	     end;

  (* data structure for SCAN to return *)
  INTFILE = file of INTEGER;
  RPGDATA = record
	      RELNAME:ALFA;
	      STACKVAL:INTEGER;
	      HEAPVAL:INTEGER;
	      VERVAL:INTEGER;
	      XSW,ASW,ZSW,LSW,TSW,MSW,CSW,DSW,CRSW,RPGSW:BOOLEAN
	    end;
  RPGPT = ^ RPGDATA;
  PROGFILE = packed record
		      FILID:ALFA;
		      FILIDLEN: 0..777777B;
		      NEXT:^PROGFILE;
		      WILD,NEWGEN,OLDFILE,INTERACT,SEEEOL:BOOLEAN;
		      FILIDEXT: XBIGNAMEPTR;
		    end;
  PROGFILEPT = ^ PROGFILE;

  %------------------------------------------------------------------------------\
var
  %RETURNED BY SOURCE PROGRAM SCANNER INSYMBOL:\
  %********************************************\

  SY: SYMBOL;                   %LAST SYMBOL\
  OP: OPERATOR;                 %CLASSIFICATION OF LAST SYMBOL\
  VAL: VALU;                    %VALUE OF LAST CONSTANT\
  LGTH: INTEGER;                %LENGTH OF LAST STRING CONSTANT\
  ID: ALFA;                     %LAST IDENTIFIER (POSSIBLY TRUNCATED)\
  IDLEN: INTEGER;	        %length of last identifier\
  IDEXT: XBIGNAME;
(* 255 - better page handling *)
  EOLCH: CHAR;			%LAST EOLN\
  CH: CHAR;                     %LAST CHARACTER\


  %COUNTERS:\
  %*********\

  NEXT_CREF_TEMP,		{Temporary symbol for CREF}
  RTIME,
  I: INTEGER;
  SUPPORTIX: SUPPORTS;
  LANGUAGEIX: SYMBOL;
  CHCNT: 0..132;                %CHARACTER COUNTER\
  HIGHSTART,                    %START OF HIGH SEGMENT\
  XBLT_LOC,                             %Location of XBLT op-code for XTND\
  ADJSP_LOC,		 {Location of constant for ADJSP if > 377777}
  ADJSP_VAL,		 {Value of the constant}
  CODEEND,                              %FIRST LOCATION NOT USED FOR INSTRUCTIONS\
  LCMAIN,
  LC,BEGLC: XADDRRANGE;  {Low segment counter - can XADDR in procedures}
(* 227 - prevent putting out more than one *)
  NEED_XBLT_OP: Boolean; {Set once XBLT has had space allocated, to make MCCODE
				put it out}
  XC: XADDRRANGE;        {Extension of low segment up in extended address}
  BIGSIZE: XADDRRANGE;   {Anything bigger than this is put in extended space}
  IC,BEGIC: ADDRRANGE;   {High segment counter}
  EXTENDED_ADDRESSING: BOOLEAN;
  BITS_PER_ADDRESS: INTEGER;      %18 or 36 bits\
  COMMENT_PAGE, COMMENT_LINE: INTEGER;

  %SWITCHES:\
  %*********\

  EXTEND,
  ZERO,                         %ON TO INITIALIZE LOCAL VAR'S\
  RPGENTRY,                     %ON IF CALLED CALLED BY COMPIL\
  CREF,                         %ON IF CREF LISTING BEING MADE\
  DP,BEGDP,                     %DECLARATION PART\
  RESETFLAG,                    %TO IGNORE SWITCHES WHICH MUST NOT BE RESET\
  PRTERR,                       %TO ALLOW FORWARD REFERENCES IN POINTER TYPE
				 DECLARATION BY SUPPRESSING ERROR MESSAGE\
  MAIN,                         %IF FALSE COMPILER PRODUCES EXTERNAL PROCEDURE OR FUNCTION\
  DOINITTTY,                    %TTYOPEN needed\
  TTYINUSE,                     %no longer used ?\
  TTYSEEEOL,                    %TTY:# in program state\
  DEBUG,                        %ENABLE DEBUGGING\
  DEBUGSWITCH,                  %INSERT DEBUGINFORMATION\
  LISTCODE,                     %LIST MACRO CODE\
  INITGLOBALS,                  %INITIALIZE GLOBAL VARIABLES\
  LOADNOPTR,                    %TRUE IF NO POINTERVARIABLE SHALL BE LOADED\
  ARITHCHECK,                   %SWITCH FOR DETECTING ARITH ERRORS\
  RUNTMCHECK: BOOLEAN;          %SWITCH FOR RUNTIME-TESTS\
  STACK,HEAP: ADDRRANGE;    %FIRST ADDR OF STACK AND HEAP\
  VERSION: packed record     %version no. for output\
		    case BOOLEAN of
			 TRUE:(WORD:INTEGER);
			 FALSE:(WHO:0..7B;
				MAJOR:0..777B;
				MINOR:0..77B;
				EDIT:0..777777B)
		  end;
(* 247 - entry vector *)
  EVEC_EXTRA: INTEGER;	{extra entry points in entry vector}
  


  %POINTERS:\
  %*********\

  LOCALPFPTR, EXTERNPFPTR: CTP;   %PTRS TO LOCAL/EXTERNAL PROC/FUNC-CHAIN\
  INTPTR,REALPTR,CHARPTR,ANYFILEPTR,STRINGPTR,POINTERPTR,POINTERREF,
  BOOLPTR,NILPTR,TEXTPTR: STP;    %POINTERS TO ENTRIES OF STANDARD IDS\
  UARRTYP:STP;
  UTYPPTR,UCSTPTR,UVARPTR,
  UFLDPTR,UPRCPTR,UFCTPTR,      %POINTERS TO ENTRIES FOR UNDECLARED IDS\
  ULBLPTR,
  FWPTR,	                %HEAD OF CHAIN OF FORW DECL TYPE IDS\
  TYPE_LIST,			{List of all TYPES, for CREF_TYPES}
  GLOB_TYPE_LIST: CTP;		{End of TYPE_LIST at global level}
(* 237 - get rid of ERRMPTR *)
  ERRMARR: ARRAY[1..MAXERR] OF ERRORUPDATE;  {list of error messages}
  ERRMUSED,ERRMCUR: INTEGER;	{how many entries used}
  LASTBTP: BTP;                 %HEAD OF BYTEPOINTERTABLE\
  SFILEPTR,
  FILEPTR: FTP;
  FIRSTKONST: KSP;
  FIRSTPOL: POLPT;              %Polish fixups for CASE\
  ALFAPTR, DATEPTR: STP;
  FGLOBPTR,CGLOBPTR : GTP;      %POINTER TO FIRST AND CURRENT GLOBAL INITIALIZATION RECORD\
  GLOBTESTP : TESTP;            %POINTER TO LAST PAIR OF POINTERTYPES\
  (* Here is the main structure for the SCAN linkage *)
  SCANDATA : RPGPT;             %DATA FROM SCAN OF FILE NAMES\
  NPROGFILE,                    %NEW FILE NAME\
  LPROGFILE,                    %LAST FILE NAME IN LIST\
  FPROGFILE:PROGFILEPT;         %FIRST FILE NAME IN LIST\
  LASTLABEL:CTP;                %non-loc goto\
  {Pointers to ID's for...
  {INPUT, OUTPUT, TTY,      TTYOUT}
  INFILE, OUTFILE, TTYFILE, TTYOUTFILE:CTP;

  %BOOKKEEPING OF DECLARATION LEVELS:\
  %**********************************\

  LEVEL,BEGLEVEL: LEVRANGE;             %CURRENT STATIC LEVEL\
  DISX,                         %LEVEL OF LAST ID SEARCHED BY IDSEARCH\
  TOP: DISPRANGE;               %TOP OF DISPLAY\

  DISPLAY:  array [DISPRANGE] of        %WHERE:   MEANS:\

  packed record
	   %=BLCK:  ID IS VARIABLE ID\
	   BLKNAME: ALFA;              %NAME OF BLOCK\
	   FNAME: CTP;                 %=CREC:   ID IS FIELD ID IN RECORD WITH\
	   case OCCUR: WHERE of        %         CONSTANT ADDRESS\
		CREC: (CLEV: LEVRANGE;  %=VREC:   ID IS FIELD ID IN RECORD WITH\
		       CINDR: ACRANGE;  %            VARIABLE ADDRESS\
		       CINDB: IBRANGE;
		       CRELBYTE: RELBYTE;
		       CDSPL,
		       CLC     : XADDRRANGE)
	 end;


  %ERROR MESSAGES:\
  %***************\

  ERRORFLAG: BOOLEAN;           %TRUE IF SYNTACTIC ERRORS DETECTED\
  ERRINX: 0..MAXERR;            %NR OF ERRORS IN CURRENT SOURCE LINE\
  ERRLIST: array [1..MAXERR] of packed record
					 ARW: 1..4;
					 POS: 1..CHCNTMAX;
					 NMR: 1..600;
					 TIC: CHAR
				       end;

  ERRMESS15 : array [1..24] of packed array [1..15] of CHAR;
  ERRMESS20 : array [1..16] of packed array [1..20] of CHAR;
  ERRMESS25 : array [1..16] of packed array [1..25] of CHAR;
  ERRMESS30 : array [1..18] of packed array [1..30] of CHAR;
  ERRMESS35 : array [1..19] of packed array [1..35] of CHAR;
  ERRMESS40 : array [1..13] of packed array [1..40] of CHAR;
  ERRMESS45 : array [1..17] of packed array [1..45] of CHAR;
  ERRMESS50 : array [1.. 9] of packed array [1..50] of CHAR;
  ERRMESS55 : array [1.. 9] of packed array [1..55] of CHAR;
  ERRORINLINE,
  FOLLOWERROR : BOOLEAN;
  ERRLINE,
  BUFFER: array [1..CHCNTMAX] of CHAR;
  PAGECNT,SUBPAGE,CURLINE,
  LINECNT: INTEGER;
  LINENR: packed array [1..5] of CHAR;


  %EXPRESSION COMPILATION:\
  %***********************\

  GATTR: ATTR;                  %DESCRIBES THE EXPR CURRENTLY COMPILED\
(* 235 - remove SETMAP *)
  CHARMAP:array [0..177B] of INTEGER; %fast mapping to upper case\


  %DEBUG-SYSTEM:\
  %*************\

  LASTSTOP: ADDRRANGE;          %LAST BREAKPOINT\
  LASTLINE,                     %LINENUMBER FOR BREAKPOINTS\
  LINEDIFF,                     %DIFFERENCE BETWEEN ^ AND LINECNT\
  LASTPAGE:INTEGER;             %LAST PAGE THAT CONTAINS A STOP\
  PAGEHEADADR,                  %OVERGIVE TO DEBUG.PAS\
  LASTPAGER: ADDRRANGE;         %POINTS AT LAST PAGERECORD\
  PAGER: PAGEELEM;                      %ACTUAL PAGERECORD\
  DEBUGENTRY: DEBENTRY;
  IDRECSIZE: array [IDCLASS] of INTEGER;
  STRECSIZE: array [STRUCTFORM] of INTEGER;



  %STRUCTURED CONSTANTS:\
  %*********************\

  LETTERSORDIGITS,LETTERS,DIGITS,LETTERSDIGITSORLEFTARROW,HEXADIGITS: set of CHAR;
  CONSTBEGSYS,SIMPTYPEBEGSYS,TYPEBEGSYS,BLOCKBEGSYS,SELECTSYS,FACBEGSYS,
  LANGUAGESYS,STATBEGSYS,TYPEDELS: SETOFSYS;
  RW:  array [1..45%NR. OF RES. WORDS\] of ALFA;
  FRW: array [1..11%ALFALENG+1\] of 1..46%NR. OF RES. WORDS + 1\;
  RSY: array [1..45%NR. OF RES. WORDS\] of SYMBOL;
  SSY: array [' '..'_'] of SYMBOL;
  ROP: array [1..45%NR. OF RES. WORDS\] of OPERATOR;
  SOP: array [' '..'_'] of OPERATOR;
  NA:  array [1..82] of ALFA;
  NALEN: array[1..82] of INTEGER;
  MACHNA: array [1..82] of MACHINE;
  OTHERMACHINE: MACHINE;
  EXTNA: array [39..53] of ALFA;
  EXTLANGUAGE: array [39..53] of SYMBOL;
  MNEMONICS : array [1..45] of packed array [1..60] of CHAR;


  %VARIABLES FROM BODY\
  %*******************\

  {* Chantab is very strange.  It is used as a kludge because we need
  * two global request chains for each of INPUT, OUTPUT, TTY, and TTYOUTPUT.
  * So the second one is stored here.  From an identifier record, you can
  * look at CHANNEL to find which of these corresponds to that one.}
  CHANTAB:array [1..4] of ADDRRANGE;
  FILEINBLOCK:array [LEVRANGE] of BOOLEAN;   {True is there is a local file}
  LSTNEW,NEWBND: ADDRRANGE;       %references to these global variables\
  PFPOINT,PFDISP:ADDRRANGE;       %ADDRESS OF FIRST CODE IN PROCEDURE\
  RELBLOCK: packed record
		     case BOOLEAN of
			  TRUE: (COMPONENT: array [1..20] of INTEGER);
			  FALSE: (ITEM: ADDRRANGE; COUNT: ADDRRANGE;
				  RELOCATOR: RELWORD;
				  CODE: array [0..17] of INTEGER)
		   end;

  RNTS: record
	  NAME: array [SUPPORTS] of ALFA;
	  LINK: packed array [SUPPORTS] of ADDRRANGE
	end;

  CODE: packed record
		 RELOCATION:  packed array [CODERANGE] of RELBYTE;
		 INFORMATION: packed array [CODERANGE] of CHAR;
		 case INTEGER of
		      1: (INSTRUCTION: array [CODERANGE] of PDP10INSTR);
		      2: (WORD:        array [CODERANGE] of INTEGER);
		      3: (HALFWORD:    array [CODERANGE] of HALFS)
	       end;

  LABELS: array [1..LABMAX] of record
				 LABSVAL,LABSADDR: INTEGER
			       end;
  GOTOS: array [1..LABMAX] of record
				GOTOVAL,GOTOADDR: INTEGER
			      end;

  REGC,                           %TOP OF REGISTERSTACK\
  REGCMAX: ACRANGE;               %MAXIMUM OF REGISTERS FOR EXPRESSION STACK\
  LIX,JIX,CIX,
  INSERTSIZE,INSERTIC,            %TOO INSERT LCMAX IN ENTRYCODE\
  PFSTART: INTEGER;               %START OF NORMAL ENTRYCODE OF EACH FUNC. OR PROC.\
  IX: INTEGER;
  STKOFF, STKOFFMAX, CORALLOC: INTEGER;   %STACK SPACE NEEDED ABOVE LOCALS\
  LCMAX: XADDRRANGE; LCP: CTP;
  ERRFILE: TEXT;
  OUTPUTREL: file of INTEGER;     %RELOCATABLE BINARY OUTPUT\
  WITHIX,                         %TOP OF WITH-REG STACK\
  HIGHESTCODE,                    %MAXIMUM OF HIGH SEGMENTS ADDRESS\
  MAINSTART,                      %FIRST CODE OF BODY OF MAIN\
  GLOBTOPP,GLOBBASIS,
  EVEC_LOC,                       %Entry Vector location\
  STARTADDR: INTEGER;             %STARTADDRESSE\

(* 264 - CCLSW and friends are external *)
  CCLSW: ARRAY[0..MAXCCLSW] of ADDRRANGE;

  LOOKBLOCK: array [0..6] of INTEGER;
  LST,REL: packed array [1..3] of CHAR;
  FILENAME,XADDRSYM: ALFA;
  DAY: packed array [1..9] of CHAR;
  REQFILE,ENTRYDONE: BOOLEAN;
  THISFILE: STP;
  GOTARG: BOOLEAN;

  LIBIX: INTEGER;
  LIBORDER: packed array [1..4] of SYMBOL;
  LIBRARY: array [PASCALSY..COBOLSY] of record
					  INORDER, CALLED: BOOLEAN;
					  NAME: ALFA;
					  PROJNR: ADDRRANGE;
					  PROGNR: ADDRRANGE;
					  DEVICE: ALFA
					end;

  %------------------------------------------------------------------------------\

initprocedure;
  begin

    LST:= 'LST';    REL:= 'REL';    FILENAME:= '          ';  LOOKBLOCK[0] := 6;

    MNEMONICS[ 1] := '***001***002***003***004***005***006***007***010***011***012';
    MNEMONICS[ 2] := '***013***014***015***016***017***020***021***022***023***024';
    MNEMONICS[ 3] := '***025***026***027***030***031***032***033***034***035***036';
    MNEMONICS[ 4] := '***037CALL  INIT  ***042***043***044***045***046CALLI OPEN  ';
    MNEMONICS[ 5] := 'TTCALL***052***053***054RENAMEIN    OUT   SETSTSSTATO STATUS';
    MNEMONICS[ 6] := 'STATZ INBUF OUTBUFINPUT OUTPUTCLOSE RELEASMTAPE UGETF USETI ';
    MNEMONICS[ 7] := 'USETO LOOKUPENTER UJEN  ***101***102***103JSYS  ADJSP ***106';
    MNEMONICS[ 8] := '***107DFAD  DFSB  DFMP  DFDV  ***114***115***116***117DMOVE ';
    MNEMONICS[ 9] := 'DMOVN FIX   ***123DMOVEMDMOVNMFIXR  FLTR  UFA   DFN   FSC   ';
    MNEMONICS[10] := 'IBP   ILDB  LDB   IDPB  DPB   FAD   FADL  FADM  FADB  FADR  ';
    MNEMONICS[11] := 'FADRI FADRM FADRB FSB   FSBL  FSBM  FSBB  FSBR  FSBRI FSBRM ';
    MNEMONICS[12] := 'FSBRB FMP   FMPL  FMPM  FMPB  FMPR  FMPRI FMPRM FMPRB FDV   ';
    MNEMONICS[13] := 'FDVL  FDVM  FDVB  FDVR  FDVRI FDVRM FDVRB MOVE  MOVEI MOVEM ';
    MNEMONICS[14] := 'MOVES MOVS  MOVSI MOVSM MOVSS MOVN  MOVNI MOVNM MOVNS MOVM  ';
    MNEMONICS[15] := 'MOVMI MOVMM MOVMS IMUL  IMULI IMULM IMULB MUL   MULI  MULM  ';
    MNEMONICS[16] := 'MULB  IDIV  IDIVI IDIVM IDIVB DIV   DIVI  DIVM  DIVB  ASH   ';
    MNEMONICS[17] := 'ROT   LSH   JFFO  ASHC  ROTC  LSHC  ***247EXCH  BLT   AOBJP ';
    MNEMONICS[18] := 'AOBJN JRST  JFCL  XCT   MAP   PUSHJ PUSH  POP   POPJ  JSR   ';
    MNEMONICS[19] := 'JSP   JSA   JRA   ADD   ADDI  ADDM  ADDB  SUB   SUBI  SUBM  ';
    MNEMONICS[20] := 'SUBB  CAI   CAIL  CAIE  CAILE CAIA  CAIGE CAIN  CAIG  CAM   ';
    MNEMONICS[21] := 'CAML  CAME  CAMLE CAMA  CAMGE CAMN  CAMG  JUMP  JUMPL JUMPE ';
    MNEMONICS[22] := 'JUMPLEJUMPA JUMPGEJUMPN JUMPG SKIP  SKIPL SKIPE SKIPLESKIPA ';
    MNEMONICS[23] := 'SKIPGESKIPN SKIPG AOJ   AOJL  AOJE  AOJLE AOJA  AOJGE AOJN  ';
    MNEMONICS[24] := 'AOJG  AOS   AOSL  AOSE  AOSLE AOSA  AOSGE AOSN  AOSG  SOJ   ';
    MNEMONICS[25] := 'SOJL  SOJE  SOJLE SOJA  SOJGE SOJN  SOJG  SOS   SOSL  SOSE  ';
    MNEMONICS[26] := 'SOSLE SOSA  SOSGE SOSN  SOSG  SETZ  SETZI SETZM SETZB AND   ';
    MNEMONICS[27] := 'ANDI  ANDM  ANDB  ANDCA ANDCAIANDCAMANDCABSETM  XMOVEISETMM ';
    MNEMONICS[28] := 'SETMB ANDCM ANDCMIANDCMMANDCMBSETA  SETAI SETAM SETAB XOR   ';
    MNEMONICS[29] := 'XORI  XORM  XORB  IOR   IORI  IORM  IORB  ANDCB ANDCBIANDCBM';
    MNEMONICS[30] := 'ANDCBBEQV   EQVI  EQVM  EQVB  SETCA SETCAISETCAMSETCABORCA  ';
    MNEMONICS[31] := 'ORCAI ORCAM ORCAB SETCM SETCMISETCMMSETCMBORCM  ORCMI ORCMM ';
    MNEMONICS[32] := 'ORCMB ORCB  ORCBI ORCBM ORCBB SETO  SETOI SETOM SETOB HLL   ';
    MNEMONICS[33] := 'XHLLI HLLM  HLLS  HRL   HRLI  HRLM  HRLS  HLLZ  HLLZI HLLZM ';
    MNEMONICS[34] := 'HLLZS HRLZ  HRLZI HRLZM HRLZS HLLO  HLLOI HLLOM HLLOS HRLO  ';
    MNEMONICS[35] := 'HRLOI HRLOM HRLOS HLLE  HLLEI HLLEM HLLES HRLE  HRLEI HRLEM ';
    MNEMONICS[36] := 'HRLES HRR   HRRI  HRRM  HRRS  HLR   HLRI  HLRM  HLRS  HRRZ  ';
    MNEMONICS[37] := 'HRRZI HRRZM HRRZS HLRZ  HLRZI HLRZM HLRZS HRRO  HRROI HRROM ';
    MNEMONICS[38] := 'HRROS HLRO  HLROI HLROM HLROS HRRE  HRREI HRREM HRRES HLRE  ';
    MNEMONICS[39] := 'HLREI HLREM HLRES TRN   TLN   TRNE  TLNE  TRNA  TLNA  TRNN  ';
    MNEMONICS[40] := 'TLNN  TDN   TSN   TDNE  TSNE  TDNA  TSNA  TDNN  TSNN  TRZ   ';
    MNEMONICS[41] := 'TLZ   TRZE  TLZE  TRZA  TLZA  TRZN  TLZN  TDZ   TSZ   TDZE  ';
    MNEMONICS[42] := 'TSZE  TDZA  TSZA  TDZN  TSZN  TRC   TLC   TRCE  TLZE  TRCA  ';
    MNEMONICS[43] := 'TLCA  TRCN  TLCN  TDC   TSC   TDCE  TSCE  TDCA  TSCA  TDCN  ';
    MNEMONICS[44] := 'TSCN  TRO   TLO   TROE  TLOE  TROA  TLOA  TRON  TLON  TDO   ';
    MNEMONICS[45] := 'TSO   TDOE  TSOE  TDOA  TSOA  TDON  TSON  ***700            ';
  end;

initprocedure %SEARCH LIBRARIES\;
  begin
    LIBRARY[PASCALSY].INORDER   := FALSE;
    LIBRARY[FORTRANSY].INORDER  := FALSE;
    LIBRARY[ALGOLSY].INORDER    := FALSE;
    LIBRARY[COBOLSY].INORDER    := FALSE;
    LIBRARY[PASCALSY].CALLED    := FALSE;
    LIBRARY[FORTRANSY].CALLED   := FALSE;
    LIBRARY[ALGOLSY].CALLED     := FALSE;
    LIBRARY[COBOLSY].CALLED     := FALSE;
    LIBRARY[PASCALSY].NAME      := PASLIB;
    LIBRARY[FORTRANSY].NAME     := 'FORLIB    ';
    LIBRARY[ALGOLSY].NAME       := 'ALGLIB    ';
    LIBRARY[COBOLSY].NAME       := 'LIBOL     ';
    LIBRARY[PASCALSY].DEVICE    := PASDEV;
    LIBRARY[FORTRANSY].DEVICE   := 'SYS       ';
    LIBRARY[ALGOLSY].DEVICE     := 'SYS       ';
    LIBRARY[COBOLSY].DEVICE     := 'SYS       ';
    LIBRARY[PASCALSY].PROJNR    := PASPROJ;
    LIBRARY[FORTRANSY].PROJNR   := 0;
    LIBRARY[ALGOLSY].PROJNR     := 0;
    LIBRARY[COBOLSY].PROJNR     := 0;
    LIBRARY[PASCALSY].PROGNR    := PASPROG;
    LIBRARY[FORTRANSY].PROGNR   := 0;
    LIBRARY[ALGOLSY].PROGNR     := 0;
    LIBRARY[COBOLSY].PROGNR     := 0;
  end %SEARCH LIBRARIES\;

initprocedure %STANDARDNAMES\;
  begin
    NA[ 1] := 'FALSE     '; 
    NALEN[ 1] := 5;
    NA[ 2] := 'TRUE      '; 
    NALEN[ 2] := 4;
    NA[ 3] := 'INPUT     ';
    NALEN[ 3] := 5;
    NA[ 4] := 'OUTPUT    '; 
    NALEN[ 4] := 6;
    NA[ 5] := 'TTY       '; 
    NALEN[ 5] := 3;
    NA[ 6] := 'TTYOUTPUT ';
    NALEN[ 6] := 9;
    NA[ 7] := 'GET       '; 
    NALEN[ 7] := 3;
    NA[ 8] := 'GETLN     '; 
    NALEN[ 8] := 5;
    NA[ 9] := 'PUT       ';
    NALEN[ 9] := 3;
    NA[10] := 'PUTLN     '; 
    NALEN[10] := 5;
    NA[11] := 'RESET     '; 
    NALEN[11] := 5;
    NA[12] := 'REWRITE   ';
    NALEN[12] := 7;
    NA[13] := 'READ      '; 
    NALEN[13] := 4;
    NA[14] := 'READLN    '; 
    NALEN[14] := 6;
    NA[15] := 'BREAK     ';
    NALEN[15] := 5;
    NA[16] := 'WRITE     '; 
    NALEN[16] := 5;
    NA[17] := 'WRITELN   '; 
    NALEN[17] := 7;
    NA[18] := 'PACK      ';
    NALEN[18] := 4;
    NA[19] := 'UNPACK    '; 
    NALEN[19] := 6;
    NA[20] := 'NEW       '; 
    NALEN[20] := 3;
    NA[21] := 'MARK      ';
    NALEN[21] := 4;
    NA[22] := 'RELEASE   '; 
    NALEN[22] := 7;
    NA[23] := 'GETLINENR '; 
    NALEN[23] := 9;
    NA[24] := 'PUT8BITSTO';  {TTY is added in ENTERSTDNAMES}
    NALEN[24] := 13;
    NA[25] := 'PAGE      '; 
    NALEN[25] := 4;
    NA[26] := 'DATE      '; 
    NALEN[26] := 4;
    NA[27] := 'RUNTIME   ';
    NALEN[27] := 7;
    NA[28] := 'TIME      '; 
    NALEN[28] := 4;
    NA[29] := 'ABS       '; 
    NALEN[29] := 3;
    NA[30] := 'SQR       ';
    NALEN[30] := 3;
    NA[31] := 'TRUNC     '; 
    NALEN[31] := 5;
    NA[32] := 'ODD       '; 
    NALEN[32] := 3;
    NA[33] := 'ORD       ';
    NALEN[33] := 3;
    NA[34] := 'CHR       '; 
    NALEN[34] := 3;
    NA[35] := 'PRED      '; 
    NALEN[35] := 4;
    NA[36] := 'SUCC      ';
    NALEN[36] := 4;
    NA[37] := 'EOF       '; 
    NALEN[37] := 3;
    NA[38] := 'EOLN      '; 
    NALEN[38] := 4;
    NA[39] := 'SIN       ';
    NALEN[39] := 3;
    NA[40] := 'COS       '; 
    NALEN[40] := 3;
    NA[41] := 'EXP       '; 
    NALEN[41] := 3;
    NA[42] := 'SQRT      ';
    NALEN[42] := 4;
    NA[43] := 'LN        '; 
    NALEN[43] := 2;
    NA[44] := 'ARCTAN    '; 
    NALEN[44] := 6;
    NA[45] := 'LOG       ';
    NALEN[45] := 3;
    NA[46] := 'SIND      '; 
    NALEN[46] := 4;
    NA[47] := 'COSD      '; 
    NALEN[47] := 4;
    NA[48] := 'SINH      ';
    NALEN[48] := 4;
    NA[49] := 'COSH      '; 
    NALEN[49] := 4;
    NA[50] := 'TANH      '; 
    NALEN[50] := 4;
    NA[51] := 'ARCSIN    ';
    NALEN[51] := 6;
    NA[52] := 'ARCCOS    '; 
    NALEN[52] := 6;
    NA[53] := 'RANDOM    ';
    NALEN[53] := 6;
    NA[54] := 'STRSET    '; 
    NALEN[54] := 6;
    NA[55] := 'STRWRITE  ';
    NALEN[55] := 8;
    NA[56] := 'GETINDEX  '; 
    NALEN[56] := 8;
    NA[57] := 'CLOSE     ';
    NALEN[57] := 5;
    NA[58] := 'CALLI     '; 
    NALEN[58] := 5;
    NA[59] := 'RENAME    ';
    NALEN[59] := 6;
    NA[60] := 'DISMISS   '; 
    NALEN[60] := 7;
    NA[61] := 'UPDATE    ';
    NALEN[61] := 6;
    NA[62] := 'DUMPIN    '; 
    NALEN[62] := 6;
    NA[63] := 'DUMPOUT   ';
    NALEN[63] := 7;
    NA[64] := 'USETI     '; 
    NALEN[64] := 5;
    NA[65] := 'USETO     ';
    NALEN[65] := 5;
    NA[66] := 'BREAKIN   '; 
    NALEN[66] := 7;
    NA[67] := 'NEWZ      ';
    NALEN[67] := 4;
    NA[68] := 'APPEND    '; 
    NALEN[68] := 6;
    NA[69] := 'PUTX      ';
    NALEN[69] := 4;
    NA[70] := 'SETPOS    '; 
    NALEN[70] := 6;
    NA[71] := 'NEXTBLOCK ';
    NALEN[71] := 9;
    NA[72] := 'GETX      '; 
    NALEN[72] := 4;
    NA[73] := 'DELETE    ';
    NALEN[73] := 6;
    NA[74] := 'RCLOSE    '; 
    NALEN[74] := 6;
    NA[75] := 'JSYS      ';
    NALEN[75] := 4;
    NA[76] := 'DISPOSE   '; 
    NALEN[76] := 7;
    NA[77] := 'NEXTFILE  ';
    NALEN[77] := 8;
    NA[78] := 'CURPOS    '; 
    NALEN[78] := 6;
    NA[79] := 'SPACELEFT ';
    NALEN[79] := 9;
    NA[80] := 'ROUND     '; 
    NALEN[80] := 5;
    NA[81] := 'RECSIZE   ';
    NALEN[81] := 7;
    NA[82] := 'NEWL      ';
    NALEN[82] := 4;
    MACHNA[24] := T10NAME; MACHNA[58] := T10NAME;
    MACHNA[62] := T10NAME; MACHNA[63] := T10NAME;
    MACHNA[64] := T10NAME; MACHNA[65] := T10NAME;
    MACHNA[71] := T10NAME;
    MACHNA[74] := T20NAME; MACHNA[75] := T20NAME;
    MACHNA[77] := T20NAME; MACHNA[79] := T10NAME;
    MACHNA[82] := T20NAME
  end %STANDARDNAMES\;

initprocedure %EXTERNAL NAMES\;
  begin
    EXTNA[39] := 'SIN       '; EXTLANGUAGE[39] := FORTRANSY;
    EXTNA[40] := 'COS       '; EXTLANGUAGE[40] := FORTRANSY;
    EXTNA[41] := 'EXP       '; EXTLANGUAGE[41] := FORTRANSY;
    EXTNA[42] := 'SQRT      '; EXTLANGUAGE[42] := FORTRANSY;
    EXTNA[43] := 'ALOG      '; EXTLANGUAGE[43] := FORTRANSY;
    EXTNA[44] := 'ATAN      '; EXTLANGUAGE[44] := FORTRANSY;
    EXTNA[45] := 'ALOG10    '; EXTLANGUAGE[45] := FORTRANSY;
    EXTNA[46] := 'SIND      '; EXTLANGUAGE[46] := FORTRANSY;
    EXTNA[47] := 'COSD      '; EXTLANGUAGE[47] := FORTRANSY;
    EXTNA[48] := 'SINH      '; EXTLANGUAGE[48] := FORTRANSY;
    EXTNA[49] := 'COSH      '; EXTLANGUAGE[49] := FORTRANSY;
    EXTNA[50] := 'TANH      '; EXTLANGUAGE[50] := FORTRANSY;
    EXTNA[51] := 'ASIN      '; EXTLANGUAGE[51] := FORTRANSY;
    EXTNA[52] := 'ACOS      '; EXTLANGUAGE[52] := FORTRANSY;
    EXTNA[53] := 'RAN       '; EXTLANGUAGE[53] := FORTRANSY;

  end %EXTERNAL NAMES\;

initprocedure %RUNTIME-, DEBUG-SUPPORTS\;
  begin

    RNTS.NAME[STACKOVERFLOW]             := 'CORERR    ';
    RNTS.NAME[DEBSTACK]                  := 'DCORER    ';
    RNTS.NAME[DEBUGCALL]                 := 'PDDT.     ';
    RNTS.NAME[BADPOINT]                  := 'PTRER.    ';
    RNTS.NAME[ALLOCATE]                  := 'NEW       ';
    RNTS.NAME[CLEARALLOC]                := 'NEWCL.    ';
    RNTS.NAME[LOCALALLOC]                := 'NEW.L     ';
    RNTS.NAME[DEALLOCATE]                := 'DISPOS    ';
    RNTS.NAME[WITHFILEDEALLOCATE]        := 'DISPF.    ';
    RNTS.NAME[EXITGOTO]                  := 'GOTOC.    ';
    RNTS.NAME[EXITPROGRAM]               := 'END       ';
    RNTS.NAME[GETLINE]                   := 'GETLN     ';
    RNTS.NAME[GETFILE]                   := 'GET.      ';
    RNTS.NAME[PUTLINE]                   := 'PUTLN     ';
    RNTS.NAME[PUTFILE]                   := 'PUT       ';
    RNTS.NAME[PUTXFILE]                  := 'PUTX      ';
    RNTS.NAME[RESETFILE]                 := 'RESETF    ';
    RNTS.NAME[REWRITEFILE]               := 'REWRIT    ';
    RNTS.NAME[RESETSTRING]               := 'STSET.    ';
    RNTS.NAME[REWRITESTRING]             := 'STWR.     ';
    RNTS.NAME[WRITEOCTAL]                := 'WRTOCT    ';
    RNTS.NAME[WRITEHEXADECIMAL]          := 'WRTHEX    ';
    RNTS.NAME[WRITEINTEGER]              := 'WRTINT    ';
    RNTS.NAME[WRITECHARACTER]            := 'WRITEC    ';
    RNTS.NAME[WRITEREAL]                 := 'WRTREA    ';
    RNTS.NAME[WRITEBOOLEAN]              := 'WRTBOL    ';
    RNTS.NAME[WRITESTRING]               := 'WRTUST    ';
    RNTS.NAME[WRITEPACKEDSTRING]         := 'WRTPST    ';
    RNTS.NAME[WRITERECORD]               := '.WRREC    ';
    RNTS.NAME[WRITESCALAR]               := '.WRSCA    ';
    RNTS.NAME[READINTEGER]               := '.READI    ';
    RNTS.NAME[READCHARACTER]             := '.READC    ';
    RNTS.NAME[READREAL]                  := '.READR    ';
    RNTS.NAME[READRECORD]                := '.READD    ';
    RNTS.NAME[CONVERTINTEGERTOREAL]      := 'INTREA    ';
    RNTS.NAME[CONVERTREALTOINTEGER]      := 'TRUNC     ';
    RNTS.NAME[BREAKOUTPUT]               := 'BREAK     ';
    RNTS.NAME[OPENTTY]                   := 'TTYPR.    ';
    RNTS.NAME[INITIALIZEDEBUG]           := 'INDEB.    ';
    RNTS.NAME[ENTERDEBUG]                := 'EXDEB.    ';
    RNTS.NAME[GETCHARACTER]              := 'GETCH     ';
    RNTS.NAME[PUTPAGE]                   := 'PUTPG     ';
    RNTS.NAME[INDEXERROR]                := 'INXERR    ';
    RNTS.NAME[ERRORINASSIGNMENT]         := 'SRERR     ';
    RNTS.NAME[FILEUNINITIALIZED]         := 'ILFIL.    ';
    RNTS.NAME[INITFILEBLOCK]             := 'INITB.    ';
    RNTS.NAME[CLOSEFILE]                 := 'CLOFIL    ';
    RNTS.NAME[READSTRING]                := 'REDUS.    ';
    RNTS.NAME[READPACKEDSTRING]          := 'REDPS.    ';
    RNTS.NAME[READFILENAME]              := 'GETFN.    ';
    RNTS.NAME[NAMEFILE]                  := 'RENAME    ';
    RNTS.NAME[DISFILE]                   := 'RESDEV    ';
    RNTS.NAME[UPFILE]                    := 'UPDATE    ';
    RNTS.NAME[APFILE]                    := 'APPEND    ';
    RNTS.NAME[READDUMP]                  := 'DUMPIN    ';
    RNTS.NAME[WRITEDUMP]                 := 'DUMPOU    ';
    RNTS.NAME[SETIN]                     := 'USETIN    ';
    RNTS.NAME[SETOUT]                    := 'USETOU    ';
    RNTS.NAME[BREAKINPUT]                := 'BREAKI    ';
    RNTS.NAME[SETPOSF]                   := 'SETPOS    ';
    RNTS.NAME[CURPOSF]                   := 'CURPOS    ';
    RNTS.NAME[NEXTBLOCKF]                := 'NEXTBL    ';
    RNTS.NAME[SPACELEFTF]                := 'SPCLF.    ';
    RNTS.NAME[GETXF]                     := 'GETX.     ';
    RNTS.NAME[DELFILE]                   := 'DELF.     ';
    RNTS.NAME[RELFILE]                   := 'RELF.     ';
    RNTS.NAME[INITMEM]                   := 'PASIM.    ';
    RNTS.NAME[INITFILES]                 := 'PASIF.    ';
    RNTS.NAME[GETDAYTIME]                := 'DAYTM.    ';

  end %RUNTIME-, DEBUG-SUPPORTS\;

initprocedure %INITSCALARS\;
  begin
    EXTENDED_ADDRESSING := FALSE;  BITS_PER_ADDRESS := 18;
(* 227 - init XBLT_LOC, prevent more than one *)
    XBLT_LOC := 0;  NEED_XBLT_OP := FALSE;
    CHANTAB[1] := 0; CHANTAB[2] := 0; CHANTAB[3] := 0; CHANTAB[4] := 0;
    FWPTR := nil; LASTBTP := nil;  FGLOBPTR := nil; FILEPTR := nil;
(* 237 - get rid of ERRMPTR *)
    LOCALPFPTR:=nil; EXTERNPFPTR:= nil; GLOBTESTP := nil; ERRMUSED := 0;
    HEAP := 0; STACK := 0;
    LISTCODE := FALSE; LOADNOPTR := TRUE; INITGLOBALS := FALSE;
    RUNTMCHECK := TRUE; ARITHCHECK := TRUE;
    TTYINUSE := TRUE; FOLLOWERROR := FALSE; ERRORINLINE := FALSE;
    RESETFLAG := TRUE; TTYSEEEOL := FALSE;
    DP := TRUE; PRTERR := TRUE; ERRORFLAG := FALSE; MAIN := TRUE;
    ENTRYDONE := FALSE; DEBUG := FALSE; DEBUGSWITCH := FALSE;
    COMMENT_PAGE := 0; FPROGFILE := nil; LPROGFILE := nil;
    LASTLABEL := nil;
    LC := PROGRST;       %START OF LOWSEGMENT AVAILABLE TO PROGRAM\
    CHCNT := 0; LINECNT := 1; PAGECNT := 1; SUBPAGE := 0;
    LASTLINE := -1; LASTPAGE := 0;
    LIBIX := 0; ERRINX := 0; LSTNEW := 0; NEWBND := 0;
(* 231 *)
    NEXT_CREF_TEMP := 1; TYPE_LIST := nil;
  end %INITSCALARS\;

initprocedure %INITSETS\;
  begin
    DIGITS := ['0'..'9'];
    LETTERS := ['A'..'Z'];
    HEXADIGITS := ['0'..'9','A'..'F'];
    LETTERSORDIGITS := [ '0'..'9','A'..'Z'];
    LETTERSDIGITSORLEFTARROW := ['0'..'9','A'..'Z','_'];
    LANGUAGESYS := [FORTRANSY,ALGOLSY,COBOLSY,PASCALSY];
    CONSTBEGSYS := [ADDOP,INTCONST,REALCONST,STRINGCONST,IDENT];
    SIMPTYPEBEGSYS := [ADDOP,INTCONST,REALCONST,STRINGCONST,IDENT,LPARENT];
    TYPEBEGSYS := [ADDOP,INTCONST,REALCONST,STRINGCONST,IDENT,LPARENT,
		   ARROW,PACKEDSY,ARRAYSY,RECORDSY,SETSY,FILESY];
    TYPEDELS := [ARRAYSY,RECORDSY,SETSY,FILESY];
    BLOCKBEGSYS := [INCLUDESY,LABELSY,CONSTSY,TYPESY,VARSY,INITPROCSY,
		    PROCEDURESY,FUNCTIONSY,BEGINSY];
    SELECTSYS := [ARROW,PERIOD,LBRACK];
    FACBEGSYS := [INTCONST,REALCONST,STRINGCONST,IDENT,LPARENT,LBRACK,NOTSY];
    STATBEGSYS := [BEGINSY,GOTOSY,IFSY,WHILESY,REPEATSY,LOOPSY,
		   FORSY,WITHSY,CASESY]
  end %INITSETS\;

initprocedure %RESWORDS\;
  begin
    RW[ 1] := 'IF        '; RW[ 2] := 'DO        '; RW[ 3] := 'OF        ';
    RW[ 4] := 'TO        '; RW[ 5] := 'IN        '; RW[ 6] := 'OR        ';
    RW[ 7] := 'END       '; RW[ 8] := 'FOR       '; RW[ 9] := 'VAR       ';
    RW[10] := 'DIV       '; RW[11] := 'MOD       '; RW[12] := 'SET       ';
    RW[13] := 'AND       '; RW[14] := 'NOT       '; RW[15] := 'THEN      ';
    RW[16] := 'ELSE      '; RW[17] := 'WITH      '; RW[18] := 'GOTO      ';
    RW[19] := 'LOOP      '; RW[20] := 'CASE      '; RW[21] := 'TYPE      ';
    RW[22] := 'FILE      '; RW[23] := 'EXIT      '; RW[24] := 'BEGIN     ';
    RW[25] := 'UNTIL     '; RW[26] := 'WHILE     '; RW[27] := 'ARRAY     ';
    RW[28] := 'CONST     '; RW[29] := 'LABEL     '; RW[30] := 'ALGOL     ';
    RW[31] := 'COBOL     '; RW[32] := 'EXTERN    '; RW[33] := 'PASCAL    ';
    RW[34] := 'RECORD    '; RW[35] := 'DOWNTO    '; RW[36] := 'PACKED    ';
    RW[37] := 'OTHERS    '; RW[38] := 'REPEAT    '; RW[39] := 'FORTRAN   ';
    RW[40] := 'FORWARD   '; RW[41] := 'PROGRAM   '; RW[42] := 'INCLUDE   ';
    RW[43] := 'FUNCTION  '; RW[44] := 'PROCEDURE ';
    RW[45] := 'INITPROCED';  

{WARNING: INITPROCEDURE is too long.  So we can't really use this 
 mechanism.  Instead there is a special-case test in INSYMBOL, since this
 is the only symbol longer than 10 char'S}

    FRW[1] :=  1; FRW[2] :=  1; FRW[3] :=  7; FRW[4] := 15; FRW[5] := 24;
    FRW[6] := 32; FRW[7] := 39; FRW[8] := 43; FRW[9] := 44; FRW[10] := 45;
    FRW[11] := 45
  end %RESWORDS\;

initprocedure %SYMBOLS\;
  begin
    RSY[1] := IFSY; RSY[2] := DOSY; RSY[3] := OFSY; RSY[4] := TOSY;
    RSY[5] := RELOP; RSY[6] := ADDOP; RSY[7] := ENDSY; RSY[8] := FORSY;
    RSY[9] := VARSY; RSY[10] := MULOP; RSY[11] := MULOP; RSY[12] := SETSY;
    RSY[13] := MULOP; RSY[14] := NOTSY; RSY[15] := THENSY;
    RSY[16] := ELSESY; RSY[17] := WITHSY; RSY[18] := GOTOSY;
    RSY[19] := LOOPSY; RSY[20] := CASESY; RSY[21] := TYPESY;
    RSY[22] := FILESY; RSY[23] := EXITSY; RSY[24] := BEGINSY;
    RSY[25] := UNTILSY; RSY[26] := WHILESY; RSY[27] := ARRAYSY;
    RSY[28] := CONSTSY; RSY[29] := LABELSY;
    RSY[30] := ALGOLSY; RSY[31] := COBOLSY;
    RSY[32] := EXTERNSY; RSY[33] := PASCALSY; RSY[34] := FORTRANSY;
    RSY[34] := RECORDSY; RSY[35]:= DOWNTOSY; RSY[36] := PACKEDSY;
    RSY[37] := OTHERSSY; RSY[38]:= REPEATSY; RSY[39] := FORTRANSY;
    RSY[40] := FORWARDSY; RSY[41] := PROGRAMSY; RSY[42] := INCLUDESY;
    RSY[43] := FUNCTIONSY; RSY[44] := PROCEDURESY; RSY[45] := INITPROCSY;

    SSY['A'] := OTHERSY; SSY['B'] := OTHERSY; SSY['C'] := OTHERSY;
    SSY['D'] := OTHERSY; SSY['E'] := OTHERSY; SSY['F'] := OTHERSY;
    SSY['G'] := OTHERSY; SSY['H'] := OTHERSY; SSY['I'] := OTHERSY;
    SSY['J'] := OTHERSY; SSY['K'] := OTHERSY; SSY['L'] := OTHERSY;
    SSY['M'] := OTHERSY; SSY['N'] := OTHERSY; SSY['O'] := OTHERSY;
    SSY['P'] := OTHERSY; SSY['Q'] := OTHERSY; SSY['R'] := OTHERSY;
    SSY['S'] := OTHERSY; SSY['T'] := OTHERSY; SSY['U'] := OTHERSY;
    SSY['V'] := OTHERSY; SSY['W'] := OTHERSY; SSY['X'] := OTHERSY;
    SSY['Y'] := OTHERSY; SSY['Z'] := OTHERSY; SSY['0'] := OTHERSY;
    SSY['1'] := OTHERSY; SSY['2'] := OTHERSY; SSY['3'] := OTHERSY;
    SSY['4'] := OTHERSY; SSY['5'] := OTHERSY; SSY['6'] := OTHERSY;
    SSY['7'] := OTHERSY; SSY['8'] := OTHERSY; SSY['9'] := OTHERSY;
    SSY['_'] := OTHERSY;
    SSY['+'] := ADDOP;   SSY['-'] := ADDOP;   SSY['*'] := MULOP;
    SSY['/'] := MULOP;   SSY['('] := LPARENT; SSY[')'] := RPARENT;
    SSY['$'] := OTHERSY; SSY['='] := RELOP;   SSY[' '] := OTHERSY;
    SSY[','] := COMMA;   SSY['.'] := PERIOD;  SSY[''''] := OTHERSY;
    SSY['['] := LBRACK;  SSY[']'] := RBRACK;  SSY[':'] := COLON;
    SSY['#'] := RELOP;   SSY['%'] := OTHERSY; SSY['!'] := ADDOP;
    SSY['&'] := MULOP;   SSY['^'] := ARROW;   SSY['\'] := OTHERSY;
    SSY['<'] := RELOP;   SSY['>'] := RELOP;   SSY['@'] := RELOP;
    SSY['"'] := RELOP;   SSY['?'] := NOTSY;   SSY[';'] := SEMICOLON;
  end %SYMBOLS\;

initprocedure %OPERATORS\;
  begin
    ROP[ 1] := NOOP; ROP[ 2] := NOOP; ROP[ 3] := NOOP; ROP[ 4] := NOOP;
    ROP[ 5] := INOP; ROP[ 6] := OROP; ROP[ 7] := NOOP; ROP[ 8] := NOOP;
    ROP[ 9] := NOOP; ROP[10] := IDIV; ROP[11] := IMOD; ROP[12] := NOOP;
    ROP[13] :=ANDOP; ROP[14] := NOOP; ROP[15] := NOOP; ROP[16] := NOOP;
    ROP[17] := NOOP; ROP[18] := NOOP; ROP[19] := NOOP; ROP[20] := NOOP;
    ROP[21] := NOOP; ROP[22] := NOOP; ROP[23] := NOOP; ROP[24] := NOOP;
    ROP[25] := NOOP; ROP[26] := NOOP; ROP[27] := NOOP; ROP[28] := NOOP;
    ROP[29] := NOOP; ROP[30] := NOOP; ROP[31] := NOOP; ROP[32] := NOOP;
    ROP[33] := NOOP; ROP[34] := NOOP; ROP[35] := NOOP; ROP[36] := NOOP;
    ROP[37] := NOOP; ROP[38] := NOOP; ROP[39] := NOOP; ROP[40] := NOOP;
    ROP[41] := NOOP; ROP[42] := NOOP; ROP[43] := NOOP; ROP[44] := NOOP;
    ROP[45] := NOOP;

    SOP['+'] := PLUS; SOP['-'] := MINUS; SOP['*'] := MUL;  SOP['/'] := RDIV;
    SOP['='] := EQOP; SOP['#'] := NEOP;  SOP['!'] := OROP; SOP['&'] := ANDOP;
    SOP['<'] := LTOP; SOP['>'] := GTOP;  SOP['@'] := LEOP; SOP['"'] := GEOP;
    SOP[' '] := NOOP; SOP['$'] := NOOP; SOP['%'] := NOOP; SOP['('] := NOOP;
    SOP[')'] := NOOP; SOP[','] := NOOP; SOP['.'] := NOOP; SOP['0'] := NOOP;
    SOP['1'] := NOOP; SOP['2'] := NOOP; SOP['3'] := NOOP; SOP['4'] := NOOP;
    SOP['5'] := NOOP; SOP['6'] := NOOP; SOP['7'] := NOOP; SOP['8'] := NOOP;
    SOP['9'] := NOOP; SOP[':'] := NOOP; SOP[';'] := NOOP; SOP['?'] := NOOP;
    SOP['A'] := NOOP; SOP['B'] := NOOP; SOP['C'] := NOOP; SOP['D'] := NOOP;
    SOP['E'] := NOOP; SOP['F'] := NOOP; SOP['G'] := NOOP; SOP['H'] := NOOP;
    SOP['I'] := NOOP; SOP['J'] := NOOP; SOP['K'] := NOOP; SOP['L'] := NOOP;
    SOP['M'] := NOOP; SOP['N'] := NOOP; SOP['O'] := NOOP; SOP['P'] := NOOP;
    SOP['Q'] := NOOP; SOP['R'] := NOOP; SOP['S'] := NOOP; SOP['T'] := NOOP;
    SOP['U'] := NOOP; SOP['V'] := NOOP; SOP['W'] := NOOP; SOP['X'] := NOOP;
    SOP['Y'] := NOOP; SOP['Z'] := NOOP; SOP['['] := NOOP; SOP['\'] := NOOP;
    SOP[']'] := NOOP; SOP['^'] := NOOP; SOP['_'] := NOOP; SOP[''''] := NOOP;
  end %OPERATORS\;

initprocedure %RECORDSIZES\;
  begin
{By removing some comments in WRITEMC, you can produce a version of the
 compiler that prints these numbers out for you.}
    (* define size of types for copyctp *)
    IDRECSIZE[TYPES]  := 6;
    IDRECSIZE[KONST]  := 7;
    IDRECSIZE[VARS]   := 6;
    IDRECSIZE[FIELD]  := 6;
    IDRECSIZE[PROC]   := 6;  {The debugger doesn't use the fields in PROC&FUNC}
    IDRECSIZE[FUNC]   := 6;
    IDRECSIZE[PARAMS] := 6;
    IDRECSIZE[LABELT] := 7;
    STRECSIZE[SCALAR] := 2;
    STRECSIZE[SUBRANGE]:=4;
    STRECSIZE[CONFORMANT] := 3;
    STRECSIZE[POINTER]:= 2;
    STRECSIZE[POWER]  := 2;
    STRECSIZE[QPOWER] := 2;
    STRECSIZE[ARRAYS] := 3;
    STRECSIZE[RECORDS]:= 3;
    STRECSIZE[FILES]  := 2;
    STRECSIZE[TAGFWITHID]:=3;
    STRECSIZE[TAGFWITHOUTID] := 3;
    STRECSIZE[VARIANT] := 4
  end;

initprocedure %ERRORMESSAGES\;
  begin
    ERRMESS15[ 1] := '":" expected   ';
    ERRMESS15[ 2] := '")" expected   ';
    ERRMESS15[ 3] := '"(" expected   ';
    ERRMESS15[ 4] := '"[" expected   ';
    ERRMESS15[ 5] := '"]" expected   ';
    ERRMESS15[ 6] := '";" expected   ';
    ERRMESS15[ 7] := '"=" expected   ';
    ERRMESS15[ 8] := '"," expected   ';
    ERRMESS15[ 9] := '":=" expected  ';
    ERRMESS15[10] := '"OF" expected  ';
    ERRMESS15[11] := '"DO" expected  ';
    ERRMESS15[12] := '"IF" expected  ';
    ERRMESS15[13] := '"END" expected ';
    ERRMESS15[14] := '"THEN" expected';
    ERRMESS15[15] := '"EXIT" expected';
    ERRMESS15[16] := 'Illegal symbol ';
    ERRMESS15[17] := 'No sign allowed';
    ERRMESS15[18] := 'Number expected';
    ERRMESS15[19] := 'Not implemented';
    ERRMESS15[20] := 'Error in type  ';
    ERRMESS15[21] := 'Compiler error ';
    ERRMESS15[22] := '"." expected   ';
    ERRMESS15[23] := 'Error in factor';
    ERRMESS15[24] := 'Too many digits';

    ERRMESS20[ 1] := '"BEGIN" expected    ';
    ERRMESS20[ 2] := '"UNTIL" expected    ';
    ERRMESS20[ 3] := 'Error in options    ';
    ERRMESS20[ 4] := 'Constant too large  ';
    ERRMESS20[ 5] := 'Digit must follow   ';
    ERRMESS20[ 6] := 'Exponent too large  ';
    ERRMESS20[ 7] := 'Constant expected   ';
    ERRMESS20[ 8] := 'Simple type expected';
    ERRMESS20[ 9] := 'Identifier expected ';
    ERRMESS20[10] := 'Realtype not allowed';
    ERRMESS20[11] := 'Multidefined label  ';
    ERRMESS20[12] := 'Filename expected   ';
    ERRMESS20[13] := 'Set type expected   ';
    ERRMESS20[14] := 'Undeclared exitlabel';
    ERRMESS20[15] := 'Undeclared label    ';
    ERRMESS20[16] := 'Illegal character   ';

    ERRMESS25[ 1] := '"TO"/"DOWNTO" expected   ';
    ERRMESS25[ 2] := '8 OR 9 in octal number   ';
    ERRMESS25[ 3] := 'Identifier not declared  ';
    ERRMESS25[ 4] := 'File not allowed here    ';
    ERRMESS25[ 5] := 'Integer constant expected';
    ERRMESS25[ 6] := 'Error in parameterlist   ';
    ERRMESS25[ 7] := 'Already forward declared ';
    ERRMESS25[ 8] := 'This format for real only';
    ERRMESS25[ 9] := 'Varianttype must be array';
    ERRMESS25[10] := 'Type conflict of operands';
    ERRMESS25[11] := 'Multidefined case label  ';
    ERRMESS25[12] := 'Octal for integer only   ';
    ERRMESS25[13] := 'Array index out of bounds';
    ERRMESS25[14] := 'Must be array or record  ';
    ERRMESS25[15] := 'Must be at least 5 words ';
    ERRMESS25[16] := 'Data won''t fit in memory ';

    ERRMESS30[ 1] := 'String constant is too long   ';
    ERRMESS30[ 2] := 'Identifier already declared   ';
    ERRMESS30[ 3] := 'Subrange bounds must be scalar';
    ERRMESS30[ 4] := 'Incompatible subrange types   ';
    ERRMESS30[ 5] := 'Incompatible with tagfieldtype';
    ERRMESS30[ 6] := 'Index type may not be integer ';
    ERRMESS30[ 7] := 'Type of variable is not array ';
    ERRMESS30[ 8] := 'Type of variable is not record';
    ERRMESS30[ 9] := 'No such field in this record  ';
    ERRMESS30[10] := 'Expression too complicated    ';
    ERRMESS30[11] := 'Illegal type of operand(s)    ';
    ERRMESS30[12] := 'Tests on equality allowed only';
    ERRMESS30[13] := 'Strict inclusion not allowed  ';
    ERRMESS30[14] := 'Structure comparison illegal  ';
    ERRMESS30[15] := 'Illegal type of expression    ';
    ERRMESS30[16] := 'Value of case label too large ';
    ERRMESS30[17] := 'Too many nested withstatements';
    ERRMESS30[18] := 'Conformant array not allowed  ';

    ERRMESS35[ 1] := 'String constant contains "<CR><LF>"';
    ERRMESS35[ 2] := 'Basetype requires more than 72 bits';
    ERRMESS35[ 3] := 'Basetype must be scalar or subrange';
    ERRMESS35[ 4] := 'More than 12 files declared by user';
    ERRMESS35[ 5] := 'File as value parameter not allowed';
    ERRMESS35[ 6] := 'Procedure too long (too much code) ';
    ERRMESS35[ 7] := 'No packed structure allowed here   ';
    ERRMESS35[ 8] := 'Variant must belong to tagfieldtype';
    ERRMESS35[ 9] := 'Type of operand(s) must be boolean ';
    ERRMESS35[10] := 'Set element types not compatible   ';
    ERRMESS35[11] := 'Assignment to files not allowed    ';
    ERRMESS35[12] := 'Too many labels in this procedure  ';
    ERRMESS35[13] := 'Too many cases in case statement   ';
    ERRMESS35[14] := 'Control variable may not be formal ';
    ERRMESS35[15] := 'Illegal type of for-controlvariable';
    ERRMESS35[16] := 'Type of filecomponent must be char ';
    ERRMESS35[17] := 'Constant not in bounds of subrange ';
    ERRMESS35[18] := 'Illegal when assigning to function ';
    ERRMESS35[19] := 'Data structure larger than 128K    ';

    ERRMESS40[ 1] := 'Identifier is not of appropriate class  ';
    ERRMESS40[ 2] := 'Tagfield type must be scalar or subrange';
    ERRMESS40[ 3] := 'Index type must be scalar or subrange   ';
    ERRMESS40[ 4] := 'Too many nested scopes of identifiers   ';
    ERRMESS40[ 5] := 'Pointer forward reference unsatisfied   ';
    ERRMESS40[ 6] := 'Previous declaration was not forward    ';
    ERRMESS40[ 7] := 'Type of variable must be file or pointer';
    ERRMESS40[ 8] := 'Missing corresponding variantdeclaration';
    ERRMESS40[ 9] := 'Too many variants in call of NEW (max 6)';
    ERRMESS40[10] := 'More than four errors in this sourceline';
    ERRMESS40[11] := 'No initialization on records or files   ';
    ERRMESS40[12] := 'Assignment to func. must be in its body ';
    ERRMESS40[13] := 'Too many parameters (must fit in AC''s)  ';

    ERRMESS45[ 1] := 'Low bound may not be greater than high bound ';
    ERRMESS45[ 2] := 'Identifier or "CASE" expected in fieldlist   ';
    ERRMESS45[ 3] := 'Too many nested procedures and/or functions  ';
    ERRMESS45[ 4] := 'File declaration in procedures not allowed   ';
    ERRMESS45[ 5] := 'Missing result type in function declaration  ';
    ERRMESS45[ 6] := 'Assignment to formal function is not allowed ';
    ERRMESS45[ 7] := 'Index type is not compatible with declaration';
    ERRMESS45[ 8] := 'Error in type of standard procedure parameter';
    ERRMESS45[ 9] := 'Error in type of standard function parameter ';
    ERRMESS45[10] := 'Real and string tagfields not implemented    ';
    ERRMESS45[11] := 'Set element type must be scalar or subrange  ';
    ERRMESS45[12] := 'In initprocedure only assignments possible   ';
    ERRMESS45[13] := 'No constant or expression for VAR argument   ';
    ERRMESS45[14] := 'EXTERN declaration not allowed in procedures ';
    ERRMESS45[15] := 'Body of forward declared procedure missing   ';
    ERRMESS45[16] := 'Must be user-declared PASCAL proc. or func.  ';
    ERRMESS45[17] := 'Can''t initialize data in extended data area  ';

    ERRMESS50[ 1] := 'Too many forward references of procedure entries  ';
    ERRMESS50[ 2] := 'Assignment to standard function is not allowed    ';
    ERRMESS50[ 3] := 'Parameter type does not agree with declaration    ';
    ERRMESS50[ 4] := 'Initialization only by assignment of constants    ';
    ERRMESS50[ 5] := 'Label type incompatible with selecting expression ';
    ERRMESS50[ 6] := 'Statement must end with ";","END","ELSE"or"UNTIL" ';
    ERRMESS50[ 7] := 'Not allowed in initprocedures (packed structure?) ';
    ERRMESS50[ 8] := 'File mentioned in PROGRAM statement not declared  ';
    ERRMESS50[ 9] := 'Variable mentioned in PROGRAM statement not a file';

    ERRMESS55[ 1] := 'Function result type must be scalar,subrange or pointer';
    ERRMESS55[ 2] := 'Forward decl. func:repetition of resulttype not allowed';
    ERRMESS55[ 3] := 'Forward decl.: repetition of parameter list not allowed';
    ERRMESS55[ 4] := 'Number of parameters does not agree with declaration   ';
    ERRMESS55[ 5] := 'Resulttype of parameter func. does not agree with decl.';
    ERRMESS55[ 6] := 'Selected expression must have type of control variable ';
    ERRMESS55[ 7] := 'INITPROCEDURE can''t be within a procedure or function  ';
    ERRMESS55[ 8] := 'Array bound out of range specified for index variables ';
    ERRMESS55[ 9] := 'Illegal to set index of conformant array or "FOR" loop ';
  end %ERROR MESSAGES\;
initprocedure  %character mapping tables\;
  begin
    CHARMAP[0B] := 0B;      CHARMAP[1B] := 1B;      CHARMAP[2B] := 2B;      CHARMAP[3B] := 3B;
    CHARMAP[4B] := 4B;      CHARMAP[5B] := 5B;      CHARMAP[6B] := 6B;      CHARMAP[7B] := 7B;
    CHARMAP[10B] := 10B;    CHARMAP[11B] := 11B;    CHARMAP[12B] := 12B;    CHARMAP[13B] := 13B;
    CHARMAP[14B] := 14B;    CHARMAP[15B] := 15B;    CHARMAP[16B] := 16B;    CHARMAP[17B] := 17B;
    CHARMAP[20B] := 20B;    CHARMAP[21B] := 21B;    CHARMAP[22B] := 22B;    CHARMAP[23B] := 23B;
    CHARMAP[24B] := 24B;    CHARMAP[25B] := 25B;    CHARMAP[26B] := 26B;    CHARMAP[27B] := 27B;
    CHARMAP[30B] := 30B;    CHARMAP[31B] := 31B;    CHARMAP[32B] := 32B;    CHARMAP[33B] := 33B;
    CHARMAP[34B] := 34B;    CHARMAP[35B] := 35B;    CHARMAP[36B] := 36B;    CHARMAP[37B] := 37B;
    CHARMAP[40B] := 40B;    CHARMAP[41B] := 41B;    CHARMAP[42B] := 42B;    CHARMAP[43B] := 43B;
    CHARMAP[44B] := 44B;    CHARMAP[45B] := 45B;    CHARMAP[46B] := 46B;    CHARMAP[47B] := 47B;
    CHARMAP[50B] := 50B;    CHARMAP[51B] := 51B;    CHARMAP[52B] := 52B;    CHARMAP[53B] := 53B;
    CHARMAP[54B] := 54B;    CHARMAP[55B] := 55B;    CHARMAP[56B] := 56B;    CHARMAP[57B] := 57B;
    CHARMAP[60B] := 60B;    CHARMAP[61B] := 61B;    CHARMAP[62B] := 62B;    CHARMAP[63B] := 63B;
    CHARMAP[64B] := 64B;    CHARMAP[65B] := 65B;    CHARMAP[66B] := 66B;    CHARMAP[67B] := 67B;
    CHARMAP[70B] := 70B;    CHARMAP[71B] := 71B;    CHARMAP[72B] := 72B;    CHARMAP[73B] := 73B;
    CHARMAP[74B] := 74B;    CHARMAP[75B] := 75B;    CHARMAP[76B] := 76B;    CHARMAP[77B] := 77B;
    CHARMAP[100B] := 100B;  CHARMAP[101B] := 101B;  CHARMAP[102B] := 102B;  CHARMAP[103B] := 103B;
    CHARMAP[104B] := 104B;  CHARMAP[105B] := 105B;  CHARMAP[106B] := 106B;  CHARMAP[107B] := 107B;
    CHARMAP[110B] := 110B;  CHARMAP[111B] := 111B;  CHARMAP[112B] := 112B;  CHARMAP[113B] := 113B;
    CHARMAP[114B] := 114B;  CHARMAP[115B] := 115B;  CHARMAP[116B] := 116B;  CHARMAP[117B] := 117B;
    CHARMAP[120B] := 120B;  CHARMAP[121B] := 121B;  CHARMAP[122B] := 122B;  CHARMAP[123B] := 123B;
    CHARMAP[124B] := 124B;  CHARMAP[125B] := 125B;  CHARMAP[126B] := 126B;  CHARMAP[127B] := 127B;
    CHARMAP[130B] := 130B;  CHARMAP[131B] := 131B;  CHARMAP[132B] := 132B;  CHARMAP[133B] := 133B;
    CHARMAP[134B] := 134B;  CHARMAP[135B] := 135B;  CHARMAP[136B] := 136B;  CHARMAP[137B] := 137B;
    CHARMAP[140B] := 140B;  CHARMAP[141B] := 101B;  CHARMAP[142B] := 102B;  CHARMAP[143B] := 103B;
    CHARMAP[144B] := 104B;  CHARMAP[145B] := 105B;  CHARMAP[146B] := 106B;  CHARMAP[147B] := 107B;
    CHARMAP[150B] := 110B;  CHARMAP[151B] := 111B;  CHARMAP[152B] := 112B;  CHARMAP[153B] := 113B;
    CHARMAP[154B] := 114B;  CHARMAP[155B] := 115B;  CHARMAP[156B] := 116B;  CHARMAP[157B] := 117B;
    CHARMAP[160B] := 120B;  CHARMAP[161B] := 121B;  CHARMAP[162B] := 122B;  CHARMAP[163B] := 123B;
    CHARMAP[164B] := 124B;  CHARMAP[165B] := 125B;  CHARMAP[166B] := 126B;  CHARMAP[167B] := 127B;
    CHARMAP[170B] := 130B;  CHARMAP[171B] := 131B;  CHARMAP[172B] := 132B;  CHARMAP[173B] := 173B;
    CHARMAP[174B] := 174B;  CHARMAP[175B] := 175B;  CHARMAP[176B] := 176B;  CHARMAP[177B] := 177B;

  end;
  %character mapping tables\

  %-------------------------------------------------------------------------------\
procedure REINIT;
  begin
    EXTENDED_ADDRESSING := FALSE;  BITS_PER_ADDRESS := 18;
(* 227 - init XBLT_LOC, prevent more than one *)
    XBLT_LOC := 0;  NEED_XBLT_OP := FALSE;
    CHANTAB[1] := 0; CHANTAB[2] := 0; CHANTAB[3] := 0; CHANTAB[4] := 0;
    FWPTR := nil; LASTBTP := nil;  FGLOBPTR := nil; FILEPTR := nil;
(* 237 - get rid of ERRMPTR *)
    LOCALPFPTR:=nil; EXTERNPFPTR:= nil; GLOBTESTP := nil; ERRMUSED := 0;
    HEAP := 0; STACK := 0; CREF := FALSE;  REQFILE := FALSE;
    LISTCODE := FALSE; LOADNOPTR := TRUE; INITGLOBALS := FALSE;
    RUNTMCHECK := TRUE;  ARITHCHECK := TRUE;
    TTYINUSE := TRUE; FOLLOWERROR := FALSE; ERRORINLINE := FALSE;
    RESETFLAG := TRUE;  TTYSEEEOL := FALSE;
    DP := TRUE; PRTERR := TRUE; ERRORFLAG := FALSE; MAIN := TRUE;
    ENTRYDONE := FALSE; DEBUG := FALSE; DEBUGSWITCH := FALSE;
    COMMENT_PAGE := 0;  FPROGFILE := nil; LPROGFILE := nil;
    HIGHSTART := 400000B;
    IC := HIGHSTART;     %START OF HIGHSEGMENT\
    LC := PROGRST;       %START OF LOWSEGMENT AVAILABLE TO PROGRAM\
    XC := 1000000B;	{Place to put extension of highsegment}
    BIGSIZE := 2000B;   {Anything in low seg bigger than this is put in XC}
    CHCNT := 0; LINECNT := 1; PAGECNT := 1; SUBPAGE := 0; CURLINE := 1;
    LASTLINE := -1; LASTPAGE := 0;
    LIBIX := 0; ERRINX := 0; LSTNEW := 0; NEWBND := 0;
(* 231 *)
    NEXT_CREF_TEMP := 1; TYPE_LIST := nil;
(* 237 *)
    EVEC_EXTRA := 0;
    with PAGER.WORD1 do
      begin
	INSTR:=0; AC:=0; INDBIT:=0; INXREG:=0; ADDRESS:=0
      end;
    PAGER.LHALF := 0; PAGER.RHALF := 0;
    DEBUGENTRY.LASTPAGEELEM := PAGER;
    LASTSTOP := 0; LASTPAGER := 0;
    DEBUGENTRY.STANDARDIDTREE := nil;
    DEBUGENTRY.GLOBALIDTREE := nil;
    FILENAME := '          ';
    LIBRARY[PASCALSY].INORDER   := FALSE;
    LIBRARY[FORTRANSY].INORDER  := FALSE;
    LIBRARY[ALGOLSY].INORDER    := FALSE;
    LIBRARY[COBOLSY].INORDER    := FALSE;
    LIBRARY[PASCALSY].CALLED    := FALSE;
    LIBRARY[FORTRANSY].CALLED   := FALSE;
    LIBRARY[ALGOLSY].CALLED     := FALSE;
    LIBRARY[COBOLSY].CALLED     := FALSE;
  end;


procedure PAGEHEAD;
  begin
    PAGE;
    WRITE(HEADER,'  ',DAY,'     ',SCANDATA^.RELNAME);
    if REQFILE then
      WRITE('  ****Included File****');
    WRITE('     PAGE ',PAGECNT:0);
    if SUBPAGE > 0 then
      WRITE('-',SUBPAGE:0);
    WRITELN;  WRITELN;  CURLINE := 1
  end;

procedure NEWLINE;
  begin
    WRITELN;
    CURLINE := CURLINE+1;
    if CURLINE > 53 then
      begin
	SUBPAGE := SUBPAGE + 1; PAGEHEAD
      end
  end;

procedure NEWPAGER;
  begin
    with PAGER, WORD1 do
      begin
	AC := PAGECNT div 16;
	INXREG := PAGECNT mod 16; ADDRESS := LASTPAGER;
	LHALF := LASTLINE; RHALF := LASTSTOP;
	LASTLINE := -1
      end
  end;


procedure BEGOFLINE;
  begin
    if CREF then
      WRITE(CHR(177B),'A');
    if CHCNT > CHCNTMAX then
      CHCNT := CHCNTMAX;
    if LISTCODE then
      begin
	if BEGDP then
	  begin
	    WRITE(BEGLC:6:O);
	    if (BEGLC < PROGRST) or (BEGLEVEL > 1) then
	      WRITE(' ')
	    else
	      WRITE('''')
	  end
	else
	  WRITE(BEGIC:6:O,'''');
	WRITE(' ':2)
      end;
    if LINENR='-----' then
      WRITE(LINECNT:5)
    else
      WRITE(LINENR);
    WRITE(' ':3)
  end;

procedure WRITEBUFFER;
  begin
    if LISTCODE then
      begin
	if CREF then
	  WRITE(CHR(177B),'B');
	BEGOFLINE; WRITE(BUFFER:CHCNT);
	for CHCNT := 1 to 17 do BUFFER[CHCNT] := ' ';
	CHCNT := 17; NEWLINE;
      end
  end;

(* 255 - handle end of page better *)
procedure GETNEXTLINE(EOLCH:CHAR);
  begin
    loop
      GETLINENR(LINENR);
    exit if (EOLCH # CHR(14B)) and (LINENR <> '     ');    %TEST END OF PAGE\
      if DEBUG and (LASTLINE > -1) then
	NEWPAGER;
      PAGECNT := PAGECNT + 1; SUBPAGE := 0;
      PAGEHEAD; LINECNT := 1;
      if LINENR = '     ' then
        begin
        READLN;  {Don't treat an SOS page mark as a real line}
	end
      else EOLCH := ' '
    end;
    if CREF then
      WRITE(CHR(177B),'B');
    BEGIC:=IC; BEGLC:=LC; BEGDP:=DP; BEGLEVEL:=LEVEL;
  end;

procedure BEGSTUFF; (* needed for file switch *)
  begin
    if CREF then
      WRITE(CHR(177B),'B');
    BEGIC:=IC;BEGLC:=LC;BEGDP:=DP;BEGLEVEL:=LEVEL;
    CHCNT:=0
  end;
procedure PASXIT(var A,B,C:file);
  extern;
procedure PUSHF(var F:file;S:STRGARR;L:INTEGER);
  extern;
procedure POPF(var F:file);
  extern;
procedure ANALYS(var F:file);
  extern;
procedure CLRIBF;
  extern;
function OVERFLOW:BOOLEAN;
  extern;
procedure CURNAME(var F:file;var S:STRING);
  extern;
function CTPCOM(c1,c2:CTP): INTEGER;
  extern;
function IDCOMP(c:CTP; idlen: integer; id: alfa; idext: xbigname): INTEGER;
  extern;
function IDCOMPP(c:CTP; idlen: integer; id: alfa; idext: xbignameptr): INTEGER;
  extern;

procedure ENDSTUFF;
  var
    I,K: INTEGER;
  begin
    BEGOFLINE;
    WRITE(BUFFER:CHCNT); NEWLINE;
    if ERRORINLINE then
      begin  %OUTPUT ERROR MESSAGES\
	if LISTCODE then
	  K := 11
	else
	  K := 2;
	WRITE(' ':K,'***** '); LISTCODE := FALSE;
(* 231 - put errors in file *)
	if LINENR = '-----' then
	  begin
	    WRITE(TTY,LINECNT:5);
	    WRITE(ERRFILE,LINECNT:5);
	  end
	else
	  begin
	    WRITE(TTY,LINENR);
	    WRITE(ERRFILE,LINENR)
	  end;
	WRITELN(TTY,' ':3,BUFFER:CHCNT); WRITE(TTY,'P*',PAGECNT:3,'** ');
	WRITELN(ERRFILE,' ':3,BUFFER:CHCNT); 
	WRITE(ERRFILE,'P*',PAGECNT:3,'** ');
	for K:=1 to CHCNT do
	  if BUFFER[K] = CHR(11B) then
	    ERRLINE[K] := CHR(11B);
	WRITE(ERRLINE :  CHCNT); WRITELN(TTY,ERRLINE : CHCNT); NEWLINE;
        WRITELN(ERRFILE,ERRLINE : CHCNT);
	for K := 1 to ERRINX do
	  with ERRLIST[K] do
	    begin
	      WRITE(' ':15,ARW:1,'.',TIC,':  '); 
	      WRITE(TTY,ARW:1,'.',TIC,':  ');
	      WRITE(ERRFILE,ARW:1,'.',TIC,':  ');
	      ERRMCUR := 1;
	      while ERRMCUR <= ERRMUSED do
		    with ERRMARR[ERRMCUR] do
		      begin
			if NMR = NUMBER then
			  begin
			    case FORM of
			      C:
				begin
				  WRITE(STRING:10,' --> ');
			 	  WRITE(TTY,STRING:10,' --> ');
			 	  WRITE(ERRFILE,STRING:10,' --> ')
				end;
			      D:
				begin
				  WRITE(INTVAL:5,' --> ');
				  WRITE(TTY,INTVAL:5,' --> ');
				  WRITE(ERRFILE,INTVAL:5,' --> ');
				end
			    end;
{Found it - set NUMBER to 0, to prevent this from being used again,
 in case of two errors of the same kind on one line.  Set ERRMCUR
 high to exit loop.}
			    NUMBER := 0; ERRMCUR := ERRMUSED + 1;
			  end;
		        ERRMCUR := ERRMCUR + 1;
		      end;
	      I := NMR mod 50;
	      case NMR div 50 of
		3:
		  begin
		    WRITE(ERRMESS15[I]); WRITE(TTY,ERRMESS15[I]);
		    WRITE(ERRFILE,ERRMESS15[I])
		  end;
		4:
		  begin
		    WRITE(ERRMESS20[I]); WRITE(TTY,ERRMESS20[I]);
		    WRITE(ERRFILE,ERRMESS20[I])
		  end;
		5:
		  begin
		    WRITE(ERRMESS25[I]); WRITE(TTY,ERRMESS25[I]);
		    WRITE(ERRFILE,ERRMESS25[I])
		  end;
		6:
		  begin
		    WRITE(ERRMESS30[I]); WRITE(TTY,ERRMESS30[I]);
		    WRITE(ERRFILE,ERRMESS30[I])
		  end;
		7:
		  begin
		    WRITE(ERRMESS35[I]); WRITE(TTY,ERRMESS35[I]);
		    WRITE(ERRFILE,ERRMESS35[I])
		  end;
		8:
		  begin
		    WRITE(ERRMESS40[I]); WRITE(TTY,ERRMESS40[I]);
		    WRITE(ERRFILE,ERRMESS40[I])
		  end;
		9:
		  begin
		    WRITE(ERRMESS45[I]); WRITE(TTY,ERRMESS45[I]);
		    WRITE(ERRFILE,ERRMESS45[I])
		  end;
		10:
		  begin
		    WRITE(ERRMESS50[I]); WRITE(TTY,ERRMESS50[I]);
		    WRITE(ERRFILE,ERRMESS50[I]);
		  end;
		11:
		  begin
		    WRITE(ERRMESS55[I]); WRITE(TTY,ERRMESS55[I]);
		    WRITE(ERRFILE,ERRMESS55[I])
		  end
	      end;
	      NEWLINE; WRITELN(TTY); WRITELN(ERRFILE);
	    end;
	ERRINX := 0; ERRORINLINE := FALSE;
	for I := 1 to CHCNT do ERRLINE [I] := ' ';
(* 237 - get rid of ERRMPTR *)
	ERRMUSED := 0;
      end
  end;

procedure ENDOFLINE(OKEOF:BOOLEAN);
  begin
    ENDSTUFF;
    if EOF(INPUT) and not OKEOF then  (* detect unexpected eof *)
      begin
	WRITE('Unexpected end of file'); NEWLINE;
	WRITELN(TTY,'?  Unexpected end of file');
	WRITELN(ERRFILE,'?  Unexpected end of file');
	CLOSE(ERRFILE);
	if COMMENT_PAGE <> 0 then    (* we're in a comment *)
	  begin
	    WRITE('UNTERMINATED COMMENT AT ',COMMENT_PAGE:0,
		  '/',COMMENT_LINE:0); NEWLINE;
	    WRITELN(TTY,'?  UNTERMINATED COMMENT AT ',COMMENT_PAGE:0,
		    '/',COMMENT_LINE:0)
	  end;
	(* abort creation of rel file on error *)
	REWRITE(OUTPUTREL); CLRIBF;
	(* popf to be sure we get main file closed in reqfile *)
	if REQFILE then
	  begin
	    CLOSE(INPUT); POPF(INPUT)
	  end;
	PASXIT(INPUT,OUTPUT,OUTPUTREL)
      end;
(* 255 - better page mark handling *)
(* 256 - handle bare CR *)
{here we do in effect a READLN, but we have to find the actual EOLN
 character.  Unlike the real READLN, we treat a bare CR (or series 
 of CR's) as a normal end of line sequence.}
    while ORD(INPUT^) = 15B do
      GET(INPUT);
    EOLCH := INPUT^;
    if EOLN(INPUT) then  {skip EOLN char. If we had bare CR, nothing}
      GET(INPUT);
    LINECNT := LINECNT + 1;
    if not EOF(INPUT) then
      GETNEXTLINE(EOLCH);
    CHCNT := 0
  end  %ENDOFLINE\;
procedure ERROR(FERRNR: INTEGER);
  var
    LPOS,LARW : INTEGER;
  begin
    if not FOLLOWERROR then
      begin
	ERRORFLAG := TRUE;
	if ERRINX >= MAXERR then
	  begin
	    ERRLIST[MAXERR].NMR := 410; ERRINX := MAXERR
	  end
	else
	  begin
	    ERRINX := ERRINX + 1;
	    with ERRLIST[ERRINX] do
	      begin
		NMR := FERRNR; TIC := '^'
	      end
	  end;
	FOLLOWERROR := TRUE; ERRORINLINE := TRUE;
	if (FERRNR = 215) or (FERRNR = 356)
	  or (FERRNR = 405) or (FERRNR = 464)
	then
	  begin
	   ERRLIST[ERRINX].TIC := ' ';
	   FOLLOWERROR := FALSE;
	  end
	else
	  if EOLN(INPUT) then
	    ERRLINE [CHCNT] := '^'
	  else
	    ERRLINE [CHCNT-1] := '^';
	if ERRINX > 1 then
	  with ERRLIST [ ERRINX-1] do
	    begin
	      LPOS := POS; LARW := ARW
	    end;
	with ERRLIST [ERRINX] do
	  begin
	    POS := CHCNT;
	    if ERRINX = 1 then
	      ARW := 1
	    else
	      if LPOS = CHCNT then
		ARW := LARW
	      else
		ARW := LARW + 1
	  end
      end
  end %ERROR\;

procedure ERRORWITHTEXT (FERRNR: INTEGER; FTEXT: ALFA);
  begin
    ERROR(FERRNR); 
(* 237 - get rid of ERRMPTR *)
    if ERRMUSED < MAXERR then
      begin
        ERRMUSED := ERRMUSED + 1;
	with ERRMARR[ERRMUSED] do
          begin
	    FORM := C;
	    NUMBER := FERRNR; STRING := FTEXT;
          end;
      end
  end %ERROR WITH TEXT\;
procedure INSYMBOL;
  %READ NEXT BASIC SYMBOL OF SOURCE PROGRAM AND RETURN ITS
   DESCRIPTION IN THE GLOBAL VARIABLES SY, OP, ID, VAL AND LGTH\
  (* prevent recursive comment scanning *)
  label
    2;
  const
    HEXMAX = 9;  DIGMAX = 12;  MAXEXP = 35;
    MAX8 =  37777777777B;  TEST8 =  40000000000B;  MIN8 = 400000000000B;
    MAX10 = 3435973836; {maximum number, sans last digit}
    MAX16 = 17777777777B;
  type
    NUMCONV = record case BOOLEAN of
			  TRUE:(OCT:packed array [1..DIGMAX] of 0..7);
			  FALSE:(INT:INTEGER)
	      end;
    HEXCONV = record case BOOLEAN of
			  TRUE:(HEX:packed array [1..HEXMAX] of 0..15);
			  FALSE:(INT:INTEGER)
	      end;
  var
    I,K,ASCALE,SCALE,EXP,IVAL: INTEGER;
    RVAL,R,FAC: REAL; STRINGTOOLONG,SIGN: BOOLEAN;
    DIGIT: array [1..DIGMAX] of 0..9;
    STRING: array [1..STRGLGTH] of CHAR;
    LVP: CSP;
    NC:NUMCONV;
    HC:HEXCONV;

  procedure NEXTCH;
    begin
      if EOLN(INPUT) then
	CH := ' '
      else
	begin
	  %READ(CH);\
	  CH := INPUT^; GET(INPUT); %THIS CHANGE SAVES 3 INSTRUCTIONS AT RUN-TIME\
	  CHCNT := CHCNT + 1;
	  if CHCNT <= CHCNTMAX then
	    BUFFER[CHCNT] := CH
	end;
      (* map lower case to upper.  Need separate NEXTCH for
       * strings now, since we don't do mapping there. *)
      CH := CHR(CHARMAP[ORD(CH)])
    end;

  procedure NEXTSTRCH;
    begin
      if EOLN(INPUT) then
	CH := ' '
      else
	begin
	  CH := INPUT^; GET(INPUT);
	  CHCNT := CHCNT + 1;
	  if CHCNT <= CHCNTMAX then
	    BUFFER[CHCNT] := CH
	end
    end;

  procedure OPTIONS;
    var
      LCH : CHAR; LSWITCH : BOOLEAN;
    begin
      repeat
	NEXTCH; LCH := CH;
	if not (CH in ['\','*']) then
	  NEXTCH;
	if not (CH in ['+','-']) then
	  if (LCH in ['P','H','S','V']) and (CH = ':') then
	    begin
	      NEXTCH;  INSYMBOL;
	      if SY # INTCONST then
		ERROR(203)
	      else
		begin
		  if LCH in ['H','S']  (* S AND H FOR STACK AND HEAP *) then
		    begin
		      if (VAL.IVAL mod 1000B) = 0 then
			VAL.IVAL := VAL.IVAL -1;
		      if VAL.IVAL > MAX_SECTION then
			VAL.IVAL := (VAL.IVAL div 1000B)*1000B + 777B
		    end;
		  if LCH in ['H','S','P'] then
		    if (VAL.IVAL < 0) or (VAL.IVAL > MAXADDR) then
		      ERROR(203);
		  if LCH = 'S' then
		    STACK := VAL.IVAL
		  else
		    if LCH = 'H' then
		      HEAP := VAL.IVAL
		    else
		      if LCH = 'P' then  (* variable start of hi seg *)
			begin
			  if RESETFLAG then
			    begin
			      HIGHSTART := VAL.IVAL;
			      IC := HIGHSTART
			    end
			end
		      else
(* 247 - extra entry vector locations *)
			begin
			  VERSION.WORD := VAL.IVAL;
			  if CH = ':' then
			    begin
			      NEXTCH;  INSYMBOL;
			      if SY # INTCONST then
				ERROR(203);
			      EVEC_EXTRA := VAL.IVAL
			    end
			end
		end
	    end
	  else
	    ERROR(203)
	else
	  begin
	    LSWITCH := CH = '+';
	    case LCH of
	      'L':  LISTCODE := LSWITCH;
	      'T':  if RESETFLAG then
		      TTYINUSE := LSWITCH
		    else WRITELN(TTY,'%Too late to set option T');
	      'M':  if RESETFLAG then
		      MAIN := LSWITCH
		    else WRITELN(TTY,'%Too late to set option M');
	      'C':  begin
		RUNTMCHECK := LSWITCH; ARITHCHECK := LSWITCH
	      end;
	      'A':  ARITHCHECK := LSWITCH;
	      'X':    if RESETFLAG then
			EXTENDED_ADDRESSING := LSWITCH
		      else WRITELN(TTY,'%Too late to set option X');
	      'Z':  ZERO := LSWITCH;
	      'D':  begin
		DEBUGSWITCH := LSWITCH;
		if RESETFLAG then
		  DEBUG := LSWITCH
		else
		  if LSWITCH then
		    DEBUG := TRUE
	      end
	      end
	  end;
	if EOLN(INPUT) then
	  ENDOFLINE(FALSE);
	if not ((CH in ['\','*']) or (LCH = 'H')) then
	  NEXTCH
      until CH # ','
    end   %OPTIONS\;

  procedure NEWCH;
    begin
      if EOLN(INPUT) then
	ENDOFLINE(FALSE);
      NEXTCH
    end;

  procedure SCANCOMMENT(STOPCH:CHAR);
    begin
      COMMENT_PAGE := PAGECNT; { pagecnt had better not be 0 }
      COMMENT_LINE := LINECNT; NEWCH;
      if CH='$' then
	OPTIONS;
      if (STOPCH = '\') or (STOPCH = '}') then
	while CH # STOPCH do NEWCH
      else
	repeat
	  while CH # '*' do NEWCH;
	  NEXTCH
	until CH=STOPCH;
      COMMENT_PAGE := 0;
      NEWCH
    end;

  begin
  2:
    %INSYMBOL\
    while (CH = ' ') or (ORD(CH) = 11B) do
      begin
	if EOLN(INPUT) then
	  ENDOFLINE(FALSE);
	NEXTCH
      end;

    case CH of
      'A','B','C','D','E','F','G','H','I',
      'J','K','L','M','N','O','P','Q','R',
      'S','T','U','V','W','X','Y','Z':
	begin
	  K := 0; ID := '          ';
	  repeat
	    if K < ALFALENG then
	      begin
		K := K + 1; ID[K] := CH
	      end
	    else if K < MAXNAMELEN then
	      begin
		K := K + 1; IDEXT.CHARS[K] := CH
	      end;
	    NEXTCH
	  until not (CH in LETTERSDIGITSORLEFTARROW);
	  IDLEN := K;
	  if K > ALFALENG then
	    begin
	      if K = 13 then
	        if ID = 'INITPROCED' then
	          if IDEXT.CHARS[11] = 'U' then
		    if IDEXT.CHARS[12] = 'R' then
		      if IDEXT.CHARS[13] = 'E' then
		        begin
		          SY := INITPROCSY; OP := NOOP; goto 1
		        end;
{fill the rest of the last word with blanks}
	      for I := K + 1 to ((K + 4) DIV 5) * 5 do
	        IDEXT.CHARS[I] := ' ' 
	    end
	  else
	    for I := FRW[K] to FRW[K+1] - 1 do
	      if RW[I] = ID then
	        begin
		  SY := RSY[I]; OP := ROP[I]; goto 1
	        end;
	  SY := IDENT; OP := NOOP;
  1:
	end;
      '0','1','2','3','4','5','6','7','8','9':
	begin
	  if OVERFLOW then
	    ;
	  {clear old errors}
	  SY := INTCONST; OP := NOOP;
	  ID := '          ';
	  I := 0;
	  repeat
	    I := I + 1;
	    if I <= ALFALENG then
	      ID[I] := CH
	    else if I <= MAXNAMELEN then
	      IDEXT.CHARS[I] := CH;
	    if I <= DIGMAX then
	      DIGIT[I] := ORD(CH) - ORD('0');
	    NEXTCH
	  until not (CH in DIGITS);
	  IDLEN := I;
	  IVAL := 0;
	  if CH = 'B' then
	    begin
	      if I > DIGMAX then
		begin
		  ERROR(174); I := DIGMAX
		end;
	      NC.INT:=0;
	      for K := 1 to I do
		if DIGIT[K] in [8,9] then
		  ERROR(252)
		else
		  NC.OCT[K+DIGMAX-I] := DIGIT[K];
	      VAL.IVAL := NC.INT;
	      NEXTCH
	    end
	  else
	    begin
	      SCALE := 0;
	      for K := 1 to I do
		if SCALE > 0 then
		  SCALE := SCALE + 1
		else
		  if IVAL < MAX10 then
		    IVAL := 10*IVAL + DIGIT[K]
		  else
		    if (IVAL = MAX10) and (DIGIT[K] <= 7) then
		      IVAL := 10*IVAL + DIGIT[K]
		    else
		      SCALE := SCALE + 1;
	      if CH = '.' then
		begin
		  NEXTCH;
		  if CH = '.' then
		    CH := ':'
		  else
		    begin
		      SY := REALCONST;
		      if not (CH in DIGITS) then
			ERROR(205)
		      else
			repeat
			  if SCALE > 0 then
			    SCALE := SCALE + 1
			  else
			    if IVAL < MAX10 then
			      IVAL := 10*IVAL + (ORD(CH)-ORD('0'))
			    else
			      if (IVAL = MAX10) and (CH <= '7') then
				IVAL := 10*IVAL + (ORD(CH)-ORD('0'))
			      else
				SCALE := SCALE + 1;
			  SCALE := SCALE - 1; NEXTCH
			until  not (CH in DIGITS);
		    end
		end;
	      if CH = 'E' then
		begin
		  SY := REALCONST;  NEXTCH;
		  SIGN := CH='-';
		  if (CH='+') or (CH='-') then
		    NEXTCH;
		  EXP := 0;
		  if not (CH in DIGITS) then
		    ERROR(205)
		  else
		    repeat
		      EXP := 10*EXP + (ORD(CH) - ORD('0'));
		      NEXTCH
		    until  not (CH in DIGITS);
		  if SIGN then
		    SCALE := SCALE - EXP
		  else
		    SCALE := SCALE + EXP
		end;
	      if SY = REALCONST then
		begin
		  RVAL := IVAL;
		  if SCALE # 0 then
		    begin
		      FAC := 10.0; ASCALE := ABS(SCALE);
		      loop
			if ODD(ASCALE) then
			  if SCALE > 0 then
			    RVAL := RVAL*FAC
			  else
			    RVAL := RVAL/FAC;
			ASCALE := ASCALE div 2;
		      exit if ASCALE=0;
			FAC := SQR(FAC);
		    end;
		      if OVERFLOW then
			begin
			  ERROR(206); RVAL := 0.0
			end
		    end;
		  NEWZ(LVP,REEL);
		  LVP^.RVAL := RVAL;  VAL.VALP := LVP
		end {REAL}
	      else
		{INTEGER}
		if SCALE = 0 then
		  VAL.IVAL := IVAL
		else
		  begin
		    ERROR(204); VAL.IVAL := 0
		  end
	    end
	end;
      '"':
	begin
	  SY := INTCONST; OP := NOOP; IVAL := 0; I := 0; HC.INT := 0;
	  NEXTCH;
	  while CH in HEXADIGITS do
	    begin
	      I := I + 1;
	      if I <= HEXMAX then
		if CH in DIGITS then
		  DIGIT[I] := 16*IVAL + ORD(CH) - ORD('0')
		else
		  DIGIT[I] := 16*IVAL + ORD(CH) - 67B;
	      NEXTCH
	    end;
	  if I > HEXMAX then
	    begin
	      ERROR(174);  I := HEXMAX
	    end;
	  for K := 1 to I do HC.HEX[K+HEXMAX-I] := DIGIT[K];
	  VAL.IVAL := HC.INT
	end;
      '''':
	begin
	  LGTH := 0; SY := STRINGCONST;   OP := NOOP;
	  STRINGTOOLONG := FALSE;
	  repeat
	    repeat
	      (* different NEXTCH so don't map lower case, etc. *)
	      NEXTSTRCH;
	      if LGTH < STRGLGTH then
		begin
		  LGTH := LGTH + 1; STRING[LGTH] := CH
		end
	      else
		STRINGTOOLONG := TRUE
	    until (EOLN(INPUT)) or (CH = '''');
	    if STRINGTOOLONG then
	      ERROR(301);
	    if EOLN(INPUT)  and  (CH#'''') then
	      ERROR(351)
	      (* different NEXTCH so don't map lower case, etc. *)
	      (* don't use nextstrch for char after end of string[caused loop] *)
	    else
	      NEXTCH  %this is embedded ' or char after string\
	  until CH # '''';
	  LGTH := LGTH - 1;   %NOW LGTH = NR OF CHARS IN STRING\
	  if LGTH = 1 then
	    VAL.IVAL := ORD(STRING[1])
	  else
	    begin
	      if LGTH = 0 then
		I := 1
	      else
		I := LGTH;
	      NEWZ(LVP,STRG:I);
	      with LVP^ do
		begin
		  SLGTH := LGTH;
		  for I := 1 to LGTH do SVAL[I] := STRING[I]
		end;
	      VAL.VALP := LVP
	    end
	end;
      ':':
	begin
	  OP := NOOP; NEXTCH;
	  if CH = '=' then
	    begin
	      SY := BECOMES; NEXTCH
	    end
	  else
	    SY := COLON
	end;
      '.':
	begin
	  OP := NOOP; NEXTCH;
	  if CH = '.' then
	    begin
	      SY := COLON; NEXTCH
	    end
	  else
	    SY := PERIOD
	end;
      '?','*','&','+','-','!','\',
      '@','#','=',
      ')','[',']',',',';','^','_','$':
	begin
	  SY := SSY[CH]; OP := SOP[CH];
	  NEXTCH
	end;

      '(':
	begin
	  NEXTCH;
	  if CH='*' then
	    begin
	      SCANCOMMENT(')'); goto 2
	    end
	  else
	    begin
	      SY := LPARENT; OP := NOOP
	    end
	end;
      '{':
	begin
	  SCANCOMMENT('}'); goto 2
	end;
      '%':
	begin
	  SCANCOMMENT('\'); goto 2
	end;
      '/':
	begin
	  NEXTCH;
	  if CH='*' then
	    begin
	      SCANCOMMENT('/'); goto 2
	    end
	  else
	    begin
	      SY := MULOP; OP := RDIV
	    end
	end;
      '<','>':
	begin
	  SY := SSY[CH]; OP := SOP[CH]; NEXTCH;
	  if CH = '=' then
	    begin
	      if OP = LTOP then
		OP := LEOP
	      else
		OP := GEOP;
	      NEXTCH
	    end
	  else
	    if (CH = '>') and (OP = LTOP) then
	      begin
		OP := NEOP;  NEXTCH
	      end
	end;
      others:
	begin
	  ERROR(216); NEWCH; INSYMBOL
	end
      end %CASE\
  end %INSYMBOL\;
procedure ENTERID(FCP: CTP);
  %ENTER ID POINTED AT BY FCP INTO THE NAME-TABLE,
   WHICH ON EACH DECLARATION LEVEL IS ORGANISED AS
   AN UNBALANCED BINARY TREE\
  var
    NAM: ALFA; LCP, LCP1: CTP; LLEFT: BOOLEAN;
  begin
(* 231 *)
    if FCP^.KLASS = TYPES
{See documentation for CREF_TYPES - in the case of types, we generate
 a unique numeric and use it instead of the actual name.  At the end of
 the block, CREF_TYPES puts out the real definition}
      then
	begin
	  FCP^.CREF_TEMP := NEXT_CREF_TEMP;  {get the numeric}
	  if CREF then
	    WRITE(CHR(1B),CHR(6),NEXT_CREF_TEMP:6:O,CHR(2B));
						  {use it for CREF entry}
	  NEXT_CREF_TEMP := NEXT_CREF_TEMP + 1;   {advance it for the next}
	  FCP^.NEXT := TYPE_LIST;	{link us in list for CREF_TYPES}
	  TYPE_LIST := FCP;
	end
      else if CREF then
	WRITE(CHR(1B),CHR(21),FCP^.NAME,'/',DISPLAY[TOP].BLKNAME,CHR(2B));
    LCP := DISPLAY[TOP].FNAME;
    if LCP = nil then
      DISPLAY[TOP].FNAME := FCP
    else 
{For the sake of "efficiency" we have microcoded this loop.  You
 should ignore the stuff off at the right, and read the comments
 as part of the code.}
      begin
	repeat
	  LCP1 := LCP;
	  case CTPCOM(LCP,FCP) of
	    -1: begin LCP := LCP^.RLINK; LLEFT := FALSE end;
	    0:  begin error(302); LCP := LCP^.RLINK; LLEFT := FALSE end;
	    1:  begin LCP := LCP^.LLINK; LLEFT := TRUE end
	  end
	until LCP = nil;
	if LLEFT then
	  LCP1^.LLINK := FCP
	else
	  LCP1^.RLINK := FCP
      end;
    with FCP^ do
      begin
	LLINK := nil; RLINK := nil; SELFCTP := nil
      end
  end %ENTERID\;

procedure SEARCHSECTION(FCP: CTP; var FCP1: CTP);
  %TO FIND RECORD FIELDS AND FORWARD DECLARED PROCEDURE ID'S
   --> PROCEDURE PROCEDUREDECLARATION
   --> PROCEDURE SELECTOR\
  begin
    while FCP # nil do
      case IDCOMP(FCP,IDLEN,ID,IDEXT) of
	-1: FCP := FCP^.RLINK;
	 0:  goto 1;
	 1:  FCP := FCP^.LLINK
      end;
  1:
    FCP1 := FCP
  end %SEARCHSECTION\;

(* 231 - pass CREF as an arg, so we can turn it off when doing forwards *)

procedure IDSEARCH(FIDCLS: SETOFIDS; var FCP: CTP; CREF_TEMP: INTEGER);
  var
    LCP: CTP;
  begin
    for DISX := TOP downto 0 do
      begin
	LCP := DISPLAY[DISX].FNAME;
	while LCP # nil do
	  case IDCOMP(LCP,IDLEN,ID,IDEXT) of
	    -1: LCP := LCP^.RLINK;
	    0:  if LCP^.KLASS in FIDCLS then
		  goto 1
	        else
		  begin
		    if PRTERR then
		      ERROR(401);
		    goto 2
		  end;
	    1: LCP := LCP^.LLINK;
	  end;
      end;
  2:
    LCP := nil;  {Use NIL if don't find something better below}
    (* save some info for so CREF will know the block name *)
    DISX := TOP; %IF FORWARD, WILL BE IN THIS BLOCK\
    while DISPLAY[DISX].OCCUR <> BLCK do DISX := DISX - 1;
    %SEARCH NOT SUCCSESSFUL; SUPPRESS ERROR MESSAGE IN CASE
     OF FORWARD REFERENCED TYPE ID IN POINTER TYPE DEFINITION
     --> PROCEDURE SIMPLETYPE\
    if PRTERR then
      begin
	ERROR(253);
	%TO AVOID RETURNING NIL, REFERENCE AN ENTRY
	 FOR AN UNDECLARED ID OF APPROPRIATE CLASS
	 --> PROCEDURE ENTERUNDECL\
	if TYPES in FIDCLS then
	  LCP := UTYPPTR
	else
	  if VARS in FIDCLS then
	    LCP := UVARPTR
	  else
	    if FIELD in FIDCLS then
	      LCP := UFLDPTR
	    else
	      if KONST in FIDCLS then
		LCP := UCSTPTR
	      else
		if PROC in FIDCLS then
		  LCP := UPRCPTR
		  (* non-loc gotos *)
		else
		  if FUNC in FIDCLS then
		    LCP := UFCTPTR
		  else
		    LCP := ULBLPTR
      end;
  1:
(* 231 *)

{If CREF_TEMP is non-zero, then we are evaluating a forward reference that
 has already been handed to CREF.  At that time a temporary symbol was
 GENSYM'ed using Xnnnn.  We now have to tell CREF the real symbol.  That is
 what the ^H operator is for.

 Also, if the symbol is a TYPES, then we aren't using its name, but rather
 its "unique numeric".  CREF_TYPES, q.v., will rename these at the end of
 the block.}

    if CREF then
      if CREF_TEMP <> 0
	then WRITE(CHR(10B),CHR(6),CREF_TEMP:6:O,CHR(6),LCP^.CREF_TEMP:6:O)
      else if LCP^.KLASS = TYPES
	then WRITE(CHR(1B),CHR(6),LCP^.CREF_TEMP:6:O)
      else WRITE(CHR(1B),CHR(21),ID,'/',DISPLAY[DISX].BLKNAME);
    FCP := LCP
  end %IDSEARCH\;

procedure SETNAME(LCP: CTP);
{Sets up the NAME fields in LCP}
	var I: INTEGER;
  begin
  with LCP^ do
    begin
    NAME := ID; NAMELEN := IDLEN;
    if IDLEN > ALFALENG then
      begin
	NEWZ(NAMEEXT,CHARFORM:IDLEN);
	with NAMEEXT^ do
	  for I := 1 to (IDLEN - 6) div 5 do
	    WORDS[I] := IDEXT.WORDS[I]
      end
    end
  end;

procedure GETBOUNDS(FSP: STP; var FMIN,FMAX: INTEGER);
  %GET INTERNAL BOUNDS OF SUBRANGE OR SCALAR TYPE\
  %ASSUME (FSP # NIL) AND (FSP^.FORM <= SUBRANGE) AND (FSP # INTPTR)
   AND  NOT COMPTYPES(REALPTR,FSP)\
  begin
    with FSP^ do
      if FORM = SUBRANGE then
	begin
	  FMIN := MIN.IVAL; FMAX := MAX.IVAL
	end
      else
	begin
	  FMIN := 0;
	  if FSP = CHARPTR then
	    FMAX := 177B
	  else
	    if FCONST # nil then
	      FMAX := FCONST^.VALUES.IVAL
	    else
	      FMAX := 0
	end
  end %GETBOUNDS\;


procedure SKIPIFERR(FSYINSYS:SETOFSYS; FERRNR:INTEGER; FSKIPSYS: SETOFSYS);
  var
    I,OLDCHCNT,OLDLINECNT : INTEGER;
  begin
    if not (SY in FSYINSYS) then
      begin
	ERROR(FERRNR);
	OLDLINECNT := LINECNT; OLDCHCNT := CHCNT;
	while not (SY in FSKIPSYS or FSYINSYS) do
	  begin
	    if OLDLINECNT # LINECNT then
	      OLDCHCNT := 1;
	    for I := OLDCHCNT to CHCNT-1 do
	      if I <= CHCNTMAX then
		ERRLINE [I] := '*';
	    OLDCHCNT := CHCNT; OLDLINECNT := LINECNT; ERRORINLINE := TRUE;
	    INSYMBOL
	  end
	    %SKIP INPUT STRING UNTIL RELEVANT SYMBOL FOUND\
      end;
    FOLLOWERROR := FALSE
  end;

procedure IFERRSKIP(FERRNR: INTEGER; FSYS: SETOFSYS);
  begin
    SKIPIFERR(FSYS,FERRNR,FSYS)
  end;

procedure ERRANDSKIP(FERRNR: INTEGER; FSYS: SETOFSYS);
  begin
    SKIPIFERR([ ],FERRNR,FSYS)
  end;

(* 233 - do better temp allocation *)

{LC represents the low segment.  In the main body, it is an address in the
low seg.  In a procedure it is an offset from AC 16.  If you need a local,
LC is the place to allocate it.  TEMP_LC gets you a block of size N from
this area.  At the moment we allocate things in a strict stackwise fashion.  
The method will be described below.  However you should not depend upon
this.  Don't change LC yourself, but always use TEMP_LC.  In addition there
is a DISP_LC for disposing of blocks allocated this way.  At the moment it
doesn't do anything, but we are preparing for more intelligent optimizations,
which would require better allocation procedures.  At the moment we have
commented out DISP_LC and all calls to it, since they aren't needed.

The following is classified information. It should only be used for working
with the allocation procedures.  Everyone should use TEMP_LC and DISP_LC
to allocate and deallocate.  Currently LC space is allocated as a stack.
Anyone who needs extra space takes it from LC, and increments LC by the
size of the space taken.  STATEMENT saves the old value and restores it
on exit.  This assumes that no uses cross statement boundaries.  Note that
FOR statements use some temps for the life of the loop.  This means that
different recursive levels of statement may have different old values.}

function TEMP_LC(SIZE:XADDRRANGE):XADDRRANGE;
  begin
    TEMP_LC := LC;
    LC := LC + SIZE;
    if LC > LCMAX then
      LCMAX := LC;
  end;

{procedure DISP_LC(ADDR,SIZE:XADDRRANGE);
  begin
  end;}

procedure PROGSTAT;
  var
    STSYM,ENDSYM:SYMBOL; I: INTEGER;
  begin
    if SY=PROGRAMSY then
      begin
	if MAIN then
	  begin
	    STSYM:=LPARENT; ENDSYM := RPARENT
	  end
	else
	  begin
	    STSYM:=COMMA; ENDSYM := SEMICOLON
	  end;
	INSYMBOL;
	if SY # IDENT then
	  ERROR(209);
	FILENAME := ID;  INSYMBOL;
	if SY = STSYM then
	  begin
	    repeat
	      INSYMBOL;
	      if not (SY = IDENT) then
		ERROR(209);
	      NEWZ(NPROGFILE);
	      NPROGFILE^.FILID := ID;  
	      NPROGFILE^.FILIDLEN := IDLEN;
	      if IDLEN > 10 then
	        begin
		NEWZ(NPROGFILE^.FILIDEXT,CHARFORM:IDLEN);
		with NPROGFILE^.FILIDEXT^ do
		  for I := 1 to (IDLEN - 6) div 5 do
		    WORDS[I] := IDEXT.WORDS[I];
		end;
	      NPROGFILE^.NEXT := nil;
	      if FPROGFILE = nil then
		begin
		  FPROGFILE := NPROGFILE;  LPROGFILE := NPROGFILE
		end
	      else
		begin
		  LPROGFILE^.NEXT := NPROGFILE;  LPROGFILE := NPROGFILE
		end;
	      INSYMBOL;
	      if (SY=COLON) and MAIN then
		begin
		  INSYMBOL;
		  while SY in [ADDOP,MULOP,RELOP] do
		    begin
		      if (OP = MUL) and (not TOPS10) then
			NPROGFILE^.WILD := TRUE
		      else
			if OP = PLUS then
			  NPROGFILE^.NEWGEN := TRUE
			else
			  if OP = MINUS then
			    NPROGFILE^.OLDFILE := TRUE
			  else
			    if OP = RDIV then
			      NPROGFILE^.INTERACT := TRUE
			    else
			      if OP = NEOP then
				NPROGFILE^.SEEEOL := TRUE
			      else
				ERROR(158);
		      INSYMBOL
		    end
		end;
	      IFERRSKIP(158,[ENDSYM,COMMA])
	    until SY=ENDSYM;
	    if MAIN then
	      INSYMBOL
	  end;
	(* Allow null file list in prog. statement *)
	IFERRSKIP(156,[SEMICOLON]);
	INSYMBOL
      end
  end;
{CREF_TYPES is needed because of the possibility of ambiguous forward
type references.  Thus for types we have to follow the FAIL protocol of
using unique numerics for all references and then doing defines and
merges on them.  The define must be done after all references, i.e. at
the end of the block.}

  procedure CREF_TYPES(OLD_TYPE_LIST: CTP);
    begin
      if CREF then
	while TYPE_LIST <> OLD_TYPE_LIST do
			 {loop over all types defined in this block}
    	  with TYPE_LIST^ do
	    begin
	      WRITE(CHR(11B){define},CHR(6){length},CREF_TEMP:6:O,
      		    CHR(21),NAME,'/',DISPLAY[TOP].BLKNAME);
	      TYPE_LIST := NEXT
	    end
(* 234 - We must restore TYPE_LIST even if CREF is not being done.
  The problem is that we are about to exit a block.  TYPE_LIST
  currently points to symbols defined in the current block.  Once
  we exit from this block, that part of the symbol table is
  garbage.  Thus we must make TYPE_LIST point to OLD_TYPE_LIST.
  If we don't, then the next time we ENTERID on a type decl.,
  we will set its NEXT field to garbage.  Of course when CREF
  is off we don't care what the NEXT field is.  But COPYCTP will
  try to follow it, and we will have big trouble. *)
      else TYPE_LIST := OLD_TYPE_LIST
    end;


procedure BLOCK(FPROCP: CTP; FSYS,LEAVEBLOCKSYS: SETOFSYS);
  var
    LSY: SYMBOL;
    ORIGLINENR:packed array [1..5] of CHAR;
    ORIGPAGECNT,ORIGSUBPAGE,ORIGLINECNT:INTEGER;
    ORIGPAGE:PAGEELEM;
    ORIGCH:CHAR;
    LCPAR: XADDRRANGE;  %SAVES LOCATION FROM WHERE
			LOCAL AREAS ARE SET TO ZERO\
    HEAPMARK,GLOBMARK: INTEGER;
    FORWPTR : CTP;           %TEST FOR FORWORD DECLARED PROCEDURES\
(* 231 *)
    OLD_TYPE_LIST: CTP;	{Pointer to tail of TYPE_LIST so we can tell
			 which part is in this block}

  procedure CONSTANT(FSYS: SETOFSYS; var FSP: STP; var FVALU: VALU);
    var
      LSP,LSP1: STP; LCP: CTP; SIGN: (NONE,POS,NEG);
    begin
      LSP := nil; FVALU.IVAL := 0;
      SKIPIFERR(CONSTBEGSYS,207,FSYS);
      if SY in CONSTBEGSYS then
	begin
	  if SY = STRINGCONST then
	    begin
	      if LGTH = 1 then
		LSP := CHARPTR
	      else
		if LGTH = ALFALENG then
		  LSP := ALFAPTR
		else
		  begin
		    NEWZ(LSP,ARRAYS); NEWZ(LSP1,SUBRANGE);
		    with LSP^ do
		      begin
			AELTYPE := CHARPTR; INXTYPE := LSP1;
			SIZE := (LGTH+4) div 5; ARRAYPF := TRUE;
			BITSIZE := BITMAX; SELFSTP := nil
		      end;
		    with LSP1^ do
		      begin
			SIZE := 1; BITSIZE := BITMAX;
			MIN.IVAL := 1; MAX.IVAL := LGTH; RANGETYPE  := nil
		      end
		  end;
	      FVALU := VAL; INSYMBOL
	    end
	  else
	    begin
	      SIGN := NONE;
	      if (SY = ADDOP) and (OP in [PLUS,MINUS]) then
		begin
		  if OP = PLUS then
		    SIGN := POS
		  else
		    SIGN := NEG;
		  INSYMBOL
		end;
	      if SY = IDENT then
		begin
		  IDSEARCH([KONST],LCP,0);
		  with LCP^ do
		    begin
		      LSP := IDTYPE; FVALU := VALUES
		    end;
		  if SIGN # NONE then
		    if LSP = INTPTR then
		      begin
			if SIGN = NEG then
			  FVALU.IVAL := -FVALU.IVAL
		      end
		    else
		      if LSP = REALPTR then
			begin
			  if SIGN = NEG then
			    FVALU.VALP^.RVAL := -FVALU.VALP^.RVAL
			end
		      else
			ERROR(167);
		  INSYMBOL;
		end
	      else
		if SY = INTCONST then
		  begin
		    if SIGN = NEG then
		      VAL.IVAL := -VAL.IVAL;
		    LSP := INTPTR; FVALU := VAL; INSYMBOL
		  end
		else
		  if SY = REALCONST then
		    begin
		      if SIGN = NEG then
			VAL.VALP^.RVAL := -VAL.VALP^.RVAL;
		      LSP := REALPTR; FVALU := VAL; INSYMBOL
		    end
		  else
		    ERRANDSKIP(168,FSYS)
	    end;
	  IFERRSKIP(166,FSYS)
	end;
      FSP := LSP
    end %CONSTANT\;

(* 235 - 4-word sets *)

{WARNING: if you use this for anything involving sets, it is better to
 call this routine before generating any code.  See the comment at
 the beginning of SETCONSTRUCTOR}

  function COMPTYPES(FSP1,FSP2: STP) : BOOLEAN;
    %DECIDE WHETHER STRUCTURES POINTED AT BY FSP1 AND FSP2 ARE COMPATIBLE\
    var
      NXT1,NXT2: CTP; COMP: BOOLEAN; LMIN1,LMIN2,LMAX1,LMAX2,I: INTEGER;
      LTESTP1,LTESTP2: TESTP;
    begin
      if FSP1 = FSP2 then
	COMPTYPES := TRUE
      else
	if (FSP1 # nil) and (FSP2 # nil) then
	  if FSP1^.FORM = FSP2^.FORM then
	    case FSP1^.FORM of
	      SCALAR:
		COMPTYPES := FALSE;
		% IDENTICAL SCALARS DECLARED ON DIFFERENT LEVELS ARE
		 NOT RECOGNIZED TO BE COMPATIBLE\
	      SUBRANGE:
		COMPTYPES := COMPTYPES(FSP1^.RANGETYPE,FSP2^.RANGETYPE);
(* 240 - conformant *)
	      CONFORMANT:
		COMPTYPES := COMPTYPES(FSP1^.BOUNDTYPE,FSP2^.BOUNDTYPE);
	      POINTER:
		begin
		  COMP := FALSE; LTESTP1 := GLOBTESTP;
		  LTESTP2 := GLOBTESTP;
		  while LTESTP1 # nil do
		    with LTESTP1^ do
		      begin
			if (ELT1 = FSP1^.ELTYPE) and (ELT2 = FSP2^.ELTYPE) then
			  COMP := TRUE;
			LTESTP1 := LASTTESTP
		      end;
		  if not COMP then
		    begin
		      NEWZ(LTESTP1);
		      with LTESTP1^ do
			begin
			  ELT1 := FSP1^.ELTYPE;  ELT2 := FSP2^.ELTYPE;
			  LASTTESTP := GLOBTESTP
			end;
		      GLOBTESTP := LTESTP1;
		      COMP := COMPTYPES(FSP1^.ELTYPE,FSP2^.ELTYPE)
		    end;
		  COMPTYPES := COMP; GLOBTESTP := LTESTP2
		end;
(* 235 - set of char *)
	      POWER,QPOWER:
		COMPTYPES := COMPTYPES(FSP1^.ELSET,FSP2^.ELSET);
(* 240 - conformant *)
	      ARRAYS:
		if COMPTYPES(FSP1^.AELTYPE,FSP2^.AELTYPE) and
		   COMPTYPES(FSP1^.INXTYPE,FSP2^.INXTYPE) and
		   (FSP1^.ARRAYPF = FSP2^.ARRAYPF)
		  then if FSP1^.ARRAYCONF or FSP2^.ARRAYCONF
		    then COMPTYPES := TRUE
		    else
		      begin
		      GETBOUNDS (FSP1^.INXTYPE,LMIN1,LMAX1);
		      GETBOUNDS (FSP2^.INXTYPE,LMIN2,LMAX2);
		      COMPTYPES := (LMIN1 = LMIN2) and (LMAX1 = LMAX2)
		      end
		  else COMPTYPES := FALSE;
		%ALTERNATIVES: -- ADD A THIRD BOOLEAN TERM: INDEXTYPE MUST
		 BE COMPATIBLE. MAY GIVE TROUBLE FOR ASSIGNMENT OF STRINGCONSTANTS
		 -- ADD A FOURTH BOOLEAN TERM: LOWBOUNDS MUST
		 BE THE SAME\
	      RECORDS:
		COMPTYPES := FALSE;
	      FILES:
		COMPTYPES := COMPTYPES(FSP1^.FILTYPE,FSP2^.FILTYPE)
	      end %CASE\
	  else
	    %FSP1^.FORM # FSP2^.FORM\
	    if FSP1^.FORM = SUBRANGE then
	      COMPTYPES := COMPTYPES(FSP1^.RANGETYPE,FSP2)
(* 240 - conformants *)
	    else if FSP2^.FORM = SUBRANGE then
	      COMPTYPES := COMPTYPES(FSP1,FSP2^.RANGETYPE)
	    else if FSP1^.FORM = CONFORMANT then
	      COMPTYPES := COMPTYPES(FSP1^.BOUNDTYPE,FSP2)
	    else if FSP2^.FORM = CONFORMANT then
	      COMPTYPES := COMPTYPES(FSP1,FSP2^.BOUNDTYPE)
	    else
{ [] hack - see SETCONSTRUCTOR }
		begin
		COMPTYPES := FALSE;
		if FSP2^.FORM = QPOWER then
		  if FSP2^.ELSET = nil then
		    if FSP1^.FORM = POWER then
		      begin FSP2^.FORM := POWER; COMPTYPES := TRUE;
			    FSP2^.SIZE := 2 end;
		if FSP1^.FORM = QPOWER then
		  if FSP1^.ELSET = nil then
		    if FSP2^.FORM = POWER then
		      begin FSP1^.FORM := POWER; COMPTYPES := TRUE;
			    FSP1^.SIZE := 2 end;
		end
	else
	  COMPTYPES := TRUE
    end %COMPTYPES\;

  procedure CHECKASSIGN(LATTR:ATTR);
{This makes sure that we can validly assign to GATTR.  Make sure it
 is not a bound identifier}
    begin
    if LATTR.NOASSIGN then
      ERROR(559);
    end;

  function STRING(FSP: STP) : BOOLEAN;
    begin
      STRING := FALSE;
      if FSP # nil then
	if FSP^.FORM = ARRAYS then
	  if COMPTYPES(FSP^.AELTYPE,CHARPTR) then
	    STRING := TRUE
    end %STRING\;
    function CHECKSIZE(I:INTEGER):INTEGER;

{There are three limits on size:
 Offsets: In section 0, code generation can handle offsets
  up to 777777.  In non-0 sections, the high-order bit is taken
  to be a sign bit, so we can only go up to 377777.  These are
  enforced here as a limit on size of objects.
 Relocatable addresses: Addresses in the low segment can't be
  bigger than the start of the high-seg or LINK will think it
  they are in the wrong segment.  These are enforced by verifying
  that LCMAIN < HIGHSTART at the end.
 Size of local storage for any one block: This is limited to 
  377777, since otherwise ADJSP will see it as negative.}

      begin
	if EXTENDED_ADDRESSING then
	  if ABS(I) >= MAXXADDR then
            begin ERROR(369); I := 0 end
	  else
	else if ABS(I) > MAXADDR then
	  begin ERROR(369); I := 0 end;
	CHECKSIZE := I
      end;

    function LOG2(FVAL: INTEGER): BITRANGE;
      var
	E: BITRANGE; H: INTEGER;
      begin
	E := 0;  H := 1;
	{There are two complicating issues here:
	1 - 200 000 000 000 is the highest power of 2, so the
	loop below goes forever for them
	2 - the caller has often added 1, thus making 377 777 777 777
	into 400 000 000 000, which is negative!!
	In both of these cases we want to return 35}
	if (FVAL-1) >= 200000000000B then
	  E := 35
	else
	  repeat
	    E := E + 1; H := H * 2
	  until FVAL <= H;
	LOG2 := E
      end %LOG2\;
    procedure SIMPLETYPE(FSYS: SETOFSYS;
			 var FSP: STP;
			 var FSIZE: XADDRRANGE;
			 var FBITSIZE: BITRANGE);
      var
	LSP,LSP1: STP; LCP,LCP1: CTP; TTOP: DISPRANGE;
	I,LCNT: INTEGER; LVALU: VALU; LBITSIZE: BITRANGE;
      begin
	FSIZE := 1;
	SKIPIFERR(SIMPTYPEBEGSYS,208,FSYS);
	if SY in SIMPTYPEBEGSYS then
	  begin
	    if SY = LPARENT then
	      begin
		TTOP := TOP;   %DECL. CONSTS LOCAL TO INNERMOST BLOCK\
		while DISPLAY[TOP].OCCUR # BLCK do TOP := TOP - 1;
		NEWZ(LSP,SCALAR,DECLARED);
		LSP^.SIZE := 1; LCP1 := nil; LCNT := 0;
		repeat
		  INSYMBOL;
		  if SY = IDENT then
		    begin
		      NEWZ(LCP,KONST);
		      SETNAME(LCP);
		      with LCP^ do
			begin
			  IDTYPE := LSP; NEXT := LCP1;
			  VALUES.IVAL := LCNT;
			end;
		      ENTERID(LCP); LCNT := LCNT + 1;
		      LCP1 := LCP; INSYMBOL
		    end
		  else
		    ERROR(209);
		  IFERRSKIP(166,FSYS or [COMMA,RPARENT])
		until SY # COMMA;
		TOP := TTOP;
		with LSP^ do
		  begin
		    SELFSTP := nil; FCONST := LCP1; BITSIZE := LOG2(LCNT)
		  end;
		if SY = RPARENT then
		  INSYMBOL
		else
		  ERROR(152)
	      end
	    else
	      begin
		if SY = IDENT then
		  begin
		    IDSEARCH([TYPES,KONST],LCP,0);
		    INSYMBOL;
		    if LCP^.KLASS = KONST then
		      begin
			NEWZ(LSP,SUBRANGE);
			with LSP^, LCP^ do
			  begin
			    SELFSTP := nil; RANGETYPE := IDTYPE;
			    if STRING(RANGETYPE) then
			      begin
				ERROR(303); RANGETYPE := nil
			      end;
			    MIN := VALUES; SIZE := 1
			  end;
			if SY = COLON then
			  INSYMBOL
			else
			  ERROR(151);
			CONSTANT(FSYS,LSP1,LVALU);
			with LSP^ do
			  begin
			    MAX := LVALU;
			    if MIN.IVAL<0 then
			      BITSIZE := BITMAX
			    else
			      BITSIZE := LOG2(MAX.IVAL + 1);
			    if RANGETYPE # LSP1 then
			      ERROR(304)
			  end
		      end
		    else
		      begin
			LSP := LCP^.IDTYPE;
			if LSP # nil then
			  FSIZE := LSP^.SIZE;
		      end
		  end %SY = IDENT\
		else
		  begin
		    NEWZ(LSP,SUBRANGE);
		    CONSTANT(FSYS or [COLON],LSP1,LVALU);
		    if STRING(LSP1) then
		      begin
			ERROR(303); LSP1 := nil
		      end;
		    with LSP^ do
		      begin
			RANGETYPE := LSP1; MIN := LVALU; SIZE := 1
		      end;
		    if SY = COLON then
		      INSYMBOL
		    else
		      ERROR(151);
		    CONSTANT(FSYS,LSP1,LVALU);
		    with LSP^ do
		      begin
			SELFSTP := nil; MAX := LVALU;
			if MIN.IVAL<0 then
			  BITSIZE := BITMAX
			else
			  BITSIZE := LOG2(MAX.IVAL + 1);
			if RANGETYPE # LSP1 then
			  ERROR(304)
		      end
		  end;
		if LSP # nil then
		  with LSP^ do
		    if FORM = SUBRANGE then
		      if RANGETYPE # nil then
			if RANGETYPE = REALPTR then
			  (* make subranges of real illegal *)
			  ERROR(210)
			else
			  if MIN.IVAL > MAX.IVAL then
			    ERROR(451)
	      end;
	    FSP := LSP;
	    if LSP#nil then
	      FBITSIZE := LSP^.BITSIZE
	    else
	      FBITSIZE := 0;
	    IFERRSKIP(166,FSYS)
	  end
	else
	  begin
	    FSP := nil; FBITSIZE := 0
	  end
      end %SIMPLETYPE\;

  procedure TYP(FSYS: SETOFSYS; var FSP: STP; var FSIZE: XADDRRANGE;
		var FBITSIZE: BITRANGE);
    var
      FHASFILE,LHASFILE:BOOLEAN;
      LSP,LSP1,LSP2: STP; OLDTOP: DISPRANGE; LCP: CTP;
      LSIZE,DISPL: XADDRRANGE;
      I,LMIN,LMAX: INTEGER;
      PACKFLAG: BOOLEAN; LBITSIZE: BITRANGE;
      LBTP: BTP; BITCOUNT:INTEGER;

      (* internal files *)
    procedure FIELDLIST(FSYS: SETOFSYS;
			var FRECVAR: STP;
			var FFIRSTFIELD: CTP;
			var FHASFILE:BOOLEAN);
      var
	LHASFILE:BOOLEAN;
	LCP,LCP1,NXT,NXT1: CTP; LSP,LSP1,LSP2,LSP3,LSP4,TAGSP: STP;
	MINSIZE,MAXSIZE,LSIZE: XADDRRANGE; LVALU: VALU;
	LBITSIZE: BITRANGE;
	LBTP: BTP; MINBITCOUNT:INTEGER;
	LID : ALFA; LIDLEN: INTEGER; LIDEXT: XBIGNAMEPTR;

      procedure RECSECTION(var FCP: CTP; FSP: STP);
	begin
	  if not PACKFLAG or (LSIZE > 1)  or  (LBITSIZE = 36) then
	    begin
	      if BITCOUNT > 0 then
		begin
		  DISPL := DISPL + 1; BITCOUNT := 0
		end;
	      with FCP^ do
		begin
		  IDTYPE := FSP; FLDADDR := DISPL;
		  PACKF := NOTPACK; FCP := NEXT;
		  DISPL := DISPL + LSIZE
		end
	    end
	  else
	    begin %PACK RECORD-SECTION\
	      BITCOUNT := BITCOUNT + LBITSIZE;
	      if BITCOUNT>BITMAX then
		begin
		  DISPL := DISPL + 1;
		  BITCOUNT := LBITSIZE
		end;
	      if (LBITSIZE = 18)  and  (BITCOUNT in [18,36]) then
		begin
		  with FCP^ do
		    begin
		      IDTYPE := FSP;  FLDADDR := DISPL;
		      if BITCOUNT = 18 then
			PACKF := HWORDL
		      else
			PACKF := HWORDR;
		      FCP := NEXT
		    end
		end
	      else
		begin
		  NEWZ(LBTP,RECORDD);
		  with LBTP^.BYTE do
		    begin
		      SBITS := LBITSIZE;  PBITS := BITMAX - BITCOUNT;
		      if DISPL > HWCSTMAX then
			begin 
			  XRELADDR := DISPL;
			  XBIT := 1;
			  XIBIT := 0;
			  XIREG := BIXREG
			end
		      else
			begin
			  RELADDR := DISPL;
			  XBIT := 0;
		          IBIT := 0;
			  IREG := BIXREG
		        end;
		    end;
		  with LBTP^ do
		    begin
		      LAST := LASTBTP; FIELDCP := FCP
		    end;
		  LASTBTP := LBTP;
		  with FCP^ do
		    begin
		      IDTYPE := FSP;  PACKF := PACKK;  FCP := NEXT
		    end
		end
	    end
	end % RECSECTION\;
      begin {FIELDLIST}
	NXT1 := nil; LSP := nil; FRECVAR := nil; FHASFILE := FALSE;
	while SY = SEMICOLON do INSYMBOL;
	SKIPIFERR(FSYS or [IDENT,CASESY],452,FSYS);
	while SY = IDENT do
	  begin
	    NXT := NXT1;
	    loop
	      if SY = IDENT then
		begin
		  NEWZ(LCP,FIELD);
		  SETNAME(LCP);
		  with LCP^ do
		    begin
		    IDTYPE := nil; NEXT := NXT
		    end;
		  NXT := LCP;
		  ENTERID(LCP);  INSYMBOL
		end
	      else
		ERROR(209);
	      SKIPIFERR([COMMA,COLON],166,FSYS or [SEMICOLON,CASESY]);
	    exit if SY # COMMA;
	      INSYMBOL
	  end;
	    if SY = COLON then
	      INSYMBOL
	    else
	      ERROR(151);
	    TYP(FSYS or [CASESY,SEMICOLON],LSP,LSIZE,LBITSIZE);
	    if LSP # nil then
	      if (LSP^.FORM = FILES) or LSP^.HASFILE then
		FHASFILE := TRUE;
	    while NXT # NXT1 do RECSECTION(NXT,LSP);
	    %RESERVES SPACE FOR ONE RECORDSECTION \
	    NXT1 := LCP;
	    while SY = SEMICOLON do
	      begin
		INSYMBOL;
		SKIPIFERR(FSYS or [IDENT,CASESY,SEMICOLON],452,FSYS)
	      end
	  end %WHILE\;
	NXT := nil;
	while NXT1 # nil do
	  with NXT1^ do
	    begin
	      LCP := NEXT; NEXT := NXT; NXT := NXT1; NXT1 := LCP
	    end;
	FFIRSTFIELD := NXT;
	if SY = CASESY then
	  begin
	    LCP := nil;  %POSSIBILITY OF NO TAGFIELDIDENTIFIER\
	    INSYMBOL;
	    if SY = IDENT then
	      begin
{We are possibly overclever here.  LID was used for two reasons:
 to save the current symbol incase it is going to be the tag field
 name, and in case it is going to be the tag type.  We avoid
 creating LIDEXT except in the first case.}
		LID := ID;
		LIDLEN := IDLEN;
	        INSYMBOL;
{Note: if this is really colon or OF, it doesn't touch IDEXT.  So
 we can still get the IDEXT from the original symbol.  If it is
 something else, there is an error, and we don't need it anyway.}
		if (SY#COLON) and (SY#OFSY) then
		  begin
		    ERROR(151); ERRANDSKIP(160,FSYS or [LPARENT]); GOTO 1
		  end
		else
		  begin
		    if SY = COLON then
		      begin
			if LIDLEN > 10 then
			  begin
			    NEWZ(LIDEXT,CHARFORM:IDLEN);
			    with LIDEXT^ do
			      for I := 1 to (LIDLEN - 6) div 5 do
			        WORDS[I] := IDEXT.WORDS[I]
			  end;
			NEWZ(LSP,TAGFWITHID);  NEWZ(LCP,FIELD);
			with LCP^ do
			  begin
			    NAME := LID; NAMELEN := LIDLEN; NAMEEXT := LIDEXT;
			    IDTYPE := nil; NEXT := nil
			  end;
			ENTERID(LCP); INSYMBOL;
			if SY # IDENT then
			  begin
			    ERRANDSKIP(209,FSYS or [LPARENT]); goto 1
			  end
			else
			  begin
			    LID := ID;
			    LIDLEN := IDLEN;
			    {Note - no LIDEXT - this is because the
				next symbol should be OF, which will
				not involve the IDEXT part}
			    INSYMBOL;
			    if SY # OFSY then
			      begin
				ERRANDSKIP(160,FSYS or [LPARENT]); goto 1
			      end
			    else
			  end
		      end
		    else
		      NEWZ(LSP,TAGFWITHOUTID);
		    with LSP^ do
		      begin
			SIZE:= 0; SELFSTP := nil;  FSTVAR := nil;
			if FORM=TAGFWITHID then
			  TAGFIELDP:=nil
			else
			  TAGFIELDTYPE := nil
		      end;
		    FRECVAR := LSP;  ID := LID; IDLEN := LIDLEN;
		    IDSEARCH([TYPES],LCP1,0);
		    TAGSP := LCP1^.IDTYPE;
		    if TAGSP # nil then
		      if (TAGSP^.FORM <= SUBRANGE) or STRING(TAGSP) then
			begin
			  if COMPTYPES(REALPTR,TAGSP) then
			    ERROR(210)
			  else
			    if STRING(TAGSP) then
			      ERROR(169);
			  with LSP^ do
			    begin
			      BITSIZE := TAGSP^.BITSIZE;
			      if FORM = TAGFWITHID then
				TAGFIELDP := LCP
			      else
				TAGFIELDTYPE := TAGSP
			    end;
			  if LCP # nil then
			    begin
			      LBITSIZE :=TAGSP^.BITSIZE;
			      LSIZE := TAGSP^.SIZE;
			      RECSECTION(LCP,TAGSP); %RESERVES SPACE FOR THE TAGFIELD \
			    end;
			  if BITCOUNT > 0 then
			    LSP^.SIZE:= CHECKSIZE(DISPL + 1)
			  else
			    LSP^.SIZE:= CHECKSIZE(DISPL);
			end
		      else
			ERROR(402);
		    INSYMBOL
		  end
	      end
	    else
	      begin
		TAGSP := nil; ERRANDSKIP(209,FSYS or [LPARENT])
	      end;
      1:
	    LSP1 := nil; MINSIZE := DISPL; MAXSIZE := DISPL;
	    MINBITCOUNT:=BITCOUNT;
	    while SY = SEMICOLON do INSYMBOL;
	    loop
	      LSP2 := nil;
	      loop
		CONSTANT(FSYS or [COMMA,COLON,LPARENT],LSP3,LVALU);
		if  not COMPTYPES(TAGSP,LSP3) then
		  ERROR(305);
		NEWZ(LSP3,VARIANT);
		with LSP3^ do
		  begin
		    NXTVAR := LSP1; SUBVAR := LSP2; VARVAL := LVALU;
		    BITSIZE := LSP^.BITSIZE; SELFSTP := nil
		  end;
		LSP1 := LSP3; LSP2 := LSP3;
	      exit if SY # COMMA;
		INSYMBOL;
	    end;
	      if SY = COLON then
		INSYMBOL
	      else
		ERROR(151);
	      if SY = LPARENT then
		INSYMBOL
	      else
		ERROR(153);
	      (* internal files *)
	      FIELDLIST(FSYS or [RPARENT,SEMICOLON],LSP2,LCP,LHASFILE);
	      FHASFILE := FHASFILE or LHASFILE;
	      if DISPL > MAXSIZE then
		MAXSIZE := DISPL;
	      while LSP3 # nil do
		begin
		  LSP4 := LSP3^.SUBVAR; LSP3^.SUBVAR := LSP2;
		  LSP3^.FIRSTFIELD := LCP;
		  LSP3^.SIZE := CHECKSIZE(DISPL);
		  LSP3 := LSP4
		end;
	      if SY = RPARENT then
		begin
		  INSYMBOL;
		  IFERRSKIP(166,FSYS or [SEMICOLON])
		end
	      else
		ERROR(152);
	      while SY = SEMICOLON do INSYMBOL;
	    exit if SY in FSYS;
	      DISPL := MINSIZE;
	      BITCOUNT:=MINBITCOUNT  %RESTAURATION \
	  end;
	    DISPL := MAXSIZE;
	    LSP^.FSTVAR := LSP1
	  end  %IF SY = CASESY\
	else
	  if LSP # nil then
	    if LSP^.FORM = ARRAYS then
	      FRECVAR := LSP
	    else
	      FRECVAR := nil;
	if BITCOUNT > 0 then
	  begin
	    DISPL:=DISPL+1; BITCOUNT := 0
	  end
      end %FIELDLIST\;

    begin %TYP\
      FHASFILE := FALSE;
      SKIPIFERR(TYPEBEGSYS,170,FSYS);
      PACKFLAG := FALSE;
      if SY in TYPEBEGSYS then
	begin
	  if SY in SIMPTYPEBEGSYS then
	    SIMPLETYPE(FSYS,FSP,FSIZE,FBITSIZE)
	  else
	    if SY = ARROW then            %^\
	      begin
		NEWZ(LSP,POINTER); FSP := LSP;
		LBITSIZE := BITS_PER_ADDRESS {Extended addressing};
		with LSP^ do
		  begin
		    SELFSTP := nil;  ELTYPE := nil;
		    SIZE := 1; BITSIZE := LBITSIZE
		  end;
		INSYMBOL;
		if SY = IDENT then
		  begin
		    {All declarations of the form ^THING must be treated as forward references.
		    The problem is that we want to use the local declaration of THING if there
		    is any.  So we have to wait til we have seen all type declarations before
		    we can look up pointer references.}
(* 231 - handle forwards in CREF *)
		    NEWZ(LCP,TYPES);
		    SETNAME(LCP);
		    with LCP^ do
		      begin
			IDTYPE := LSP;
			NEXT := FWPTR; CREF_TEMP := NEXT_CREF_TEMP;
		      end;
		    if CREF then
		      begin
      		      WRITE(CHR(1B),CHR(6),NEXT_CREF_TEMP:6:O);
		      NEXT_CREF_TEMP := NEXT_CREF_TEMP + 1;
		      end;
		    FWPTR := LCP;  INSYMBOL;
		    FBITSIZE := BITS_PER_ADDRESS {Extended addressing}
		  end
		else
		  ERROR(209);
	      end
	    else
	      begin
		if SY = PACKEDSY then
		  begin
		    INSYMBOL;  SKIPIFERR(TYPEDELS,170,FSYS);
		    PACKFLAG := TRUE
		  end;
		%ARRAY\
		if SY = ARRAYSY then
		  begin
		    INSYMBOL;
		    if SY = LBRACK
		    then
		      INSYMBOL
		    else
		      ERROR(154);
		    LSP1 := nil;
		    loop
		      NEWZ(LSP,ARRAYS);
		      with LSP^ do
			begin
			  AELTYPE := LSP1; INXTYPE := nil; SELFSTP := nil;
			  ARRAYPF := PACKFLAG; SIZE := 1
			end;
		      LSP1 := LSP;
		      SIMPLETYPE(FSYS or [COMMA,RBRACK,OFSY],LSP2,LSIZE,LBITSIZE);
		      if LSP2 # nil then
			if LSP2^.FORM <= SUBRANGE then
			  begin
			    if LSP2 = REALPTR then
			      begin
				ERROR(210); LSP2 := nil
			      end
			    else
			      if LSP2 = INTPTR then
				begin
				  ERROR(306); LSP2 := nil
				end;
			    LSP^.INXTYPE := LSP2
			  end
			else
			  begin
			    ERROR(403); LSP2 := nil
			  end;
		    exit if SY # COMMA;
		      INSYMBOL
		  end;
		    if SY = RBRACK then
		      INSYMBOL
		    else
		      ERROR(155);
		    if SY = OFSY then
		      INSYMBOL
		    else
		      ERROR(160);
		    TYP(FSYS,LSP,LSIZE,LBITSIZE);
		    if LSP # nil then
		      if (LSP^.FORM = FILES) or (LSP^.HASFILE) then
			FHASFILE := TRUE;
		    repeat
		      with LSP1^ do
			begin
			  LSP2 := AELTYPE; AELTYPE := LSP;
			  if INXTYPE # nil then
			    begin
			      GETBOUNDS(INXTYPE,LMIN,LMAX);
			      LMIN := CHECKSIZE(LMIN);
			      LMAX := CHECKSIZE(LMAX);
			      I := LMAX - LMIN + 1;
			      if ARRAYPF and (LBITSIZE<=18) then
				begin
				  NEWZ(LBTP,ARRAYY);
				  with LBTP^,BYTE do
				    begin
				      SBITS := LBITSIZE;
				      PBITS := BITMAX; XBIT := 0;
				      IBIT := 0; IREG := BIXREG; RELADDR := 0;
				      LAST := LASTBTP; LASTBTP := LBTP;
				      ARRAYSP := LSP1;
				    end;
				  LSIZE := (I+(BITMAX div LBITSIZE)-1) div (BITMAX div LBITSIZE);
				  if EXTENDED_ADDRESSING then
				    if LSIZE > MAXADDR then
				      ERROR(369);
				end
			      else
				begin
				  LSIZE := LSIZE * I;  ARRAYPF := FALSE
				end;
			      LBITSIZE := BITMAX;  BITSIZE := LBITSIZE;
			      SIZE := CHECKSIZE(LSIZE);
			    end
			end;
		      LSP := LSP1; LSP1 := LSP2
		    until LSP1 = nil
		  end
		else
		  if SY = RECORDSY then               %RECORD\
		    begin
		      INSYMBOL;
		      OLDTOP := TOP;
		      if TOP < DISPLIMIT then
			begin
			  (* save block name for CREF *)
			  TOP := TOP + 1; DISPLAY[TOP].FNAME := nil;
			  DISPLAY[TOP].BLKNAME := '.FIELDID. ';
			  DISPLAY[TOP].OCCUR := CREC
			end
		      else
			ERROR(404);
		      DISPL := 0;  BITCOUNT:=0;
		      (* internal files *)
		      FIELDLIST(FSYS-[SEMICOLON] or [ENDSY],LSP1,LCP,LHASFILE);
		      FHASFILE := FHASFILE or LHASFILE;
		      LBITSIZE := BITMAX;
		      NEWZ(LSP,RECORDS);
		      with LSP^ do
			begin
			  SELFSTP := nil;
			  FSTFLD := %LCP;\ DISPLAY[TOP].FNAME;
			  RECVAR := LSP1;  SIZE := CHECKSIZE(DISPL);
			  BITSIZE := LBITSIZE; RECORDPF := PACKFLAG
			end;
		      TOP := OLDTOP;
		      if SY = ENDSY then
			INSYMBOL
		      else
			ERROR(163)
		    end
		  else
		    if SY = SETSY then                    %SET\
		      begin
			INSYMBOL;
			if SY = OFSY then
			  INSYMBOL
			else
			  ERROR(160);
			SIMPLETYPE(FSYS,LSP1,LSIZE,LBITSIZE);
			if LSP1 # nil then
			  with LSP1^ do
			    case FORM of
			      SCALAR:
				if (LSP1=REALPTR) or (LSP1=INTPTR) then
				  ERROR(352)
				else
				  if SCALKIND =DECLARED then
				    if FCONST^.VALUES.IVAL > BASEMAX then
				      ERROR(352);
			      SUBRANGE:
				if (RANGETYPE = REALPTR)
				  or ((RANGETYPE # CHARPTR)
				      and ((MAX.IVAL > BASEMAX)
					   or (MIN.IVAL < 0))) then
				  ERROR(352);
			      others:
				begin
				  ERROR(353); LSP1 := nil
				end
			    end;
			LBITSIZE := BITMAX;
			if COMPTYPES (LSP1,CHARPTR) then
			  NEWZ(LSP,QPOWER)
			else NEWZ(LSP,POWER);
			with LSP^ do
			  begin
			    if FORM = QPOWER then
			      SIZE := 4
			    else SIZE := 2;
			    SELFSTP := nil; ELSET := LSP1;
			    BITSIZE := LBITSIZE;
			  end
		      end
		    else
		      if SY = FILESY then                     %FILE\
			begin
			  FHASFILE := TRUE;
			  INSYMBOL;
			  if SY = OFSY then
			    INSYMBOL
			  else
			    ERROR(160);
			  TYP(FSYS,LSP1,LSIZE,LBITSIZE);
			  NEWZ(LSP,FILES);
			  LBITSIZE := BITMAX;
			  with LSP^ do
			    begin
			      SELFSTP := nil;  FILTYPE := LSP1;
			      SIZE := CHECKSIZE(LSIZE + SIZEOFFILEBLOCK);
			      FILEPF := PACKFLAG; BITSIZE := LBITSIZE
			    end;
			  if LSP1 # nil then
			    if (LSP1^.FORM = FILES) or (LSP1^.HASFILE) then
			      begin
				ERROR(254); LSP^.FILTYPE := nil
			      end
			end
		      else
			LSP := nil;
		FSP := LSP; FBITSIZE := LBITSIZE
	      end;
	  IFERRSKIP(166,FSYS)
	end
      else
	FSP := nil;
      if FSP = nil then
	begin
	  FSIZE := 1;FBITSIZE := 0
	end
      else
	begin
	  FSIZE := FSP^.SIZE;
	  FSP^.HASFILE := FHASFILE
	end
    end %TYP\;
  procedure LABELDECLARATION;
    var
      LCP: CTP;
    begin
      loop
	if SY = INTCONST then
	  begin
	    NEWZ(LCP,LABELT);
	    SETNAME(LCP);
	    with LCP^ do
	      begin
		SCOPE := LEVEL; 
		IDTYPE := nil;
		NEXT := LASTLABEL; LASTLABEL := LCP;
		GOTOCHAIN := 0; LABELADDRESS := 0
	      end;
	    ENTERID(LCP);
    1:
	    INSYMBOL
	  end
	else
	  ERROR(255);
	IFERRSKIP(166,FSYS or [COMMA,SEMICOLON]);
      exit if SY # COMMA;
	INSYMBOL
    end;
      if SY = SEMICOLON then
	INSYMBOL
      else
	ERROR(156)
    end %LABELDECLARATION\;

  procedure CONSTANTDECLARATION;
    var
      LCP: CTP; LSP: STP; LVALU: VALU;
    begin
      SKIPIFERR([IDENT],209,FSYS);
      while SY = IDENT do
	begin
	  NEWZ(LCP,KONST);
	  SETNAME(LCP);
	  with LCP^ do
	    begin
	      IDTYPE := nil; NEXT := nil
	    end;
	  INSYMBOL;
	  if (SY = RELOP) and (OP = EQOP) then
	    INSYMBOL
	  else
	    ERROR(157);
	  (* REQ FILE SYNTAX *)
	  CONSTANT(FSYS or [SEMICOLON,PERIOD],LSP,LVALU);
	  ENTERID(LCP);
	  LCP^.IDTYPE := LSP; LCP^.VALUES := LVALU;
	  if SY = SEMICOLON then
	    begin
	      INSYMBOL;
	      IFERRSKIP(166,FSYS or [IDENT])
	    end
	      (* REQ FILE SYNTAX *)
	  else
	    if not ((SY=PERIOD) and REQFILE) then
	      ERROR(156)
	end
    end %CONSTANTDECLARATION\;

  procedure TYPEDECLARATION;
    var
      LCP,LCP1,LCP2: CTP; LSP: STP; LSIZE: XADDRRANGE;
      LBITSIZE: BITRANGE;
    begin
      SKIPIFERR([IDENT],209,FSYS);
      while SY = IDENT do
	begin
	  NEWZ(LCP,TYPES);
	  SETNAME(LCP);
	  with LCP^ do
	    begin
	      IDTYPE := nil; NEXT := nil
	    end;
	  INSYMBOL;
	  if (SY = RELOP) and (OP = EQOP) then
	    INSYMBOL
	  else
	    ERROR(157);
	  (* REQ FILE SYNTAX *)
	  TYP(FSYS or [SEMICOLON,PERIOD],LSP,LSIZE,LBITSIZE);
	  ENTERID(LCP);
	  with LCP^ do IDTYPE := LSP;
	  if SY = SEMICOLON then
	    begin
	      INSYMBOL; IFERRSKIP(166,FSYS or [IDENT])
	    end
	      (* REQ FILE SYNTAX *)
	  else
	    if not ((SY=PERIOD) and REQFILE) then
	      ERROR(156)
	end;
    end %TYPEDECLARATION\;

  procedure FWDRESOLVE;
	var I:INTEGER;
    begin
      {For each forward request, look up the variable requested.  If
      you find the request, use it.  Note that all declarations of
      the form ^THING produce forward requests.  This is to force
      THING to be looked up after all type declarations have been
      processed, so we get the local definition if there is one.}
      while FWPTR # nil do
	begin
	  ID := FWPTR^.NAME;
	  IDLEN := FWPTR^.NAMELEN;
	  if IDLEN > ALFALENG then
	    with FWPTR^.NAMEEXT^ do
	      for I := 1 to (IDLEN - 6) div 5 do
		IDEXT.WORDS[I] := WORDS[I];
	  PRTERR := FALSE;   %NO ERROR IF SEARCH NOT SUCCESSFUL\
(* 231 - special CREF entry *)
	  IDSEARCH([TYPES],LCP,FWPTR^.CREF_TEMP); PRTERR := TRUE;
	  if LCP <> nil then
	    begin
	      if LCP^.IDTYPE # nil then
		if LCP^.IDTYPE^.FORM = FILES then
		  ERROR(254)
		else
		  FWPTR^.IDTYPE^.ELTYPE := LCP^.IDTYPE
	    end
	  else
	    ERRORWITHTEXT(405,FWPTR^.NAME);
	  FWPTR := FWPTR^.NEXT
	end
    end %FWDRESOLVE\;

  procedure VARDECLARATION;
    var
      LCP,NXT: CTP; LSP: STP; LSIZE: XADDRRANGE;
      LBITSIZE: BITRANGE; II: INTEGER;
    begin
      NXT := nil;
      repeat
	loop
	  if SY = IDENT then
	    begin
	      NEWZ(LCP,VARS,FALSE);
	      SETNAME(LCP);
	      with LCP^ do
		begin
		  NEXT := NXT;
		  IDTYPE := nil; VKIND := ACTUAL; VLEV := LEVEL
		end;
	      ENTERID(LCP);  NXT := LCP;  INSYMBOL;
	    end
	  else
	    ERROR(209);
	  SKIPIFERR(FSYS or [COMMA,COLON] or TYPEDELS,166,[SEMICOLON]);
	exit if SY # COMMA;
	  INSYMBOL
      end;
	if SY = COLON then
	  INSYMBOL
	else
	  ERROR(151);
	TYP(FSYS or [SEMICOLON] or TYPEDELS,LSP,LSIZE,LBITSIZE);
	(* internal files *)
	if LSP <> nil then
	  if (LSP^.FORM = FILES) or LSP^.HASFILE then
	    FILEINBLOCK[LEVEL] := TRUE;
	while NXT # nil do
	  with  NXT^ do
	    begin
	      IDTYPE := LSP; 
	      if EXTENDED_ADDRESSING and (LSIZE > BIGSIZE) and (LEVEL = 1) then
		begin
		  VADDR := XC; XC := XC + LSIZE;
		end
	      else
		begin
		  VADDR := LC; LC := LC + LSIZE;
		end;
 	      NXT := NEXT
	    end;
	if SY = SEMICOLON then
	  begin
	    INSYMBOL; IFERRSKIP(166,FSYS or [IDENT])
	  end
	else
	  ERROR(156)
      until (SY # IDENT) and  not (SY in TYPEDELS);
    end %VARDECLARATION\;
  procedure PROCDECLARATION(FSY: SYMBOL);
    var
      OLDLEV: 0..MAXLEVEL; LSY: SYMBOL; LCP,LCP1: CTP; LSP: STP;
      FORW: BOOLEAN; OLDTOP: DISPRANGE;
       LLC: XADDRRANGE;  TOPPOFFSET: XADDRRANGE;

    procedure PARAMETERLIST(FSY: SETOFSYS; var FPAR: CTP; var TOPPOFFSET: XADDRRANGE);
      var
	LCP,LCP1,LCP2,LCP3: CTP; LSP: STP; LKIND: IDKIND;
	REGC:INTEGER;
	NEWFORMAT:BOOLEAN;

      procedure conftype;
        var lsp,lsp1,lsp2: stp;	    { lsp1 -> Conformant Type }
        lcp,LTYPE,UTYPE: ctp;
        LMIN,LMAX: INTEGER;
        subcount: integer; {This will be 1 normal array, 2 for second dim,etc}
        lsize: addrrange;
        lbitsize: bitrange;
	ispacked,new1,new2: Boolean; {Indicate whether 1st or 2nd bounds are
				new identifiers}
	lbtp:btp;

      begin
        ISPACKED := FALSE;
	SUBCOUNT := 0;
	LTYPE := NIL; UTYPE := NIL;
		{ *********  The following code stolen from TYP ********** }
	LSP1 := NIL;
	  REPEAT
	    if SY = PACKEDSY then
	      begin
	      ISPACKED := TRUE;
	      INSYMBOL;
	      end;
	    if SY <> ARRAYSY then
	      ERROR(170)
	    else INSYMBOL;
	    IF SY = LBRACK
	      THEN INSYMBOL
	      ELSE ERROR(154);
	    LOOP
	      NEWZ(LSP,ARRAYS);
	      WITH LSP^ DO
		BEGIN
		  AELTYPE := LSP1; INXTYPE := NIL; SELFSTP := NIL;
		  ARRAYPF := FALSE; SIZE := 1; ARRAYCONF := TRUE;
		END;
	      LSP1 := LSP;

{*****}
	    {process a local variable}
	      IF SY = IDENT
		THEN
		  BEGIN
{First see whether we have seen it before.  If so, there is nothing
 to do.}
		    SUBCOUNT := SUBCOUNT + 1;

/* Originally we thought you were allow to repeat the same
   identifier, e.g. ARRAY[I..J,I..J] OF REAL.  It now seems that this
   is not the case.   The commented out code would handle that.
		    SEARCHSECTION(DISPLAY[TOP].FNAME,LCP);
		    if LCP <> NIL
		     then begin
		      NEW1 := FALSE;
		      if not ((LCP^.CLASS = VARS) and LCP^.CONFARG) then
			ERROR(302);
		      end
		     else
*/
		      begin
		      NEW1 := TRUE;
		      NEWZ(LCP,VARS,TRUE);
		      SETNAME(LCP);
		      WITH LCP^ DO
		        BEGIN
			  NEXT := LCP1; LCP1 := LCP;
			  VADDR := LC; LC := LC + 1; REGC := REGC + 1;
			  IDTYPE := NIL; VKIND := ACTUAL; VLEV := LEVEL;
			  SOURCE := SUBCOUNT; ISUPPER := FALSE;
		        END;
		      ENTERID(LCP);
		      end;
		    LTYPE := lcp;
(* save lcp so we can add its type *)
		    INSYMBOL;
		  END
		else error(209);
	      if sy <> colon then error(151) else insymbol;
	    {process another local variable} 
	      IF SY = IDENT
		THEN
		  BEGIN


/*		    SEARCHSECTION(DISPLAY[TOP].FNAME,LCP);
		    if LCP <> NIL
		     then begin
		      NEW2 := FALSE;
		      if not ((LCP^.CLASS = VARS) and LCP^.CONFARG) then
			ERROR(302);
		      end
		     else
*/

		      begin
		      NEW2 := TRUE;
		      NEWZ(LCP,VARS,TRUE);
		      SETNAME(LCP);
		      WITH LCP^ DO
		        BEGIN
			  NEXT := LCP1; LCP1 := LCP;
			  VADDR := LC; LC := LC + 1; REGC := REGC + 1;
			  IDTYPE := NIL; VKIND := ACTUAL; VLEV := LEVEL;
			  SOURCE := SUBCOUNT; ISUPPER := TRUE;
		        END;
		      ENTERID(LCP);
		      end;
		    UTYPE := LCP;
		    INSYMBOL;
		  END
		else error(209);
	      if sy <> colon then error(151) else insymbol;
{*****}
	      if SY = IDENT then
	        begin
	          IDSEARCH([TYPES],LCP,0);
		  INSYMBOL;
		  LSP2 := LCP^.IDTYPE;
	        end
	      else 
		begin 
		  LSP2 := NIL;
		  error(209);
		end;

/*  Previously we allowed a SIMPLETYPE here, but ISO seems to want just an
    identifier
	      SIMPLETYPE(FSYS OR [semicolon,Rbrack,OFSY],LSP2,LSIZE,LBITSIZE);
*/

	      IF LSP2 # NIL
		THEN
		  IF LSP2^.FORM <= SUBRANGE
		    THEN
		      BEGIN
			IF LSP2 = REALPTR
			  THEN
			    BEGIN
			      ERROR(210); LSP2 := NIL
		    	    END
		      END
		    ELSE
		      BEGIN
			ERROR(403); LSP2 := NIL
		      END;
	      newz(lsp,conformant);
	      lsp^.boundtype := lsp2;
	      if LTYPE <> nil then
		begin
		  LTYPE^.idtype := lsp2;	{ set index's idtype }
		  lsp^.lower := LTYPE;
{ and the conformant's bounds }
		  if UTYPE <> nil then
		    begin
		      UTYPE^.idtype := lsp2;   { same for 2nd }
		      lsp^.upper := UTYPE
		    end
		end;
	      lsp2 := lsp;
	      LSP1^.INXTYPE := LSP2;
		{ we want the inxtype to point to our conformant type }
	      EXIT IF SY # semicolon;
	      INSYMBOL
	    END;
	    IF SY = RBRACK
	      THEN INSYMBOL
	      ELSE ERROR(155);
	    IF SY = OFSY
	      THEN INSYMBOL
	      ELSE ERROR(160);
	    if ISPACKED then
	      if SY in [ARRAYSY,PACKEDSY] then
	        ERROR(209);  {ISO says only one level allowed for PACKED}
	   UNTIL not (SY in [ARRAYSY,PACKEDSY]);
	    if SY = IDENT then
	      begin
	        IDSEARCH([TYPES],LCP,0);
		INSYMBOL;
		LSP := LCP^.IDTYPE;
	      end
	    else error(209);
	    LBITSIZE := LSP^.BITSIZE;
	    REPEAT
	      WITH LSP1^ DO
		BEGIN
		  LSP2 := AELTYPE; AELTYPE := LSP;
                  if ISPACKED and (LBITSIZE<=18) then
		    begin
		      ARRAYPF := TRUE;
	              NEWZ(LBTP,ARRAYY);
		      with LBTP^,BYTE do
		        begin
		    	  SBITS := LBITSIZE;
			  PBITS := BITMAX; XBIT := 0;
			  IBIT := 0; IREG := BIXREG; RELADDR := 0;
		    	  LAST := LASTBTP; LASTBTP := LBTP;
		    	  ARRAYSP := LSP1;
			end
	      	    end
	          else ARRAYPF := FALSE;
		  ISPACKED := FALSE; {only the last level gets it}
		  BITSIZE := BITMAX;
		  SIZE := 0 { we cannot know the size now }
		END;
	      LSP := LSP1; LSP1 := LSP2
	    UNTIL LSP1 = NIL;
	    { give the IDs on lcp2 the correct type (lsp) }
	    { code stolen from TYPEID }
	    {LCP2 is reversed at the moment.  Put it forwards so
		 memory alloc is right}
	    LCP3 := nil;
	    while LCP2 # nil do
	      begin
		LCP := LCP2;  LCP2 := LCP2^.NEXT;
		LCP^.NEXT := LCP3;  LCP3 := LCP
	      end;
	    WHILE LCP3 # NIL DO
		WITH LCP3^ DO
		  BEGIN
		    IDTYPE := LSP;
		    VADDR := LC;
		    LC := LC + 1;  { IDs must be formal }
(* 62 - clean up stack offset *)
		    REGC := REGC+1;
		    LCP := LCP3;  LCP3 := LCP3^.NEXT;
(* CONS the new thing on individually instead of APPENDing the whole
 * string, in order to avoid getting I and J reversed in I,J:INTEGER *)
{* Note that we are here reversing the order again.  This is because the
 * whole thing gets reversed below.}
		    LCP^.NEXT := LCP1;  LCP1 := LCP
		  END;
	
	{ ********* }

        end;

      procedure typeid;
        begin
	  (* ALLOW :FILE AS KLUDGEY THING THAT MATCHES ALL FILES *)
	  if SY in [IDENT,FILESY] then
	    begin
	      if SY=IDENT then
		begin
		  (* STRING, POINTER *)
		  IDSEARCH([TYPES,PARAMS],LCP,0);
		  (* PARAMS IS A PREDECLARED IDENTIFIER DESCRIBING
		   A CLASS OF PARAMETERS WITH REDUCED TYPE CHECKING,
		   E.G. STRING OR POINTER *)
		  LSP := LCP^.IDTYPE;
		end
	      else
		LSP:=ANYFILEPTR;
	      if LSP # nil then
		if (LKIND = ACTUAL) and (LSP^.FORM = FILES) then
		  ERROR(355);
	      {LCP2 is reversed at the moment.  Put it forwards so
		 memory alloc is right}
	      LCP3 := nil;
	      while LCP2 # nil do
		begin
		  LCP := LCP2;  LCP2 := LCP2^.NEXT;
		  LCP^.NEXT := LCP3;  LCP3 := LCP
		end;
	      while LCP3 # nil do
		begin
		  with LCP3^ do
		    begin
		      IDTYPE := LSP;  VADDR := LC;
 {POINTER and STRING are passed by a kludgey mechanism.  Since it uses 2 AC's
 we choose to call this thing call by value, with a size of 2.  STRING
 works the same for value and ref anyway.  POINTER doesn't, so we
 use pointerref instead of pointerptr to distinguish. If we call these
 things 2-word quantities passed by value, then mostly the right thing
 happens automatically.   The only other place special code is required
 is in CALLNONSTANDARD where by use a special routine in place of LOAD,
 to do the actually funny passing.}
		      if (LSP = STRINGPTR) or (LSP = POINTERPTR) then
			if (LSP = POINTERPTR) and (VKIND = FORMAL) then
{If it is POINTER called by ref, use a special tag, POINTERREF}
			  begin
			    IDTYPE := POINTERREF;  VKIND := ACTUAL
			  end
{In any case, consider it actual so the size = 2 works}
			else
			  VKIND := ACTUAL;
		      if VKIND = FORMAL then
			LC := LC + 1
		      else
			if IDTYPE # nil then
			  LC := LC + IDTYPE^.SIZE;
		      if IDTYPE = nil then
			REGC := REGC+1
		      else
			if (VKIND = ACTUAL) and (IDTYPE^.SIZE = 2) then
			  REGC := REGC+2
			else
			  REGC := REGC+1
		    end;
		  LCP := LCP3;  LCP3 := LCP3^.NEXT;
(* CONS the new thing on individually instead of APPENDing the whole
 * string, in order to avoid getting I and J reversed in I,J:INTEGER *)
{* Note that we are here reversing the order again.  This is because the
 * whole thing gets reversed below.}
		  LCP^.NEXT := LCP1;  LCP1 := LCP
		end;
	      INSYMBOL
	    end
	  else
	    ERROR(209);
	  IFERRSKIP(256,FSYS or [SEMICOLON,RPARENT])
	end; {typeid}

{argument list of parametic proc's.  This is optional}

procedure paramproc(proctype:symbol;fcp:ctp);  
			{this is inside proceduredeclaration}
  var oldlc:xaddrrange;
      oldlcp1,oldlcp2:ctp;
      oldlev:levrange;
      oldtop:disprange;
      paramcp,returncp:ctp;
      topoffset:Xaddrrange;
      lsp:stp;
      oldregc:acrange;

    begin 
      OLDLC := LC; OLDLCP1 := LCP1; OLDLCP2 := LCP2; OLDREGC := REGC;
      if PROCTYPE = PROCEDURESY then
	LC := 1
      else
	LC := 2;
      OLDLEV := LEVEL; OLDTOP := TOP;
      if LEVEL < MAXLEVEL then
	LEVEL := LEVEL + 1
      else
	ERROR(453);
      if TOP < DISPLIMIT then
	begin
	  TOP := TOP + 1;
	  with DISPLAY[TOP] do
	    begin
	      FNAME := nil; OCCUR := BLCK; BLKNAME := FCP^.NAME;
	    end %WITH DISPLAY[TOP]\
	end
      else
	ERROR(404);
      if PROCTYPE = PROCEDURESY then
	begin
	  PARAMETERLIST([SEMICOLON,RPARENT],PARAMCP,TOPPOFFSET);
	    with FCP^ do
	      begin
(* 225 - get LC right in foward proc's *)
{We put the parameterlit of PFCHAIN, since that isn't doing anything
 else for parametric procedures}
		PFCHAIN := PARAMCP; POFFSET := TOPPOFFSET; PLC := LC
	      end
	end
      else
	begin
	  PARAMETERLIST([SEMICOLON,COLON,RPARENT],PARAMCP,TOPPOFFSET);
	    with FCP^ do
	      begin
(* 225 - get LC right in foward proc's *)
		PFCHAIN := PARAMCP; POFFSET := TOPPOFFSET; PLC := LC
	      end;
	  if SY = COLON then
	    begin
	      INSYMBOL;
	      if SY = IDENT then
		begin
		  if FORW then
		    ERROR(552);
		  IDSEARCH([TYPES],RETURNCP,0);
		  LSP := RETURNCP^.IDTYPE;  FCP^.IDTYPE := LSP;
		  if LSP # nil then
		    if  not (LSP^.FORM in [SCALAR,SUBRANGE,POINTER]) then
		      begin
			ERROR(551); FCP^.IDTYPE := nil
		      end;
		  INSYMBOL
		end
	      else
		ERRANDSKIP(209,FSYS or [SEMICOLON])
	    end
	  else
	    ERROR(455)
	end;
      LEVEL := OLDLEV; TOP := OLDTOP; LC := OLDLC; 
      LCP1 := OLDLCP1; LCP2 := OLDLCP2; REGC := OLDREGC;
  end;

      begin {parameterlist}
	LCP1 := nil; REGC := REGIN+1; 
	SKIPIFERR(FSY or [LPARENT],256,FSYS);
	if SY = LPARENT then
	  begin
	    if FORW then
	      ERROR(553);
	    INSYMBOL;
	    SKIPIFERR([IDENT,VARSY,PROCEDURESY,FUNCTIONSY],256,FSYS or [RPARENT]);
	    while SY in [IDENT,VARSY,PROCEDURESY,FUNCTIONSY] do
	      begin
		if SY = PROCEDURESY then
		  begin
		    (* PROC PARAM.S *)
		    repeat
		      INSYMBOL;
		      if SY = IDENT then
			begin
			  NEWZ(LCP,PROC,DECLARED,FORMAL);
			  SETNAME(LCP);
			  with LCP^ do
			    begin
			      IDTYPE := nil; NEXT := LCP1;
			      PFLEV := LEVEL; PFADDR := LC
			    end;
			  ENTERID(LCP);
			  LCP1 := LCP; 
			  if EXTENDED_ADDRESSING then
			    begin LC := LC + 2; REGC := REGC + 2 end
			    else begin LC := LC + 1; REGC := REGC+1 end;
			  INSYMBOL;
			  if SY = LPARENT then
			    begin
			      PARAMPROC(PROCEDURESY,LCP);
			      if not (SY in [SEMICOLON,RPARENT]) then
				ERROR(156)
			    end
			end
		      else
			ERROR(209);
		      IFERRSKIP(256,FSYS or [COMMA,SEMICOLON,RPARENT])
		    until SY # COMMA
		  end
		else
		  if SY = FUNCTIONSY then
		    begin
		      (* PROC PARAM.S *)
		      LCP2 := nil;
		      NEWFORMAT := FALSE;
		      repeat
			INSYMBOL;
			if SY = IDENT then
			  begin
			    NEWZ(LCP,FUNC,DECLARED,FORMAL);
			    SETNAME(LCP);
			    with LCP^ do
			      begin
				IDTYPE := nil; NEXT := LCP2;
				PFLEV := LEVEL; PFADDR := LC
			      end;
			    ENTERID(LCP);
			    LCP2 := LCP; 
			    if EXTENDED_ADDRESSING then
			      begin LC := LC + 2; REGC := REGC + 2 end
			      else begin LC := LC + 1; REGC := REGC+1 end;
			    INSYMBOL;
			    if SY = LPARENT then
			      begin
				LCP^.NEXT := LCP1;
				LCP1 := LCP;
			        PARAMPROC(FUNCTIONSY,LCP);
			        NEWFORMAT := TRUE;
			        if not (SY in [SEMICOLON,RPARENT]) then
				  ERROR(156)
			      end
			  end;
			if not NEWFORMAT then
			  if not (SY in [COMMA,COLON] or FSYS) then
			    ERRANDSKIP(256,FSYS or [COMMA,SEMICOLON,RPARENT])
		      until SY # COMMA;
		      if NEWFORMAT then
			{nothing}
		      else if SY = COLON then
			begin
			  INSYMBOL;
			  if SY = IDENT then
			    begin
			      IDSEARCH([TYPES],LCP,0);
			      LSP := LCP^.IDTYPE;
			      if LSP # nil then
				if not (LSP^.FORM in [SCALAR,SUBRANGE,POINTER]) then
				  begin
				    ERROR(551); LSP := nil
				  end;
			      LCP3 := LCP2;
			      while LCP2 # nil do
				begin
				  LCP2^.IDTYPE := LSP; LCP := LCP2;
				  LCP2 := LCP2^.NEXT
				end;
			      LCP^.NEXT := LCP1; LCP1 := LCP3;
			      INSYMBOL
			    end
			  else
			    ERROR(209);
			  IFERRSKIP(256,FSYS or [SEMICOLON,RPARENT])
			end
		      else
			ERROR(151)
		    end
		  else
		    begin
		      if SY = VARSY then
			begin
			  LKIND := FORMAL; INSYMBOL
			end
		      else
			LKIND := ACTUAL;
		      LCP2 := nil;
		      loop
			if SY = IDENT then
			  begin
			    NEWZ(LCP,VARS,FALSE);
			    SETNAME(LCP);
			    with LCP^ do
			      begin
				IDTYPE := nil;
				VKIND := LKIND; NEXT := LCP2; VLEV := LEVEL
			      end;
			    ENTERID(LCP);  LCP2 := LCP;
			    INSYMBOL;
			  end
			else
			  ERROR(256);
			if  not (SY in [COMMA,COLON] or FSYS) then
			  ERRANDSKIP(256,FSYS or [COMMA,SEMICOLON,RPARENT])
		      exit if SY # COMMA;
			INSYMBOL
		    end;
		      if SY = COLON then
			begin
(* 240 - conformant arrays *)
			  INSYMBOL;
			  if not (SY in [ARRAYSY,PACKEDSY])
			    then TYPEID
			    else if LKIND = FORMAL
			      then CONFTYPE  {it is conformant}
			      else ERROR(369);
			end
		      else
			ERROR(151)
		    end;
		if SY = SEMICOLON then
		  begin
		    INSYMBOL;
		    SKIPIFERR(FSYS or [IDENT,VARSY,PROCEDURESY,FUNCTIONSY],256,[RPARENT])
		  end
	      end %WHILE\;
	    if SY = RPARENT then
	      begin
		INSYMBOL;
		IFERRSKIP(166,FSY or FSYS)
	      end
	    else
	      ERROR(152);
	    LCP3 := nil;
	    %REVERSE POINTERS\
	    while LCP1 # nil do
	      with LCP1^ do
		begin
		  LCP2 := NEXT; NEXT := LCP3;
		  LCP3 := LCP1; LCP1 := LCP2
		end;
	    FPAR := LCP3
	  end
	else
	  FPAR := nil;
	if (REGC - 1) > PARREGCMAX then
	  TOPPOFFSET := LC - 1
	else
	  TOPPOFFSET := 0;
      end %PARAMETERLIST\;

    begin %PROCDECLARATION\
      LLC := LC;
      if FSY = PROCEDURESY then
	LC := 1
      else
	LC := 2;
      if SY = IDENT then
	begin
	  if CREF then
	    WRITE(CHR(15B),CHR(CREF_BUG_LIMIT),ID:CREF_BUG_LIMIT);
	  SEARCHSECTION(DISPLAY[TOP].FNAME,LCP);   %DECIDE WHETHER FORW.\
	  if LCP # nil then
	    with LCP^ do
	      begin
		if KLASS = PROC then
		  FORW := FORWDECL and (FSY = PROCEDURESY) and (PFKIND = ACTUAL)
		else
		  if KLASS = FUNC then
		    FORW := FORWDECL and (FSY = FUNCTIONSY) and (PFKIND = ACTUAL)
		  else
		    FORW := FALSE;
		if not FORW then
		  ERROR(406)
	      end
	  else
	    FORW := FALSE;
	  if not FORW then
	    begin
	      if FSY = PROCEDURESY then
		NEWZ(LCP,PROC,DECLARED,ACTUAL)
	      else
		NEWZ(LCP,FUNC,DECLARED,ACTUAL);
	      SETNAME(LCP);
	      with LCP^ do
		begin
		  IDTYPE := nil; TESTFWDPTR := nil; NEXT := nil;
		  FORWDECL := FALSE; EXTERNDECL := FALSE; LANGUAGE := PASCALSY;
		  PFLEV := LEVEL; PFADDR := 0;
		  for I := 0 to MAXLEVEL do LINKCHAIN[I] := 0
		end;
	      ENTERID(LCP)
	    end
	  else
(* 225 - Get LC right in forw proc's *)
	    lc := lcp^.plc;
	  INSYMBOL
	end
      else
	begin
	  ERROR(209);
	  if FSY = PROCEDURESY then
	    LCP := UPRCPTR
	  else
	    LCP := UFCTPTR
	end;
      OLDLEV := LEVEL; OLDTOP := TOP;
      if LEVEL < MAXLEVEL then
	LEVEL := LEVEL + 1
      else
	ERROR(453);
      if TOP < DISPLIMIT then
	begin
	  TOP := TOP + 1;
	  with DISPLAY[TOP] do
	    begin
	      FNAME := nil; OCCUR := BLCK; BLKNAME := LCP^.NAME;
	      if DEBUG then
		begin
		  {* This is a dummy entry in the symbol table strictly for the debugger.
		  * The debugger looks at its NEXT field to find the procedure name}
		  NEWZ(LCP1); LCP1^ := ULBLPTR^;
		  LCP1^.NEXT := LCP; ENTERID(LCP1);
		  if FORW and (LCP^.NEXT # nil) then
		    LCP1^.RLINK := LCP^.NEXT
		    (*   LCP^.NEXT is a tree containing
		     *   the parameters.  It needs to be put into the symbol table.  Since
		     *   all legal symbols > blanks, just put it in Rlink.  Previously got
		     *   all symbols twice in debugger! *)
		end
	      else
		if FORW then
		  FNAME := LCP^.NEXT
	    end %WITH DISPLAY[TOP]\
	end
      else
	ERROR(404);
      if FSY = PROCEDURESY then
	begin
	  PARAMETERLIST([SEMICOLON],LCP1,TOPPOFFSET);
	  if not FORW then
	    with LCP^ do
	      begin
(* 225 - get LC right in foward proc's *)
		NEXT := LCP1; POFFSET := TOPPOFFSET; PLC := LC
	      end
	end
      else
	begin
	  PARAMETERLIST([SEMICOLON,COLON],LCP1,TOPPOFFSET);
	  if not FORW then
	    with LCP^ do
	      begin
(* 225 - get LC right in foward proc's *)
		NEXT := LCP1; POFFSET := TOPPOFFSET; PLC := LC
	      end;
	  if SY = COLON then
	    begin
	      INSYMBOL;
	      if SY = IDENT then
		begin
		  if FORW then
		    ERROR(552);
		  IDSEARCH([TYPES],LCP1,0);
		  LSP := LCP1^.IDTYPE;  LCP^.IDTYPE := LSP;
		  if LSP # nil then
		    if  not (LSP^.FORM in [SCALAR,SUBRANGE,POINTER]) then
		      begin
			ERROR(551); LCP^.IDTYPE := nil
		      end;
		  INSYMBOL
		end
	      else
		ERRANDSKIP(209,FSYS or [SEMICOLON])
	    end
	  else
	    if not FORW then
	      ERROR(455)
	end;
      if SY = SEMICOLON then
	INSYMBOL
      else
	ERROR(156);
      if SY = FORWARDSY then
	begin
	  if FORW then
	    ERROR(257)
	  else
	    with LCP^ do
	      begin
		TESTFWDPTR := FORWPTR; FORWPTR := LCP; FORWDECL := TRUE
	      end;
	  INSYMBOL;
	  if SY = SEMICOLON then
	    INSYMBOL
	  else
	    ERROR(156);
	  IFERRSKIP(166,FSYS)
	end % SY = FORWARDSY \
      else
	with LCP^ do
	  begin
	    if SY = EXTERNSY then
	      begin
		if FORW then
		  ERROR(257)
		else
		  EXTERNDECL := TRUE;
		INSYMBOL;
		if LEVEL # 2 then
		  ERROR(464);
		if SY in LANGUAGESYS then
		  begin
		    LANGUAGE := SY;
		    INSYMBOL
		  end;
		if (LIBIX = 0) or (not LIBRARY[LANGUAGE].INORDER) then
		  begin
		    LIBIX:= LIBIX+1;
		    LIBORDER[LIBIX]:= LANGUAGE;
		    LIBRARY[LANGUAGE].INORDER:= TRUE
		  end;
		PFLEV := 1; PFCHAIN := EXTERNPFPTR; EXTERNPFPTR := LCP;
		if SY = SEMICOLON then
		  (* ACCEPT SYNTAX OF REQUIRE FILE *)
		  begin
		    INSYMBOL;
		    IFERRSKIP(166,FSYS)
		  end
		else
		  if not((SY=PERIOD) and REQFILE) then
		    ERROR(166)
	      end % SY = EXTERNSY \
	    else
	      begin
		(* ONLY EXTERN DECL'S ALLOWED IN REQUIRE FILE *)
		if REQFILE then
		  ERROR(169);
		PFCHAIN := LOCALPFPTR; LOCALPFPTR := LCP; FORWDECL := FALSE;
		BLOCK(LCP,FSYS,[BEGINSY,FUNCTIONSY,PROCEDURESY,PERIOD,SEMICOLON]);
		if SY = SEMICOLON then
		  begin
		    INSYMBOL;
		    SKIPIFERR([BEGINSY,PROCEDURESY,FUNCTIONSY],166,FSYS)
		  end
		else
		  if MAIN or (LEVEL > 2) or (SY # PERIOD) then
		    ERROR(156)
	      end % SY # EXTERNSY \
	  end % SY # FORWARDSY \;
      LEVEL := OLDLEV; TOP := OLDTOP; LC := LLC;
      if CREF then
	WRITE(CHR(16B),CHR(CREF_BUG_LIMIT),LCP^.NAME:CREF_BUG_LIMIT)
    end %PROCDECLARATION\;
  procedure BODY(FSYS: SETOFSYS);
    const
      FILEOF =  1B; FILEOL =  2B;  FILTST=40B;
      FILBFH = 26B; FILLNR = 31B;
      FILBCT = 34B; FILBPT = 35B;  FILBIX = 43B;
      FILCMP = 44B; FILJFN =  4B;
(* 232 - tops10 *)
      FILBCT10 = 30B; FILBPT10 = 27B; FILST210 = 37B;
    var
      IDTREE: ADDRRANGE; %POINTER(IN THE USER'S CODE) TO THE IDENTIFIER-TREE\

    procedure FULLWORD(FRELBYTE: RELBYTE; FLEFTH: ADDRRANGE; FRIGHTH: ADDRRANGE);
      begin
	%FULLWORD\
	CIX := CIX + 1;
	if CIX > CIXMAX then
	  begin
	    if FPROCP = nil then
	      ERRORWITHTEXT(356,'MAIN      ')
	    else
	      ERRORWITHTEXT(356, FPROCP^.NAME);
	    CIX := 0
	  end;
	with CODE, HALFWORD[CIX] do
	  begin
	    LEFTHALF := FLEFTH;  RIGHTHALF := FRIGHTH;
	    INFORMATION[CIX] := 'W'; RELOCATION[CIX] := FRELBYTE
	  end;
	IC := IC + 1
      end %FULLWORD\;

      (* routine to allow Polish fixup in case ADDR can't be done by compiler *)
    procedure INSERTPOLISH(PLACE,ORIGINAL:ADDRRANGE;ADJUST:INTEGER);
      var
	POL:POLPT;
	{This routine requests the loader to fix up the right half of PLACE, by
	putting in ORIGINAL (assumed relocatable) + ADJUST (assumed absolute).
	A POLREC is created, and the actual request is put in the file by
	WRITEMC(WRITEPOLISH).}
      begin
	if ABS(ADJUST) > 377777B then
	  ERROR(266)
	else
	  begin
	    NEW(POL);
	    with POL^ do
	      begin
		POLTYPE := ARRAYPOL;
		WHERE := PLACE;   BASE := ORIGINAL;
		OFFSET := ADJUST;
		NEXTPOL := FIRSTPOL  {Link into chain of requests - FIRSTPOL}
	      end;
	    FIRSTPOL := POL
	  end
      end;

    procedure XPOLISH(PLACE:ADDRRANGE;LOFFSET:INTEGER);
      var
	POL:POLPT;
	{This routine requests the loader to set PLACE to the value of
	XADDRSYM, which is the start of the extended addressing section for
	this module, + OFFSET.  This is known as doing relocation the
	hard way, since LINK can't handle extended addressing}
      begin
	NEW(POL);
	with POL^ do
	  begin
	    POLTYPE := XPOL;
	    WHERE := PLACE;   BASE := 0;
	    OFFSET := LOFFSET;
	    NEXTPOL := FIRSTPOL  {Link into chain of requests - FIRSTPOL}
	  end;
	FIRSTPOL := POL
      end;

    procedure INSERTADDR(FRELBYTE: RELBYTE; FCIX:CODERANGE;FIC:ADDRRANGE);
      begin
	if not ERRORFLAG then
	  with CODE do
	    begin
	      INSTRUCTION[FCIX].ADDRESS := FIC;
	      RELOCATION[FCIX] := FRELBYTE
	    end
      end;

    procedure INCREMENTREGC;
      begin
	REGC := REGC + 1;
	if REGC > REGCMAX then
	  begin
	    ERROR(310); REGC := REGIN
	  end
      end;

(* 235 - fairly general changes to DEPCST for 4-word sets.  For space
efficiency, we want to collapse all the QSETxx's to PSET's.  That way
if the first two words of a set using QSET1 happen to be equal to some
other PSET, we use the same contant, despite the fact that it is of
a different type.  This complicates things slightly because it means
that when dealing with a QSET2, we have to compare the 2nd two words
of the ATTR with the value saved in the constant list.*)

    procedure DEPCST(KONSTTYP:CSTCLASS; FATTR:ATTR);
      var
	II:INTEGER; LKSP,LLKSP: KSP; LCSP: CSP;
	NEUEKONSTANTE,GLEICH:BOOLEAN; LCIX: CODERANGE;
	LKONSTTYP:CSTCLASS;
      begin
	I:=1;
{The problem is that all the QSETDx's are just different fixup
 modes of the QSETx's.  We would prefer not to have several copies of the
 same set, just because we have two fixup modes.  Please note that 
 QSETx's are two-word objects, involving just the words that are being
 accessed.  Effectively we just turn them into PSET's, copying the
 right 2 words of the set.  CPSET's are used when we need the whole
 4-word object.}

	if KONSTTYP in [QSET1,QSETD1,QSET2,QSETD2] then
	  LKONSTTYP := PSET
	else LKONSTTYP := KONSTTYP;
	NEUEKONSTANTE:=TRUE; LKSP := FIRSTKONST;
	while (LKSP#nil) and NEUEKONSTANTE do
	  with LKSP^,CONSTPTR^ do
	    begin
	      if CCLASS = LKONSTTYP then
		if KONSTTYP in [QSET2,QSETD2] then
		  if (INTVAL = FATTR.CVAL.VALP^.INTVAL2) and
		     (INTVAL1 = FATTR.CVAL.VALP^.INTVAL3) then
		    NEUEKONSTANTE := FALSE
		  else
		else
		case LKONSTTYP of
		  REEL:
		    if RVAL = FATTR.CVAL.VALP^.RVAL then
		      NEUEKONSTANTE := FALSE;
		  XINT,
		  INT:
		    if INTVAL = FATTR.CVAL.IVAL then
		      NEUEKONSTANTE := FALSE;
		  PSET:
		    if PVAL = FATTR.CVAL.VALP^.PVAL then
		      NEUEKONSTANTE := FALSE;
		  CPSET:
{for bootstrapping, we can't do equality tests on large sets,
 because the representation used by the compiler is different
 from that used by the generated code.}
		    if (INTVAL = FATTR.CVAL.VALP^.INTVAL) and
	               (INTVAL1 = FATTR.CVAL.VALP^.INTVAL1) and
		       (INTVAL2 = FATTR.CVAL.VALP^.INTVAL2) and
		       (INTVAL3 = FATTR.CVAL.VALP^.INTVAL3) then
		      NEUEKONSTANTE := FALSE;
		  STRD,
		  STRG:
		    if FATTR.CVAL.VALP^.SLGTH = SLGTH then
		      begin
			GLEICH := TRUE;
			II := 1;
			repeat
			  if FATTR.CVAL.VALP^.SVAL[II] # SVAL[II] then
			    GLEICH := FALSE;
			  II := II + 1
			until (II > SLGTH) or not GLEICH;
			if GLEICH then
			  NEUEKONSTANTE := FALSE
		      end
		  end %CASE\;
	      LLKSP := LKSP; LKSP := NEXTKONST
	    end %WHILE\;
	if not NEUEKONSTANTE then
	  with LLKSP^ do
	    begin
	      INSERTADDR(RIGHT,CIX,ADDR); CODE.INFORMATION[CIX]:= 'C';
	      if KONSTTYP in [PSET,STRD,QSET1,QSET2] then
		begin
		  INSERTADDR(RIGHT,CIX-1,ADDR1);
		  CODE.INFORMATION[CIX-1]:= 'C'; ADDR1 := IC-2;
		end;
	      ADDR:= IC-1
	    end
	else
	  begin
	    if KONSTTYP = INT then
	      begin
		NEWZ(LCSP,INT); LCSP^.INTVAL := FATTR.CVAL.IVAL
	      end
	    else if KONSTTYP = XINT then
	      begin
		NEWZ(LCSP,XINT); LCSP^.INTVAL := FATTR.CVAL.IVAL
	      end
	    else if KONSTTYP in [QSET1,QSETD1] then
	      begin
	 	NEWZ(LCSP,PSET); LCSP^.PVAL := FATTR.CVAL.VALP^.PVAL
	      end
	    else if KONSTTYP in [QSET2,QSETD2] then
	      begin
		NEWZ(LCSP,PSET); LCSP^.INTVAL := FATTR.CVAL.VALP^.INTVAL2;
		LCSP^.INTVAL1 := FATTR.CVAL.VALP^.INTVAL3
	      end
	    else
	      begin
{[] hack - see SETCONSTRUCTOR}
	        LCSP := FATTR.CVAL.VALP;
		if KONSTTYP = PSET then  {check for null set of wrong kind}
	          if LCSP^.CCLASS = CPSET then
		    LCSP^.CCLASS := PSET
	      end;
	    CODE.INFORMATION[CIX] := 'C';
	    if KONSTTYP in [PSET,STRD,QSET1,QSET2] then
	      CODE.INFORMATION[CIX-1] := 'C';
	    NEWZ(LKSP);
	    with LKSP^ do
	      begin
		ADDR := IC-1;
		(* two fixup chains for 2 word consts *)
		if KONSTTYP in [STRD,PSET,QSET1,QSET2] then
		  ADDR1 := IC-2;
		CONSTPTR := LCSP; NEXTKONST := nil
	      end;
	    if FIRSTKONST = nil then
	      FIRSTKONST := LKSP
	    else
	      LLKSP^.NEXTKONST := LKSP
	  end
      end %DEPCST\;

    
{EFIW procedures an EFIW word for use in another instruction.  At the
 moment we ignore the relocatable bit.  It indicates something that
 should be in the low segment but wouldn't fit.  It should be put into
 a separate multi-section, relocatable PSECT, but LINK doesn't support
 that.  So at the moment we use the absolute address and pray.}

      function EFIW(FRELBYTE : RELBYTE;
		    FINDBIT  : IBRANGE;
		    FINXREG  : ACRANGE;
		    FADDRESS : INTEGER):INTEGER;
      var EFIW_CONVERT: packed record
			  case BOOLEAN of
			       TRUE: (EFIW_WHOLE:INTEGER);
			       FALSE:(LOCAL:0..1B;
				      INDIRECT:IBRANGE;
				      INDEX:ACRANGE;
				      ADDRESS:0..7777777777B)
			       end;
      begin
	with EFIW_CONVERT do
	  begin
	    LOCAL := 0;
	    INDIRECT := FINDBIT;
	    INDEX := FINXREG;
	    ADDRESS := FADDRESS;
	    EFIW := EFIW_WHOLE
	  end
      end;

      procedure MACRO(FRELBYTE : RELBYTE;
		    FINSTR   : INSTRANGE;
		    FAC      : ACRANGE;
		    FINDBIT  : IBRANGE;
		    FINXREG  : ACRANGE;
		    FADDRESS : INTEGER);
      var
	FIX_ADDR: packed record
			   case BOOLEAN of
				TRUE: (FIX_WHOLE: INTEGER);
				FALSE:(FIX_HALVES: HALFS)
			 end;
	LATTR: ATTR;
      begin
	if not INITGLOBALS then
	  begin
	    CIX := CIX + 1;
	    if CIX > CIXMAX then
	      begin
		if FPROCP = nil then
		  ERRORWITHTEXT(356,'MAIN      ')
		else
		  ERRORWITHTEXT(356, FPROCP^.NAME);
		CIX := 0
	      end;
	    with CODE, INSTRUCTION[CIX], FIX_ADDR do
	      begin
		INSTR := FINSTR; AC := FAC;
		INDBIT :=FINDBIT; INXREG := FINXREG;
		FIX_WHOLE := FADDRESS; 

(* 257 - better segment control *)
{First check for the good case, where we know what segment it is supposed
 to be in}
		if FRELBYTE = LOWSEG then
{For lowseg code, all we have to worry about is the fact that LINK might
 get confused and use the wrong relocation value if it doesn't look like
 it is in the lowseg}
		  begin
		    if FADDRESS < PROGRST then
		      begin
			INSERTPOLISH(IC,PROGRST,FADDRESS-PROGRST);
			FIX_WHOLE := 0;
			FADDRESS := 0;
		      end;
		    if FADDRESS >= HIGHSTART then
		      begin
		        INSERTPOLISH(IC,HIGHSTART-1,FADDRESS-(HIGHSTART-1));
			FIX_WHOLE := 0;
			FADDRESS := 0;
		      end;
		    FRELBYTE := RIGHT
		  end
		else if EXTENDED_ADDRESSING then
{The following is fairly simple for absolute addresses.  
   Absolute, not indexed:  Make sure it fits in 18 bits.  Issue a warning
	if not.
   Relocatable, not indexed:  Things < 777777 are normal location, where
	there is no danger.  Things > 777777 are in extended addressing,
	and we have to do funny things to relocate them.
   Absolute, indexed:  ABS(faddress) must fit in 17 bits, or the 18'th
	bit will be taken as sign and there will be odd things.  Use
	indirection in that case.
   Relocatable, indexed:  This one is messy.  Bit 18 should be 0 in this
	case, to avoid negative intepretation.  The problem is that we
	can't tell what the final address is going to be, because of
	relocation.  The only real solution is to make sure that the
	user doesn't start the high seg above 400000.  However if this
	does happen to go over 377777, we are probably better off leaving
	it in "normal" mode, as we haven't implemented relocation in
	the main part.  Of course things above 777777 are in extended
	space, so we use indirection for them.}
	
(* 257 - better segment control *)
		  if (FRELBYTE = XSEG) or
		     (ABS(FADDRESS) > MAXADDR) or
		     (ABS(FADDRESS) > HWCSTMAX) and (FRELBYTE = 0) and
						    (INXREG <> 0) then
		    begin
		      INDBIT := 1; INXREG := 0; ADDRESS := FIX_UP;
		      INFORMATION[CIX] := ' '; RELOCATION[CIX] := 0;
		      IC := IC+1;
(* 263 - removed bogus FRELBYTE := RIGHT inserted in edit 257 *)
		      with LATTR do
			begin
			  TYPTR := INTPTR;
			  KIND := CST;
			  CVAL.IVAL := EFIW(FRELBYTE,FINDBIT,FINXREG,FADDRESS)
			end;
		      if FRELBYTE <> 0
			then DEPCST(XINT,LATTR)
			else
			  begin
			    DEPCST(INT,LATTR);
			    if FINXREG = 0 then
		              writeln(TTY,'%Non-indexed address > 777777: ',
					  IC:6:o,' ',FADDRESS:12:o);
			  end;
		      GOTO 666
		    end
		  else
		else if ABS(FADDRESS) > MAXADDR then
		  writeln(TTY,'%Address > 777777: ',IC:6:o,' ',FADDRESS:12:o);
		ADDRESS  := FIX_HALVES.RIGHTHALF;
		INFORMATION[CIX]:= ' '; RELOCATION[CIX] := FRELBYTE
	      end;
	    IC := IC + 1;
666:	  end
	else
	  ERROR(507)
      end %MACRO\;

    procedure MACRO5(FRELBYTE: RELBYTE; FINSTR : INSTRANGE; FAC,FINXREG : ACRANGE; FADDRESS : INTEGER);
      begin
	MACRO(FRELBYTE,FINSTR,FAC,0,FINXREG,FADDRESS)
      end;

    procedure MACRO4(FINSTR: INSTRANGE;
		     FAC, FINXREG: ACRANGE;
		     FADDRESS: INTEGER);
      begin
	MACRO(NO,FINSTR,FAC,0,FINXREG,FADDRESS)
      end;

    procedure MACRO3(FINSTR : INSTRANGE; FAC:ACRANGE; FADDRESS: INTEGER);
      begin
	MACRO(NO,FINSTR,FAC,0,0,FADDRESS)
      end;

    procedure MACRO4R(FINSTR : INSTRANGE;
		      FAC,FINXREG : ACRANGE;
		      FADDRESS : INTEGER);
      begin
	MACRO(RIGHT,FINSTR,FAC,0,FINXREG,FADDRESS)
      end;

    procedure MACRO3R(FINSTR: INSTRANGE; FAC:ACRANGE; FADDRESS: INTEGER);
      begin
	MACRO(RIGHT,FINSTR,FAC,0,0,FADDRESS)
      end;

    procedure MACRO3C(FINSTR: INSTRANGE; FAC: ACRANGE; IVAL: INTEGER);
      var
	LINSTR: INSTRANGE; LREGC: ACRANGE; LATTR: ATTR;
      begin
	if (IVAL # 400000000000B) then
	  if (ABS(IVAL) <= MAXADDR)
	    and ((FINSTR = OP_MOVE) or (IVAL >= 0)) then
	    begin
	      if FINSTR = OP_MOVE then
		if IVAL < 0 then
		  FINSTR := OP_HRROI
		else
		  FINSTR := OP_MOVEI
	      else
		if (FINSTR >= OP_CAML) and (FINSTR <= OP_CAMG) then
		  FINSTR := FINSTR - 10B %E.G. CAML --> CAIL\
		else
		  FINSTR := FINSTR+1;
	      MACRO3(FINSTR,FAC,IVAL);
	    end
	  else
	    begin
	      MACRO3(FINSTR,FAC,FIX_UP); 
	      LATTR.TYPTR := INTPTR;
	      LATTR.KIND := CST;
	      LATTR.CVAL.IVAL := IVAL;
              DEPCST(INT,LATTR)
	    end
	else
	  begin {IVAL = 400000000000B}
	    MACRO3(FINSTR,FAC,FIX_UP); 
	    LATTR.TYPTR := INTPTR;
	    LATTR.KIND := CST;
	    LATTR.CVAL.IVAL := IVAL;
            DEPCST(INT,LATTR)
	  end
      end;

    procedure PUTPAGER;
      begin
	with PAGER do
	  begin
	    LASTPAGER := IC;
	    with WORD1 do MACRO4R(OP_CAIA,AC,INXREG,ADDRESS);
	    FULLWORD(RIGHT,LHALF,RHALF);
	    LASTPAGE := PAGECNT
	  end
      end;

    procedure SUPPORT(FSUPPORT: SUPPORTS);
      begin
	case FSUPPORT of
	  DEBUGCALL:
	    MACRO3R(OP_XCT,0,RNTS.LINK[FSUPPORT]);
	  BADPOINT, ERRORINASSIGNMENT, INDEXERROR:
	    MACRO3R(OP_JSP,TAC0,RNTS.LINK[FSUPPORT]);
	  INITFILES, INITMEM, STACKOVERFLOW, DEBSTACK:
	    MACRO3R(OP_JSP,TAC1,RNTS.LINK[FSUPPORT]);
	  EXITPROGRAM:
	    MACRO3R(OP_JRST,0,RNTS.LINK[FSUPPORT]);
	  others:
	    MACRO3R(OP_PUSHJ,TOPP,RNTS.LINK[FSUPPORT])
	end;
	CODE.INFORMATION[CIX]:= 'E';
	RNTS.LINK[FSUPPORT]:= IC-1
      end;

    procedure PUTLINER;
      begin
	if PAGECNT # LASTPAGE then
	  PUTPAGER;
	if LINECNT # LASTLINE then
	  begin %BREAKPOINT\
	    if EXTENDED_ADDRESSING then
	      SUPPORT(DEBUGCALL);
	    if LINENR # '-----' then
	      begin
		LINECNT := 0;
		for I := 1 to 5 do
		  LINECNT := 10*LINECNT + ORD(LINENR[I]) - ORD('0')
	      end;
	    LINEDIFF := LINECNT - LASTLINE;
	    if LINEDIFF > 255 then
	      begin
		MACRO3R(OP_SKIPA,0,LASTSTOP);
		LASTSTOP := IC-1;
		MACRO3(OP_JUMP,0,LASTLINE)
	      end
	    else
	      begin
		MACRO4R(OP_JUMP,LINEDIFF mod 16,LINEDIFF div 16, LASTSTOP); %NOOP\
		LASTSTOP := IC - 1
	      end;
	    LASTLINE := LINECNT
	  end
      end;
    procedure ENTERBODY;
      var
	I, LCNT: INTEGER;
	LCP : CTP;
	LBTP: BTP;
	NONLOC, INLEVEL: BOOLEAN;
      begin
	LBTP := LASTBTP;  PFPOINT := IC;
	while LBTP # nil do
	  begin
	    with LBTP^ do
	      begin
	        case BKIND of
		  RECORDD: FIELDCP^.FLDADDR := IC;
		  ARRAYY : ARRAYSP^.ARRAYBPADDR := IC
	        end;
	        if BYTE.XBIT <> 0
		  then IC := IC+1
	      end;
	    LBTP := LBTP^.LAST;
	    IC := IC + 1
	  end;
(* 227 - set NEED_XBLT_OP to ask for it to be put out in MCCODE *)
	if (EXTENDED_ADDRESSING) and (XBLT_LOC = 0) then
	  begin
	    XBLT_LOC := IC;  IC := IC + 1;  NEED_XBLT_OP := TRUE;
	  end;
	LCP:=LASTLABEL;
	INLEVEL:=TRUE; NONLOC:=FALSE;
	while(LCP#nil) and INLEVEL do
	  with LCP^ do
	    if SCOPE=LEVEL then
	      begin
		NONLOC := NONLOC or NONLOCGOTO;
		LCP := NEXT
	      end
	    else
	      INLEVEL := FALSE;
	if FPROCP # nil then
	  begin
{In the case of normal addressing, when you get here, BASIS contains
 the current (old) frame address, in both LH and RH.  The code below
 leaves the LH alone, which becomes the dynamic superior.  It sets
 up a new RH, which becomes the static superior.  For extended
 addressing, the caller copies BASIS to DYNSUP.  This becomes the
 dynamic superior.  Then the same code figures out the static
 superior and leaves it in BASIS.  Note that only the debugger uses
 the dynamic superior info, however at the moment we are always putting
 it in, to avoid various kinds of problems, e.g. core images where
 some modules are /DEBUG and some /NODEBUG.}
	    FULLWORD(NO,0,NIL_VALUE);
	    IDTREE := CIX; %IF DEBUG, INSERT TREEPOINTER HERE\
	    (* SAVE START ADDRESS FOR DDT SYMBOL *)
	    PFDISP := IC;
	    with FPROCP^ do
	      if PFLEV > 1 then
		for I := MAXLEVEL downto PFLEV+1 do
		  if EXTENDED_ADDRESSING then
		    MACRO4(OP_MOVE,BASIS,BASIS,PREV_BASIS)
		  else
		    MACRO4(OP_HRR,BASIS,BASIS,PREV_BASIS);
	    PFSTART := IC;
	    (* clean up stack offset *)
	    if FPROCP^.POFFSET # 0 then
	      MACRO4(OP_POP,TOPP,TOPP,-FPROCP^.POFFSET-1);
	    if EXTENDED_ADDRESSING then
	      begin
		MACRO4(OP_MOVEM,DYNSUP,TOPP,DYNAMIC_SUP-FPROCP^.POFFSET);
		if FPROCP^.PFLEV = 1 then
		  MACRO4(OP_SETZM,0,TOPP,PREV_BASIS-FPROCP^.POFFSET)
		else
		  MACRO4(OP_MOVEM,BASIS,TOPP,PREV_BASIS-FPROCP^.POFFSET);
		MACRO4(OP_XMOVEI,BASIS,TOPP,-FPROCP^.POFFSET);
	      end
	    else
	      begin {NOT EXTENDED ADDRESSING}
		if FPROCP^.PFLEV = 1 then
		  MACRO4(OP_HLLZM,BASIS,TOPP,-FPROCP^.POFFSET-1)
		else
		  MACRO4(OP_MOVEM,BASIS,TOPP,-FPROCP^.POFFSET-1);
		if FPROCP^.POFFSET # 0 then
		  begin
		    MACRO4(OP_XMOVEI,BASIS,TOPP,-FPROCP^.POFFSET);
		    MACRO3(OP_HRL,BASIS,BASIS)
		  end
		else
		  MACRO3(OP_HRLS,BASIS,TOPP)
	      end;
	    if KLCPU and not TOPS10 then
	      MACRO3(OP_ADJSP,TOPP,FIX_UP)
	    else
	      MACRO4(OP_HRRI,TOPP,TOPP,FIX_UP);
	    INSERTSIZE := CIX;
	    INSERTIC := IC-1;
	    if NONLOC then
	      if EXTENDED_ADDRESSING then
		MACRO4(OP_MOVEM,TOPP,BASIS,CURR_TOPP)
	      else
		MACRO4(OP_HRLM,TOPP,BASIS,0);
	    (* If anyone has done a non-local goto into this block, save the
	     stack pointer here where the goto can recover it. *)
	    (* figure out later how many loc's above stack we need *)
	    (* LIMIT CORE ALLOCATION TO TOPS-10 VERSION *)
	    if TOPS10 then
	      if RUNTMCHECK then
		begin
		  MACRO4(OP_MOVEI,TAC0,TOPP,FIX_UP);  CORALLOC := CIX;
		  %will be fixed up - get highest core needed \
		  MACRO4(OP_CAIL,TAC0,BASIS,0); %check wraparound > 777777\
		  MACRO4(OP_CAILE,TAC0,NEWREG,0); %see if need more\
		  SUPPORT(DEBSTACK)
		end
	      else
		begin %NOT RUNTMCHECK\
		  MACRO4(OP_CAIG,NEWREG,TOPP,FIX_UP);  CORALLOC := CIX;
		  %will be fixed up - fails if wrap around 777777\
		  SUPPORT(STACKOVERFLOW)
		end;
	    REGC := REGIN+1;
	    LCP := FPROCP^.NEXT;
	    while LCP # nil do
	      with LCP^ do
		begin
		  (*  proc param.'s*)
		  if KLASS # VARS then
		    begin
		      if EXTENDED_ADDRESSING then
			if REGC < PARREGCMAX then
			  begin
			    MACRO4(OP_DMOVEM,REGC,BASIS,PFADDR);
			    REGC := REGC+2
			  end
			else REGC := PARREGCMAX + 1
		      else if REGC <= PARREGCMAX then
			begin
			  MACRO4(OP_MOVEM,REGC,BASIS,PFADDR);
			  REGC := REGC+1
			end
		    end
		  else
		    if IDTYPE # nil then
		      if (VKIND=FORMAL) or (IDTYPE^.SIZE=1) then
			begin  %COPY PARAMETERS FROM REGISTERS INTO LOCAL CELLS\
			  if REGC <= PARREGCMAX then
			    begin
			      MACRO4(OP_MOVEM,REGC,BASIS,VADDR); REGC := REGC+1
			    end
			end
		      else
			if IDTYPE^.SIZE=2 then
			  begin
			    if REGC < PARREGCMAX
			    then
			      begin
				if KACPU then
				  begin
				    MACRO4(OP_MOVEM,REGC,BASIS,VADDR);
				    MACRO4(OP_MOVEM,REGC+1,BASIS,VADDR+1);
				  end
				else
				  MACRO4(OP_DMOVEM,REGC,BASIS,VADDR);
				REGC:=REGC+2
			      end
			    else
			      REGC:=PARREGCMAX+1
			  end
			else
			  if IDTYPE^.SIZE > 0 then
			    if EXTENDED_ADDRESSING then
			      begin
				MACRO3C(OP_MOVE,TAC0,IDTYPE^.SIZE);
				if REGC <= PARREGCMAX then
				  begin
				    MACRO3(OP_MOVE,TAC1,REGC);  REGC := REGC + 1
				  end
				else
				  MACRO4(OP_MOVE,TAC1,BASIS,VADDR);
				{note - no need to save AC2 here since AC2 is
				the first argument and if this is the first
				arg we are working on we just moved AC2 to TAC1
				above}
				MACRO4(OP_XMOVEI,TAC2,BASIS,VADDR);
				MACRO3R(OP_XTND,TAC0,XBLT_LOC)
			      end
			    else
			      begin {NOT EXTENDED ADDRESSING}
				if REGC <= PARREGCMAX then
				  %copy multiple values into local cells\
				  begin
				    MACRO3(OP_HRL,TAC1,REGC);  REGC := REGC + 1
				  end
				else
				  MACRO4(OP_HRL,TAC1,BASIS,VADDR);
				MACRO4(OP_HRRI,TAC1,BASIS,VADDR);
				MACRO4(OP_BLT,TAC1,BASIS,VADDR+IDTYPE^.SIZE-1)
			      end
			  else
			    {ZERO SIZE} REGC := REGC + 1;
		  LCP := LCP^.NEXT
		end;
	    LCNT := LC - LCPAR; %# local variables\
	    if ZERO and (LCNT > 0) then
	      begin
		MACRO4(OP_SETZM,0,BASIS,LCPAR);
		if LCNT > 4 then
		  if EXTENDED_ADDRESSING then
		    begin
		      MACRO4(OP_XMOVEI,TAC1,BASIS,LCPAR);
		      MACRO4(OP_XMOVEI,TAC2,TAC1,1);
		      MACRO3C(OP_MOVE,TAC0,LCNT-1);
		      MACRO3R(OP_XTND,TAC0,XBLT_LOC)
		    end
		  else
		    begin {NOT EXTENDED ADDRESSING}
		      MACRO4(OP_HRLI,TAC1,BASIS,LCPAR);
		      MACRO4(OP_HRRI,TAC1,BASIS,LCPAR+1);
		      MACRO4(OP_BLT,TAC1,BASIS,LC-1)
		    end
		else
		  {no more than 4 locals}
		  for I := 2 to LCNT do
		    MACRO4(OP_SETZM,0,BASIS,LCPAR+I-1)
	      end
	  end
	else
	  MAINSTART := IC
      end %ENTERBODY\;

    procedure LEAVEBODY;
      var
	J,K : ADDRRANGE;
	LFILEPTR: FTP; LKSP: KSP;
	LCP : CTP; OLDID : ALFA;
      procedure ALFACONSTANT(FSTRING:ALFA);
	var
	  LCSP:CSP;
	begin
	  NEW(LCSP,STRG);
	  with LCSP^ do
	    begin
	      SLGTH := 10;
	      for I := 1 to 10 do SVAL[I] := FSTRING[I]
	    end;
	  with GATTR do
	    begin
	      TYPTR := ALFAPTR; KIND := CST; CVAL.VALP := LCSP
	    end
	end;

(* 264 - %CCLSW is extern *)
      procedure DEPCCLSW(OFFSET:INTEGER);
{arg must be <= to MAXCCLSW }
	begin
	  CODE.HALFWORD[CIX].RIGHTHALF := CCLSW[OFFSET];
	  CCLSW[OFFSET] := IC-1;
          CODE.INFORMATION[CIX] := 'E'
	end;

      begin {LEAVEBODY}
	if DEBUG then
	  PUTLINER;
	if FPROCP # nil then
	  if FILEINBLOCK[LEVEL] then
	    (* internal files - close them *)
	    begin
	      {We have to close any files in this block before we can change TOPP,
	      or we might be playing with locals above the stack!  So this is
	      coded like a non-local goto - new basis in regc, new topp in regc+1}
	      REGC := REGIN+1;
	      if EXTENDED_ADDRESSING then
		begin
		  {simulate MOVE TOPP,BASIS.  But have to subtract 1
		  since there would have been a POPJ TOPP, later.
		  Because of this, things that would be -1(TOPP) are
		  now (TOPP)}
		  MACRO4(OP_XMOVEI,REGC+1,BASIS,-1);
		  {simulate MOVE BASIS,DYNAMIC_SUP(TOPP), but not that
		  -1 has already been done on TOPP, so have to adjust}
		  MACRO4(OP_MOVE,REGC,REGC+1,DYNAMIC_SUP+1);
		  {Now get return address where POPJ would get it,
		  except that adjustment by -1 has been done}
		  MACRO4(OP_MOVE,REGC+2,REGC+1,1)
		end
	      else
		begin {NOT EXTENDED ADDRESSING}
 	          {simulate HRLS TOPP,BASIS.  But have to subtract 1
	          since there would have been a POPJ TOPP, later.
	          Because of this, things that would be -1(TOPP) are
	          now (TOPP)}
	          MACRO4(OP_XMOVEI,REGC+1,BASIS,-1);
		  MACRO3(OP_HRL,REGC+1,REGC+1);
		  {simulate HLRS BASIS,-1(TOPP), but note that -1 has
		  already been done}
		  MACRO4(OP_HLR,REGC,REGC+1,PREV_BASIS+1);
		  MACRO3(OP_HRL,REGC,REGC);
		  {now get return address from where POPJ TOPP, would
		  get it, i.e. (TOPP).  However note that -1 has been
		  done}
		  MACRO4(OP_HRRZ,REGC+2,REGC+1,1)
		end;
	      SUPPORT(EXITGOTO)
	    end
	  else
	    {NOT FILEINBLOCK[LEVEL]}
	    if EXTENDED_ADDRESSING then
	      begin
		MACRO3(OP_MOVE,TOPP,BASIS);
		MACRO4(OP_MOVE,BASIS,TOPP,DYNAMIC_SUP);
		MACRO3(OP_POPJ,TOPP,0)
	      end
	    else
	      begin {NOT EXTENDED ADDRESSING}
		MACRO3(OP_HRLS,TOPP,BASIS);
		MACRO4(OP_HLRS,BASIS,TOPP,PREV_BASIS);
		MACRO3(OP_POPJ,TOPP,0)
	      end
	else
	  begin {FPROCP = NIL}
	    if MAIN then
	      begin
		SUPPORT(EXITPROGRAM);
		STARTADDR := IC;
		(* get some core by default if none there *)
		(* entry code in case execute-only or entry at +1 *)
(* 247 - redo entry code to fit Tops-20 entry vector conventions *)
		if TOPS10 then
		  begin
		    MACRO3R(OP_JRST,1,IC+2); %PORTAL - IN CASE EXEC-ONLY\
		    MACRO3R(OP_JRST,1,IC+2); %IN CASE OFFSET =1\
		    MACRO3(OP_TDZA,TAC1,TAC1); %NORMAL ENTRY - ZERO AND SKIP\
		    MACRO3(OP_MOVEI,TAC1,1); %CCL ENTRY - SET TO ONE\
(* 264 - %CCLSW is external *)
		    MACRO3R(OP_MOVEM,TAC1,0); DEPCCLSW(0); %STORE CCL VALUE\
		    MACRO3R(OP_MOVE,TAC1,0); DEPCCLSW(4); %SEE IF INIT DONE\
		    MACRO3R(OP_JUMPN,TAC1,IC+5); %YES - DON'T RESAVE AC'S\
		    MACRO3R(OP_MOVEM,0B,0); DEPCCLSW(1); %RUNNAME\
		    MACRO3R(OP_MOVEM,7B,0); DEPCCLSW(2); %RUNPPN\
		    MACRO3R(OP_MOVEM,11B,0); DEPCCLSW(3); %RUNDEV\
		    MACRO3R(OP_SETOM,0,0); DEPCCLSW(4); %SAY WE HAVE DONE IT\
		  end
		else
		  begin
{Set up code to set CCLSW to 0 for normal entry, 1 to N for entries
 beyond the version number.  However because of certain old programs
 that use Tops-10 conventions, we must also set it to 1 in case somebody
 starts us directly at .+1}
		    MACRO3(OP_TDZA,TAC0,TAC0); %NORMAL ENTRY - ZERO AND SKIP\
		    MACRO3(OP_MOVEI,TAC0,1); %CCL ENTRY - SET TO ONE\
		    for I := 2 to EVEC_EXTRA do
		      begin
			MACRO3R(OP_JRST,0,IC + 2 + 2*(EVEC_EXTRA - I));
			MACRO3(OP_MOVEI,TAC0,I);
		      end;
(* 264 - %CCLSW is external *)
		    MACRO3R(OP_MOVEM,TAC0,0); DEPCCLSW(0); %STORE CCL VALUE\
		    MACRO3R(OP_MOVE,TAC0,0); DEPCCLSW(4); %SEE IF INIT DONE\
		    MACRO3R(OP_JUMPN,TAC0,IC+5); %YES - DON'T RESAVE AC'S\
		    MACRO3R(OP_MOVEM,1B,0); DEPCCLSW(1); %RUNNAME\
		    MACRO3R(OP_MOVEM,2B,0); DEPCCLSW(2); %RUNPPN\
		    MACRO3R(OP_MOVEM,3B,0); DEPCCLSW(3); %RUNDEV\
		    MACRO3R(OP_SETOM,0,0); DEPCCLSW(4); %SAY WE HAVE DONE IT\
		  end;
		if (HEAP = 0) and (not NOVM) then
		  HEAP := HIGHSTART - 1;
		{Note - even in the extended address version, we setup a
		local stack and heap.  The local heap will be used in
		NEWL and the stack pointer gives the runtimes the offset
		for the bottom of the stack.}
		MACRO3(OP_MOVEI,BASIS,HEAP);
		MACRO3R(OP_MOVEM,BASIS,LSTNEW); %WILL GET GLOBAL FIXUP\
		CODE.INFORMATION[CIX]:= 'E';  LSTNEW := IC-1;
		if TOPS10 then
		  begin
		    MACRO3(OP_MOVEI,BASIS,HEAP+1); %NEWBND_400000\
		    MACRO3R(OP_MOVEM,BASIS,NEWBND); %GLOBAL FIXUP\
		    CODE.INFORMATION[CIX]:= 'E';
		    NEWBND := IC-1
		  end;
		if (STACK = 0) or EXTENDED_ADDRESSING then
		  MACRO3(OP_HRRZ,BASIS,115B)  %BASIS_.JBHRL\
		else
		  MACRO3(OP_MOVEI,BASIS,STACK);
		MACRO3(OP_CAIN,BASIS,0); %IF NO HISEG\
		(* variable start of high seg *)
		MACRO3(OP_MOVEI,BASIS,HIGHSTART - 1); %START STACK 400000\
		MACRO3(OP_MOVE,NEWREG,BASIS); %NEWREG=HIGHEST ALLOC\
		MACRO3(OP_ADDI,BASIS,1); %BASIS=NEXT TO USE\
		MACRO4(OP_XMOVEI,TOPP,BASIS,FIX_UP);
		INSERTSIZE:= CIX; INSERTIC := IC-1;
		if not EXTENDED_ADDRESSING then
		  begin
		    MACRO3(OP_HRL,BASIS,BASIS);
		    MACRO3(OP_HRL,TOPP,TOPP)
		  end;
		if TOPS10 then
		  begin
		    MACRO3(OP_CALLI,0,CALLI_RESET); %.JBFF=.JBSA\
		    SUPPORT(INITMEM);
		    MACRO4(OP_CAI,NEWREG,TOPP,FIX_UP); CORALLOC := CIX;
		    if not NOVM then
		      SUPPORT(STACKOVERFLOW);
		    % GET CORE FOR STACK\
		    if ARITHCHECK then
		      begin
			MACRO3(OP_MOVEI,TAC1,110B); %TRAP ALL ARITH. EXCEPTIONS\
			MACRO3(OP_CALLI,TAC1,CALLI_APRENB); %TURN ON APR SYS\
		      end;
		  end {TOPS10}
		else
		  begin
		    MACRO3(OP_MOVEI,2,ORD(ARITHCHECK));
		    MACRO3(OP_MOVEI,3,STACK)
		  end;
		SUPPORT(INITFILES); {and memory manager on TOPS20}
		MACRO3R(OP_MOVEM,BASIS,GLOBBASIS);
		MACRO3R(OP_MOVEM,TOPP,GLOBTOPP);
		DOINITTTY := FALSE;
		LFILEPTR := SFILEPTR;
		REGC := REGIN + 1;
		LPROGFILE := FPROGFILE;
		while LPROGFILE # nil do
		  begin
		    PRTERR := FALSE;
		    with LPROGFILE^ do
		      begin
			ID := FILID;
			IDLEN := FILIDLEN;
		        if FILIDLEN > 10 then
		          with FILIDEXT^ do
		            for I := 1 to (FILIDLEN - 6) div 5 do
		              IDEXT.WORDS[I] := WORDS[I];
		      end;
		    IDSEARCH([VARS],LCP,0);
		    PRTERR := TRUE; 
		    if LCP = nil then
		      ERRORWITHTEXT(508,LPROGFILE^.FILID)
		    else
		      with LCP^ do
			begin
			  if IDTYPE # nil then
			    if IDTYPE^.FORM # FILES then
			      ERRORWITHTEXT(509,LPROGFILE^.FILID);
			  MACRO3R(OP_XMOVEI,REGC,VADDR);
(* 264 make files always be external *)
(* 274 VLEV is not meaningful for fields *)
			  if KLASS = VARS then
			    if VLEV = 0 then
			      begin
			        VADDR := IC-1;
			        CODE.INFORMATION[CIX] := 'E'
			      end;
			  ALFACONSTANT(LPROGFILE^.FILID);
			  MACRO3(OP_XMOVEI,REGC+1,FIX_UP); DEPCST(STRG,GATTR);
			  (* set up flags for gtjfn *)
			  I := 60023B; %mandatory flags for gtjfn\
			  if LPROGFILE^.WILD then
			    I := I + 100B;
			  if LPROGFILE^.NEWGEN then
			    I := I + 400000B;
			  if LPROGFILE^.OLDFILE then
			    I := I + 100000B;
			  (* put flags in ac4, not in 3 *)
			  MACRO3(OP_HRLZI,REGC+2,I);
			  if LCP = TTYFILE then
			    TTYSEEEOL := LPROGFILE^.SEEEOL;
			  if not ((LCP = TTYFILE) or (LCP = TTYOUTFILE)) then
			    SUPPORT(READFILENAME)
			end;
		    {LCP # NIL}
		    (* handle input and output as special - many changes to lcp = in/outfile *)
		    if (LCP = INFILE)       and (not LPROGFILE^.INTERACT) then
		      DOINITTTY := TRUE;
		    if (LCP = INFILE) or (LCP = OUTFILE) then
		      begin
			MACRO3(OP_SETZ,REGC-1,0);  {AC1=0 for text file}
			MACRO3(OP_SETZB,REGC+1,REGC+2);
			MACRO3(OP_SETZB,REGC+3,REGC+4);
			(* always open INPUT interactive - do GET below *)
			if LCP = INFILE then
			  MACRO3(OP_MOVEI,REGC+3,1);
			MACRO3(OP_SETZB,REGC+5,REGC+6);
			if (LCP = INFILE) and LPROGFILE^.SEEEOL then
			  if TOPS10 then
			    MACRO3(OP_MOVEI,REGC+5,40000B)
			  else
			    MACRO3(OP_MOVEI,REGC+6,20B);
			if LCP = INFILE then
			  SUPPORT(RESETFILE)
			else
			  SUPPORT(REWRITEFILE)
		      end;
		    LPROGFILE := LPROGFILE^.NEXT
		  end;
		TTYINUSE := TTYINUSE or DEBUG;
		while LFILEPTR # nil do
		  with LFILEPTR^, FILEIDENT^ do
		    begin
		      (* only open TTY here, as INPUT and OUTPUT done above *)
		      if (FILEIDENT = TTYFILE) or (FILEIDENT = TTYOUTFILE) then
			begin
			  MACRO3R(OP_XMOVEI,REGC,VADDR);
(* 264 - make files always external *)
			  VADDR := IC-1;
			  CODE.INFORMATION[CIX] := 'E';
			  MACRO3(OP_SETZ,REGC-1,0);  {0=text file}
			  MACRO3(OP_SETZB,REGC+1,REGC+2);
			  MACRO3(OP_SETZB,REGC+3,REGC+4);
			  MACRO3(OP_SETZB,REGC+5,REGC+6);
			  if (FILEIDENT = TTYFILE) and TTYSEEEOL then
			    if TOPS10 then
			      MACRO3(OP_MOVEI,REGC+5,40000B)
			    else
			      MACRO3(OP_MOVEI,REGC+6,20B);
			  if FILEIDENT = TTYFILE then
			    SUPPORT(RESETFILE)
			  else
			    SUPPORT(REWRITEFILE)
			end;
		      LFILEPTR := NEXTFTP
		    end;
		if DOINITTTY then
		  SUPPORT(OPENTTY);
		MACRO3(OP_MOVE,TAC1,74B);  %get .jbddt\
		MACRO3(OP_TRNE,TAC1,777777B);  %if zero RH\
		MACRO3(OP_TLNE,TAC1,777777B);  %or non-0 LH\
		MACRO3R(OP_JRST,0,MAINSTART); %isn't PASDDT\
		if EXTENDED_ADDRESSING then
		  MACRO3(OP_XHLLI,TAC1,TAC1);
		MACRO4(OP_PUSHJ,TOPP,TAC1,-2); %INIT PT. IS START-2\
		MACRO3R(OP_JRST,0,MAINSTART);
		if not TOPS10 then
		  {write the entry vector}
		  begin
		    EVEC_LOC := IC;
		    MACRO3R(OP_JRST,0,STARTADDR);
		    SUPPORT(EXITPROGRAM); {REENTER will exit cleanly}
		    {The following is to simulate WRTWORD(NO,VERSION.WORD)
		    which is not defined here, so we fake it...}
		    MACRO3(0,0,0); {dummy to create the word used for version}
		    CODE.WORD[CIX] := VERSION.WORD; {now put the version there}
		    CODE.INFORMATION[CIX] := 'W';
(* 247 - extra entry vector *)
		    for I := 1 to EVEC_EXTRA do
		      MACRO3R(OP_JRST,0,STARTADDR + 2*I - 1);
		  end
	      end
	  end;
	CODEEND := IC; LKSP:= FIRSTKONST;
	while LKSP # nil do
	  with LKSP^,CONSTPTR^ do
	    begin
	      KADDR:= IC;
	      case  CCLASS of
		XINT: begin  {Ask for this address to get INTVAL + XADDRSYM}
			XPOLISH(IC,INTVAL);
			IC := IC + 1; INTVAL := FIX_UP; CCLASS := INT;
		      end;
		INT,
		REEL: IC := IC + 1;
		PSET: IC := IC + 2;
(* 235 - 4-word sets *)
		CPSET: IC := IC + 4;
		STRD,
		STRG: IC := IC + (SLGTH+4) div 5
	      end;
	      %CASE\
	      LKSP := NEXTKONST
	    end  %WITH , WHILE\;

	  ADJSP_LOC := 0;  {assume we don't need indirection for ADJSP}
	  if MAIN or (LEVEL > 1) then
	    (* allocate core for loc's above stack *)
	    begin
	      if EXTENDED_ADDRESSING then
		begin
		  LCMAX := LCMAX + 2;
		  if LCMAX > MAXXADDR then
		    ERROR(266)
		end
	      else if LCMAX > MAXADDR then
		ERROR(266);
	      if FPROCP # nil then
		ADJSP_VAL := LCMAX - FPROCP^.POFFSET
	      else
		ADJSP_VAL := LCMAX;
	      if ADJSP_VAL <= HWCSTMAX
		then INSERTADDR(NO,INSERTSIZE,ADJSP_VAL)
	      else {It's too big for an immediate - we need a constant}
		begin  {Can't use ADJSP for large offsets}
		INSERTADDR(RIGHT,INSERTSIZE,IC);
		CODE.INSTRUCTION[INSERTSIZE].INSTR := OP_ADD;
		ADJSP_LOC := IC;
		IC := IC + 1;
		end;
	      %BELOW THE STACK\
	      if TOPS10 then
		INSERTADDR(NO,CORALLOC,STKOFFMAX+40B);
	      %ABOVE THE STACK\
	    end;

	if DEBUGSWITCH then
	  begin
	    if (LEVEL > 1) and (DISPLAY[TOP].FNAME # nil) then
	      INSERTADDR(RIGHT,IDTREE,IC)
	  end
	else
	  if LEVEL = 1 then
	    HIGHESTCODE := IC
      end%LEAVEBODY\;

{Here begin the code generation procedures that are used for referencing
Pascal variables and expressions.  They all depend upon ATTR's to keep
track of where the variable or expression is at the moment.  Here is what
they are used for:

FETCHBASIS - this is used to handle uplevel variable references.  In this
	case it loads BASIS of the right block into TAC1 and changes
	the ATTR to reflect the fact that the reference is now indexed
	by TAC1.  This should normally not be used directly by the user,
	as it is called by LOAD, LOADADDRESS, STORE, and MAKECODE.
FASTBASIS - this is used in handling conformant arrays and certain other
	places where there is no ATTR around.  It takes an identifier
	pointer as argument, and loads AC 1 if necessary.  It modifies
	an argument which becomes the index register.  
MAKECODE - this is sort of a generalized version of MACRO, where the
	effective address field is an ATTR rather than a bare address.
	It handles all the gory stuff needed to produce a real
	operation.  Thus a MOVE may really be a LDB if a field is
	involved.  Note that it is set up to handle MOVE in these odd
	cases but not MOVEI or MOVEM.  Instead of MOVEI you should do
	LOADADDRESS, and instead of MOVEM you should do STORE.
   WARNING: In the case of QPOWER, i.e. sets that won't fit in the
	AC's, MAKECODE ignores the AC argument that it is passed.
	Normally the first arg to MAKECODE is assumed to be loaded
	into an AC, but that is not possible for a QPOWER.  Thus in
	this case, the first arg is assumed to be described by
	GATTR.  The results are returned in the ATTR that is is
	passed as the last argument.
LOAD - This makes sure that the current thing is in REGC.  It is
	normally used after EXPRESSION or VARIABLE.  EXPRESSION will
	leave actual expressions in an AC, since it had to compute them.
	In this case it will have done INCREMENTREGC to get the AC.
	In the case of a variable or constant, it will not load them,
	and will not have done INCREMENTREGC unless it needed temps
	to do subscripts, etc.  And even then if ONFIXEDREGC was done,
	REGC will have been reset.  Thus LOAD has to do the INCREMENTREGC
	if the thing is not an expression. LOAD checks the type, and for
	things that are not expressions, does one or two INCREMENTREGC's
	and then moves then into the AC, using MAKECODE. This is
	probably better to use than doing your own MAKECODE(OP_MOVE),
	but it isn't very different.
LOADADDRESS - This loads the address of a thing instead of its contents.
	Since this is obviously meaningless for expressions, it always
	does INCREMENTREGC. It is effectively a MOVEI, except that for
	constants it creates the constant and it adjusts the AC is in
	MAKECODE.
STORE - this is effectively a MOVEM, except that it understands how to store
	into fields of records and all that stuff.
LOADSUBRANGE - this is a special version of LOAD (indeed it calls LOAD)
	that also range checks the thing it is loading if it is a subrange
	type.

Accumulator usage is somewhat odd.  There are a few cases where something
has to be an a particular AC.  This should be handled by LOAD and LOADADDRESS.
Unfortunately these routines do not guarantee to leave their results in
the same AC all the time.  Normally they do INCREMENTREGC and put the results
in the new AC.  But if the ATTR has an INDEXR (or BPADDR?) that is a normal
AC (i.e. not BASIS or a temp), they will put the result in that AC instead.
When compiling code for procedure calls, sometimes we have to make sure that
args will go in a known AC.  At the moment this is handled by using
ONFIXEDREGC as an argument to EXPRESSION or using FIXEDVARIABLE instead of
VARIABLE.  These will put the right things into the right AC's so that
the next LOAD or LOADADDRESS will put the expression in the next AC.  They
do this by knowing the exact way that LOAD and LOADADDRESS work.  This is
lousy design, but I don't have any easy way to fix it at the moment.}

{NB: The code at the start of MAKEQCODE simulates this routine, so if
 you change this, look there}

    procedure FETCHBASIS(var FATTR: ATTR);
      var
	P,Q: INTEGER;
      begin
	with FATTR do
	  if VLEVEL > 1 then
	    begin
	      P := LEVEL - VLEVEL;
	      if P=0 then
		if INDEXR=0 then
		  INDEXR := BASIS
		else
		  MACRO3(OP_ADD,INDEXR,BASIS)
	      else
		begin
		  MACRO4(OP_MOVE,TAC1,BASIS,-1);
		  for Q := P downto 2 do
		    MACRO4(OP_MOVE,TAC1,TAC1,-1);
		  if INDEXR=0 then
		    INDEXR := TAC1
		  else
		    MACRO3(OP_ADD,INDEXR,TAC1)
		end;
	      VLEVEL:=1       %DA IN WITHSTATEMENT DIE MOEGLICHKEIT BESTEHT,
			       DASS ES 2-MAL DURCH FETCHBASIS LAEUFT\
	    end
      end;
      %FETCHBASIS\

(* 240 - conformant *)
{Currently this is designed only to handle procedure arguments}
    procedure FASTBASIS(INXT: STP; var INDEXR: ACRANGE);
      var
	P,Q: INTEGER;
      begin
	INDEXR := 0;
	if INXT <> NIL then
	  begin
	    if INXT^.UPPER^.VLEV > 1 then
	      begin
	        P := LEVEL - INXT^.UPPER^.VLEV;
	        if P=0 then
	          INDEXR := BASIS
	        else
	          begin
		    MACRO4(OP_MOVE,TAC1,BASIS,-1);
		    for Q := P downto 2 do
		      MACRO4(OP_MOVE,TAC1,TAC1,-1);
		    INDEXR := TAC1
	          end;
	      end
	  end
      end; {FASTBASIS}

(* 240 - conformants *)
{LOADSIZE is used to compute the size of a conformant array.  Actually
 it is intended for one-dimensional arrays of simple objects, because
 it only gives the subscribe range, not the physical size}

    procedure LOADSIZE(REG: ACRANGE; FSP: STP);
	var INDEXR: ACRANGE; LMIN, LMAX: XADDRRANGE;
      begin
	if FSP <> NIL then
	  with FSP^ do
	    if INXTYPE <> NIL then
	      with INXTYPE^ do
		if FORM = CONFORMANT then
		  begin
		    FASTBASIS(FSP^.INXTYPE,INDEXR);
		    MACRO4(OP_MOVE,REG,INDEXR,UPPER^.VADDR);
		    MACRO4(OP_SUB,REG,INDEXR,LOWER^.VADDR);
		    MACRO3(OP_ADDI,REG,1);
		  end
		else
		  begin
		  GETBOUNDS(FSP^.INXTYPE,LMIN,LMAX);
		  MACRO3C(OP_MOVE,REG,LMAX-LMIN+1)
		  end
      end;

{PHYSSIZE is used for conformant arrays when we need the physical size.
 This routine can be used for multi-dimensional arrays, etc.}

    procedure PHYSSIZE(REG: ACRANGE; FSP: STP);
	var INDEXR: ACRANGE; RECURSE: BOOLEAN;
      begin
      if FSP <> NIL then
	with FSP^ do
	  if (FORM = ARRAYS) and ARRAYCONF then
	    begin
	      LOADSIZE(REG,FSP);  {get index range}
	      if AELTYPE <> NIL then
	        begin
	          if ARRAYPF and (AELTYPE^.BITSIZE < 18) then
	            begin
		      MACRO3(OP_ADDI,REG,BITMAX div AELTYPE^.BITSIZE - 1);
		      MACRO3(OP_IDIVI,REG,BITMAX div AELTYPE^.BITSIZE);
	            end;
	          with AELTYPE^ do
	            if (FORM = ARRAYS) and ARRAYCONF then
		      begin
	    		INCREMENTREGC;
			PHYSSIZE(REGC,FSP^.AELTYPE);
			MACRO3(OP_IMUL,REG,REGC);
	    		REGC := REGC-1;
		      end
		    else
		      if SIZE > 1 then
		        MACRO3C(OP_IMUL,REG,SIZE)
	        end {AELTYPE <> NIL}
	    end {CONFORMANT}
	  else MACRO3C(OP_MOVE,REG,SIZE)
      end;

    procedure GETPARADDR;
      begin
	FETCHBASIS(GATTR);
	with GATTR do
	  begin
	    INCREMENTREGC;
	    MACRO5(VRELBYTE,OP_MOVE,REGC,INDEXR,DPLMT);
	    INDEXR := REGC; VRELBYTE:= NO;
	    INDBIT := 0; VLEVEL := 1; DPLMT := 0
	  end
      end;

{MAKEQCODE is used for QPOWER's.  These are symmetric, and do not use FAC}

      procedure MAKEQCODE(FINSTR: INSTRANGE; var FATTR: ATTR);
	var
	  OLDREGC: ACRANGE; TEMP_ADDR: XADDRRANGE;
        begin
	  OLDREGC := REGC;
	  TEMP_ADDR := TEMP_LC(4); {Allocate space for result}
	  if (FATTR.TYPTR <> nil) and (GATTR.TYPTR <> nil) then
            begin 
{We want to avoid doing FETCHBASIS before every occurence.  So if
 FETCHBASIS is going to generate code, we essentially do LOADADDRESS
 here and manipulate these things indexed.  Otherwise we simulate
 FETCHBASIS here.  We also have to do LOADADDRESS if there is
 indirection involved, since adding 1 to the displacement will cause
 unexpected results when there is indirection.}
	    if GATTR.KIND = VARBL then
	     if (GATTR.VLEVEL > 1) or (GATTR.INDBIT <> 0) then
	      if (GATTR.VLEVEL <> LEVEL) or (GATTR.INDEXR <> 0) or
		 (GATTR.INDBIT <> 0) then
	      with GATTR do
	        begin
		  INCREMENTREGC;
		  FETCHBASIS(GATTR);
		  MACRO(VRELBYTE,OP_XMOVEI,REGC,INDBIT,INDEXR,DPLMT);
	          DPLMT := 0; INDEXR := REGC; INDBIT:=0; VRELBYTE := NO
	        end
	      else begin GATTR.INDEXR := BASIS; GATTR.VLEVEL := 1 end;
	    if FATTR.KIND = VARBL then
	     if (FATTR.VLEVEL > 1) or (FATTR.INDBIT <> 0) then
	      if (FATTR.VLEVEL <> LEVEL) or (FATTR.INDEXR <> 0) or
		 (FATTR.INDBIT <> 0) then
	      with FATTR do
	        begin
		  INCREMENTREGC;
		  FETCHBASIS(FATTR);
		  MACRO(VRELBYTE,OP_XMOVEI,REGC,INDBIT,INDEXR,DPLMT);
	          DPLMT := 0; INDEXR := REGC; INDBIT:=0; VRELBYTE := NO
	        end
	      else begin FATTR.INDEXR := BASIS; FATTR.VLEVEL := 1 end;
	    INCREMENTREGC;
	    INCREMENTREGC;
	    if FATTR.KIND = CST then
	      if KACPU then
		begin
		  MACRO3(OP_MOVE,REGC,FIX_UP);
		  MACRO3(OP_MOVE,REGC-1,FIX_UP);
		  DEPCST(QSET1,FATTR);
		end
	      else
	        begin
	          MACRO3(OP_DMOVE,REGC-1,FIX_UP);
	          DEPCST(QSETD1,FATTR);
	        end
	    else
	      with FATTR do
		if KACPU then
		  begin
		    MACRO(VRELBYTE,OP_MOVE,REGC-1,INDBIT,INDEXR,DPLMT);
		    MACRO(VRELBYTE,OP_MOVE,REGC,INDBIT,INDEXR,DPLMT+1);
		  end
	        else MACRO(VRELBYTE,OP_DMOVE,REGC-1,INDBIT,INDEXR,DPLMT);
	    if GATTR.KIND = CST then
	      begin
	        MACRO3(FINSTR,REGC,FIX_UP);
	        MACRO3(FINSTR,REGC-1,FIX_UP);
	        DEPCST(QSET1,GATTR);
	      end
	    else
	      with GATTR do
	        begin
	          MACRO(VRELBYTE,FINSTR,REGC-1,INDBIT,INDEXR,DPLMT);
	          MACRO(VRELBYTE,FINSTR,REGC,INDBIT,INDEXR,DPLMT+1);
		end;
	    if KACPU then
	      begin
		MACRO4(OP_MOVEM,REGC-1,BASIS,TEMP_ADDR);
		MACRO4(OP_MOVEM,REGC,BASIS,TEMP_ADDR+1);
	      end
	    else MACRO4(OP_DMOVEM,REGC-1,BASIS,TEMP_ADDR);
	    if FATTR.KIND = CST then
	      if KACPU then
		begin
		  MACRO3(OP_MOVE,REGC,FIX_UP);
		  MACRO3(OP_MOVE,REGC-1,FIX_UP);
		  DEPCST(QSET2,FATTR);
		end
	      else
	        begin
	          MACRO3(OP_DMOVE,REGC-1,FIX_UP);
	          DEPCST(QSETD2,FATTR);
	        end
	    else
	      with FATTR do
		if KACPU then
		  begin
		    MACRO(VRELBYTE,OP_MOVE,REGC-1,INDBIT,INDEXR,DPLMT+2);
		    MACRO(VRELBYTE,OP_MOVE,REGC,INDBIT,INDEXR,DPLMT+3);
		  end
	        else MACRO(VRELBYTE,OP_DMOVE,REGC-1,INDBIT,INDEXR,DPLMT+2);
	    if GATTR.KIND = CST then
	      begin
	        MACRO3(FINSTR,REGC,FIX_UP);
	        MACRO3(FINSTR,REGC-1,FIX_UP);
	        DEPCST(QSET2,GATTR);
	      end
	    else
	      with GATTR do
	        begin
	          MACRO(VRELBYTE,FINSTR,REGC-1,INDBIT,INDEXR,DPLMT+2);
	          MACRO(VRELBYTE,FINSTR,REGC,INDBIT,INDEXR,DPLMT+3);
		end;
	    if KACPU then
	      begin
		MACRO4(OP_MOVEM,REGC-1,BASIS,TEMP_ADDR+2);
		MACRO4(OP_MOVEM,REGC,BASIS,TEMP_ADDR+3);
	      end
	    else MACRO4(OP_DMOVEM,REGC-1,BASIS,TEMP_ADDR+2);
	    with GATTR do
	      begin
	        DPLMT := TEMP_ADDR; INDEXR := BASIS; INDBIT:=0; 
		VLEVEL := 1; EXTERNCTP := NIL; 
		PACKFG := NOTPACK; VRELBYTE := NO; KIND := VARBL;
	      end;
	    REGC := OLDREGC;
{I have no idea where DISP_LC should go}
	    end
        end; %of MAKEQCODE\

      {Warning to future modifiers: At the end of EXPRESSION, there is code
      that second-guesses the register allocation in this procedure.  If you
      change the register allocation here, please look at that code.

   WARNING: In the case of QPOWER, i.e. sets that won't fit in the
	AC's, MAKECODE ignores the AC argument that it is passed.
	Normally the first arg to MAKECODE is assumed to be loaded
	into an AC, but that is not possible for a QPOWER.  Thus in
	this case, the first arg is assumed to be described by
	GATTR.  The results are returned in the ATTR that is is
	passed as the last argument.}

    procedure MAKECODE(FINSTR: INSTRANGE; FAC: ACRANGE; var FATTR: ATTR);
      var
	LINSTR: INSTRANGE; LREGC: ACRANGE;

      begin %MAKECODE\
	with FATTR do
	  if TYPTR # nil then
	   if TYPTR^.FORM=QPOWER then
	     MAKEQCODE(FINSTR,FATTR)
	    else begin
	      case KIND of
		CST:
		  if TYPTR=REALPTR then
		    begin
		      MACRO3(FINSTR,FAC,FIX_UP); DEPCST(REEL,FATTR)
		    end
		  else
		    if TYPTR^.FORM=SCALAR then
		      with CVAL do
			{note - by using HRROI rather than HRREI, the test becomes simple,
			ie: if it fits in a half-word and it is a MOVE or it is >=0 }
			if (IVAL # 400000000000B) then
			  if (ABS(IVAL) <= MAXADDR)
			    and ((FINSTR = OP_MOVE) or (IVAL >= 0)) then
			    begin
			      if FINSTR = OP_MOVE then
				if IVAL < 0 then
				  FINSTR := OP_HRROI
				else
				  FINSTR := OP_MOVEI
			      else
				if (FINSTR >= OP_CAML) and (FINSTR <= OP_CAMG) then
				  FINSTR := FINSTR - 10B %E.G. CAML --> CAIL\
				else
				  FINSTR := FINSTR+1;
			      MACRO3(FINSTR,FAC,IVAL);
			    end
			  else
			    begin
			      MACRO3(FINSTR,FAC,FIX_UP); DEPCST(INT,FATTR)
			    end
			else
			  begin {IVAL = 400000000000B}
			    MACRO3(FINSTR,FAC,FIX_UP); DEPCST(INT,FATTR)
			  end
		    else
		      {TYPTR^.FORM # SCALAR}
		      if TYPTR=NILPTR then
			begin
			  if FINSTR = OP_MOVE then
			    FINSTR := OP_MOVEI
			  else
			    if (FINSTR >= OP_CAML) and (FINSTR <= OP_CAMG) then
			      FINSTR := FINSTR-10B
			    else
			      FINSTR := FINSTR+1;
			  MACRO3(FINSTR,FAC,NIL_VALUE);
			end
		      else
			if TYPTR^.FORM=POWER then
			  begin
			    MACRO3(FINSTR,FAC,FIX_UP);
			    MACRO3(FINSTR,FAC-1,FIX_UP);
			    DEPCST(PSET,FATTR);
			  end
			else
			  if TYPTR^.FORM=ARRAYS then
			    if TYPTR^.SIZE = 1 then
			      begin
				MACRO3(FINSTR,FAC,FIX_UP);
				DEPCST(STRG,FATTR)
			      end
			    else
			      if TYPTR^.SIZE = 2 then
				begin
				  FATTR.CVAL.VALP^.CCLASS := STRD;
				  MACRO3(FINSTR,FAC,FIX_UP);
				  MACRO3(FINSTR,FAC-1,FIX_UP);
				  DEPCST(STRD,FATTR);
				end;

{We may have to load the argument into an AC first.  E.g. consider
MAKECODE(OP_ADD,REGC,FOO).  If FOO is a packed field, we must first
load into an AC and then add it to REGC.  Obviously this AC must be
different than REGC.  There are two cases where we don't need to load
into separate AC and then do the operation:  If the operation is MOVE,
then loading into the AC is all we need.  If it is a full-word variable
then we can do the instruction directly from memory.  LREGC is the
destination of the first step, which is normally the load into the AC,
but in the case of full-word variables is the operation itself.

There is one more hack:  In the case of a MOVE, instead of loading into
the AC the user requested (FAC), if there is an INDEXR, and it is real
(i.e. not BASIS or something like that), we load into that.  Apparently
this is so complex calculations use lower-numbered AC's.  However this
causes trouble for ONFIXEDREGC, which is in fact implemented by having
EXPRESSION make sure that INDEXR is set to the AC where we are going to
finally want the expression - i.e. it second-guesses this code.  (The
same is true for FIXEDVARIABLE.)}

		VARBL:
		  begin
		    FETCHBASIS(FATTR); LREGC := FAC;
		    if (INDEXR>REGIN) and (INDEXR<=REGCMAX)
		      and ((PACKFG#NOTPACK) or (FINSTR = OP_MOVE)) then
		      if (TYPTR^.SIZE = 2) and LOADNOPTR then
			LREGC := INDEXR+1
		      else
			LREGC := INDEXR
		    else
		      if (PACKFG#NOTPACK) and (FINSTR # OP_MOVE) then
			begin
			  INCREMENTREGC; LREGC := REGC
			end;
		    case PACKFG of
		      NOTPACK:
(* 230 - bad () caused DMOVEM for KA *)
			if (TYPTR^.SIZE = 2) and LOADNOPTR then
			  if (not KACPU) and
			      ((FINSTR = OP_MOVE) or (FINSTR = OP_MOVEM)) then
			    if FINSTR = OP_MOVE then
			      MACRO5(VRELBYTE,OP_DMOVE,LREGC-1,INDEXR,DPLMT)
			    else
			      MACRO5(VRELBYTE,OP_DMOVEM,LREGC-1,INDEXR,DPLMT)
			  else
			    if LREGC <> INDEXR then
			      begin
				MACRO5(VRELBYTE,FINSTR,LREGC,INDEXR,DPLMT+1);
				MACRO5(VRELBYTE,FINSTR,LREGC-1,INDEXR,DPLMT)
			      end
			    else
			      begin
				MACRO5(VRELBYTE,FINSTR,LREGC-1,INDEXR,DPLMT);
				MACRO5(VRELBYTE,FINSTR,LREGC,INDEXR,DPLMT+1)
			      end
			else
			  MACRO(VRELBYTE,FINSTR,LREGC,INDBIT,INDEXR,DPLMT);
		      PACKK:
			begin
			  MACRO5(VRELBYTE,OP_XMOVEI,BIXREG,INDEXR,DPLMT);
			  if (BPADDR>REGIN) and (BPADDR<=REGCMAX) then
			    if (INDEXR<=REGIN) or (BPADDR<INDEXR) then
			      LREGC := BPADDR
			    else
			      LREGC := INDEXR;
			  MACRO3R(OP_LDB,LREGC,BPADDR);
			end;
		      HWORDL:  MACRO5(VRELBYTE,OP_HLRZ,LREGC,INDEXR,DPLMT);
		      HWORDR:  MACRO5(VRELBYTE,OP_HRRZ,LREGC,INDEXR,DPLMT)
		      end %CASE\;
		    if (FINSTR#OP_MOVE) and (PACKFG#NOTPACK)
		    then
		      MACRO3(FINSTR,FAC,LREGC)
		    else
		      FAC := LREGC
		  end;
		EXPR:
		  if FINSTR # OP_MOVE then
		    if TYPTR^.SIZE = 2 then
		      begin
			MACRO3(FINSTR,FAC,REG); MACRO3(FINSTR,FAC-1,REG-1)
		      end
		    else
		      MACRO3(FINSTR,FAC,REG)
		end %CASE\;
	      KIND := EXPR; REG := FAC;
 	    end;
      end;

    procedure LOAD(var FATTR: ATTR);
      begin
	with FATTR do
	  if TYPTR#nil then
(* 235 - don't try to load 4-word sets.  Just leave them where they are *)
	    if (KIND#EXPR) and (TYPTR^.FORM <> QPOWER) then
	      begin
		INCREMENTREGC;
		if (TYPTR^.SIZE = 2) and LOADNOPTR then
		  INCREMENTREGC;
		MAKECODE(OP_MOVE,REGC,FATTR); REGC := REG
	      end;
      end;
      %LOAD\

      (* common procedure for improved range check on subranges *)
    procedure LOADSUBRANGE(var GATTR:ATTR;LSP:STP);
      var
	SLATTR:ATTR; SRMIN,SRMAX:INTEGER;
      begin
	GETBOUNDS(LSP,SRMIN,SRMAX);
	if (GATTR.KIND=CST) then
	  if (GATTR.CVAL.IVAL >= SRMIN) and (GATTR.CVAL.IVAL <=SRMAX) then
	    LOAD (GATTR)
	  else
	    ERROR (367)
	else
	  begin
	    if RUNTMCHECK and ((GATTR.KIND#VARBL) or (GATTR.SUBKIND # LSP)) then
	      begin
		LOAD (GATTR);
		with SLATTR do
		  begin
		    TYPTR:=INTPTR;  KIND :=CST;  CVAL.IVAL:=SRMAX
		  end;
		MAKECODE(OP_CAMG,REGC,SLATTR);
		SLATTR.KIND:=CST;
		SLATTR.CVAL.IVAL:=SRMIN;
		MAKECODE(OP_CAMGE,REGC,SLATTR);
		SUPPORT(ERRORINASSIGNMENT)
	      end
	    else
	      LOAD (GATTR)
	  end
      end;

    procedure STORE(FAC: ACRANGE; var FATTR: ATTR);
      var
	LATTR: ATTR;
      begin
	LATTR := FATTR;
	with LATTR do
	  if TYPTR # nil then
	    begin
	      FETCHBASIS(LATTR);
	      case PACKFG of
		NOTPACK:
		  if KACPU then
		    begin
		      if TYPTR^.SIZE = 2 then
			begin
			  MACRO5(VRELBYTE,OP_MOVEM,FAC,INDEXR,DPLMT+1);
			  FAC := FAC-1
			end;
		      MACRO(VRELBYTE,OP_MOVEM,FAC,INDBIT,INDEXR,DPLMT)
		    end
		  else
		    {NOT KACPU}
		    if TYPTR^.SIZE = 2 then
		      begin
			FAC := FAC - 1;
			MACRO(VRELBYTE,OP_DMOVEM,FAC,INDBIT,INDEXR,DPLMT)
		      end
		    else
		      MACRO(VRELBYTE,OP_MOVEM,FAC,INDBIT,INDEXR,DPLMT);
		PACKK:
		  begin
		    MACRO5(VRELBYTE,OP_XMOVEI,BIXREG,INDEXR,DPLMT);
		    MACRO3R(OP_DPB,FAC,BPADDR);
		  end;
		HWORDL:  MACRO5(VRELBYTE,OP_HRLM,FAC,INDEXR,DPLMT);
		HWORDR:  MACRO5(VRELBYTE,OP_HRRM,FAC,INDEXR,DPLMT)
		end  %CASE\;
	    end %WITH\;
      end %STORE\;

      {Warning to future modifiers: At the end of EXPRESSION, there is code that
      second-guesses the register allocation in this procedure.  If you change
      the register allocation here, please look at that code.}
    procedure LOADADDRESS;
      begin
	INCREMENTREGC;
	with GATTR do
	  if TYPTR # nil then
	    begin
	      case KIND of
		CST:
		  if STRING(TYPTR) then
		    begin
		      MACRO3(OP_XMOVEI,REGC,FIX_UP);
		      DEPCST(STRG,GATTR)
		    end
(* 235 - 4-word sets *)
		  else if TYPTR <> NIL then
		   if TYPTR^.FORM = QPOWER then
		     begin
		       MACRO3(OP_XMOVEI,REGC,FIX_UP);
		       DEPCST(CPSET,GATTR)
		     end
		   else ERROR(171)
		  else ERROR(171);
		VARBL:
		  begin
		    if (INDEXR>REGIN)       and  (INDEXR <= REGCMAX) then
		      REGC := INDEXR;
		    FETCHBASIS(GATTR);
		    case PACKFG of
		      NOTPACK: MACRO(VRELBYTE,OP_XMOVEI,REGC,INDBIT,INDEXR,DPLMT);
		      PACKK,HWORDL,HWORDR: ERROR(357)
		      end
		  end {VARBL};
		EXPR:  ERROR(171)
		end {CASE};
	      KIND := VARBL;  DPLMT := 0; INDEXR:=REGC;
	      INDBIT:=0; VRELBYTE := NO
	    end {WITH, IF}
      end %LOADADDRESS\;
    procedure WRITEMC(WRITEFLAG:WRITEFORM);
      const
	MAXSIZE %OF CONSTANT-, STRUCTURE-, AND ID.-RECORD\ = 45 %WORDS\;
      type
	WANDELFORM=(KONSTANTE,PDP10CODE,REALCST,STRCST,
		    SIXBITCST,HALFWD,PDP10BP,RADIX);
	RECORDFORM=(NONE,CONSTNTREC,STRUCTUREREC,IDENTIFREC,DEBUGREC);
	BIGALFA = packed array [1..15] of CHAR;
	X_STP = ADDRRANGE; X_CTP = ADDRRANGE;
	X_STRUCTURE = packed record
{0 B 0..5}		       X_BITSIZE: BITRANGE;
{0 B 6..35}		       X_SIZE: XADDRRANGE;
{1 B 0}			       X_HASFILE: BOOLEAN;
{1 B 1}			       X_NOCODE:  BOOLEAN;
{1 B 2..5}		       case X_FORM: STRUCTFORM of
{1 B 6..16[11]}			    SCALAR:   (SCAL_DUM:0..3777B;
{1 B 17}				       case X_SCALKIND: DECLKIND of
{1 RH}						    DECLARED: (X_FCONST: X_CTP));
{1 B 6..17[12]}			    SUBRANGE: (SUB_DUM:0..7777B;
{1 RH}					       X_RANGETYPE: X_STP;
{2,3}					       X_MIN,X_MAX: VALU);
{1 B 6..17[12]}			    CONFORMANT: (CONF_DUM:0..7777B;
{1 RH}					       X_BOUNDTYPE: X_STP;
{2}					       X_UPPER,X_LOWER: X_CTP);
{1 B 6..17[12]}			    POINTER:  (POIN_DUM:0..7777B;
{1 RH}					       X_ELTYPE: X_STP);
(* 235 *)
{1 B 6..17[12]}			    POWER,QPOWER:  (POW_DUM:0..7777B;
{1 RH}					       X_ELSET: X_STP);
{1 B 6..16[11]}			    ARRAYS:   (ARR_DUM:0..3777B;
{1 B 17}				       X_ARRAYPF: BOOLEAN;
{1 RH}					       X_ARRAYBPADDR: ADDRRANGE;
{2 LH, 2 RH}				       X_AELTYPE,X_INXTYPE: X_STP);
{1 B 6..16[11]}			    RECORDS:  (REC_DUM:0..3777B;
{1 B 17}				       X_RECORDPF: BOOLEAN;
{1 RH}					       X_FSTFLD: X_CTP;
{2 LH}					       X_RECVAR: X_STP);
{1 B 6..16[11]}			    FILES:    (FIL_DUM:0..3777B;
{1 B 17}				       X_FILEPF: BOOLEAN;
{1 RH}					       X_FILTYPE: X_STP);
				    TAGFWITHID,
{1 B 6..17[12]}			    TAGFWITHOUTID: (TAG_DUM:0..7777B;
{1 RH}						    X_FSTVAR: X_STP;
						    case BOOLEAN of
{2 LH}							 TRUE : (X_TAGFP: X_CTP);
{2 LH}							 FALSE  : (X_TAGFTYPE: X_STP));
{1 B 6..16[11]}			    VARIANT:  (VAR_DUM:0..3777B;
{1 B 17}				       X_QXLYPRTWRR: BOOLEAN;
{1 RH, 2 LH}				       X_NXTVAR,X_SUBVAR: X_STP;
{2 RH}					       X_FIRSTFIELD: X_CTP;
{3}					       X_VARVAL: VALU)
			     end;

	X_IDENTIFIER = packed record
{0,1}				X_NAME: ALFA;
{2}				X_NAMELEN: 0..777777B; X_SPARE1: 0..777777B;
{3}				X_LLINK, X_RLINK: X_CTP;
{4}				X_IDTYPE: X_STP;
				X_NEXT: X_CTP;
{5, B 0}			X_NOCODE: BOOLEAN;
{5, B 1:3}			case X_KLASS: IDCLASS of
{6}				     KONST: (X_VALUES: VALU);
{5, B 4}			     VARS:   (X_VKIND: IDKIND;
{5, B 5}				      X_EXTENDED: BOOLEAN;
{5, B 6..17}				      VAR_DUM: 0..7777B;
{5, RH}					      X_VADDR: ADDRRANGE;
{The XADDR field will be created be COPYCTP directly when the thing is
 extended.}
{6}					      X_XADDR: INTEGER);
{5, B 4:5}			     FIELD: (X_PACKF: PACKKIND;
{5, B 6:35}				     X_FLDADDR: XADDRRANGE);
{NB:  This works only because FLDADDR is never relocatable when it is
 extended.  These values are usually absolute offsets.  In that case
 we have room for a 30-bit value.  When they are relocatable, they are
 the address of the byte pointer, which is local.  A relocatable
 global value would be a problem because the bits 0..5 would get
 clobbered by the full-word fixup}
			      end;

      var
{Remove the following comments and the ones at the beginning of the code
 for this procedure to get a version of the compiler that prints out
 the sizes for use in IDRECSIZE and STRRECSIZE}
/*	strfile:file of x_structure;
	idfile:file of x_identifier;  */
	I,J,L  : INTEGER; LLISTCODE: BOOLEAN; CHECKER: CTP;
	LIC  : ADDRRANGE; LFIRSTKONST: KSP; LRELBYTE: RELBYTE;
	STRING: array [1..6] of CHAR; LFILEPTR: FTP; SWITCHFLAG: FLAGRANGE;
	FILBLOCKADR : ADDRRANGE; CODEARRAY: BOOLEAN; LICMOD4: ADDRRANGE;
	LSIZE: 1..MAXSIZE; RUN1: BOOLEAN; SAVELISTCODE: BOOLEAN;
	CSP0: CSP; %INSTEAD OF NIL\
	RELARRAY, RELEMPTY: array [1..MAXSIZE] of RELBYTE;
	WANDLUNG: packed record
			   case WANDELFORM  of
				KONSTANTE:(WKONST :INTEGER;
					   WKONST2:INTEGER);
				PDP10CODE:(WINSTR :PDP10INSTR);
				REALCST  :(WREAL: REAL);
				STRCST   :(WSTRING:CHARWORD);
				SIXBITCST:(WSIXBIT:packed array [1..6] of 0..77B);
				HALFWD   :(WLEFTHALF:ADDRRANGE; WRIGHTHALF : ADDRRANGE);
				PDP10BP  :(WBYTE: BPOINTER);
				RADIX    :(FLAG: FLAGRANGE; SYMBOL: RADIXRANGE)
			 end;
	ICWANDEL: record
		    case INTEGER of
			 1:(ICVAL: INTEGER);
			 2:(ICCSP: CSP);
			 3:(ICCTP: CTP);
			 4:(ICSTP: STP)
		  end;
	RECORDWANDEL: record
			case RECORDFORM of
			     NONE:(WORD:array [1..MAXSIZE] of INTEGER);
			     CONSTNTREC:(CONSTREC: CONSTNT);
			     DEBUGREC:(DEBUGREC: DEBENTRY);
			     STRUCTUREREC:(X_STRUCTREC: X_STRUCTURE);
			     IDENTIFREC:(X_IDENTREC: X_IDENTIFIER)
		      end;

      procedure NEUEZEILE;
	begin
	  if CREF then
	    LICMOD4 := LIC mod 3
	  else
	    LICMOD4 := LIC mod 4;
	  if (LICMOD4 = 0) and LISTCODE and (LIC > 0) then
	    begin
	      NEWLINE;
	      if RELBLOCK.ITEM = 1 then
		begin
		  WRITE(LIC:6:O);
		  if LIC >= PROGRST then
		    WRITE('''')
		  else
		    WRITE(' ')
		end
	      else
		WRITE(' ':7)
	    end
	end %NEUEZEILE\;

      procedure PUTRELCODE;
	var
	  I: INTEGER;
	begin
	  with RELBLOCK do
	    begin
	      if ((COUNT > 1) or (ITEM # 1)) and (COUNT > 0) then
		begin
		  for I:= COUNT+1 to 18 do RELOCATOR[I-1] := NO;
		  for I:= 1 to COUNT+2 do
		    begin
		      OUTPUTREL^:= COMPONENT[I];
		      PUT(OUTPUTREL)
		    end
		end;
	      COUNT := 0
	    end
	end;

      procedure SHOWRELOCATION(FSIDE: RELBYTE; FRELBYTE: RELBYTE);
	begin
	  if (FRELBYTE = FSIDE) or (FRELBYTE = BOTH) then
	    WRITE('''')
	  else
	    WRITE(' ')
	end;

      procedure WRITEBLOCKST(FITEM: ADDRRANGE);
	var
	  WANDLUNG: packed record
			     case BOOLEAN of
				  TRUE: (WKONST: INTEGER);
				  FALSE: (WLEFTHALF: ADDRRANGE;WRIGHTHALF: ADDRRANGE)
			   end;
	begin
	  with RELBLOCK , WANDLUNG do
	    begin
	      if COUNT # 0 then
		PUTRELCODE;
	      ITEM:= FITEM;
	      if ITEM = 1 then
		begin
		  WKONST := LIC;  WLEFTHALF:= 0;
		  CODE[0]:= WKONST;
		  if WRIGHTHALF < PROGRST then
		    RELOCATOR[0] := NO
		  else
		    RELOCATOR[0] := RIGHT;
		  COUNT:= 1
		end
	    end
	end;

      procedure WRTWORD(FRELBYTE: RELBYTE; FWORD: INTEGER);
	var
	  WANDLUNG: packed record
			     case BOOLEAN of
				  TRUE: (WKONST: INTEGER);
				  FALSE: (WLEFTHALF: ADDRRANGE; WRIGHTHALF: ADDRRANGE)
			   end;
	begin
	  with WANDLUNG do
	    begin
	      WKONST := FWORD;
	      with RELBLOCK do
		begin
		  if COUNT = 0 then
		    WRITEBLOCKST(ITEM);
		  CODE[COUNT]:= FWORD;
		  if FRELBYTE in [LEFT,BOTH] then
{note that we depend here upon the fact that NIL_VALUE < PROGRST}
		    if (WLEFTHALF < PROGRST) {or (WLEFTHALF = NIL_VALUE)} then
		      FRELBYTE := FRELBYTE - LEFT;
		  if FRELBYTE in [RIGHT,BOTH] then
		    if (WRIGHTHALF < PROGRST) {or (WRIGHTHALF= NIL_VALUE)} then
		      FRELBYTE := FRELBYTE - RIGHT;
		  RELOCATOR[COUNT]:= FRELBYTE; LRELBYTE := FRELBYTE;
		  COUNT := COUNT+1;
		  if COUNT = 18 then
		    PUTRELCODE
		end;
	      if LLISTCODE then
		begin
		  NEUEZEILE;
		  if LIC > 0 then
		    WRITE(' ':13);
		  if WRITEFLAG > WRITELIBRARY then
		    WRITE(' ':7)
		  else
		    begin
		      WRITE(WLEFTHALF:6:O); SHOWRELOCATION(LEFT,FRELBYTE)
		    end;
		  WRITE(WRIGHTHALF:6:O); SHOWRELOCATION(RIGHT,FRELBYTE);
		  WRITE(' ':3)
		end;
	      if not CODEARRAY then
		LIC := LIC + 1
	    end
	end;

      function RADIX50(FNAME: ALFA): RADIXRANGE;
	var
	  I: INTEGER; OCTALCODE, RADIXVALUE: RADIXRANGE;
	begin
	  RADIXVALUE := 0; I := 1;
	  while (FNAME[I] # ' ') and (I <= 6) do
	    begin
	      if FNAME[I] in DIGITS then
		OCTALCODE:= ORD(FNAME[I])-ORD('0')+1
	      else
		if FNAME[I] in LETTERS then
		  OCTALCODE:= ORD(FNAME[I])-ORD('A')+11
		else
		  case FNAME[I] of
		    '.': OCTALCODE:= 37;
		    '$': OCTALCODE:= 38;
		    '%': OCTALCODE:= 39;
		    others: OCTALCODE:= 37 {use . for _}
		      end;
	      RADIXVALUE:= RADIXVALUE*50B;
	      RADIXVALUE:= RADIXVALUE + OCTALCODE;
	      I := I + 1
	    end;
	  RADIX50:= RADIXVALUE
	end;

      procedure WRITEPAIR(FRELBYTE: RELBYTE; FADDR1, FADDR2: ADDRRANGE);
	begin
	  with WANDLUNG do
	    begin
	      WLEFTHALF:= FADDR1;  WRIGHTHALF:= FADDR2;
	      WRTWORD(FRELBYTE,WKONST)
	    end
	end;

      procedure WRITEIDENTIFIER(FFLAG: FLAGRANGE; FSYMBOL: ALFA);
	begin
	  LLISTCODE := FALSE;
	  with WANDLUNG do
	    begin
	      if LISTCODE and (WRITEFLAG > WRITEHISEG) then
		begin
		  if ((not CREF) and (LIC mod 4 = 0) or
		      CREF and (LIC mod 3 = 0)) and (LIC > 0) then
		    begin
		      NEWLINE;
		      WRITE(' ':7)
		    end;
		  if LIC > 0 then
		    WRITE(' ':13);
		  WRITE(FSYMBOL:6,' ':11)
		end;
	      if LISTCODE and CREF then
		LIC := LIC+1;
	      if FFLAG # 6B then
		begin
		  FLAG:= FFLAG; SYMBOL:= RADIX50(FSYMBOL)
		end;
	      WRTWORD(NO,WKONST); LLISTCODE := LISTCODE
	    end
	end;

      procedure WRITEFIRSTLINE;
	begin
	  if LISTCODE then
	    begin
	      NEWLINE;
	      if CREF then
		LICMOD4 := LIC mod 3
	      else
		LICMOD4 := LIC mod 4;
	      if LICMOD4 > 0 then
		begin
		  WRITE(LIC-LICMOD4:6:O);
		  if LIC >= PROGRST then
		    WRITE('''')
		  else
		    WRITE(' ');
		  WRITE(' ':LICMOD4*30);
		  if (WRITEFLAG = WRITECODE) and CODEARRAY then
		    WRITE(' ':2)
		end
	    end
	end;

      procedure WRITEHEADER(FTEXT: BIGALFA);
	begin
	  LIC := 0;
	  if LISTCODE then
	    begin
	      NEWLINE; WRITE(FTEXT:15,':',' ':4)
	    end
	end;
      procedure MCGLOBALS;
	begin %MCGLOBALS\
	  if LISTCODE and (FGLOBPTR # nil) then
	    WRITEBUFFER;
	  while FGLOBPTR # nil do
	    with FGLOBPTR^ do
	      begin
		LIC := FIRSTGLOB; WRITEFIRSTLINE;
		J := FCIX;  WRITEBLOCKST(1);
		for I := FIRSTGLOB to LASTGLOB do
		  begin
		    WANDLUNG.WINSTR := CODE.INSTRUCTION[J]; J := J + 1;
		    WRTWORD(NO,WANDLUNG.WKONST)
		  end;
		FGLOBPTR := NEXTGLOBPTR
	      end
	end %MCGLOBALS\;

      procedure MCCODE;

	procedure WRITERECORD;
	  begin
	    for I := 1 to LSIZE do
	      WRTWORD(RELARRAY[I], RECORDWANDEL.WORD[I])
	  end;

	function CONSTRECSIZE(FCSP: CSP): INTEGER;
	  begin
	    with FCSP^ do
	      case CCLASS of
		INT,PSET: CONSTRECSIZE := 5;
(* 236 - 4-word sets *)
		CPSET: CONSTRECSIZE := 7;
		REEL      : CONSTRECSIZE := 4;
		STRD,STRG:CONSTRECSIZE := 4 + (SLGTH+4) div 5
		end
	  end;

	procedure COPYCSP(FCSP:CSP);
	  begin
	    if FCSP # nil then
	      with FCSP^ do
		if RUN1 then
		  begin
		    if SELFCSP = CSP0%NIL\ then
		      with ICWANDEL do
			begin
			  ICVAL := IC; SELFCSP := ICCSP;
			  NOCODE := TRUE;  IC := IC + CONSTRECSIZE(FCSP)
			end
		  end
		else
		  if NOCODE then
		    begin
		      RECORDWANDEL.CONSTREC := FCSP^;
		      LSIZE := CONSTRECSIZE(FCSP);
		      RELARRAY := RELEMPTY;
		      WRITERECORD; NOCODE := FALSE
		    end
	  end %COPYCSP\;

	procedure COPYSTP(FSP:STP);
	  forward;

	procedure COPYCTP(FCP:CTP);
		var i:integer;
	  begin
	    if FCP # nil then
	      with FCP^ do
		if RUN1 and (SELFCTP=nil) or not RUN1 and NOCODE then
		  begin
		    if RUN1 then
		      with ICWANDEL do
			begin
{space for extended name}
			  if NAMELEN > ALFALENG then
			    IC := IC + (NAMELEN - 6) div 5;
			  ICVAL := IC;
			  SELFCTP := ICCTP; NOCODE := TRUE;
			  IC := IC + IDRECSIZE[KLASS];
			  if KLASS = VARS
			    then if VADDR > MAXADDR
			      then IC := IC + 1
			end %WITH\
		    else
		      %NOW RUN 2\
		      with RECORDWANDEL.X_IDENTREC, ICWANDEL do
			begin
{first do the extended name if any}
			  if NAMELEN > ALFALENG then
			    with NAMEEXT^ do
			      for I := 1 to (NAMELEN - 6) div 5 do
			        WRTWORD(NO,WORDS[I]);
			  RELARRAY := RELEMPTY;  X_NAME := NAME;
			  X_NAMELEN := NAMELEN;
			  if LLINK = nil then
			    X_LLINK := NIL_VALUE
			  else
			    begin
			      ICCTP := LLINK^.SELFCTP;
			      X_LLINK := ICVAL;
			      RELARRAY[4] := 1
			    end;
			  if RLINK = nil then
			    X_RLINK := NIL_VALUE
			  else
			    begin
			      ICCTP := RLINK^.SELFCTP;
			      X_RLINK := ICVAL;
			      RELARRAY[4] := RELARRAY[4] + 2
			    end;
			  if NEXT = nil then
			    X_NEXT := NIL_VALUE
			  else
			    begin
			      ICCTP := NEXT^.SELFCTP;
			      X_NEXT := ICVAL;
			      RELARRAY[5] := 1B
			    end;
			  X_KLASS := KLASS;
			  X_VALUES := VALUES; {May be changed below...}
			  if IDTYPE = nil then
			    X_IDTYPE := NIL_VALUE
			  else
			    begin
			      ICSTP := IDTYPE^.SELFSTP;
			      X_IDTYPE := ICVAL;
			      RELARRAY[5] := RELARRAY[5] + 2;

			      case KLASS of
				KONST:
				  if IDTYPE^.FORM > POINTER then
				    begin
				      X_VALUES.VALP := VALUES.VALP^.SELFCSP;
				      RELARRAY[7] := 1B
				    end
				  else
				    if IDTYPE = REALPTR then
				      begin
					WANDLUNG.WREAL := VALUES.VALP^.RVAL;
					X_VALUES.IVAL := WANDLUNG.WKONST
				      end;
				VARS:
				  begin
				    if VLEV < 2 then {relocatable}
				      if VADDR > MAXADDR then
					begin
					  X_EXTENDED := TRUE;
					  ICCTP := SELFCTP;
					  XPOLISH(ICVAL+6,VADDR);
					  X_XADDR := FIX_UP
					end
				      else
					begin
					 X_EXTENDED := FALSE;
					 X_VADDR := VADDR;
					 RELARRAY[6] := 1;
				        end
				    else
				      if VADDR > MAXADDR then
					begin
					  X_EXTENDED := TRUE;
					  X_XADDR := VADDR
					end
				      else
					begin
					  X_EXTENDED := FALSE;
					  X_VADDR := VADDR
					end;
				    X_VKIND := VKIND;
				  end;
				FIELD:
				  begin
				    if PACKF = PACKK then
				      RELARRAY[6] := 1;
				    X_PACKF := PACKF;
				    X_FLDADDR := FLDADDR
				  end;
				end
			    end;
			  LSIZE := IDRECSIZE[KLASS];
			  if KLASS = VARS
			    then if VADDR > MAXADDR
			      then LSIZE := LSIZE + 1;
			  WRITERECORD;
			  NOCODE := FALSE
			end %WITH RECORDWANDEL\;

		    COPYCTP(LLINK);
		    COPYCTP(RLINK);
		    COPYSTP(IDTYPE);
		    {The following is somewhat of a kludge. We don't want to do COPYCTP
		    on the NEXT field of a procedure.  If we did, the following could
		    happen:
		    procedure foo(x:integer); forward;
		    ...
		    foo(1);
		    ...
		    procedure foo;
		    var i,j;
		    When the final declaration of FOO is supplied, the symbol table is
		    initialized from symboltable(FOO)^.NEXT, which contains the parameters,
		    as supplied in the forward decl.  Then I and J are added to the symbol
		    table.  The result is that X points to I and J in the symbol table
		    tree.  This is all fine.  The problem comes when the identifier
		    record for FOO is put into the .REL file before the final declaration.
		    If COPYCTP traces the NEXT field, then the identifier records for all
		    the parameters are also put out.  Since a given identifier is put out
		    only once, this means that X is put into the .REL file before pointers
		    to I and J are added to it.  The effect is that the debugger can't
		    see I and J.
		    It turns out that the  debugger never uses the NEXT field of a
		    procedure entry.  Thus it is not crucial to have a correctly mapped
		    value when the identifier record for the procedure is put out.
		    If we don't call COPYCTP on NEXT, then the NEXT field put into the
		    .REL file will be junk, but at least records for the parameters won't
		    be put out prematurely.  They will get put out eventually even without
		    tracing NEXT, since they will show up in the symbol table for the
		    procedure when it is finally declared.  That should suffice.}

		    if not (KLASS in [PROC,FUNC]) then
		      COPYCTP(NEXT);
		    if (KLASS = KONST)  and (IDTYPE # nil) then
		      if IDTYPE^.FORM > POINTER then
			COPYCSP(VALUES.VALP)
		  end %WITH FCP^\
	  end %COPYCTP\;
	procedure COPYSTP;
	  begin
	    if FSP # nil then
	      with FSP^ do
		if RUN1 and (SELFSTP = nil)     or  not RUN1 and NOCODE then
		  begin
		    if RUN1 then
		      with ICWANDEL do
			begin
			  NOCODE:=TRUE;
			  ICVAL := IC; SELFSTP := ICSTP;
			  IC := IC + STRECSIZE[FORM]
			end
		    else
		      %NOW RUN 2\
		      if NOCODE then
			with RECORDWANDEL.X_STRUCTREC, ICWANDEL do
			  begin
{It turns out that all the various types have pointers at 1[RH], so
 we set it here}
			    RELARRAY := RELEMPTY; RELARRAY[2] := 1;
			    X_SIZE := SIZE;  X_NOCODE := NOCODE;
			    X_BITSIZE := BITSIZE;  X_HASFILE := HASFILE;
			    X_FORM := FORM;

			    case FORM of
			      SCALAR:
				begin
				  X_SCALKIND := SCALKIND;
				  if SCALKIND = DECLARED then
				    if FCONST = nil then
				      X_FCONST := NIL_VALUE
				    else
				      begin
					ICCTP := FCONST^.SELFCTP;
					X_FCONST := ICVAL
				      end
				end;
			      SUBRANGE:
				begin
				  ICSTP := RANGETYPE^.SELFSTP;
				  X_RANGETYPE := ICVAL;
				  X_MIN := MIN;  X_MAX := MAX;
				end;
			      CONFORMANT:
			        begin
				  if BOUNDTYPE <> NIL then
				    begin
				      ICSTP := BOUNDTYPE^.SELFSTP;
				      X_BOUNDTYPE := ICVAL
				    end;
				  if UPPER <> NIL then
				    begin
				      ICCTP := UPPER^.SELFCTP;
				      X_UPPER := ICVAL;
				    end;
				  if LOWER <> NIL then
				    begin
				      ICCTP := LOWER^.SELFCTP;
				      X_LOWER := ICVAL;
				    end;
				  RELARRAY[3] := 3;
				end;
			      POINTER:
				begin
				  if ELTYPE = nil then
				    X_ELTYPE := NIL_VALUE
				  else
				    begin
				      ICSTP := ELTYPE^.SELFSTP;
				      X_ELTYPE := ICVAL
				    end
				end;
(* 235 *)
			      POWER,QPOWER:  begin
				ICSTP := ELSET^.SELFSTP;
				X_ELSET := ICVAL;
			      end;
			      ARRAYS:
				begin
				  X_ARRAYPF := ARRAYPF;
				  X_ARRAYBPADDR := ARRAYBPADDR;
				  if AELTYPE = nil then
				    X_AELTYPE := NIL_VALUE
				  else
				    begin
				      ICSTP := AELTYPE^.SELFSTP;
				      X_AELTYPE := ICVAL
				    end;
				  if INXTYPE = nil then
				    X_INXTYPE := NIL_VALUE
				  else
				    begin
				      ICSTP := INXTYPE^.SELFSTP;
				      X_INXTYPE := ICVAL
				    end;
				  RELARRAY[3] := 3;
				end;
			      RECORDS:
				begin
				  X_RECORDPF := RECORDPF;
				  if FSTFLD = nil then
				    X_FSTFLD := NIL_VALUE
				  else
				    begin
				      ICCTP := FSTFLD^.SELFCTP;
				      X_FSTFLD := ICVAL
				    end;
				  if RECVAR = nil then
				    X_RECVAR := NIL_VALUE
				  else
				    begin
				      ICSTP := RECVAR^.SELFSTP;
				      X_RECVAR := ICVAL;
				      RELARRAY[3] := 2
				    end
				end;
			      FILES:   begin
				X_FILEPF := FILEPF;
				if FILTYPE = nil then
				  X_FILTYPE := NIL_VALUE
				else
				  begin
				    ICSTP := FILTYPE^.SELFSTP;
				    X_FILTYPE := ICVAL
				  end
			      end;
			      TAGFWITHID: begin
				ICSTP := FSTVAR^.SELFSTP;
				X_FSTVAR := ICVAL;
				ICCTP := TAGFIELDP^.SELFCTP;
				X_TAGFP := ICVAL;
				RELARRAY[3] := 2
			      end;
			      TAGFWITHOUTID:
				begin
				  ICSTP := FSTVAR^.SELFSTP;
				  X_FSTVAR := ICVAL;
				  ICSTP := TAGFIELDTYPE^.SELFSTP;
				  X_TAGFTYPE := ICVAL;
				  RELARRAY[3] := 2
				end;
			      VARIANT:
				begin
				  X_VARVAL := VARVAL;
				  X_QXLYPRTWRR := QXLYPRTWRR;
				  if SUBVAR = nil then
				    X_SUBVAR := NIL_VALUE
				  else
				    begin
				      ICSTP := SUBVAR^.SELFSTP;
				      X_SUBVAR := ICVAL
				    end;
				  if FIRSTFIELD = nil then
				    X_FIRSTFIELD := NIL_VALUE
				  else
				    begin
				      ICCTP := FIRSTFIELD^.SELFCTP;
				      X_FIRSTFIELD := ICVAL;
				      RELARRAY[3] := 1
				    end;
				  if NXTVAR = nil then
				    X_NXTVAR := NIL_VALUE
				  else
				    begin
				      ICSTP := NXTVAR^.SELFSTP;
				      X_NXTVAR := ICVAL;
				      RELARRAY[3] := RELARRAY[3] + 2
				    end;
				end
			      end %CASE\;
			    LSIZE := STRECSIZE[FORM]; WRITERECORD;
			    NOCODE := FALSE
			  end %RUN 2\;
		    case FORM of
		      SCALAR:
			if SCALKIND = DECLARED then
			  COPYCTP(FCONST);
		      SUBRANGE:COPYSTP(RANGETYPE);
		      CONFORMANT:
			begin
			  COPYSTP(BOUNDTYPE);
			  COPYCTP(UPPER);
			  COPYCTP(LOWER)
		        end;
		      POINTER: COPYSTP(ELTYPE);
(* 235 - 4-word sets *)
		      POWER,QPOWER: COPYSTP(ELSET);
		      ARRAYS:
			begin
			  COPYSTP(AELTYPE);
			  COPYSTP(INXTYPE)
			end;
		      RECORDS:
			begin
			  COPYCTP(FSTFLD);
			  COPYSTP(RECVAR)
			end;
		      FILES:   COPYSTP(FILTYPE);
		      TAGFWITHID,
		      TAGFWITHOUTID:
			begin
			  COPYSTP(FSTVAR);
			  if FORM = TAGFWITHID then
			    COPYCTP(TAGFIELDP)
			  else
			    COPYSTP(TAGFIELDTYPE)
			end;
		      VARIANT:
			begin
			  COPYSTP(NXTVAR);
			  COPYSTP(SUBVAR);
			  COPYCTP(FIRSTFIELD)
			end
		      end %CASE\
		  end %WITH\
	  end %COPYSTP\;

	begin
	  %MCCODE\
	  CODEARRAY := FALSE; LLISTCODE:= FALSE;
	  if LISTCODE then
	    WRITEBUFFER;
	  if LASTBTP # nil then
	    with LASTBTP^ do
	      case BKIND of
		RECORDD:  LIC := FIELDCP^.FLDADDR;
		ARRAYY :  LIC := ARRAYSP^.ARRAYBPADDR
	      end;
	  WRITEFIRSTLINE; WRITEBLOCKST(1);
	  while LASTBTP # nil do
	    begin
	      with LASTBTP^,BYTE do
		begin
		  if LISTCODE then
		    begin
		      NEUEZEILE;
		      if LICMOD4 = 0 then
			WRITE(' ':7)
		      else
			WRITE(' ':5);
		      WRITE(' POINT  ',SBITS:2,',');
		      if IBIT = 0 then
			WRITE('  ')
		      else
			WRITE(' @');
		      if XBIT <> 0 then
		        WRITE(XRELADDR:5:O,'(',XIREG:2:O,'),',35-PBITS:2)
		      else WRITE(RELADDR:5:O,'(',IREG:2:O,'),',35-PBITS:2);
		    end;
		  with WANDLUNG do
		    begin
		      WBYTE := BYTE;  WRTWORD(NO,WKONST);
		      if WBYTE.XBIT <> 0 then
			WRTWORD(NO,WKONST2);
		    end;
		  LASTBTP := LAST
		end
	    end % WHILE\;
(* 227 - don't set CODEARRAY for XBLT - this is part of the block used
   for byte pointers and should be handled contiguously with it *)
	  if NEED_XBLT_OP then  {XBLT_LOC was set in this block}
	    begin
	      WRTWORD(NO,XOP_XBLT);
	      NEED_XBLT_OP := FALSE
	    end;
	  LIC := CODEEND - CIX - 1;
	  CODEARRAY := TRUE;
	  WRITEBLOCKST(1); WRITEFIRSTLINE;
	  for I := 0 to CIX do
	    with CODE, INSTRUCTION[I], HALFWORD[I] do
	      begin
		LRELBYTE := RELOCATION[I]; WRTWORD(LRELBYTE,WORD[I]);
		if LISTCODE then
		  begin
		    NEUEZEILE;
		    if LICMOD4 = 0 then
		      WRITE(' ':7)
		    else
		      WRITE(' ':5);
		    case INFORMATION[I] of
		      'W':
			begin
			  WRITE(' ':6,LEFTHALF :6:O); SHOWRELOCATION(LEFT,LRELBYTE);
			  WRITE(RIGHTHALF:6:O); SHOWRELOCATION(RIGHT,LRELBYTE);
			  WRITE(' ':5)
			end;
			%'B': WITH WANDLUNG.WBYTE DO
			 BEGIN
			 WANDLUNG.WKONST := WORD[I];
			 WRITE(' POINT  ',SBITS:2,',');
			 IF IBIT = 0 then WRITE('  ')
			 ELSE WRITE(' @');
			 WRITE(RELADDR:5:O,'(',IREG:2:O,'),',35-PBITS:2)
			 END;\
		      others:
			begin
			  (* UNPACK CAN'T DO THIS NOW *)
			  %UNPACK(MNEMONICS[(INSTR+9) DIV 10],((INSTR+9) MOD 10)*6+1,STRING,6);\
			  for J := 1 to 6 do
			    STRING[J] := MNEMONICS[(INSTR+9) div 10, ((INSTR+9) mod 10)*6 + J];
			  WRITE(' ',STRING:6, ' ',AC:2:O,', ');
			  if INDBIT = 0 then
			    WRITE(' ')
			  else
			    WRITE('@');
			  WRITE(ADDRESS:6:O); SHOWRELOCATION(RIGHT,LRELBYTE);
			  if INXREG > 0 then
			    WRITE('(',INXREG:2:O,')',INFORMATION[I]:1)
			  else
			    WRITE(' ':4,INFORMATION[I]:1)
			end
		      end
		  end;
		LIC := LIC + 1
	      end  %FOR \;
	  CODEARRAY := FALSE; LLISTCODE := LISTCODE;
	  if FIRSTKONST # nil then
	    begin
	      LFIRSTKONST := FIRSTKONST; WRITEFIRSTLINE; WRITEBLOCKST(1);
	      while LFIRSTKONST # nil do
		begin
		  with LFIRSTKONST^.CONSTPTR^ do
		    case CCLASS of
		      INT,
		      REEL: WRTWORD(NO,INTVAL);
		      PSET:
			begin
			  % THE SET IS PICKED UP
			   AND WRITTEN OUT AS TWO OCTAL NUMBERS \
			  WRTWORD(NO,INTVAL);  WRTWORD(NO,INTVAL1);
			end;
(* 235 - 4-word set *)
		      CPSET:
			begin WRTWORD(NO,INTVAL); WRTWORD(NO,INTVAL1);
			      WRTWORD(NO,INTVAL2); WRTWORD(NO,INTVAL3) end;
		      STRD,
		      STRG: with WANDLUNG do begin
			J :=0; WKONST := 0;
			for I := 1 to SLGTH do
			  begin
			    J := J + 1;  WSTRING[J] := SVAL[I];
			    if J = 5 then
			      begin
				J := 0;  WRTWORD(NO,WKONST);
				WKONST := 0
			      end
			  end;
			if J # 0 then
			  WRTWORD(NO,WKONST)
		      end
		    end;
		  LFIRSTKONST := LFIRSTKONST^.NEXTKONST
		end     %WHILE\
	    end;
	  if ADJSP_LOC <> 0 then
	    WRTWORD(NO,ADJSP_VAL);
	  if DEBUG then
	    begin
	      if DEBUGSWITCH then
		begin
		  WRITEFIRSTLINE;
		  for RUN1 := TRUE downto FALSE do
		    COPYCTP(DISPLAY[TOP].FNAME);
		  if LEVEL = 1 then
		    begin
		      (* set globalidtree and standardidtree *)
		      for RUN1 := TRUE downto FALSE do
			COPYCTP(DISPLAY[0].FNAME);
		      if DISPLAY[TOP].FNAME = nil then
			DEBUGENTRY.GLOBALIDTREE := nil
		      else
			DEBUGENTRY.GLOBALIDTREE := DISPLAY[TOP].FNAME^.SELFCTP;
		      DEBUGENTRY.STANDARDIDTREE := DISPLAY[0].FNAME^.SELFCTP
		    end
		end %DEBUGSWITCH\;
	      if LEVEL = 1 then
		begin
		  with DEBUGENTRY do
		    begin
		      NEWPAGER; LASTPAGEELEM := PAGER;
		      INTPOINT  := INTPTR^. SELFSTP;
		      REALPOINT := REALPTR^.SELFSTP;
		      CHARPOINT := CHARPTR^.SELFSTP;
		      NEXTDEB := 0; %LINK WILL MAKE THIS PTR TO SIMILAR ENTRY IN NEXT MODULE\
		      MODNAME := FILENAME;  CURNAME(INPUT,SOURCE)
		    end;
		  PAGEHEADADR := IC;
		  LSIZE := 44; %LENGTH OF DEBUGENTRY-RECORD\
		  RELARRAY[1] := 0;
		  for I:=2 to 8 do  RELARRAY[I] := 1;
		  for I:= 9 to LSIZE do  RELARRAY[I] := 0;
		  RECORDWANDEL.DEBUGREC := DEBUGENTRY;
		  IC := IC + LSIZE;  WRITERECORD;
		  HIGHESTCODE := IC;
		  if LISTCODE then
		    NEWLINE;
		  WRITEHEADER('LINK IN CHAIN 1');
		  LLISTCODE := FALSE;
		  WRITEBLOCKST(12B); %LINK BLOCK\
		  WRITEPAIR(NO,0,1); %LINK NUMBER 1\
		  LLISTCODE := LISTCODE;
		  WRITEPAIR(RIGHT,0,PAGEHEADADR)  %NEXTDEB FIELD\
		end
	    end;
	  if LISTCODE then
	    NEWLINE
	end %MCCODE\;
      procedure MCVARIOUS;
	var
	  INLEVEL: BOOLEAN; PNAME:ALFA;
	begin %MCVARIOUS\
	  case WRITEFLAG of
	    WRITEBLK:
	      begin
		PNAME := DISPLAY[TOP].BLKNAME;
		WRITEHEADER('LOCAL SYMBOLS  ');
		WRITEBLOCKST(2);
		WRITEIDENTIFIER(2B,PNAME);
		WRITEPAIR(RIGHT,0,PFSTART);
		I:=5;
		while PNAME[I] = ' ' do I := I - 1;
		if PFDISP#PFSTART then
		  begin
		    PNAME[I+1] := '.';
		    WRITEIDENTIFIER(2B,PNAME);
		    WRITEPAIR(RIGHT,0,PFDISP)
		  end;
		if PFPOINT#PFDISP then
		  begin
		    PNAME[I+1] := '%';
		    WRITEIDENTIFIER(2B,PNAME);
		    WRITEPAIR(RIGHT,0,PFPOINT)
		  end
	      end;
	      (* Polish fixups *)
	    WRITEPOLISH:
	      begin
		WRITEHEADER('POLISH FIXUPS  ');
		while FIRSTPOL <> nil do
		  with FIRSTPOL^ do
		  if POLTYPE = ARRAYPOL then
		    begin
		      {A Polish fixup block looks like this:
		      type 11
		      operator,,0             0 means next half word is operand
		      operand1,,0
		      operand2,,-1            -1 means put in RH of result addr
		      place to put result,,0
		      }
		      WRITEBLOCKST(11B);
		      if OFFSET < 0 then
			WRITEPAIR(NO,4,0)  {4 - SUB}
		      else
			WRITEPAIR(NO,3,0);
		      {3 - ADD}
		      WRITEPAIR(LEFT,BASE,0);
		      WRITEPAIR(NO,ABS(OFFSET),777777B);
		      WRITEPAIR(LEFT,WHERE,0);
		      PUTRELCODE;
		      FIRSTPOL := NEXTPOL  {CDR down list}
		    end
		  else
		    begin
		      {An XPOL block looks like this:
		      type 11
		      operator,,2             2 means next word is Radix50
		      symbolname
		      1,,LH		      1 means a fullword value
		      RH,,-3		      -7 is fullword fixup
		      addr,,0
		      }
		      WRITEBLOCKST(11B);
		      WRITEPAIR(NO,3,2);	{add}
		      WRITEIDENTIFIER(14B,XADDRSYM);
		      WRITEPAIR(NO,1,OFFSET DIV 1000000B);
		      WRITEPAIR(NO,OFFSET MOD 1000000B,777775B);
		      WRITEPAIR(LEFT,WHERE,0);
		      PUTRELCODE;
		      FIRSTPOL := NEXTPOL  {CDR down list}
		    end;
		if CREF and LISTCODE then
		  NEWLINE
	      end;

	    WRITEINTERNALS:
	      begin
		WRITEHEADER('LOCAL REQUESTS '); INLEVEL := TRUE;
		WRITEBLOCKST(8); CHECKER := LOCALPFPTR;
		while (CHECKER # nil) and INLEVEL do
		  with CHECKER^ do
		    if PFLEV = LEVEL then
		      begin
			if PFADDR # 0 then
			  for I := 0 to MAXLEVEL do
			    if LINKCHAIN[I] # 0 then
			      WRITEPAIR(BOTH,LINKCHAIN[I],PFADDR-I);
			CHECKER:= PFCHAIN
		      end
		    else
		      INLEVEL := FALSE;
		if LEVEL > 1 then
		  LOCALPFPTR := CHECKER;
		while FIRSTKONST # nil do
		  with FIRSTKONST^, CONSTPTR^ do
		    begin
		      WRITEPAIR(BOTH,ADDR,KADDR);
		      if (CCLASS in [PSET,STRD]) and (ADDR1 <> 0) then
			WRITEPAIR(BOTH,ADDR1,KADDR+1);
		      FIRSTKONST:= NEXTKONST
		    end;
		INLEVEL := TRUE;
		while (LASTLABEL # nil) and INLEVEL do
		  with LASTLABEL^ do
		    if SCOPE = LEVEL then
		      begin
			if GOTOCHAIN # 0 then
			  if LABELADDRESS = 0 then
			    ERRORWITHTEXT(215,NAME)
			  else
			    WRITEPAIR(BOTH,GOTOCHAIN,LABELADDRESS);
			LASTLABEL := NEXT
		      end
		    else
		      INLEVEL := FALSE;
		if CREF and LISTCODE then
		  NEWLINE
	      end;
	    WRITEEND:
	      begin
		WRITEHEADER('HIGHSEG-BREAK  ');
		WRITEBLOCKST(5);
		WRITEPAIR(RIGHT,0,HIGHESTCODE);
		WRITEHEADER('LOWSEG-BREAK   ');
		WRITEPAIR(RIGHT,0,LCMAIN); PUTRELCODE
	      end;

	    WRITESTART:
	      if MAIN then
		begin
		  WRITEHEADER('VERSION NUMBER ');
		  LIC := 137B;
		  WRITEBLOCKST(1);
		  if LISTCODE then
		    with VERSION do
		      WRITE('    ',WHO:1:O,'  ',MAJOR:3:O,'  ',MINOR:2:O,'  ',EDIT:6:O);
		  LLISTCODE := FALSE;
		  WRTWORD(NO,VERSION.WORD);
		  LLISTCODE := LISTCODE;
		  WRITEHEADER('STARTADDRESS   ');
		  WRITEBLOCKST(7);
		  if TOPS10 then
		    WRITEPAIR(RIGHT,0,STARTADDR)
		  else
(* 247 - full Tops-20 entry vector *)
		    WRITEPAIR(RIGHT,3+EVEC_EXTRA,EVEC_LOC)
		end;

	    WRITEENTRY:
	      begin
		WRITEBLOCKST(4);
		(* USE LIST OF ENTRIES IN PROGRAM IF APPROPRIATE *)
		if MAIN or (FPROGFILE = nil) then
		  WRITEIDENTIFIER(0,FILENAME)
		else
		  begin
		    NPROGFILE := FPROGFILE;
		    while NPROGFILE # nil do
		      begin
			WRITEIDENTIFIER(0,NPROGFILE^.FILID);
			NPROGFILE := NPROGFILE^.NEXT
		      end
		  end
	      end;

	    WRITENAME:
	      begin
		WRITEBLOCKST(6);
		WRITEIDENTIFIER(0,FILENAME)
	      end;

	    WRITEHISEG:
	      begin
		LLISTCODE := FALSE;
		WRITEBLOCKST(3);
		WRITEPAIR(NO,HIGHSTART,HIGHSTART)
	      end
	    end %CASE\
	end %MCVARIOUS\;

      procedure MCSYMBOLS;
	var
	  ENTRYFOUND: BOOLEAN;
	  POLHEADERDONE:BOOLEAN;
	  CHAN:INTEGER;
	begin %MCSYMBOLS\
	  WRITEHEADER('ENTRYPOINT(S)  ');
	  WRITEBLOCKST(2); SAVELISTCODE := LISTCODE;
	  LISTCODE := FALSE;
	  for SWITCHFLAG := 1B to 2B do
	    begin
	      if MAIN then
		begin
		  WRITEIDENTIFIER(SWITCHFLAG,FILENAME);
		  WRITEPAIR(RIGHT,0,STARTADDR)
		end
	      else
		begin
		  (* LOOK FOR FTN=FILENAME ONLY IF NOT SPEC. IN PROGRAM STATE. *)
		  CHECKER := LOCALPFPTR;
		  if FPROGFILE=nil then
		    ENTRYFOUND := FALSE
		  else
		    ENTRYFOUND := TRUE;
		  while CHECKER # nil do
		    with CHECKER^ do
		      begin
			if PFADDR # 0 then
			  begin
			    if not ENTRYFOUND then
			      ENTRYFOUND := FILENAME = NAME;
			    WRITEIDENTIFIER(SWITCHFLAG,NAME);
			    WRITEPAIR(RIGHT,0,PFADDR);
			    if PFCHAIN = nil then
			      if not ENTRYFOUND then
				begin
				  WRITEIDENTIFIER(SWITCHFLAG,FILENAME);
				  WRITEPAIR(RIGHT,0,PFADDR)
				end
			  end;
			CHECKER:= PFCHAIN
		      end
		end;
	      LISTCODE := SAVELISTCODE; LIC := 0
	    end;
	  SWITCHFLAG:= 11B; WRITEHEADER('GLOBAL SYMBOLS ');
	  (* Note - 11B gives SUPPRESSED global symbols *)
	  WRITEIDENTIFIER(SWITCHFLAG,'%PASXT    ');
	  if EXTENDED_ADDRESSING then
	    WRTWORD(NO,1)
	  else
	    WRTWORD(NO,0);
	  WRITEIDENTIFIER(SWITCHFLAG,'%PASVR    ');
	  WRTWORD(NO,PAS_VERSION);
	  WRITEIDENTIFIER(SWITCHFLAG,'%PASFB    ');
	  WRTWORD(NO,SIZEOFFILEBLOCK);
	  if MAIN then
	    begin
	      SWITCHFLAG := 1B;
	      WRITEIDENTIFIER(SWITCHFLAG,'.MAIN.    ');
	      WRTWORD(RIGHT,MAINSTART);
	      if KLCPU and not TOPS10 then {PASIO needs this even if
					    not extended addressing}
		begin
		  WRITEIDENTIFIER(SWITCHFLAG,'.XSTRT    ');
		  WRITEPAIR(NO,1,0)
 	        end
	    end
	  else
	    begin
(* 264 - make files be defined in PASLIB *)
	      SWITCHFLAG:= 14B; 
	    end;
	  WRITEHEADER('GLOBAL REQUESTS');
	  if MAIN then
	    begin
	      WRITEIDENTIFIER(14B,'%CCLSW    ');
	      WRITEPAIR(RIGHT,0,CCLSW[0]);
	      WRITEIDENTIFIER(14B,'%RNNAM    ');
	      WRITEPAIR(RIGHT,0,CCLSW[1]);
	      WRITEIDENTIFIER(14B,'%RNPPN    ');
	      WRITEPAIR(RIGHT,0,CCLSW[2]);
	      WRITEIDENTIFIER(14B,'%RNDEV    ');
	      WRITEPAIR(RIGHT,0,CCLSW[3]);
	      WRITEIDENTIFIER(14B,'%CCLDN    ');
	      WRITEPAIR(RIGHT,0,CCLSW[4]);
	    end;
	  FILEPTR := SFILEPTR;
	  while FILEPTR # nil do
	    with FILEPTR^, FILEIDENT^ do
	      begin
		if VADDR # 0 then
		  begin
		    WRITEIDENTIFIER(14B,NAME);
		    WRITEPAIR(RIGHT,0,VADDR)
		  end;
		FILEPTR:= NEXTFTP
	      end;
	  CHECKER:= EXTERNPFPTR;
	  while CHECKER # nil do
	    with CHECKER^ do
	      begin
		if LINKCHAIN[0] # 0 then
		  begin
		    if PFLEV = 0 then
		      WRITEIDENTIFIER(14B,EXTERNALNAME)
		    else
		      WRITEIDENTIFIER(14B,NAME);
		    WRITEPAIR(RIGHT,0,LINKCHAIN[0])
		  end;
		CHECKER:= PFCHAIN
	      end;
	  (* EXTERNAL REF TO RUNTIMES FOR DYNAMIC CORE ALLOC *)
	  if LSTNEW # 0 then
	    begin
	      WRITEIDENTIFIER(14B,'LSTNEW    ');
	      WRITEPAIR(RIGHT,0,LSTNEW)  % GLOBAL FIXUP TO INIT. CODE\
	    end;
	  if NEWBND # 0 then
	    begin
	      WRITEIDENTIFIER(14B,'NEWBND    ');
	      WRITEPAIR(RIGHT,0,NEWBND)  % DITTO \
	    end;
	  for SUPPORTIX:= SUCC(FIRSTSUPPORT) to PRED(LASTSUPPORT) do
	    if RNTS.LINK[SUPPORTIX] # 0 then
	      begin
		WRITEIDENTIFIER(14B,RNTS.NAME[SUPPORTIX]);
		WRITEPAIR(RIGHT,0,RNTS.LINK[SUPPORTIX])
	      end;
	  if KLCPU and not TOPS10 then  {PASIO expects this even if
					extended addressing is not in use}
	    begin
	      WRITEHEADER('.ASSIGN        ');
	      WRITEBLOCKST(100B);
	      WRITEIDENTIFIER(1B,XADDRSYM);
	      WRITEIDENTIFIER(14B,'.XSTRT    ');
	      WRTWORD(NO,XC-1000000B);
	      PUTRELCODE
	    end;
	  {In non-main modules, if there are references to TTY^, etc., a
	  Polish fixup may be needed to resolve them.}
	  POLHEADERDONE := FALSE;
	  FILEPTR := SFILEPTR;
(* 264 - files are always external *)
	    while FILEPTR # nil do
	      with FILEPTR^, FILEIDENT^ do
		begin
		  if CHANTAB[CHANNEL] <> 0 then
		    begin
		      if not POLHEADERDONE then
			begin
			  WRITEHEADER('SYMBOLIC POLISH');
			  POLHEADERDONE := TRUE
			end;
		      {A Polish fixup block looks like this:
		      type 11
		      operator,,2             2 means next word is global req - that is operand
		      operand1
		      0,,operand2             0 means next half word is operand
		      -1,,place to put        -1 means put in RH of result addr
		      }
		      WRITEBLOCKST(11B);
		      WRITEPAIR(NO,3,2);  {add}
		      WRITEIDENTIFIER(0,NAME);
		      WRITEPAIR(NO,0,FILCMP);
		      WRITEPAIR(RIGHT,777777B,CHANTAB[CHANNEL]);
		      PUTRELCODE
		    end;
		  FILEPTR:= NEXTFTP
		end;
	  if POLHEADERDONE and CREF and LISTCODE then
	    NEWLINE;
	end %MCSYMBOLS\;

      procedure MCLIBRARY;
	begin
	  %MCLIBRARY\
	  WRITEHEADER('LINK LIBRARIES ');
	  WRITEBLOCKST(15);
	  for L := 1 to 2 do
	    begin
	      for I := 1 to LIBIX do
		with LIBRARY[LIBORDER[I]] do
		  if CALLED then
		    with WANDLUNG do
		      begin
			for J := 1 to 6 do
			  WSIXBIT[J] := ORD(NAME[J]) - 40B;
			WRITEIDENTIFIER(6B,NAME);
			WRITEPAIR(NO,PROJNR,PROGNR);
			for J := 1 to 6 do
			  WSIXBIT[J] := ORD(DEVICE[J]) - 40B;
			WRITEIDENTIFIER(6B,DEVICE); LIC := LIC + 1
		      end;
	      I := 1;
	      (* load PASLIB first *)
	      for LANGUAGEIX := PASCALSY to FORTRANSY do
		with LIBRARY[LANGUAGEIX] do
		  begin
		    CALLED := (not INORDER and CALLED) or (LANGUAGEIX = PASCALSY);
		    LIBORDER[I] := LANGUAGEIX; I := I + 1
		  end;
	      LIBIX := 2
	    end;
	end %MCLIBRARY\;

      begin  %WRITEMC\
{Remove these comments and those in the VAR section, and you will get a
 version of the compiler that prints out the sizes to be used in
 IDRECSIZE and STRRECSIZE}
/*	writeln(tty,'IDREC:TYPES',recsize(idfile,types));
	writeln(tty,'IDREC:KONST',recsize(idfile,konst));
	writeln(tty,'IDREC:VARS',recsize(idfile,vars));
	writeln(tty,'IDREC:FIELD',recsize(idfile,field));
	writeln(tty,'IDREC:PROC',recsize(idfile,proc));
	writeln(tty,'IDREC:FUNC',recsize(idfile,func));
	writeln(tty,'IDREC:PARAMS',recsize(idfile,params));
	writeln(tty,'IDREC:LABELT',recsize(idfile,labelt));

	writeln(tty,'STRREC:SCALAR',recsize(strfile,scalar));
	writeln(tty,'STRREC:SUBRANGE',recsize(strfile,subrange));
	writeln(tty,'STRREC:POINTER',recsize(strfile,pointer));
	writeln(tty,'STRREC:POWER',recsize(strfile,power));
	writeln(tty,'STRREC:ARRAYS',recsize(strfile,arrays));
	writeln(tty,'STRREC:RECORDS',recsize(strfile,records));
	writeln(tty,'STRREC:FILES',recsize(strfile,files));
	writeln(tty,'STRREC:TAGFWITHID',recsize(strfile,tagfwithid));
	writeln(tty,'STRREC:TAGFWITHOUTID',recsize(strfile,tagfwithoutid));
	writeln(tty,'STRREC:VARIANT',recsize(strfile,variant));  */

	LIC := 0;
	CODEARRAY := FALSE;
	if not ERRORFLAG then
	  begin
	    if CREF and LISTCODE then
	      WRITE(CHR(177B),'F');
	    for I:=1 to MAXSIZE do RELEMPTY[I] := 0;
	    with ICWANDEL do
	      begin
		ICVAL := 0;  CSP0 := ICCSP
	      end;
	    LLISTCODE := LISTCODE;
	    case WRITEFLAG of
	      WRITEGLOBALS     : MCGLOBALS;    %LINK-ITEM 01B\
	      WRITECODE        : MCCODE;       %LINK-ITEM 01B\
	      WRITESYMBOLS     : MCSYMBOLS;    %LINK-ITEM 02B\
	      WRITEBLK,                        %LINK-ITEM 02B\
	      WRITEINTERNALS,          %LINK-ITEM 10B\
	      (* Polish fixups *)
	      WRITEPOLISH,                     %LINK-ITEM 11B\
	      WRITEENTRY,                      %LINK-ITEM 04B\
	      WRITEEND,                        %LINK-ITEM 05B\
	      WRITESTART,                      %LINK-ITEM 07B\
	      WRITEHISEG,                      %LINK-ITEM 03B\
	      WRITENAME        : MCVARIOUS;    %LINK-ITEM 06B\
	      WRITELIBRARY     : MCLIBRARY     %LINK-ITEM 17B\
	      end %CASE\;
	    if LISTCODE and (WRITEFLAG > WRITEHISEG) then
	      NEWLINE;
	    if CREF and LISTCODE then
	      WRITE(CHR(177B),'B')
	  end %IF ERRORFLAG\
	else
	  if WRITEFLAG = WRITECODE then
	    LASTBTP := nil
      end %WRITEMC\;
    procedure STATEMENT(FSYS,STATENDS: SETOFSYS);
      type
	VALUEKIND = (ONREGC,ONFIXEDREGC,TRUEJMP,FALSEJMP);
      var
	LCP: CTP;
	IX,J: INTEGER;
(* 233 better allocation of LC area *)
	LCBASE: XADDRRANGE;


{NB: For conformant arrays, this does FASTBASIS}
	procedure SUBLOWBOUND(REG: ACRANGE; INXT: STP; var CINDEX: ACRANGE; 
				LMIN, LMAX: XADDRRANGE);
	  begin
	  if INXT <> NIL then
	    if INXT^.FORM = CONFORMANT then
	      begin
		FASTBASIS(INXT,CINDEX);
		if RUNTMCHECK then
		  begin
		    MACRO4(OP_CAML,REG,CINDEX,LMIN);
		    MACRO4(OP_CAMLE,REG,CINDEX,LMAX);
		    SUPPORT(INDEXERROR)
		  end;
		MACRO4(OP_SUB,REG,CINDEX,LMIN);
	      end
	    else
	      begin
	    	if LMIN > 0 then
	      	  MACRO3C(OP_SUB,REG,LMIN)
	    	else
	      	  if LMIN < 0 then
		    MACRO3C(OP_ADD,REG,-LMIN);
	        if RUNTMCHECK then
	          begin
		    MACRO3(OP_CAIL,REG,0);
		    MACRO3C(OP_CAMLE,REG,LMAX-LMIN);
		    SUPPORT(INDEXERROR)
	          end
	      end
	  end {SUBLOWBOUND};

	procedure OFFBOUND(REG: ACRANGE; ARRTYPE: STP; var CINDEX: ACRANGE; 
				LBOUND,OFFSET: XADDRRANGE);
	  begin
	    if ARRTYPE <> NIL then
	      if ARRTYPE^.INXTYPE <> NIL then
	        if ARRTYPE^.INXTYPE^.FORM = CONFORMANT then
	          begin
		    FASTBASIS(ARRTYPE^.INXTYPE,CINDEX);
		    MACRO4(OP_MOVE,REG,CINDEX,LBOUND);
		    if OFFSET > 0 then
		      MACRO3C(OP_ADD,REG,OFFSET)
	            else if OFFSET < 0 then
		      MACRO3C(OP_SUB,REG,-OFFSET)
	          end
	        else
	          MACRO3C(OP_MOVE,REG,LBOUND+OFFSET)
	  end; {OFFBOUND}

      procedure EXPRESSION(FSYS: SETOFSYS; FVALUE:VALUEKIND);
	forward;

      procedure MAKEREAL(var FATTR: ATTR);
	begin
	  if FATTR.TYPTR=INTPTR then
	    begin
	      LOAD(FATTR);
	      if KACPU then
		begin
		  MACRO3(OP_MOVEI,TAC1,FATTR.REG);
		  SUPPORT(CONVERTINTEGERTOREAL);
		end
	      else
		with CODE.INSTRUCTION[CIX] do
		  if (INSTR = OP_MOVE) and (AC = FATTR.REG) then
		    INSTR := OP_FLTR
		  else
		    MACRO3(OP_FLTR,FATTR.REG,FATTR.REG);
	      FATTR.TYPTR := REALPTR
	    end;
	  if GATTR.TYPTR=INTPTR then
	    MAKEREAL(GATTR)
	end;

      procedure SELECTOR(FSYS: SETOFSYS; FCP: CTP);
	var
	  LATTR: ATTR;
	  LCP: CTP;
	  LSP: STP;
	  LMIN,LMAX,INDEXVALUE,INDEXOFFSET: INTEGER;
(* 240 - conformant *)
	  CONFIND: STP;
	  CONF,CONF2: BOOLEAN;
	  CINDEX: ACRANGE;
	  OLDIC: ACRANGE;


	begin {SELECTOR}
	  with FCP^, GATTR do
	    begin
(* 274 - clear EXTERNCTP always *)
	      TYPTR := IDTYPE; KIND := VARBL; PACKFG := NOTPACK; EXTERNCTP := NIL;
	      case KLASS of
		VARS:
		  begin
		    VLEVEL := VLEV;  DPLMT := VADDR; INDEXR := 0;
		    NOASSIGN := CONFARG or VNOASSIGN;
		    if VLEV > 1 then
		      VRELBYTE:= NO

(* 260 - find a few more odd cases *)
{If VADDR < PROGRST, we really have an absolute reference.  The
 most common case is 0, at the end of a fixup chain.  One would
 at first think that VRELBYTE := NO should be here.  However that
 doesn't work.  Apparently other places in the code assume that
 all lowseg code is marked RIGHT.  The routine that actually
 outputs code turns it into NO at the very last moment.  So we
 use RIGHT here.  We don't want to use LOWSEG, because that would
 really cause lowseg relocation to be forced, using a Polish fixup.}

		    else if VADDR < PROGRST {0 or JOBDAT} then
		      VRELBYTE:= RIGHT
(* 257 - better segment control *)
		    else if VADDR > MAXADDR then
		      VRELBYTE:= XSEG
		    else 
		      VRELBYTE:= LOWSEG;
(* 274 - EXTERNCTP := NIL moved above *)
		    if IDTYPE^.FORM = FILES then
(* 264 - make std files always be extern *)
(* 274 - VLEV is only meaningful if KLASS = VARS *)
		      if KLASS=VARS then
			if VLEV=0 then
			  EXTERNCTP := FCP;
		    if VKIND=ACTUAL then
		      INDBIT:=0
		    else
		      INDBIT:=1
		  end;
		FIELD:
		  with DISPLAY[DISX] do
		    if OCCUR = CREC then
		      begin
			VLEVEL := CLEV; PACKFG := PACKF; VRELBYTE:= CRELBYTE;
			NOASSIGN := FALSE;
			if PACKFG = PACKK then
			  begin
			    BPADDR := FLDADDR;  DPLMT := CDSPL
			  end
			else
			  DPLMT := CDSPL + FLDADDR;
			INDEXR := CINDR; INDBIT:=CINDB
		      end
		    else
		      ERROR(171);
		FUNC:
		  if PFDECKIND = STANDARD then
		    ERROR(502)
		  else
		    if PFLEV = 0 then
		      ERROR(502)   %EXTERNAL FCT\
		    else
		      if PFKIND = FORMAL then
			ERROR(456)
			(* BE SURE WE'RE IN THE BODY OF THE FTN *)
		      else
			if (LEVEL <= PFLEV) or (DISPLAY[PFLEV+1].BLKNAME # NAME) then
			  ERROR(412)
			else
			  begin
			    (* use pflev+1 for vlevel, to allow assignment from inner function *)
			    VLEVEL := PFLEV + 1; VRELBYTE := NO;
			    DPLMT := 1;   %IMPL. RELAT. ADDR. OF FCT. RESULT\
			    INDEXR :=0; INDBIT :=0; NOASSIGN := FALSE;
			  end
		end %CASE\
	    end %WITH\;
	  IFERRSKIP(166,SELECTSYS or FSYS);
	  while SY in SELECTSYS do
	    begin
	      if FCP^.KLASS = FUNC then
		ERROR(368);
	      if SY = LBRACK then
		begin
		  %[\
		  if GATTR.INDBIT = 1 then
		    GETPARADDR;
		  OLDIC := GATTR.INDEXR;
		  INDEXOFFSET := 0;
		  loop
		    LATTR := GATTR; INDEXVALUE := 0;
		    with LATTR do
		      if TYPTR # nil then
			begin
			  if TYPTR^.FORM # ARRAYS then
			    begin
			      ERROR(307); TYPTR := nil
			    end;
			  LSP := TYPTR
			end;
		    INSYMBOL;
		    EXPRESSION(FSYS or [COMMA,RBRACK],ONREGC);
		    CONF := FALSE; CONF2 := FALSE;
		    if GATTR.TYPTR # nil then
		      if GATTR.TYPTR^.FORM # SCALAR then
			ERROR(403);
		    if LATTR.TYPTR # nil then
		      with LATTR,TYPTR^ do
			begin
			  CONFIND := INXTYPE;
			  if COMPTYPES(INXTYPE,GATTR.TYPTR) then
			    begin
			      if INXTYPE # nil then
				begin
				  if INXTYPE^.FORM = CONFORMANT then
				    begin
				    CONF := TRUE;
				    LMIN := INXTYPE^.LOWER^.VADDR;
				    LMAX := INXTYPE^.UPPER^.VADDR
				    end
				  else
				    begin
				    GETBOUNDS(INXTYPE,LMIN,LMAX);
				    if GATTR.KIND = CST then
				      if (GATTR.CVAL.IVAL < LMIN) or
					 (GATTR.CVAL.IVAL > LMAX) then
				        ERROR(263)
				    end
				end
			    end
			  else
			    ERROR(457);
			  TYPTR := AELTYPE;
			  if TYPTR <> NIL then
			    CONF2 := (TYPTR^.FORM=ARRAYS) and TYPTR^.ARRAYCONF
			end;
		      if (GATTR.KIND = CST) and not CONF and not CONF2 then
		        INDEXVALUE := GATTR.CVAL.IVAL
		      else LOAD(GATTR);
		  exit if SY # COMMA;
		    with LATTR do
		      if TYPTR#nil then
(* 240 conformant *)
			if (GATTR.KIND = CST) and not CONF and not CONF2 then
			  DPLMT := DPLMT +(INDEXVALUE - LMIN) * TYPTR^.SIZE
			else
			  begin
			    SUBLOWBOUND(REGC,CONFIND,CINDEX,LMIN,LMAX);
			    if CONF2 then
			      begin
				PHYSSIZE(TAC0,TYPTR);
				MACRO3(OP_IMUL,REGC,TAC0)
			      end
			    else
			      if TYPTR^.SIZE > 1 then
			        MACRO3C(OP_IMUL,REGC,TYPTR^.SIZE);
			    if OLDIC = 0 then
			      OLDIC := REGC
			    else
			      if OLDIC > REGCMAX then
				begin
				  MACRO3(OP_ADD,REGC,OLDIC);
				  OLDIC := REGC
				end
			      else
				begin
				  MACRO3(OP_ADD,OLDIC,REGC);
				  REGC := REGC - 1
				end;
			    INDEXR := OLDIC
			  end;
		    GATTR := LATTR
		end %LOOP\;
		  with LATTR do
		    if TYPTR # nil then
		      begin
(* 240 - conformant *)
			if (GATTR.KIND = CST) and not CONF and not CONF2 then
			  INDEXOFFSET :=  (INDEXVALUE - LMIN) * TYPTR^.SIZE
			else
			  begin
			    if (TYPTR^.SIZE > 1) or CONF or CONF2 or
			        RUNTMCHECK then
			      SUBLOWBOUND(REGC,CONFIND,CINDEX,LMIN,LMAX)
			    else
			      INDEXOFFSET := -LMIN;
			    if CONF2 then
			      begin
				PHYSSIZE(TAC0,TYPTR);
				MACRO3(OP_IMUL,REGC,TAC0)
			      end
			    else
			      if TYPTR^.SIZE > 1 then
			        MACRO3C(OP_IMUL,REGC,TYPTR^.SIZE);
			    INDEXR := REGC
			  end;
			if LSP^.ARRAYPF then
			  begin
			    if not KLCPU then
			      INCREMENTREGC;
			    if INDEXR=OLDIC then
			      begin
				INCREMENTREGC; INDEXR := 0
			      end;
			    if not KLCPU then
			      begin
				MACRO4(OP_HRREI,REGC,INDEXR,INDEXOFFSET);
				INCREMENTREGC;   %test for IDIVI-instruction\
				REGC := REGC-1; INDEXOFFSET := 0;
				MACRO3R(OP_MOVE,REGC-1,LSP^.ARRAYBPADDR);
				MACRO3(OP_IDIVI,REGC,BITMAX div LSP^.AELTYPE^.BITSIZE);
				MACRO3(OP_IBP,0,REGC-1);
				MACRO3R(OP_SOJGE,REGC+1,IC-1);
				BPADDR := REGC-1;  PACKFG := PACKK;
				INDEXR := REGC
			      end
			    else
			      begin (* KL CODE*)
				if EXTENDED_ADDRESSING then
				  if REGC = INDEXR then
				    MACRO3C(OP_ADD,REGC,INDEXOFFSET+1)
				  else
				    begin
				      MACRO3C(OP_MOVE,REGC,INDEXOFFSET+1);
				      if INDEXR <> 0 then
				        MACRO3(OP_ADD,REGC,INDEXR)
				    end
				else MACRO4(OP_HRREI,REGC,INDEXR,INDEXOFFSET+1);
				MACRO3R(OP_ADJBP,REGC,LSP^.ARRAYBPADDR);
				BPADDR := REGC; PACKFG := PACKK; INDEXR := 0;
				INDEXOFFSET := 0
			      end
			  end;
			DPLMT := DPLMT + INDEXOFFSET;
			KIND := VARBL;
			if (OLDIC # INDEXR) and (OLDIC # 0) then
			  begin
			    if INDEXR = 0 then
			      INDEXR := OLDIC
			    else
			      if OLDIC > REGCMAX then
				MACRO3(OP_ADD,INDEXR,OLDIC)
			      else
				begin
				  MACRO3(OP_ADD,OLDIC,INDEXR);
				  REGC := REGC - 1;
				  INDEXR := OLDIC
				end
			  end
		      end %WITH.. IF TYPTR # NIL\;
		  GATTR := LATTR;
		  if SY = RBRACK then
		    INSYMBOL
		  else
		    ERROR(155)
		end %IF SY = LBRACK\
	      else
		if SY = PERIOD then
		  begin
		    %.\
		    with GATTR do
		      begin
			if TYPTR # nil then
			  if TYPTR^.FORM # RECORDS then
			    begin
			      ERROR(308); TYPTR := nil
			    end;
			if INDBIT=1 then
			  GETPARADDR;
			INSYMBOL;
			if SY = IDENT then
			  begin
			    if TYPTR # nil then
			      begin
				SEARCHSECTION(TYPTR^.FSTFLD,LCP);
				if CREF then
				  WRITE(CHR(1B),CHR(21),ID,'/.FIELDID. ');
				if LCP = nil then
				  begin
				    ERROR(309); TYPTR := nil
				  end
				else
				  with LCP^ do
				    begin
				      TYPTR := IDTYPE;PACKFG := PACKF;
				      if PACKFG = PACKK then
					BPADDR := FLDADDR
				      else
					DPLMT := DPLMT + FLDADDR
				    end
			      end;
			    INSYMBOL
			  end %SY = IDENT\
			else
			  ERROR(209)
		      end %WITH GATTR\
		  end %IF SY = PERIOD\
		else
		  begin
		    %^\
		    if GATTR.TYPTR # nil then
		      with GATTR,TYPTR^ do
			if FORM = FILES then
			  begin
			    TYPTR := FILTYPE;
			    {What we are trying to do here is to generate code like
			    XMOVEI 2,INPUT+FILCMP
			    In the usual case, we just do a loadaddress on the file, after add
			    filcmp to the displacement.  There are two cases where this won't
			    work:
			    - when the address is an external reference, since it then
			    becomes an address in a fixup chain, and can't have FILCMP
			    added to it at compile time.  Thus we have a separate
			    fixup chain stored in CHANTAB which the loader will add
			    FILCMP to after fixing up.
			    - when the thing is indirect, since we have to add the displacment
			    after doing the indirection.  The only solution there is
			    an ADDI, as far as I can see.
			    Hamburg used to just do a LOAD, which works because at INPUT there
			    is a pointer to INPUT+FILCMP.  I can't do that because if the
			    FCB isn't initialized that will be garbage, and I need the real
			    address to do the validity check}
			    with FCP^ do
(* 264 - files are always external *)
(* 274 - VLEV is only meaningful for VARS *)
			      if (KLASS = VARS) and (VLEV = 0) then
				begin
				  INCREMENTREGC;
				  MACRO3R(OP_XMOVEI,REGC,CHANTAB[CHANNEL]);
				  CHANTAB[CHANNEL] := IC-1;
				  CODE.INFORMATION[CIX] := 'E';
				  with GATTR do
				    begin
				      KIND := VARBL;  DPLMT := 0; INDEXR:=REGC;
				      INDBIT:=0; VRELBYTE := NO
				    end
				end
			      else
				if INDBIT = 0 then
				  begin
				    DPLMT := DPLMT + FILCMP;
				    LOADADDRESS;
				  end
				else
				  begin
				    LOADADDRESS;
				    MACRO3(OP_ADDI,REGC,FILCMP)
				  end;
			    if RUNTMCHECK then
			      begin
				{See if the file is open.  A magic value of 314157 is left in FILTST if so }
				MACRO4(OP_MOVE,TAC0,REGC,FILTST-FILCMP);
				MACRO3(OP_CAIE,TAC0,314157B);
				SUPPORT(FILEUNINITIALIZED)
			      end
			  end
			else
			  if FORM = POINTER then
			    begin
			      TYPTR := ELTYPE;
			      if TYPTR # nil then
				with GATTR do
				  begin
				    LOADNOPTR := FALSE;
				    LOAD(GATTR); LOADNOPTR := TRUE;
				    if RUNTMCHECK then
				      begin
{At the moment NIL_VALUE = 0, so only one test is needed}
					{MACRO3(OP_CAIE,REG,0);}
					MACRO3(OP_CAIN,REG,NIL_VALUE);
					SUPPORT(BADPOINT)
				      end;
				    INDEXR := REG; DPLMT := 0; INDBIT:=0;
				    PACKFG := NOTPACK; KIND := VARBL;
				    VRELBYTE:= NO
				  end
			    end
			  else
			    ERROR(407);
		    INSYMBOL
		  end;
	      IFERRSKIP(166,FSYS or SELECTSYS)
	    end %WHILE\;
	  with GATTR do
	    if TYPTR#nil then
	      if TYPTR^.SIZE = 2 then
		begin
		  if INDBIT = 1 then
		    GETPARADDR;
		  if (INDEXR>REGIN) and (INDEXR<=REGCMAX) then
		    INCREMENTREGC
		end
	end %SELECTOR\;
      procedure CALL(FSYS: SETOFSYS; FCP: CTP);
	var
	  LKEY: 1..45;
	  LFOLLOWERROR, NORIGHTPARENT : BOOLEAN;

	procedure GETFILENAME(DEFAULTFILE:CTP;
			      TEXTPROC:BOOLEAN;
			      var FILETYPE:STP;
			      var GOTARG:BOOLEAN;
			      CHECK:BOOLEAN);
	  var
	    GOTFILE : BOOLEAN;  FILEREGC: ACRANGE;
	    {When we are finished we will have loaded a file into REGC, and parsed
	    the next parameter if there is one, using EXPRESSION with REGC incremented}
	  begin
	    INCREMENTREGC;  {by default we will load into 3}
	    FILEREGC := REGC;  {but file goes into 2, which this still is}
	    {REGC = 2}
	    GOTARG := FALSE; NORIGHTPARENT := TRUE; GOTFILE := FALSE;
	    if SY = LPARENT then
	      begin
		NORIGHTPARENT := FALSE;
		INSYMBOL;
		EXPRESSION(FSYS or [COMMA,RPARENT,COLON],ONFIXEDREGC);
		{REGC = 3 if expression (file can't be), 2 otherwise}
		GOTFILE := FALSE;
		{We have an expression, see if it is a legal file.  If so, load it into
		REGC (note: no incrementregc first) and do a few tests.  We have to do
		our own loading mostly to avoid the INCREMENTREGC done by LOADADDRESS}
		with GATTR do
		  if TYPTR <> nil then
		    with TYPTR^ do
		      if FORM = FILES then
			begin
			  if TEXTPROC then
			    if not (COMPTYPES(FILTYPE,CHARPTR)) then
			      ERROR(366);
			  {Yes, it is a legal file.  Now load it}
			  {If TTY that is supposed to be mapped to TTYOUTPUT, handle that}
{There ought to be an easier way.  What we are trying to do here is to
 figure out whether the expression was TTY.  Unfortunately, syntax analysis
 has already been done, so we have to figure this out from the semantics.
 Obvious things don't work.  E.g. we can't check at the beginning to see
 whether the first symbol is TTY, because it might be TTY^, which is
 something quite different.  Nor can we, as Hamburg did, look to see whether
 the most recent file seen was TTY, because it might be X[TTY^].  So we
 do it the hard way by decoding the semantics.}
			  if DEFAULTFILE = OUTFILE then
			    if EXTERNCTP <> nil then
			      if EXTERNCTP = TTYFILE then
			        begin
			          EXTERNCTP := TTYOUTFILE;
				  DPLMT := TTYOUTFILE^.VADDR
			        end
			      else {not TTY, nothing to do}
			    else if (DPLMT = TTYFILE^.VADDR) and 
				    (INDEXR = 0) and
				    (VRELBYTE in [RIGHT,LOWSEG]) and
				    (INDBIT = 0) then
			      DPLMT := TTYOUTFILE^.VADDR;
			  FETCHBASIS(GATTR);
			  MACRO(VRELBYTE,OP_XMOVEI,REGC,INDBIT,INDEXR,DPLMT);
			  KIND := VARBL;  DPLMT := 0; INDEXR:=REGC;
			  INDBIT:=0; VRELBYTE := NO;
		          if EXTERNCTP <> NIL
		            then 
			      begin
				EXTERNCTP^.VADDR := IC - 1;
				CODE.INFORMATION[CIX] := 'E'
			      end;
			  GOTFILE := TRUE;
			  FILETYPE := TYPTR;
			  if RUNTMCHECK and CHECK then
			    begin
			      MACRO4(OP_MOVE,TAC0,REGC,FILTST);  {File test word}
			      MACRO3(OP_CAIE,TAC0,314157B);  {True if file is open}
			      SUPPORT(FILEUNINITIALIZED)   {Not open}
			    end;
			  {Now see if there is an arg}
			  if SY <> RPARENT then
			    begin
			      if SY = COMMA then
				INSYMBOL
			      else
				ERROR(158);
			      {Note that this is guaranteed not to change REGC unless it sees an
			      expression, in which case it advances to 3.  We can't have two
			      advances (i.e. due to the EXPRESSION above and this one), since
			      this is done only if the one above saw a file, which can't have
			      advanced REGC}
			      EXPRESSION(FSYS or [COMMA,RPARENT,COLON],ONFIXEDREGC);
			      GOTARG := TRUE
			    end
			end;
		{Now we are done processing a file arg}
		if not GOTFILE then
		  {If expression wasn't a file, use it as arg}
		  GOTARG := TRUE
	      end {IF RPARENT};
	    {At this point REGC = 2 unless what we saw was an expr (which a file
	    can't be), in which case REGC = 3 and it is loaded}
	    if not GOTFILE then
	      with DEFAULTFILE^ do
		begin
		  {If we didn't get a file above, here is the code to do it}
		  MACRO3R(OP_XMOVEI,FILEREGC,VADDR);
		  if not GOTARG then
		    with GATTR do
		      begin
			KIND := VARBL;  DPLMT := 0; INDEXR:=REGC;
			INDBIT:=0; VRELBYTE := NO
		      end;
(* 264 - files are always extern *)
(* 274 - VLEV is only meaningful for VARS *)
		  if KLASS = VARS then
		    if VLEV=0 then
		      begin
		        VADDR:=IC-1; CODE.INFORMATION[CIX]:='E'
		      end;
		  FILETYPE := IDTYPE;
		  if RUNTMCHECK and CHECK then
		    begin
		      MACRO4(OP_MOVE,TAC0,FILEREGC,FILTST);  {File test word}
		      MACRO3(OP_CAIE,TAC0,314157B);  {True if file is open}
		      SUPPORT(FILEUNINITIALIZED);   {Not open}
		    end
		end
		  {If we saw an arg, REGC is exactly like it would have been with a
		  simple   INCREMENTREGC;  EXPRESSION;  which is the whole point.
		  That is,it is 2 unless an expression was seen, in which case the
		  expression is loaded into 3.  If we didn't see an expression, then
		  REGC is guaranteed to be 2.  Very shady...}
	  end %GETFILENAME\;

	procedure VARIABLE(FSYS: SETOFSYS);
	  var
	    LCP: CTP;
	  begin
	    if SY = IDENT then
	      begin
		IDSEARCH([VARS,FIELD],LCP,0); INSYMBOL
	      end
	    else
	      begin
		ERROR(209); LCP := UVARPTR
	      end;
	    SELECTOR(FSYS,LCP)
	  end %VARIABLE\;

(* 230 - possible AC allocation problem in use of VARIABLE; LOADADDRESS *)

{FixedVariable is a version of Variable intended for use in runtimes,
 where the address of a variable has to be loaded into a fixed AC.  It is
 the counterpart of a call to Expression with ONFIXEDREGC.  That is,
 it makes sure that REGC is left the same as it was before the call
 to VARIABLE.  (Selector can increment it, which is the only reason
 this is needed.)  The code plays games that match those in LOADADDRESS
 and MAKECODE, so that LOADADDRESS following this will load the address
 of the variable into the next AC.  Most of the code here is taken
 from the end of EXPRESSION, and is explained completely there.}

	procedure FIXEDVARIABLE(FSYS: SETOFSYS);
	    var TESTREGC:ACRANGE;
	  begin
	    TESTREGC := REGC+1;
	    VARIABLE(FSYS);
	    if GATTR.TYPTR # nil then
	      with GATTR,TYPTR^ do
  	        if (PACKFG = PACKK) and (BPADDR>REGIN)
		    and (BPADDR<=REGCMAX) then
	          if (INDEXR <= REGIN) or (BPADDR<INDEXR) then
	            begin
	              if BPADDR<> TESTREGC then
		        begin
		          MACRO3(OP_MOVE,TESTREGC,BPADDR);
		          BPADDR := TESTREGC
		        end
	            end
	          else
		    begin
		      if INDEXR<>TESTREGC then
		        begin
		          MACRO3(OP_MOVE,TESTREGC,INDEXR);
		          INDEXR := TESTREGC
		        end
		    end
	        else
	          if (INDEXR>REGIN) and (INDEXR<=REGCMAX)
		       and (INDEXR<>TESTREGC) then
		    begin
		      MACRO3(OP_MOVE,TESTREGC,INDEXR);
		      INDEXR := TESTREGC
		    end;
	    REGC := TESTREGC - 1;
	  end;

	procedure GETFN(TEST:BOOLEAN);
	  begin
(* 230 - possible AC allocation problem with VARIABLE *)
	    FIXEDVARIABLE(FSYS or [RPARENT,COLON,COMMA]);
	    LOADADDRESS;
	    if GATTR.TYPTR#nil then
	      if GATTR.TYPTR^.FORM#FILES then
		ERROR(212)
	      else
		if GATTR.EXTERNCTP <> nil then
		  begin
		    GATTR.EXTERNCTP^.VADDR := IC - 1;
		    CODE.INFORMATION[CIX] := 'E'
		  end;
	    (* internal files *)
	    if TEST and RUNTMCHECK then
	      begin
		MACRO4(OP_MOVE,TAC0,REGC,FILTST);  {File test word}
		MACRO3(OP_CAIE,TAC0,314157B);  {Magic value if it is open}
		SUPPORT(FILEUNINITIALIZED)   {Not open}
	      end
	  end;
	procedure GETPUTRESETREWRITE;
	  var
	    LMAX,LMIN: INTEGER;
	    LATTR: ATTR;
	    ADR : SUPPORTS;
	    DEFAULT : array [1..6] of BOOLEAN;
	    I,J : INTEGER;

	  procedure GETSTRINGADDRESS;

	    var
	      LMAX,LMIN: INTEGER;
	      FLAGBITS: packed record case BOOLEAN of
					   TRUE: (DUM:0..777777B;USETTY:BOOLEAN;WILDOK:BOOLEAN);
					   FALSE: (DUM2:0..777777B; RH:0..777777B)
			       end;
	    begin
	      if SY=COMMA then
		begin
		  INSYMBOL;
		  EXPRESSION(FSYS or [COMMA,RPARENT,COLON],ONFIXEDREGC);
		  with GATTR do
		    if TYPTR#nil then
		      with TYPTR^ do
			if(FORM=ARRAYS) and ARRAYPF then
			  if COMPTYPES(AELTYPE,CHARPTR) then
			    begin
			      DEFAULT[I] := FALSE;
			      I := I + 1;  DEFAULT[I] := FALSE;
			      LOADADDRESS;
			      INCREMENTREGC;
			      LOADSIZE(REGC,TYPTR);
			    end
			  else
			    ERROR(212)
			else
			  ERROR(212);
		  if (SY=COLON) then
		    begin
		      INSYMBOL;
		      FLAGBITS.RH := 0;
		      while SY in [RELOP,ADDOP,MULOP] do
			begin
			  if OP = LEOP then   (* @ *)
			    FLAGBITS.USETTY := TRUE
			  else
			    if (OP = MUL) and (not TOPS10) then
			      FLAGBITS.WILDOK := TRUE
			    else
			      ERROR(158);
			  INSYMBOL
			end;
		      (* pass flags in reg 4 with length, not 3 with address *)
		      MACRO3(OP_HRLI,REGC,FLAGBITS.RH)
		    end
		end
	    end {getstringaddress};

	  begin {getputresetrewrite}
(* 230 - possible AC allocation problem with VARIABLE *)
	    FIXEDVARIABLE(FSYS or [RPARENT,COMMA]);
	    LOADADDRESS;
	    LATTR := GATTR;
	    if GATTR.TYPTR # nil then
	      if GATTR.TYPTR^.FORM # FILES then
		ERRANDSKIP(458,FSYS or [RPARENT])
	      else
		begin
		  if GATTR.EXTERNCTP <> nil then
		    begin
		      GATTR.EXTERNCTP^.VADDR := IC - 1;
		      CODE.INFORMATION[CIX] := 'E'
		    end;
		  if (LKEY>=5) and (LKEY#28) then
		    begin
		      for I := 1 to 6 do DEFAULT[I] := TRUE;
		      I := 1;
		      GETSTRINGADDRESS % OF FILENAME \;
		      while not DEFAULT[I] and (SY=COMMA) do
			begin
			  I := I + 1;  INSYMBOL;
			  (* OPTION STRING AS 3RD ARG *)
			  if I = 3 then
			    begin
			      EXPRESSION(FSYS or [COMMA,RPARENT],ONFIXEDREGC);
			      with GATTR do
				if TYPTR#nil then
				  with TYPTR^ do
				    if(FORM=ARRAYS) and ARRAYPF then
				      if COMPTYPES(AELTYPE,CHARPTR) then
					begin
					  DEFAULT[3] := FALSE;
					  LOADADDRESS;
					  with CODE.INSTRUCTION[CIX] do
					    if (INSTR = OP_MOVE) or (INSTR = OP_XMOVEI) then
					      AC := BIXREG
					    else
					      MACRO3(OP_MOVE,BIXREG,REGC);
					  LOADSIZE(REGC,TYPTR);
					  MACRO3(OP_HRLZ,REGC,REGC);
					end
				      else
					ERROR(212)  {not CHAR array}
				    else
				      begin  {not packed array}
					LOAD(GATTR); DEFAULT[3] := FALSE
				      end
			    end {I=3}
			      (* ONLY TOPS10 HAS XBLOCK ARG *)
			  else
			    if (not TOPS10) or (I # 4) or ((SY=INTCONST)and(VAL.IVAL=0)) then
			      begin
				EXPRESSION(FSYS or [COMMA,RPARENT],ONFIXEDREGC);
				if GATTR.TYPTR#nil then
				  begin
				    LOAD(GATTR); DEFAULT[I] := FALSE;
				    (* allow sets, since they are elegant for specifying bits *)
				    if GATTR.TYPTR^.FORM = POWER then
				      REGC := REGC-1
				  end
			      end
			    else
			      begin
(* 230 - possible AC allocation problem with VARIBLE *)
				FIXEDVARIABLE(FSYS or[COMMA,RPARENT]);
				if GATTR.TYPTR # nil then
				  if not (GATTR.TYPTR^.FORM in [ARRAYS,RECORDS]) then
				    ERROR(264)
				  else
				    if GATTR.TYPTR^.SIZE<5 then
				      ERROR(265)
				    else
				      begin
					LOADADDRESS; DEFAULT[I]:=FALSE
				      end
				else
				  ERROR(458)
			      end;
			end;
		      for I := 1 to 6 do
			if DEFAULT[I] then
			  begin
			    INCREMENTREGC;  
			    if TOPS10 and (I=6)
			      then MACRO3(OP_SETO,REGC,0)
			      else MACRO3(OP_SETZ,REGC,0)
			  end;
		    end;
		  (* internal files *)
		  if LKEY in [5,6,29,36] then  {openning}
		    begin
		      if LATTR.TYPTR <> nil then
			if LATTR.TYPTR^.FORM = FILES then
			  if COMPTYPES(LATTR.TYPTR^.FILTYPE,CHARPTR) then
			    {In AC1, put size of component, or 0 if text file}
			    MACRO3(OP_SETZ,TAC1,0)
			  else
			    MACRO3C(OP_MOVE,TAC1,LATTR.TYPTR^.FILTYPE^.SIZE)
			    {Normally we would have to type filtype^ for nil, but if it is nil, the
			    comptypes above will succeed, and this code will never happen.}
		    end
		      (* don't validty check for DISMISS *)
		  else
		    if RUNTMCHECK and (LKEY <> 28) then
		      begin
			MACRO4(OP_MOVE,TAC0,REGIN+1,FILTST);{File test word}
			MACRO3(OP_CAIE,TAC0,314157B); {Magic value if open}
			SUPPORT(FILEUNINITIALIZED);   {Not open}
		      end;
		  case LKEY of
		    2: ADR:= GETLINE;
		    4: ADR:= PUTLINE;
		    5: ADR:= RESETFILE;
		    6: ADR:= REWRITEFILE;
		    27:ADR:= NAMEFILE;
		    28:ADR:= DISFILE;
		    29:ADR:= UPFILE;
		    36:ADR:= APFILE
		  end;
		  SUPPORT(ADR)
		end
	  end;
	  (* SETSTRING, TO ALLOW I/O TO STRINGS *)
	procedure SETSTRING;
	  var
	    CINDEX,LREGC:ACRANGE;
	    LMIN,LMAX:XADDRRANGE;
	    CONF:BOOLEAN;
	    ARRAY1,OFFSET,FILEP,LIMIT:ATTR;
	    NOOFF,NOLIM: BOOLEAN;

(* 232 - Tops-10 Changes have been made throughout this routine *)
{The following variables are offsets into the file control block.
 They are variables because the offsets are different for Tops-10
 and Tops-20.
  BCT_LOC - FILBCT
  BPT_LOC - FILBPT
  LIM_LOC - the place where we put the upper bound of the string,
    FILBFH on Tops-20 and FILST2 on Tops10}

	    BCT_LOC,BPT_LOC,LIM_LOC: INTEGER;

	  begin
	    if TOPS10 then
	      begin BCT_LOC := FILBCT10; BPT_LOC := FILBPT10; 
		    LIM_LOC := FILST210 end
	    else begin BCT_LOC := FILBCT; BPT_LOC := FILBPT;
		    LIM_LOC := FILBFH end;
	    LREGC := REGC;  NOOFF := FALSE;  NOLIM:=FALSE;
	    GETFN(FALSE);
	    {If the file block is not legal yet, call routine to make it so}
	    MACRO4(OP_MOVE,TAC0,REGC,FILTST);  {File test word}
	    MACRO3(OP_CAIE,TAC0,314157B);  {Magic value if it is open}
	    SUPPORT(INITFILEBLOCK);
	    FILEP := GATTR;
	    if SY = COMMA then
	      INSYMBOL
	    else
	      ERROR(158);
	    VARIABLE(FSYS or [RPARENT,COMMA]);
	    LOADADDRESS;
	    with GATTR do
	      begin
		KIND := EXPR; REG := INDEXR;
		if TYPTR # nil then
		  with TYPTR^ do
		    if FORM # ARRAYS then
		      ERROR(458)
		    else
		      if FILEP.TYPTR#nil then
			if not ARRAYPF then
			  ERROR(458)
	      end;
	    ARRAY1 := GATTR;
	    if SY = RPARENT then
	      NOOFF := TRUE
	    else
	      if SY = COMMA then
		begin
		  INSYMBOL;
		  EXPRESSION(FSYS or [RPARENT,COMMA],ONREGC);
		  if GATTR.TYPTR # nil then
		    if GATTR.TYPTR^.FORM # SCALAR then
		      ERROR(458)
		    else
		      if not COMPTYPES(ARRAY1.TYPTR^.INXTYPE,GATTR.TYPTR) then
			ERROR(458);
		  OFFSET := GATTR;
		  if OFFSET.KIND = EXPR then
		    INCREMENTREGC
		end
	      else
		ERROR(158);
	    if SY = RPARENT then
	      NOLIM := TRUE
	    else
	      if SY = COMMA then
		begin
		  INSYMBOL;
		  EXPRESSION(FSYS or [RPARENT],ONREGC);
		  if GATTR.TYPTR # nil then
		    if GATTR.TYPTR^.FORM # SCALAR then
		      ERROR(458)
		    else
		      if not COMPTYPES(ARRAY1.TYPTR^.INXTYPE,GATTR.TYPTR) then
			ERROR(458);
		  LIMIT := GATTR;
		  if LIMIT.KIND = EXPR then
		    INCREMENTREGC
		end
	      else
		ERROR(158);
	    if not ERRORFLAG then
	      begin
		CONF := FALSE;
		if ARRAY1.TYPTR^.INXTYPE <> NIL then
		  with ARRAY1.TYPTR^.INXTYPE^ do
		    if ARRAY1.TYPTR^.INXTYPE^.FORM = CONFORMANT then
		      begin
			CONF := TRUE;
			LMIN := LOWER^.VADDR;
			LMAX := UPPER^.VADDR;
		      end
		    else
		      GETBOUNDS(ARRAY1.TYPTR^.INXTYPE,LMIN,LMAX);
		if not NOLIM then
		  begin
		    if LIMIT.KIND # EXPR then
		      begin
			LOAD(LIMIT); INCREMENTREGC
		      end;
{Here we adjust LIMIT to be with respect to the lower bound.  So (LIMIT) + 1
 is the number of characters in the array we are allowed to look at.  Note
 that OFFSET is not yet taken into account.  Since LMAX has been similarly
 adjusted, we can compare (LIMIT) with LMAX to see whether the user has
 asked for a limit outside the array}
		    SUBLOWBOUND(LIMIT.REG,ARRAY1.TYPTR^.INXTYPE,CINDEX,
			LMIN,LMAX);
		  end;
{Now we offset and bounds-check OFFSET.  (OFFSET) = 0 means we start at
f the beginning of the array.  Note we allow an OFFSET that is one too large,
 to let the user force immediate end of file.}
		if not NOOFF then
		  begin
		    if OFFSET.KIND # EXPR then
		      begin
			LOAD(OFFSET); INCREMENTREGC
		      end;
		    SUBLOWBOUND(OFFSET.REG,ARRAY1.TYPTR^.INXTYPE,CINDEX,
			LMIN,LMAX);
		    INCREMENTREGC;
{Now we set FILBCT to the number of char's he has asked for.  This is
 (LIMIT) + 1 - (OFFSET).}
		    if NOLIM then
		      begin
			LOADSIZE(REGC,ARRAY1.TYPTR);
			if OFFSET.REG <> 0 then
			  MACRO3(OP_SUB,REGC,OFFSET.REG);
		      end
		    else
		      begin
			MACRO3(OP_MOVE,REGC,LIMIT.REG);
			if OFFSET.REG <> 0 then
			  MACRO3(OP_SUB,REGC,OFFSET.REG);
			MACRO3(OP_ADDI,REGC,1);
			if RUNTMCHECK then
			  begin
			    MACRO3(OP_CAIGE,REGC,0);  SUPPORT(INDEXERROR)
			  end
		      end;
		    MACRO4(OP_MOVEM,REGC,FILEP.INDEXR,BCT_LOC);
{FILBIX - base address of the array}
		    MACRO4(OP_MOVEM,ARRAY1.REG,FILEP.INDEXR,FILBIX);
{FILBPT - pointer to first element to use - start adjusted by OFFSET}
		    MACRO3R(OP_HLLZ,REGC,ARRAY1.TYPTR^.ARRAYBPADDR);
		    if KLCPU then
		      begin
			MACRO3(OP_ADJBP,OFFSET.REG,REGC);
			MACRO4(OP_MOVEM,OFFSET.REG,FILEP.INDEXR,BPT_LOC)
		      end
		    else
		      begin
			MACRO3(OP_IDIVI,OFFSET.REG,BITMAX div ARRAY1.TYPTR^.AELTYPE^.BITSIZE);
			MACRO3(OP_HRR,REGC,OFFSET.REG);
			MACRO3(OP_CAILE,OFFSET.REG+1,0);
			MACRO3(OP_IBP,0,REGC);
			MACRO3R(OP_SOJG,OFFSET.REG+1,IC-1);
			MACRO4(OP_MOVEM,REGC,FILEP.INDEXR,BPT_LOC)
		      end
		  end
		else
		  begin
{Same calculations without an OFFSET}
		    INCREMENTREGC;
{FILBCT - LMAX + 1 or (LIMIT) + 1}
		    if NOLIM
		    then
		      LOADSIZE(REGC,ARRAY1.TYPTR)
		    else
		      begin
		        MACRO3(OP_MOVE,REGC,LIMIT.REG);
			MACRO3(OP_ADDI,REGC,1);
		      end;
		    MACRO4(OP_MOVEM,REGC,FILEP.INDEXR,BCT_LOC);
{FILBIX - base address of array}
		    MACRO4(OP_MOVEM,ARRAY1.REG,FILEP.INDEXR,FILBIX);
{FILBPT - start of array, as no offset}
		    MACRO3R(OP_MOVE,REGC,ARRAY1.TYPTR^.ARRAYBPADDR);
		    MACRO4(OP_MOVEM,REGC,FILEP.INDEXR,BPT_LOC)
		  end;
		if NOLIM then
		  OFFBOUND(REGC,ARRAY1.TYPTR,CINDEX,LMAX,1)
		else
		  begin
		    OFFBOUND(REGC,ARRAY1.TYPTR,CINDEX,LMIN,1);
		    MACRO3(OP_ADD,REGC,LIMIT.REG)
		  end;
		MACRO4(OP_MOVEM,REGC,FILEP.INDEXR,LIM_LOC);
		case LKEY of
		  22: SUPPORT(RESETSTRING);
		  23: SUPPORT(REWRITESTRING)
		  end
	      end;
	    REGC := LREGC
	  end {SETSTRING};

	procedure GETINDEX;
	  var
	    LREGC:ACRANGE;
	    FILEP:ATTR;
	  begin
	    LREGC := REGC;  GETFN(TRUE);
	    FILEP := GATTR;
	    if SY = COMMA then
	      INSYMBOL
	    else
	      ERROR(158);
	    VARIABLE(FSYS or [RPARENT]);
	    LOADADDRESS;
	    with GATTR do
	      if TYPTR # nil then
		with TYPTR^ do
		  if (FORM # SCALAR) and (FORM # SUBRANGE) then
		    ERROR(458);
	    if not ERRORFLAG then
	      begin
		INCREMENTREGC;
		with FILEP do
		  begin
(* 232 - fix string I/O in Tops-10 *)
		    if TOPS10 then
		      begin
		      MACRO4(OP_MOVE,REGC,INDEXR,FILST210);
		      MACRO4(OP_SUB,REGC,INDEXR,FILBCT10);
		      end
		    else
		      begin
		      MACRO4(OP_MOVE,REGC,INDEXR,FILBFH);
		      MACRO4(OP_SUB,REGC,INDEXR,FILBCT);
		      end;
		    MACRO4(OP_MOVEM,REGC,GATTR.INDEXR,0)
		  end
	      end;
	    REGC := LREGC
	  end;
	procedure READREADLN;
	  var
	    LADDR : SUPPORTS;  LMIN,LMAX:INTEGER; LATTR:ATTR;
	    READREC: BOOLEAN; LREGC: ACRANGE;
	    {This procedure is complicated by a number of special cases.  The first is
	    the question of whether the file is text or binary.  The code for a binary
	    file is more or less completely different.  (Note also that only READLN
	    is not legal for a binary file.)  The second question is whether the
	    address is passed to the runtimes or whether they return a value.  For
	    binary files we must pass the address of the variable to be filled, since
	    it can be arbitrarily big.  Similarly for strings.  For simple values,
	    the runtimes return the value in AC 3, and we must do a store.  This is
	    to allow for storing into packed objects (what kind of address could be
	    pass for that?)  We do LOADADDRESS for binary files and strings, and
	    for simple objects we do STORE afterwards.}
	  begin
	    if LKEY = 7 then  {read?}
	      GETFILENAME(INFILE,FALSE,THISFILE,GOTARG,TRUE)  {might be binary}
	    else
	      GETFILENAME(INFILE,TRUE,THISFILE,GOTARG,TRUE);
	    {must be text}
	    if (LKEY = 7) and not GOTARG then
	      ERROR(554);
	    {READ must have args}
	    READREC := FALSE;   {now see if a binary file}
	    if LKEY = 7 then
	      if not COMPTYPES(CHARPTR,THISFILE^.FILTYPE) then
		READREC := TRUE;
	    LREGC := REGC;
	    if GOTARG then
	      loop
		CHECKASSIGN(GATTR);
		LATTR := GATTR;
		if READREC then
		  begin {separate code for binary files}
		    LADDR := READRECORD;
		    if GATTR.TYPTR#nil then
		      if not COMPTYPES(THISFILE^.FILTYPE,GATTR.TYPTR) then
			ERROR(260);
		    LOADADDRESS
		  end
		else
		  begin  {Here is the code for TEXT files}
		    LADDR := READCHARACTER;
		    if GATTR.TYPTR#nil then
		      if GATTR.TYPTR^.FORM<=SUBRANGE then
			if COMPTYPES(INTPTR,GATTR.TYPTR) then
			  LADDR := READINTEGER
			else
			  if COMPTYPES(REALPTR,GATTR.TYPTR) then
			    LADDR := READREAL
			  else
			    if COMPTYPES(CHARPTR,GATTR.TYPTR) then
			      LADDR := READCHARACTER
			    else
			      ERROR(169)
		      else
			with GATTR.TYPTR^ do
			  if FORM = ARRAYS then
			    if COMPTYPES(CHARPTR,AELTYPE) then
			      begin
				(* read into packed objects *)
				LOADADDRESS;  {of array}
				INCREMENTREGC;
				LOADSIZE(REGC,GATTR.TYPTR);
				if ARRAYPF then
				  LADDR := READPACKEDSTRING
				else
				  LADDR := READSTRING;
				if SY = COLON then
				  begin
				    INSYMBOL;
				    (* allow set of break characters *)
(* 230 - possible AC alloction problem with VARIABLE *)
				    FIXEDVARIABLE(FSYS or [COMMA,RPARENT,COLON]);
				    CHECKASSIGN(GATTR);
				    LOADADDRESS;
				    if not COMPTYPES(INTPTR,GATTR.TYPTR) then
				      ERROR(458)
				  end
				else
				  begin
				    INCREMENTREGC;
				    MACRO3(OP_SETZ,REGC,0)
				  end;
				if SY = COLON then
				  begin
				    INSYMBOL;
				    EXPRESSION(FSYS or [COMMA,RPARENT],ONFIXEDREGC);
				    if GATTR.TYPTR # nil then
(* 235 - set of char *)
				      if (GATTR.TYPTR^.FORM = QPOWER) then
					if COMPTYPES(GATTR.TYPTR^.ELSET,CHARPTR) then
					  begin
					    LOADADDRESS;  REGC := REGC-1
					  end
					else
					  ERROR(458)
				      else
					ERROR(458)
				  end {SY = COLON}
				else
				  MACRO3(OP_SETZ,REGC+1,0)
			      end
			    else
			      ERROR(458)
			  else
			    ERROR(458);
		  end;
		{of TEXT file case}
		REGC := LREGC;
		if not (READREC or (LADDR in [READSTRING,READPACKEDSTRING])) then
		  begin
		    {This is for reading single words, which may go into packed structures.
		    Note that we have to redo the ac allocation because the read routine
		    will return a value in AC 3, which quite likely is used as INDEXR or
		    BPADDR.  Since we are pushing the active AC's anyway, we might as well
		    pop them back into a different place.}
		    INCREMENTREGC;  {place that read will return the value}
		    if (LATTR.INDEXR > REGIN) and (LATTR.INDEXR <= 10B) then
		      begin
			MACRO3(OP_PUSH,TOPP,LATTR.INDEXR);
			INCREMENTREGC;
			LATTR.INDEXR := REGC  {Place to put this value afterwards}
		      end;
		    if (LATTR.PACKFG = PACKK) and (LATTR.BPADDR > REGIN)
		      and (LATTR.BPADDR <= 10B) then
		      begin
			MACRO3(OP_PUSH,TOPP,LATTR.BPADDR);
			INCREMENTREGC;
			LATTR.BPADDR := REGC
		      end;
		    REGC := LREGC;  {RESTORE REGC}
		    SUPPORT(LADDR);
		    if (LATTR.PACKFG = PACKK) and (LATTR.BPADDR > REGIN)
		      and (LATTR.BPADDR <= 10B) then
		      MACRO3(OP_POP,TOPP,LATTR.BPADDR);
		    if (LATTR.INDEXR > REGIN) and (LATTR.INDEXR <= 10B) then
		      MACRO3(OP_POP,TOPP,LATTR.INDEXR);
		    FETCHBASIS(LATTR);   {NOW DO THE STORE}
		    STORE(REGC+1,LATTR)
		  end
		else
		  SUPPORT(LADDR);
	      exit if SY # COMMA;
		INSYMBOL;
		VARIABLE(FSYS or [COMMA,COLON,RPARENT])
	    end;
	    if LKEY = 8 then
	      SUPPORT(GETLINE)
	  end %READREADLN\;
	procedure PUTX;
	  begin
	    GETFN(TRUE);
	    case LKEY of
	      37: SUPPORT(PUTXFILE);
	      41: SUPPORT(DELFILE)
	      end
	  end;

	procedure BREAK;
	  begin
	    GETFILENAME(OUTFILE,FALSE,THISFILE,GOTARG,TRUE);
	    if GOTARG then
	      ERROR(554);
	    SUPPORT(BREAKOUTPUT);
	  end;

	procedure CLOSE;
	  begin
	    if (LKEY = 25) or (LKEY = 42) then
	      GETFILENAME(OUTFILE,FALSE,THISFILE,GOTARG,FALSE)
	    else
	      GETFILENAME(INFILE,FALSE,THISFILE,GOTARG,FALSE);
	    if GOTARG then
	      LOAD(GATTR)
	    else
	      begin
		INCREMENTREGC;  MACRO3(OP_SETZ,REGC,0)
	      end;
	    case LKEY of
	      25: SUPPORT(CLOSEFILE);
	      34: SUPPORT(BREAKINPUT);
	      39: SUPPORT(NEXTBLOCKF);
	      42: SUPPORT(RELFILE)
	      end
	  end;

	procedure DUMP;
	  var
	    FILEP:ATTR;
	    S:INTEGER;
	  begin
	    GETFN(TRUE);  FILEP:=GATTR;
	    if SY=COMMA then
	      INSYMBOL
	    else
	      ERROR(158);
	    EXPRESSION(FSYS or[COMMA,RPARENT],ONFIXEDREGC);
	    LOADADDRESS;
	    if GATTR.TYPTR#nil then
	      S:=GATTR.TYPTR^.SIZE;
	    if SY=COMMA then
	      begin
		INSYMBOL;
		EXPRESSION(FSYS or [RPARENT],ONFIXEDREGC);
		if COMPTYPES(INTPTR,GATTR.TYPTR) then
		  LOAD(GATTR)
		else
		  ERROR(458);
		if RUNTMCHECK then
		  begin
		    MACRO3C(OP_CAMLE,REGC,S); SUPPORT(INDEXERROR)
		  end
	      end
	    else
	      begin
		INCREMENTREGC;
		MACRO3C(OP_MOVE,REGC,GATTR.TYPTR^.SIZE)
	      end;
	    if LKEY=30 then
	      SUPPORT(READDUMP)
	    else
	      SUPPORT(WRITEDUMP)
	  end;

	procedure USET;
	  var
	    FILEP:ATTR;
	  begin
	    GETFN(TRUE); FILEP:=GATTR;
	    if SY = COMMA then
	      INSYMBOL
	    else
	      ERROR(158);
	    EXPRESSION(FSYS or [COMMA,RPARENT],ONFIXEDREGC);
	    LOAD(GATTR);
	    if GATTR.TYPTR=nil then
	      ERROR(458)
	    else
	      if GATTR.TYPTR#INTPTR then
		ERROR(458);
	    if LKEY # 33 then
	      begin
		if SY=COMMA then
		  begin
		    INSYMBOL;
		    EXPRESSION(FSYS or [RPARENT],ONFIXEDREGC);
		    LOAD(GATTR);
		  end
		else
		  begin
		    INCREMENTREGC;
		    MACRO3(OP_SETZ,REGC,0)
		  end;
		case LKEY of
		  32:SUPPORT(SETIN);
		  38:SUPPORT(SETPOSF)
		  end
	      end
	    else
	      SUPPORT(SETOUT)
	  end;
	procedure WRITEWRITELN;
	  var
	    LSP: STP;
	    DEFAULT,REALFORMAT,WRITEOCT: BOOLEAN;
	    LSIZE,LMIN,LMAX: INTEGER;
	    LADDR: SUPPORTS;
	    WRITEREC: BOOLEAN;
	  begin
	    {First scan file name and see if binary file}
	    if LKEY = 10 then   {WRITE?}
	      GETFILENAME(OUTFILE,FALSE,THISFILE,GOTARG,TRUE)  {Yes, might be binary}
	    else
	      GETFILENAME(OUTFILE,TRUE,THISFILE,GOTARG,TRUE);
	    {No, WRITELN not legal for binary files}
	    if (LKEY = 10) and not GOTARG then
	      ERROR(554);
	    WRITEREC := FALSE;
	    if LKEY = 10 then   {Now see if it was a binary file}
	      if not COMPTYPES(CHARPTR,THISFILE^.FILTYPE) then
		WRITEREC := TRUE;
	    if GOTARG then
	      loop
		LSP := GATTR.TYPTR; LSIZE := LGTH; WRITEOCT := FALSE;
		if LSP # nil then
		  if LSP^.FORM <= POWER then
		    begin
		      {Note that the values of LADDR set here are used only for binary files.
		      LADDR is reset below for text files.  Only in case of error will these
		      values remain for a text file, and in that case having them prevents
		      an ill mem ref}
		      LOAD(GATTR); LADDR := WRITESCALAR
		    end
		  else
		    begin
		      if (GATTR.KIND = VARBL) and (GATTR.INDEXR = TOPP) then
			ERROR(458);
		      LOADADDRESS;  LADDR := WRITERECORD
		    end;
		if WRITEREC then
		  begin {For binary files, make sure of type match}
		    if GATTR.TYPTR#nil then
		      if not COMPTYPES(THISFILE^.FILTYPE,GATTR.TYPTR) then
			ERROR(260)
		  end  {end binary}
		else
		  begin
		    if SY = COLON then
		      begin
			INSYMBOL;
			EXPRESSION(FSYS or [COMMA,COLON,RPARENT],ONFIXEDREGC);
			if GATTR.TYPTR # nil then
			  if GATTR.TYPTR # INTPTR then
			    ERROR(458);
			LOAD(GATTR); DEFAULT := FALSE
		      end
		    else
		      begin
			DEFAULT := TRUE;
			INCREMENTREGC %RESERVE REGISTER FOR DEFAULT VALUE\
		      end;
		    if LSP = INTPTR then
		      begin
			LADDR := WRITEINTEGER; LSIZE := 12
		      end;
		    if SY = COLON then
		      begin
			INSYMBOL;
			if (SY = IDENT) and ((ID='O         ') or (ID='H         ')) then
			  begin
			    if not COMPTYPES(LSP,INTPTR) then
			      ERROR(262);
			    if ID = 'O         ' then
			      LADDR := WRITEOCTAL
			    else
			      begin
				LADDR := WRITEHEXADECIMAL; LSIZE := 11
			      end;
			    INSYMBOL
			  end
			else
			  begin
			    EXPRESSION(FSYS or [COMMA,RPARENT],ONFIXEDREGC);
			    if GATTR.TYPTR # nil then
			      if GATTR.TYPTR # INTPTR then
				ERROR(458);
			    if LSP # REALPTR then
			      ERROR(258);
			    LOAD(GATTR); REALFORMAT := FALSE
			  end
		      end
		    else
		      REALFORMAT := TRUE;
		    if LSP = INTPTR then
		      goto 1;
		    if LSP = CHARPTR then
		      begin
			LSIZE := 1; LADDR := WRITECHARACTER
		      end
		    else
		      if LSP = REALPTR then
			begin
			  LSIZE := 16; LADDR := WRITEREAL;
			  if REALFORMAT then
			    MACRO3(OP_MOVEI,REGIN+4,123456B);
			end
		      else
			if LSP = BOOLPTR then
			  begin
			    LSIZE := 6; LADDR := WRITEBOOLEAN
			  end
			else
			  if LSP # nil then
			    begin
			      if LSP^.FORM = SCALAR then
				ERROR(169)
			      else
				if STRING(LSP) then
				  begin
(* 240 - conformant *)
				    LOADSIZE(REGIN+4,LSP);
				    if DEFAULT then
				      begin
					MACRO3(OP_MOVE,REGIN+3,REGIN+4);
					DEFAULT := FALSE
				      end;
				    if LSP^.ARRAYPF then
				      LADDR := WRITEPACKEDSTRING
				    else
				      LADDR := WRITESTRING
				  end
				else
				  ERROR(458)
			    end;
	  1:
		    if DEFAULT then
		      MACRO3C(OP_MOVE,REGIN+3,LSIZE)
		  end;
		{of IF WRITEREC}
		SUPPORT(LADDR);
		REGC :=REGIN + 1;
	      exit if SY # COMMA;
		INSYMBOL;
		EXPRESSION(FSYS or [COMMA,COLON,RPARENT],ONFIXEDREGC);
	    end;
	    if LKEY = 11 then
	      SUPPORT(PUTLINE);
	  end %WRITE\;
	  (* PACK and UNPACK have been rewritten to be as described in Jensen and Wirth *)
	procedure PACK;

	  % PACK(A,I,Z) MEANS:
	   FOR L := LMIN(Z) TO LMAX(Z) DO Z[L] := A[L-LMIN(Z)+I] \

	  var
	    ARRAY1,OFFSET1,ARRAY2,OFFSET2: ATTR;
	    LADDR,START,STOP,LMIN1,LMAX1,LMIN2,LMAX2: XADDRRANGE;
(* 240 - conformant *)
	    CONF1,CONF2: BOOLEAN;
	    CINDEX, LREGC, LREGC2: ACRANGE;

	  begin
	    LREGC := REGC; START := 0;
	    VARIABLE(FSYS or [COMMA,RPARENT]);
	    LOADADDRESS;
	    with GATTR do
	      begin
		KIND := EXPR; REG := INDEXR;
		if TYPTR = nil then
		  TYPTR := UARRTYP
		else
		  with TYPTR^ do
		    if FORM # ARRAYS then
		      ERROR(458)
		    else
		      if ARRAYPF then
			ERROR(458)
	      end;
	    ARRAY1 := GATTR;
	    if SY = COMMA then
	      INSYMBOL
	    else
	      ERROR(158);
	    EXPRESSION(FSYS or [COMMA,RPARENT],ONREGC);
	    if GATTR.TYPTR # nil then
	      if GATTR.TYPTR^.FORM # SCALAR then
		ERROR(458)
	      else
		if not COMPTYPES(ARRAY1.TYPTR^.INXTYPE,GATTR.TYPTR) then
		  ERROR(458);
	    OFFSET1 := GATTR;
	    if SY = COMMA then
	      INSYMBOL
	    else
	      ERROR(158);
	    VARIABLE(FSYS or [RPARENT]);  LOADADDRESS;
	    with GATTR do
	      begin
		KIND := EXPR; REG := INDEXR;
		if TYPTR # nil then
		  with TYPTR^ do
		    if FORM # ARRAYS then
		      ERROR(458)
		    else
		      if not ARRAYPF
			or not (COMPTYPES(AELTYPE,ARRAY1.TYPTR^.AELTYPE)
				and  COMPTYPES(INXTYPE,ARRAY1.TYPTR^.INXTYPE)) then
			ERROR(458)
	      end;
	    ARRAY2 := GATTR;
	    if not ERRORFLAG then
	      begin
		with ARRAY1.TYPTR^ do
		  if INXTYPE # nil then
		    if INXTYPE^.FORM = CONFORMANT then
		      begin
		        CONF1 := TRUE;
			LMIN1 := INXTYPE^.LOWER^.VADDR;
			LMAX1 := INXTYPE^.UPPER^.VADDR
		      end
		    else
		      begin
			CONF1 := FALSE;
			GETBOUNDS(INXTYPE,LMIN1,LMAX1);
		      end;
		with ARRAY2.TYPTR^ do
		  if INXTYPE # nil then
		    if INXTYPE^.FORM = CONFORMANT then
		      begin
		        CONF2 := TRUE;
			LMIN2 := INXTYPE^.LOWER^.VADDR;
			LMAX2 := INXTYPE^.UPPER^.VADDR
		      end
		    else
		      begin
			CONF2 := FALSE;
			GETBOUNDS(INXTYPE,LMIN2,LMAX2);
		      end;

		if (OFFSET1.KIND = CST) and not CONF1 and not
			(RUNTMCHECK and CONF2) then
		  begin
		    STOP := LMAX2 - LMIN2 + 1; {This is junk if CONF2}
		    START := OFFSET1.CVAL.IVAL - LMIN1;
		    if not CONF2 then  {If CONF2 we can't check}
		      if (START < 0) or (START > (LMAX1+1-STOP)) then
		        ERROR(263);
		    INCREMENTREGC;  LREGC2 := REGC;
		    LOADSIZE(LREGC2,ARRAY2.TYPTR);
		  end
		else
		  begin
		    INCREMENTREGC;  LREGC2 := REGC;
		    LOADSIZE(LREGC2,ARRAY2.TYPTR);
		    LOAD(OFFSET1);
		    if CONF1 then
		      begin
			FASTBASIS(ARRAY1.TYPTR^.INXTYPE,CINDEX);
			if RUNTMCHECK then
			  begin
			  MACRO4(OP_CAML,OFFSET1.REG,CINDEX,LMIN1);
			  MACRO3(OP_MOVE,BIXREG,OFFSET1.REG);
			  MACRO3(OP_ADD,BIXREG,LREGC2);
			  MACRO3(OP_SUBI,BIXREG,1);
			  MACRO4(OP_CAMLE,BIXREG,CINDEX,LMAX1);
			  SUPPORT(INDEXERROR);
			  end;
			MACRO4(OP_SUB,OFFSET1.REG,CINDEX,LMIN1);
			MACRO3(OP_ADD,ARRAY1.REG,OFFSET1.REG);
		      end
		    else with OFFSET1 do
		      begin
			if LMIN1 > 0 then
			  MACRO3C(OP_SUB,REG,LMIN1)
			else
			  if LMIN1 < 0 then
			    MACRO3C(OP_ADD,REG,-LMIN1);
			if RUNTMCHECK then
			  begin
			    MACRO3C(OP_MOVE,BIXREG,LMAX1+1);
			    MACRO3(OP_SUB,BIXREG,OFFSET2.REG);
			    MACRO3(OP_CAIL,REG,0);
			    MACRO3(OP_CAMLE,REG,BIXREG);
			    SUPPORT(INDEXERROR)
			  end;
			MACRO3(OP_ADD,ARRAY1.REG,REG);
		      end
		  end;
		INCREMENTREGC;
		MACRO3(OP_MOVE,BIXREG,ARRAY2.REG);
		MACRO3R(OP_MOVE,REGC,ARRAY2.TYPTR^.ARRAYBPADDR);
		LADDR := IC;
		MACRO4(OP_MOVE,TAC0,ARRAY1.REG,START);
		MACRO3(OP_IDPB,TAC0,REGC);
		MACRO3(OP_AOS,0,ARRAY1.REG);
		MACRO3R(OP_SOJG,LREGC2,LADDR)
	      end;
	    REGC := LREGC
	  end {PACK};

	procedure UNPACK;

	  % UNPACK(Z,A,I) MEANS:
	   FOR L := LMIN(Z) TO LMAX(Z) DO A[L-LMIN(Z)+I] := Z[L] \

	  var
	    ARRAY1,OFFSET1,ARRAY2,OFFSET2: ATTR;
	    LADDR,START,STOP,LMIN1,LMAX1,LMIN2,LMAX2: XADDRRANGE;
(* 140 - conformant *)
	    CONF1,CONF2: BOOLEAN;
	    CINDEX, LREGC, LREGC2: ACRANGE;

	  begin
	    LREGC := REGC; START := 0;
	    VARIABLE(FSYS or [COMMA,RPARENT]);
	    LOADADDRESS;
	    with GATTR do
	      begin
		KIND := EXPR; REG := INDEXR;
		if TYPTR = nil then
		  TYPTR := UARRTYP
		else
		  with TYPTR^ do
		    if FORM # ARRAYS then
		      ERROR(458)
		    else
		      if not ARRAYPF then
			ERROR(458)
	      end;
	    ARRAY1 := GATTR;
	    if SY = COMMA then
	      INSYMBOL
	    else
	      ERROR(158);
	    VARIABLE(FSYS or [COMMA,RPARENT]);  LOADADDRESS;
	    with GATTR do
	      begin
		KIND := EXPR; REG := INDEXR;
		if TYPTR = nil then
		  TYPTR := UARRTYP
		else
		  with TYPTR^ do
		    if FORM # ARRAYS then
		      ERROR(458)
		    else
		      if ARRAYPF
			or not (COMPTYPES(AELTYPE,ARRAY1.TYPTR^.AELTYPE)
				and COMPTYPES(INXTYPE,ARRAY1.TYPTR^.INXTYPE)) then
			ERROR(458)
	      end;
	    ARRAY2 := GATTR;
	    if SY = COMMA then
	      INSYMBOL
	    else
	      ERROR(158);
	    EXPRESSION(FSYS or [RPARENT],ONREGC);
	    if GATTR.TYPTR # nil then
	      if GATTR.TYPTR^.FORM # SCALAR then
		ERROR(458)
	      else
		if not COMPTYPES(ARRAY2.TYPTR^.INXTYPE,GATTR.TYPTR) then
		  ERROR(458);
	    OFFSET2 := GATTR;
	    if not ERRORFLAG then
	      begin
(* 140 - conformant *)
		with ARRAY1.TYPTR^ do
		  if INXTYPE # nil then
		    if INXTYPE^.FORM = CONFORMANT then
		      begin
		        CONF1 := TRUE;
			LMIN1 := INXTYPE^.LOWER^.VADDR;
			LMAX1 := INXTYPE^.UPPER^.VADDR
		      end
		    else
		      begin
			CONF1 := FALSE;
			GETBOUNDS(INXTYPE,LMIN1,LMAX1);
		      end;
		with ARRAY2.TYPTR^ do
		  if INXTYPE # nil then
		    if INXTYPE^.FORM = CONFORMANT then
		      begin
		        CONF2 := TRUE;
			LMIN2 := INXTYPE^.LOWER^.VADDR;
			LMAX2 := INXTYPE^.UPPER^.VADDR
		      end
		    else
		      begin
			CONF2 := FALSE;
			GETBOUNDS(INXTYPE,LMIN2,LMAX2);
		      end;
		if (OFFSET2.KIND = CST) and not CONF2 and not
		      (CONF1 and RUNTMCHECK) then
		  begin
		    STOP := LMAX1 - LMIN1 + 1; {Junk if CONF1}
		    START := OFFSET2.CVAL.IVAL - LMIN2;
		    if not CONF1 then  {can't do test if CONF1}
		      if (START < 0) or (START > (LMAX2+1-STOP)) then
		      	ERROR(263);
		    INCREMENTREGC;  LREGC2 := REGC;
		    MACRO3C(OP_MOVE,LREGC2,STOP);
		  end
		else
		  begin
		    INCREMENTREGC; LREGC2 := REGC;
		    LOADSIZE(LREGC2,ARRAY1.TYPTR);
		    LOAD(OFFSET2);
		    if CONF2 then
		      begin
			FASTBASIS(ARRAY2.TYPTR^.INXTYPE,CINDEX);
			if RUNTMCHECK then
			  begin
			  MACRO4(OP_CAML,OFFSET2.REG,CINDEX,LMIN2);
			  MACRO3(OP_MOVE,BIXREG,OFFSET2.REG);
			  MACRO3(OP_ADD,BIXREG,LREGC2);
			  MACRO3(OP_SUBI,BIXREG,1);
			  MACRO4(OP_CAMLE,BIXREG,CINDEX,LMAX2);
			  SUPPORT(INDEXERROR)
			  end;
			MACRO4(OP_SUB,OFFSET2.REG,CINDEX,LMIN2);
			MACRO3(OP_ADD,ARRAY2.REG,OFFSET1.REG);
		      end
		    else with OFFSET2 do
		      begin
			if LMIN2 > 0 then
			  MACRO3C(OP_SUB,REG,LMIN2)
			else
			  if LMIN2 < 0 then
			    MACRO3C(OP_ADD,REG,-LMIN2);
			if RUNTMCHECK then
			  begin
			    MACRO3C(OP_MOVE,BIXREG,LMAX2+1);
			    MACRO3(OP_SUB,BIXREG,OFFSET1.REG);
			    MACRO3(OP_CAIL,REG,0);
			    MACRO3(OP_CAMLE,REG,BIXREG);
			    SUPPORT(INDEXERROR)
			  end;
			MACRO3(OP_ADD,ARRAY2.REG,REG);
		      end
		  end;
		INCREMENTREGC;
		MACRO3(OP_MOVE,BIXREG,ARRAY1.REG);
		MACRO3R(OP_MOVE,REGC,ARRAY1.TYPTR^.ARRAYBPADDR);
		LADDR := IC;
		MACRO3(OP_ILDB,TAC0,REGC);
		MACRO4(OP_MOVEM,TAC0,ARRAY2.REG,START);
		MACRO3(OP_AOS,0,ARRAY2.REG);
		MACRO3R(OP_SOJG,LREGC2,LADDR)
	      end;
	    REGC := LREGC
	  end {UNPACK};
	procedure NEW;
	  const
	    TAGFMAX=5;
	  var
	    ADR:SUPPORTS; SIZEREG:ACRANGE;
	    LSP,LSP1: STP; VARTS,LMIN,LMAX: INTEGER;
	    FIRSTLOAD:BOOLEAN;
	    LSIZE,LSZ: XADDRRANGE; LVAL: VALU;
	    LATTR: ATTR; I,TAGFC: INTEGER;
	    TAGFSAV: array [0..TAGFMAX] of record
					     TAGFVAL: INTEGER;
					     TAGFADDR: XADDRRANGE;
					     LPACKKIND:PACKKIND;
					     TAGWITHID:BOOLEAN
					   end;

(* 230 - get AC's right *)
	   procedure SAVEACS;

{we are worried about forms such as NEW(foo[i,j]:x*y).  The VARIABLE that
 is used to scan the first arg may leave things in the AC's needed to access
 the variable.  The computation of the stuff after the : may mess them up.
 In any case they would have to be saved, since the code for NEW doesn't
 save the AC's.  So we check for things in the AC's that are needed and
 PUSH them.  POP's will be done after the call to NEW or DISPOSE.  This
 is the problem with a one-pass compiler - we would like to just save
 the expression FOO[I,J] and generate code after the call to new.  But
 VARIABLE may generate code as a sideeffect.}
	
	      begin
		if (GATTR.INDEXR > REGIN) and (GATTR.INDEXR <= REGCMAX) then
		  MACRO3(OP_PUSH,TOPP,GATTR.INDEXR);
		if (GATTR.PACKFG = PACKK) and (GATTR.BPADDR > REGIN)
		    and (GATTR.BPADDR <= REGCMAX) then
		  MACRO3(OP_PUSH,TOPP,GATTR.BPADDR);
	      end;

	  begin
	    for I:=0 to TAGFMAX do TAGFSAV[I].TAGWITHID := FALSE;
	    if LKEY = 44 {dispose} then
	      begin
(* 230 - this code didn't work for things other than simple variables *)
		FIXEDVARIABLE(FSYS or [COMMA,COLON,RPARENT]);
		SAVEACS;
		LATTR := GATTR;  {We have to use a local copy so that
				 if AC1 is loaded here, that fact is
				 not saved for the store later.}
		LOAD(LATTR);
		INCREMENTREGC; SIZEREG := REGC;
	      end
	    else
	      if LKEY in [14,35,45] then {NEW, NEWZ, NEWL}
		begin   (* all except file names *)
		  INCREMENTREGC; SIZEREG := REGC;
		  VARIABLE(FSYS or [COMMA,COLON,RPARENT]);
		  SAVEACS;
		end
		  (* validate files for get and put stuff, but not for RECSIZE,
		   which seems OK even if the file isn't open yet *)
	      else
		begin
		  GETFN(LKEY in [1,3,40]); SIZEREG := REGIN+2
		end;

	    LSP := nil; VARTS := 0; LSIZE := 0; TAGFC := -1;
	    LATTR := GATTR;
	    if GATTR.TYPTR # nil then
	      with GATTR.TYPTR^ do
		if (LKEY in [14,35,44,45]) and (FORM=POINTER) or
		  (LKEY in [1,3,15,40]) and (FORM=FILES) then
		  begin
		    %WARNING: This code depends upon fact that ELTYPE and FILTYPE are in the same place\
		    if ELTYPE # nil then
		      begin
			LSIZE := ELTYPE^.SIZE;
			if ELTYPE^.FORM = RECORDS then
			  LSP := ELTYPE^.RECVAR
			else
			  if ELTYPE^.FORM = ARRAYS then
			    LSP := ELTYPE
		      end
		  end
		else
		  ERROR(458);
	    while SY = COMMA do
	      begin
		INSYMBOL; CONSTANT(FSYS or [COMMA,COLON,RPARENT],LSP1,LVAL);
		VARTS := VARTS + 1;
		%CHECK TO INSERTADDR HERE: IS CONSTANT IN TAGFIELDTYPE RANGE\
		if LSP = nil then
		  ERROR(408)
		else
		  if STRING(LSP1) or (LSP1=REALPTR) then
		    ERROR(460)
		  else
		    begin
		      TAGFC := TAGFC + 1;
		      if TAGFC > TAGFMAX then
			begin
			  ERROR(409);TAGFC := TAGFMAX; goto 1
			end;
		      if LSP^.FORM = TAGFWITHID then
			begin
			  if LSP^.TAGFIELDP # nil then
			    if COMPTYPES(LSP^.TAGFIELDP^.IDTYPE,LSP1) then
			      with TAGFSAV[TAGFC],LSP^.TAGFIELDP^ do
				begin
				  TAGFVAL := LVAL.IVAL;  TAGFADDR:= FLDADDR;
				  LPACKKIND:= PACKF;  TAGWITHID:=TRUE
				end
			    else
			      begin
				ERROR(458);goto 1
			      end
			end
		      else
			if LSP^.FORM=TAGFWITHOUTID then
			  begin
			    if not COMPTYPES(LSP^.TAGFIELDTYPE,LSP1) then
			      begin
				ERROR(458); goto 1
			      end
			  end
			else
			  begin
			    ERROR(358);goto 1
			  end;
		      LSP1 := LSP^.FSTVAR;
		      while LSP1 # nil do
			with LSP1^ do
			  if VARVAL.IVAL = LVAL.IVAL then
			    begin
			      LSIZE :=SIZE; LSP := SUBVAR; goto 1
			    end
			  else
			    LSP1:=NXTVAR;
		      LSIZE := LSP^.SIZE; LSP := nil
		    end;
	  1:
	      end %WHILE\;
	    if SY = COLON then
	      begin
		INSYMBOL;
		EXPRESSION(FSYS or [RPARENT],ONREGC);
		if LSP = nil then
		  ERROR(408)
		else
		  if LSP^.FORM # ARRAYS then
		    ERROR(259)
		  else
		    begin
		      if  not COMPTYPES(GATTR.TYPTR,LSP^.INXTYPE) then
			ERROR(458);
		      LSZ := 1; LMIN := 1;
		      if LSP^.INXTYPE # nil then
			GETBOUNDS(LSP^.INXTYPE,LMIN,LMAX);
		      if LSP^.AELTYPE # nil then
			LSZ := LSP^.AELTYPE^.SIZE;
		      LOAD(GATTR);
		      if RUNTMCHECK then
			begin
			  MACRO3C(OP_CAML,REGC,LMIN);
			  MACRO3C(OP_CAMLE,REGC,LMAX);
			  SUPPORT(INDEXERROR)
			end;
		      if LSZ # 1 then
			MACRO3C(OP_IMUL,REGC,LSZ);
		      if LSP^.ARRAYPF then
			begin
			  LSZ := BITMAX div LSP^.AELTYPE^.BITSIZE-1-(LMIN-1);
			  if LSZ > 0 then
			    MACRO3C(OP_ADD,REGC,LSZ)
			  else
			    if LSZ < 0 then
			      MACRO3C(OP_SUB,REGC,-LSZ);
			  INCREMENTREGC; REGC := REGC - 1;
			  %FOR TESTING BECAUSE IDIV WORKS ON AC+1 TOO\
			  MACRO3C(OP_IDIV,REGC,BITMAX div LSP^.AELTYPE^.BITSIZE);
			  LSZ := LSIZE - LSP^.SIZE;
			end
		      else
			LSZ := LSIZE - LSP^.SIZE - LSZ*(LMIN - 1);
		      if EXTENDED_ADDRESSING then
			begin
			  MACRO3(OP_MOVE,SIZEREG,REGC);
			  MACRO3C(OP_ADD,SIZEREG,LSZ)
			end
		      else MACRO4(OP_MOVEI,SIZEREG,REGC,LSZ)
		    end
	      end
	    else
	      MACRO3C(OP_MOVE,SIZEREG,LSIZE);
	    if LATTR.TYPTR # nil then
	      begin
		case LKEY of
		  1:  if COMPTYPES(LATTR.TYPTR^.FILTYPE,CHARPTR) then
			ADR := GETCHARACTER
		      else
			ADR := GETFILE;
		  3:  ADR := PUTFILE;
		  14: if ZERO then
			ADR := CLEARALLOC
		      else
			ADR := ALLOCATE;
		  15: with GATTR do begin
		    TYPTR:=INTPTR; REG:=SIZEREG;
		    KIND:=EXPR; REGC:=SIZEREG
		  end;
		  35: ADR := CLEARALLOC;
		  40: if COMPTYPES(LATTR.TYPTR^.FILTYPE,CHARPTR) then
			ERROR(458)
		      else
			ADR:=GETXF;
		  44: if LATTR.TYPTR^.ELTYPE <> nil then
			if LATTR.TYPTR^.ELTYPE^.HASFILE then
			  ADR := WITHFILEDEALLOCATE
			else
			  ADR := DEALLOCATE
		      else
			ADR := DEALLOCATE;
		  45: ADR := LOCALALLOC
		end;
(* 230 - get the AC's right *)

{NEW and DISPOSE garbage the AC's up to 6.  We have already saved anything
 that is going to be used in STORE's afterwards.  However if there are
 any active WITH AC's, they have to be saved.  After the call, for
 NEW and DISPOSE we restore anything that was saved above.}

		if (LKEY in [14,35,44,45]) and (REGCMAX < 6) then
		  for I := 0 to WITHIX do
		    with DISPLAY[TOP-I] do
		      if (CINDR#0) and (CINDR <= 6) then
			MACRO4(OP_MOVEM,CINDR,BASIS,CLC);
		if (LKEY in [14,35,44,45]) then
		  begin
		    SUPPORT(ADR);
		    if (LATTR.PACKFG = PACKK) and (LATTR.BPADDR > REGIN)
		      and (LATTR.BPADDR <= 6) then
		      MACRO3(OP_POP,TOPP,LATTR.BPADDR);
		    if (LATTR.INDEXR > REGIN) and (LATTR.INDEXR <= 6) then
		      MACRO3(OP_POP,TOPP,LATTR.INDEXR);
		  end
		else
		  if LKEY#15 then
		    SUPPORT(ADR);
		(* restore WITH ac's *)
		if (LKEY in [14,35,44,45]) and (REGCMAX < 6) then
		  for I := 0 to WITHIX do
		    with DISPLAY[TOP-I] do
		      if (CINDR#0) and (CINDR <= 6) then
			MACRO4(OP_MOVE,CINDR,BASIS,CLC)
	      end;
	    if LKEY in [14,35,45] then {NEW, NEWZ, NEWL}
	      begin
		REGC := REGIN + 1;  FIRSTLOAD := TRUE;
		for I := 0 to TAGFC do
		  with TAGFSAV[I] do
		    if TAGWITHID then
		      begin
			MACRO3(OP_MOVEI,TAC0,TAGFVAL);
			case LPACKKIND of
			  NOTPACK:  MACRO4(OP_MOVEM,TAC0,REGC,TAGFADDR);
			  HWORDR:   MACRO4(OP_HRRM,TAC0,REGC,TAGFADDR);
			  HWORDL:   MACRO4(OP_HRLM,TAC0,REGC,TAGFADDR);
			  PACKK :
			    begin
			      if FIRSTLOAD then
				begin
				  MACRO3(OP_MOVE,BIXREG,REGC);
				  FIRSTLOAD := FALSE
				end;
			      MACRO3R(OP_DPB,TAC0,TAGFADDR)
			    end
			  end%CASE\
		      end;
		STORE(REGC,LATTR)
	      end
	    else
	      if LKEY=44 then
		begin
(* 230 - get the AC's right *)
		  MACRO3(OP_MOVEI,TAC0,NIL_VALUE);
		  STORE(TAC0,LATTR)
		end
	  end %NEW\;
	procedure CALLI;
	  type
	    ARGFORM=(BAREAC,XWD,TWOWORDS,ONEWORD);
	  var
	    LSP:STP; LVAL,ACVAL:VALU;
	    LH,RH,BOOL,RESUL:ATTR;
	    ARG:ARGFORM;
	  begin
	    ARG := XWD;  %default format\
	    CONSTANT(FSYS or [RPARENT,COMMA],LSP,LVAL);
	    if not(COMPTYPES(INTPTR,LSP)) then
	      ERROR(458);
	    if SY = COMMA then
	      INSYMBOL
	    else
	      ERROR(158);
	    if SY=COMMA %,,WORD\ then
	      begin
		INSYMBOL;  ARG := ONEWORD;
		EXPRESSION(FSYS or [RPARENT,COMMA],ONREGC);
		LOAD(GATTR);  LH := GATTR
	      end
	    else
	      if SY=COLON  %:ac\ then
		begin
		  ARG := BAREAC;  INSYMBOL;
		  CONSTANT(FSYS or [RPARENT,COMMA],LSP,ACVAL);
		  if not(COMPTYPES(INTPTR,LSP)) then
		    ERROR(458)
		end
	      else
		begin  %lh,rh   or w1:w2\
		  EXPRESSION(FSYS or [RPARENT,COMMA,COLON],ONREGC);
		  LOAD(GATTR);  LH := GATTR;
		  if SY = COMMA then
		    INSYMBOL
		  else
		    if SY=COLON then
		      begin
			ARG:=TWOWORDS; INSYMBOL
		      end
		    else
		      ERROR(158);
		  EXPRESSION(FSYS or [RPARENT,COMMA],ONREGC);
		  if GATTR.TYPTR # nil then
		    if (GATTR.TYPTR^.FORM <= POWER) or (ARG=TWOWORDS) then
		      LOAD(GATTR)
		    else
		      begin
			LOADADDRESS;  GATTR.KIND:=EXPR;
			GATTR.REG:=GATTR.INDEXR
		      end;
		  RH := GATTR;
		end  %of lh,rh and w1:w2\;
	    if SY = COMMA then
	      INSYMBOL
	    else
	      ERROR(158);
	    VARIABLE(FSYS or [RPARENT,COMMA]);
	    CHECKASSIGN(GATTR);
	    if GATTR.TYPTR = nil then
	      ERROR(458)
	    else
	      if not(GATTR.TYPTR^.FORM in [SUBRANGE,SCALAR]) then
		ERROR(458)
	      else
		LOADADDRESS;
	    RESUL:=GATTR;
	    if SY = COMMA then
	      INSYMBOL
	    else
	      ERROR(158);
	    VARIABLE(FSYS or [RPARENT]);
	    CHECKASSIGN(GATTR);
	    if not COMPTYPES(BOOLPTR,GATTR.TYPTR) then
	      ERROR(158)
	    else
	      LOADADDRESS;
	    BOOL := GATTR;
	    if not ERRORFLAG then
	      begin
		case ARG of
		  BAREAC: REGC := ACVAL.IVAL;
		  XWD: begin
		    REGC := RH.REG; MACRO3(OP_HRL,RH.REG,LH.REG)
		  end;
		  ONEWORD: REGC := LH.REG;
		  TWOWORDS: begin
		    REGC := LH.REG;
		    if (REGC+1) # RH.REG then
		      MACRO3(OP_MOVE,REGC+1,RH.REG)
		  end
		  end %CASE\;
		MACRO3(OP_MOVEI,TAC1,1);
		MACRO4(OP_MOVEM,TAC1,BOOL.INDEXR,0);
		MACRO3(OP_CALLI,REGC,LVAL.IVAL);
		MACRO4(OP_SETZM,0,BOOL.INDEXR,0);
		MACRO4(OP_MOVEM,REGC,RESUL.INDEXR,0)
	      end
	  end;

	procedure JSYS;
	  var
	    LVAL:VALU; LSP:STP; JSYSNUM,NUMRETS,I:INTEGER;
	    RETSAVE:ATTR; SAVERET,ERCAL,DONE1:BOOLEAN;
	    REALREGC:ACRANGE;
(* 233 - better temp allocation *)
	    SAVELC,SAVEBPADDR,SAVEINDEXR:XADDRRANGE;
	    BYTELH:array[ACRANGE]of ADDRRANGE;

	  procedure LOADARG;
	    (* Handles input args for jsys:
	     simple vars - use their values
	     sets - use LH word only
	     files - use jfn word
	     packed arrays - make byte ptr to it
	     other - make pointer to it
	     *)
	    begin
	      EXPRESSION (FSYS or [RPARENT,COMMA,SEMICOLON,COLON],ONFIXEDREGC);
	      if GATTR.TYPTR # nil then
		if (GATTR.TYPTR^.FORM < POWER) then
		  LOAD(GATTR)
		else
		  if (GATTR.TYPTR^.FORM = POWER) then
		    begin
		      (* can't treat as integer. have to load both words and throw away 2nd *)
		      LOAD(GATTR);
		      REGC := REGC-1
		    end
		  else
		    if (GATTR.TYPTR^.FORM = FILES) then
		      begin
			LOADADDRESS;
			if GATTR.EXTERNCTP <> nil then
			  begin
			    GATTR.EXTERNCTP^.VADDR := IC - 1;
			    CODE.INFORMATION[CIX] := 'E'
			  end;
			MACRO4(OP_MOVE,REGC,REGC,FILJFN)
		      end
		    else
		      begin
		        if (GATTR.TYPTR^.FORM = ARRAYS) 
			    and GATTR.TYPTR^.ARRAYPF then
			  if DONE1 then
			    BYTELH[REGC+1] := 
		   	      440000B + GATTR.TYPTR^.AELTYPE^.BITSIZE * 100B
			  else BYTELH[1] := 
		   	      440000B + GATTR.TYPTR^.AELTYPE^.BITSIZE * 100B;
			LOADADDRESS
		      end
	    end;
	    {LoadArg}
	  procedure STOREARG;
	    (* stores results of jsys.  As above, but error for
	     anything bigger than a word *)
	    begin
	      VARIABLE(FSYS or [RPARENT,COMMA]);
	      CHECKASSIGN(GATTR);
	      if GATTR.TYPTR # nil then
		if (GATTR.TYPTR^.FORM < POWER) then
		  STORE(REALREGC,GATTR)
		else
		  if (GATTR.TYPTR^.FORM = POWER) then
		    begin
		      GATTR.TYPTR := INTPTR;
		      STORE(REALREGC,GATTR)
		    end
		  else
		    if (GATTR.TYPTR^.FORM = FILES) then
		      begin
			LOADADDRESS;  {ADDR OF FILE NOW IN REGC}
			if GATTR.EXTERNCTP <> nil then
			  begin
			    GATTR.EXTERNCTP^.VADDR := IC - 1;
			    CODE.INFORMATION[CIX] := 'E'
			  end;
			(* internal files *)
			{We have to compile code to see if the file is initialized.  If not,
			call INITB. to do so.  INITB. needs the file in AC 2.  Note that
			the AC use here is such that REGC is always above 2, so the only
			reason for 2 not to be free is that realregc is using it.  This is
			certainly not the best possible code, but at this point I am going
			for the last code in the compiler to implement it.}
			MACRO3(OP_EXCH,TAC2,REGC);
			MACRO4(OP_MOVE,TAC0,TAC2,FILTST);
			MACRO3(OP_CAIE,TAC0,314157B);
			SUPPORT(INITFILEBLOCK);
			if REALREGC = TAC2 then
			  MACRO4(OP_MOVEM,REGC,TAC2,FILJFN)
			else
			  MACRO4(OP_MOVEM,REALREGC,TAC2,FILJFN)
		      end
		    else
		      ERROR(458)
	    end;
	    {StoreArg}

	  function XOR(i,j:INTEGER):INTEGER;
		var ci,cj:record case Boolean of
			true:(word:integer);
			false:(bits:set of 0..35)
			end;
	    begin
	    ci.word := i;
	    cj.word := j;
	    ci.bits := (ci.bits or cj.bits) - (ci.bits and cj.bits);
	    xor := ci.word
	    end;

	  begin (* JSYS *)
	    ERCAL := FALSE; SAVERET := FALSE; NUMRETS := 0; DONE1 := FALSE;
(* 230 - prevent garbaging AC's if return code goes into odd kind of var *)
(* 233 - better temp allocation *)
	    SAVEBPADDR := 0;  SAVEINDEXR := 0;
	    for I := 1 to PARREGCMAX do
	      BYTELH[I] := 0;   {used for string arguments}
	    CONSTANT(FSYS or [RPARENT,COMMA,SEMICOLON],LSP,LVAL);
	    JSYSNUM := LVAL.IVAL;
	    if not COMPTYPES (INTPTR, LSP) then
	      ERROR(458);
	    if SY = COMMA then
	      begin (* return spec *)
		INSYMBOL;
		CONSTANT(FSYS or [RPARENT,COMMA,SEMICOLON],LSP,LVAL);
		if LVAL.IVAL < 0 then
		  ERCAL := TRUE;
		NUMRETS := ABS(LVAL.IVAL);
		if not COMPTYPES (INTPTR, LSP) then
		  ERROR(458);
		if SY = COMMA then
		  begin (* RETURN VAR *)
		    INSYMBOL;
		    VARIABLE(FSYS or [RPARENT,SEMICOLON]);
		    CHECKASSIGN(GATTR);
		    if COMPTYPES (INTPTR,GATTR.TYPTR) then
		      begin
			SAVERET := TRUE; RETSAVE := GATTR
		      end
		    else
		      ERROR (459);
(* 230 - prevent garbaging AC's if return code goes into odd kind of var *)
{if the address of the variable depends upon any AC's, e.g. it is an
 array element, then we had better save them, because the rest of the
 code may garbage them.}
		    if (retsave.indexr > 0) and (retsave.indexr <= regcmax) or
		       (retsave.bpaddr > 0) and (retsave.bpaddr <= regcmax) 
		      then
			begin
			  if (retsave.indexr > 0) and
			      (retsave.indexr <= regcmax) then
			    begin
			    saveindexr := temp_lc(1); {get us one word}
			    macro4(op_movem,retsave.indexr,basis,saveindexr);
			    end;
			  if (retsave.bpaddr > 0) and
			      (retsave.bpaddr <= regcmax) then
			    begin
			    savebpaddr := temp_lc(1);
			    macro4(op_movem,retsave.bpaddr,basis,savebpaddr);
			    end;
			end;
		  end
	      end; (* RETURN SPEC *)
	    if SY = SEMICOLON then
	      begin (* PROLOG *)
		INSYMBOL;  REGC := 1;
		if SY # SEMICOLON then
		  loop (* NON-EMPTY PROLOG *)
		    LOADARG;
		    if SY = COLON then
		      begin
			INSYMBOL;  REALREGC := REGC;  LOADARG;
			MACRO3(OP_HRL,REALREGC,REALREGC);
			MACRO3(OP_HRR,REALREGC,REGC);
			REGC := REALREGC;
{If we do A:B, forget it if A and B are strings}
			if DONE1 then
			  begin
			    BYTELH[REGC] := 0;
			    BYTELH[REGC+1] := 0
			  end
			else
			  begin
			    BYTELH[1] := 0;
			    BYTELH[2] := 0
			  end;
		      end;
		    if not DONE1 then
		      begin
			{Here we prepared a place on the display to store the value}
(* 233 - better way of allocating temp's *)
			SAVELC := TEMP_LC(1);
			MACRO4(OP_MOVEM,TAC2,BASIS,SAVELC);
			DONE1 := TRUE;  REGC := 1
		      end;
		  exit if SY # COMMA;
		    INSYMBOL
		end (* non-empty prolog *)
	      end; (* prolog *)
	    (* main call *)
	    if DONE1 then
	      begin
(* 233 - better way of allocating temp's *)
		MACRO4(OP_MOVE,TAC1,BASIS,SAVELC);
		{DISP_LC(SAVELC,1);} {No longer need this temp}
	      end;
	    if SAVERET then
	      MACRO3(OP_MOVEI,TAC0,NUMRETS+1);
	    for I := 1 to PARREGCMAX do
{Now put in the LH of byte pointers, moving the address to another REG
 if extended addressing}
	      if BYTELH[I] <> 0 then
		if EXTENDED_ADDRESSING then
  		  begin
		    INCREMENTREGC;
		    MACRO3(OP_MOVE,REGC,I);  {REGC is the address}
		    MACRO3(OP_HRLZI,I,BYTELH[I] + REGC)
		  end
		else MACRO3(OP_HRLI,I,BYTELH[I]);
	    MACRO3(OP_JSYS,0,JSYSNUM);
	    if ERCAL then
	      begin
		MACRO3R(OP_JUMP,16B,IC+NUMRETS);
		NUMRETS := NUMRETS -1
	      end;
	    for I := 1 to NUMRETS do
	      if SAVERET then
		MACRO3(OP_SUBI,TAC0,1)
	      else
		MACRO3(OP_JFCL,0,0);
	    if SY = SEMICOLON then
	      begin (* if epilog, save reg a over store *)
		{find a place in the display to save ac 2}
(* 233 - better way to allocation temp's *)
		SAVELC := TEMP_LC(1);
		MACRO4(OP_MOVEM,TAC2,BASIS,SAVELC);
		MACRO3(OP_MOVE,TAC2,TAC1);  DONE1 := TRUE
	      end
	    else
	      DONE1 := FALSE;
	    if SAVERET then
	      begin
(* 230 - prevent garbaging AC's if return code goes into odd kind of var *)
(* 233 - better temp allocation *)
{If the address where this stuff is supposed to go depends upon
 any AC's that were saved on the stack, get them back.  However we
 have to get them back into AC's above 4, so as not to interfere
 with anything returned by the JSYS.}
		if saveindexr <> 0 then {index reg saved on stack}
		  begin
		    macro4(op_move,5,basis,saveindexr);
		    retsave.indexr := 5;
		    {disp_lc(saveindexr,1);} {we're now done with this space}
		  end;
		if savebpaddr <> 0 then {bpaddr saved on stack}
		  begin
		    macro4(op_move,6,basis,savebpaddr);
		    retsave.bpaddr := 6;
		    {disp_lc(savebpaddr,1);}
		  end;
		STORE(0,RETSAVE);
	      end;
	    if SY = SEMICOLON then
	      begin (* epilog *)
		REALREGC := 1;
		repeat
		  INSYMBOL;
		  REGC := 4; (* so temp ac's start at 5 *)
		  REALREGC := REALREGC + 1;
		  if REALREGC > 4 then
		    ERROR(458);
		  STOREARG;
		  if DONE1 then
		    begin
		      (* use display instead of stack to store ac 2 *)
(* 233 - better way to allocate temp's *)
		      MACRO4(OP_MOVE,TAC2,BASIS,SAVELC);
		      {DISP_LC(SAVELC,1);} {We don't need this temp more}
		      REALREGC := 1;
		      DONE1 := FALSE
		    end
		until SY # COMMA
	      end; (* EPILOG *)
(* 230 - prevent garbaging AC's if return code goes into odd kind of var *)
(* 233 - better temp allocation *)
	  end; (* JSYS *)
	procedure MARK;
	  begin
	    VARIABLE(FSYS or [RPARENT]);
	    CHECKASSIGN(GATTR);
	    if COMPTYPES(INTPTR,GATTR.TYPTR) then
	      if NOVM then
		begin
		  LOADADDRESS;
		  MACRO4(OP_MOVEM,NEWREG,GATTR.INDEXR,0)
		end
	      else
		begin
		  LOADADDRESS; INCREMENTREGC;
		  MACRO3R(OP_MOVE,REGC,LSTNEW);
		  CODE.INFORMATION[CIX]:= 'E';
		  LSTNEW:=IC-1;  %GLOBAL FIXUP\
		  MACRO4(OP_MOVEM,REGC,GATTR.INDEXR,0)
		end
	    else
	      ERROR(459)
	  end %MARK\;

	procedure RELEASE;
	  begin
	    EXPRESSION(FSYS or [RPARENT],ONREGC);
	    if GATTR.TYPTR = INTPTR then
	      begin
		LOAD(GATTR);
		if NOVM then
		  MACRO3(OP_MOVEM,REGC,NEWREG)
		else
		  begin
		    MACRO3R(OP_MOVEM,REGC,LSTNEW);
		    CODE.INFORMATION[CIX]:= 'E';
		    LSTNEW := IC-1;  % GLOBAL FIXUP \
		  end
	      end
	    else
	      ERROR(458)
	  end %RELEASE\;
	procedure GETLINENR;
	  begin
	    GETFILENAME(INFILE,TRUE,THISFILE,GOTARG,TRUE);
	    if not GOTARG then
	      ERROR(554);
	    if GATTR.KIND <> VARBL then
	      ERROR(458)
	    else
	      if GATTR.TYPTR # nil then
		if COMPTYPES(CHARPTR,GATTR.TYPTR^.AELTYPE) and (GATTR.TYPTR^.FORM = ARRAYS) then
		  begin
		    MACRO4(OP_MOVE,REGC,REGC,FILLNR); STORE(REGC,GATTR)
		  end
		else
		  ERROR(458);
	  end;

	procedure PUT8BITSTOTTY;
	  begin
	    EXPRESSION(FSYS or [RPARENT],ONREGC);  LOAD(GATTR);
	    MACRO3(OP_TTCALL,15B%IONEOU\,GATTR.REG)
	  end %PUT8BITSTOTTY\;

	procedure PAGE;
	  begin
	    GETFILENAME(OUTFILE,TRUE,THISFILE,GOTARG,TRUE);
	    if GOTARG then
	      ERROR(554);
	    SUPPORT(PUTPAGE)
	  end;
	  (* support for tops-20 time and runtime *)
	procedure JSYSF(JSYSNUM,HIREG:INTEGER);
	  var
	    I:INTEGER;
	  begin
	    if HIREG > REGC then
	      HIREG := REGC;
	    for I := 2 to HIREG do MACRO3(OP_PUSH,TOPP,I);
	    if JSYSNUM = JSYS_RUNTM then
	      MACRO3(OP_MOVNI,TAC1,5);
	    MACRO3(OP_JSYS,0,JSYSNUM);
	    with GATTR do
	      begin
		INCREMENTREGC; TYPTR := INTPTR; REG := REGC; KIND := EXPR;
		MACRO3(OP_MOVE,REGC,TAC1)
	      end;
	    for I := HIREG downto 2 do MACRO3(OP_POP,TOPP,I)
	  end;


	procedure RUNTIME;
	  begin
	    if TOPS10 then
	      with GATTR do
		begin
		  INCREMENTREGC; TYPTR := INTPTR; REG := REGC; KIND := EXPR;
		  MACRO3(OP_SETZ,REGC,0);
		  MACRO3(OP_CALLI,REGC,CALLI_RUNTIM)
		end
	    else
	      JSYSF(JSYS_RUNTM,3)
	  end;

	procedure ABS;
	  begin
	    with GATTR do
	      if (TYPTR = INTPTR) or (TYPTR = REALPTR) then
		with CODE.INSTRUCTION[CIX] do
		  if INSTR = OP_MOVE then
		    INSTR := OP_MOVM
		  else
		    MACRO3(OP_MOVM,REG,REG)
	      else
		begin
		  ERROR(459); TYPTR:= INTPTR
		end
	  end %ABS\;

	procedure TIME;
	  begin
	    with GATTR do
	      begin
		INCREMENTREGC; TYPTR := INTPTR; REG := REGC; KIND := EXPR;
		if TOPS10 then
		  MACRO3(OP_CALLI,REGC,CALLI_MSTIME)
		else
		  begin
		    SUPPORT(GETDAYTIME);  MACRO3(OP_POP,TOPP,REGC)
		  end
	      end
	  end;

	procedure SQR;
	  begin
	    with GATTR do
	      if TYPTR = INTPTR then
		MACRO3(OP_IMUL,REG,REG)
	      else
		if TYPTR = REALPTR then
		  MACRO3(OP_FMPR,REG,REG)
		else
		  begin
		    ERROR(459); TYPTR := INTPTR
		  end
	  end %SQR\;

	procedure TRUNC;
	  var
	    INSTRUC:1..777;
	  begin
	    if LKEY = 5 then
	      INSTRUC := OP_FIX
	    else
	      INSTRUC := OP_FIXR;
	    if GATTR.TYPTR # REALPTR then
	      ERROR(459)
	    else
	      if KACPU then
		begin
		  if LKEY=5 then
		    MACRO3(OP_MOVEI,TAC1,GATTR.REG)
		  else
		    MACRO3(OP_HRROI,TAC1,GATTR.REG);
		  SUPPORT(CONVERTREALTOINTEGER)
		end
	      else
		with CODE.INSTRUCTION[CIX] do
		  if (INSTR = OP_MOVE) and (AC = GATTR.REG) then
		    INSTR := INSTRUC
		  else
		    MACRO3(INSTRUC,GATTR.REG,GATTR.REG);
	    GATTR.TYPTR := INTPTR
	  end %TRUNC\;

	procedure ODD;
	  begin
	    with GATTR do
	      begin
		if TYPTR # INTPTR then
		  ERROR(459);
		MACRO3(OP_ANDI,REG,1);  TYPTR := BOOLPTR
	      end
	  end %ODD\;

	procedure ORD;
	  begin
	    if GATTR.TYPTR # nil then
	      if GATTR.TYPTR^.FORM >= POWER then
		ERROR(459);
	    GATTR.TYPTR := INTPTR
	  end %ORD\;

	procedure CHR;
	  begin
	    if GATTR.TYPTR # INTPTR then
	      ERROR(459);
	    GATTR.TYPTR := CHARPTR
	  end %CHR\;

	procedure PREDSUCC;
	  var
	    LSTRPTR:STP; LATTR: ATTR;
	  begin
	    if GATTR.TYPTR # nil then
	      if (GATTR.TYPTR^.FORM>SUBRANGE) or (GATTR.TYPTR=REALPTR) then
		ERROR(459)
	      else
		if RUNTMCHECK then
		  begin
		    LSTRPTR:=GATTR.TYPTR;
		    if (LSTRPTR^.FORM=SUBRANGE) and (LSTRPTR^.RANGETYPE#nil) then
		      LSTRPTR:=LSTRPTR^.RANGETYPE;
		    if LKEY=9 then
		      begin
			if LSTRPTR=INTPTR then
			  begin
			    MACRO3R(OP_JFCL,10B,IC+1);
			    MACRO3(OP_SOS,0,REGC);
			    MACRO3R(OP_JFCL,10B,IC+2);
			    MACRO3(OP_SKIPA,0,0);
			    SUPPORT(ERRORINASSIGNMENT)
			  end
			else
			  begin % CHAR OR DECLARED \
			    MACRO3R(OP_SOSGE,0,REGC);
			    SUPPORT(ERRORINASSIGNMENT)
			  end
		      end % LKEY = 9 \
		    else
		      begin % LKEY = 10 \
			if LSTRPTR=INTPTR then
			  begin
			    MACRO3R(OP_JFCL,10B,IC+1);
			    MACRO3(OP_AOS,0,REGC);
			    MACRO3R(OP_JFCL,10B,IC+2);
			    MACRO3(OP_SKIPA,0,0);
			    SUPPORT(ERRORINASSIGNMENT)
			  end
			else
			  begin %CHAR OR DECLARED\
			    with LATTR do
			      begin
				TYPTR := LSTRPTR; KIND := CST;
				CVAL.IVAL := 0;
				if LSTRPTR=CHARPTR then
				  CVAL.IVAL := 177B
				else
				  if LSTRPTR^.FCONST # nil then
				    CVAL.IVAL:=LSTRPTR^.FCONST^.VALUES.IVAL;
				MAKECODE(OP_CAML,REGC,LATTR);
				SUPPORT(ERRORINASSIGNMENT);
				MACRO3(OP_AOS,0,REGC)
			      end
			  end
		      end % LKEY = 10 \;
		  end % RUNTMCHECK \
		else
		  if LKEY = 9 then
		    MACRO3(OP_SUBI,REGC,1)
		  else
		    MACRO3(OP_ADDI,REGC,1)
	  end %PREDSUCC\;

	procedure EOFEOLN;
	  begin
	    (* USE GETFILENAME, SO DEFAULTS TO INPUT *)
	    (* PREDECL FILES ARE SPECIAL *)
	    GETFILENAME(INFILE,FALSE,THISFILE,GOTARG,TRUE);
	    if GOTARG then
	      ERROR(554);
	    with GATTR do
	      begin
		KIND := EXPR; REG := INDEXR;
		if LKEY=11 then
		  begin
		    MACRO4(OP_SKIPE,REG,REG,FILEOF);
		    MACRO3(OP_MOVEI,REG,1)
		  end
		else
		  MACRO4(OP_MOVE,REG,REG,FILEOL);
		TYPTR := BOOLPTR
	      end
	  end %EOF\;

	procedure PROTECTION;
	  (* FOR DETAILS SEE  DEC-SYSTEM-10 MONITOR CALLS MANUAL, 3.2.4 *)
	  begin
	    EXPRESSION (FSYS or [RPARENT], ONREGC);
	    if GATTR.TYPTR = BOOLPTR then
	      begin
		if TOPS10 then
		  begin
		    LOAD(GATTR);
		    MACRO3(OP_CALLI,REGC,CALLI_SETUWP);
		    MACRO3(OP_HALT,4,IC)
		  end
	      end
	    else
	      ERROR(458)
	  end;
	procedure CALLNONSTANDARD;
	  var
	    NXT,LNXT,LCP,NXT1: CTP;
	    ARTYPE,LSP: STP;
	    PKIND,LKIND: IDKIND;    LB: BOOLEAN;
	    FCONF,CONF, HAS_PAREN: BOOLEAN;
	    BOUND,TEMP_COPY: XADDRRANGE;
	    CINDEX,STREGC,ADDRREG: ACRANGE;
	    SAVECOUNT,P,I,NOFPAR: INTEGER;
	    TOPPOFFSET,OFFSET,PARLIST,ACTUALPAR,
		FIRSTPAR,LLC: XADDRRANGE;
	    LREGC: ACRANGE;

{Compares two procedures.}
	  function COMP_PROCS(LCP1,LCP2:CTP):BOOLEAN;
	    var NXT1,NXT2:CTP; ANSWER: BOOLEAN;
	    begin
	    if LCP1^.KLASS <> LCP2^.KLASS
	      then COMP_PROCS := FALSE
	      else begin
	      ANSWER := TRUE;
	      if LCP1^.KLASS = FUNC
		then ANSWER := COMPTYPES(LCP1^.IDTYPE,LCP2^.IDTYPE);
	      if not ANSWER
	       then COMP_PROCS := FALSE
	       else begin
	       if LCP1^.PFKIND = FORMAL
		 then NXT1 := LCP1^.PFCHAIN
		 else NXT1 := LCP1^.NEXT;
	       if LCP2^.PFKIND = FORMAL
		 then NXT2 := LCP2^.PFCHAIN
		 else NXT2 := LCP2^.NEXT;
	       if (LCP1^.PFKIND = FORMAL) and (NXT1 = NIL) or
		 (LCP2^.PFKIND = FORMAL) and (NXT2 = NIL) 
		then COMP_PROCS := TRUE {He didn't tell us his args, so
					 we allow it - this is for
					 compatibility with J&W}
		else begin
		ANSWER := TRUE;
		while ANSWER and (NXT1 <> NIL) and (NXT2 <> NIL) do
		  begin
		  if NXT1^.KLASS <> NXT2^.KLASS 
		    then ANSWER := FALSE
		    else if NXT1^.KLASS in [FUNC,PROC]
		      then ANSWER := COMP_PROCS(NXT1,NXT2)
		      else if NXT1^.VKIND <> NXT2^.VKIND
			then ANSWER := FALSE
			else ANSWER := COMPTYPES(NXT1^.IDTYPE,NXT2^.IDTYPE);
		  NXT1 := NXT1^.NEXT; NXT2:= NXT2^.NEXT
		  end;
	        COMP_PROCS := ANSWER and (NXT1 = NIL) and (NXT2 = NIL)
		end
	       end
	      end
	    end;

	  procedure PARAMFUDGE;
	    var
	      LMIN,LMAX:INTEGER;
	      (* This is used to handle special parameter types with
	       reduced type checking, such as STRING, POINTER.  They
	       are always one of STRINGPTR, POINTERPTR, DESCRIPPTR, or
	       POINTERREF. STRINGPTR is for STRING, the other two for
	       POINTER. POINTERREF is for call by ref *)
	    begin
	      with GATTR.TYPTR^ do
		if LSP=STRINGPTR then
		  if (FORM=ARRAYS) and ARRAYPF then
		    if COMPTYPES(AELTYPE,CHARPTR) then
		      begin  (* STRING *)
(* 240 - conformant *)
			LOADADDRESS; 
(* 275 - string descriptor *)
		        if FCP^.LANGUAGE <> PASCALSY then
		          if EXTENDED_ADDRESSING then
			    MACRO3(OP_TLO,REGC,610000B)   {global pointer}
		          else
			    MACRO3(OP_HRLI,REGC,440700B); {normal pointer}
		    	INCREMENTREGC;
			LOADSIZE(REGC,GATTR.TYPTR);
		      end
		    else
		      ERROR(503)
		  else
		    ERROR(503)
		else
		  if FORM=POINTER then  {pointerptr or pointerref}
		    if ELTYPE <> nil then
		      begin (* POINTER *)
			(* fix up pointer by ref *)
			if LSP = POINTERPTR then
			  LOAD(GATTR)
			else
			  LOADADDRESS;
			INCREMENTREGC;
			MACRO3C(OP_MOVE,REGC,ELTYPE^.SIZE)
		      end
		    else  (* bad type decl - already have error *)
		  else
		    ERROR(503);
	      GATTR.TYPTR := LSP  (* so comptypes later succeeds *)
	    end;

	  begin {CallNonStandard}
	    NOFPAR:= 0; TOPPOFFSET := 0; PARLIST := 0; ACTUALPAR := 0;
	    with FCP^ do
	      begin
		LKIND := PFKIND;
		if LKIND = FORMAL then
		  NXT := PFCHAIN  {we have to hide the arg list for
				   parametric procedures}
		else NXT := NEXT; 
(* 246 - for Tops-10, make space to store NEWREG *)
		if (KLASS = FUNC) or
	           (TOPS10 and (LKIND = ACTUAL) and (LANGUAGE <> PASCALSY))
		   then
		  FIRSTPAR := 2
		else
		  FIRSTPAR := 1;
		(* PROC PARAM.S *)
		if LKIND = ACTUAL then
		  if EXTERNDECL then
		    LIBRARY[LANGUAGE].CALLED:= TRUE;
		SAVECOUNT := REGC - REGIN;
		if  SAVECOUNT > 0 then
		  begin
(* 233 - better temp allocation *)
		    LLC := TEMP_LC(SAVECOUNT);
		    if KACPU then
		      if SAVECOUNT > 3 then
			begin
			  MACRO3(OP_HRLI,TAC1,TAC2);
			  MACRO4(OP_HRRI,TAC1,BASIS,LLC);
			  MACRO4(OP_BLT,TAC1,BASIS,LLC+SAVECOUNT-1)
			end
		      else
			for I := 1 to SAVECOUNT do
			  MACRO4(OP_MOVEM,REGIN+I,BASIS,LLC+I-1)
		    else
		      {NOT KACPU}
		      if (SAVECOUNT > 6) and (not EXTENDED_ADDRESSING) then
			begin
			  MACRO3(OP_HRLI,TAC1,TAC2);
			  MACRO4(OP_HRRI,TAC1,BASIS,LLC);
			  MACRO4(OP_BLT,TAC1,BASIS,LLC+SAVECOUNT-1)
			end
		      else
			begin
			  {note - for extended addressing, BLT fails
			  only for the case that the area we are saving
			  registers to crosses a section boundary, but
			  the overhead to setup an XBLT here probably
			  isnt worth it since we can do DMOVEMs anyway.
			  To use XBLT we would have to save at least AC2 then
			  setup 3 ACs, then do the XBLT.  I dont think
			  it is really necessary since the maximum to save
			  ACs 2-13 this way is 6 instructions anyway!}
			  I := 1;
			  while I < SAVECOUNT do
			    begin
			      MACRO4(OP_DMOVEM,REGIN+I,BASIS,LLC+I-1);
			      I := I + 2
			    end;
			  if I = SAVECOUNT then
			    MACRO4(OP_MOVEM,REGIN+I,BASIS,LLC+I-1)
			end
		  end;
		LREGC:= REGC;
		if LKIND = FORMAL then
		  REGC := REGIN
		else
		  if LANGUAGE # PASCALSY then
		    REGC:= PARREGCMAX
		  else
		    REGC:= REGIN
	      end;
	    if SY = LPARENT then
	      begin
		CONF := FALSE; 
		repeat
		  if not CONF then
		    INSYMBOL;
		  LB := FALSE;  %DECIDE WHETHER PROC/FUNC MUST BE PASSED\
		  if LKIND = ACTUAL then
		    if NXT = nil then
		      ERROR(554)
		    else
		      LB := NXT^.KLASS in [PROC,FUNC]
		  else 
		    if NXT = nil then  {old style param procedures}
		      LB := FALSE  {can't tell, so make an assumption}
		    else
		      LB := NXT^.KLASS in [PROC,FUNC];
		  %FOR FORMAL PROC/FUNC LB IS FALSE AND EXPRESSION
		   WILL BE CALLED, WHICH WILL ALLWAYS INTERPRET A PROC/FUNC ID
		   AT ITS BEGINNING AS A CALL RATHER THAN A PARAMETER PASSING.
		   IN THIS IMPLEMENTATION, PARAMETER PROCEDURES/FUNCTIONS
		   ARE THEREFORE NOT ALLOWED TO HAVE PROCEDURE/FUNCTION
		   PARAMETERS\
		  if LB then
		    begin   %PASS FUNCTION OR PROCEDURE\
		      if SY # IDENT then
			ERRANDSKIP(209,FSYS or [COMMA,RPARENT])
		      else
			begin
			  if NXT^.KLASS = PROC then
			    IDSEARCH([PROC],LCP,0)
			  else
			    begin
			      IDSEARCH([FUNC],LCP,0);
			    end;
			  if not COMP_PROCS(LCP,NXT)
			    then ERROR(260);
			  INSYMBOL;
			  IFERRSKIP(166,FSYS or [COMMA,RPARENT])
			end;
		      with LCP^ do
			if (PFDECKIND = STANDARD)
			  or (PFKIND = ACTUAL) and (LANGUAGE # PASCALSY) then
			  ERROR (466)
			else
			  begin
			    INCREMENTREGC;
			    if PFLEV > 1 then
			      P := LEVEL - PFLEV
			    else
			      P := 0;
			    if PFKIND = ACTUAL then
			      begin
				if P = 0 then
				  if EXTENDED_ADDRESSING then
				    MACRO3(OP_MOVE,REGC+1,BASIS)
				  else
				    MACRO3(OP_HRLZ,REGC,BASIS)
				else
				  if P=1 then
				    if EXTENDED_ADDRESSING then
				      MACRO4(OP_MOVE,REGC+1,BASIS,PREV_BASIS)
				    else
				      MACRO4(OP_HRLZ,REGC,BASIS,PREV_BASIS)
				  else
				    begin %P>1\
				      MACRO4(OP_MOVE,REGC,BASIS,PREV_BASIS);
				      for I := 3 to P do
					MACRO4(OP_MOVE,REGC,REGC,PREV_BASIS);
				      if EXTENDED_ADDRESSING then
					MACRO4(OP_MOVE,REGC+1,REGC,PREV_BASIS)
				      else
					MACRO4(OP_HRLZ,REGC,REGC,PREV_BASIS)
				    end;
				if PFADDR = 0 then
				  begin
				    if EXTENDED_ADDRESSING then
				      MACRO3R(OP_XMOVEI,REGC,LINKCHAIN[P])
				    else
				      MACRO3R(OP_HRRI,REGC,LINKCHAIN[P]);
				    LINKCHAIN[P] := IC - 1;
				    if EXTERNDECL then
				      CODE.INFORMATION[CIX] := 'E'
				    else
				      CODE.INFORMATION[CIX] := 'F'
				  end
				else
				  if EXTENDED_ADDRESSING then
				    MACRO3R(OP_XMOVEI,REGC,PFADDR)
				  else
				    MACRO3R(OP_HRRI,REGC,PFADDR);
				if EXTENDED_ADDRESSING then
				  INCREMENTREGC;  {two regs for extended}
			      end %OF PFKIND = ACTUAL \
			    else
			      begin %PFKIND = FORMAL \
			      if P = 0 then
			        if EXTENDED_ADDRESSING then
				  MACRO4(OP_DMOVE,REGC,BASIS,PFADDR)
				else MACRO4(OP_MOVE,REGC,BASIS,PFADDR)
			      else
				begin
				  MACRO4(OP_MOVE,REGC,BASIS,PREV_BASIS);
				  for I := 2 to P do
				    MACRO4(OP_MOVE,REGC,REGC,PREV_BASIS);
				  if EXTENDED_ADDRESSING then
				    begin
				    MACRO4(OP_DMOVE,REGC,REGC,PFADDR);
				    INCREMENTREGC
				    end
			            else MACRO4(OP_MOVE,REGC,REGC,PFADDR);
				end;
			      if EXTENDED_ADDRESSING then
				INCREMENTREGC;  {two regs for extended}
			      end
			  end;
		    end %IF LB\
		  else
		    begin
		      STREGC := REGC;
		      if not CONF then
{if last was CONF, we are going to reuse this}
{The purpose of HAS_PAREN is to detect the use of (A) to cause call
 by copy in conformant arrays.  The compiler will normally consider
 (A) to be to be a VARBL, so the only way we can tell that the () 
 was there is to look for it explicitly!}
			begin
(* 250 *)
{In case there are conformant bounds, do an INCREMENTREGC for each one,
 so that REGC is set up to the register we want.  This matters only in
 the case of indexed references, etc., where something is going to be
 loaded into an AC.  Note that we allow REGC to go up to PARREGCMAX+1
 before calling EXPRESSION.  This means that the actual REGC used to
 load can go up to PARREGCMAX+2.  This is somewhat nonstandard.  Since
 the normal code won't let REGC go above PARREGCMAX+1, this makes sure
 that the anything that has to be left in an AC can always be left where
 it goes while we load up the bounds in lower AC's.  Note that in all
 such cases LOADADDRESS will reset REGC, and that if REGC is above
 PARREGCMAX, the normal code will save it at the right place in the
 display.  Thus we don't have to take any particular precautions to
 handle funny values of REGC.}
			  NXT1 := NXT;
			  while (NXT1 <> NIL) and (REGC <= PARREGCMAX) do
			    if NXT1^.KLASS=VARS then
			      if NXT1^.CONFARG then
				begin
				  CONF := TRUE;
				  INCREMENTREGC;
				  NXT1 := NXT1^.NEXT
				end
			      else NXT1 := NIL
			    else NXT1 := NIL;
			  HAS_PAREN := SY = LPARENT;
			  EXPRESSION(FSYS or [COMMA,RPARENT],ONFIXEDREGC);

{One might at first think that it would be easiest to always put REGC back,
 that it would be a no-op expect when we adjusted it just above.  However
 when a real expression is parsed, REGC is advanced, and we don't want to
 put it back.  This is not a problem with the conformant array case because
 conformant arrays are always variables, not expressions, so REGC is
 never advanced for them.}

			  if CONF then
			    REGC := STREGC;
			end;
		      CONF := FALSE;
		      if NXT <> NIL then
		        if NXT^.KLASS = VARS then
		          if NXT^.CONFARG then
			    CONF := TRUE;
		      if CONF then
			with NXT^ do
			      begin
{We have been asked to pass the bound of an array.  We have to find
 out which one and which bound}
				INCREMENTREGC;
				ARTYPE := GATTR.TYPTR;
			        for I := 2 to NXT^.SOURCE do
				  if ARTYPE <> NIL then
				    if ARTYPE^.FORM = ARRAYS then
				      ARTYPE := ARTYPE^.AELTYPE;
{FCONF remembers whether the formal was conformant}
				FCONF := FALSE;
			        if ARTYPE <> NIL then
				  if ARTYPE^.FORM = ARRAYS then
				    if ARTYPE^.INXTYPE <> NIL then
				    with ARTYPE^.INXTYPE^ do
				      if FORM = CONFORMANT then
					begin
					  FCONF := TRUE;
					  if ISUPPER then
					    BOUND := UPPER^.VADDR
					  else BOUND := LOWER^.VADDR;
					end
				      else 
					begin
					  if ISUPPER then
					    BOUND := MAX.IVAL
					  else BOUND := MIN.IVAL
					end
				  else BOUND := 0
				else BOUND := 0;
				OFFBOUND(REGC,ARTYPE,CINDEX,BOUND,0);
{If the conformant was a subrange type, make sure it fits}
				if NXT^.IDTYPE^.BOUNDTYPE <> NIL then
				  with NXT^.IDTYPE^.BOUNDTYPE^ do
				    if FORM = SUBRANGE then
				      if FCONF then {we are passing conf}
					if RUNTMCHECK then
					  begin
					  MACRO4(OP_CAML,REGC,CINDEX,MIN.IVAL);
					  MACRO4(OP_CAMLE,REGC,CINDEX,MAX.IVAL);
					  SUPPORT(INDEXERROR)
					  end
					else {no check}
				      else if (BOUND < MIN.IVAL) or
					      (BOUND > MAX.IVAL) then
					ERROR(558);
			      end
		      else
		        if GATTR.TYPTR # nil then
			begin
			  if (NXT # nil) or (LKIND = FORMAL) then
			    begin
{Very special handling for passing bounds on conformant array}
			      if NXT <> nil then
				begin
				  LSP := NXT^.IDTYPE; PKIND := NXT^.VKIND
				end
			      else {old-style parametric proc for this
				    case we call all arguments actual}
				begin
				  LSP := GATTR.TYPTR; PKIND := ACTUAL
				end;
			      if LSP # nil then
				begin
				  if PKIND = ACTUAL then
				    if LSP^.SIZE <= 2 then
				      begin
(* 236 - 4-word sets - handle [] *)
{ [] hack - see SETCONSTRUCTOR }
					if LSP^.FORM = POWER then
					  if GATTR.TYPTR^.FORM = QPOWER then
					    if GATTR.TYPTR^.ELSET = nil then
					      with GATTR.TYPTR^ do
					        begin
						  FORM := POWER;
						  SIZE := 2;
					        end;
					if (LSP = STRINGPTR) or
					  (LSP = POINTERPTR) or
					  (LSP = POINTERREF) then
					  PARAMFUDGE
					else
					  if LSP^.FORM = SUBRANGE then
					    LOADSUBRANGE(GATTR,LSP)
					  else
					    LOAD(GATTR);
					if COMPTYPES(REALPTR,LSP)
					  and (GATTR.TYPTR = INTPTR) then
					  MAKEREAL(GATTR)
				      end
				    else
				      begin {LSP^.SIZE > 2}
					LOADADDRESS;
					{note - the address loaded into REGC
					here will be used in a BLT a couple of
					pages down from here.  For extended
					addressing an XBLT will be done so load
					the whole address}
					if (LKIND = ACTUAL) and (FCP^.LANGUAGE # PASCALSY)
					  and (not EXTENDED_ADDRESSING) then
					  CODE.INSTRUCTION[CIX].INSTR := OP_HRLI
				      end
				  else
				    {PKIND # ACTUAL}
				    if GATTR.KIND in [VARBL,CST] then
				      begin
				      CHECKASSIGN(GATTR);
{If he is trying to pass a constant by reference, set the flag for
 making a local copy.}
				      if GATTR.KIND = CST
					then HAS_PAREN := TRUE;
{If he is making a local copy, make sure it is kosher}
				      if HAS_PAREN then
					if NXT <> NIL then
					  with NXT^ do
					    if IDTYPE <> NIL then
					      with IDTYPE^ do 
						if (FORM = ARRAYS) and
						    ARRAYCONF then
						      {OK}
						else ERROR(463) {not CONF}
					    else
					else ERROR(463); {unknown kind
						of arg, from old-style
						parametric proc}
				      LOADADDRESS;
{Here we handle the hairy kludge perpetrated by ISO:  (A) is supposed
 to cause us to make a local copy and then pass the address of the
 anonymous local.  We have to make sure that the thing we are passing
 is not a conformant array, since if it were we wouldn't know how
 much space to allocate to the copy on the stack!}
				      if has_paren
					then begin
					if GATTR.TYPTR^.INXTYPE <> NIL
					  then if GATTR.TYPTR^.INXTYPE^.FORM =
						 CONFORMANT
					    then error(318)
					    else begin
					    temp_copy := 
						temp_lc(gattr.typtr^.size);
					    if extended_addressing then
					      begin
					      incrementregc;
					      incrementregc;
{We now have the current address of the thing, but it is in the next lower
 AC. We need to have a length code.  So put it in the right AC}
					      code.instruction[cix].ac := 
						regc-1;
					      macro3c(op_move,regc-2,
						gattr.typtr^.size);
					      macro4(op_xmovei,regc,basis,
						temp_copy);
					      macro3r(op_xtnd,regc-2,
						xblt_loc);
					      regc := regc - 2;
					      macro4(op_xmovei,regc,basis,
						temp_copy);
					      end
					     else begin
					      code.instruction[cix].instr :=
						op_hrli;
					      macro4(op_hrri,regc,basis,
						temp_copy);
					      macro4(op_blt,regc,basis,
						temp_copy+gattr.typtr^.size -
						1);
					      macro4(op_movei,regc,basis,
						temp_copy);
					      end
					    end {not conformant}
					end {need local copy}
				      end {it was a VARBL}
				    else
				      ERROR(463);
				  (* ALLOW EXTERNAL FILE REFERENCES *)
				  if GATTR.TYPTR#nil then
				    if GATTR.TYPTR^.FORM=FILES then
				      if GATTR.EXTERNCTP <> nil then
					begin
					  GATTR.EXTERNCTP^.VADDR := IC - 1;
					  CODE.INFORMATION[CIX] := 'E'
					end;
				  (* proc param's that don't fit in ac's *)
				  if not COMPTYPES(LSP,GATTR.TYPTR) then
				    ERROR(503)
				end
			    end
			end
		    end;
		  if REGC>PARREGCMAX then
{We punt in an old-style parametric procedure if the 
args don't fit in the AC's}
		      if NXT=NIL then
			ERROR(413)
		      else
			begin
			  if TOPPOFFSET = 0 then
			    begin
			      if LKIND = ACTUAL then
			        LNXT := FCP^.NEXT
			      else LNXT := FCP^.PFCHAIN; {find arg list
							in odd place for
							parametric procs}
			      if (LKIND = FORMAL) or
				 (FCP^.LANGUAGE = PASCALSY) then
				TOPPOFFSET := FCP^.POFFSET + 1
			      else
				begin
				  TOPPOFFSET := 1 + FIRSTPAR;
				  repeat
				    with LNXT^ do
				      begin
					NOFPAR := NOFPAR +1;
					TOPPOFFSET := TOPPOFFSET + 1;
					if VKIND = ACTUAL then
					  TOPPOFFSET := TOPPOFFSET + IDTYPE^.SIZE;
					/* if LKIND = ACTUAL then */
					  LNXT := NEXT
				      end;
				  until LNXT = nil;
				  PARLIST := 1 + FIRSTPAR;
				  ACTUALPAR := PARLIST + NOFPAR
				end;
			      (* TOPS20 DETECTION OF STACK OVERFLOW *)
			      if KLCPU and not TOPS10 and not EXTENDED_ADDRESSING then
				MACRO3(OP_ADJSP,TOPP,TOPPOFFSET)
			      else
				MACRO3C(OP_ADD,TOPP,TOPPOFFSET);
			      (* keep track of how many loc's above stack are used *)
			      STKOFF := STKOFF + TOPPOFFSET;
			      if STKOFF > STKOFFMAX then
				STKOFFMAX := STKOFF
			    end;
			  with NXT^ do
			    begin
			      if (LKIND = FORMAL) or
				 (FCP^.LANGUAGE = PASCALSY) then
				if KLASS # VARS then
				  (* parameter proc's that don't fit in ac's *)
				  if EXTENDED_ADDRESSING then
				    begin
				      REGC := REGC - 1;
				      MACRO4(OP_DMOVEM,REGC,TOPP,PFADDR+1-TOPPOFFSET)
				    end
				  else MACRO4(OP_MOVEM,REGC,TOPP,PFADDR+1-TOPPOFFSET)
				else
				  if KACPU then
				    begin
				      if (VKIND=ACTUAL) and (IDTYPE^.SIZE=2) then
					begin
					  MACRO4(OP_MOVEM,REGC,TOPP,VADDR+2-TOPPOFFSET);
					  REGC := REGC - 1
					end;
				      if (IDTYPE^.SIZE > 0)
					or (VKIND <> ACTUAL) then
					MACRO4(OP_MOVEM,REGC,TOPP,VADDR+1-TOPPOFFSET)
				    end
				  else
				    {NOT KACPU}
				    if (VKIND=ACTUAL) and (IDTYPE^.SIZE=2) then
				      begin
					REGC := REGC - 1;
					MACRO4(OP_DMOVEM,REGC,TOPP,VADDR+1-TOPPOFFSET)
				      end
				    else
				      MACRO4(OP_MOVEM,REGC,TOPP,VADDR+1-TOPPOFFSET)
			      else
				if KLASS # VARS then
				  (* proc param's that don't fit in ac's *)
				  ERROR(466)
				else
				  begin
				    if VKIND = ACTUAL then
				      begin
					if IDTYPE^.SIZE <= 2 then
					  if KACPU then
					    begin
					      if IDTYPE^.SIZE = 2 then
						begin
						  MACRO4(OP_MOVEM,REGC,TOPP,ACTUALPAR+1-TOPPOFFSET);
						  REGC := REGC - 1
						end;
					      if (IDTYPE^.SIZE > 0) then
						MACRO4(OP_MOVEM,REGC,TOPP,ACTUALPAR-TOPPOFFSET);
(* 226 - the HRRI was missing - this broke Fortran calling for KA'S *)
					      MACRO4(OP_HRRI,REGC,TOPP,ACTUALPAR-TOPPOFFSET)
					    end
					  else
					    begin {NOT KACPU}
					      if IDTYPE^.SIZE = 2 then
						begin
						  REGC := REGC - 1;
						  MACRO4(OP_DMOVEM,REGC,TOPP,ACTUALPAR-TOPPOFFSET)
						end
					      else
						MACRO4(OP_MOVEM,REGC,TOPP,ACTUALPAR-TOPPOFFSET);

					      if EXTENDED_ADDRESSING then
						MACRO4(OP_XMOVEI,REGC,TOPP,ACTUALPAR-TOPPOFFSET)
					        else MACRO4(OP_HRRI,REGC,TOPP,ACTUALPAR-TOPPOFFSET)
					    end
					else
					  {IDTYPE^.SIZE > 2}
					  if EXTENDED_ADDRESSING then
					    begin
					      MACRO3C(OP_MOVE,TAC0,IDTYPE^.SIZE);
					      MACRO3(OP_MOVE,TAC1,REGC);
					      if REGC > 2 then
						MACRO3(OP_MOVE,BIXREG,TAC2);
					      MACRO4(OP_XMOVEI,TAC2,TOPP,ACTUALPAR-TOPPOFFSET);
					      MACRO3R(OP_XTND,TAC0,XBLT_LOC);
					      if REGC > 2 then
						MACRO3(OP_MOVE,TAC2,BIXREG);
					      MACRO4(OP_XMOVEI,REGC,TOPP,ACTUALPAR-TOPPOFFSET)
					    end
					  else
					    begin {NOT EXTENDED ADDRESSING}
					      {note for the non-extended version
					      an HRLI was done a couple of pages
					      back.}
					      MACRO4(OP_HRRI,REGC,TOPP,ACTUALPAR-TOPPOFFSET);
					      MACRO4(OP_BLT,REGC,TOPP,ACTUALPAR+IDTYPE^.SIZE-1-TOPPOFFSET);
					      (* BLT may change regc, so reset it since used below *)
					      MACRO4(OP_HRRI,REGC,TOPP,ACTUALPAR-TOPPOFFSET)
					    end;
					ACTUALPAR := ACTUALPAR + IDTYPE^.SIZE
				      end;
				    if EXTENDED_ADDRESSING then
				      MACRO4(OP_MOVEM,REGC,TOPP,PARLIST-TOPPOFFSET)
				    else
				      MACRO4(OP_HRRZM,REGC,TOPP,PARLIST-TOPPOFFSET);
				    PARLIST := PARLIST + 1
				  end;
			      REGC := PARREGCMAX
			    end
			end;
		  if /*(LKIND = ACTUAL) and */ (NXT # nil) then
		    NXT := NXT^.NEXT;
		until (SY # COMMA) and not CONF;
		if SY = RPARENT then
		  INSYMBOL
		else
		  ERROR(152)
	      end %IF LPARENT\;
	    for I := 0 to WITHIX do
	      with DISPLAY[TOP-I] do
		if (CINDR # 0) and (CINDR # BASIS) then
		  MACRO4(OP_MOVEM,CINDR,BASIS,CLC);
	    with FCP^ do
	      begin
		if LKIND = FORMAL then
		  begin		  
		  if TOPPOFFSET <> 0 then
		    STKOFF := STKOFF - TOPPOFFSET;
		  end
		else
		  if (LANGUAGE = PASCALSY) and (TOPPOFFSET # 0) then
		    (* keep track of offsets above top of stack *)
		    STKOFF := STKOFF - TOPPOFFSET
		  else
		    if (LANGUAGE # PASCALSY) and (TOPPOFFSET = 0) then
		      begin
			TOPPOFFSET:= FIRSTPAR+2;
			if KLCPU and not TOPS10 and not EXTENDED_ADDRESSING then
			  MACRO3(OP_ADJSP,TOPP,TOPPOFFSET)
			else
			  MACRO3C(OP_ADD,TOPP,TOPPOFFSET);
			(* keep track of how many loc's above stack are used *)
			STKOFF := STKOFF + TOPPOFFSET;
			if STKOFF > STKOFFMAX then
			  STKOFFMAX := STKOFF
		      end;
		if PFLEV > 1 then
		  P := LEVEL - PFLEV
		else
		  P:= 0;
		if LKIND = ACTUAL then
		  begin
		    if NXT # nil then
		      ERROR(554);
		    if LANGUAGE # PASCALSY then
		      begin
			MACRO3(OP_MOVSI,TAC0,-NOFPAR);
			MACRO4(OP_MOVEM,TAC0,TOPP,FIRSTPAR-TOPPOFFSET);
			MACRO4(OP_MOVEM,BASIS,TOPP,-TOPPOFFSET);
(* 246 - save NEWREG *)
			if TOPS10 then
			  if KLASS = PROC then
			    MACRO4(OP_MOVEM,NEWREG,TOPP,1-TOPPOFFSET);
			MACRO4(OP_XMOVEI,BASIS,TOPP,FIRSTPAR-TOPPOFFSET+1);
			if NOFPAR = 0 then
			  MACRO4(OP_SETZM,0,TOPP,FIRSTPAR-TOPPOFFSET+1)
		      end;
	     	    if (LANGUAGE = PASCALSY) and EXTENDED_ADDRESSING then
		      MACRO3(OP_MOVE,DYNSUP,BASIS);
		    if PFADDR = 0 then
		      begin
			MACRO3R(OP_PUSHJ,TOPP,LINKCHAIN[P]); LINKCHAIN[P]:= IC-1;
			if EXTERNDECL then
			  CODE.INFORMATION[CIX] := 'E'
			else
			  CODE.INFORMATION[CIX] := 'F'
		      end
		    else
		      MACRO3R(OP_PUSHJ,TOPP,PFADDR-P);
		    if LANGUAGE # PASCALSY then
		      begin
			if KLCPU and not TOPS10 and not EXTENDED_ADDRESSING then
			  MACRO3(OP_ADJSP,TOPP,-TOPPOFFSET)
			else
			  MACRO3C(OP_SUB,TOPP,TOPPOFFSET);
			(* keep track of how many loc's above stack are used *)
			STKOFF := STKOFF - TOPPOFFSET;
			if KLASS = FUNC then
			  if KACPU then
			    begin
			      MACRO4(OP_MOVEM,TAC0,TOPP,2);
			      if IDTYPE^.SIZE = 2 then
				MACRO4(OP_MOVEM,TAC1,TOPP,3)
			    end
			  else
			    if IDTYPE^.SIZE = 2 then
			      MACRO4(OP_DMOVEM,TAC0,TOPP,2)
			    else
			      MACRO4(OP_MOVEM,TAC0,TOPP,2);
			MACRO4(OP_MOVE,BASIS,TOPP,0);
(* 246 - save NEWREG for Fortran procedures *)
			if TOPS10 then
			  if KLASS = PROC then
			    MACRO4(OP_MOVE,NEWREG,TOPP,1);
		      end
		  end  (* OF LKIND = ACTUAL *)
		else
		  begin
		    if EXTENDED_ADDRESSING then
		      MACRO3(OP_MOVE,DYNSUP,BASIS);
		    if P = 0 then
		      begin
			MACRO4(OP_MOVE,TAC1,BASIS,PFADDR);
			if EXTENDED_ADDRESSING then
			  MACRO4(OP_MOVE,BASIS,BASIS,PFADDR+1)
			else
			  MACRO4(OP_HLR,BASIS,BASIS,PFADDR)
		      end
		    else
		      begin
			MACRO4(OP_MOVE,TAC1,BASIS,PREV_BASIS);
			for I := 2 to P do
			  MACRO4(OP_MOVE,TAC1,TAC1,PREV_BASIS);
			if EXTENDED_ADDRESSING then
			  MACRO4(OP_MOVE,BASIS,TAC1,PFADDR+1)
			else
			  MACRO4(OP_HLR,BASIS,TAC1,PFADDR);
			MACRO4(OP_MOVE,TAC1,TAC1,PFADDR)
		      end;
		    MACRO4(OP_PUSHJ,TOPP,TAC1,0)
		  end
	      end;
	    for I := 0 to WITHIX do
	      with DISPLAY[TOP-I] do
		if (CINDR # 0) and (CINDR # BASIS) then
		  MACRO4(OP_MOVE,CINDR,BASIS,CLC);
	    if SAVECOUNT > 0 then
	      begin
		if KACPU then
		  if SAVECOUNT > 3 then
		    begin
		      MACRO4(OP_HRLI,TAC1,BASIS,LLC);
		      MACRO3(OP_HRRI,TAC1,TAC2);
		      MACRO3(OP_BLT,TAC1,SAVECOUNT+1)
		    end
		  else
		    for I := 1 to SAVECOUNT do
		      MACRO4(OP_MOVE,REGIN+I,BASIS,LLC+I-1)
		else
		  begin {NOT KACPU}
		    I := 1;
		    while I < SAVECOUNT do
		      begin
			MACRO4(OP_DMOVE,REGIN+I,BASIS,LLC+I-1);
			I := I + 2
		      end;
		    if I = SAVECOUNT then
		      MACRO4(OP_MOVE,REGIN+I,BASIS,LLC+I-1)
		  end;
(* 233 - better temp allocation *)
		{if SAVECOUNT > 0 then
		  DISP_LC(LLC,SAVECOUNT);}
	      end;
	    GATTR.TYPTR := FCP^.IDTYPE; REGC := LREGC
	  end %CALLNONSTANDARD\;

	begin %CALL\
	  if FCP^.PFDECKIND = STANDARD then
	    begin
	      LKEY := FCP^.KEY;
	      if FCP^.KLASS = PROC then
		begin
		  if not (LKEY in [7,8,9,10,11,17,19,25,34,39,42] ) then
		    if SY = LPARENT then
		      INSYMBOL
		    else
		      ERROR(153);
		  (* APPEND, UPDATE, RENAME, use REG5 and REG6 *)
		  if (LKEY in [5,6,7,8,10,11,27,29,36]) and (REGCMAX <= 8) then
		    ERROR(317);
		  %REGISTER USED BY RUNTIME SUPPORT FREE OR NOT  \
		  case LKEY of
		    2,4,
		    5,6,27,28,29,36:  GETPUTRESETREWRITE;
		    7,
		    8:
		      begin
			READREADLN;
			if NORIGHTPARENT then
			  goto 9
		      end;
		    9:
		      begin
			BREAK;
			if NORIGHTPARENT then
			  goto 9
		      end;
		    10,
		    11:
		      begin
			WRITEWRITELN;
			if NORIGHTPARENT then
			  goto 9
		      end;
		    12:    PACK;
		    13:    UNPACK;
		    1,3,14,35,40,44,45:   NEW;
		    15:    MARK;
		    16:    RELEASE;
		    17:    GETLINENR;
		    18:    PUT8BITSTOTTY;
		    19:
		      begin
			PAGE;
			if NORIGHTPARENT then
			  goto 9
		      end;
		    21:    PROTECTION;
		    22,23:  SETSTRING;
		    24:     GETINDEX;
		    25,34,39,42:    begin
		      CLOSE;
		      if NORIGHTPARENT then
			goto 9
		    end;
		    26:CALLI;
		    30,31:DUMP;
		    32,33,38:USET;
		    37,41:PUTX;
		    43:JSYS
		    end
		end
	      else
		begin
		  if not (LKEY in [1,2,11,12]) then
		    begin
		      if SY = LPARENT then
			INSYMBOL
		      else
			ERROR(153);
		      if LKEY#15 then
			EXPRESSION(FSYS or [RPARENT],ONREGC);
		      if not (LKEY in [7,8,11,12,15]) then
			LOAD(GATTR)
		    end;
		  case LKEY of
		    1:    RUNTIME;
		    2:    TIME;
		    3:    ABS;
		    4:    SQR;
		    5,14:    TRUNC;
		    6:    ODD;
		    7:    ORD;
		    8:    CHR;
		    9,10:  PREDSUCC;
		    11,12: begin
		      EOFEOLN;
		      if NORIGHTPARENT then
			goto 9
		    end;
		    15: NEW
		  end;
		  if LKEY < 3 then
		    goto 9
		end;
	      if SY = RPARENT then
		INSYMBOL
	      else
		ERROR(152);
	9:
	    end %STANDARD PROCEDURES AND FUNCTIONS\
	  else
	    CALLNONSTANDARD
	end %CALL\;
      procedure EXPRESSION;
	var
	  LATTR: ATTR; LOP: OPERATOR; LSIZE: XADDRRANGE;
	  LOFFSET: INTEGER; DEFAULT,NEEDSHIFT: BOOLEAN;
	  BOOLREGC,TESTREGC:ACRANGE; LINSTR,LINSTR1: INSTRANGE;
	  LREGC1, LREGC2, LREGC3, LREGC4, LREGC5: ACRANGE;
(* 235 - 4-word set equality *)
	  NEGATE,SETINCLUSION : BOOLEAN;
(* 240 - conformant *)
	  LMIN,LMAX: XADDRRANGE;
	  CONF: BOOLEAN;

	procedure CHANGEBOOL(var FINSTR: INSTRANGE);
	  begin
	    if (FINSTR >= OP_CAML) and (FINSTR <= OP_CAMLE) then
	      FINSTR := FINSTR+4  %CAML,CAME,CAMLE --> CAMGE,CAMN,CAMG\
	    else
	      if (FINSTR >= OP_CAMGE) and (FINSTR <= OP_CAMG) then
		FINSTR := FINSTR-4  %SAME IN THE OTHER WAY\;
	  end;

	procedure SEARCHCODE(FINSTR:INSTRANGE; FATTR: ATTR);
	  procedure CHANGEOPERANDS(var FINSTR:INSTRANGE);
	    begin
	      if FINSTR=OP_CAML then
		FINSTR := OP_CAMG
	      else
		if FINSTR = OP_CAMLE then
		  FINSTR := OP_CAMGE
		else
		  if FINSTR=OP_CAMGE then
		    FINSTR := OP_CAMLE
		  else
		    if FINSTR = OP_CAMG then
		      FINSTR := OP_CAML
		    else
		      if FINSTR = OP_ANDCM then
			FINSTR := OP_ANDCA
		      else
			if FINSTR = OP_ANDCA then
			  FINSTR := OP_ANDCM
	    end;

	  begin
	    if GATTR.TYPTR <> nil then
	      if GATTR.TYPTR^.FORM = QPOWER then
		begin
		  MAKEQCODE(FINSTR,FATTR);
		  GOTO 666
		end;
	    with GATTR do
	      if FATTR.KIND = EXPR then
		begin
		  MAKECODE(FINSTR,FATTR.REG,GATTR); REG := FATTR.REG
		end
	      else
		if KIND = EXPR then
		  begin
		    CHANGEOPERANDS(FINSTR); MAKECODE(FINSTR,REG,FATTR)
		  end
		else
		  if (KIND=VARBL) and ((PACKFG#NOTPACK)
				       or (INDEXR>REGIN) and (INDEXR<=REGCMAX) and
				       ((FATTR.INDEXR<=REGIN) or (FATTR.INDEXR>REGCMAX))) then
		    begin
		      LOAD(GATTR); CHANGEOPERANDS(FINSTR); MAKECODE(FINSTR,REG,FATTR)
		    end
		  else
		    begin
		      LOAD(FATTR); MAKECODE(FINSTR,FATTR.REG,GATTR);
		      REG := FATTR.REG
		    end;
666:
	  end;

{Sorry about this intentation, but I don't like the right hand side
 going off my screen.  Despite appearances, this is part of FACTOR}

(* 235 - new set constructor code for 4-word sets *)

{Here is the place that we explain the [] kludge.  Somebody once told me
 it was a bad idea to have two different sizes of sets.  They were right.
 I am doing it because the efficiency differences between 2 and 4 words
 are so great.  The only problem is, what does [] mean?  In this routine,
 I make it a CHAR set.  COMPTYPES is hacked so that whenever you compare
 a QPOWER with null ELTYPE against a POWER (we hope that is the only
 case where there is a QPOWER will null type), we turn it into a POWER,
 i.e. a size-2 empty set.  This works in EXPRESSION and its subroutines.
 However in CALLNONSTANDARD and one other place, they don't call
 COMPTYPES until after generating code, so we have to check explicitly
 there.  Also, in DEPCST, we have to handle this changed constant.  Although
 it will be POWER, with size of 2, it will still show as CPSET in its
 constant record.  So DEPCST has to check for this case and turn it into
 PSET.  This is because PSET's get 2 fixups and CPSET's only 1.  So it
 does matter.  Now you may ask - this is all fine, but what about
 [] + [].  Well...., MAKEQCODE will cause this to have the same type,
 namely QPOWER with ELTYPE = NIL.  So it will be converted when necessary
 also.}

procedure SETCONSTRUCTOR;
var
  CSTPART: set of 0..71;
  CCSTPART: array[0..3] of packed array[0..35] of Boolean;
  VARPART, RANGEPART: BOOLEAN;
  LSP: STP;
  LRMIN: INTEGER;
  LATTR: ATTR;
  VARLOC: XADDRRANGE;
  INITIX: CODERANGE;
  I,J: INTEGER;
  LVP:CSP;

  procedure INITVARLOC(workreg:acrange);
{if extended addressing is in effect, we can also use workreg - 1}
  begin
  if not varpart
    then begin
    varpart := true;
    if extended_addressing then
      begin
      macro3(op_movei,workreg-1,4);
      macro(right,op_xmovei,workreg,1,0,fix_up);
      initix := cix;
      macro4(op_xmovei,workreg+1,basis,varloc);
      macro3r(op_xtnd,workreg-1,xblt_loc)
      end
     else
      begin
      macro(right,op_hrli,workreg,1,0,fix_up);
      initix := cix;
      macro4(op_hrri,workreg,basis,varloc);
      macro4(op_blt,workreg,basis,varloc+3);
      end;
    end
  end;

begin
  INSYMBOL; CSTPART := [ ]; VARPART := FALSE;
  VARLOC := TEMP_LC(4);
  for I := 0 to 3 do
    for J := 0 to 35 do
      CCSTPART[I,J] := FALSE;
  NEWZ(LSP,QPOWER);
  with LSP^ do
    begin
      ELSET := nil; SIZE:= 4
    end;
  if SY = RBRACK then
    begin
      with GATTR do
	begin
	  TYPTR:=LSP; KIND:=CST;
	  NEWZ(LVP,CPSET); LVP^.CARVAL := CCSTPART;
	  CVAL.VALP := LVP
	end;
      INSYMBOL
    end
  else
    begin {SY # RBRACK}
      (* AC usage in the following is documented at the end.  In order to get
       any sanity at all, REGC has to be kept the same whatever the expression
       types found.  Since an expression will advance REGC in most cases, we
       have to be sure it gets advanced in others.  This means incrementregc
       for constants and LOAD otherwise.  We don't LOAD constants because if
       the other half of the range is also constant we will just remember it
       as constant and not do a load at all. *)

{The following loop is folded.  This is really part of the loop}
    (* RANGEPART IS FLAG: 1ST EXPRESSION IS VARIABLE *)
    RANGEPART := FALSE;
    if EXTENDED_ADDRESSING then
      INCREMENTREGC;
    INCREMENTREGC; INCREMENTREGC;  (* FIRST EXPR *)
    EXPRESSION(FSYS or [COMMA,RBRACK,COLON],ONFIXEDREGC);
    if COMPTYPES (GATTR.TYPTR,CHARPTR) then
      begin {SET OF CHAR}
      LSP^.ELSET := CHARPTR;
      loop
	if GATTR.TYPTR # nil then
	  if GATTR.TYPTR^.FORM # SCALAR then
	    begin
	      ERROR(461); GATTR.TYPTR := nil
	    end
	  else
	    if COMPTYPES(GATTR.TYPTR,CHARPTR) then
	      begin (* LOAD IF VAR, SAVE IN LRMIN IF CONST *)
		if GATTR.KIND = CST then
		  begin (* FIRST EXPR IS CONST *)
		    INCREMENTREGC;
		    if (GATTR.CVAL.IVAL<0)
		      or (GATTR.CVAL.IVAL>CHARMAX) then
		      begin
			ERROR(352); GATTR.CVAL.IVAL := 0
		      end;
		    LRMIN := GATTR.CVAL.IVAL;
		  end
		else
		  begin (* FIRST EXPR IS NOT A CONSTANT *)
		    RANGEPART := TRUE; (* SIGNAL VARIABLE *)
		    LOAD(GATTR);
		    (* range check sets *)
		    if RUNTMCHECK then
		      begin
			MACRO3(OP_CAIG,REGC,CHARMAX);
			MACRO3(OP_CAIGE,REGC,0);
			SUPPORT(ERRORINASSIGNMENT)
		      end;
		  end;
		if SY <> COLON then (* ONLY ONE EXPR *)
		  if not RANGEPART then
		    begin (* CONSTANT *)
		      CSTPART := [1];
		      CCSTPART[LRMIN DIV 32,LRMIN MOD 32] := TRUE;
		      if EXTENDED_ADDRESSING then
			REGC := REGC - 4
		      else REGC := REGC - 3;
		    end
		  else
		    begin (* ONE VARIABLE *)
		    regc := regc - 1;
		    initvarloc(regc-1);
		    macro3(op_move,regc,regc+1);
		    macro3(op_lsh,regc,-5);
		    if extended_addressing then
		      begin
		      macro4(op_xmovei,tac0,basis,varloc);
		      macro3(op_add,regc,tac0)
		      end
		     else macro4(op_addi,regc,basis,varloc);
		    macro3(op_andi,regc+1,37B);
		    macro3(op_movn,regc+1,regc+1);
		    macro3(op_movsi,regc-1,400000B);
		    macro4(op_lsh,regc-1,regc+1,0);
		    macro4(op_iorm,regc-1,regc,0);
		    if extended_addressing then
		      regc := regc - 3
		    else regc := regc - 2;
		    end
		else
		  begin (* RANGE *)
		    INSYMBOL;
		    EXPRESSION(FSYS or [COMMA,RBRACK],ONFIXEDREGC);
		    if GATTR.TYPTR <> nil (* 2ND EXPR *)
		    then
		      if GATTR.TYPTR^.FORM <> SCALAR then
			begin
			  ERROR(461);
			  GATTR.TYPTR := nil
			end
		      else
			{SCALAR}
			if COMPTYPES(GATTR.TYPTR,CHARPTR) then
			  begin
			    if GATTR.KIND = CST then
			      begin
				INCREMENTREGC;
				if (GATTR.CVAL.IVAL < 0)
				  or (GATTR.CVAL.IVAL > CHARMAX) then
				  begin
				    ERROR(352); GATTR.CVAL.IVAL := 0
				  end;
			      end
			    else
			      LOAD(GATTR);
			    if (GATTR.KIND = CST) and (not RANGEPART) then
			      begin (* CONSTANT RANGE *)
				while(LRMIN <= GATTR.CVAL.IVAL) do
				  begin
				    CSTPART := [0];
				    CCSTPART[LRMIN DIV 32,LRMIN MOD 32] := 
									true;
				    LRMIN := LRMIN+1
				  end;
				if EXTENDED_ADDRESSING then
				  REGC := REGC - 5
				else REGC := REGC - 4
			      end
			    else
			      begin (* VARIABLE LIMITS ON RANGE *)
				if not RANGEPART then
				  (* FIRST PART IS CONSTANT *) (* SO NOT IN AC YET *)
				  MACRO3(OP_MOVEI,REGC-1,LRMIN);
				if GATTR.KIND = CST then  (* same for second *)
				  MACRO3(OP_MOVEI,REGC,GATTR.CVAL.IVAL);
				if (GATTR.KIND <> CST) and RUNTMCHECK then
				  begin
				    MACRO3(OP_CAIG,REGC,CHARMAX);
				    MACRO3(OP_CAIGE,REGC,0);
				    SUPPORT(ERRORINASSIGNMENT);
				  end;
				(* HERE IS WHAT IS IN THE AC'S:
				 REGC+1  - work
				 REGC    - RH LIMIT --> address
				 REGC-1  - LH LIMIT --> address
				 REGC-2  - mask for RH limit
				 REGC-3  - mask for LH limit
				 *)
				initvarloc(regc-3);
				macro3(op_camle,regc-1,regc);
			        if extended_addressing then
				  macro3r(op_jrst,0,ic+27)
				else macro3r(op_jrst,0,ic+26);
				macro3(op_setob,regc-2,regc-3);
				macro3(op_move,regc+1,regc);
				macro3(op_andi,regc+1,37B);
				macro4(op_movni,regc+1,regc+1,1);
				macro4(op_lsh,regc-2,regc+1,0);
				macro3(op_setca,regc-2,0);
				macro3(op_lsh,regc,-5);
				if extended_addressing
				  then begin
				  macro4(op_xmovei,tac0,basis,varloc);
				  macro3(op_add,regc,tac0)
			          end
				 else macro4(op_addi,regc,basis,varloc);
				macro3(op_move,regc+1,regc-1);
				macro3(op_andi,regc+1,37B);
				macro3(op_movn,regc+1,regc+1);
				macro4(op_lsh,regc-3,regc+1,0);
				macro3(op_trz,regc-3,17B);
				macro3(op_lsh,regc-1,-5);
				if extended_addressing
				  then macro3(op_add,regc-1,tac0)
				  else macro4(op_addi,regc-1,basis,varloc);
				macro3(op_camn,regc-1,regc);
				macro3(op_andb,regc-2,regc-3);
				macro4(op_iorm,regc-3,regc-1,0);
				macro3(op_aoj,regc-1,0);
				macro3(op_caml,regc-1,regc);
				macro3r(op_jrst,0,ic+4);
				macro3(op_hrroi,tac0,777760B);
				macro4(op_movem,0,regc-1,0);
				macro3r(op_aoja,regc-1,ic-4);
				macro4(op_iorm,regc-2,regc,0);
				if extended_addressing then
				  regc := regc - 5
				else REGC := REGC -4;
			      end (* VARIABLE LIMITS ON RANGE? *)
			  end {SCALAR?}
		  end;
	      end
	    else
	      ERROR(360);
      exit if not(SY in [COMMA]);
	INSYMBOL;
	(* RANGEPART IS FLAG: 1ST EXPRESSION IS VARIABLE *)
	RANGEPART := FALSE;
	if EXTENDED_ADDRESSING then
	  INCREMENTREGC;
	INCREMENTREGC; INCREMENTREGC;  (* FIRST EXPR *)
	EXPRESSION(FSYS or [COMMA,RBRACK,COLON],ONFIXEDREGC);
    end {LOOP};
      if SY = RBRACK then
	INSYMBOL
      else
	ERROR(155);
      if VARPART then
	begin
          with GATTR do
	    begin
	      DPLMT := VARLOC; INDEXR := BASIS; INDBIT:=0; VRELBYTE := NO;
	      VLEVEL := 1; EXTERNCTP := NIL; TYPTR := LSP; KIND := VARBL;
	      PACKFG := NOTPACK;
	    end;
	  NEW(LVP,CPSET);LVP^.CARVAL := CCSTPART;
	  LATTR.KIND:=CST; LATTR.CVAL.VALP := LVP;
{This is a no-op - it is here to provide the address of the constant
 portion.  We can't do the DEPCST until the end, since we don't have
 all the constant stuff until then.  So we fix up the code done in
 INITVARLOC to point here.  In the old version, we just OR'ed the
 contant part in here.  However in this version we are using an in-core
 work area.  Since it would have to be cleared, it is more efficient
 to initialize it to the constant part, even at the cost of one no-op}
	  MACRO3(OP_SETA,0,FIX_UP);
	  DEPCST(CPSET,LATTR);
	  INSERTADDR(RIGHT,INITIX,IC-1);	
	end
      else
	begin
	  NEWZ(LVP,CPSET); LVP^.CARVAL := CCSTPART;
	  GATTR.CVAL.VALP := LVP; GATTR.TYPTR := LSP;
	end
      end {SET OF CHAR}
    else {SET OF INTEGER}
      begin
      LSP^.FORM := POWER;  {was initialized to QPOWER}
      LSP^.SIZE := 2;
      LSP^.ELSET := GATTR.TYPTR;          
      loop
	if GATTR.TYPTR # nil then
	  if GATTR.TYPTR^.FORM # SCALAR then
	    begin
	      ERROR(461); GATTR.TYPTR := nil
	    end
	  else
	    if COMPTYPES(LSP^.ELSET,GATTR.TYPTR) then
	      begin (* LOAD IF VAR, SAVE IN LRMIN IF CONST *)
		if GATTR.KIND = CST then
		  begin (* FIRST EXPR IS CONST *)
		    INCREMENTREGC;
		    if (GATTR.CVAL.IVAL<0)
		      or (GATTR.CVAL.IVAL>BASEMAX) then
		      begin
			ERROR(352); GATTR.CVAL.IVAL := 0
		      end;
		    LRMIN := GATTR.CVAL.IVAL;
		  end
		else
		  begin (* FIRST EXPR IS NOT A CONSTANT *)
		    RANGEPART := TRUE; (* SIGNAL VARIABLE *)
		    LOAD(GATTR);
		    (* range check sets *)
		    if RUNTMCHECK then
		      begin
			MACRO3(OP_CAIG,REGC,BASEMAX);
			MACRO3(OP_CAIGE,REGC,0);
			SUPPORT(ERRORINASSIGNMENT)
		      end;
		  end;
		if SY <> COLON then (* ONLY ONE EXPR *)
		  if not RANGEPART then
		    begin (* CONSTANT *)
		      CSTPART := CSTPART or [LRMIN];
		      REGC := REGC - 3;
		    end
		  else
		    begin (* ONE VARIABLE *)
		      MACRO3(OP_MOVN,REGC,REGC);
		      REGC := REGC - 1;
		      MACRO3(OP_MOVSI,REGC-1,400000B);
		      MACRO3(OP_SETZ,REGC,0);
		      MACRO4(OP_LSHC,REGC-1,REGC+1,0);
		      if VARPART then
			begin
			  MACRO3(OP_IOR,REGC-3,REGC-1);
			  MACRO3(OP_IOR,REGC-2,REGC);
			  REGC := REGC-2
			end
		      else
			VARPART := TRUE;
		      GATTR.KIND := EXPR; GATTR.REG := REGC
		    end
		else
		  begin (* RANGE *)
		    INSYMBOL;
		    EXPRESSION(FSYS or [COMMA,RBRACK],ONFIXEDREGC);
		    if GATTR.TYPTR <> nil (* 2ND EXPR *)
		    then
		      if GATTR.TYPTR^.FORM <> SCALAR then
			begin
			  ERROR(461);
			  GATTR.TYPTR := nil
			end
		      else
			{SCALAR}
			if COMPTYPES(LSP^.ELSET,GATTR.TYPTR) then
			  begin
			    if GATTR.KIND = CST then
			      begin
				INCREMENTREGC;
				if (GATTR.CVAL.IVAL < 0)
				  or (GATTR.CVAL.IVAL > BASEMAX) then
				  begin
				    ERROR(352); GATTR.CVAL.IVAL := 0
				  end;
			      end
			    else
			      LOAD(GATTR);
			    if (GATTR.KIND = CST) and (not RANGEPART) then
			      begin (* CONSTANT RANGE *)
				while(LRMIN <= GATTR.CVAL.IVAL) do
				  begin
				    CSTPART := CSTPART or [LRMIN];
				    LRMIN := LRMIN+1
				  end;
				REGC := REGC - 4
			      end
			    else
			      begin (* VARIABLE LIMITS ON RANGE *)
				if not RANGEPART then
				  (* FIRST PART IS CONSTANT *) (* SO NOT IN AC YET *)
				  MACRO3(OP_MOVEI,REGC-1,LRMIN);
				if GATTR.KIND = CST then  (* same for second *)
				  MACRO3(OP_MOVEI,REGC,GATTR.CVAL.IVAL);
				if (GATTR.KIND <> CST) and RUNTMCHECK then
				  begin
				    MACRO3(OP_CAIG,REGC,BASEMAX);
				    MACRO3(OP_CAIGE,REGC,0);
				    SUPPORT(ERRORINASSIGNMENT);
				  end;
				(* HERE IS WHAT IS IN THE AC'S:
				 REGC    - RH LIMIT
				 REGC-1  - LH LIMIT
				 REGC-2  - DOUBLE WORD OF BITS
				 REGC-3         "
				 *)
				MACRO3(OP_SETOB,REGC-3,REGC-2);
				MACRO4(OP_LSHC,REGC-3,REGC-1,0);
				MACRO3(OP_SUBI,REGC,71);
				MACRO3(OP_MOVN,REGC,REGC);
				MACRO3(OP_ADD,REGC-1,REGC);
				MACRO3(OP_MOVN,REGC-1,REGC-1);
				MACRO4(OP_LSHC,REGC-3,REGC-1,0);
				MACRO4(OP_LSHC,REGC-3,REGC,0);
				REGC := REGC -2;
				if VARPART then
				  begin
				    MACRO3(OP_IOR,REGC-3,REGC-1);
				    MACRO3(OP_IOR,REGC-2,REGC);
				    REGC := REGC-2
				  end
				else
				  VARPART := TRUE;
				GATTR.KIND := EXPR; GATTR.REG := REGC
			      end (* VARIABLE LIMITS ON RANGE? *)
			  end {SCALAR?}
		  end;
		LSP^.ELSET := GATTR.TYPTR;
		GATTR.TYPTR :=LSP
	      end
	    else
	      ERROR(360);
      exit if not(SY in [COMMA]);
	INSYMBOL;
	(* RANGEPART IS FLAG: 1ST EXPRESSION IS VARIABLE *)
	RANGEPART := FALSE;
	INCREMENTREGC; INCREMENTREGC;  (* FIRST EXPR *)
	EXPRESSION(FSYS or [COMMA,RBRACK,COLON],ONFIXEDREGC);
    end {LOOP};
      if SY = RBRACK then
	INSYMBOL
      else
	ERROR(155);
      if VARPART then
	begin
	  if CSTPART # [ ] then
	    begin
	      NEW(LVP,PSET);LVP^.PVAL := CSTPART;
	      GATTR.KIND:=CST; GATTR.CVAL.VALP := LVP;
	      MAKECODE(OP_IOR,REGC,GATTR)
	    end
	end
      else
	begin
	  NEWZ(LVP,PSET); LVP^.PVAL := CSTPART;
	  GATTR.CVAL.VALP := LVP
	end
    end {SET OF INTEGER}
  end {non-null SET}
end; %SETCONSTRUCTOR\

	procedure SIMPLEEXPRESSION(FSYS: SETOFSYS);
	  var
	    LATTR: ATTR; LOP: OPERATOR; SIGNED : BOOLEAN;
	    NEWREALCSP: CSP; QSET: BOOLEAN;

	  procedure TERM(FSYS: SETOFSYS);
	    var
	      LATTR: ATTR; LOP: OPERATOR; QSET: BOOLEAN;

	    procedure FACTOR(FSYS: SETOFSYS);
	      var
		LCP: CTP; LVP: CSP;
		LSP: STP;

	      begin %FACTOR\
		if not (SY in FACBEGSYS) then
		  begin
		    ERRANDSKIP(173,FSYS or FACBEGSYS);
		    GATTR.TYPTR := nil
		  end;
		if SY in FACBEGSYS then
		  begin
		    case SY of
		      %ID\                    IDENT:
			begin
			  IDSEARCH([KONST,VARS,FIELD,FUNC],LCP,0);
			  INSYMBOL;
			  if LCP^.KLASS = FUNC then
			    begin
			      CALL(FSYS,LCP);
			      if LCP^.PFDECKIND=DECLARED then
				with LCP^,GATTR do
				  begin
				    TYPTR :=IDTYPE; KIND :=VARBL;
				    PACKFG :=NOTPACK;
				    VRELBYTE := NO;
				    VLEVEL :=1; DPLMT :=2;
				    INDEXR := TOPP; INDBIT :=0;
				    if TYPTR # nil then
				      if TYPTR^.SIZE = 1 then
					LOAD(GATTR)
				  end
			    end
			  else
			    if LCP^.KLASS = KONST then
			      with GATTR, LCP^ do
				begin
				  TYPTR := IDTYPE; KIND := CST;
				  CVAL := VALUES
				end
			    else
			      SELECTOR(FSYS,LCP);
			  if GATTR.TYPTR # nil then
			    %ELIM. SUBR. TYPES TO\
			    with GATTR, TYPTR^ do     %SIMPLIFY LATER TESTS\
			      if FORM = SUBRANGE then
				TYPTR := RANGETYPE
			end {IDENT};
		      %CST\           INTCONST:
			begin
			  with GATTR do
			    begin
			      TYPTR := INTPTR; KIND := CST;
			      CVAL := VAL
			    end;
			  INSYMBOL
			end {INTCONST};
		      REALCONST:
			begin
			  with GATTR do
			    begin
			      TYPTR := REALPTR; KIND := CST;
			      CVAL := VAL
			    end;
			  INSYMBOL
			end {REALCONST};
		      STRINGCONST:
			with GATTR do
			  begin
			    CONSTANT(FSYS,TYPTR,CVAL); KIND := CST
			  end;
		      %(\                       LPARENT:
			begin
			  INSYMBOL;
			  EXPRESSION(FSYS or [RPARENT],ONREGC);
			  if SY = RPARENT
			  then
			    INSYMBOL
			  else
			    ERROR(152)
			end;
		      % NOT \           NOTSY:
			begin
			  INSYMBOL; FACTOR(FSYS);
			  if GATTR.TYPTR = BOOLPTR then
			    begin
			      LOAD(GATTR); MACRO3(OP_ANDCAI,REGC,1)
			    end
			  else
			    begin
			      ERROR(359); GATTR.TYPTR := nil
			    end
			end;
(* 235 - 4-word sets *)
		      %[\                     LBRACK:
			SETCONSTRUCTOR
		      end %CASE\;
		    IFERRSKIP(166,FSYS)
		  end;
		%IF SY IN FACBEGSYS\
	      end %FACTOR\;

	    begin
	      %TERM\
	      QSET := FALSE;
	      FACTOR(FSYS or [MULOP]);
	      while SY = MULOP do
		begin
		  if OP in [RDIV,IDIV,IMOD] then
		    LOAD(GATTR);
		  %BECAUSE OPERANDS ARE NOT
		   ALLOWED TO BE CHOSEN\
		  LATTR := GATTR; LOP := OP;
		  INSYMBOL; FACTOR(FSYS or [MULOP]);
		  if (LATTR.TYPTR # nil) and (GATTR.TYPTR # nil) then
		    case LOP of
		      %*\                     MUL:
			if (LATTR.TYPTR = INTPTR) and (GATTR.TYPTR = INTPTR)
			then
			  SEARCHCODE(OP_IMUL,LATTR)
			else
(* 235 - 4-word sets *)
			  if (LATTR.TYPTR^.FORM in [POWER,QPOWER]) and
				 COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) then
			    begin
			    SEARCHCODE(OP_AND,LATTR);
			    if LATTR.TYPTR^.FORM = QPOWER then
			      QSET := TRUE;
			    end
			  else
			    begin
			      MAKEREAL(LATTR);
			      if (LATTR.TYPTR = REALPTR)
				and (GATTR.TYPTR = REALPTR) then
				SEARCHCODE(OP_FMPR,LATTR)
			      else
				begin
				  ERROR(311); GATTR.TYPTR := nil
				end
			    end;
		      %/\                     RDIV:
			begin
			  MAKEREAL(LATTR);
			  if (LATTR.TYPTR = REALPTR)
			    and (GATTR.TYPTR = REALPTR) then
			    SEARCHCODE(OP_FDVR,LATTR)
			  else
			    begin
			      ERROR(311); GATTR.TYPTR := nil
			    end
			end;
		      %DIV\                   IDIV:
			if (LATTR.TYPTR = INTPTR)
			  and (GATTR.TYPTR = INTPTR) then
			  SEARCHCODE(OP_IDIV,LATTR)
			else
			  begin
			    ERROR(311); GATTR.TYPTR := nil
			  end;
		      %MOD\                   IMOD:
			if (LATTR.TYPTR = INTPTR)
			  and (GATTR.TYPTR = INTPTR) then
			  begin
			    SEARCHCODE(OP_IDIV,LATTR);GATTR.REG := GATTR.REG+1
			  end
			else
			  begin
			    ERROR(311); GATTR.TYPTR := nil
			  end;
		      % AND \         ANDOP:
			if COMPTYPES(LATTR.TYPTR,GATTR.TYPTR)
(* 235 - 4-word sets *)
			  and ((LATTR.TYPTR^.FORM in [POWER,QPOWER])
			       or (GATTR.TYPTR = BOOLPTR)) then
			  begin
			  SEARCHCODE(OP_AND,LATTR);
			  if LATTR.TYPTR^.FORM = QPOWER then
			    QSET := TRUE
			  end
			else
			  begin
			    ERROR(311); GATTR.TYPTR := nil
			  end
		      end %CASE\
		  else
		    GATTR.TYPTR := nil;
		  if not QSET then {QPOWER does not change the AC's}
		    REGC:=GATTR.REG;
		end %WHILE\
	    end %TERM\;

	  begin
	    %SIMPLEEXPRESSION\
	    QSET := FALSE;
	    SIGNED := FALSE;
	    if (SY = ADDOP) and (OP in [PLUS,MINUS]) then
	      begin
		SIGNED := OP = MINUS; INSYMBOL
	      end;
	    TERM(FSYS or [ADDOP]);
	    if SIGNED then
	      with GATTR do
		if TYPTR # nil then
		  if (TYPTR = INTPTR) or (TYPTR = REALPTR) then
		    if KIND = CST then
		      if TYPTR = INTPTR then
			CVAL.IVAL := - CVAL.IVAL
			(* have to put negated value in new place, since old one
			 might be a CONST declaration used elsewhere *)
		      else
			begin
			  NEW(NEWREALCSP);
			  NEWREALCSP^.CCLASS := REEL;
			  NEWREALCSP^.RVAL := -CVAL.VALP^.RVAL;
			  CVAL.VALP := NEWREALCSP
			end
		    else
		      begin {KIND # CST}
			LOAD(GATTR);
			with CODE, INSTRUCTION[CIX] do
			  if INSTR = OP_MOVE then
			    INSTR := OP_MOVN
			  else
			    MACRO3(OP_MOVN,GATTR.REG,GATTR.REG)
		      end
		  else
		    begin
		      ERROR(311); GATTR.TYPTR := nil
		    end;
	    while SY = ADDOP do
	      begin
		if OP = MINUS then
		  LOAD(GATTR);
		%BECAUSE OPD MAY NOT BE CHOSEN\
		LATTR := GATTR; LOP := OP;
		INSYMBOL; TERM(FSYS or [ADDOP]);
		if (LATTR.TYPTR # nil) and (GATTR.TYPTR # nil) then
		  case LOP of
		    %+\             PLUS:
		      if (LATTR.TYPTR = INTPTR) and (GATTR.TYPTR = INTPTR) then
			SEARCHCODE(OP_ADD,LATTR)
			(* ALLOW + AS SET UNION *)
		      else
(* 235 - 4-word sets *)
			if(LATTR.TYPTR^.FORM in [POWER,QPOWER]) and 
				COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) then
			  begin
			  SEARCHCODE(OP_IOR,LATTR);
			  if LATTR.TYPTR^.FORM = QPOWER then
			    QSET := TRUE
			  end
			else
			  begin
			    MAKEREAL(LATTR);
			    if (LATTR.TYPTR = REALPTR) and (GATTR.TYPTR = REALPTR) then
			      SEARCHCODE(OP_FADR,LATTR)
			    else
			      begin
				ERROR(311); GATTR.TYPTR := nil
			      end
			  end;
		    %-\                     MINUS:
		      if (LATTR.TYPTR = INTPTR) and (GATTR.TYPTR = INTPTR) then
			SEARCHCODE(OP_SUB,LATTR)
			(* ALLOW - AS SET DIFFERENCE *)
		      else
(* 435 *)
			if (LATTR.TYPTR^.FORM in [POWER,QPOWER]) and
				 COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) then
			  begin
			  SEARCHCODE(OP_ANDCM,LATTR);
			  if LATTR.TYPTR^.FORM = QPOWER then
			    QSET := TRUE
			  end
			else
			  begin
			    MAKEREAL(LATTR);
			    if (LATTR.TYPTR = REALPTR) and (GATTR.TYPTR = REALPTR) then
			      SEARCHCODE(OP_FSBR,LATTR)
			    else
			      if COMPTYPES(LATTR.TYPTR,GATTR.TYPTR)
(* 435 - 4-word sets *)
				and (LATTR.TYPTR^.FORM in [POWER,QPOWER]) then
				SEARCHCODE(OP_ANDCM,LATTR)
			      else
				begin
				  ERROR(311); GATTR.TYPTR := nil
				end
			  end;
		    % OR \          OROP:
		      if COMPTYPES(LATTR.TYPTR,GATTR.TYPTR)
			and ((GATTR.TYPTR = BOOLPTR)
(* 435 - 4-word sets *)
			     or (LATTR.TYPTR^.FORM in [POWER,QPOWER])) then
			begin
			SEARCHCODE(OP_IOR,LATTR);
			if LATTR.TYPTR^.FORM = QPOWER then
			  QSET := TRUE
			end
		      else
			begin
			  ERROR(311); GATTR.TYPTR := nil
			end
		    end %CASE\
		else
		  GATTR.TYPTR := nil;
		if not QSET then  {QPOWER does not change the AC's}
		  REGC:=GATTR.REG
	      end %WHILE\
	  end %SIMPLEEXPRESSION\;
	begin
	  %EXPRESSION\
	  TESTREGC := REGC+1;
	  SIMPLEEXPRESSION(FSYS or [RELOP]);
	  if SY = RELOP then
	    begin
	      if FVALUE in [ONREGC,ONFIXEDREGC] then
		begin
		  INCREMENTREGC; MACRO3(OP_MOVEI,REGC,1); BOOLREGC := REGC
		end;
	      if GATTR.TYPTR # nil then
		if STRING(GATTR.TYPTR) then
		  (* STRING IS ONLY STRUCTURE ALLOWED *)
		  LOADADDRESS;
	      LREGC1 := REGC;
	      LATTR := GATTR; LOP := OP;
	      if (FVALUE in [ONREGC,ONFIXEDREGC]) and (REGC < BOOLREGC) then
		REGC := BOOLREGC;
	      INSYMBOL; SIMPLEEXPRESSION(FSYS);
	      if GATTR.TYPTR # nil then
		if STRING(GATTR.TYPTR) then
		  (* STRING IS ONLY STRUCTURE ALLOWED *)
		  LOADADDRESS;
	      LREGC2 := REGC;
	      if (LATTR.TYPTR # nil) and (GATTR.TYPTR # nil) then
		begin
		  if LOP = INOP then
(* 435 - 4-word sets *)
		    if GATTR.TYPTR^.FORM in [POWER,QPOWER] then
		      if COMPTYPES(LATTR.TYPTR,GATTR.TYPTR^.ELSET) then
			begin
			  LOAD(LATTR);
			  if (FVALUE in [ONREGC,ONFIXEDREGC]) and (REGC < BOOLREGC) then
			    REGC := BOOLREGC;
			  if GATTR.TYPTR^.FORM = QPOWER then
			    begin
			      LOADADDRESS;
			      MACRO3(OP_MOVE,TAC0,LATTR.REG);
			      MACRO3(OP_LSH,TAC0,-5);
			      MACRO3(OP_ADD,GATTR.INDEXR,TAC0);
			      MACRO4(OP_MOVE,GATTR.INDEXR,GATTR.INDEXR,0);
			      MACRO3(OP_ANDI,LATTR.REG,37B);
			      MACRO4(OP_LSH,GATTR.INDEXR,LATTR.REG,0);
			      REGC := GATTR.INDEXR;  {no-op}
			    end
			  else
			    begin
			      LOAD(GATTR); REGC := GATTR.REG - 1;
			      MACRO4(OP_LSHC,REGC,LATTR.REG,0);
			    end;
			  if FVALUE = TRUEJMP then
			    LINSTR := OP_CAIGE
			  else
			    LINSTR := OP_CAIL;
			  MACRO3(LINSTR,REGC,0);
			end
		      else
			begin
			  ERROR(260); GATTR.TYPTR := nil
			end
		    else
		      begin
			ERROR(213); GATTR.TYPTR := nil
		      end
		  else
		    begin
		      if LATTR.TYPTR # GATTR.TYPTR then
			MAKEREAL(LATTR);
		      if COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) then
			begin
			  LSIZE := LATTR.TYPTR^.SIZE;
			  case LATTR.TYPTR^.FORM of
			    POINTER:
			      if LOP in [LTOP,LEOP,GTOP,GEOP] then
				ERROR (312);
			    POWER:
			      if LOP in [LTOP,GTOP] then
				ERROR(313);
			    ARRAYS:
			      if not STRING(LATTR.TYPTR) then
				(* STRING IS ONLY STRUCT. ALLOWED *)
(* 246 - use the right error message *)
				ERROR(314);
			    RECORDS,
			    FILES:
			      ERROR(314)
			  end;
			  with LATTR.TYPTR^ do
			    begin
			      DEFAULT := TRUE; LOFFSET := 3; SETINCLUSION := FALSE;
			      NEGATE := FALSE;
			      case LOP of
				LTOP:
				  begin
				    LINSTR := OP_CAML; LINSTR1 := OP_CAMLE
				  end;
				LEOP:
(* 235 - 4-word sets *)
				  if FORM in [POWER,QPOWER] then
				    begin
				      SEARCHCODE(OP_ANDCM,LATTR);
				      SETINCLUSION := TRUE
				    end
				  else
				    begin
				      LINSTR := OP_CAMLE; LINSTR1 := OP_CAMLE
				    end;
				GTOP:
				  begin
				    LINSTR := OP_CAMG; LINSTR1 := OP_CAMGE
				  end;
				GEOP:
(* 235 - 4-word sets *)
				  if FORM in [POWER,QPOWER] then
				    begin
				      SEARCHCODE(OP_ANDCA,LATTR);
				      SETINCLUSION := TRUE
				    end
				  else
				    begin
				      LINSTR := OP_CAMGE; LINSTR1 := OP_CAMGE
				    end;
(* 235 - 4 word sets *)
				NEOP:
				 if FORM = QPOWER then
				  begin
				    SEARCHCODE(OP_XOR,LATTR);
				    SETINCLUSION := TRUE;
				    NEGATE := TRUE
				  end
				 else
				  begin
				    LINSTR := OP_CAMN; DEFAULT := FALSE
				  end;
(* 235 - 4 word sets *)
				EQOP:
				 if FORM = QPOWER then
				  begin
				    SEARCHCODE(OP_XOR,LATTR);
				    SETINCLUSION := TRUE;
				  end
				 else
				  begin
				    LINSTR := OP_CAME; DEFAULT := FALSE; LOFFSET := 2
				  end
				end {CASE};
			      if FVALUE = TRUEJMP then
				CHANGEBOOL(LINSTR);
			      (* STRING IS ONLY STRUCTURE *)
			      if FORM#ARRAYS then
				begin
				  if SIZE = 1 then
				    SEARCHCODE(LINSTR,LATTR)
				  else
				    if SETINCLUSION then
(* 235 - 4-word sets *)
				      if FORM = QPOWER then
					begin
					MACRO4(OP_SKIPN,0,BASIS,GATTR.DPLMT);
					MACRO4(OP_SKIPE,0,BASIS,GATTR.DPLMT+1);
					MACRO3R(OP_JRST,0,IC+3);
					MACRO4(OP_SKIPN,0,BASIS,GATTR.DPLMT+2);
					MACRO4(OP_SKIPE,0,BASIS,GATTR.DPLMT+3);
					if (FVALUE = TRUEJMP) or
					  NEGATE and (FVALUE <> TRUEJMP) then
					  MACRO3R(OP_JRST,0,IC+2);
					end
				      else begin
					MACRO3(OP_SKIPN,0,GATTR.REG);
					MACRO3(OP_SKIPE,0,GATTR.REG-1);
					if FVALUE = TRUEJMP then
					  MACRO3R(OP_JRST,0,IC+2)
				      end
				    else
				      begin
					LOAD(LATTR);
					if (FVALUE in [ONREGC,ONFIXEDREGC]) and (REGC<BOOLREGC) then
					  REGC := BOOLREGC;
					LOAD(GATTR);
					if DEFAULT then
					  begin
					    MACRO3(LINSTR1,LATTR.REG-1,GATTR.REG-1);
					    MACRO3R(OP_JRST,0,IC+4)   %FALSE\
					  end;
					MACRO3(OP_CAME,LATTR.REG-1,GATTR.REG-1);
					MACRO3R(OP_JRST,0,IC+LOFFSET);
					MACRO3(LINSTR,LATTR.REG,GATTR.REG)
				      end
				end
			      else
				begin (*STRING*)
				  (* THIS CODE IS ONLY FOR STRINGS *)
				  CONF := FALSE;
				  if (LATTR.TYPTR <> NIL) and
				     (GATTR.TYPTR <> NIL) then
				    if (LATTR.TYPTR^.INXTYPE <> NIL) and
				       (GATTR.TYPTR^.INXTYPE <> NIL) then
				      begin
				      if (LATTR.TYPTR^.ARRAYCONF or
					 GATTR.TYPTR^.ARRAYCONF) and
					 RUNTMCHECK then
					begin
					INCREMENTREGC; INCREMENTREGC;
					LOADSIZE(REGC-1,LATTR.TYPTR);
					LOADSIZE(REGC,GATTR.TYPTR);
				        MACRO3(OP_CAME,REGC,REGC-1);
					SUPPORT(INDEXERROR);
					REGC := REGC-2;
				        end;
				      if not LATTR.TYPTR^.ARRAYCONF then
					GETBOUNDS(LATTR.TYPTR^.INXTYPE,
						  LOFFSET,LSIZE)
				      else if not GATTR.TYPTR^.ARRAYCONF then
					GETBOUNDS(GATTR.TYPTR^.INXTYPE,
						  LOFFSET,LSIZE)
				      else CONF := TRUE
				      end;
				  LSIZE:=LSIZE-LOFFSET+1;
				  if ARRAYPF and not CONF then
				    begin
				      LOFFSET:=(LSIZE mod 5)*700B;
				      LSIZE:=LSIZE div 5
				    end
				  else
				    LOFFSET:=0;
				  if CONF then
				      begin
				{Size is already here:}
					INCREMENTREGC; LREGC5 := REGC;
					INCREMENTREGC; LREGC3 := REGC;
					INCREMENTREGC; LREGC4 := REGC;
					INCREMENTREGC;
					if ARRAYPF then
					  begin
					    MACRO3(OP_HRLZI,LREGC3,440700B+
						   LREGC1);
					    MACRO3(OP_HRLZI,LREGC4,440700B+
						   LREGC2);
					  end
					else
					  begin
					    MACRO3(OP_HRLZI,LREGC3,444400B+
						   LREGC1);
					    MACRO3(OP_HRLZI,LREGC4,444400B+
						   LREGC2);
					  end;
					MACRO3(OP_SETZB,TAC1,TAC0);
					MACRO3R(OP_JUMPE,LREGC5,IC+6);
					MACRO3(OP_ILDB,TAC1,LREGC3);
					MACRO3(OP_ILDB,TAC0,LREGC4);
					MACRO3R(OP_SOJLE,LREGC5,IC+3);
					MACRO3(OP_CAMN,TAC1,TAC0);
					MACRO3R(OP_JRST,0,IC-4);
					REGC := REGC - 3;
				      end
				  else if (LSIZE = 0) and (LOFFSET=0) then
				    MACRO3(OP_SETZB,TAC1,TAC0)
				  else
				    begin {SIZE > 0}
				      INCREMENTREGC;  LREGC3 := REGC;
				      INCREMENTREGC;  LREGC4 := REGC;
				      if LSIZE = 0 then
					begin
					  MACRO3(OP_HRLZI,LREGC3,LOFFSET+440000B+LREGC1);
					  MACRO3(OP_HRLZI,LREGC4,LOFFSET+440000B+LREGC2);
					  MACRO3(OP_ILDB,TAC1,LREGC3);
					  MACRO3(OP_ILDB,TAC0,LREGC4)
					end
				      else
					begin {LSIZE > 0}
					  if ARRAYPF then
					    begin
					      MACRO3(OP_HRLZI,LREGC3,444300B+LREGC1);
					      MACRO3(OP_HRLZI,LREGC4,444300B+LREGC2)
					    end
					  else
					    begin
					      MACRO3(OP_HRLZI,LREGC3,444400B+LREGC1);
					      MACRO3(OP_HRLZI,LREGC4,444400B+LREGC2)
					    end;
					  INCREMENTREGC;
					  if LSIZE > 1 then
					    MACRO3(OP_MOVEI,REGC,LSIZE);
					  MACRO3(OP_ILDB,TAC1,LREGC3);
					  MACRO3(OP_ILDB,TAC0,LREGC4);
					  if (LOFFSET=0) then
					    begin
					      if LSIZE>1 then
						begin
						  MACRO3(OP_CAMN,TAC1,TAC0);
						  MACRO3R(OP_SOJG,REGC,IC-3)
						end
					    end
					  else
					    begin {OFFSET # 0}
					      MACRO3(OP_CAME,TAC1,TAC0);
					      if LSIZE>1 then
						begin
						  MACRO3R(OP_JRST,0,IC+6);
						  MACRO3R(OP_SOJG,REGC,IC-4)
						end
					      else
						MACRO3R(OP_JRST,0,IC+5);
					      MACRO3(OP_HRLI,LREGC3,LOFFSET+LREGC1);
					      MACRO3(OP_HRLI,LREGC4,LOFFSET+LREGC2);
					      MACRO3(OP_ILDB,TAC1,LREGC3);
					      MACRO3(OP_ILDB,TAC0,LREGC4)
					    end;
					  {OFFSET # 0}
					  REGC:=REGC-1
					end;
				      {LSIZE > 0}
				      REGC := REGC - 2;
				    end;
				  {(LSIZE > 0) OR (LOFFSET > 0)}
				  MACRO3(LINSTR,TAC1,TAC0);
				  REGC := REGC - 2
				end {STRING}
			    end
			end
		      else
			ERROR(260)
		    end;
		  if FVALUE in [ONREGC,ONFIXEDREGC] then
		    begin
		      MACRO3(OP_SETZ,BOOLREGC,0); REGC := BOOLREGC
		    end
		  else
		    MACRO3(OP_JRST,0,FIX_UP);
		end;
	      %(IF LATTR.TYPTR#NIL) AND (GATTR.TYPTR#NIL) THEN \
	      GATTR.TYPTR := BOOLPTR; GATTR.KIND := EXPR; GATTR.REG := REGC
	    end %SY = RELOP\
	  else
	    if FVALUE in [TRUEJMP,FALSEJMP] then
	      begin
		LOAD(GATTR);
		if GATTR.TYPTR#BOOLPTR then
		  ERROR (359);
		if FVALUE = TRUEJMP then
		  LINSTR := OP_JUMPN
		else
		  LINSTR := OP_JUMPE;
		MACRO3(LINSTR,GATTR.REG,0)
	      end
	    else
	      if GATTR.KIND=EXPR then
		REGC := GATTR.REG;
	  if GATTR.TYPTR # nil then
	    with GATTR,TYPTR^ do
	      {Warning to modifiers:  the following code depends upon the register
	      allocation in MAKECODE for the case where opcode=MOVE, and in
	      LOADADDRESS.  Please be sure to keep them consistent!}
	      {Onfixedregc means we are in a context where the result has to go in
	      a particular AC.  So if we had a complex calculation that ended up
	      with it in a higher AC, we have to move it down.  That is for
	      KIND=EXPR.  For KIND=CST or VARBL (the only other cases), we have
	      to make sure REGC was not changed, as the caller will expect that.
	      It could be changed by an array with a complex subscript calculation.
	      Note that we in the case KIND=VARBL we may leave AC's set up with
	      info needed to access arrays (in the fieldS INDEXR and/or BPADDR).
	      So in that case this amounts to second-guessing LOAD and MAKECODE
	      to make sure that whichever place the result will be loaded
	      (usually INDEXR or BPADDR) is pointing to the fixed AC.}

	      if FVALUE = ONFIXEDREGC then
		begin
		  if KIND=EXPR then
		    begin
		      if SIZE = 2 then
			TESTREGC := TESTREGC + 1;
		      if TESTREGC # REGC then
			begin
			  if SIZE = 2 then
			    MACRO3(OP_MOVE,TESTREGC-1,REGC-1);
			  MACRO3(OP_MOVE,TESTREGC,REGC);
			  REG := TESTREGC; REGC := TESTREGC;
			end
		    end
		  else
		    if KIND=VARBL then
		      begin
			if (PACKFG = PACKK) and (BPADDR>REGIN) and (BPADDR<=REGCMAX) then
			  if (INDEXR <= REGIN) or (BPADDR<INDEXR) then
			    begin
			      if BPADDR<> TESTREGC then
				begin
				  MACRO3(OP_MOVE,TESTREGC,BPADDR);
				  BPADDR := TESTREGC
				end
			    end
			  else
			    begin
			      if INDEXR<>TESTREGC then
				begin
				  MACRO3(OP_MOVE,TESTREGC,INDEXR);
				  INDEXR := TESTREGC
				end
			    end
			else
			  if (INDEXR>REGIN) and (INDEXR<=REGCMAX) and (INDEXR<>TESTREGC) then
			    begin
			      MACRO3(OP_MOVE,TESTREGC,INDEXR);
			      INDEXR := TESTREGC
			    end;
			REGC := TESTREGC - 1;
		      end
		    else
		      REGC := TESTREGC-1
		end
	end %EXPRESSION\;
      procedure ASSIGNMENT(FCP: CTP);
	var
	  LATTR,SLATTR: ATTR;
	  SRMIN,SRMAX: INTEGER;
	  SIZEREG,REGSAVE: ACRANGE;

	procedure STOREGLOBALS;
	  type
	    WANDELFORM = (PTRW,INTW,REELW,PSETW,CSETW,STRGW,INSTW,CARRW);
	  var
	    WANDEL : record
		       case KW : WANDELFORM of
			    PTRW: (WPTR :GTP %TO ALLOW NIL\);
			    INTW: (WINT : INTEGER;
				   WINT1 : INTEGER; %TO PICK UP SECOND WORD OF SET\
				   WINT2 : INTEGER;
				   WINT3 : INTEGER);
				
			    REELW: (WREEL: REAL);
			    PSETW: (WSET : set of 0..71);
			    CSETW: (WCSET: set of CHAR);
{This is needed only because of bootstrapping.  WCSET won't work as long as
 we are using the old compiler}
		            CARRW:  (WCARVAL: array[0..3] of
				    packed array [0..35] of BOOLEAN);
			    STRGW: (WSTRG: CHARWORD);
			    INSTW: (WINST: PDP10INSTR)
		     end;
	    I,J : INTEGER;
	  procedure STOREWORD;
	    begin
	      CIX := CIX + 1;
	      if CIX > CIXMAX then
		begin
		  CIX := 0; ERRORWITHTEXT(356,'INITPROCD.')
		end;
	      with CGLOBPTR^ do
		begin
		  CODE.INSTRUCTION[CIX] := WANDEL.WINST;
		  LASTGLOB := LASTGLOB + 1
		end
	    end;

	  procedure GETNEWGLOBPTR;
	    var
	      LGLOBPTR : GTP;
	    begin
	      NEWZ(LGLOBPTR);
	      with LGLOBPTR^ do
		begin
		  NEXTGLOBPTR := nil;
		  FIRSTGLOB       := 0;
		end;
	      if CGLOBPTR # nil then
		CGLOBPTR^.NEXTGLOBPTR := LGLOBPTR;
	      CGLOBPTR := LGLOBPTR;
	    end;
	  begin
	    %STOREGLOBALS\
	    if FGLOBPTR = nil then
	      begin
		GETNEWGLOBPTR;
		FGLOBPTR := CGLOBPTR;
	      end
	    else
	      if LATTR.DPLMT # CGLOBPTR^.LASTGLOB + 1 then
		GETNEWGLOBPTR;
	    if LATTR.DPLMT > MAXADDR then
	      begin
	        ERROR(467);
	        LATTR.DPLMT := 0
	      end;
	    with WANDEL,CGLOBPTR^,GATTR,CVAL do
	      begin
		if FIRSTGLOB = 0 then
		  begin
		    FIRSTGLOB := LATTR.DPLMT;
		    LASTGLOB := FIRSTGLOB - 1;
		    FCIX := CIX + 1;
		  end;
		case TYPTR^.FORM of
		  SCALAR,
		  SUBRANGE:
		    begin
		      (* 30-Sep-80 Andy Hisgen, CMU,  Problems with xreal:=xinteger,
		       *                                   and with subranges.
		       * The lines below used to read --
		       *               IF TYPTR = REALPTR
		       *               THEN
		       *                 IF LATTR.TYPTR=INTPTR
		       *                 THEN WREEL := IVAL
		       *                 ELSE WREEL := VALP^.RVAL
		       *               ELSE WINT  := IVAL;
		       * Unfortunately, that was testing to see if the RightHandSide (GATTR) was
		       * a real, and if so doing weird things.  For example, that let the
		       * assignment "x:=2", where x is a real, go thru, but without doing
		       * any conversion, thus x contained the bit pattern for the integer 2.
		       * The problem here seems to have been that the roles of LATTR and
		       * GATTR got reversed in the coder's mind.  Below, we have reversed
		       * them back.
		       *    A second unrelated problem was that subrange checking was not
		       * being done.  In the code below, we now handle this.
		       *)
		      if LATTR.TYPTR = REALPTR then
			if GATTR.TYPTR = INTPTR then
			  WREEL := IVAL
			else
			  WREEL := VALP^.RVAL
		      else
			begin (*left isn't real*)
			  if LATTR.TYPTR^.FORM = SUBRANGE then
			    begin (*left is subrange*)
			      GETBOUNDS(LATTR.TYPTR,SRMIN,SRMAX);
			      if not((SRMIN <= IVAL) and (IVAL <= SRMAX)) then
				ERROR(367);
			    end; (*left is subrange*)
			  WINT := IVAL;
			end; (*left isn't real*)
		      (*30-Sep-80 end of changes for xreal:=integer and for subranges*)
		      STOREWORD
		    end;
		  POINTER:
		    begin
		      WPTR := nil; STOREWORD
		    end;
		  POWER:
		    begin
		      WSET := VALP^.PVAL; STOREWORD;
		      WINT := WINT1 %GET SECOND WORD OF SET\;
		      STOREWORD;
		    end;
(* 235 - 4-word sets *)
		  QPOWER:
		    begin
		      WCARVAL := VALP^.CARVAL; STOREWORD;
		      WINT := WINT1; STOREWORD;
		      WINT := WINT2; STOREWORD;
		      WINT := WINT3; STOREWORD;
		   end;
		  ARRAYS   : with VALP^,WANDEL do begin
		    J := 0; WINT := 0;
		    for I := 1 to SLGTH do
		      begin
			J := J + 1;  WSTRG[J] := SVAL[I];
			if J=5 then
			  begin
			    J := 0; STOREWORD; WINT := 0
			  end
		      end;
		    if J#0 then
		      STOREWORD
		  end;

		  RECORDS,
		  FILES    :      ERROR(411)
		  end %CASE\;
	      end % WITH \;
	  end % STOREGLOBALS \;

	begin %ASSIGNMENT\
	  SELECTOR(FSYS or [BECOMES],FCP);
	  if SY = BECOMES then
	    begin
	      CHECKASSIGN(GATTR);
	      LATTR := GATTR;
	      INSYMBOL;
	      EXPRESSION(FSYS,ONREGC);
	      if (LATTR.TYPTR # nil) and (GATTR.TYPTR # nil) then
		if COMPTYPES(LATTR.TYPTR,GATTR.TYPTR)
		  or (REALPTR=LATTR.TYPTR) and (GATTR.TYPTR=INTPTR) then
		  if INITGLOBALS then
		    if GATTR.KIND = CST then
		      STOREGLOBALS
		    else
		      ERROR(504)
		  else
		    if (GATTR.KIND=CST) and (GATTR.CVAL.IVAL=0)
		      and (LATTR.PACKFG=NOTPACK) then
		      begin
			FETCHBASIS(LATTR);
			with LATTR do
			  begin
			    (* check subranges *)
			    if LATTR.TYPTR^.FORM = SUBRANGE then
			      begin
				GETBOUNDS(LATTR.TYPTR,SRMIN,SRMAX);
				if (0 < SRMIN) or (0 > SRMAX) then
				  ERROR(367)
			      end;
			    MACRO(VRELBYTE,OP_SETZM,0,INDBIT,INDEXR,DPLMT)
			  end
		      end
		    else
		      case LATTR.TYPTR^.FORM of
			SCALAR,
			POINTER,
			POWER:
			  begin
			    LOAD(GATTR);
			    if COMPTYPES(REALPTR,LATTR.TYPTR) and (GATTR.TYPTR=INTPTR) then
			      MAKEREAL(GATTR);
			    STORE(GATTR.REG,LATTR)
			  end;
			SUBRANGE:
			  begin
			    LOADSUBRANGE(GATTR,LATTR.TYPTR);
			    STORE(GATTR.REG,LATTR)
			  end;

			ARRAYS:
			  if GATTR.TYPTR^.ARRAYCONF or
			     LATTR.TYPTR^.ARRAYCONF then
			      begin
				REGSAVE := REGC;
{Leave room for the BLT or XBLT args}
				if (GATTR.INDEXR > 0) and
				   (GATTR.INDEXR <= REGCMAX) then
{If GATTR is in AC, the LOADADDRESS will not INCREMENTREGC}
				else INCREMENTREGC;  {LOADADDRESS}
				if EXTENDED_ADDRESSING then
				  INCREMENTREGC;  {XBLT code does an INCR}
				INCREMENTREGC;  {to get us a free AC}
				SIZEREG := REGC;
				PHYSSIZE(REGC,GATTR.TYPTR);
				if RUNTMCHECK then
				  begin
				    INCREMENTREGC;
				    PHYSSIZE(REGC,LATTR.TYPTR);
				    MACRO3(OP_CAME,REGC,REGC-1);
				    SUPPORT(INDEXERROR);
				  end;
				REGC := REGSAVE;
				if not GATTR.TYPTR^.ARRAYCONF
				  then 
				    begin
				      LATTR.TYPTR := GATTR.TYPTR;
				      goto 99;
				    end;
				if not LATTR.TYPTR^.ARRAYCONF
				  then
				    begin
				      GATTR.TYPTR := LATTR.TYPTR;
				      goto 99
				    end;
			{Here for conformant arrays of unknown size}
				if EXTENDED_ADDRESSING then
				  begin
				    LOADADDRESS; INCREMENTREGC;
				    FETCHBASIS(LATTR);
				    WITH LATTR DO
				      MACRO(VRELBYTE,OP_XMOVEI,REGC,
							INDBIT,INDEXR,DPLMT);
				    MACRO3(OP_MOVE,REGC-2,SIZEREG);
				    MACRO3R(OP_XTND,REGC-2,XBLT_LOC)
				  end
				else
				  begin {NOT EXTENDED ADDRESSING}
				    LOADADDRESS;
				    CODE.INSTRUCTION[CIX].INSTR := OP_HRLI;
				    FETCHBASIS(LATTR);
				    WITH LATTR DO
				      MACRO(VRELBYTE,OP_HRRI,REGC,INDBIT,
					    INDEXR,DPLMT);
				    MACRO4(OP_ADDI,SIZEREG,REGC,-1);
				    MACRO4(OP_BLT,REGC,SIZEREG,0);
				  end
			      end {array}
			  else goto 99;
(* 235 - 4-word sets *)
			QPOWER,
			RECORDS:
99:			  if GATTR.TYPTR^.SIZE > 0 then
			    if (GATTR.TYPTR^.SIZE <= 2) then
			      begin
				LOAD(GATTR); STORE(GATTR.REG,LATTR)
			      end
			    else
			      {size > 2} with LATTR do
				if EXTENDED_ADDRESSING then
				  begin
				    LOADADDRESS; INCREMENTREGC;
				    FETCHBASIS(LATTR);
				    MACRO(VRELBYTE,OP_XMOVEI,REGC,INDBIT,INDEXR,DPLMT);
				    MACRO3C(OP_MOVE,REGC-2,TYPTR^.SIZE);
				    MACRO3R(OP_XTND,REGC-2,XBLT_LOC)
				  end
				else
				  begin {NOT EXTENDED ADDRESSING}
				    LOADADDRESS;
				    CODE.INSTRUCTION[CIX].INSTR := OP_HRLI;
				    FETCHBASIS(LATTR);
				    MACRO(VRELBYTE,OP_HRRI,REGC,INDBIT,INDEXR,DPLMT);
				    if INDBIT=0 then
				      MACRO5(VRELBYTE,OP_BLT,REGC,INDEXR,DPLMT+TYPTR^.SIZE-1)
				    else
				      begin
					INCREMENTREGC;
					MACRO3(OP_MOVE,REGC,REGC-1);
					MACRO4(OP_BLT,REGC,REGC-1,TYPTR^.SIZE-1)
				      end
				  end;
			FILES: ERROR(361)
			end
		else
		  ERROR(260)
	    end %SY = BECOMES\
	  else
	    ERROR(159);
	end %ASSIGNMENT\;
      procedure GOTOSTATEMENT;
	var
	  I,J,JJ:INTEGER; LCP:CTP;
	begin
	  if SY = INTCONST then
	    begin
	      PRTERR := FALSE;
	      IDSEARCH([LABELT],LCP,0);
	      PRTERR := TRUE;
	      if LCP # nil then
		with LCP^ do
		  (* See if the goto is out of the current block.  If so, handle
		   specially, since we have to restore the basis and topp.  Except
		   for the global level, we recover the basis by tracing the static
		   links.  Then we arranged for topp's RH to be stored in the LH
		   of word 0 of the display.  Global labels are odd because the
		   static link will be 0.  So the global topp and basis are stored
		   in special variables. *)
		  (* As of this edit, we have to call GOTOC. in order to
		   close files in the blocks exited.  In order to prevent problems
		   if we are interrupted while this is happening, we can't really
		   change BASIS or TOPP until after the files are closed, else we
		   might be trying to close a file whose control block is above TOPP.
		   So we REGC is the new BASIS and REGC+1 is the new TOPP *)
		  if SCOPE # LEVEL then
		    begin
		      INCREMENTREGC;
		      if SCOPE = 1 then
			begin
			  MACRO3R(OP_MOVE,REGC,GLOBBASIS);
			  MACRO3R(OP_MOVE,REGC+1,GLOBTOPP)
			end
		      else
			begin
			  MACRO4(OP_MOVE,REGC,BASIS,PREV_BASIS);
			  for I := SCOPE to LEVEL - 2 do
			    MACRO4(OP_MOVE,REGC,REGC,PREV_BASIS);
			  if EXTENDED_ADDRESSING then
			    MACRO4(OP_MOVE,REGC+1,REGC,CURR_TOPP)
			  else
			    begin
			      MACRO3(OP_HRL,REGC,REGC);
			      MACRO4(OP_HLR,REGC+1,REGC,0);
			      MACRO3(OP_HRL,REGC+1,REGC+1)
			    end
			end;
		      MACRO3R(OP_XMOVEI,REGC+2,GOTOCHAIN);
		      GOTOCHAIN := IC-1;
		      CODE.INFORMATION[CIX] := 'F';
		      NONLOCGOTO := TRUE;
		      SUPPORT(EXITGOTO);
		      goto 2
		    end;
	      for I := 1 to LIX do
		begin
		  with LABELS[I] do
		    if LABSVAL = VAL.IVAL then
		      begin
			MACRO3R(OP_JRST,0,LABSADDR);
			goto 2
		      end
		end;
	      MACRO3(OP_JRST,0,FIX_UP);
	      for I:=1 to JIX do
		begin
		  with GOTOS[I] do
		    if GOTOVAL = VAL.IVAL then
		      begin
			J:= CODE.INSTRUCTION[GOTOADDR].ADDRESS;
			JJ:= GOTOADDR;
			while J#0 do
			  begin
			    JJ:=J; J:= CODE.INSTRUCTION[J].ADDRESS
			  end;
			INSERTADDR(NO,JJ,CIX);
			goto 2
		      end
		end;
	      for I:=1 to JIX do
		with GOTOS[I] do
		  if GOTOVAL = -1 then
		    begin
		      GOTOVAL:=VAL.IVAL;
		      GOTOADDR:=CIX;
		      goto 2
		    end;
	      JIX :=JIX+1;
	      if JIX > LABMAX then
		begin
		  ERROR(362);   JIX := LABMAX
		end;
	      with GOTOS[JIX] do
		begin
		  GOTOVAL := VAL.IVAL;   GOTOADDR:=CIX
		end;
	2:
	      INSYMBOL
	    end
	  else
	    ERROR(255)
	end %GOTOSTATEMENT\;

      procedure COMPOUNDSTATEMENT;
	begin
	  loop
	    repeat
	      STATEMENT(FSYS,STATENDS)
	    until   not (SY in STATBEGSYS);
	  exit if SY # SEMICOLON;
	    INSYMBOL
	end;
	  if SY = ENDSY then
	    INSYMBOL
	  else
	    ERROR(163)
	end %COMPOUNDSTATEMENET\;

      procedure IFSTATEMENT;
	var
	  LCIX1,LCIX2: CODERANGE;
	begin
	  EXPRESSION(FSYS or [THENSY],FALSEJMP);
	  LCIX1 := CIX;
	  if SY = THENSY then
	    INSYMBOL
	  else
	    ERROR(164);
	  STATEMENT(FSYS or [ELSESY],STATENDS or [ELSESY]);
	  if SY = ELSESY then
	    begin
	      MACRO3(OP_JRST,0,FIX_UP);  LCIX2 := CIX;
	      INSERTADDR(RIGHT,LCIX1,IC);
	      INSYMBOL; STATEMENT(FSYS,STATENDS);
	      INSERTADDR(RIGHT,LCIX2,IC)
	    end
	  else
	    INSERTADDR(RIGHT,LCIX1,IC)
	end %IFSTATEMENT\;
      procedure CASESTATEMENT;
	type
	  CIP = ^CASEINFO;
	  CASEINFO = packed record
			      NEXT: CIP;
			      CSSTART: ADDRRANGE;
			      CSEND: CODERANGE;
			      CSLAB: INTEGER
			    end;
	var
	  LSP,LSP1: STP;
	  FSTPTR,LPT1,LPT2,LPT3,OTHERSPTR: CIP;
	  LVAL: VALU;
	  LIC,LADDR,JUMPADDR: ADDRRANGE;
	  LCIX: CODERANGE;
	  LMIN,LMAX: INTEGER;

	procedure INSERTBOUND(FCIX:CODERANGE;FIC: ADDRRANGE;BOUND:INTEGER);
	  var
	    LCIX1:CODERANGE;
	    LIC1: ADDRRANGE;
	    LATTR:ATTR;
	  begin
	    if (BOUND>=0) and (BOUND <= MAXADDR) then
	      INSERTADDR(NO,FCIX,BOUND)
	    else
	      begin
		LCIX1:=CIX; LIC1 := IC;
		CIX:=FCIX; IC := FIC;
		with LATTR do
		  begin
		    KIND:=CST; CVAL.IVAL:=BOUND; TYPTR:=nil
		  end;
		DEPCST(INT,LATTR);
		CIX:=LCIX1; IC:= LIC1;
		with CODE.INSTRUCTION[FCIX] do
		  INSTR:=INSTR+10B  %CAILE-->CAMLE, CAIL-->CAML\
	      end
	  end;

	begin {CASESTATEMENT}
	  OTHERSPTR:=nil;
	  EXPRESSION(FSYS or [OFSY,COMMA,COLON],ONREGC);
	  LOAD(GATTR);
	  MACRO3(OP_CAIL,REGC,FIX_UP);%<<<---------- LMIN IS INSERTED HERE\
	  MACRO3(OP_CAILE,REGC,FIX_UP);%<<<--------- LMAX IS INSERTED HERE\
	  MACRO3(OP_JRST,0,FIX_UP);%<<<------------- START OF "OTHERS" IS INSERTED HERE\
	  MACRO(NO,OP_JRST,0,1,REGC,FIX_UP);%<<<---- START OF JUMP TABLE IS INSERTED HERE\
	  LCIX := CIX; LIC := IC;
	  LSP := GATTR.TYPTR;
	  if LSP # nil then
	    if (LSP^.FORM # SCALAR) or (LSP = REALPTR) then
	      begin
		ERROR(315); LSP := nil
	      end;
	  if SY = OFSY then
	    INSYMBOL
	  else
	    ERROR(160);
	  (* allow extra semicolon *)
	  while SY = SEMICOLON do INSYMBOL;
	  FSTPTR := nil; LPT3 := nil;
	  loop
	    loop
	      CONSTANT(FSYS or [COMMA,COLON],LSP1,LVAL);
	      if LSP # nil then
		if COMPTYPES(LSP,LSP1) then
		  begin
		    LPT1 := FSTPTR; LPT2 := nil;
		    if ABS(LVAL.IVAL) > HWCSTMAX then
		      ERROR(316);
		    while LPT1 # nil do
		      with LPT1^ do
			begin
			  if CSLAB <= LVAL.IVAL then
			    begin
			      if CSLAB = LVAL.IVAL then
				ERROR(261);
			      goto 1
			    end;
			  LPT2 := LPT1; LPT1 := NEXT
			end;
	1:
		    NEWZ(LPT3);
		    with LPT3^ do
		      begin
			NEXT := LPT1; CSLAB := LVAL.IVAL;
			CSSTART := IC; CSEND := 0
		      end;
		    if LPT2 = nil then
		      FSTPTR := LPT3
		    else
		      LPT2^.NEXT := LPT3
		  end
		else
		  ERROR(505);
	    exit if SY # COMMA;
	      INSYMBOL
	  end;
	    if SY = COLON then
	      INSYMBOL
	    else
	      ERROR(151);
	    repeat
	      STATEMENT(FSYS,STATENDS)
	    until   not (SY in STATBEGSYS);
	    if LPT3 # nil then
	      begin
		MACRO3(OP_JRST,0,FIX_UP);  LPT3^.CSEND := CIX
	      end;
	    (* allow extra semicolons *)
	    while SY = SEMICOLON do INSYMBOL;
	  exit if SY in (FSYS or STATENDS);
	    if SY=OTHERSSY then
	      begin
		INSYMBOL;
		if SY=COLON then
		  INSYMBOL
		else
		  ERROR(151);
		NEWZ(OTHERSPTR);
		with OTHERSPTR^ do
		  begin
		    CSSTART:=IC;
		    repeat
		      STATEMENT(FSYS,STATENDS)
		    until not(SY in STATBEGSYS);
		    MACRO3(OP_JRST,0,FIX_UP);  CSEND := CIX;
		    (* allow extra semicolons *)
		    while SY=SEMICOLON do INSYMBOL;
		    goto 2
		  end
	      end
	end;
	2:
	  if FSTPTR # nil then
	    begin
	      LMAX := FSTPTR^.CSLAB;
	      %REVERSE POINTERS\
	      LPT1 := FSTPTR; FSTPTR := nil;
	      repeat
		LPT2 := LPT1^.NEXT; LPT1^.NEXT := FSTPTR;
		FSTPTR := LPT1; LPT1 := LPT2
	      until LPT1 = nil;
	      LMIN := FSTPTR^.CSLAB;
	      INSERTBOUND(LCIX-2,LIC-2,LMAX);
	      INSERTBOUND(LCIX-3,LIC-3,LMIN);
	      (* Polish fixups to avoid problem with LOADER *)
	      INSERTPOLISH(LIC-1,IC,-LMIN);  {put IC-LMIN at LIC-1}
	      if LMAX - LMIN < CIXMAX-CIX then
		begin
		  LADDR := IC + LMAX - LMIN + 1;
		  if OTHERSPTR=nil then
		    JUMPADDR:=LADDR
		  else
		    begin
		      INSERTADDR(RIGHT,OTHERSPTR^.CSEND,LADDR);
		      JUMPADDR:=OTHERSPTR^.CSSTART
		    end;
		  INSERTADDR(RIGHT,LCIX-1,JUMPADDR);
		  repeat
		    with FSTPTR^ do
		      begin
			while CSLAB > LMIN do
			  begin
			    FULLWORD(RIGHT,IFIW,JUMPADDR); LMIN := LMIN + 1
			  end;
			FULLWORD(RIGHT,IFIW,CSSTART);
			if CSEND # 0 then
			  INSERTADDR(RIGHT,CSEND,LADDR);
			FSTPTR := NEXT; LMIN := LMIN + 1
		      end
		  until FSTPTR = nil
		end
	      else
		ERROR(363)
	    end;
	  if SY = ENDSY then
	    INSYMBOL
	  else
	    ERROR(163)
	end %CASESTATEMENT\;
      procedure REPEATSTATEMENT;
	var
	  LADDR: ADDRRANGE;
	begin
	  LADDR := IC;
	  loop
	    repeat
	      STATEMENT(FSYS or [UNTILSY],STATENDS or [UNTILSY])
	    until   not (SY in STATBEGSYS);
	  exit if SY # SEMICOLON;
	    INSYMBOL
	end;
	  if SY = UNTILSY then
	    begin
	      INSYMBOL; EXPRESSION(FSYS,FALSEJMP);
	      INSERTADDR(RIGHT,CIX,LADDR);
	    end
	  else
	    ERROR(202)
	end %REPEATSTATEMENT\;

      procedure WHILESTATEMENT;
	var
	  LADDR: ADDRRANGE; LCIX: CODERANGE;
	begin
	  LADDR := IC;
	  EXPRESSION(FSYS or [DOSY],FALSEJMP);
	  LCIX := CIX;
	  if SY = DOSY then
	    INSYMBOL
	  else
	    ERROR(161);
	  STATEMENT(FSYS,STATENDS);
	  MACRO3R(OP_JRST,0,LADDR);
	  INSERTADDR(RIGHT,LCIX,IC)
	end %WHILESTATEMENT\;

      procedure FORSTATEMENT;
	var
	  LATTR,SATTR: ATTR; LSP: STP; LSY: SYMBOL;
	  LCIX: CODERANGE; LADDR,LDPLMT: XADDRRANGE; LINSTR: INSTRANGE;
	  LREGC,LINDREG: ACRANGE; LINDBIT: IBRANGE; LRELBYTE: RELBYTE;
(* 233 - better temp allocation *)
	  SAVEINDEXR,UBTEMP:XADDRRANGE; OLDVNOASSIGN: BOOLEAN;
	begin
          UBTEMP := 0; SAVEINDEXR := 0;
	  if SY = IDENT then
	    begin
	      IDSEARCH([VARS],LCP,0);
	      with LCP^, LATTR do
		begin
		  TYPTR := IDTYPE; KIND := VARBL; 
		  NOASSIGN := CONFARG or VNOASSIGN;
		  if VKIND = ACTUAL then
		    begin
		      VLEVEL := VLEV;
		      if VLEV > 1 then
			VRELBYTE := NO

(* 260 - find a few more odd cases *)
{If VADDR < PROGRST, we really have an absolute reference.  The
 most common case is 0, at the end of a fixup chain.  One would
 at first think that VRELBYTE := NO should be here.  However that
 doesn't work.  Apparently other places in the code assume that
 all lowseg code is marked RIGHT.  The routine that actually
 outputs code turns it into NO at the very last moment.  So we
 use RIGHT here.  We don't want to use LOWSEG, because that would
 really cause lowseg relocation to be forced, using a Polish fixup.}

		      else if VADDR < PROGRST {0 or JOBDAT} then
		        VRELBYTE:= RIGHT
(* 257 - better segment control *)
		      else if VADDR > MAXADDR then
			VRELBYTE := XSEG
		      else
			VRELBYTE := LOWSEG;
		      DPLMT := VADDR; INDEXR :=0; PACKFG := NOTPACK;
		      INDBIT:=0;
	      	      CHECKASSIGN(LATTR);
		      OLDVNOASSIGN := VNOASSIGN;
	              VNOASSIGN := TRUE;
		    end
		  else
		    begin
		      ERROR(364); TYPTR := nil
		    end
		end;
	      if LATTR.TYPTR # nil then
		if COMPTYPES(REALPTR,LATTR.TYPTR) or (LATTR.TYPTR^.FORM > SUBRANGE) then
		  begin
		    ERROR(365); LATTR.TYPTR := nil
		  end;
	      INSYMBOL
	    end
	  else
	    begin
	      ERRANDSKIP(209,FSYS or [BECOMES,TOSY,DOWNTOSY,DOSY]);
	      LATTR.TYPTR := nil
	    end;
	  if SY = BECOMES then
	    begin
	      INSYMBOL; EXPRESSION(FSYS or [TOSY,DOWNTOSY,DOSY],ONREGC);
	      if GATTR.TYPTR # nil then
		if GATTR.TYPTR^.FORM # SCALAR then
		  ERROR(315)
		else
		  if COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) then
		    begin
		      if LATTR.TYPTR # nil then
			if LATTR.TYPTR^.FORM = SUBRANGE then
			  LOADSUBRANGE(GATTR,LATTR.TYPTR)
			else
			  LOAD(GATTR)
		    end
		  else
		    ERROR(556);
	      LREGC := GATTR.REG
	    end
	  else
	    ERRANDSKIP(159,FSYS or [TOSY,DOWNTOSY,DOSY]);
	  if SY in [TOSY,DOWNTOSY] then
	    begin
	      LSY := SY; INSYMBOL; EXPRESSION(FSYS or [DOSY],ONREGC);
	      if GATTR.TYPTR # nil then
		if GATTR.TYPTR^.FORM # SCALAR then
		  ERROR(315)
		else
		  if COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) then
		    begin
		      with GATTR do
			{This test checks for forms of upper bound that must be copied into a local
			variable. Originally, they tried to use variables in place instead of
			copying, to save the MOVE, MOVEM.  The problem is that if the user changes
			the variable inside the loop, you have the wrong upper bound.  We
			interpret the language spec as requiring the bound to be evaluated only
			once, at the start.  The following test, commented out, was the original
			test, to see whether the object could be used in place for a CAMGE, or
			needed to be copied.  Now we copy all variables, as just discussed.}
			{IF ((KIND = VARBL) AND ((VLEVEL > 1) AND (VLEVEL < LEVEL) OR
			(PACKFG # NOTPACK) OR (INDEXR > 0) AND (INDEXR <= REGCMAX))) OR
			(KIND = EXPR) }
			if (KIND = VARBL) or (KIND = EXPR) then
			  begin
			    if LATTR.TYPTR # nil then
			      if LATTR.TYPTR^.FORM = SUBRANGE then
				LOADSUBRANGE(GATTR,LATTR.TYPTR)
			      else
				LOAD(GATTR);
(* 233 - better temp allocation *)
			    UBTEMP := TEMP_LC(1);
			    MACRO4(OP_MOVEM,REGC,BASIS,UBTEMP);
			    KIND := VARBL; INDBIT := 0; INDEXR := BASIS; VLEVEL := 1;
			    DPLMT := UBTEMP; PACKFG := NOTPACK; VRELBYTE := NO
			  end
			else
			  if LATTR.TYPTR # nil then
			    if (LATTR.TYPTR^.FORM = SUBRANGE) and RUNTMCHECK
			    then
			      begin
				(* must copy, since otherwise at end of loop
				 makecode will think it is in an AC *)
				SATTR := GATTR;
				LOADSUBRANGE(SATTR,LATTR.TYPTR)
			      end;
		      FETCHBASIS(LATTR);
		      with LATTR do
			begin
			  if (INDEXR>0) and (INDEXR<=REGCMAX) then
			    begin
			      MACRO(NO,OP_XMOVEI,INDEXR,INDBIT,INDEXR,DPLMT);
(* 233 - better temp allocation *)
			      SAVEINDEXR := TEMP_LC(1);
			      LINDBIT := 1; LDPLMT := SAVEINDEXR; LINDREG := BASIS;
			      MACRO4(OP_MOVEM,INDEXR,BASIS,LDPLMT);
			    end
			  else
			    begin
			      LINDBIT := INDBIT; LINDREG := INDEXR; LDPLMT := DPLMT
			    end;
			  LRELBYTE:= VRELBYTE
			end;
		      MACRO(LRELBYTE,OP_MOVEM,LREGC,LINDBIT,LINDREG,LDPLMT);
		      if LSY = TOSY then
			LINSTR := OP_CAMLE
		      else
			LINSTR := OP_CAMGE;
		      LADDR := IC;
		      MAKECODE(LINSTR,LREGC,GATTR)
		    end
		  else
		    ERROR(556)
	    end
	  else
	    ERRANDSKIP(251,FSYS or [DOSY]);
	  MACRO3(OP_JRST,0,FIX_UP);  LCIX := CIX;
	  if SY = DOSY then
	    INSYMBOL
	  else
	    ERROR(161);
(* 233 - better temp allocation [removed ADDTOLC] *)
	  STATEMENT(FSYS,STATENDS);
	  {if SAVEINDEXR <> 0 then
	    DISP_LC(SAVEINDEXR,1);}
	  {if UBTEMP <> 0 then
	    DISP_LC(UBTEMP,1);}
	  if LSY = TOSY then
	    LINSTR  := OP_AOS
	  else
	    LINSTR := OP_SOS;
	  MACRO(LRELBYTE,LINSTR,LREGC,LINDBIT,LINDREG,LDPLMT);
	  MACRO3R(OP_JRST,0,LADDR); INSERTADDR(RIGHT,LCIX,IC);
	  if LATTR.TYPTR <> NIL then
	    LCP^.VNOASSIGN := OLDVNOASSIGN
	end %FORSTATEMENT\;

      procedure LOOPSTATEMENT;
	var
	  LADDR: ADDRRANGE; LCIX: CODERANGE;
	begin
	  LADDR := IC;
	  loop
	    repeat
	      STATEMENT(FSYS or [EXITSY],STATENDS or [EXITSY])
	    until   not (SY in STATBEGSYS);
	  exit if SY # SEMICOLON;
	    INSYMBOL
	end;
	  if SY = EXITSY then
	    begin
	      INSYMBOL;
	      if SY = IFSY then
		begin
		  INSYMBOL; EXPRESSION(FSYS or [SEMICOLON,ENDSY],TRUEJMP);
		end
	      else
		ERRANDSKIP(162,FSYS or [SEMICOLON,ENDSY]);
	      LCIX := CIX;
	      loop
		repeat
		  STATEMENT(FSYS,STATENDS)
		until  not (SY in STATBEGSYS);
	      exit if SY # SEMICOLON;
		INSYMBOL
	    end;
	      MACRO3R(OP_JRST,0,LADDR); INSERTADDR(RIGHT,LCIX,IC)
	    end
	  else
	    ERROR(165);
	  if SY = ENDSY then
	    INSYMBOL
	  else
	    ERROR(163)
	end %LOOPSTATEMENT\;

      procedure WITHSTATEMENT;
	var
(* 233 - better temp allocation *)
	  I: INTEGER;
	  LCP: CTP; 
	  LCNT1: DISPRANGE; OLDREGC: ACRANGE;
	begin
	  LCNT1 := 0; OLDREGC := REGCMAX;
	  loop
	    if SY = IDENT then
	      begin
		IDSEARCH([VARS,FIELD],LCP,0); INSYMBOL
	      end
	    else
	      begin
		ERROR(209); LCP := UVARPTR
	      end;
	    SELECTOR(FSYS or [COMMA,DOSY],LCP);
	    if GATTR.TYPTR # nil then
	      if GATTR.TYPTR^.FORM = RECORDS then
		if TOP < DISPLIMIT then
		  begin
		    TOP := TOP + 1; LCNT1 := LCNT1 + 1;
		    WITHIX := WITHIX + 1;
		    DISPLAY[TOP].FNAME := GATTR.TYPTR^.FSTFLD;
		    with DISPLAY[TOP],GATTR do
		      begin
			OCCUR := CREC;  BLKNAME := '.FIELDID. ';
			if INDBIT = 1 then
			  GETPARADDR;
			FETCHBASIS(GATTR);
			if (INDEXR#0) and (INDEXR # BASIS) then
			  begin
			    if INDEXR # REGCMAX then
			      begin
				MACRO3(OP_MOVE,REGCMAX,INDEXR);
				INDEXR := REGCMAX
			      end;
			    REGCMAX := REGCMAX-1;
			    if REGCMAX < REGC then
			      begin
				ERROR(317); REGC := REGCMAX
			      end
			  end;
			CLEV := VLEVEL; CRELBYTE := VRELBYTE;
			CINDR := INDEXR; CINDB:=INDBIT;
			CDSPL := DPLMT;
			if (CINDR#0)  and  (CINDR#BASIS) then
			  CLC := TEMP_LC(1);
		      end
		  end
		else
		  ERROR(404)
	      else
		ERROR(308);
	  exit if SY # COMMA;
	    INSYMBOL
	end;
	  if SY = DOSY then
	    INSYMBOL
	  else
	    ERROR(161);
	  STATEMENT(FSYS,STATENDS);
	  REGCMAX:=OLDREGC;
(* 233 - better temp allocation *)
	  {for I := 0 to LCNT1-1 do
	    with DISPLAY[TOP-I] do
	      if (CINDR#0) and (CINDR # BASIS) then
		DISP_LC(CLC,1);}
	  TOP := TOP - LCNT1; WITHIX := WITHIX - LCNT1;
	end %WITHSTATEMENT\;

      begin %STATEMENT\
(* 233 - better temp allocation *)
{The following is part of the particular allocation strategy used by
TEMP_LC, and may change if it changes}
	LCBASE := LC;
	if SY = INTCONST then
	  begin %LABEL\
	    PRTERR := FALSE;
	    IDSEARCH([LABELT],LCP,0);
	    PRTERR := TRUE;
	    if LCP # nil then
	      with LCP^ do
		if SCOPE = LEVEL then
		  LABELADDRESS := IC;
	    for IX:=1 to LIX do
	      begin
		with LABELS[IX] do
		  if LABSVAL = VAL.IVAL then
		    begin
		      ERROR(211);
		      goto 1
		    end
	      end;
	    LIX := LIX+1;
	    if LIX > LABMAX then
	      begin
		ERROR(362);  LIX:=LABMAX
	      end;
	    with LABELS[LIX] do
	      begin
		LABSVAL:=VAL.IVAL;  LABSADDR:=IC
	      end;
	    for IX:=1 to JIX do
	      begin
		with GOTOS[IX] do
		  if GOTOVAL = VAL.IVAL then
		    begin
		      J:=CODE.INSTRUCTION[GOTOADDR].ADDRESS;
		      INSERTADDR(RIGHT,GOTOADDR,IC);
		      while J#0 do
			begin
			  GOTOADDR:=J;
			  J:=CODE.INSTRUCTION[GOTOADDR].ADDRESS;
			  INSERTADDR(RIGHT,GOTOADDR,IC)
			end;
		      GOTOVAL:=-1;
		      goto 1
		    end
	      end;
      1:
	    INSYMBOL;
	    if SY = COLON then
	      INSYMBOL
	    else
	      ERROR(151)
	  end;
	if DEBUG and not INITGLOBALS then
	  PUTLINER;
	if  not (SY in FSYS or [IDENT]) then
	  ERRANDSKIP(166,FSYS);
	if SY in STATBEGSYS or [IDENT] then
	  begin
	    REGC:=REGIN;
	    if INITGLOBALS and (SY # IDENT) then
	      ERROR(462)
	    else
	      case SY of
		IDENT:
		  begin
		    IDSEARCH([VARS,FIELD,FUNC,PROC],LCP,0); INSYMBOL;
		    if LCP^.KLASS = PROC then
		      if INITGLOBALS then
			ERROR(462)
		      else
			CALL(FSYS,LCP)
		    else
		      ASSIGNMENT(LCP)
		  end;
		BEGINSY:
		  begin
		    INSYMBOL; COMPOUNDSTATEMENT
		  end;
		GOTOSY:
		  begin
		    INSYMBOL; GOTOSTATEMENT
		  end;
		IFSY:
		  begin
		    INSYMBOL; IFSTATEMENT
		  end;
		CASESY:
		  begin
		    INSYMBOL; CASESTATEMENT
		  end;
		WHILESY:
		  begin
		    INSYMBOL; WHILESTATEMENT
		  end;
		REPEATSY:
		  begin
		    INSYMBOL; REPEATSTATEMENT
		  end;
		LOOPSY:
		  begin
		    INSYMBOL; LOOPSTATEMENT
		  end;
		FORSY:
		  begin
		    INSYMBOL; FORSTATEMENT
		  end;
		WITHSY:
		  begin
		    INSYMBOL; WITHSTATEMENT
		  end
		    end;
	    SKIPIFERR(STATENDS,506,FSYS)
	  end;
(* 233 - better temp allocation *)
	LC := LCBASE;
	REGC := REGIN  %RE-INITIALIZE REGISTER COUNTER TO AVOID OVERFLOW DURING SUBSEQUENT
	EXPRESSION EVALUATIONS IN REPEATSTATEMENT OR LOOPSTATEMENT \;
      end %STATEMENT\;
    begin %BODY\
      LIX:=0; JIX:=0; REGCMAX:=WITHIN; WITHIX := -1;  FIRSTKONST := nil;
      (* Polish fixups for CASE *)
      FIRSTPOL := nil;
      if not ENTRYDONE then
	begin
	  ENTRYDONE:= TRUE;
	  WRITEMC(WRITEENTRY);
	  WRITEMC(WRITENAME);
	  WRITEMC(WRITEHISEG)
	end;
      CIX := -1;
      if INITGLOBALS then
	begin
	  CGLOBPTR := nil;
	  loop
	    if SY # ENDSY then
	      STATEMENT([SEMICOLON,ENDSY],[SEMICOLON,ENDSY]);
	  exit if SY # SEMICOLON;
	    INSYMBOL
	end;
	  if SY = ENDSY then
	    INSYMBOL
	  else
	    ERROR(163);
	  WRITEMC(WRITEGLOBALS)
	end
      else
	begin      %BODY PROPER\
	  ENTERBODY;
	  if FPROCP # nil then
	    FPROCP^.PFADDR:= PFSTART
	  else
	    LC:= 1;
	  LCMAX := LC;
	  (* keep track of how many loc's above stack are used *)
	  STKOFFMAX := 0;
	  STKOFF := 0;
	  if MAIN or (LEVEL > 1) then
	    begin
	      loop
		repeat
		  STATEMENT(FSYS or [SEMICOLON,ENDSY],[SEMICOLON,ENDSY])
		until  not (SY in STATBEGSYS);
	      exit if SY # SEMICOLON;
		INSYMBOL
	    end;
	      if SY = ENDSY then
		INSYMBOL
	      else
		ERROR(163);
	      for IX:=1 to JIX do %TEST ON UNDEFINED LABELS\
		begin
		  with GOTOS[IX] do
		    if GOTOVAL # -1 then
		      begin
			ERROR(215);
(* 237 - get rid of errmptr *)
			if ERRMUSED < MAXERR then
			  begin
			    ERRMUSED := ERRMUSED + 1;
			    with ERRMARR[ERRMUSED] do
			      begin
			      FORM := D; NUMBER := 215; 
			      INTVAL := GOTOVAL; 
			      end
			  end;
		      end
		end
	    end;

	  LEAVEBODY;
	  WRITEMC(WRITECODE);
	  if FPROCP # nil then
	    WRITEMC(WRITEBLK);
	  (* Polish fixups for CASE *)
	  if FIRSTPOL # nil then
	    WRITEMC(WRITEPOLISH);
	  if FIRSTKONST # nil then
	    WRITEMC(WRITEINTERNALS)
	  else
	    if LOCALPFPTR # nil then
	      begin
		if LOCALPFPTR^.PFLEV = LEVEL then
		  WRITEMC(WRITEINTERNALS)
		  (* ALWAYS WRITE INTERNALS IF REF TO NON-LOC GOTO *)
		else
		  if LASTLABEL # nil then
		    if LASTLABEL^.SCOPE = LEVEL then
		      WRITEMC(WRITEINTERNALS)
	      end
	    else
	      if LASTLABEL # nil then
		if LASTLABEL^.SCOPE = LEVEL then
		  WRITEMC(WRITEINTERNALS);
	  if LEVEL = 1 then
	    begin
	      WRITEMC(WRITESYMBOLS);
	      WRITEMC(WRITELIBRARY);
	      WRITEMC(WRITESTART);
	      WRITEMC(WRITEEND)
	    end
	end % BODY PROPER\
    end %BODY\;
    (* PROCEDURES FOR FILE SWITCHING *)
  procedure OPENALT;
    begin
      REQFILE := TRUE;
      ORIGPAGECNT := PAGECNT; ORIGSUBPAGE := SUBPAGE; ORIGLINENR := LINENR;
      ORIGPAGE := PAGER; ORIGLINECNT := LINECNT; ORIGCH := CH;
      ENDSTUFF;
      PUSHF(INPUT,VAL.VALP^.SVAL,VAL.VALP^.SLGTH);
      if EOF then
	begin (* nb: on the 20, analys does not show the file name in most cases *)
	  WRITE('Failure to open INCLUDEd file: ',VAL.VALP^.SVAL:VAL.VALP^.SLGTH);
	  NEWLINE;
	  WRITELN(TTY,'Failure to open INCLUDEd file: ',VAL.VALP^.SVAL:VAL.VALP^.SLGTH);
	  ANALYS(INPUT); WRITELN(TTY);
	  REWRITE(OUTPUTREL);
	  CLRIBF;
	  CLOSE(INPUT);
	  POPF(INPUT);
	  PASXIT(INPUT,OUTPUT,OUTPUTREL)
	end;
      PAGECNT := 1; SUBPAGE := 0; LINECNT := 1; CH := ' ';
      READLN;  {because pushf does an interactive open}
      GETLINENR(LINENR);
      PAGEHEAD;
      WRITE(VAL.VALP^.SVAL:VAL.VALP^.SLGTH);
      NEWLINE; NEWLINE;
      BEGSTUFF
    end;

  procedure CLOSEALT;
    begin
      ENDSTUFF;
      POPF(INPUT);
      PAGECNT := ORIGPAGECNT; SUBPAGE := ORIGSUBPAGE + 1;
      PAGEHEAD;
      WRITE('Main file continued'); NEWLINE; NEWLINE;
      LINENR := ORIGLINENR; CH := ORIGCH;
      PAGER := ORIGPAGE; LINECNT := ORIGLINECNT;
      BEGSTUFF
    end;

  procedure INCLUSION;
    begin
      if not (SY = STRINGCONST) then
	begin
	  ERROR(212); REQFILE := FALSE
	end
      else
	begin
	  OPENALT;
	  INSYMBOL
	end
    end;
  begin %BLOCK\
    MARK(HEAPMARK);
    FILEINBLOCK[LEVEL] := FALSE;
    DP := TRUE; FORWPTR := nil;
(* 231 - save current end of TYPE_LIST *)
    OLD_TYPE_LIST := TYPE_LIST;
    repeat
      LCPAR := LC;
      repeat
	if (SY=INCLUDESY) or REQFILE then
	  begin
	    INSYMBOL;
	    INCLUSION;
	  end;
	if (SY = LABELSY) and not REQFILE then
	  begin
	    INSYMBOL; LABELDECLARATION
	  end;
	if SY = CONSTSY then
	  begin
	    INSYMBOL; CONSTANTDECLARATION
	  end;
	if SY = TYPESY then
	  begin
	    INSYMBOL; TYPEDECLARATION
	  end;
	if not REQFILE then
	  begin
	    LCPAR := LC;
	    if SY = VARSY then
	      begin
		INSYMBOL; VARDECLARATION
	      end;
	    {Note that FWDRESOLVE must be called after the VAR section because
	    ^FOO in the VAR section is treated as a forward reference to FOO.
	    We can't resolve this until after the end of the var section,
	    since otherwise we might accept ^FOO where FOO is a type in an
	    outer block, but a local variable in the current block.  This seems
	    to be illegal}
	    FWDRESOLVE;
	    while SY = INITPROCSY do
	      begin
		if LEVEL # 1 then
		  ERROR(557);
		INSYMBOL;
		if SY # SEMICOLON then
		  ERRANDSKIP(156,[BEGINSY])
		else
		  INSYMBOL;
		if SY = BEGINSY then
		  begin
		    MARK(GLOBMARK); INITGLOBALS := TRUE;
		    INSYMBOL; BODY(FSYS or [SEMICOLON,ENDSY]);
		    if SY = SEMICOLON then
		      INSYMBOL
		    else
		      ERROR(166);
		    INITGLOBALS := FALSE; RELEASE(GLOBMARK);
		  end
		else
		  ERROR(201);
	      end;
	    if LEVEL=1 then
	      LCMAIN := LC;
	  end;
	while SY in [PROCEDURESY,FUNCTIONSY] do
	  begin
	    LSY := SY; INSYMBOL; PROCDECLARATION(LSY)
	  end;
	while FORWPTR # nil do
	  with FORWPTR^ do
	    begin
	      if FORWDECL then
		ERRORWITHTEXT(465,NAME);
	      FORWPTR := TESTFWDPTR
	    end;
	(* REQ FILE ENDS IN PERIOD *)
	if (MAIN or (LEVEL > 1)) and not REQFILE then
	  begin
	    if SY # BEGINSY then
	      ERROR(201)
	  end
	    %This else is top level of /NOMAIN.  If anything is here
	     other than a period we have to turn on /MAIN, since otherwise
	     BODY will refuse to scan anything.\
	else
	  if SY # PERIOD then
	    begin
	      ERROR(172);
	      if not REQFILE then
		MAIN := TRUE
	    end;
	(* CLOSE REQFILE *)
	if REQFILE then
	  begin
	    REQFILE := FALSE;
	    CLOSEALT;
	    INSYMBOL;
	    if SY = SEMICOLON then
	      INSYMBOL
	    else
	      if SY = COMMA then
		REQFILE := TRUE
	      else
		ERROR(166);
	  end;
      until not ((SY in BLOCKBEGSYS - [BEGINSY]) or REQFILE);
      DP := FALSE;
      if SY = BEGINSY then
	INSYMBOL;
      %ELSE ERROR(201) REDUNDANT HERE - MSG PRINTED ABOVE\
      BODY(FSYS or [CASESY]);
      SKIPIFERR(LEAVEBLOCKSYS,166,FSYS)
    until SY in LEAVEBLOCKSYS;
    CREF_TYPES(OLD_TYPE_LIST);
    RELEASE(HEAPMARK);
  end %BLOCK\;
procedure ENTERSTDTYPES;
  var
    LBTP: BTP; LSP: STP;
  begin
    %TYPE UNDERLIEING:\
    %*****************\

    NEWZ(INTPTR,SCALAR,STANDARD);     %INTEGER\
    with INTPTR^ do
      begin
	SIZE := 1;BITSIZE := BITMAX; SELFSTP := nil
      end;
    NEWZ(REALPTR,SCALAR,STANDARD);    %REAL\
    with REALPTR^ do
      begin
	SIZE := 1;BITSIZE := BITMAX; SELFSTP := nil
      end;
    NEWZ(CHARPTR,SCALAR,STANDARD);    %CHAR\
    with CHARPTR^ do
      begin
	SIZE := 1;BITSIZE := 7; SELFSTP := nil
      end;
    NEWZ(BOOLPTR,SCALAR,DECLARED);    %BOOLEAN\
    with BOOLPTR^ do
      begin
	SIZE := 1;BITSIZE := 1; SELFSTP := nil
      end;
    NEWZ(NILPTR,POINTER);             %NIL\
    with NILPTR^ do
      begin
	ELTYPE := nil; SIZE := 1; BITSIZE := BITS_PER_ADDRESS; SELFSTP := nil
      end;
    NEWZ(TEXTPTR,FILES);                                      %TEXT\
    with TEXTPTR^ do
      begin
	FILTYPE := CHARPTR; SIZE := SIZEOFFILEBLOCK + 1; BITSIZE := BITMAX;
	FILEPF := FALSE; SELFSTP := nil; HASFILE := TRUE
      end;
    (* ALLOW "FILE" AS TYPE IN PROC DECL - ANY TYPE OF FILE *)
    NEWZ(ANYFILEPTR,FILES);
    with ANYFILEPTR^ do
      begin
	FILTYPE := nil; SIZE := SIZEOFFILEBLOCK + 1; BITSIZE := BITMAX;
	FILEPF := FALSE; SELFSTP := nil; HASFILE := TRUE
      end;
    NEWZ(LSP,SUBRANGE);
    with LSP^ do
      begin
	RANGETYPE := INTPTR; MIN.IVAL := 1; MAX.IVAL := 9; SELFSTP := nil
      end;
    NEWZ(DATEPTR,ARRAYS);
    with DATEPTR^ do
      begin
	ARRAYPF := TRUE; ARRAYBPADDR := 0;
	SELFSTP := nil; AELTYPE := CHARPTR; INXTYPE := LSP;
	SIZE := 2; BITSIZE := 36
      end;
    NEWZ(LBTP,ARRAYY);
    with LBTP^, BYTE do
      begin
	SBITS := 7; PBITS := BITMAX; XBIT := 0;
	IBIT := 0; IREG := BIXREG; RELADDR := 0;
	LAST := LASTBTP; LASTBTP := LBTP; ARRAYSP := DATEPTR
      end;
    NEWZ(LSP,SUBRANGE);
    with LSP^ do
      begin
	RANGETYPE := INTPTR; MIN.IVAL := 1; MAX.IVAL := ALFALENG; SELFSTP := nil
      end;
    NEWZ(ALFAPTR,ARRAYS);
    with ALFAPTR^ do
      begin
	ARRAYPF := TRUE; ARRAYBPADDR := 0;
	SELFSTP := nil; AELTYPE := CHARPTR; INXTYPE := LSP;
	SIZE := 2; BITSIZE := 36
      end;
    NEWZ(STRINGPTR,ARRAYS);
    with STRINGPTR^ do
      begin
	ARRAYPF := TRUE; SELFSTP := nil; AELTYPE := CHARPTR;
	INXTYPE := nil; SIZE := 2; BITSIZE := 36
      end;
    NEWZ(POINTERPTR,POINTER);
    with POINTERPTR^ do
      begin
	ELTYPE := nil; SIZE := 2; BITSIZE := 36; SELFSTP := nil
      end;
    NEWZ(POINTERREF,POINTER);
    with POINTERREF^ do
      begin
	ELTYPE := nil; SIZE := 2; BITSIZE := 36; SELFSTP := nil
      end;
    NEWZ(LBTP,ARRAYY);
    with LBTP^, BYTE do
      begin
	SBITS := 7; PBITS := BITMAX; XBIT := 0;
	IBIT := 0; IREG := BIXREG; RELADDR := 0;
	LAST := LASTBTP; LASTBTP := LBTP; ARRAYSP := ALFAPTR
      end;
  end %ENTERSTDTYPES\;

procedure ENTERSTDNAMES;
  var
    CP,CP1: CTP; I,J: INTEGER; LFILEPTR :FTP;
  begin
    %NAME:\
    %*****\

    NEWZ(CP,TYPES);                                           %INTEGER\
    with CP^ do
      begin
	NAME := 'INTEGER   '; NAMELEN := 7; IDTYPE := INTPTR; NEXT := nil
      end;
    ENTERID(CP);
    NEWZ(CP,TYPES);                                           %REAL\
    with CP^ do
      begin
	NAME := 'REAL      '; NAMELEN := 4; IDTYPE := REALPTR; NEXT := nil
      end;
    ENTERID(CP);
    NEWZ(CP, TYPES);                                           %CHAR\
    with CP^ do
      begin
	NAME := 'CHAR      '; NAMELEN := 4; IDTYPE := CHARPTR; NEXT := nil
      end;
    ENTERID(CP);
    NEWZ(CP,TYPES);                                           %BOOLEAN\
    with CP^ do
      begin
	NAME := 'BOOLEAN   '; NAMELEN := 7; IDTYPE := BOOLPTR; NEXT := nil
      end;
    ENTERID(CP);
    NEWZ(CP,TYPES);                                           %TEXT\
    with CP^ do
      begin
	NAME := 'TEXT      '; NAMELEN := 4; IDTYPE := TEXTPTR; NEXT := nil
      end;
    ENTERID(CP);
    NEWZ(CP,TYPES);
    with CP^ do
      begin
	NAME := 'ALFA      '; NAMELEN := 4; IDTYPE := ALFAPTR; NEXT := nil
      end;
    ENTERID(CP);
    NEWZ(CP,PARAMS);
    with CP^ do
      begin
	NAME := 'STRING    ';  NAMELEN := 6; IDTYPE := STRINGPTR; NEXT := nil
      end;
    ENTERID(CP);
(* 275 - string descriptor *)
    NEWZ(CP,PARAMS);
    with CP^ do
      begin
	NAME := 'STRINGPTR ';  NAMELEN := 9; IDTYPE := STRINGPTR; NEXT := nil
      end;
    ENTERID(CP);
    NEWZ(CP,PARAMS);
    with CP^ do
      begin
	NAME := 'POINTER   ';  NAMELEN := 7; IDTYPE := POINTERPTR; NEXT := nil
      end;
    ENTERID(CP);
    NEWZ(CP,KONST);                                           %NIL\
    with CP^ do
      begin
	NAME := 'NIL       '; NAMELEN := 3; IDTYPE := NILPTR;
	NEXT := nil; VALUES.IVAL := NIL_VALUE
      end;
    ENTERID(CP);
    NEWZ(CP,KONST);                                           %ALFALENG\
    with CP^ do
      begin
	NAME := 'ALFALENG  '; NAMELEN := 8; IDTYPE := INTPTR;
	NEXT := nil; VALUES.IVAL := 10
      end;
    ENTERID(CP);
    NEWZ(CP,KONST);
    with CP^ do
      begin
	NAME := 'MAXINT    '; NAMELEN := 6; IDTYPE := INTPTR;
	NEXT := nil; VALUES.IVAL := 377777777777B
      end;
    ENTERID(CP);
    CP1 := nil;
    for I := 1 to 2 do
      begin
	NEWZ(CP,KONST);                             %FALSE,TRUE\
	with CP^ do
	  begin
	    NAME := NA[I]; NAMELEN := NALEN[I]; IDTYPE := BOOLPTR;
	    NEXT := CP1; VALUES.IVAL := I - 1;
	  end;
	ENTERID(CP); CP1 := CP
      end;
    BOOLPTR^.FCONST := CP;

    for I := 3 to 6 do
      begin
	NEWZ(CP,VARS,FALSE);      %INPUT,OUTPUT,TTY,TTYOUTPUT\
	case I of
	  3:INFILE := CP;
	  4:OUTFILE := CP;
	  5:TTYFILE := CP;
	  6:TTYOUTFILE := CP
	end;
	with CP^ do
	  begin
	    NAME := NA[I]; NAMELEN := NALEN[I]; IDTYPE := TEXTPTR; 
	    CHANNEL := I-2;
	    VKIND := ACTUAL; NEXT := nil; VLEV := 0; VADDR:= LC;
	    LC := LC + 1 %BUFFERSIZE FOR TYPE CHAR\ + SIZEOFFILEBLOCK;
	    NEWZ(LFILEPTR);
	    with LFILEPTR^ do
	      begin
		NEXTFTP := FILEPTR;  FILEIDENT := CP
	      end;
	    FILEPTR := LFILEPTR
	  end;
	ENTERID(CP)
      end;
    SFILEPTR := FILEPTR;       %REMEMBER TOP OF STANDARD FILES\

(* 264 - %CCLSW is extern *)
    for I := 0 to MAXCCLSW do
      CCLSW[I] := 0;
    GLOBTOPP := LC; LC:=LC+1; GLOBBASIS := LC; LC:=LC+1;
    (* allow us to distinguish tops10 and tops20 specific ftns *)
    if TOPS10 then
      OTHERMACHINE := T20NAME
    else
      OTHERMACHINE := T10NAME;

    % GET,GETLN,PUT,PUTLN,RESET,REWRITE,READ,READLN,
     WRITE,WRITELN,PACK,UNPACK,NEW,MARK,RELEASE,GETLINR,
     PUT8BITSTOTTY,PAGE\

    for I := 7 to 25 do
      (* restrict tops10 and tops20 specific *)
      if MACHNA[I] # OTHERMACHINE then
	begin
	  NEWZ(CP,PROC,STANDARD);
	  with CP^ do
	    begin
	      NAME := NA[I]; NAMELEN := NALEN[I]; IDTYPE := nil;
	      NEXT := nil; KEY := I - 6
	    end;
	  if I = 24 then
	    begin
	      NEWZ(CP^.NAMEEXT,CHARFORM:13);
	      with CP^.NAMEEXT^ do
		begin
		  CHARS[11] := 'T'; CHARS[12] := 'T'; CHARS[13] := 'Y'
		end;
	    end;
	  ENTERID(CP)
	end;

    for I := 54 to 76 do
      if MACHNA[I] # OTHERMACHINE then
	begin
	  NEWZ(CP,PROC,STANDARD);
	  with CP^ do
	    begin
	      NAME := NA[I]; NAMELEN := NALEN[I]; IDTYPE := nil;
	      NEXT := nil; KEY := I - 32
	    end;
	  ENTERID(CP)
	end;

    NEWZ(CP1,VARS,FALSE);
    with CP1^ do
      begin
	NAME:='          '; NAMELEN := 0; IDTYPE:=ANYFILEPTR;
	VKIND:=FORMAL; NEXT:=nil; VLEV:=1; VADDR:=2
      end;

    for I:=77 to 79 do
      if MACHNA[I] # OTHERMACHINE then
	begin
	  NEWZ(CP,FUNC,DECLARED,ACTUAL);
	  with CP^ do
	    begin
	      NAME := NA[I]; NAMELEN := NALEN[I]; IDTYPE:=INTPTR; NEXT:=CP1;
	      FORWDECL:=FALSE;
	      EXTERNDECL := TRUE; PFLEV:=0; PFADDR:=0; PFCHAIN:=EXTERNPFPTR;
	      EXTERNPFPTR:=CP;
	      for J:=0 to MAXLEVEL do LINKCHAIN[J]:=0;
	      EXTERNALNAME:=NA[I];  LANGUAGE:=PASCALSY
	    end;
	  ENTERID(CP)
	end;

    if MACHNA[82] # OTHERMACHINE then
      begin
	NEWZ(CP,PROC,STANDARD);
	with CP^ do
	  begin
	    NAME := NA[82]; NAMELEN := NALEN[82]; IDTYPE := nil;
	    NEXT := nil; KEY := 45
	  end;
	ENTERID(CP)
      end;

    NEWZ(CP,FUNC,DECLARED,ACTUAL);
    with CP^ do
      begin
	NAME := NA[26]; NAMELEN := NALEN[26]; IDTYPE := DATEPTR; NEXT := nil;
	FORWDECL := FALSE;
	EXTERNDECL := TRUE; PFLEV := 0; PFADDR := 0; PFCHAIN := EXTERNPFPTR;
	EXTERNPFPTR := CP;
	for I := 0 to MAXLEVEL do LINKCHAIN[I] := 0;
	EXTERNALNAME := NA[26];  LANGUAGE := FORTRANSY
      end;
    ENTERID(CP);

    % RUNTIME,TIME,ABS,SQR,TRUNC,ODD,ORD,CHR,PRED,SUCC,EOF,EOLN \

    for I := 27 to 38 do
      begin
	NEWZ(CP,FUNC,STANDARD);
	with CP^ do
	  begin
	    NAME := NA[I]; NAMELEN := NALEN[I]; IDTYPE := nil;
	    NEXT := nil; KEY := I - 26
	  end;
	ENTERID(CP)
      end;

    for I := 80 to 81 do
      begin
	NEWZ(CP,FUNC,STANDARD);
	with CP^ do
	  begin
	    NAME := NA[I]; NAMELEN := NALEN[I]; IDTYPE := nil;
	    NEXT := nil; KEY := I - 66
	  end;
	ENTERID(CP)
      end;

    NEWZ(CP,VARS,FALSE); %PARAMETER OF PREDECLARED FUNCTIONS\
    with CP^ do
      begin
	NAME := '          '; IDTYPE := REALPTR;
	VKIND := ACTUAL; NEXT := nil; VLEV := 1; VADDR := 2
      end;

    % SIN,COS,EXP,SQRT,ALOG,ATAN,ALOG10,
     SIND,COSD,SINH,COSH,TANH,ASIN,ACOS,RAN \

    for I := 39 to 53 do
      begin
	NEWZ(CP1,FUNC,DECLARED,ACTUAL);
	with CP1^ do
	  begin
	    NAME := NA[I]; NAMELEN := NALEN[I]; IDTYPE := REALPTR; NEXT := CP;
	    FORWDECL := FALSE; EXTERNDECL := TRUE; PFLEV := 0; PFADDR := 0;
	    PFCHAIN:= EXTERNPFPTR; EXTERNPFPTR:= CP1; EXTERNALNAME := EXTNA[I];
	    for J := 0 to MAXLEVEL do LINKCHAIN[J] := 0;
	    LANGUAGE := EXTLANGUAGE[I]
	  end;
	ENTERID(CP1)
      end;
    LCMAIN := LC;
  end %ENTERSTDNAMES\;

procedure ENTERUNDECL;
  var
    I: INTEGER;
  begin
    NEWZ(UTYPPTR,TYPES);
    with UTYPPTR^ do
      begin
	NAME := '          '; IDTYPE := nil; NEXT := nil
      end;
    NEWZ(UCSTPTR,KONST);
    with UCSTPTR^ do
      begin
	NAME := '          '; IDTYPE := nil; NEXT := nil;
	VALUES.IVAL := 0
      end;
    NEWZ(UVARPTR,VARS,FALSE);
    with UVARPTR^ do
      begin
	NAME := '          '; IDTYPE := nil; VKIND := ACTUAL;
	NEXT := nil; VLEV := 0; VADDR := 0
      end;
    NEWZ(UARRTYP,ARRAYS);
    with UARRTYP^ do
      begin
	ARRAYPF := FALSE; SELFSTP := nil; AELTYPE := nil;
	INXTYPE := nil; SIZE := 777777B; BITSIZE := 36
      end;
    NEWZ(UFLDPTR,FIELD);
    with UFLDPTR^ do
      begin
	NAME := '          '; IDTYPE := nil; NEXT := nil; FLDADDR := 0;
	PACKF := NOTPACK
      end;
    NEWZ(UPRCPTR,PROC,DECLARED,ACTUAL);
    with UPRCPTR^ do
      begin
	NAME := '          '; IDTYPE := nil; FORWDECL := FALSE;
	for I := 0 to MAXLEVEL do LINKCHAIN[I] := 0;
	NEXT := nil; EXTERNDECL := FALSE; PFLEV := 0; PFADDR := 0
      end;
    NEWZ(UFCTPTR,FUNC,DECLARED,ACTUAL);
    with UFCTPTR^ do
      begin
	NAME := '          '; IDTYPE := nil; NEXT := nil;
	for I := 0 to MAXLEVEL do LINKCHAIN[I] := 0;
	FORWDECL := FALSE; EXTERNDECL := FALSE; PFLEV := 0; PFADDR := 0
      end;

    NEWZ(ULBLPTR,LABELT);
    with ULBLPTR^ do
      begin
	NAME := '          '; IDTYPE := nil; NEXT := nil;
	SCOPE := 0; GOTOCHAIN := 0; LABELADDRESS := 0
      end;
  end %ENTERUNDECL\;

procedure ENTERDEBNAMES;
  var
    CP:CTP;
  begin
    NEWZ(CP,PROC,STANDARD);
    with CP^ do
      begin
	NAME := 'PROTECTION'; NAMELEN := 10; IDTYPE := nil; NEXT := nil; 
	KEY:= 21
      end;
    ENTERID(CP);
  end;

function PASPRM(var I,O:TEXT;var R:INTFILE;var E:TEXT):RPGPT;
  extern;
begin {Main program}
  %ENTER STANDARD NAMES AND STANDARD TYPES:\
  %****************************************\

  REINIT;
  RTIME := RUNTIME; DAY := DATE;
  LEVEL := 0; TOP := 0;
  with DISPLAY[0] do
    begin
      FNAME := nil; OCCUR := BLCK; BLKNAME := '.PREDEFIN.'
    end;
  ENTERSTDTYPES; ENTERSTDNAMES; ENTERUNDECL; ENTERDEBNAMES;

(* 231 *)
  GLOB_TYPE_LIST := TYPE_LIST;

  TOP := 1; LEVEL := 1;
  with DISPLAY[1] do
    begin
      FNAME := nil; OCCUR := BLCK; BLKNAME := '.GLOBAL.  '
    end;

  %OPEN COMPILER FILES\
  %*******************\

  REWRITE(TTYOUTPUT);
  SCANDATA := PASPRM(INPUT,OUTPUT,OUTPUTREL,ERRFILE);
  with SCANDATA ^ do
    begin
      VERSION.WORD := VERVAL;
      (* I haven't figured out what to do about lookup blocks.  Commented out for now *)
      if TOPS10 then
	RESET(INPUT %,'',true,lookblock,40000B,4000B\)  {tag for SOS}
      else
	RESET(INPUT,'',0,0,0,20B);
      {see EOL char's}
      %if eof then begin         {tag for SOS}
       analys(input);
       pasxit(input,output,outputrel);
       end;
      get(input);\                 {tag for SOS}
      if VERSION.WORD = 0 then
	VERSION.WORD := LOOKBLOCK[6];
      LOOKBLOCK[6] := VERSION.WORD;
      for I := 1 to 5 do LOOKBLOCK[I] := 0;
      REWRITE(OUTPUT%,'',0,LOOKBLOCK\);  {tag for SOS}
      for I := 1 to 5 do LOOKBLOCK[I] := 0;
      REWRITE(OUTPUTREL%,'',0,LOOKBLOCK\);  {tag for SOS}
(* 231 - temporary file name until we fix scanner *)
      if TOPS10
        then REWRITE(ERRFILE,'','/O')
	else REWRITE(ERRFILE,'','/O');
      if not EOF(ERRFILE)
        then begin
	ANALYS(ERRFILE);
	WRITELN(TTY);
	WRITELN(TTY,'% No error file will be written');
	REWRITE(ERRFILE,'NUL:')
	end;
      FILENAME := RELNAME;
      if FILENAME = '          ' then
	FILENAME := '.NONAM    ';
      %A BLANK ENTRY NAME IS BAD NEWS\
      LISTCODE := LSW;
      TTYINUSE := TSW;
      MAIN := MSW;
      RUNTMCHECK := CSW;
      ARITHCHECK := ASW;
      DEBUGSWITCH := DSW;
      CREF:=CRSW;
      DEBUG := DSW;
      RPGENTRY := RPGSW;
      HEAP := HEAPVAL;
      STACK := STACKVAL;
      ZERO := ZSW;
      EXTENDED_ADDRESSING := XSW
    end;

  %WRITE HEADER\
  %************\

  PAGEHEAD;
  %NEW LINE FOR ERROR MESSAGES OR PROCEDURENAMES\
(* 255 - better page handling *)
  GETNEXTLINE(' ');     %GETS FIRST LINENUMBER IF ANY\
  CH := ' '; INSYMBOL; RESETFLAG := FALSE;
  if EXTENDED_ADDRESSING then
    begin
      BITS_PER_ADDRESS := 36;
      LIBRARY[PASCALSY].NAME := 'PSXLIB    '
    end;
  NILPTR^.BITSIZE := BITS_PER_ADDRESS;
(* 266 - 264 removed a bit too much from this test *)
  if not MAIN then
    begin
      LC := PROGRST; LCMAIN := LC;
    end;
(* 264 - files are always extern *)
      while SFILEPTR # nil do
	with SFILEPTR^, FILEIDENT^ do
	  begin
	    VADDR:= 0; SFILEPTR:= NEXTFTP
	  end;
      SFILEPTR := FILEPTR;

  %COMPILE:\
  %********\

  if CREF then
    WRITE(CHR(15B),CHR(CREF_BUG_LIMIT),'.GLOBAL.  ':CREF_BUG_LIMIT);
  for I := 1 to CHCNTMAX do ERRLINE[I] := ' ';
  RELBLOCK.COUNT:= 0;
  for SUPPORTIX := FIRSTSUPPORT to LASTSUPPORT do
    RNTS.LINK[SUPPORTIX] := 0;
  PROGSTAT;
  if MAIN then
    XADDRSYM := 'XMAIS.    '
  else
    begin
      XADDRSYM := FILENAME;
      I := 1;
      while (I < 7) and (XADDRSYM[I] <> ' ') do
	I := I + 1;
      if I = 7 {no blanks} then
	I := 3;  {As good as any - put the $ in the middle}
      XADDRSYM[I] := '$'
    end;
  if RPGENTRY then
    WRITELN(TTY,'PASCAL:',CHR(11B),FILENAME:6);
  BLOCK(nil,BLOCKBEGSYS or STATBEGSYS-[CASESY],[PERIOD]);

  if (HIGHESTCODE > MAXADDR) or (LCMAIN >= HIGHSTART) then
    ERROR(266);

  if CREF then
    WRITE(CHR(16B),CHR(CREF_BUG_LIMIT),'.GLOBAL.  ':CREF_BUG_LIMIT);
  CREF_TYPES(GLOB_TYPE_LIST);
  LEVEL := 0; TOP := 0;
  CREF_TYPES(nil);
  ENDOFLINE(TRUE);
  if CREF and not EOF(INPUT) then
    WRITE(CHR(177B),'A');
  %balances <ro>B from ENDOFLINE\
  NEWLINE; NEWLINE;
  if not ERRORFLAG then
    begin
      WRITE('No ');
      if not RPGENTRY then
	WRITE(TTY,'No ')
    end
  else
    WRITE(TTY,'?');
  WRITE('error detected'); NEWLINE;
  if (not RPGENTRY) or ERRORFLAG then
    WRITELN(TTY,'error detected');
(* 231 - add ERRFILE *)
  if ERRORFLAG then
    begin
      CLOSE(ERRFILE);
      REWRITE(OUTPUTREL);
      CLRIBF
    end
  else
      begin
(* 276 - new versions of Tops-10 require the sequence LOOKUP, RENAME *)
	if TOPS10 then
	  begin
	    CLOSE(ERRFILE);
	    RESET(ERRFILE,'',TRUE)
	  end;
	DELETE(ERRFILE);
	if not RPGENTRY then
	  WRITELN(TTY);
	NEWLINE;
	I := (HIGHESTCODE - HIGHSTART + 511) div 512;
	if not RPGENTRY then
	  WRITELN(TTY,'Highseg: ',I:3,'P');
	WRITE('Highseg: ',I:3,'P'); NEWLINE;
	I := (LCMAIN + 511) div 512;
	if not RPGENTRY then
	  WRITELN(TTY,'Lowseg : ',I:3,'P');
	WRITE('Lowseg : ',I:3,'P'); NEWLINE;
	I := (XC - 1000000B + 511) div 512;
	if I > 0 then
	  begin 
	    if not RPGENTRY then
	      WRITELN(TTY,'Extended data area : ',I:3,'P');
	    WRITE('Extended data area: ',I:3,'P'); NEWLINE;
	  end
      end;
  if not RPGENTRY then
    begin
      RTIME := RUNTIME - RTIME;
      WRITE(TTY,'Runtime: ',(RTIME div 60000):3,':');
      RTIME := RTIME mod 60000;
      WRITE(TTY,(RTIME div 1000):2,'.');
      RTIME := RTIME mod 1000;
      WRITELN(TTY,RTIME:3)
    end;
  PASXIT(INPUT,OUTPUT,OUTPUTREL)
end.
