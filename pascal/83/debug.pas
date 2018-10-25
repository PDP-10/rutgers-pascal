%$C-,M-,D-\
(*&Rl  for PFORM, make reserved words lower case *)
(*&Nu   make non-reserved words upper case *)
(*&Xu   make procedure names upper case *)
(*&I2   indent 2 spaces per level *)
(*&G2   indent contents of BEGIN/END 2 extra spaces *)
program DEBUG,DEBUG;
  INCLUDE 'pasprm.pas';
  INCLUDE 'pasext.pas';           (* set extended_addressing true or false *)
  %**************************************************************
   *                                                            *
   *                        PASCAL-DDT PROGRAM                  *
   *                        ******************                  *
   *                                                            *
   *          VERSION OF 29-Dec-81                              *
   *                                                            *
   *          AUTHOR: PETER PUTFARKEN                           *
   *          INSTITUT FUER INFORMATIK,  D - 2 HAMBURG 13       *
   *          SCHLUETERSTRASSE 70  /  GERMANY                   *
   *************************************************************\

  (* local change history
   !
   !prehistory      map lower case to upper, as in compiler
   ! 1       fix writefieldlist so it doesn't assume variant
   !            descriptors are sorted by VARVAL.  (They aren't.)
   ! 2       FIX LINEINTERVAL TO SET UP GPAGE.  NEEDED BY STOPSEARCH
   ! 3       detect uninitialized pointers, as well as NIL
   ! 4       fix writefieldlist to print tagid if not packed
   ! 5       do mark and release, for efficiency
   !            clear string to NIL each entry, in case user did release
   !            since he might have overwritten the old place
   ! 6       Type out ASCII using ^ for ctl char's
   ! 7       Add support for multiple modules
   ! 8       Get rid of NEW by passing pointers from outside
   ! 9       Fix CHARPTR to detect subranges of CHAR
   ! 10      output sets of char with new char translation
   ! 11      take care of new types LABELT and PARAM
   ! 12      Change the output file from TTY to Dump_file.  Keep all program
   !            error reports to file TTY
   ! 13      Add a stack-dump command. Procedures used to impliment this are
   !            ONE_VAR_OUT,SECTION_OUT,OUT,STACK_OUT.
   ! 14      Impliment a command to move about the stack by adding a
   !            parameter to the OPEN command.
   ! 15      Add the ability to kill all stops. eg. STOP NOT ALL.
   ! 16      Reformat the output from TRACEOUT so that it is easier to read
   ! 17      Rewrite WriteStructure to print identical contiguous array
   !            elements once only, with the range of indeces it is the value
   !            of.
   ! 18      Add an optional parameter to TRACE to specify how far down
   !            to trace the stack.
   ! 19      Merge the new code with my current release, and clean up
   !            ill-structured code (parameters passed as global variables).
   ! 20      Old tops-10 edit 13, to recover partially typed lines
   ! 21      Add access to source files
   ! 22      Prevent HELP END from proceeding!
   !            QUIT command
   !            SHOW command to set number of lines to show
   ! 23      let you use command names are variables
   ! 24      internal files
   ! 25      Hex and Octal printout
   ! 26      Make E=, i.e. abbreviations of END, work
   ! 27      Handle page marks correctly
   ! 28	     Extensive changes to support extended addressing
   ! 29	     4-word sets
   ! *)

const
  STOPMAX = 20;
  BUFFMAX = 120;
  BITMAX = 36;
  STRGLGTH = 120;
  NIL_VALUE = 0;
  BIXREG = 14B;         (* BIX is register used for byte pointer index *)
  OP_JUMP = 320B;
  OP_SKIPA = 334B;
  OP_PUSHJ = 260B;
  PREV_BASIS = -1;
  DYNAMIC_SUP = -2;
  BLANK=' ';
  FNAMESIZE = 170;
  CACHESIZE = 20;
  NUMPREDEC = 16;
type
  HALFWORD = 0..777777B;
  XADDRRANGE = 0..7777777777B;
  LOCAL_ADDRESS = HALFWORD;
  GLOBAL_ADDRESS = INTEGER;
  ACRANGE = 0..15; BIT = 0..1;
  LINEELEM = packed record
		      case INTEGER of
			   1: (CODE:0..677B;
			       AC:  ACRANGE;
			       IB:  BIT;
			       INXR:ACRANGE;
			       ADP: LOCAL_ADDRESS {^LINEELEM});
			   2: (CONSTANT1: INTEGER;
			       DB2: HALFWORD;
			       ABSLINE: HALFWORD)
		    end;
  PAGEELEM = packed record
		      INSTR: 0..677B;
		      AC: ACRANGE;
		      DUMMYBIT: BIT;
		      INXREG: ACRANGE;
		      PAGPTR: LOCAL_ADDRESS {^PAGEELEM};
		      LASTLINE: HALFWORD;
		      LASTSTOP: HALFWORD {^LINEELEM}
		    end;

  STRINGTYP = packed array [1:STRGLGTH] of CHAR;
  CSP = ^CONSTNT;
  CSTCLASS = (INT,REEL,PSET,STRD,STRG);
  CONSTNT = record
	      SELFCSP: CSP;
	      NOCODE: BOOLEAN;
	      case CCLASS: CSTCLASS of
		   REEL: (RVAL: REAL);
		   STRD,
		   STRG: (SLGTH: 0..STRGLGTH;
			  SVAL: STRINGTYP)
	    end;
  VALU = record
	   case INTEGER of
		1: (IVAL: INTEGER);
		2: (RVAL: REAL);
		3: (VALP: CSP)
	 end;
  BITS5 = 0..37B; BITS6 = 0..77B;
(* 29 - 4-word sets *)
  STRUCTFORM = (SCALAR,CONFORMANT,SUBRANGE,POINTER,POWER,QPOWER,ARRAYS,
		RECORDS,FILES,TAGFWITHID,TAGFWITHOUTID,VARIANT);
  FORMSET = set of STRUCTFORM;
  DECLKIND = (STANDARD,DECLARED);
  BIGNAME = record case Boolean of
	true: (CHARS: packed array[11..200]of CHAR);
	false:(WORDS: array[1..38] of INTEGER)
	end;
  STP = ^STRUCTURE; CTP = ^IDENTIFIER;
  NAMEPTR = ^BIGNAME;
  X_STP = LOCAL_ADDRESS;  X_CTP = LOCAL_ADDRESS;
  STRUCTURE = packed record
		       BITSIZE: 0..36;
		       SIZE: XADDRRANGE;
		       HASFILE: BOOLEAN;
		       NOCODE: BOOLEAN;
		       case FORM: STRUCTFORM of
			    SCALAR: (SCAL_DUM:0..3777B;
				     case SCALKIND: DECLKIND of
					  DECLARED: (FCONST: X_CTP));
			    SUBRANGE:       (SUB_DUM:0..7777B;
					     RANGETYPE: X_STP;
					     MIN,MAX: VALU);
			    CONFORMANT: (CONF_DUM:0..7777B;
				       BOUNDTYPE: X_STP;
				       UPPER,LOWER: X_CTP);
			    
			    POINTER:        (POIN_DUM:0..7777B;
					     ELTYPE: X_STP);
(* 28 - 4-word sets *)
			    QPOWER,
			    POWER:  (POW_DUM:0..7777B;
				     ELSET: X_STP);
			    ARRAYS: (ARR_DUM:0..3777B;
				     ARRAYPF: BOOLEAN;
				     ARRAYBPADDR: LOCAL_ADDRESS;
				     AELTYPE,INXTYPE: X_STP);
			    RECORDS:(REC_DUM:0..3777B;
				     RECORDPF:BOOLEAN;
				     FSTFLD: X_CTP;
				     RECVAR: X_STP);
			    FILES:  (FIL_DUM:0..3777B;
				     FILEPF: BOOLEAN;
				     FILTYPE: X_STP);
			    TAGFWITHID,
			    TAGFWITHOUTID: (TAG_DUM:0..7777B;
					    FSTVAR: X_STP;
					    case BOOLEAN of
						 TRUE: (TAGFIELDP: X_CTP);
						 FALSE: (TAGFIELDTYPE: X_STP));
			    VARIANT:(VAR_DUM:0..7777B;
				     NXTVAR,SUBVAR: X_STP;
				     FIRSTFIELD: X_CTP;
				     VARVAL: VALU)
		     end;
  (* ALFA = PACKED ARRAY[1..ALFALENG] OF CHAR; *)
  LEVRANGE = 0..8;
  IDCLASS = (TYPES,KONST,VARS,FIELD,PROC,FUNC,LABELT,PARAMS);
  IDKIND = (ACTUAL,FORMAL);
  PACKKIND = (NOTPACK,PACKK,HWORDR,HWORDL);

{If NAMELEN > 10, the extra characters are immediately before the
 IDENTIFIER record}
  IDENTIFIER = packed record
			NAME: ALFA;
			NAMELEN: 0..777777B; SPARE1: 0..777777B;
			LLINK, RLINK: X_CTP;
			IDTYPE: X_STP;
			NEXT: X_CTP;
			NOCODE: BOOLEAN;
			case KLASS: IDCLASS of
			     KONST: (VALUES: VALU);
			     VARS:  (VKIND: IDKIND;
				     EXTENDED: BOOLEAN;
				     VDUMMY: 0..7777B;
				     VADDR: LOCAL_ADDRESS;
				     XADDR: INTEGER);
			     FIELD: (PACKF: PACKKIND;
				     FLDADDR: XADDRRANGE)
		      end;
  ACR = ^ ACTIVATIONRECORD;
  X_ACR = LOCAL_ADDRESS;
  ACTIVATIONRECORD = array [0..0] of INTEGER;
  ATTRKIND = (CST,VARBL,EXPR);
  ATTR = record
	   TYPTR: STP;
	   case KIND: ATTRKIND of
		CST:  (CVAL: VALU);
		VARBL:(PACKFG: BOOLEAN;
		       GADDR: XADDRRANGE;
		       GBASIS: XADDRRANGE;
		       GBITCOUNT: 0:BITMAX)
	 end;
  LEFTORRIGHT=(LEFT,RIGHT,FULL);
  DEBPTR=^DEBUGENTRY;
  X_DEBPTR = LOCAL_ADDRESS;
  DEBUGENTRY = record
		 NEXTDEB: X_DEBPTR;
		 LASTPAGEELEM: PAGEELEM;
		 GLOBALIDTREE: X_CTP;
		 STANDARDIDTREE: X_CTP;
		 INTPTR: X_STP;
		 REALPTR: X_STP;
		 CHARPTR: X_STP;
		 MODNAME: ALFA;
		 FNAME: packed array [1..FNAMESIZE] of CHAR
	       end;
  STATUSKIND = (INITK, STOPK, DDTK, RUNTMERRK);
  BREAKKIND = (BRK_CLEAR, BRK_STEP, BRK_SKIP);

  DYNENTRY = record
	       REGISTRS: X_ACR;
	       STOPPY: INTEGER;
	       ENTRYPTR: X_DEBPTR;
	       STACKBOTTOM: X_ACR;
	       STATUS: STATUSKIND;
	       RETURNADDR: INTEGER
	     end;
  SYS = (STOPSY, TYPESY, FINDSY, TRACESY, STEPSY, ENDSY, NOTSY, LISTSY, EOLSY,
	 AMBIG, IDENT, INTCONST, STRINGCONST, CHARCONST, REALCONST, LBRACK,
	 RBRACK, COMMA, PERIOD, STAR, ARROW, PLUS, MINUS, SLASHSY, BECOMES,
	 EQSY, OPENSY, STACKDUMPSY, ALLSY, OTHERSY, COMMENT, HELPSY, QUITSY,
	 SHOWSY, TERMSY,DDTSY);
  SETOFSYS = set of SYS;
  PRINTTYPE = (DECIMAL,OCTAL,HEX);
var
  DUMP_FILE: TEXT;  {this file is used for the stackdump command, when it
		    is given a file name as an argument.  this file is
		    open only during execution of the command.}
  SOURCE: TEXT;  {this is the current source file}
  {Note: all_blank and depth_limit are local variables used in command
  parsing.  Do not refer to depth_limit as a global, but pass down its
  value if you want it.}
  ALL_BLANK: BOOLEAN;
  SECTION, CODE_SECTION,
  DEPTH_LIMIT: INTEGER;
  BASIS: ACR;
  CH: CHAR;
  ID: ALFA;
  IDEXT: BIGNAME;
  IDLEN: INTEGER;
  VAL: VALU;
  STRINIT: BOOLEAN;
  LGTH: INTEGER;
  I, CHCNT, LEFTSPACE: INTEGER;
  SY: SYS;
  PREDEC: array [1..NUMPREDEC] of ALFA;
  PREDECTRAN: array [1..NUMPREDEC] of SYS;
  PROCEED: BOOLEAN;  {Command sets this to cause exit from PASDDT}
  OLDEOLN: CHAR;
  BUFFER: packed array [1..BUFFMAX] of CHAR;
  BUFFLNG: 0:BUFFMAX;
  GPAGE: INTEGER;     %CURRENT PAGENUMBER\
  STOPTABLE: array [1..STOPMAX] of record
				     MODENTRY: DEBUGENTRY;
				     THISLINE, PAGE: INTEGER;
				     ORIGINALCONT: LINEELEM;
				     THISADDR: ^LINEELEM
				   end;
  STOPNR: 0..STOPMAX;
  ENTRY1: DEBUGENTRY;
  POINTERCV: record
	       case INTEGER of
		    0:(ADDR: INTEGER);
		    1:(FIX_ACR: ACR);
		    2:(STRINGPTR: ^STRINGTYP);
		    3:(FIX_DEBPTR: DEBPTR);
		    5:(FIX_CTP: CTP);
		    6:(FIX_STP: STP);
		    7:(FIX_ADRS: packed record
					  FIX_SECTION: HALFWORD;
					  FIX_LADR: LOCAL_ADDRESS
					end);
		    8:(FIX_PAGPTR: ^PAGEELEM);
		    9:(FIX_STOP: ^LINEELEM);
		   10:(FIX_GENPT: ^INTEGER);
		   11:(FIX_NAMEPTR: NAMEPTR)
	     end;
  CURENT: DEBPTR;  %This is used to check whether the currently
		    open module is the one where the current break is.  If
		    not, all the user can look at are global variables.
		    This is actually the nextdeb field from entry1 of
		    the broken module.  The nextdeb field is used to test
		    for equality, since it is different for each debugentry\
  SOURCEENT: DEBPTR;  %This plays the same role for the source file
		       mechanism.  this is the nextdeb field of the currently
		       open file\
  STEPMODE: BOOLEAN;
  ACCUS: ACR;
  LADDR: LOCAL_ADDRESS;
  DIGITS, LETTERSDIGITSORLEFTARROW: set of CHAR;
  NL: BOOLEAN;
  NULLPTR: ACR;
  GATTR: ATTR;
  SETMAP: array [0..177B] of INTEGER;
  CALL_BASIS: ACR;        {Basis of currently open stack level.}
  CALL_ADDRESS: INTEGER;
  POS_IN_STACK: INTEGER; {Stack level (small integer) currently open}
  NO_OF_CALLS: INTEGER;  {Largest stack level active}
  {Sourcefile line number stuff}
  STLINE,STPAGE: INTEGER;
  DOTLINE,DOTPAGE: INTEGER;  {Current page/line}
  SEARCHSTRING: STRINGTYP;  {Previous arg from FIND}
  SEARCHLENGTH: INTEGER;
  LINECACHE:array [0..CACHESIZE] of  {cache of info about positions in file}
  record
    NEXTDEB: DEBPTR;   {Copy of sourceent when entry was made.  Used
		       to verify that this entry is for the right file}
    CPAGE: INTEGER;    {source page for this entry - zero this to
		       invalidate the entry}
    CLINE: INTEGER;    {source line for this entry}
    CPOSITION: INTEGER {byte position in the file, for setpos}
  end;
  SHOWLINES: INTEGER;      {Number of lines to show in showcontext}
  PRINTRADIX: PRINTTYPE;   {Radix to print scalars}
  NO_OP_1,NO_OP_2: INTEGER;  {No-ops or LUUO's}

  (******************************************************************************************************)

initprocedure;
  begin
    DIGITS :=['0'..'9'];
    LETTERSDIGITSORLEFTARROW:=['A'..'Z','0'..'9', '_'];
    STRINIT := FALSE;
    SHOWLINES := 3;
  end;

initprocedure;
  begin
    PREDEC[1] := 'ALL       ';  PREDECTRAN[1] := ALLSY;
    PREDEC[2] := 'END       ';  PREDECTRAN[2] := ENDSY;
    PREDEC[3] := 'FIND      ';  PREDECTRAN[3] := FINDSY;
    PREDEC[4] := 'HELP      ';  PREDECTRAN[4] := HELPSY;
    PREDEC[5] := 'LIST      ';  PREDECTRAN[5] := LISTSY;
    PREDEC[6] := 'NOT       ';  PREDECTRAN[6] := NOTSY;
    PREDEC[7] := 'OPEN      ';  PREDECTRAN[7] := OPENSY;
    PREDEC[8] := 'QUIT      ';  PREDECTRAN[8] := QUITSY;
    PREDEC[9] := 'SHOW      ';  PREDECTRAN[9] := SHOWSY;
    PREDEC[10]:= 'STACKDUMP ';  PREDECTRAN[10]:= STACKDUMPSY;
    PREDEC[11]:= 'STEP      ';  PREDECTRAN[11]:= STEPSY;
    PREDEC[12]:= 'STOP      ';  PREDECTRAN[12]:= STOPSY;
    PREDEC[13]:= 'TERMS     ';  PREDECTRAN[13]:= TERMSY;
    PREDEC[14]:= 'TRACE     ';  PREDECTRAN[14]:= TRACESY;
    PREDEC[15]:= 'TYPE      ';  PREDECTRAN[15]:= TYPESY;
    PREDEC[16]:= 'DDT       ';  PREDECTRAN[16]:= DDTSY;
  end;

initprocedure %char mapping for set of char output\ ;
  begin
    SETMAP[0B] := 30B;      SETMAP[1B] := 11B;      SETMAP[2B] := 40B;      SETMAP[3B] := 41B;
    SETMAP[4B] := 42B;      SETMAP[5B] := 43B;      SETMAP[6B] := 44B;      SETMAP[7B] := 45B;
    SETMAP[10B] := 46B;     SETMAP[11B] := 47B;     SETMAP[12B] := 50B;     SETMAP[13B] := 51B;
    SETMAP[14B] := 52B;     SETMAP[15B] := 53B;     SETMAP[16B] := 54B;     SETMAP[17B] := 55B;
    SETMAP[20B] := 56B;     SETMAP[21B] := 57B;     SETMAP[22B] := 60B;     SETMAP[23B] := 61B;
    SETMAP[24B] := 62B;     SETMAP[25B] := 63B;     SETMAP[26B] := 64B;     SETMAP[27B] := 65B;
    SETMAP[30B] := 66B;     SETMAP[31B] := 67B;     SETMAP[32B] := 70B;     SETMAP[33B] := 71B;
    SETMAP[34B] := 72B;     SETMAP[35B] := 73B;     SETMAP[36B] := 74B;     SETMAP[37B] := 75B;
    SETMAP[40B] := 76B;     SETMAP[41B] := 77B;     SETMAP[42B] := 100B;    SETMAP[43B] := 101B;
    SETMAP[44B] := 102B;    SETMAP[45B] := 103B;    SETMAP[46B] := 104B;    SETMAP[47B] := 105B;
    SETMAP[50B] := 106B;    SETMAP[51B] := 107B;    SETMAP[52B] := 110B;    SETMAP[53B] := 111B;
    SETMAP[54B] := 112B;    SETMAP[55B] := 113B;    SETMAP[56B] := 114B;    SETMAP[57B] := 115B;
    SETMAP[60B] := 116B;    SETMAP[61B] := 117B;    SETMAP[62B] := 120B;    SETMAP[63B] := 121B;
    SETMAP[64B] := 122B;    SETMAP[65B] := 123B;    SETMAP[66B] := 124B;    SETMAP[67B] := 125B;
    SETMAP[70B] := 126B;    SETMAP[71B] := 127B;    SETMAP[72B] := 130B;    SETMAP[73B] := 131B;
    SETMAP[74B] := 132B;    SETMAP[75B] := 133B;    SETMAP[76B] := 134B;    SETMAP[77B] := 135B;
    SETMAP[100B] := 136B;   SETMAP[101B] := 137B;   SETMAP[102B] := 140B;   SETMAP[103B] := 173B;
    SETMAP[104B] := 174B;   SETMAP[105B] := 175B;   SETMAP[106B] := 176B;   SETMAP[107B] := 177B;
  end;
procedure ANALYS(var F:file);
  extern;

function ISDISK(var F:file):BOOLEAN;
  extern;

procedure DEBNAM(var F:file; S:STRING);
  extern;

procedure QUIT;
  extern;

function MAGIC(BASIS:ACR):INTEGER;
  extern;

procedure SETDDT(EXTENDED: BOOLEAN; DDTFUNC: BREAKKIND);
  extern; {change contents of PDDT.}

procedure GETDDT;
  extern;                   {load DDT if needed and jump to it}


procedure LOAD_ADDRESS(LOC: INTEGER);
  begin
    with POINTERCV do
      begin
	ADDR := LOC;
	if not EXTENDED_ADDRESSING then
	  FIX_ADRS.FIX_SECTION := 0
	else
	  if FIX_ADRS.FIX_SECTION = 0 then
	    if {(ADDR # 0) and} (ADDR # NIL_VALUE) then
	      FIX_ADRS.FIX_SECTION := CODE_SECTION
      end
  end {LOAD_ADDRESS};

function E_DEB(LOC: LOCAL_ADDRESS): DEBPTR;
  begin
    with POINTERCV do
      begin
	LOAD_ADDRESS(LOC);  E_DEB := FIX_DEBPTR
      end
  end {E_STP};

function E_STP(LOC: LOCAL_ADDRESS): STP;
  begin
    with POINTERCV do
      begin
	LOAD_ADDRESS(LOC);  E_STP := FIX_STP
      end
  end {E_STP};

function E_CTP(LOC: LOCAL_ADDRESS): CTP;
  begin
    with POINTERCV do
      begin
	LOAD_ADDRESS(LOC);  E_CTP := FIX_CTP
      end
  end {E_CTP};

function E_NAMEPTR(LOC: LOCAL_ADDRESS): NAMEPTR;
  begin
    with POINTERCV do
      begin
	LOAD_ADDRESS(LOC);  E_NAMEPTR := FIX_NAMEPTR
      end
  end;

  (* Hex and Octal printout *)
function HEXLEN(HEXNUM:INTEGER): INTEGER;
  (* find length of number in hex chars  *)
  var
    LEN: INTEGER;
    CVTHEX: record
	      case BOOLEAN of
		   TRUE: (INT:INTEGER);
		   FALSE: (HEX:packed array [1..9] of 0..15)
	    end;
  begin
    CVTHEX.INT := HEXNUM;  LEN := 9;
    while ((CVTHEX.HEX[10-LEN] = 0) and (LEN > 1)) do LEN := LEN - 1;
    HEXLEN := LEN
  end;

  (* Hex and Octal printout *)
function OCTLEN(OCTNUM:INTEGER): INTEGER;
  (* find length of number in octal chars  *)
  var
    LEN: INTEGER;
    CVTOCT: record
	      case BOOLEAN of
		   TRUE: (INT:INTEGER);
		   FALSE: (OCT:packed array [1..12] of 0..7)
	    end;
  begin
    CVTOCT.INT := OCTNUM;  LEN := 12;
    while ((CVTOCT.OCT[13-LEN] = 0) and (LEN > 1)) do LEN := LEN - 1;
    OCTLEN := LEN
  end;
procedure DEBUG(var ENTRY2:DYNENTRY;
		STRING:CSP;
		STRINGPTR,STRINGINDEX: STP);

{The compiler produces a linked list of line page entries, each of which
 points to a linked list of line number entries.  These are interspersed
 with code, where they show up as no-ops.  In order to implement
 single-stepping, we want to turn them from no-ops into LUUO's.  An
 LUUO causes execution of the instruction in location 41.  Normally we
 make this a no-op.  To do single-stepping, we just put a pushj to the
 debugger in location 41.  This extremely elegant suggestion is due to
 John Hall of Rutgers University.  It is probably slightly slower to
 execute an LUUO with location 41 than having real no-ops inline.  
 However we don't expect it to be more than about one instruction worth
 of time.  In non-zero sections we use XCT's instead, but that takes
 more code.
   This procedure traces down the list of line numbers turning all of
 the no-ops into LUUO's.  Different LUUO's are used for one-word and
 two-word line number entries, although at the moment no distinction is
 made in their processing. It may not work in non-zero sections, which
 is OK, since it isn't needed there.}
    PROCEDURE makeluuos;
    VAR
      lentry1:debugentry;
      PAGER: PAGEELEM; LINEPT: INTEGER;
      LADDR: HALFWORD;
     BEGIN
     if tops10 then protection(false);
     pointercv.addr := entry2.entryptr;	%first module\
     while pointercv.addr <> 0 do
        begin
        lentry1 := pointercv.fix_debptr^;
        PAGER := LENTRY1.LASTPAGEELEM;	%first page in module\
	while pager.pagptr <> 0 do
	   begin
	   linept := pager.laststop;
	      while linept <> 0 do
		 begin
	         pointercv.addr := linept;
	  	 with pointercv.fix_stop^ do
		   begin
  	    	   if code = op_jump
	             then code := 1%LUUO\
{Note: SKIPA is a two-word line number.  We leave the second word alone.
 It is already a no-op.  If we replaced it with another LUUO, we would
 get two breaks for that line when single-stepping.}
	    	   else if code = op_skipa
	             then code := 2%LUUO\
	    	   %else already LUUO, nothing\;
	  	   linept := adp
		   end;
	 	 end;  %search lines\
	   pointercv.addr := pager.pagptr;
	   pager := pointercv.fix_pagptr^
 	   end;
        pointercv.addr := lentry1.nextdeb;
        end; %module loop\
     if tops10 then protection(true)
     end; %makeluuos\

  procedure OPENSOURCE(var F:TEXT; var M:DEBUGENTRY);
    {Open the source file whose name is specified in the debugentry.
    Note that error recovery is used when the file is openned.
    If there is a problem, the user is asked for a new source file name, which
    is stored in debugentry, so he doesn't have to be asked again next time.}
    var
      I:INTEGER;
      LPTR,MNEXT:DEBPTR;
    begin
      loop
	if TOPS10 then
	  begin
	    DEBNAM(F,M.FNAME); RESET(F,'',TRUE,0,40000B,4000B)
	  end
	else
	  RESET(F,M.FNAME,TRUE,0,0,30B);
      exit if ISDISK(F) and not EOF(F);
	if not EOF(F) then
	  WRITELN(TTY,'? Source file must be on disk')
	else
	  ANALYS(F);
	WRITELN(TTY,'> Please specify a different file name for ',
		M.MODNAME,', or "NUL:" to ignore');
	WRITE(TTY,'*'); READLN(TTY); READ(TTY,M.FNAME);
	{Now that we have reset the name, we have to find the master record that
	it came from, since what we were passed is entry1, which is only a copy
	of the "real" record.  This is in case we come back to the same module
	later, we want to use the new name the user just gave us.}
        LPTR := E_DEB(ENTRY2.ENTRYPTR);
        MNEXT := E_DEB(M.NEXTDEB);
          LOAD_ADDRESS(LPTR^.NEXTDEB);
        while POINTERCV.FIX_DEBPTR <> MNEXT do
	  begin
	    LPTR := POINTERCV.FIX_DEBPTR; LOAD_ADDRESS(LPTR^.NEXTDEB)
	  end;
        LPTR^.FNAME := M.FNAME;
        {Also, we have to invalidate any cache entries for this file, since it
	now refers to something else}
				(* DEBUGGING CODE writeln(tty,'Just opened ',
				   m.fname,' checking cache'); *)
        for I := 1 to CACHESIZE do
	  with LINECACHE[I] do
				(* DEBUGGING CODE begin
				   if cpage <> 0
				     then writeln(tty,cline:1,'/',cpage:1,
							ord(nextdeb):6:o);
				   if nextdeb = mnext
				     then writeln(tty,'Invalidating'); *)
	    if NEXTDEB = MNEXT then
	      CPAGE := 0;
				(* DEBUGGING CODE
				   end; 
				   writeln(tty,'MNEXT, setting SOURCEENT: ',
				   ord(mnext):8:o); *)
	end;
      SOURCEENT := E_DEB(M.NEXTDEB); DOTLINE := 1; DOTPAGE := 1
    end;

  procedure CHECKSOURCE(var F:TEXT; var M:DEBUGENTRY);
    begin
				(* DEBUGGING CODE
				   writeln(tty,'Sourceent: ',
				   ord(sourceent):8:o,' m.nextdeb: ',
				   ord(e_deb(m.nextdeb)):8:o); *)
      if SOURCEENT # E_DEB(M.NEXTDEB) then
	OPENSOURCE(F,M)
    end;

  procedure FINDPGLN (var F:TEXT; PAGE,LINE:INTEGER);

    label
      666;
    var
      I,CURPAGE,CURLINE:INTEGER;
      CURSOSNUM,SOSNUM:packed array [1..5] of CHAR;
      BESTIND,BESTPAGE,BESTLINE,BESTPOS: INTEGER;
      DONTCACHE: BOOLEAN;  {Don't enter in cache}

    begin
      {First try to come as near as possible using cache}
      {Can always start at beginning!}
      BESTPAGE := 1; BESTLINE := 1;  BESTPOS := 0; BESTIND := 0;
      DONTCACHE := FALSE;

      (*DEBUGGING CODE
       for i := 1 to 10 do with linecache[i] do
       writeln(tty,i:2,':',cline:0,'/',cpage:0,',',cposition:0);
       END DEBUGGING *)

      for I := 1 to CACHESIZE do
	with LINECACHE[I] do
	  if NEXTDEB = SOURCEENT then
	    if (CPAGE < PAGE)   {cache loc <= requested}
	      or ((CPAGE = PAGE) and (CLINE <= LINE)) then
	      if (CPAGE > BESTPAGE)    {cache loc > best seen}
		or ((CPAGE = BESTPAGE) and (CLINE > BESTLINE)) then
		begin   {Found better candidate: use it}
		  BESTPAGE := CPAGE;    BESTLINE := CLINE;
		  BESTPOS := CPOSITION; BESTIND := I
		end;

      (* DEBUGGING CODE
       if bestind <> 0 then
       writeln(tty,'Cache hit, entry ',bestind:0,', page=',bestpage:0,
       ', line=',bestline:0,', pos=',bestpos:0);
       END DEBUGGING *)

      {Now move this entry to the top of cache}
      if BESTIND > 2  {Needs movement} then
	begin
	  LINECACHE[0] := LINECACHE[BESTIND];  {save best one}
	  for I := BESTIND downto 2 do    {move others down}
	    LINECACHE[I] := LINECACHE[I-1];
	  LINECACHE[1] := LINECACHE[0];    {put best in top position}
	end;
      {Now go to best starting position}
      SETPOS(F,BESTPOS);
      {And move forward if needed}
      {After this loop we are on the first char of the requested page.
      Note that we are following the convention that an SOS page mark
      is equivalent to changing the end of line for the previous
      line to a form feed}
      CURLINE := BESTLINE;
      for CURPAGE := BESTPAGE+1 to PAGE do
	begin
	  while not EOF(F) and (F^ <> CHR(14B)) do
	    GET(f);
          if EOF(F) then
	    begin
	      LINE := 1; PAGE := CURPAGE; goto 666
	    end;
	  GETLINENR(F,CURSOSNUM);
{We check for the case of form feed in the middle of a line in an SOS
 file.  We have to remember this because then the line does not begin
 with an SOS line number.  Thus we had better not enter it in the cache.}
	  if (CURSOSNUM <> '-----') and (CURSOSNUM <> '     ') then
	    DONTCACHE := TRUE;
          GET(F);  {first char of new page}
          CURLINE := 1;
        end;
      {We have now found page}
      GETLINENR(F,CURSOSNUM);
      if CURSOSNUM = '-----'  {File not SOS numbered} then
	for CURLINE := CURLINE+1 to LINE do
	  begin
	    while not EOLN(F) do GET(F);
	    while (F^ = CHR(15B)) do GET(F);
	    if (F^ = CHR(14B)) or EOF(F) then
	      begin
		READLN(F); PAGE := PAGE + 1; LINE := 1; goto 666
	      end;
	    if EOLN(F) then  {skip EOLN - nothing if bare CR}
	      GET(F)
	  end
      else
	begin
	  if (LINE > 99999) or (LINE < 0) then
	    SOSNUM := 'AAAAA'   {something bigger than any legal number}
	  else
	    for I := 0 to 4 do
	      begin
		SOSNUM[5-I] := CHR((LINE mod 10) + 60B);
		LINE := LINE div 10
	      end;
	  while (CURSOSNUM < SOSNUM) do
	    begin
	      DONTCACHE := FALSE;
	      while not EOLN(F) do GET(F);
	      while (F^ = CHR(15B)) do GET(F);
	      if (F^ = CHR(14B)) or EOF(F) then
	        begin
		  READLN(F); PAGE := PAGE + 1; LINE := 1; goto 666
	        end;
	      if EOLN(F) then  {skip EOLN - nothing if bare CR}
	        GET(F);  {first char of next line}
	      GETLINENR(F,CURSOSNUM)
	    end;
	  if CURSOSNUM = '     ' then  {as we check for FF above, this
					should never happen}
	    LINE := 1
	  else
	    begin
	      LINE := 0;
	      for I := 1 to 5 do LINE := LINE*10 + (ORD(CURSOSNUM[I]) - 60B)
	    end
	end;
      {We found the thing we wanted exactly, so put it in the cache if not there}
      if not EOF(F) and ((LINE <> BESTLINE) or (PAGE <> BESTPAGE)) 
	and not DONTCACHE then
	begin

	  (* DEBUGGING CODE
	   writeln(tty,'Entering in cache: page=',page:0,', line=',line:0,
	   ', pos=',curpos(f)-1);
	   END DEBUGGING *)

	  for I := CACHESIZE downto 2 do   {make space for new entry}
	    LINECACHE[I] := LINECACHE[I-1];
	  with LINECACHE[1] do   {now make it entry 1}
	    begin
	      NEXTDEB := SOURCEENT;
	      CPAGE := PAGE; CLINE := LINE;
	      if CURSOSNUM <> '-----' then
		CPOSITION := CURPOS(F) - 7
	      else
		CPOSITION := CURPOS(F) - 1
	    end
	end;
    666:
      if not EOF(F) then
	begin
	  DOTLINE := LINE; DOTPAGE := PAGE
	end
    end;

  procedure NEXTLINE(VAR F:TEXT;VAR LINE,PAGE:INTEGER);
      var CURSOSNUM:packed array [1..5] of CHAR;
    begin
      while not EOLN(F) do GET(F);
      while (F^ = CHR(15B)) do GET(F);
      if (F^ = CHR(14B)) or EOF(F) then
	begin
	  PAGE := PAGE + 1; LINE := 1
	end
      else
	LINE := LINE + 1;
      if EOLN(F) then {skip EOLN - nothing if bare CR}
        GET(F);  {first char of next line}
{SOS line numbers are invisible, so we skip any that we see}
      GETLINENR(F,CURSOSNUM);
      while not EOF(F) and (CURSOSNUM = '     ') do
	begin
	  PAGE := PAGE + 1; LINE := 1;
	  READLN(F);
	  GETLINENR(F,CURSOSNUM)
	end
    end;      

  procedure STSEARCH(ST:STRINGTYP;LEN:INTEGER;LINE,PAGE:INTEGER);
    {Assumes the file is positioned to the first character to be searched for
    Things are left pointing to the start of text on the last line searched
    (or EOF).}
    var
      TSTRING,AHEAD,SEEN:array [1..STRGLGTH] of CHAR;
      LASTPOS,TARGET,AHEADPT:INTEGER;
      CUR:CHAR;
      CURSOSNUM:packed array [1..5] of CHAR;
    begin
      {Start by skipping a line, since search is never supposed to stay on
      the existing line.}
      for TARGET := 1 to LEN do
	if (ST[TARGET] >= 'a') and (ST[TARGET] <= 'z') then
	  TSTRING[TARGET] := CHR(ORD(ST[TARGET]) - 40B)
	else
	  TSTRING[TARGET] := ST[TARGET];
      NEXTLINE(SOURCE,LINE,PAGE);
      if (SOURCE^ >= 'a') and (SOURCE^ <= 'z') then
	SOURCE^ := CHR(ORD(SOURCE^) - 40B);
      LASTPOS := CURPOS(SOURCE)-1;  AHEADPT := 0;
      loop
	TARGET := 1;
	loop
	  if AHEADPT <= 0 then
	    begin
	      CUR := SOURCE^;
	      if EOLN(SOURCE) then
		begin
		  NEXTLINE(SOURCE,LINE,PAGE);
		  LASTPOS := CURPOS(SOURCE) - 1
		end
	      else
		GET(SOURCE);
	      if (SOURCE^ >= 'a') and (SOURCE^ <= 'z') then
		SOURCE^ := CHR(ORD(SOURCE^) - 40B);
	    end
	  else
	    begin
	      CUR := AHEAD[AHEADPT]; AHEADPT := AHEADPT - 1
	    end;
	  SEEN[TARGET] := CUR;
	exit if (CUR <> TSTRING[TARGET]) or (TARGET = LEN);
	  TARGET := TARGET+1
      end;
      exit if EOF(SOURCE) or (CUR = TSTRING[TARGET]);
	for TARGET := TARGET downto 2 do
	  begin
	    AHEADPT := AHEADPT + 1;
	    AHEAD[AHEADPT] := SEEN[TARGET]
	  end
    end;
      GETLINENR(SOURCE,CURSOSNUM);
      if CURSOSNUM <> '-----' then
	  begin  {note: NEXTLINE makes sure it is never a page mark}
	    LINE := 0;
	    for I := 1 to 5 do LINE := LINE*10 + (ORD(CURSOSNUM[I]) - 60B)
	  end;
      if not EOF(SOURCE) then
	begin
	  SETPOS(SOURCE,LASTPOS); DOTLINE := LINE; DOTPAGE := PAGE
	end
    end;

  procedure SHOWSTCONTEXT(GOTSTRING:BOOLEAN;REPCOUNT:INTEGER);
    var
      R,LINE,PAGE:INTEGER;
      SOSNUM:packed array [1..5] of CHAR;
    begin
      PAGE := DOTPAGE;
      FINDPGLN(SOURCE,DOTPAGE,DOTLINE);
      if GOTSTRING then
	begin
	  SEARCHSTRING := STRING^.SVAL;
	  SEARCHLENGTH := STRINGINDEX^.MAX.IVAL
	end;
      for R := 1 to REPCOUNT do
	STSEARCH(SEARCHSTRING,SEARCHLENGTH,DOTLINE,DOTPAGE);
      if (PAGE <> DOTPAGE) and not EOF(SOURCE) then
	WRITELN(TTY,'Page ',DOTPAGE:0);
      LINE := DOTLINE; PAGE := DOTPAGE;
      for I := 0 to SHOWLINES-1 do
	begin
	  if EOF(SOURCE) then
	    goto 1;
	  GETLINENR(SOURCE,SOSNUM);
	  if SOSNUM = '-----' then
	    WRITE(TTY,LINE:0,' ')
	  else if SOSNUM = '     ' then
	   {nothing}
	  else 
	    WRITE(TTY,SOSNUM,' ');
	  while not EOLN(SOURCE) do 
	    begin 
	    WRITE(TTY,SOURCE^);
	    GET(SOURCE);
	    end;
	  if SOSNUM <> '     ' then
	    WRITELN(TTY);
	  while SOURCE^ = CHR(15B) do GET(SOURCE);
	  if SOURCE^ = CHR(14B) then
	    begin
	      LINE := 1;  PAGE := PAGE + 1;
	      WRITELN(TTY,'Page ',PAGE:0);
	    end
	  else LINE := LINE + 1;
	  if EOLN(SOURCE) then {skip EOLN - none if bare CR}
	    GET(SOURCE);
	end;
    1:
    end;

  procedure SHOWCONTEXT(PAGE,LINE:INTEGER);
    var
      I:INTEGER;
      SOSNUM:packed array [1..5] of CHAR;
    begin
      if PAGE <= 0 then
	PAGE := 1;
      if LINE <= 0 then
	LINE := 1;
      FINDPGLN(SOURCE,PAGE,LINE);
      PAGE := DOTPAGE; LINE := DOTLINE;
      for I := 0 to SHOWLINES-1 do
	begin
	  if EOF(SOURCE) then
	    goto 1;
	  GETLINENR(SOURCE,SOSNUM);
	  if SOSNUM = '-----' then
	    WRITE(TTY,LINE:0,' ')
	  else if SOSNUM = '     ' then
	   {nothing}
	  else 
	    WRITE(TTY,SOSNUM,' ');
	  while not EOLN(SOURCE) do 
	    begin 
	    WRITE(TTY,SOURCE^);
	    GET(SOURCE);
	    end;
	  if SOSNUM <> '     ' then
	    WRITELN(TTY);
	  while SOURCE^ = CHR(15B) do GET(SOURCE);
	  if SOURCE^ = CHR(14B) then
	    begin
	      LINE := 1;  PAGE := PAGE + 1;
	      WRITELN(TTY,'Page ',PAGE:0);
	    end
	  else LINE := LINE + 1;
	  if EOLN(SOURCE) then {skip EOLN - none if bare CR}
	    GET(SOURCE);
	end;
    1:
    end;

  procedure ERROR;
    begin
      WRITE(TTY, '> ', '^ ':CHCNT+1);  GATTR.TYPTR := nil
    end;

  function ENDOK:BOOLEAN;
    begin
      ENDOK := TRUE;
      if SY <> EOLSY then
	begin
	  ERROR;  WRITELN(TTY,'Junk after end of command');
	  ENDOK := FALSE
	end
    end;

  procedure NEWLINE(var OUTFILE:TEXT);
    begin
      WRITELN(OUTFILE);
      WRITE(OUTFILE,'> ',' ':LEFTSPACE);
      CHCNT := LEFTSPACE
    end;

  function LENGTH(FVAL: INTEGER): INTEGER;
    var
      E, H: INTEGER;
    begin
      if FVAL < 0 then
	begin
	  E := 1; FVAL := -FVAL
	end
      else
	E := 0;
      H := 1;
      if FVAL >= 10000000000 then
	E := E + 11
      else
	repeat
	  E := E + 1; H := H * 10
	until (FVAL < H);
      LENGTH := E
    end;
  procedure INSYMBOL;
    const
      MAXEXP = 35;
    var
      IVAL,SCALE,EXP: INTEGER;
      RVAL,R,FAC: REAL;
      STRINGTOOLONG, SIGN: BOOLEAN;

    procedure NEXTCH;
      begin
	if EOLN(TTY) then
	  CH:=' '
	else
	  READ(TTY,CH);
	if ORD(CH) >= 140B then
	  CH := CHR(ORD(CH)-40B);
	CHCNT := CHCNT + 1
      end;

    procedure NEXTCHSTR;
      begin
	if EOLN(TTY) then
	  CH:=' '
	else
	  READ(TTY,CH);
	CHCNT := CHCNT + 1
      end;

    begin
      while not EOLN(TTY) and (CH=' ') do NEXTCH;
      case CH of
	' ': SY := EOLSY;
	';','!': SY := COMMENT;
	'A','B','C','D','E','F','G','H','I','J','K','L','M',
	'N','O','P','Q','R','S','T','U','V','W','X','Y',
	'Z':
	  begin
	    ID := '          '; I := 0;
	    repeat
	      if I < ALFALENG then
		begin
		  I := I + 1;  ID[I] := CH
		end
	      else if I < 200 then
		begin
		  I := I + 1;  IDEXT.CHARS[I] := ch
		end;
	      NEXTCH
	    until not (CH in LETTERSDIGITSORLEFTARROW);
	    IDLEN := I;
	    if I > 10 then
	      for I := I + 1 to ((I + 4) div 5) * 5 do
		IDEXT.CHARS[I] := ' ';
	    SY := IDENT;
	  end;
	'0','1','2','3','4','5','6','7','8',
	'9':
	  begin
	    IVAL := 0; SY := INTCONST;
	    repeat
	      IVAL := 10*IVAL + ORD(CH)-ORD('0');
	      NEXTCH
	    until not (CH in DIGITS);
	    SCALE := 0;
	    if CH = '.' then
	      begin
		NEXTCH;
		if CH = '.' then
		  CH := ':'
		else
		  begin
		    RVAL := IVAL; SY := REALCONST;
		    if not (CH in DIGITS) then
		      begin
			ERROR; WRITELN(TTY,'Digit must follow')
		      end
		    else
		      repeat
			RVAL := 10.0*RVAL + (ORD(CH) - ORD('0'));
			SCALE := SCALE - 1; NEXTCH
		      until not (CH in DIGITS)
		  end
	      end;
	    if CH = 'E' then
	      begin
		if SCALE = 0 then
		  begin
		    RVAL := IVAL; SY := REALCONST
		  end;
		NEXTCH; EXP := 0;
		SIGN := CH='-';
		if (CH = '+') or SIGN then
		  NEXTCH;
		if not (CH in DIGITS) then
		  begin
		    ERROR; WRITELN(TTY,'Digit must follow')
		  end
		else
		  repeat
		    EXP := 10*EXP + ORD(CH) - ORD('0'); NEXTCH
		  until not (CH in DIGITS);
		if SIGN then
		  SCALE := SCALE - EXP
		else
		  SCALE := SCALE + EXP;
		if ABS(SCALE + LENGTH(IVAL) - 1) > MAXEXP then
		  begin
		    ERROR; WRITELN(TTY,'Exponent too large'); SCALE := 0
		  end
	      end;
	    if SCALE # 0 then
	      begin
		R := 1.0;   %NOTE POSSIBLE OVERFLOW OR UNDERFLOW\
		if SCALE < 0 then
		  begin
		    FAC := 0.1; SCALE := -SCALE
		  end
		else
		  FAC := 10.0;
		repeat
		  if ODD(SCALE) then
		    R := R*FAC;
		  FAC := SQR(FAC); SCALE := SCALE div 2
		until SCALE = 0;
		%NOW R = 10^SCALE\
		RVAL := RVAL*R
	      end;
	    if SY = INTCONST then
	      VAL.IVAL := IVAL
	    else
	      VAL.RVAL := RVAL
	  end;
	'=':
	  begin
	    SY := EQSY; NEXTCH
	  end;
	':':
	  begin
	    NEXTCH;
	    if CH = '=' then
	      begin
		SY := BECOMES; NEXTCH
	      end
	    else
	      SY := OTHERSY
	  end;
	'''':
	  begin
	    LGTH := 0; STRINGTOOLONG := FALSE;
	    if not STRINIT then
	      begin
		STRINIT := TRUE;
		with  STRINGINDEX^ do
		  begin
		    SIZE := 1; BITSIZE := 7;  FORM := SUBRANGE;
		    RANGETYPE := ENTRY1.INTPTR; MIN.IVAL := 1
		  end;
		with STRINGPTR^ do
		  begin
		    BITSIZE := BITMAX; AELTYPE := ENTRY1.CHARPTR;
		    FORM := ARRAYS;
		    INXTYPE := ORD(STRINGINDEX); ARRAYPF := TRUE
		  end;
		STRING^.CCLASS := STRG
	      end;
	    repeat
	      repeat
		NEXTCHSTR;
		if LGTH < STRGLGTH then
		  begin
		    LGTH := LGTH + 1; STRING^.SVAL[LGTH] := CH
		  end
		else
		  STRINGTOOLONG := TRUE
	      until EOLN(TTY) or (CH = '''');
	      if STRINGTOOLONG then
		begin
		  ERROR; WRITELN(TTY,'String constant is too long')
		end;
	      if CH # '''' then
		begin
		  ERROR; WRITELN(TTY,'String constant contains "<CR><LF>"')
		end
	      else
		NEXTCH
	    until CH # '''';
	    LGTH := LGTH - 1;   %NOW LGTH = NR OF CHARS IN STRING\
	    if LGTH = 1 then
	      begin
		SY := CHARCONST; VAL.IVAL := ORD(STRING^.SVAL[1]);
		STRINGINDEX^.MAX.IVAL := 1;
		STRINGPTR^.SIZE := 1;
	      end
	    else
	      begin
		SY := STRINGCONST;
		STRINGINDEX^.MAX.IVAL := LGTH;
		STRINGPTR^.SIZE := (LGTH + 4) div 5;
		STRING^.SLGTH := LGTH;
		VAL.VALP := STRING
	      end
	  end;
	'/':
	  begin
	    SY := SLASHSY; NEXTCH
	  end;
	'[':
	  begin
	    SY := LBRACK; NEXTCH
	  end;
	']':
	  begin
	    SY := RBRACK; NEXTCH
	  end;
	'.':
	  begin
	    SY := PERIOD; NEXTCH
	  end;
	'*':
	  begin
	    SY := STAR; NEXTCH
	  end;
	'^':
	  begin
	    SY := ARROW; NEXTCH
	  end;
	',':
	  begin
	    SY := COMMA;  NEXTCH
	  end;
	'+':
	  begin
	    SY := PLUS;   NEXTCH
	  end;
	'-':
	  begin
	    SY := MINUS;  NEXTCH
	  end;
	others: SY := OTHERSY
      end;
    end %INSYMBOL\;
  procedure COMMAND(LEGAL:SETOFSYS);
    var
      I,J,K:INTEGER;
    begin
      if SY = IDENT then
	begin
	  I := 0;  {which command matches match}
	  for J := 1 to NUMPREDEC do
	    if PREDECTRAN[J] in LEGAL then
	      begin
		for K := 1 to 10 do
		  if PREDEC[J,K] <> ID[K] then
		    goto 1;
    1:
		if K > 10  {exact match} then
		  I := J
		else
		  if ID[K] = ' '   {abbreviation} then
		    if I = 0 then
		      I := J  {unique abbrev}
		    else
		      I := -1 {ambiguous abbrev}
	      end;
	  if I > 0  {unique abbrev} then
	    SY := PREDECTRAN[I]
	  else
	    if I < 0  {ambig} then
	      SY := AMBIG
	end
    end;

  function ACRPOINT(FINT:INTEGER;LLEFT:LEFTORRIGHT): ACR;
    %CONVERTS INTEGER TO ACR-POINTER\
    var
      ACR_INT: packed record
			case INTEGER of
			     0:(LINT: INTEGER);
			     1:(LACR,LACL: HALFWORD)
		      end;
    begin
      with ACR_INT do
	begin
	  LINT := FINT;
	  case LLEFT of
	    LEFT: LOAD_ADDRESS(LACL);
	    RIGHT: LOAD_ADDRESS(LACR);
	    FULL:  LOAD_ADDRESS(LINT)
	  end;
	  ACRPOINT := POINTERCV.FIX_ACR
	end
    end;

  procedure TESTGLOBALBASIS(SIDE:LEFTORRIGHT);
    begin
      %This routine sees whether we should use the global symbol
       table.  Two checks are needed.  If the currently open
       module is not the one where the break is, then none of
       its locals are accessible and only the global symbol table
       should be used.  If it is the right module, we need only
       see if the basis is at the bottom of the stack\
      if (E_DEB(ENTRY1.NEXTDEB) # CURENT) and (SIDE=RIGHT) then
	BASIS := NULLPTR
      else
	begin
	  LOAD_ADDRESS(ENTRY2.STACKBOTTOM);
	  if BASIS = POINTERCV.FIX_ACR then
	    BASIS := NULLPTR
	end
    end;

  function IDTREE: CTP;
    %POINTS TO THE IDTREE OF THE PROCEDURE, TO WHICH BASIS POINTS\
    var
      I: INTEGER;
      LACR: ACR;
    begin
      if BASIS = NULLPTR then
	IDTREE := E_CTP(ENTRY1.GLOBALIDTREE)
      else
	begin
	  LACR := ACRPOINT (BASIS^[0] + PREV_BASIS, FULL);
	  I := LACR^[0];
	  %I is now a "pushj p,proc".  However if proc is a parameter it is
	   "pushj p,0(1)".  We next check for that, and call MAGIC.  You don't
	   want to know how MAGIC works, but it returns the address of the
	   routine called by the pushj\
	  if (I mod 1000000B)=0 then
	    I := MAGIC(BASIS);
	  %Now backup over the "MOVE 16,-1(16)" or "HRR 16,-1(16)" to find the
	   pointer we are looking for\
	  repeat
	    I := I - 1; LACR := ACRPOINT (I, RIGHT);
	    LOAD_ADDRESS(LACR^[0])
	  until POINTERCV.FIX_ADRS.FIX_LADR # 777777B;
	  IDTREE := POINTERCV.FIX_CTP
	end
    end;

  procedure FIRSTBASIS(SIDE:LEFTORRIGHT);
    %GENERATES BASISPOINTER TO 'ACTIVATION RECORD' OF UNDERBREAKED PROCEDURE\
    begin
      BASIS := CALL_BASIS;  TESTGLOBALBASIS(SIDE)
    end;

  procedure SUCCBASIS(SIDE: LEFTORRIGHT);
    %GENERATES BASISPOINTER TO 'ACTIVATIONRECORD'
     OF STATIC/DYNAMIC HIGHER PROCEDURE)\
    %SIDE:  RIGHT FOR STATIC LINK
     LEFT FOR DYNAMIC LINK\
    begin
      if EXTENDED_ADDRESSING then
	if SIDE = RIGHT then
	  BASIS := ACRPOINT(BASIS^[0+PREV_BASIS], FULL)
	else
	  BASIS := ACRPOINT(BASIS^[0+DYNAMIC_SUP], FULL)
      else
	BASIS := ACRPOINT(BASIS^[0+PREV_BASIS], SIDE);
      TESTGLOBALBASIS(SIDE)
    end;

  procedure SEARCHSECTION(FCP: CTP; var FCP1: CTP);
	var i:integer; namext: nameptr;
    begin
      while FCP # nil do with FCP^ do
	begin
	  if NAMELEN < IDLEN then
	    FCP := E_CTP(RLINK)
	  else if NAMELEN > IDLEN then
	    FCP := E_CTP(LLINK)
	  else if NAME < ID then
	    FCP := E_CTP(RLINK)
	  else if NAME > ID then
	    FCP := E_CTP(LLINK)
	  else if NAMELEN <= 10 then
	    goto 1  {for short names, we now know they are equal}
	  else begin
	    NAMEXT := E_NAMEPTR(ORD(FCP) - (NAMELEN - 6) div 5);
	    for i := 1 to (NAMELEN - 6) div 5 do
	      if IDEXT.WORDS[I] <> NAMEXT^.WORDS[I] then
	        goto 2;
	    goto 1;  {all the same}
2:	    if NAMEXT^.WORDS[I] < IDEXT.WORDS[I] then
	      FCP := E_CTP(RLINK)
	    else FCP := E_CTP(LLINK);
	    end
	end;
    1:
      FCP1 := FCP
    end %SEARCHSECTION\;

  procedure WRITENAME(var F:text;FCP: CTP);
	var i:integer; namext: nameptr;
    begin
    with FCP^ do
      begin
      if NAMELEN <= 10 then
        write(F,NAME:NAMELEN)
      else begin
      write(F,NAME);
      NAMEXT := E_NAMEPTR(ORD(FCP) - (NAMELEN - 6) div 5);
      for i := 11 to NAMELEN do
	write(F,NAMEXT^.CHARS[I])
      end
     end
    end;

  procedure SEARCHID(var FCP: CTP);
    var
      LCP: CTP;
    begin
      FIRSTBASIS(RIGHT);
      loop
	SEARCHSECTION(IDTREE, LCP);
	if LCP # nil then
	  goto 1
      exit if BASIS = NULLPTR;
	SUCCBASIS (RIGHT%=STATIC\)
    end;
      SEARCHSECTION(E_CTP(ENTRY1.STANDARDIDTREE), LCP);
    1:
      FCP := LCP
    end;

  function CHARPTR(FSP: STP): BOOLEAN;
    begin
      CHARPTR := FALSE;
      LOAD_ADDRESS(ENTRY1.CHARPTR);
      if FSP # nil then
	with POINTERCV do
	  if FSP^.FORM = SUBRANGE then
	    CHARPTR := FSP^.RANGETYPE = ORD(FIX_STP)
	  else
	    CHARPTR := FSP = FIX_STP
    end;

  procedure GETBOUNDS(FSP: STP; var FMIN,FMAX: INTEGER);
    %GET INTERNAL BOUNDS OF SUBRANGE OR SCALAR TYPE\
    %ASSUME (FSP # NIL) AND (FSP^.FORM <= SUBRANGE) AND (FSP # INTPTR)
     AND  NOT COMPTYPES(REALPTR,FSP)\
    var
      FCONST_CTP: CTP;
    begin
      with FSP^ do
	if FORM = SUBRANGE then
	  begin
	    FMIN := MIN.IVAL; FMAX := MAX.IVAL
	  end
	else
	  begin
	    FMIN := 0;
	    if CHARPTR(FSP) then
	      FMAX := 177B
	    else
	      if FCONST # NIL_VALUE then
		begin
		  FCONST_CTP := E_CTP(FCONST);
		  FMAX := FCONST_CTP^.VALUES.IVAL
		end
	      else
		FMAX := 0
	  end
    end %GETBOUNDS\;

  procedure GETABOUNDS(FSP: STP; LBASIS: XADDRRANGE; var FMIN,FMAX: INTEGER);
	var LCP: CTP;
    begin
      with FSP^ do
        begin
        LCP := E_CTP(LOWER);
	if FORM = CONFORMANT then
	  if LCP^.EXTENDED then
	    begin
	      POINTERCV.ADDR := LCP^.XADDR + LBASIS;
	      FMIN := POINTERCV.FIX_GENPT^;
	      LCP := E_CTP(UPPER);
	      POINTERCV.ADDR := LCP^.XADDR + LBASIS;
	      FMAX := POINTERCV.FIX_GENPT^;
	    end
	  else
	    begin
	      POINTERCV.ADDR := LCP^.VADDR + LBASIS;
	      FMIN := POINTERCV.FIX_GENPT^;
	      LCP := E_CTP(UPPER);
	      POINTERCV.ADDR := LCP^.VADDR + LBASIS;
	      FMAX := POINTERCV.FIX_GENPT^;
	    end
        else GETBOUNDS(FSP,FMIN,FMAX);	
	end;
    end;

  function GETSIZE(FSP: STP; LBASIS: XADDRRANGE): XADDRRANGE;
	var LCP: CTP; LSP: STP; MIN,MAX:XADDRRANGE;
    begin
      if FSP^.FORM = ARRAYS then
        begin
	  LSP := E_STP(FSP^.INXTYPE);
	  if LSP^.FORM = CONFORMANT then
	    begin
	      GETABOUNDS(LSP,LBASIS,MIN,MAX);
	      LSP := E_STP(FSP^.AELTYPE);
	      MAX := MAX - MIN + 1; {MAX - number of entries}
	      if FSP^.ARRAYPF then
		begin
		  MIN := BITMAX DIV LSP^.BITSIZE; {MIN - bytes per word}
		  GETSIZE := (MAX + MIN - 1) DIV MIN
		end
	      else GETSIZE := MAX * GETSIZE(LSP,LBASIS)
	    end
	  else GETSIZE := FSP^.SIZE
	end
      else GETSIZE := FSP^.SIZE;
    end;

  function COMPTYPES(FSP1,FSP2: STP; FBASIS:XADDRRANGE) : BOOLEAN;
    %DECIDE WHETHER STRUCTURES POINTED AT BY FSP1 AND FSP2 ARE COMPATIBLE\
    var
      NXT1,NXT2: CTP;
      COMP: BOOLEAN;
      LMIN,LMAX,I: INTEGER;
    begin
      if FSP1 = FSP2 then
	COMPTYPES := TRUE
      else
	if (FSP1 # nil) and (FSP2 # nil) then
	  if FSP1^.FORM = FSP2^.FORM then
	    case FSP1^.FORM of
	      SCALAR: COMPTYPES := FALSE;
		% IDENTICAL SCALARS DECLARED ON DIFFERENT LEVELS ARE
		 NOT RECOGNIZED TO BE COMPATIBLE\
	      SUBRANGE: COMPTYPES := COMPTYPES(E_STP(FSP1^.RANGETYPE),
					       E_STP(FSP2^.RANGETYPE),
					       FBASIS);
	      CONFORMANT: COMPTYPES := COMPTYPES(E_STP(FSP1^.BOUNDTYPE),
						 E_STP(FSP2^.BOUNDTYPE),
						 FBASIS);
	      POINTER:        COMPTYPES := COMPTYPES(E_STP(FSP1^.ELTYPE),
						     E_STP(FSP2^.ELTYPE),
						     FBASIS);
(* 28 - 4-word sets *)
	      QPOWER,
	      POWER:  COMPTYPES := COMPTYPES(E_STP(FSP1^.ELSET),
					     E_STP(FSP2^.ELSET),
					     FBASIS);
	      ARRAYS:
	        begin
	          GETABOUNDS (E_STP(FSP1^.INXTYPE),FBASIS,LMIN,LMAX);
		  I := LMAX-LMIN;
		  GETABOUNDS (E_STP(FSP2^.INXTYPE),FBASIS,LMIN,LMAX);
		  COMPTYPES := COMPTYPES(E_STP(FSP1^.AELTYPE),
					 E_STP(FSP2^.AELTYPE),FBASIS) and
			       (FSP1^.ARRAYPF = FSP2^.ARRAYPF)
			        and (I = LMAX - LMIN)
	   	end;
		%ALTERNATIVES: -- ADD A THIRD BOOLEAN TERM: INDEXTYPE MUST
		 BE COMPATIBLE. MAY GIVE TROUBLE FOR ENT OF STRINGCONSTANTS
		 -- ADD A FOURTH BOOLEAN TERM: LOWBOUNDS MUST
		 BE THE SAME\
	      RECORDS:
		begin
		  NXT1 := E_CTP(FSP1^.FSTFLD);
		  NXT2 := E_CTP(FSP2^.FSTFLD); COMP := TRUE;
		  while (NXT1 # nil) and (NXT2 # nil) do
		    begin
		      COMP := COMPTYPES(E_STP(NXT1^.IDTYPE),
					E_STP(NXT2^.IDTYPE),FBASIS) and COMP;
		      NXT1 := E_CTP(NXT1^.NEXT);
		      NXT2 := E_CTP(NXT2^.NEXT)
		    end;
		  COMPTYPES := (COMP and (NXT1 = nil) and (NXT2 = nil)
				and (FSP1^.RECVAR = NIL_VALUE)
				and (FSP2^.RECVAR = NIL_VALUE))
		end;
		%IDENTICAL RECORDS ARE RECOGNIZED TO BE COMPATIBLE
		 IF NO VARIANTS OCCUR\
	      FILES:  COMPTYPES := COMPTYPES(E_STP(FSP1^.FILTYPE),
					     E_STP(FSP2^.FILTYPE),FBASIS)
	      end %CASE\
	  else
	    %FSP1^.FORM # FSP2^.FORM\
	    if FSP1^.FORM = SUBRANGE then
	      COMPTYPES := COMPTYPES(E_STP(FSP1^.RANGETYPE),FSP2,FBASIS)
	    else if FSP2^.FORM = SUBRANGE then
		COMPTYPES := COMPTYPES(FSP1,E_STP(FSP2^.RANGETYPE),FBASIS)
	    else if FSP1^.FORM = CONFORMANT then
		COMPTYPES := COMPTYPES(E_STP(FSP1^.BOUNDTYPE),FSP2,FBASIS)
	    else if FSP2^.FORM = CONFORMANT then
		COMPTYPES := COMPTYPES(FSP1,E_STP(FSP2^.BOUNDTYPE),FBASIS)
	      else
		begin
		COMPTYPES := FALSE;
		if FSP2^.FORM = QPOWER then
		  if FSP2^.ELSET = nil_value then
		    if FSP1^.FORM = POWER then
		      begin FSP2^.FORM := POWER; COMPTYPES := TRUE;
			    FSP2^.SIZE := 2 end;
		if FSP1^.FORM = QPOWER then
		  if FSP1^.ELSET = nil_value then
		    if FSP2^.FORM = POWER then
		      begin FSP1^.FORM := POWER; COMPTYPES := TRUE;
			    FSP1^.SIZE := 2 end;
		end
	else
	  COMPTYPES := TRUE
    end %COMPTYPES\;
  function NEXTBYTE(FBITSIZE: INTEGER): INTEGER;
    var
      LVAL,J: INTEGER;
      BYTE_INT: record
		  case BOOLEAN of
		       FALSE: (BITS: packed array [1..36] of BIT);
		       TRUE : (INTCONST: INTEGER)
		end;
    begin
      with GATTR do
	begin
	  LVAL := 0;
	  if PACKFG then
	    begin
	      if FBITSIZE + GBITCOUNT  >  BITMAX then
		begin
		  GADDR := GADDR + 1;  GBITCOUNT := 0
		end;
	      with BYTE_INT do
		begin
		  INTCONST := BASIS^[GADDR];
		  for J := GBITCOUNT + 1  to GBITCOUNT + FBITSIZE do
		    LVAL := LVAL*2 + BITS[J]
		end;
	      GBITCOUNT := GBITCOUNT + FBITSIZE;
	    end
	  else
	    begin
	      LVAL := BASIS^[GADDR];  GADDR := GADDR + 1
	    end;
	end %WITH GATR\;
      NEXTBYTE := LVAL;
    end %NEXTBYTE\;

  procedure PUTNEXTBYTE(FBITSIZE, FVAL: INTEGER);
    var
      J: INTEGER;
      INT_BYTE: record
		  case BOOLEAN of
		       FALSE: (BITS: packed array [1:36] of BIT);
		       TRUE:  (INTCONST: INTEGER)
		end;
    begin
      with GATTR, INT_BYTE do
	begin
	  if FBITSIZE + GBITCOUNT > BITMAX then
	    begin
	      INTCONST := BASIS^[GADDR+1];  GBITCOUNT := 0
	    end
	  else
	    INTCONST := BASIS^[GADDR];
	  for J := (GBITCOUNT + FBITSIZE) downto (GBITCOUNT + 1) do
	    begin
	      BITS[J] := FVAL mod 2;  FVAL := FVAL div 2
	    end;
	  BASIS^[GADDR] := INTCONST
	end
    end;

  procedure GETFIELD(FCP:CTP);
    var
      BYTEPTRCHANGE:  packed record
			       case BOOLEAN of
				    FALSE: (BYTEPTRCONST: INTEGER;
					    BYTEPTRWORD2: INTEGER);
				    TRUE:  (SBITS,PBITS: 0..BITMAX;
					    XBIT: BIT;
					    IBIT: BIT;
					    IREG: ACRANGE;
					    RELADDR: LOCAL_ADDRESS;
					    XXBIT: BIT;
					    XIBIT: BIT;
					    XIREG: ACRANGE;
					    XRELADDR: XADDRRANGE);
			     end;
    begin
      with FCP^, GATTR do
	begin
	  if KLASS # FIELD then
	    WRITELN(TTY,'!Error in getfield');
	  case PACKF of
	    NOTPACK,
	    HWORDL: begin
	      GADDR := GADDR + FLDADDR; GBITCOUNT := 0
	    end;
	    HWORDR: begin
	      GADDR := GADDR + FLDADDR; GBITCOUNT := 18
	    end;
	    PACKK: with BYTEPTRCHANGE do begin
	      BYTEPTRCONST := BASIS^[FLDADDR];
	      if XBIT <> 0 then
	        begin
		  BYTEPTRWORD2 := BASIS^[FLDADDR+1];
		  if (XIREG <> BIXREG) or (XIBIT = 1) then
		    WRITELN(TTY,'!Error in getfield(illegal extended bytepointer)');
	          GADDR := GADDR + XRELADDR {extended form}
		end
	      else
		begin
	        if (IREG <> BIXREG) or (IBIT = 1) then
		  WRITELN(TTY,'!Error in getfield(illegal bytepointer)');
	        GADDR := GADDR + RELADDR {normal form}
		end;
	      GBITCOUNT := BITMAX - SBITS -PBITS;
	    end
	    end %CASE\;
	  PACKFG := PACKF # NOTPACK;
	  TYPTR := E_STP(IDTYPE)
	end %WITH\
    end %GETFIELD\;
  procedure WRITESCALAR(var OUTFILE:TEXT;FVAL:INTEGER; FSP: STP);
    var
      LCP: CTP; LENG: INTEGER;
      LVALU: VALU;
    begin
      if FSP # nil then
	with FSP^ do
	  case FORM of
	    SCALAR:
	      begin
		if SCALKIND = STANDARD then
		  if FSP = E_STP(ENTRY1.INTPTR) then
		    begin
		      (* Hex and Octal printout *)
		      if PRINTRADIX = HEX then
			begin
			  LENG := HEXLEN(FVAL);
			  WRITE(OUTFILE,'"',FVAL:LENG:H);
			  LENG := LENG + 1
			end
		      else
			if PRINTRADIX = OCTAL then
			  begin
			    LENG := OCTLEN(FVAL);
			    WRITE (OUTFILE, FVAL:LENG:O,'B');
			    LENG := LENG + 1
			  end
			else
			  begin
			    LENG := LENGTH(FVAL); WRITE(OUTFILE, FVAL:LENG)
			  end
		    end
		  else
		    if FSP = E_STP(ENTRY1.REALPTR) then
		      with LVALU do
			begin
			  IVAL := FVAL; WRITE(OUTFILE, RVAL); LENG := 16
			end
		    else
		      %==>CHARPTR\
		      if FVAL < 40B then
			begin
			  WRITE(OUTFILE,'''^',CHR(FVAL+100B),''''); LENG := 4
			end
		      else
			begin
			  WRITE(OUTFILE,'''', CHR(FVAL),'''');  LENG := 3
			end
		else
		  %SCALKIND==>DECLARED\
		  begin
		    LCP := E_CTP(FCONST);
		    if FVAL >= 0 then
		      while LCP^.VALUES.IVAL > FVAL do LCP := E_CTP(LCP^.NEXT);
		    with LCP^ do
		      if VALUES.IVAL # FVAL then
			begin
			  LENG := LENGTH(FVAL);
			  WRITE(OUTFILE,FVAL:LENG, '(Out of range)');
			  LENG := LENG + 14
			end
		      else
			WRITENAME(OUTFILE,LCP)
		  end
	      end;
	    SUBRANGE:
	      begin
		WRITESCALAR(OUTFILE,FVAL,E_STP(RANGETYPE)); LENG := 0
	      end;
	    CONFORMANT:
	      begin
		WRITESCALAR(OUTFILE,FVAL,E_STP(BOUNDTYPE)); LENG := 0
	      end;
	    POINTER:
	      if FVAL = ORD(nil) then
		begin
		  WRITE(OUTFILE,'NIL'); LENG := 3
		end
	      else
		if EXTENDED_ADDRESSING then
		  begin
		    SECTION := FVAL div 1000000B;
		    LENG := OCTLEN(SECTION);
		    WRITELN(OUTFILE,SECTION:LENG:O, ',,', FVAL:6:O, 'B');
		    LENG := LENG + 9
		  end
		else
		  begin
		    WRITE(OUTFILE,FVAL:6:O,'B'); LENG:=7
		  end;
	    others: WRITE(TTY,'!Err in writescalar')
	    end %CASE\;
      CHCNT := CHCNT + LENG
    end;

  procedure WRITESTRUCTURE(var OUTFILE:TEXT; FSP: STP);
    type threevalue = 1..3;
    var
      INX : INTEGER;
(* 28 - 4-word sets *)
      LLSP: STP;
      CINX, LINX: CHAR;
      LMIN, LMAX, LENG, LSPACE: INTEGER;
      LADDR: GLOBAL_ADDRESS;
      NOCOMMA, ELLIPSIS: BOOLEAN;
      SETWANDEL: record
		   case THREEVALUE of
			1: (CONST1: INTEGER; CONST2: INTEGER;
			    CONST3: INTEGER; CONST4: INTEGER);
			2:  (MASK: set of 0..71);
			3:  (CMASK: set of CHAR)
		 end;
      OATTR: ATTR;
      LASTEQ: BOOLEAN;
      NEXTEQ: BOOLEAN;
      CURRCOMPO: INTEGER;
      AELTYPE_STP: STP;

    procedure WRITEFIELDLIST(var OUTFILE:TEXT;
			     FPACK: BOOLEAN;
			     FNEXTFLD: CTP;
			     FRECVAR: STP);
      var
	LSP: STP; J: INTEGER;
	LATTR : ATTR;
	VARFLAG: BOOLEAN;
	TAGFIELDP_CTP: CTP;
      begin
	LATTR := GATTR;
	while FNEXTFLD # nil do with FNEXTFLD^ do
	  begin
	    NEWLINE(OUTFILE);  WRITENAME(OUTFILE,FNEXTFLD); 
	    WRITE(OUTFILE,': ');
	    CHCNT := CHCNT + 2 + NAMELEN;  NL := TRUE;
	    GETFIELD(FNEXTFLD);
	    WRITESTRUCTURE(OUTFILE,E_STP(IDTYPE));
	    GATTR := LATTR;
	    FNEXTFLD := E_CTP(FNEXTFLD^.NEXT)
	  end;
	if FRECVAR # nil then
	  if FRECVAR^.FORM = TAGFWITHID then
	    begin
	      TAGFIELDP_CTP := E_CTP(FRECVAR^.TAGFIELDP);
	      with TAGFIELDP_CTP^ do
		begin
		  NEWLINE(OUTFILE); WRITENAME(OUTFILE,TAGFIELDP_CTP);
		  WRITE(OUTFILE,': '); CHCNT := CHCNT + NAMELEN + 2;
		  GETFIELD(TAGFIELDP_CTP);
		  if FPACK then
		    with POINTERCV do
		      begin
			LOAD_ADDRESS(IDTYPE);
			J := NEXTBYTE(FIX_STP^.BITSIZE)
		      end
		  else
		    J := BASIS^[GATTR.GADDR];
		  WRITESCALAR(OUTFILE, J, E_STP(IDTYPE));
		  GATTR:=LATTR
		end;
	      LSP := E_STP(FRECVAR^.FSTVAR);
	      loop
		VARFLAG := LSP # nil;
		if not VARFLAG then
		  begin
		    WRITE(TTY,'No fields for this variant'); goto 1
		  end
	      exit if LSP^.VARVAL.IVAL = J;
		LSP := E_STP(LSP^.NXTVAR)
	    end %LOOP\;
	      with LSP^ do
		begin
		  if FORM # VARIANT then
		    begin
		      WRITE(TTY,'Err in wrfldlst'); goto 1
		    end;
		  GATTR := LATTR;
		  WRITEFIELDLIST(OUTFILE,FPACK,E_CTP(FIRSTFIELD),E_STP(SUBVAR))
		end;
      1:
	    end
      end;

    begin %WRITESTRUCTURE\
      if FSP # nil then
	with FSP^, GATTR do
	  case FORM of
	    SCALAR,
	    SUBRANGE,
	    POINTER:
	      begin
		I := NEXTBYTE(FSP^.BITSIZE);  WRITESCALAR(OUTFILE,I,FSP)
	      end;
	    POWER:
	      begin
		NOCOMMA := TRUE; WRITE(OUTFILE, '['); LENG := 1;
		with SETWANDEL do
		  begin
		    CONST1 := BASIS^[GADDR]; CONST2 := BASIS^[GADDR+1];
		    GADDR := GADDR + 2;
		    for INX := 0 to 71 do
		      if INX in MASK then
			begin
			  if NOCOMMA then
			    NOCOMMA := FALSE
			  else
			    WRITE(OUTFILE,',');
			  LENG := LENG + 1;
			  I := INX;
			  WRITESCALAR(OUTFILE,I,E_STP(ELSET))
			end
		  end %WITH SETWANDEL\;
		WRITE(OUTFILE,']'); CHCNT := CHCNT + LENG
	      end %POWER\;
(* 28 - 4-word sets *)
	    QPOWER:
	      begin
		NOCOMMA := TRUE; WRITE(OUTFILE, '['); LENG := 1;
		LINX := CHR(200B); ELLIPSIS := FALSE;
		with SETWANDEL do
		  begin
		    CONST1 := BASIS^[GADDR]; CONST2 := BASIS^[GADDR+1];
		    CONST3 := BASIS^[GADDR+2]; CONST4 := BASIS^[GADDR+3];
		    GADDR := GADDR + 4;
		    for CINX := CHR(0) to CHR(177B) do
		      if CINX in CMASK then
			if ELLIPSIS then
			  if CINX <> SUCC(LINX) then {end of ellipsis}
			    begin
			      WRITE(OUTFILE,'..');
			      LENG := LENG + 2;
			      WRITESCALAR(OUTFILE,ORD(LINX),E_STP(ELSET));
			      if NOCOMMA then
			        NOCOMMA := FALSE
			      else
			        WRITE(OUTFILE,',');
			      LENG := LENG + 1;
			      WRITESCALAR(OUTFILE,ORD(CINX),E_STP(ELSET));
			      LINX := CINX
			    end
			  else
			    LINX := CINX {continue ellipsis}
			else
			  if CINX = SUCC(LINX) then {start ellipsis}
			    begin
			      ELLIPSIS := TRUE;
			      LINX := CINX
			    end
			  else
			    begin
			      if NOCOMMA then
			        NOCOMMA := FALSE
			      else
			        WRITE(OUTFILE,',');
			      LENG := LENG + 1;
			      WRITESCALAR(OUTFILE,ORD(CINX),E_STP(ELSET));
			      LINX := CINX
			    end;
		    if ELLIPSIS then
		      begin
			WRITE(OUTFILE,'..');
			LENG := LENG + 2;
			WRITESCALAR(OUTFILE,ORD(LINX),E_STP(ELSET))
		      end
		  end %WITH SETWANDEL\;
		WRITE(OUTFILE,']'); CHCNT := CHCNT + LENG
	      end %QPOWER\;
	    ARRAYS:
	      begin
		if E_STP(INXTYPE) <> NIL then
		  GETABOUNDS(E_STP(INXTYPE), GBASIS, LMIN, LMAX);
		GBITCOUNT := 0;
		if CHARPTR(E_STP(AELTYPE)) and ARRAYPF then
		  begin %STRING\
		    LENG := LMAX - LMIN + 1;
		    POINTERCV.ADDR := GADDR;
		    WRITE (OUTFILE, '''');
		    for INX := 1 to LENG do
		      if ORD(POINTERCV.STRINGPTR^[INX]) < 40B then
			begin
			  WRITE(OUTFILE,'^',CHR(ORD(POINTERCV.STRINGPTR^[INX])+100B));
			  CHCNT := CHCNT+1
			end
		      else
			WRITE (OUTFILE,POINTERCV.STRINGPTR^[INX]);
		    WRITE (OUTFILE, '''');
		    GADDR  :=  GADDR  +  (LENG-1) div 5;
		    CHCNT := CHCNT + LENG + 2
		  end %STRING\
		else
		  begin
		    PACKFG := ARRAYPF;
		    LASTEQ := FALSE;
		    for INX := LMIN to LMAX do
		      begin
			AELTYPE_STP := E_STP(AELTYPE);
			if INX = LMAX then
			  NEXTEQ := FALSE
			else
			  if AELTYPE_STP^.FORM <= POINTER then
			    begin
			      OATTR := GATTR;
			      CURRCOMPO := NEXTBYTE(AELTYPE_STP^.BITSIZE);
			      NEXTEQ := CURRCOMPO=NEXTBYTE(AELTYPE_STP^.BITSIZE);
			      GATTR := OATTR
			    end
			  else
			    begin
			      NEXTEQ := TRUE;  I:=0;
			      loop
				AELTYPE_STP := E_STP(AELTYPE);
				NEXTEQ:=(BASIS^[GADDR+I] = BASIS^[GADDR+
				    GETSIZE(AELTYPE_STP,GBASIS)+I]);
			      exit if not NEXTEQ or
				(I = GETSIZE(AELTYPE_STP,GBASIS)-1);
				I := I + 1
			    end;
			    end (* FORM>POINTER *);
			if not(LASTEQ and NEXTEQ) then
			  begin
			    if NL then
			      NEWLINE(OUTFILE)
			    else
			      NL := TRUE;
			    WRITE(OUTFILE,'[');
			    WRITESCALAR(OUTFILE,INX,E_STP(INXTYPE));
			    WRITE(OUTFILE,']'); CHCNT:=CHCNT+2
			  end;
			if not NEXTEQ then
			  begin
			    WRITE(OUTFILE,'=');  CHCNT := CHCNT+1;
			    LEFTSPACE := LEFTSPACE + 3;
			    NL := TRUE;
			    WRITESTRUCTURE(OUTFILE,E_STP(AELTYPE));
			    LEFTSPACE := LEFTSPACE - 3;
			  end
			else
			  begin
			    if not LASTEQ then
			      begin
				WRITE(OUTFILE,'..');  CHCNT := CHCNT+2;  NL:=FALSE
			      end;
			    AELTYPE_STP := E_STP(AELTYPE);
			    if AELTYPE_STP^.FORM <= POINTER then
			      CURRCOMPO:=NEXTBYTE(AELTYPE_STP^.BITSIZE)
			    else
			      GADDR:=GADDR+GETSIZE(AELTYPE_STP,GBASIS);
			  end (* NEXTEQ *);
			LASTEQ := NEXTEQ;
		      end (* FOR *);
		  end (* NOT STRING *);
		if ARRAYPF then
		  begin
		    GADDR := GADDR + 1; GBITCOUNT := 0
		  end
	      end %ARRAYS\;
	    RECORDS:
	      begin
		WRITE(OUTFILE,'RECORD');
		LSPACE := LEFTSPACE; LEFTSPACE := CHCNT + 1;
		LADDR := GADDR;
		WRITEFIELDLIST(OUTFILE,RECORDPF,E_CTP(FSTFLD),E_STP(RECVAR));
		GADDR := LADDR + SIZE; GBITCOUNT := 0;
		LEFTSPACE := LEFTSPACE - 1; NEWLINE(OUTFILE);
		WRITE(OUTFILE,'END');
		LEFTSPACE := LSPACE
	      end;
	    FILES:   WRITE(OUTFILE,'!File')
	    end %CASE FORM\
    end %WRITESTRUCTURE\;
  procedure SIMPLEFACTOR;
    forward;

  procedure SELECTOR;
    var
      LCP: CTP;
      LMIN, LMAX: INTEGER;
      LATTR: ATTR;
      INDEX, I, INDEXOFFSET, BYTESINWORD: INTEGER;
      AELTYPE_STP: STP;
    begin
      while SY in [LBRACK,ARROW,PERIOD] do  with GATTR do
	case SY of
	  LBRACK:
	    begin
	      repeat
		if TYPTR # nil then
		  if TYPTR^.FORM # ARRAYS then
		    begin
		      ERROR; WRITELN(TTY,'Type of variable is not array')
		    end;
		INSYMBOL;
		if not (SY in [IDENT, INTCONST, PLUS, MINUS, CHARCONST]) then
		  begin
		    ERROR; WRITELN(TTY,'Illegal symbol')
		  end;
		if TYPTR # nil then
		  begin
		    LATTR := GATTR; SIMPLEFACTOR;
		    if COMPTYPES(GATTR.TYPTR, E_STP(LATTR.TYPTR^.INXTYPE),
					      LATTR.GBASIS) then
		      with GATTR do
			begin
			  if KIND = CST then
			    INDEX := CVAL.IVAL
			  else
			    if PACKFG then
			      INDEX := NEXTBYTE(TYPTR^.BITSIZE)
			    else
			      INDEX := BASIS^[GADDR];
			  GATTR := LATTR
			end
		    else
		      begin
			ERROR; WRITELN(TTY,'Index-type is not compatible with declaration')
		      end
		  end %TYPTR # NIL\;
		if TYPTR # nil then
		  with TYPTR^ do
		    begin
		      GETABOUNDS(E_STP(INXTYPE), GATTR.GBASIS, LMIN, LMAX);
		      INDEXOFFSET := INDEX - LMIN;
		      if INDEXOFFSET < 0 then
			I := - INDEXOFFSET
		      else
			if INDEX > LMAX then
			  I:= INDEX - LMAX
			else
			  goto 1;
		      ERROR; WRITE(TTY,'array-index by ', I:LENGTH(I),' ');
		      if INDEXOFFSET < 0 then
			WRITELN(TTY, 'less than low bound')
		      else
			WRITELN(TTY,'greater than high bound');
    1:
		      AELTYPE_STP := E_STP(AELTYPE);
		      if ARRAYPF then
			begin
			  PACKFG := TRUE;
			  BYTESINWORD := BITMAX div AELTYPE_STP^.BITSIZE;
			  I := INDEXOFFSET mod BYTESINWORD;
			  GADDR := GADDR + (INDEXOFFSET div BYTESINWORD);
			  if INDEXOFFSET < 0 then
			    begin
			      GADDR := GADDR-1;
			      GBITCOUNT := (BYTESINWORD + I) * AELTYPE_STP^.BITSIZE
			    end
			  else
			    GBITCOUNT := I * AELTYPE_STP^.BITSIZE
			end
		      else
			GADDR := GADDR + (GETSIZE(AELTYPE_STP,GBASIS)
					  * INDEXOFFSET);
		      TYPTR := AELTYPE_STP
		    end %TYPTR # NIL\
	      until SY # COMMA;
	      if SY = RBRACK then
		INSYMBOL
	      else
		begin
		  ERROR; WRITELN(TTY,'"]" expected')
		end
	    end;
	  PERIOD:
	    begin
	      if TYPTR # nil then
		if TYPTR^.FORM # RECORDS then
		  begin
		    ERROR; WRITELN(TTY,'Type of variable is not record')
		  end;
	      INSYMBOL;
	      if SY = IDENT then
		begin
		  if TYPTR # nil then
		    begin
		      SEARCHSECTION(E_CTP(TYPTR^.FSTFLD), LCP);
		      if LCP = nil then
			begin
			  ERROR; WRITELN(TTY,'No such field in this record')
			end
		      else
			GETFIELD(LCP)
		    end %TYPTR # NIL\;
		  INSYMBOL
		end
	      else
		begin
		  ERROR; WRITELN(TTY,'Identifier expected')
		end
	    end %PERIOD\;
	  ARROW:
	    begin
	      INSYMBOL;
	      if TYPTR # nil then
		case TYPTR^.FORM of
		  POINTER:
		    begin
		      if PACKFG then
			GADDR := NEXTBYTE(18)
		      else
			GADDR := BASIS^[GADDR];
		      if GADDR = ORD(nil) then
			begin
			  ERROR; WRITELN(TTY,'Pointer is NIL')
			end
		      else
			if GADDR = 0 then
			  begin
			    ERROR; WRITELN(TTY,'Uninitialized pointer')
			  end
			else
			  TYPTR := E_STP(TYPTR^.ELTYPE)
		    end;
		  FILES:
		    begin
		      GADDR := BASIS^[GADDR];  TYPTR := E_STP(TYPTR^.FILTYPE)
		    end;
		  others:
		    begin
		      ERROR;
		      WRITELN(TTY,'Type of variable must be file or pointer')
		    end
		  end %CASE FORM\;
	      PACKFG := FALSE; GBITCOUNT := 0
	    end %ARROW\
	  end %CASE\
    end %SELECTOR\;

  procedure VARIABLE;
    var
      LCP: CTP;

    begin   %VARIABLE\
      SEARCHID(LCP);
      if LCP = nil
      then
	begin
	  ERROR; WRITELN(TTY,'not found')
	end
      else
	begin
	  with LCP^, GATTR  do
	    case KLASS of
	      TYPES,PARAMS:
		begin
		  ERROR; WRITELN(TTY,'!type')
		end;
	      KONST:
		begin
		  KIND := CST; CVAL := VALUES; TYPTR := E_STP(IDTYPE)
		end;
	      VARS:
		begin
		  KIND := VARBL;  
		  if EXTENDED then
		    GADDR := XADDR + ORD(BASIS)
		  else GADDR := VADDR + ORD(BASIS);
		  {write(tty,'variable, vaddr= ',vaddr:6:o, ', basis= ',
		  ord(basis):6:o, ', Gaddr= ',gaddr:6:o);}
		  GBASIS := ORD(BASIS);
		  BASIS := NULLPTR;  GBITCOUNT := 0;
		  if VKIND = FORMAL then
		    GADDR := BASIS^[GADDR];
		  TYPTR := E_STP(IDTYPE); PACKFG := FALSE;  SELECTOR
		end;
	      FIELD: %WRITE(TTY,'Not implemented; Try <record>.<field> ...')\;
	      PROC:
		begin
		  ERROR; WRITELN(TTY,'!Procedure')
		end;
	      FUNC:
		begin
		  ERROR; WRITELN(TTY,'!Function')
		end
	      end %CASE CLASS\
	end
    end %VARIABLE\;

  procedure SIMPLEFACTOR;  (* UNSIGNED AND SIGNED CONSTANTS AND VARIABLES *)
    var
      SIGNED : BOOLEAN;
    begin
      if SY = IDENT then
	begin
	  INSYMBOL; VARIABLE
	end
      else
	with GATTR do
	  begin
	    KIND := CST; CVAL := VAL;
	    case SY of
	      INTCONST:
		begin
		  TYPTR := E_STP(ENTRY1.INTPTR); INSYMBOL
		end;
	      REALCONST:
		begin
		  TYPTR := E_STP(ENTRY1.REALPTR); INSYMBOL
		end;
	      CHARCONST:
		begin
		  TYPTR := E_STP(ENTRY1.CHARPTR); INSYMBOL
		end;
	      STRINGCONST:
		begin
		  TYPTR := STRINGPTR; INSYMBOL
		end;
	      PLUS,
	      MINUS:
		begin
		  SIGNED := SY=MINUS;
		  INSYMBOL; SIMPLEFACTOR;
		  if not (COMPTYPES(TYPTR,E_STP(ENTRY1.INTPTR),0)
			  or COMPTYPES(TYPTR,E_STP(ENTRY1.REALPTR),0)) then
		    begin
		      ERROR; WRITELN(TTY,'No sign allowed here')
		    end
		  else
		    if SIGNED then
		      if KIND=CST then
			CVAL.IVAL := -CVAL.IVAL
		      else
			begin
			  ERROR; WRITELN(TTY,'Signed variables not implemented')
			end
		end %MINUS\
	      end %CASE\
	  end %WITH GATTR\
    end  (* SIMPLEFACTOR *);

  procedure ASSIGNMENT;
    var
      LATTR: ATTR;
      BYTE:  INTEGER;
      CBASIS: ACR;
    begin
      if GATTR.KIND # VARBL then
	begin
	  ERROR; WRITELN(TTY,'Assignment allowed to variables only')
	end
      else
	begin
	  LATTR := GATTR;  SIMPLEFACTOR;
	  if ENDOK then
	    if COMPTYPES(GATTR.TYPTR, LATTR.TYPTR, LATTR.GBASIS) then
	      begin
		if LATTR.PACKFG then
		  begin
		    with GATTR do
		      if KIND = CST then
			BYTE := CVAL.IVAL
		      else
			if PACKFG then
			  BYTE := NEXTBYTE(TYPTR^.BITSIZE)
			else
			  BYTE := BASIS^[GADDR];
		    GATTR := LATTR;
		    PUTNEXTBYTE(GATTR.TYPTR^.BITSIZE, BYTE)
		  end (* IF PACKFG *)
		else
		  if GATTR.KIND = CST then
		    if GATTR.TYPTR^.FORM = ARRAYS  {must be string} then
		      begin
			CBASIS := ACRPOINT(ORD(GATTR.CVAL.VALP)+4,RIGHT);
			for I := 0 to (4+GATTR.CVAL.VALP^.SLGTH)div 5 - 1 do
			  BASIS^[LATTR.GADDR+I] := CBASIS^[I]
		      end
		    else
		      BASIS^[LATTR.GADDR] := GATTR.CVAL.IVAL
		  else
		    if GATTR.PACKFG then
		      BASIS^[LATTR.GADDR] := NEXTBYTE(GATTR.TYPTR^.BITSIZE)
		    else
		      for I := 0 to GETSIZE(LATTR.TYPTR,GATTR.GBASIS) - 1  do
			BASIS^[LATTR.GADDR + I] := BASIS^[GATTR.GADDR + I]
	      end (* IF COMPTYPES *)
	    else
	      begin
		ERROR; WRITELN(TTY, 'Type-conflict in assignment')
	      end
	end (* KIND=VARIABLE *)
    end (* ASSIGNMENT *);
  function STOPSEARCH(FLINE:GLOBAL_ADDRESS;MODULE:DEBPTR):INTEGER;
    begin
      for I := 1 to STOPMAX do with STOPTABLE[I] do
	if (PAGE=GPAGE) and (THISLINE=FLINE)
	  and (E_DEB(MODENTRY.NEXTDEB)=MODULE) then
	  begin
	    STOPSEARCH := I;  goto 1 %EXIT\
	  end;
      STOPSEARCH := 0; %NOT FOUND\
    1:
    end;

  function PAGEVALUE(FPAGER: PAGEELEM): INTEGER;
    begin
      with FPAGER do  PAGEVALUE := AC*16 + INXREG
    end;

  function LINEVALUE (var FLINER: LINEELEM;
		      FLINE: INTEGER;
		      MODULE:DEBPTR) : INTEGER;
    begin
      while FLINER.CODE = OP_PUSHJ do
	begin
	  I := STOPSEARCH(FLINE , MODULE);
	  if I = 0 then
	    begin
	      WRITELN(TTY,'> Stop table destroyed');  LINEVALUE := -1; goto 1
	    end;
	  FLINER.CONSTANT1 := STOPTABLE[I] . ORIGINALCONT . CONSTANT1
	end %PUSHJ\;
      with FLINER do
	if CODE = NO_OP_1 %one-word number entry\ then
	  LINEVALUE := FLINE - (AC + 16*INXR)
	else
	  begin
	    if CODE # NO_OP_2 %two-word number entry\ then
	      begin
		WRITELN(TTY,'> Internal confusion: bad instruction in line-chain. Lastline=',FLINE:5);
		LINEVALUE := -1; goto 1
	      end;
	    if ABSLINE = 777777B then
	      LINEVALUE := -1
	    else
	      LINEVALUE := ABSLINE
	  end;
    1:
    end %LINEVALUE\;

  function STRLEN(S:ALFA):INTEGER;
    var
      I:INTEGER;
    begin
      I := 0;
      if S[10] # ' ' then
	I := 10
      else
	while S[I+1] # ' ' do I := I + 1;
      STRLEN := I
    end;

  function GETLINPAG(var LINENR,GPAGE,DEFPAGE:INTEGER;FOLLOW:SETOFSYS):
    BOOLEAN;
    %READS LINENUMBER AND PAGENUMBER\
    begin
      GETLINPAG := FALSE;
      if SY = STAR then
	begin
	  INSYMBOL;
	  if SY in FOLLOW then
	    begin
	      LINENR := DOTLINE; GPAGE := DOTPAGE; GETLINPAG := TRUE
	    end
	  else
	    begin
	      ERROR; WRITELN(TTY,'Junk after line number')
	    end
	end
      else
	if SY # INTCONST then
	  begin
	    ERROR; WRITELN(TTY,'Not a line number')
	  end
	else
	  begin
	    LINENR := VAL.IVAL; GPAGE := DEFPAGE %DEFAULT\;
	    INSYMBOL;
	    if SY = SLASHSY then
	      begin
		INSYMBOL;
		if SY # INTCONST then
		  begin
		    ERROR; WRITELN(TTY,'Illegal page number')
		  end
		else
		  begin
		    GPAGE := VAL.IVAL; INSYMBOL
		  end
	      end;
	    if SY in FOLLOW then
	      GETLINPAG := TRUE
	    else
	      begin
		ERROR; WRITELN(TTY,'Junk after linenumber')
	      end
	  end
    end;

  procedure FINDOUT;
    {Here when a FIND command is found}
    var
      REPCOUNT:INTEGER;
    begin
      REPCOUNT := 1;
      if SY = INTCONST then
	begin
	  REPCOUNT := VAL.IVAL; INSYMBOL
	end;
      if SY = EOLSY then
	SHOWSTCONTEXT(FALSE,REPCOUNT)
      else
	if (SY = STRINGCONST) or (SY = CHARCONST) then
	  begin
	    INSYMBOL;
	    if ENDOK then
	      SHOWSTCONTEXT(TRUE,REPCOUNT)
	  end
	else
	  begin
	    ERROR; WRITELN(TTY,'> Expecting ''target string''')
	  end
    end;

  procedure TYPEOUT;
    {Here when a TYPE command is found}
    var
      STPAGE,STLINE,ENDPAGE,ENDLINE:INTEGER;
      I,J:INTEGER; SOSEND,SOSNUM:packed array [1..5]of CHAR;
    begin
      if not GETLINPAG(STLINE,STPAGE,DOTPAGE,[EOLSY,STAR,INTCONST]) then
	goto 666;
      if SY = EOLSY then
	begin
	  ENDLINE := STLINE; ENDPAGE := STPAGE
	end
      else
	if not GETLINPAG(ENDLINE,ENDPAGE,STPAGE,[EOLSY]) then
	  goto 666;

      if (ENDPAGE < STPAGE) or ((ENDPAGE = STPAGE) and (ENDLINE < STLINE)) then
	begin
	  WRITELN(TTY,'> Order of lines reversed?'); goto 666
	end;

      {stpage/stline and endpage/endline are now set up.  Do the typeout.}
      FINDPGLN(SOURCE,STPAGE,STLINE);
      STPAGE := DOTPAGE; STLINE := DOTLINE;
      GETLINENR(SOURCE,SOSNUM);
      if SOSNUM = '-----' then
	{code for non-SOS files}
	while (STPAGE < ENDPAGE) or
	((STPAGE = ENDPAGE) and (STLINE <= ENDLINE)) do
	  begin
	    if EOF(SOURCE) then
	      goto 666;
	    WRITE(TTY,STLINE:0,' ');
	    while not EOLN(SOURCE) do 
	      begin 
	        WRITE(TTY,SOURCE^);
	        GET(SOURCE);
	      end;
	    WRITELN(TTY);
	    while SOURCE^ = CHR(15B) do GET(SOURCE);
	    if SOURCE^ = CHR(14B) then
	      begin
	        STLINE := 1;  STPAGE := STPAGE + 1;
	        WRITELN(TTY,'Page ',STPAGE:0);
	      end
	    else STLINE := STLINE + 1;
	    if EOLN(SOURCE) then {skip EOLN - none if bare CR}
	      GET(SOURCE);
	  end
      else
	begin
	  {Code for SOS files}
	  J := ENDLINE;
	  if (ENDLINE > 99999) then
	    SOSEND := 'AAAAA'   {something bigger than any legal number}
	  else
	    for I := 0 to 4 do
	      begin
		SOSEND[5-I] := CHR((J mod 10) + 60B);
		J := J div 10
	      end;
	  while ((STPAGE < ENDPAGE) or
	  ((STPAGE = ENDPAGE) and (SOSNUM <= SOSEND))) do
	    begin
	    if EOF(SOURCE) then
	      goto 666;
	    if SOSNUM <> '     ' then
	      begin
	      WRITE(TTY,SOSNUM,' ');
	      while not EOLN(SOURCE) do 
	        begin 
	          WRITE(TTY,SOURCE^);
	          GET(SOURCE);
	        end;
	      WRITELN(TTY);
	      end;
	    while SOURCE^ = CHR(15B) do GET(SOURCE);
	    if SOURCE^ = CHR(14B) then
	      begin
	        STLINE := 1;  STPAGE := STPAGE + 1;
	        WRITELN(TTY,'Page ',STPAGE:0);
	      end
	    else STLINE := STLINE + 1;
	    if EOLN(SOURCE) then {skip EOLN - none if bare CR}
	      GET(SOURCE);
	    GETLINENR(SOURCE,SOSNUM)
	    end
	end;
    666:
    end;


  procedure BREAKPOINT;
    var
      LINENR: INTEGER;
      PAGER: PAGEELEM;
      LLE: LINEELEM;
      LLINE,LPAGE: INTEGER;
      OLDLINE: INTEGER;
      OLDADDR: ^LINEELEM;
      CHANGEPTR: ^LINEELEM;

    begin   %BREAKPOINT\
      COMMAND([LISTSY,NOTSY]);
      case SY of
	LISTSY:
	  begin
	    INSYMBOL;
	    if ENDOK then
	      for I := 1 to STOPMAX do
		with STOPTABLE[I] do
		  if PAGE > 0 then
		    WRITELN(TTY,'> ', MODENTRY.MODNAME:STRLEN(MODENTRY.MODNAME),
			    ':', THISLINE:1,'/', PAGE:1)
	  end;
	NOTSY:
	  begin
	    INSYMBOL;  COMMAND([ALLSY]);
	    if SY = ALLSY then
	      begin
		INSYMBOL;
		if ENDOK then
		  for I:= 1 to STOPMAX do
		    if (STOPTABLE[I].THISADDR # nil) then
		      begin
			STOPTABLE[I].PAGE:=0;
			if TOPS10 then
			  PROTECTION(FALSE);
			STOPTABLE[I].THISADDR^.CONSTANT1 :=  STOPTABLE[I].ORIGINALCONT.CONSTANT1;
			if TOPS10 then
			  PROTECTION(TRUE);
			STOPTABLE[I].THISADDR := nil;
		      end
	      end
	    else
	      {NOT ALLSY}
	      if GETLINPAG(LINENR,GPAGE,DOTPAGE,[EOLSY]) then
		begin
		  I := STOPSEARCH(LINENR,E_DEB(ENTRY1.NEXTDEB));
		  if I = 0 then
		    WRITELN(TTY, '> ? No such stop')
		  else
		    with STOPTABLE[I] do
		      begin
			PAGE := 0;
			if TOPS10 then
			  PROTECTION(FALSE);
			THISADDR^.CONSTANT1 := ORIGINALCONT.CONSTANT1;
			if TOPS10 then
			  PROTECTION(TRUE);
			THISADDR := nil
		      end
		end
	  end;
	INTCONST,STAR:
	  if GETLINPAG(LINENR,GPAGE,DOTPAGE,[EOLSY])
	    and (STOPSEARCH(LINENR,E_DEB(ENTRY1.NEXTDEB)) = 0) then
	    begin  %A NEW STOP\
	      STOPNR := 1;
	      while STOPTABLE[STOPNR].PAGE # 0 do STOPNR := STOPNR + 1;
	      if STOPNR > STOPMAX then
		WRITELN(TTY,'> Too many stops')
	      else
		begin
		  %EXECUTE STOP\
		  %1.STEP: SEARCH PAGE\
		  PAGER := ENTRY1.LASTPAGEELEM;
		  LPAGE := PAGEVALUE(PAGER);
		  if LPAGE < GPAGE then
		    WRITELN(TTY,'> Pagenumber too large')
		  else
		    begin
		      while LPAGE > GPAGE do with POINTERCV do
			begin
			  LOAD_ADDRESS(PAGER.PAGPTR);
			  PAGER := FIX_PAGPTR^;
			  LPAGE := PAGEVALUE(PAGER)
			end;
		      if LPAGE # GPAGE then
			begin
			  WRITELN(TTY,'> Can''t stop on this page'); goto 1
			end;
		      with LLE, PAGER do
			begin
			  LLINE := LASTLINE; ADP := LASTSTOP
			end;
		      if LLINE < LINENR then
			WRITELN(TTY,'> Linenumber too large')
		      else
			begin
			  while LLINE > LINENR do
			    begin
			      OLDLINE := LLINE;   LOAD_ADDRESS(LLE.ADP);
			      OLDADDR := POINTERCV.FIX_STOP;
			      LLE := POINTERCV.FIX_STOP^;
			      LLINE := LINEVALUE(LLE,LLINE,E_DEB(ENTRY1.NEXTDEB))
			    end;
			  if LLINE # LINENR then
			    begin
			      WRITE(TTY,'> Next possible: ',OLDLINE:LENGTH(OLDLINE),' (Y or N)? ');
			      READLN(TTY);
			      INSYMBOL;
			      if ((SY = IDENT)
				  and ((ID = 'Y         ') or (ID = 'YES       '))) then
				{nothing}
			      else
				if ((SY = IDENT)
				    and ((ID = 'N         ') or (ID = 'NO        '))) then
				  goto 1
				else
				  begin
				    WRITELN(TTY,'> NO assumed');  goto 1
				  end;
			      LLE.ADP := ORD(OLDADDR); LLINE := OLDLINE
			    end;
			  if EXTENDED_ADDRESSING
			    then LOAD_ADDRESS(LLE.ADP-1) (* replace XCT *)
			    else load_address (lle.adp); (* replace the LUUO *)
			  CHANGEPTR := POINTERCV.FIX_STOP;
			  with STOPTABLE[STOPNR] do
			    begin
			      MODENTRY := ENTRY1; THISLINE := LLINE;  PAGE := GPAGE;
			      ORIGINALCONT := CHANGEPTR^; THISADDR := CHANGEPTR
			    end;
			  if TOPS10 then
			    PROTECTION(FALSE);
			  CHANGEPTR^.CONSTANT1 := ENTRY2.STOPPY;
			  if TOPS10 then
			    PROTECTION(TRUE)
			end
		    end
		end;
    1:
	    end %INTCONST\;
	others: begin
	  ERROR;  WRITELN(TTY,'> Expecting legal option of STOP command')
	end
	end %CASE\
    end %BREAKPOINT\;


  procedure LINEINTERVAL(FADDR: GLOBAL_ADDRESS;
			 var LIN1,LIN2,PAG: INTEGER;
			 var LENTRY1:DEBUGENTRY);
    var
      PAGER: PAGEELEM; LINER: LINEELEM;
      LADDR: GLOBAL_ADDRESS;
    begin
      with POINTERCV do
	begin
	  LOAD_ADDRESS(ENTRY2.ENTRYPTR); LENTRY1 := FIX_DEBPTR^;  %first module\
	  loop    %search modules\
	    PAGER := LENTRY1.LASTPAGEELEM;  %first page in module\
	    LOAD_ADDRESS(PAGER.LASTSTOP);
	    if FADDR <= ADDR        %see if above this module\ then
	      loop    %no - search pages\
		LOAD_ADDRESS(PAGER.PAGPTR);  LADDR := ADDR;
	      exit if LADDR <= FADDR;         %laddr=0 on dummy page 0\
		PAGER := FIX_PAGPTR^
	    end
	    else
	      LADDR := 0;
	    %above this module - laddr=0 mean fail\
	    LOAD_ADDRESS(LENTRY1.NEXTDEB);
	  exit if (LADDR # 0) or (ADDR = 0);  %found or tried last module\
	    LENTRY1 := POINTERCV.FIX_DEBPTR^
	end;
	  LINER.ADP := PAGER.LASTSTOP
	end {WITH POINTERCV};
      PAG := PAGEVALUE(PAGER); LIN2 := PAGER.LASTLINE;
      GPAGE := PAG;  LIN1 := LIN2;
      loop
	LOAD_ADDRESS(LINER.ADP);
	LADDR := POINTERCV.ADDR;
	LINER := POINTERCV.FIX_STOP^
      exit if LADDR <= FADDR;
	LIN2 := LIN1; LIN1 := LINEVALUE(LINER,LIN2,E_DEB(LENTRY1.NEXTDEB))
    end;
      if LADDR = FADDR {If exact match, only give him one} then
	LIN2 := LIN1;
      if LIN1 < 0 then
	LIN1 := 0
    end %LINEINTERVAL\;

  procedure STOPMESSAGE(FADDR: GLOBAL_ADDRESS);
    var
      LIN1, LIN2, PAG: INTEGER;
    begin   %NB - will reset ENTRY1 to module found in LINEINTERVAL\
      LINEINTERVAL(FADDR,LIN1,LIN2,PAG,ENTRY1);
      WRITE(TTY, '> Stop in ',ENTRY1.MODNAME:STRLEN(ENTRY1.MODNAME),':',
	    LIN1:LENGTH(LIN1), '/', PAG:LENGTH(PAG));
      if LIN2 <> LIN1 then
	WRITE(TTY,':',LIN2:LENGTH(LIN2));
      WRITELN(TTY); CHECKSOURCE(SOURCE,ENTRY1);
      CURENT := E_DEB(ENTRY1.NEXTDEB); SHOWCONTEXT(PAG,LIN1)
    end %STOPMESSAGE\;

  procedure TRACEOUT(var OUTFILE:TEXT;TRACE_LIMIT:INTEGER);
    var
      LCP: CTP;
      NEXT_CTP: CTP;
      LADDR: GLOBAL_ADDRESS;
      LIN1, LIN2, PAG: INTEGER;
      LENTRY1:DEBUGENTRY;
      DEPTH : INTEGER;
    begin   %NB - will not reset global ENTRY1\
      FIRSTBASIS(LEFT);  LEFTSPACE := 0;
      LADDR:=CALL_ADDRESS;  DEPTH := POS_IN_STACK;
      if TRACE_LIMIT <= DEPTH then
	begin
	  WRITE(OUTFILE,'>   Depth  Module Name  Subprogram  Page    Line');
	  NEWLINE(OUTFILE);
	  loop
	    LCP := IDTREE;  WRITE(OUTFILE,DEPTH:6,'   ');
	    LINEINTERVAL (LADDR, LIN1,  LIN2, PAG, LENTRY1);
	  exit if (BASIS = NULLPTR) or (DEPTH = TRACE_LIMIT);
	    if LCP = nil then
	      WRITE(OUTFILE,LENTRY1.MODNAME,'   ','Local D-? ','  ',PAG:3,'   ',LIN1:5)
	    else
	      begin
		NEXT_CTP := E_CTP(LCP^.NEXT);
		WRITE(OUTFILE,LENTRY1.MODNAME,'   ',NEXT_CTP^.NAME,
		      '  ',PAG:3,'   ',LIN1:5)
	      end;
	    NEWLINE(OUTFILE);  DEPTH := DEPTH - 1;
	    LADDR := ORD(ACRPOINT(BASIS^[0]+PREV_BASIS,FULL));
	    SUCCBASIS(LEFT%=DYNAMIC\)
	end;
	  if (BASIS = NULLPTR) or (DEPTH = 0) then
	    WRITE(OUTFILE,LENTRY1.MODNAME,'   MAIN        ',PAG:3,'   ',LIN1:5)
	  else
	    if LCP = nil then
	      WRITE(OUTFILE,LENTRY1.MODNAME,'   ','Local D-? ','  ',PAG:3,'   ',LIN1:5)
	    else
	      begin
		NEXT_CTP := E_CTP(LCP^.NEXT);
		WRITE(OUTFILE,LENTRY1.MODNAME,'   ',NEXT_CTP^.NAME,
		      '  ',PAG:3,'   ',LIN1:5)
	      end;
	  NEWLINE(OUTFILE);
	end
      else
	WRITE (OUTFILE, '>');
      NEWLINE(OUTFILE);
      if NO_OF_CALLS = 0 then
	WRITE (OUTFILE, 'No subprograms called')
      else
	if NO_OF_CALLS = 1 then
	  WRITE (OUTFILE, 'One subprogram called')
	else
	  WRITE (OUTFILE, NO_OF_CALLS, ' subprograms called');
    end %TRACEOUT\;

  procedure SETMOD;
    var
      LENTRY1:DEBUGENTRY;
      PAG,LIN1,LIN2:INTEGER;
    begin
      begin
	if SY = INTCONST then
	  begin
	    if VAL.IVAL > NO_OF_CALLS then
	      begin
		ERROR;
		WRITELN(TTY,'The stack is ',NO_OF_CALLS +1:0,' deep');
		INSYMBOL
	      end
	    else
	      begin
		BASIS:=ACRPOINT(ACCUS^[0+16B],FULL);
		CALL_ADDRESS:=ENTRY2.RETURNADDR;
		for I:= 1 to NO_OF_CALLS - VAL.IVAL do
		  begin
		    CALL_ADDRESS := ORD(ACRPOINT(BASIS^[0]+PREV_BASIS,FULL));
		    SUCCBASIS(LEFT)
		  end;
		CALL_BASIS := BASIS;
		LINEINTERVAL(CALL_ADDRESS,LIN1,LIN2,PAG,ENTRY1);
		CHECKSOURCE(SOURCE,ENTRY1);
		CURENT := E_DEB(ENTRY1.NEXTDEB);
		POS_IN_STACK := VAL.IVAL;
		INSYMBOL
	      end
	  end;
	if (SY # IDENT) and (SY # EOLSY) then
	  WRITELN(TTY, '> Module name expected')
	else
	  if SY = IDENT then
	    begin
	      LOAD_ADDRESS(ENTRY2.ENTRYPTR);
	      LENTRY1 := POINTERCV.FIX_DEBPTR^;
	      while ((LENTRY1.MODNAME # ID)
	      and (ORD(LENTRY1.NEXTDEB) # 0)) do
		begin
		  LOAD_ADDRESS(LENTRY1.NEXTDEB);
		  LENTRY1 := POINTERCV.FIX_DEBPTR^
		end;
	      if LENTRY1.MODNAME = ID then
		begin
		  ENTRY1:=LENTRY1;  CHECKSOURCE(SOURCE,ENTRY1)
		end
	      else
		WRITELN(TTY,'> Requested module not found')
	    end
      end
    end;
  procedure INIT;
    begin
      if ORD(ENTRY2.ENTRYPTR) # 0 then
	begin
	  CODE_SECTION := ENTRY2.RETURNADDR div 1000000B;
	  LOAD_ADDRESS(ENTRY2.ENTRYPTR);
	  ENTRY1 := POINTERCV.FIX_DEBPTR^;
	  while ORD(ENTRY1.NEXTDEB) # 0 do
	    begin
	      LOAD_ADDRESS(ENTRY1.NEXTDEB);
	      ENTRY1 := POINTERCV.FIX_DEBPTR^  %main prog is end of list\
	    end
	end
      else
	begin
	  WRITELN (TTY, '> No modules compiled with /DEBUG');
	  QUIT
	end;
      NULLPTR := ACRPOINT(0,FULL);
      CURENT := E_DEB(ENTRY1.NEXTDEB);
      LOAD_ADDRESS(ENTRY2.REGISTRS);
      ACCUS := POINTERCV.FIX_ACR;
      CALL_ADDRESS := ENTRY2.RETURNADDR;
      CALL_BASIS := ACRPOINT(ACCUS^[0+16B],FULL);
      BASIS := CALL_BASIS;
      TESTGLOBALBASIS(LEFT);
      NO_OF_CALLS:=0;
      while BASIS # NULLPTR do
	begin
	  NO_OF_CALLS := NO_OF_CALLS + 1;  SUCCBASIS(LEFT)
	end;
      POS_IN_STACK:=NO_OF_CALLS
    end;

  procedure ONE_VAR_OUT(var OUTFILE:TEXT;LCP:CTP);
    var
      LBASIS:ACR;
      IDTYPE_STP: STP;
    begin
      LBASIS:=BASIS;
      with LCP^,GATTR do
	begin
	  KIND:=VARBL;  
	  if EXTENDED then
	    GADDR := XADDR + ORD(BASIS)
	  else GADDR:=VADDR+ORD(BASIS);
	  GBASIS := ORD(BASIS);
	  BASIS:=NULLPTR;  GBITCOUNT:=0;
	  if VKIND=FORMAL then
	    GADDR:=NULLPTR^[GADDR];
	  TYPTR := E_STP(IDTYPE);  PACKFG:=FALSE;
	  WRITENAME(OUTFILE,LCP);
	  WRITE(OUTFILE,' = '); CHCNT:=CHCNT+1;
(* 28 - 4-word sets *)
	  if TYPTR^.FORM > QPOWER then
	    begin
	      NL:=TRUE;  LEFTSPACE:=2
	    end;
	  WRITESTRUCTURE(OUTFILE,E_STP(IDTYPE));
	  IDTYPE_STP := E_STP(IDTYPE);
(* 28 - 4-word sets *)
	  if IDTYPE_STP^.FORM >= QPOWER then
	    begin
	      LEFTSPACE:=0; NEWLINE(OUTFILE)
	    end;
	  NEWLINE(OUTFILE);
	end (* WITH *);
      BASIS:=LBASIS
    end (* ONE_VAR_OUT *);


  procedure SECTION_OUT(var OUTFILE:TEXT;LCP:CTP;FFORMSET:FORMSET);
    var
      LINK_CTP: CTP;
      IDTYPE_STP: STP;
    begin
      with LCP^ do
	begin
	  LINK_CTP := E_CTP(LLINK);
	  if LINK_CTP <> nil then
	    SECTION_OUT(OUTFILE,LINK_CTP,FFORMSET);
	  IDTYPE_STP := E_STP(IDTYPE);
	  if (KLASS=VARS) and (IDTYPE_STP^.FORM in FFORMSET) then
	    ONE_VAR_OUT(OUTFILE,LCP);
	  LINK_CTP := E_CTP(RLINK);
	  if LINK_CTP <> nil then
	    SECTION_OUT(OUTFILE,LINK_CTP,FFORMSET);
	end (* WITH *);
    end (* SECTION_OUT *);


  procedure STACK_OUT(var OUTFILE:TEXT;S_DUMP_LIMIT:INTEGER);
    var
      TREEPNT:CTP;
      NEXT_CTP: CTP;
      LADDR:GLOBAL_ADDRESS;
      LIN1,LIN2,PAG:INTEGER;
      SAVE_ENTRY1:DEBUGENTRY;
      DEPTH  :  INTEGER;
    begin
      SAVE_ENTRY1:=ENTRY1;  CHCNT:=0;  DEPTH := POS_IN_STACK;
      FIRSTBASIS(LEFT);  LADDR := CALL_ADDRESS;
      if S_DUMP_LIMIT <= DEPTH then
	loop
	  LINEINTERVAL(LADDR,LIN1,LIN2,PAG,ENTRY1);  TREEPNT:=IDTREE;
	  if TREEPNT # nil then
	    begin
	      if BASIS=NULLPTR then
		WRITE(OUTFILE,'  MAIN')
	      else
		begin
		  NEXT_CTP := E_CTP(TREEPNT^.NEXT);
		  if NEXT_CTP^.KLASS = FUNC then
		    WRITE(OUTFILE,'FUNCTION ')
		  else
		    WRITE(OUTFILE,'PROCEDURE ');
		  WRITE(OUTFILE,NEXT_CTP^.NAME:STRLEN(NEXT_CTP^.NAME));
		end;
	      WRITE(OUTFILE,' In module ',ENTRY1.MODNAME);
	      NEWLINE(OUTFILE);
	      SECTION_OUT(OUTFILE,TREEPNT,[SCALAR,SUBRANGE,POINTER]);
	      NEWLINE(OUTFILE);
(* 28 -4-word sets *)
	      SECTION_OUT(OUTFILE,TREEPNT,[POWER,QPOWER,ARRAYS,RECORDS]);
	    end
	  else
	    WRITE(OUTFILE,' There is no information about this part of the program (local D- ??)');
	  NEWLINE(OUTFILE);
	exit if (BASIS=NULLPTR) or (S_DUMP_LIMIT = DEPTH);
	  LADDR:=ORD(ACRPOINT(BASIS^[0]+PREV_BASIS,FULL));
	  SUCCBASIS(LEFT);  DEPTH := DEPTH - 1
      end; (* LOOP *)
      ENTRY1 := SAVE_ENTRY1;  WRITELN(OUTFILE)
    end (* ALL_VAR_OUT *);

    % this was not being called, so it is now just a comment...
     !procedure HEAP_OUT;
     !
     !label
     !  1;
     !
     !type
     !  ALLOC_HEAD = packed record
     !                           VAR_TYPE : STP;
     !                           NEXT     : ^ALLOC_HEAD;
     !                         end;
     !
     !var
     ! REC  :  record
     !               case INTEGER of
     !                    1:(INT  :  INTEGER);
     !                    2:(PTR  :  ^ALLOC_HEAD);
     !             end;
     !  HEAP_BTTM : INTEGER;
     !  PREV_REC  : INTEGER;
     !begin
     !  MARK (HEAP_BTTM);  LOAD_ADDRESS(HEAP_BTTM);
     !  REC.INT := POINTERCV.ADDR;  PREV_REC := 0;
     !  while REC.PTR # nil do
     !       begin
     !         if (ORD (REC.PTR) < HEAP_BTTM) or
     !           (ORD (REC.PTR) < PREV_REC)
     !         then goto 1
     !         else if (ORD(REC.PTR^.VAR_TYPE) < ORD (nil)) or
     !           (ORD(REC.PTR^.VAR_TYPE) >= ORD (ENTRY2.STACKBOTTOM))
     !         then
     !1:             begin
     !             NEWLINE(TTYOUTPUT);
     !             NEWLINE(TTYOUTPUT);
     !             WRITE (TTY, 'Heap chain shattered.  Abandoning HEAP DUMP.');
     !             REC.PTR := nil;
     !           end
     !         else begin
     !           NEWLINE(TTYOUTPUT);
     !           WRITE (TTY, ORD (REC.PTR) + 1:6:O, 'B^=');
     !           if REC.PTR^.VAR_TYPE = nil
     !           then begin
     !             NEWLINE(TTYOUTPUT);
     !             WRITE (TTY,'Type of variable no known.');
     !           end
     !           else begin
     !             with GATTR do
     !               begin
     !                 NL := TRUE;
     !                 TYPTR := REC.PTR^.VAR_TYPE;
     !                 KIND := VARBL;
     !                 PACKFG := FALSE;
     !                 GADDR := ORD (REC.PTR) + 1;
     !                 GBITCOUNT := 0;
     !               end;
     !             WRITESTRUCTURE (TTYOUTPUT,REC.PTR^.VAR_TYPE);
     !           end; (* TYPE POINTER OK *)
     !           PREV_REC := ORD (REC.PTR);
     !           REC.PTR := REC.PTR^.NEXT
     !         end (* rec ok *)
     !       end (* While *)
     !end; (* Heap_out *)
     ! %The entire HEAP_OUT procedure is just a comment...\
  procedure HELP;
    begin
      COMMAND([TERMSY]);
      if SY = TERMSY then
	begin
	  WRITELN(TTY,'> The following terms are used in the command summary:');
	  WRITELN(TTY,'>');
	  WRITELN(TTY,'>  depth: number as shown in TRACE.');
	  WRITELN(TTY,'>  depth-cutoff: don''t show anything for depth numbers less than');
	  WRITELN(TTY,'>      this.  See TRACE for depth numbers. If omitted, show all.');
	  WRITELN(TTY,'>  file-name: any file name, must be in ''''.  If omitted, use terminal.');
	  WRITELN(TTY,'>  line-no:  123/45 - line 123 on page 45');
	  WRITELN(TTY,'>            123    - line 123 on current page');
	  WRITELN(TTY,'>      *      - current page and line');
	  WRITELN(TTY,'>                (use * = to see what current line/page is)');
	  WRITELN(TTY,'>  module-name: as shown in TRACE.  Usually name of the .REL file');
	  WRITELN(TTY,'>  repeat: number of occurences to find with single command');
	  WRITELN(TTY,'>  string: piece of text to look for, in quotes.  If omitted,');
	  WRITELN(TTY,'>  Previous string is reused.');
	  WRITELN(TTY,'>  value: any constant or pascal variable.');
	  WRITELN(TTY,'>  var: any legal pascal variable. Allows subscripts and dots');
	  WRITELN(TTY,'>  depth: number as shown in TRACE.');
	end
      else
	begin
	  WRITELN(TTY,'> The following commands are implemented: [] means optional');
	  WRITELN(TTY,'>');
	  WRITELN(TTY,'>  DDT                     enter DDT');
	  WRITELN(TTY,'>  END                     end debugging - continue the program');
	  WRITELN(TTY,'>  FIND [repeat] [''string''] find string in source file');
	  WRITELN(TTY,'>  HELP [TERMS]            TERMS for defn''s of terms');
	  WRITELN(TTY,'>  STOP line-no            puts break point at that line');
	  WRITELN(TTY,'>  STOP NOT line-no        remove a specific break');
	  WRITELN(TTY,'>  STOP NOT ALL            remove all break points');
	  WRITELN(TTY,'>  STOP LIST               list all break points');
	  WRITELN(TTY,'>  TRACE [depth-cutoff]    show active procedures');
	  WRITELN(TTY,'>  TYPE line-no [line-no]  show lines from source file');
	  WRITELN(TTY,'>  var = [O | H]           show value of variable (octal or hex)');
	  WRITELN(TTY,'>  var := value            set variable');
	  WRITELN(TTY,'>  STACKDUMP [depth-cutoff] [file-name]  show all var''s - to file');
	  WRITELN(TTY,'>  SHOW number             set number of lines to show at breaks');
	  WRITELN(TTY,'>  QUIT                    exit, closing open files');
	  WRITELN(TTY,'>    Single stepping mode - recognized by the "S>" prompt');
	  WRITELN(TTY,'>  STEP            enter step mode and do one line');
	  WRITELN(TTY,'>  <cr>            execute next line');
	  WRITELN(TTY,'>  <esc>           continue pgm until it exits current proc');
	  WRITELN(TTY,'>  END             leave step mode and continue program');
	  WRITELN(TTY,'>      [Other commands are still legal in step mode]');
	  WRITELN(TTY,'>    Don''t worry if you don''t understand this one:');
	  WRITELN(TTY,'>  OPEN [depth] [module-name]  set context');
	end
    end;
  begin   (* *** DEBUG *** *)
    INIT;
    LADDR := ENTRY2.RETURNADDR;
    case ENTRY2.STATUS of
      INITK:  begin
{We use LUUO's as our "no-ops" in normal mode.  In extended mode we use
 a separate XCT, with JUMP and SKIPA as the no-ops}
	if not EXTENDED_ADDRESSING then
	  begin
	    MAKELUUOS;
	    NO_OP_1 := 1;
	    NO_OP_2 := 2;
  	  end
	else
	  begin
	    NO_OP_1 := OP_JUMP;
	    NO_OP_2 := OP_SKIPA
	  end;
	STEPMODE := FALSE;
	LINEINTERVAL(LADDR,I,STLINE,STPAGE,ENTRY1);
	LADDR := 0; ENTRY2.RETURNADDR := 0;
	WRITELN(TTY, '> Stop at main BEGIN - module ',
		ENTRY1.MODNAME:STRLEN(ENTRY1.MODNAME),
		' open at ',STLINE:0,'/',STPAGE:0);
	OPENSOURCE(SOURCE,ENTRY1);
	SHOWCONTEXT(STPAGE,STLINE)
      end;
      STOPK:
	begin
	  for I := STOPMAX downto 0 do
	    if ORD (STOPTABLE[I].THISADDR) = LADDR then
	      goto 1;
  1:
	  WRITELN(TTY);
	  if I > 0 then
	    with STOPTABLE[I] do
	      begin
		ENTRY1:=MODENTRY;
		CHECKSOURCE(SOURCE,ENTRY1);
		CURENT := E_DEB(ENTRY1.NEXTDEB);
		WRITELN(TTY,'> Stop at ', ENTRY1.MODNAME:STRLEN(ENTRY1.MODNAME), ':',
			THISLINE:LENGTH(THISLINE), '/', PAGE:LENGTH(PAGE));
		SHOWCONTEXT(PAGE,THISLINE)
	      end
	  else
	    STOPMESSAGE(LADDR)
	end;
      DDTK:
	begin
	  WRITELN(TTY, '> Stop by DDT command');  STOPMESSAGE(LADDR)
	end;
      RUNTMERRK:
	begin
	  WRITELN(TTY);  WRITELN(TTY,'> Stop by runtime error');
	  STOPMESSAGE(LADDR)
	end
      end %CASE\;
    BUFFLNG := 0;
    while not EOLN(TTY) do
      begin
	BUFFLNG := BUFFLNG + 1;
	%READ (TTY, BUFFER[BUFFLNG])\ BUFFER[BUFFLNG] := TTY^; GET(TTY)
      end;
    OLDEOLN := TTY^;
    PROCEED := FALSE;  {proceed is set by END and STEP - exits this loop}
    repeat
      if STEPMODE then
	WRITE(TTY,'S> ')
      else
	WRITE(TTY,'>> ');
      READLN(TTY);
      CHCNT := 1;  {0 would be for prompt '> ', so '>> ' needs 1}
      if EOLN(TTY) then
	CH := ' '
      else
	begin
	  READ(TTY,CH);
	  if ORD(CH) >= 140B then
	    CH := CHR(ORD(CH)-40B);
	end;
      INSYMBOL;
      COMMAND([TYPESY,QUITSY,SHOWSY,FINDSY,STOPSY,TRACESY,ENDSY,STEPSY,
	       OPENSY,HELPSY,STACKDUMPSY,DDTSY]);
      case SY of
	TYPESY: begin
	  INSYMBOL;
	  if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	    goto 2;
	  TYPEOUT
	end;
	QUITSY: begin
	  INSYMBOL;
	  if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	    goto 2;
	  if ENDOK then
	    QUIT
	end;
	SHOWSY: begin
	  INSYMBOL;
	  if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	    goto 2;
	  if SY = INTCONST then
	    begin
	      INSYMBOL;
	      if ENDOK then
		SHOWLINES := VAL.IVAL
	    end
	  else
	    begin
	      ERROR; WRITELN(TTY,'Number expected')
	    end
	end;
	FINDSY:
	  begin
	    INSYMBOL;
	    if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	      goto 2;
	    FINDOUT
	  end;
	STAR:   begin
	  INSYMBOL;
	  if SY <> EQSY then
	    begin
	      ERROR; WRITELN(TTY,'> Unrecognized command')
	    end
	  else
	    WRITELN(TTY,'> ',DOTLINE:0,'/',DOTPAGE:0)
	end;
	STOPSY:
	  begin
	    INSYMBOL;
	    if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	      goto 2;
	    BREAKPOINT
	  end;
	TRACESY:begin
	  DEPTH_LIMIT := 0;  INSYMBOL;
	  if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	    goto 2;
	  if SY = INTCONST then
	    begin
	      DEPTH_LIMIT := VAL.IVAL; INSYMBOL
	    end;
	  if ENDOK then
	    TRACEOUT(TTYOUTPUT,DEPTH_LIMIT);
	  WRITELN(TTY)
	end;
	AMBIG:  begin
	  INSYMBOL;
	  if SY in [LBRACK,ARROW,PERIOD,EQSY,BECOMES] then
	    goto 2;
	  ERROR;  WRITELN(TTY,'Ambiguous abbreviation')
	end;
	IDENT:
	  begin
	    INSYMBOL;
  2:
	    if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	      begin
		{NULLPTR := ACRPOINT(0,FULL);}
		VARIABLE;
		case SY of
		  EQSY: begin
		    PRINTRADIX := DECIMAL; INSYMBOL;
		    if SY = IDENT then
		      if ID = 'H         ' then
			begin
			  PRINTRADIX := HEX; INSYMBOL
			end
		      else
			if ID = 'O         ' then
			  begin
			    PRINTRADIX := OCTAL; INSYMBOL
			  end;
		    if ENDOK then
		      with GATTR do
			if TYPTR # nil then
			  begin
			    WRITE(TTY,'> '); CHCNT := 0; LEFTSPACE := 0; NL := FALSE;
			    if KIND = CST
			    then
			      if TYPTR^.FORM = ARRAYS then
				begin
				  WRITE(TTYOUTPUT,CVAL.VALP^.SVAL:CVAL.VALP^.SLGTH);
				  CHCNT := CHCNT+CVAL.VALP^.SLGTH
				end
			      else
				WRITESCALAR(TTYOUTPUT,CVAL.IVAL,TYPTR)
			    else
			      WRITESTRUCTURE(TTYOUTPUT, TYPTR);
			    WRITELN(TTY)
			  end;
		    PRINTRADIX := DECIMAL
		  end;
		  BECOMES:
		    begin
		      INSYMBOL; ASSIGNMENT
		    end;
		  others:
		    begin
		      ERROR; WRITELN(TTY, '"=" or ":=" expected')
		    end
		  end
	      end
	    else
	      begin
		ERROR;
		WRITELN(TTY,'Unrecognized command - Type HELP for help.')
	      end
	  end;
	ENDSY: begin
	  INSYMBOL;
	  if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	    goto 2;
	  if ENDOK then
	    begin
	      STEPMODE := FALSE;  
	      SETDDT(EXTENDED_ADDRESSING,BRK_CLEAR); {set PDDT. to JFCL}
	      PROCEED := TRUE
	    end
	end;
	STEPSY:begin
	  INSYMBOL;
	  if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	    goto 2;
	  if ENDOK then
	    begin
	      STEPMODE := TRUE;  
	      SETDDT(EXTENDED_ADDRESSING,BRK_STEP); {set PDDT. to a call to debug}
	      PROCEED := TRUE
	    end
	end;
	EOLSY: if STEPMODE then
		 begin  {This is a step command in STEP mode}
		   PROCEED := TRUE;
		   if TTY^ = CHR(33B)  {if altmode, continue until exit this routine} then
		     begin
		       WRITELN(TTY);
		       SETDDT(EXTENDED_ADDRESSING,BRK_SKIP); {set PDDT. to call checklevel}
		     end
		   else
		     SETDDT(EXTENDED_ADDRESSING,BRK_STEP);
		   {normal break}
		 end;
	OPENSY:
	  begin
	    INSYMBOL;
	    if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	      goto 2;
	    SETMOD
	  end;
	DDTSY:
	  begin
	    INSYMBOL;
	    if SY in [LBRACK,ARROW,PERIOD,EQSY,BECOMES] then
	      goto 2;
	    GETDDT;         {enter DDT}
	  end;
	HELPSY:
	  begin
	    INSYMBOL;
	    if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	      goto 2;
	    HELP
	  end;
	STACKDUMPSY:
	  begin
	    for I:=1 to STRGLGTH do STRING^.SVAL[I]:=BLANK;
	    INSYMBOL;
	    if SY in [LBRACK,ARROW,PERIOD, EQSY,BECOMES] then
	      goto 2;
	    DEPTH_LIMIT := 0;
	    if SY = INTCONST then
	      begin
		DEPTH_LIMIT := VAL.IVAL; INSYMBOL
	      end;
	    if (SY = STRINGCONST) or (SY = CHARCONST) then
	      INSYMBOL;
	    if ENDOK then
	      begin
		ALL_BLANK:=TRUE;
		for I:=1 to STRGLGTH do
		  ALL_BLANK:=ALL_BLANK and (STRING^.SVAL[I] = BLANK);
		if ALL_BLANK then
		  begin
		    TRACEOUT(TTYOUTPUT,DEPTH_LIMIT);
		    NEWLINE(TTYOUTPUT);
		    STACK_OUT(TTYOUTPUT,DEPTH_LIMIT);
		  end
		else
		  begin
		    REWRITE(DUMP_FILE,STRING^.SVAL);
		    if not(EOF(DUMP_FILE)) then
		      begin
			ERROR;  ANALYS(DUMP_FILE)
		      end
		    else
		      begin
			TRACEOUT(DUMP_FILE,DEPTH_LIMIT);
			NEWLINE(DUMP_FILE);
			STACK_OUT(DUMP_FILE,DEPTH_LIMIT)
		      end;
		    CLOSE(DUMP_FILE)
		  end
	      end
	  end;
	others: WRITELN(TTY,'> No such command.  Type HELP for help')
	end %CASE\
    until PROCEED;
    if (ENTRY2.STATUS = RUNTMERRK) and not TOPS10 then
      WRITELN(TTY,'> WARNING: Continuing after an error -- Do not trust results!');
    if (ENTRY2.STATUS = RUNTMERRK) and TOPS10 then
      WRITELN(TTY,'> Cannot continue')
    else
      begin
	if TOPS10  {for tops-20, nothing needs to be done} then
	  if (BUFFLNG = 0) and ((TTY^ = CHR(15B)) or (OLDEOLN <> CHR(15B))) then
	    begin  {We at least as many char's as we can}
	      if (OLDEOLN <> CHR(15B)) and (TTY^ = CHR(15B)) then
		GET(TTY);
	      {We have 2 char's (CRLF) - need one only}
	      TTY^ := OLDEOLN   {restore EOLN to saved one}
	    end
	  else
	    begin
	      WRITE  (TTY, '> Input deleted: ');
	      for I := 1 to BUFFLNG do
		if ORD(BUFFER[I]) < 40B then
		  WRITE(TTY,'^',CHR(ORD(BUFFER[I])+100B))
		else
		  WRITE(TTY,BUFFER[I]);
	      case ORD(OLDEOLN) of
		12B: WRITELN(TTY,'<LF>');
		15B: WRITELN(TTY,'<CR>');
		33B: WRITELN(TTY,'<ESC>');
		others: WRITELN(TTY,'^',CHR(ORD(OLDEOLN)+100B))
	      end;
	      WRITE(TTY, '> Type it again: '); READLN(TTY)
	    end
      end
  end %DEBUG\.
