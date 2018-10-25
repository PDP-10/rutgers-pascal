{$m-,d-,c-}
program pasprm;
{  this is a replacement for pasprm in paslnk.mac, for use when you do
	not want to interface to the EXEC.  It calls a COMND jsys scanner}

include 'pascmd.pas','string.pas';

const
	noswitch=0;
	zero=1;
	stack=2;
	objectlist=3;
	nomain=4;
	nodebug=5;
	nocheck=6;
	nobinary=7;
	list=8;
	heap=9;
	debug=10;
	cref=11;
	version=12;
	object=13;
	load=14;
	noload=15;
	arithcheck=16;
	noarithcheck=17;
	extend=18;
	noextend=19;
	run=20;
	offset=21;
	optimize=22;

type retblk=record
	relnam:alfa;
	stkval:integer;
	heaval:integer;
	verval:integer;
	rpgsw:Boolean;
	crsw:Boolean;
	dsw:Boolean;
	csw:Boolean;
	msw:Boolean;
	tsw:Boolean;
	lsw:Boolean;
	zsw:Boolean;
	asw:Boolean;
	xsw:Boolean;
	end;

     retptr = ^ retblk;

     string3 = packed array [1..3] of char;

     filename = packed array[1..200] of char;

var
	nul: packed array [1..5] of char;
	aswseen,dobin,dolist,havebin,havelist,ccl:Boolean;
	runoffset,i,ptr,lstptr,key:integer;
	runtable,switchtable:table;
	first,rpg,swdone,dodeb:Boolean;  {on to load with pasddt}
	listname,relname,errname,buf:filename;
	r:retptr;
	idate,odate:array[1:1]of integer;
	xwd:packed record case Boolean of
		true:(full:integer);
		false:(lh:0..777777B;rh:0..777777B)
		end;

initprocedure;
	begin
	dodeb := false;  nul := 'NUL: ';
	first := true;
	end;

procedure quit; extern;

procedure pascmp; extern;

function getrpg:Boolean; extern;

procedure prget(i:integer); extern;

procedure runlink(var f:file;offset:integer); extern;

procedure comerr;
  begin
  cmerrmsg;
  quit
  end;

function getoct:integer;
		var x:packed record case Boolean of
			true:(word:integer);
			false:(junk:0..777777B;page:0..777B;addr:0..777B)
			end;
	begin
	x.word := cmnum8;
	if cmerr
	  then comerr;  {can only happen if RPG}
	with x do
	  begin
	    if junk <> 0
	      then begin
		writeln(tty); 
		writeln(tty,'?  Must be not over 777777');
		writeln(tty,'note - for extended addressing specify number of sections');
		if rpg
		  then quit;
		cmagain
	       end
	      else begin {junk = 0}
		if page = 0
		  then begin
		    if addr > 37B
		      then begin
			writeln(tty); 
			writeln(tty,'? number of sections exceeds 37');
			if rpg
			  then quit;
			cmagain
		       end
		    end
		  else begin {page <>0}
		    if addr = 0
		      then page := page-1;
		    addr := 777B
		   end;
		getoct := word
	      end
	   end
	end;

procedure getfile(outfile:Boolean; defext: string3; var f:text);
  begin
  if outfile
    then gjgen(400000000000B)	{an output file}
    else gjgen(100000000000B);  {an input file}
  gjext(defext);
  cmfil(f);
  if cmerr
    then comerr;  {Can only happen if RPG}
  end;  


function pasprm(var infile,outfile,relfile,errfile:text):retptr;

procedure dorun;
  begin
  runoffset := 0;
  swdone := false;
  havebin := false;
  repeat
    case cmint of
	-run: begin
	     getfile(false,'EXE',relfile);
	     havebin := true
	     end;
	-offset: runoffset := cmnum;
	others:	begin
		writeln(tty,'? Argument missing or extra argument');
		if rpg
		  then quit
		  else cmagain
		end
	end;
    cmmult;	{multiple mode}
    cmcfm;	{CRLF}
    i := cmswi(runtable);  {or switch - i is dummy return}
    i := cmdo;  {now actually do it}
    if cmerr
      then comerr;  {Can only happen if RPG}
   until i = 1;
  if not havebin
    then jsys(20B{gtjfn},2;100001B:0,{-1:}'SYS:LINK.EXE';relfile);
  runlink(relfile,runoffset)
  end;  

begin
if first then
  rpg := getrpg;  {True if we were run from the EXEC}
newz(r);

{Switchtable is table of compiler switches}
switchtable := tbmak(18);
tbadd(switchtable,zero,'ZERO',0);
tbadd(switchtable,version,'VERSION:',0);
tbadd(switchtable,stack,'STACK:',0);
tbadd(switchtable,optimize,'OPTIMIZE',0);
tbadd(switchtable,object,'OBJECT',0);
tbadd(switchtable,nomain,'NOMAIN',0);
tbadd(switchtable,noextend,'NOEXTEND',0);
tbadd(switchtable,nodebug,'NODEBUG',0);
tbadd(switchtable,nocheck,'NOCHECK',0);
tbadd(switchtable,nobinary,'NOBINARY',0);
tbadd(switchtable,noarithcheck,'NOARITHCHECK',0);
tbadd(switchtable,objectlist,'MACHINE-CODE',0);
tbadd(switchtable,list,'LIST',0);
tbadd(switchtable,heap,'HEAP:',0);
tbadd(switchtable,extend,'EXTEND',0);
tbadd(switchtable,debug,'DEBUG',0);
tbadd(switchtable,cref,'CREF',0);
tbadd(switchtable,arithcheck,'ARITHCHECK',0);

runtable := tbmak(2);
tbadd(runtable,run,'RUN:',0);
tbadd(runtable,offset,'OFFSET:',0);

if not rpg
  then cminir('PASCAL>')
  else begin
  cmini('');
  prget(cmstat)
  end;

if rpg
  then cmauto(false);

dobin := not rpg;  {Assume a .REL file, except EXEC will ask for it
		    explicitly if it is desired}
dolist := false;
havebin := false; {don't have JFN's yet}
havelist := false;
aswseen := false;
with r^ do
	begin
	dsw := true;
	csw := true;
	msw := true;
	tsw := true;
	end;
ccl := rpg or (cmmode = rescan);
r^.rpgsw := ccl;  {Controls output of PASCAL: message from compiler}

cmmult;			{need also /RUN}
gjgen(100000000000B);	{an input file}
gjext('PAS');
cmfil(infile);		{This is the main part of the command}
i := cmswi(runtable);
i := cmdo;
if cmerr
  then comerr;		{Can only happen if in RPG}
if i = 2
  then dorun;  

{Now that we have the input, we can make up default names for outputs,
 and the module name}

{Module name - 10 char's}
putstr('          ',10,relname,1);
jsys(30B{jfns};{-1:}relname,infile,001000B:0);
if relname[1] = chr(0)
	then begin
	putstr('MAIN',4,relname,1);
	relname[5] := chr(0);
	end;
with r^ do
	begin
	ptr := findnull(relname);
	relname[ptr] := ' ';
	putstr(relname,10,relnam,1);
	end;

{.LST file}
listname := relname;
lstptr := ptr;
putstr('.LST',4,listname,ptr);
listname[ptr+4] := chr(0);

{.REL file - truncate to 6 char's}
if ptr > 7
  then ptr := 7;
putstr('.REL',4,relname,ptr);
relname[ptr+4] := chr(0);

{.ERR file}
errname := relname;
putstr('.ERR.0',6,errname,ptr);
listname[ptr+6] := chr(0);

swdone := false;
loop
    cmmult;	{multiple mode}
    cmcfm;	{CRLF}
    i := cmswi(switchtable);  {or switch - i is dummy return}
    i := cmdo;  {now actually do it}
    if cmerr
      then comerr;  {Can only happen if RPG}
    exit if i = 1  {done if CRLF}    
    with r^ do
	case cmint of
		zero: zsw := true;
		-version: verval := cmnum8;
		-stack: stkval := getoct;
		optimize: begin
			csw := false;
			dsw := false
			end;
		object: lsw := true;
		-object: begin
			 getfile(true,'REL',relfile);
			 dobin := true;
			 havebin := true
			 end;
		nomain: msw := false;
		nodebug: dsw := false;
		arithcheck: begin asw := true; aswseen := true end;
		noarithcheck: begin asw := false; aswseen := true end;
		nocheck: csw := false;
		nobinary: dobin := false;
		objectlist: lsw := true;
		list: dolist := true;
		-list:  begin 
			getfile(true,'LST',outfile);
			dolist := true;
			havelist := true
			end;
		-heap: heaval := getoct;
		debug: dsw := true;
		cref: crsw := true;
		noswitch: swdone := true;
		extend: xsw := true;
		noextend: xsw := false;
		others:	begin
			writeln(tty,'? Argument missing or extra argument');
			if rpg
			  then quit
			  else cmagain
			end
		end;
	end;

{The default for /ARITH is the setting of /CHECK}
if not aswseen
  then r^.asw := r^.csw;

if dobin
  then if havebin
    then
    else jsys(20B{gtjfn},2;400001B:0,{-1:}relname;relfile)
  else jsys(20B{gtjfn},2;400011B:0,{-1:}nul;relfile);

if dolist or r^.crsw or r^.lsw
  then if havelist
    then
    else if r^.crsw
      then begin
      putstr('CRF',3,listname,lstptr+1);
      jsys(20B{gtjfn},2;400001B:0,{-1:}listname;outfile)
      end
      else jsys(20B{gtjfn},2;400001B:0,{-1:}listname;outfile)
  else jsys(20B{gtjfn},2;400011B:0,{-1:}nul;outfile);

jsys(20B{gtjfn},2;400001B:0,errname;errfile);

first := false;
pasprm := r
end;

procedure pasxit(var infile,outfile,relfile:text);
	begin
	close(infile);
	close(outfile);
	close(relfile);
	pascmp  {this restarts the program}
	end
.

