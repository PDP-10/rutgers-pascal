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
	compile=13;
	load=14;
	noload=15;
	arithcheck=16;
	noarithcheck=17;
	extend=18;
	noextend=19;

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

var
	nul: packed array [1..5] of char;
	aswseen,noexec,nolink,force,nobin,dolist,ccl:Boolean;
	i,ptr,key:integer;
	switchtable:table;
	swdone,dodeb:Boolean;  {on to load with pasddt}
	buf:packed array[1:100]of char;
	r:retptr;
	idate,odate:array[1:1]of integer;
	xwd:packed record case Boolean of
		true:(full:integer);
		false:(lh:0..777777B;rh:0..777777B)
		end;

initprocedure;
	begin
	dodeb := false;  nul := 'NUL: ';
	end;

procedure quit; extern;

procedure pascmp; extern;

procedure runlink; extern;

procedure calllink;
		var jobno:array[1:1]of integer;
		    tempname:packed array[1:12] of char;
		    i,j:integer;
	begin
	if nolink
		then quit;
	jsys(507B{getji},2;-1,-1:jobno,0);
	tempname := '000LNK.TMP;T';
	i := jobno[1];
	for j := 3 downto 1 do
		begin
		tempname[j] := chr((i mod 10) + 60B);
		i := i div 10;
		end;
	rewrite(output,tempname);
	if dodeb
		then writeln('SYS:PASDDT');
	writeln(buf:findnull(buf)-1);
	if noexec
		then writeln('/G')
		else writeln('/G/E');
	close(output);
	runlink
	end;

function getoct:integer;
		var x:packed record case Boolean of
			true:(word:integer);
			false:(junk:0..777777B;page:0..777B;addr:0..777B)
			end;
	begin
	x.word := cmnum8;
	with x do
	  begin
	    if junk <> 0
	      then begin
		writeln(tty); 
		writeln(tty,'?  Must be not over 777777');
		writeln(tty,'note - for extended addressing specify number of sections');
		cmagain
	       end
	      else begin {junk = 0}
		if page = 0
		  then begin
		    if addr > 37B
		      then begin
			writeln(tty); 
			writeln(tty,'? number of sections exceeds 37');
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

function pasprm(var infile,outfile,relfile,errfile:text):retptr;
begin
newz(r);
with r^ do
	begin
	dsw := true;
	csw := true;
	msw := true;
	tsw := true;
	end;
nobin := false;
dolist := false;
force := false;
noexec := false;
nolink := false;
aswseen := false;

{Switchtable is table of compiler switches}
switchtable := tbmak(19);
tbadd(switchtable,zero,'ZERO',0);
tbadd(switchtable,version,'VERSION:',0);
tbadd(switchtable,stack,'STACK:',0);
tbadd(switchtable,objectlist,'OBJECTLIST',0);
tbadd(switchtable,nomain,'NOMAIN',0);
tbadd(switchtable,noload,'NOLOAD',0);
tbadd(switchtable,nodebug,'NODEBUG',0);
tbadd(switchtable,nocheck,'NOCHECK',0);
tbadd(switchtable,nobinary,'NOBINARY',0);
tbadd(switchtable,noarithcheck,'NOARITHCHECK',0);
tbadd(switchtable,load,'LOAD',0);
tbadd(switchtable,list,'LIST',0);
tbadd(switchtable,heap,'HEAP:',0);
tbadd(switchtable,debug,'DEBUG',0);
tbadd(switchtable,cref,'CREF',0);
tbadd(switchtable,compile,'COMPILE',0);
tbadd(switchtable,arithcheck,'ARITHCHECK',0);
tbadd(switchtable,extend,'EXTEND',0);
tbadd(switchtable,noextend,'NOEXTEND',0);

cminir('PASCAL>');
ccl := cmmode = rescan;
r^.rpgsw := ccl;

gjgen(100000000000B);	{an input file}
gjext('PAS');
cmfil(infile);		{This is the main part of the command}

swdone := false;
loop
    cmmult;	{multiple mode}
    cmcfm;	{CRLF}
    i := cmswi(switchtable);  {or switch - i is dummy return}
    i := cmdo;  {now actually do it}
    exit if i = 1  {done if CRLF}    
    with r^ do
	case cmint of
		zero: zsw := true;
		-version: verval := cmnum8;
		-stack: stkval := getoct;
		objectlist: lsw := true;
		nomain: msw := false;
		nodebug: dsw := false;
		arithcheck: begin asw := true; aswseen := true end;
		noarithcheck: begin asw := false; aswseen := true end;
		nocheck: csw := false;
		nobinary: nobin := true;
		list: dolist := true;
		-heap: heaval := getoct;
		debug: dodeb := true;
		cref: crsw := true;
		compile: force := true;
		load: noexec := true;
		noload: nolink := true;
		noswitch: swdone := true;
		extend: xsw := true;
		noextend: xsw := false;
		end;
	end;

{The default for /ARITH is the setting of /CHECK}
if not aswseen
  then r^.asw := r^.csw;

{And make the rel file be the input name.REL.  also copy name
 as output module name.}
putstr('          ',10,buf,1);
jsys(30B{jfns};{-1:}buf,infile,001000B:0);
if buf[1] = chr(0)
	then begin
	putstr('MAIN',4,buf,1);
	buf[5] := chr(0);
	end;
with r^ do
	begin
	ptr := findnull(buf);
	buf[ptr] := ' ';
	putstr(buf,10,relnam,1);
	end;
if ptr > 7
  then ptr := 7;
putstr('.REL',4,buf,ptr);
buf[ptr+4] := chr(0);

{Here we see if a compilation is really needed, by checking creation dates}
if ccl then begin
jsys(20B{gtjfn},2,i;100001B:0,{-1:}buf;relfile);
if i = 2
  then begin
  jsys(63B{gtfdb};infile,1:5,idate);
  jsys(63B{gtfdb};relfile,1:5,odate);
  if (odate[1] > idate[1]) and not force
    then begin  {not needed - call link now}
    jsys(23B{rljfn},2;0:infile);
    jsys(30B{jfns};{-1:}buf,0:relfile,201100B:1);
    jsys(23B{rljfn},2;0:relfile);
    calllink
    end
  end;
jsys(23B{rljfn},2;0:relfile);
end;

if nobin
  then jsys(20B{gtjfn},2;400011B:0,{-1:}nul;relfile)
  else jsys(20B{gtjfn},2;400001B:0,{-1:}buf;relfile);
if dolist or r^.crsw or r^.lsw
  then begin
  if r^.crsw
    then putstr('CRF',3,buf,ptr+1)
    else putstr('LST',3,buf,ptr+1);
  jsys(20B{gtjfn},2;400001B:0,{-1:}buf;outfile);
  end
  else jsys(20B{gtjfn},2;400011B:0,{-1:}nul;outfile);

putstr('ERR.0',5,buf,ptr+1);
jsys(20B{gtjfn},2;1:0,buf;errfile);

pasprm := r
end;

procedure pasxit(var infile,outfile,relfile:text);
	begin
	close(infile);
	if ccl
		then jsys(30B{jfns};{-1:}buf,0:relfile,201100B:1);
	close(outfile);
	close(relfile);
	if ccl
		then calllink
		else pascmp
	end
.

