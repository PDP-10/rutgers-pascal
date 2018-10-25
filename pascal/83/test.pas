{to run with Fortran:
  exec test.pas,zerovr.mac,string.mac,pasfor.for
 without Fortran, you must edit out the code involving FORTPR and FORTFN
 and execute without PASFOR.FOR.  The test should be done both ways, as
 different memory management is used.  Look for ### to find where to
 edit}

{this program is designed to test the extended-addressing version of
 Pascal-20.  For this reason it concentrates on those things that are
 most likely to have been affected by the conversion to extended
 addressing.  It tests the runtime system fairly thoroughly, but it
 does not attempt to use every possible construct of the language,
 and so must not be considered a complete test of the compiler.  If
 used in combination with the Pascal Verification Suite it would 
 probably be a reasonable checkout.  It contains well over 100 checks.
 Each of them either prints an error message or increments a counter.
 The value of the counter is printed at the end.  It was intended that
 the program would first be run using the original version of Pascal-20,
 and the value printed by that would be compared to the value printed
 with the new version.  The error messages from this program are
 all of the form "Failure nnn", where nnn is a number.  There are a
 few cases where Pascal should generate an error, since this program also
 checks error processing.  It will print a warning before each expected
 error message.  Any other error messages are an indication that something
 is wrong.

}

include 's:<pascal>pascmd.pas';

label 11;
type
  foorec=record
	word:integer;
	f:text;
	next:^foorec
	end;
   array20=array[1..20]of integer;
   array513=array[1..513]of integer;
   array128000=array[1..128000]of integer;
   str4=packed array[1..4]of char;
   case5=0..4;
   array2=array[1..2]of integer;
var
	thisdate:packed array[1..9]of char;
	break:brkmsk;
	thistime:tadrec;
	data1:integer;
	data2:array[1..2]of integer;
	data3:array[1..3]of integer;
	data7:array[1..7]of integer;
	file1:file of integer;
	file2:file of array[1..2]of integer;
	file3:file of array[1..3]of integer;
        file7:file of array[1..7]of integer;
	tfile:text;
	x:real;
	ch:char;
	longbuf:packed array[1..48]of char;
	ar3:array[1..3]of char;
	str3:packed array[1..3]of char;
	foo:^foorec;
	buf,buf2:packed array[1..10]of char;
	unpacked:array[1..20]of char;
	ints,i,j,oldp,oldo,ret,newioj:integer;
	oks:integer;
	point20,point20a,point20b:^array20;
	point513:^array513;
	point128000,pointa128000,pointb128000:^array128000;
	dodefer,extended:Boolean;
	convert:packed record case case5 of
		0:(word:integer;word2:integer);
		1:(lh:0..777777B;rh:0..777777B);
		2:(bits:set of 0..71);
		3:(float:real);
	 	4:(ad: record address:^integer end);
		end;
	comtable:table;
	comfile,comfile2:text;

procedure zerovr; extern;

{### remove CASIZE and HSIZE for Fortran, and FORTPR - FORINI for non-Fortran}

function casize:integer;extern;
function hsize:integer;
  begin
  if extended
    then hsize := casize
    else hsize := casize mod 1000000B
  end;

/*
procedure fortpr(var x:array2;i:integer); extern fortran;
function fortfn(x:array2;i:integer):real; extern fortran;
procedure forini; extern;
*/

function curjfn(var f:file):integer; extern;
procedure setjfn(var f:file;i:integer); extern;
function to6(a:alfa):integer; extern;
procedure from6(i:integer;var a:alfa); extern;

procedure analysis(var f:file); extern;

procedure psidefine(chan,level:integer;procedure p); extern;
procedure psienable(chan:integer); extern;
procedure psidisable(chan:integer); extern;
procedure entercrit; extern;
procedure leavecrit; extern;

FUNCTION POWER(X,Y:REAL):REAL; EXTERN;
FUNCTION IPOWER(I,J:INTEGER):INTEGER;EXTERN;
FUNCTION MPOWER(X:REAL;I:INTEGER):REAL;EXTERN;

procedure putstr(s1:string;i:integer;s2:string;j:integer); extern;
function findnull(s:string):integer; extern;
procedure arrwrite(var f:file;var a:alfa); extern;
procedure arrset(var f:file;var a:alfa); extern;


procedure ok;
  begin
  oks := oks+1
  end;

procedure showov(chan:integer;oldpc:integer;deferred:Boolean;
	var pcflags:array2);
  begin
  writeln(tty,'Chan: ',chan:1,', Oldpc: ',oldpc:12:o,', Deferred: ',
	  deferred:1, ', Flags: ',pcflags[1]:12:o);
  writeln(tty,'       , Oldpc: ',pcflags[2]:12:o);
{Clear the flag so we see the next overflow.  In an actual application
 we would come up with a better way to do this.}
  pcflags[1] := 0;
  writeln(tty,'Please type the string 123');
  readln(tty,buf2);
  if buf2 <> '123       '
    then writeln(tty,'Failure 130')
    else ok;
  if not (chan in [6,7]) or (deferred <> dodefer)
    then writeln(tty,'Failure 131')
    else ok;
  ints := ints+1
  end;  

function getmem(addr:integer):integer;
   var convert: record case Boolean of
	true: (int:integer);
	false: (point:^integer)
	end;
  begin
  convert.int := addr;
  getmem := convert.point^
  end;

function near(a,b:real):Boolean;
  begin
  near := (abs(a-b) / (abs(a) + abs(b))) < 0.001
  end;

procedure checktemp;
{Checks for left-over PAS-INTERNAL files}
  begin
  reset(input,'PAS-INTERNAL.*':*,'/O/I');
  if not eof(input)
    then writeln(tty,'Failure 11')
    else ok
  end;

procedure clreof(var f:file); extern;

function erstat(var f:file):integer; extern;

procedure strtest;
   var localfile:text;
begin
buf := '          ';
strwrite(localfile,buf);
write(localfile,'abc');
if buf <> 'abc       '
  then writeln(tty,'failure 170')
  else ok;
strset(localfile,buf);
get(localfile);
if localfile^ <> 'b'
  then writeln(tty,'failure 171')
  else ok;
getindex(localfile,i);
if (i <> 3) or eof(localfile) or eoln(localfile)
  then writeln(tty,'failure 172')
  else ok;
read(localfile,buf2);
getindex(localfile,i);
if not eof(localfile) or (i <> 12)
  then writeln(tty,'failure 173')
  else ok;
end;

procedure filetest2(spec:str4;mode:integer);
{this is really a part of FILETEST.  It is separated because the user
 is warned to expect errors from FILETEST, and one of the things we
 are testing is whether we can trap error conditions.  Note that we
 test each of the funny options in two ways:  as bits in the flag
 arguments, and as switches}

  begin
  rewrite(tfile,'',spec);
  writeln(tfile,'123 A456');
{format errors}
  reset(tfile,'',spec,0,0,4B);
  read(tfile,i,j);
  if (i <> 123) or not eof(tfile) or (erstat(tfile) <> 600415B)
    then writeln(tty,'Failure 91')
    else ok;
  reset(tfile,'','/f',0,0,mode*10000B);
  read(tfile,i,j);
  if (i <> 123) or not eof(tfile) or (erstat(tfile) <> 600415B)
    then writeln(tty,'Failure 92')
    else ok;
{open errors}
  rewrite(tfile,'foo.bar',spec);
  writeln(tfile,'123 456');
  close(tfile);
  reset(tfile,'foo.non-existent',spec,0,0,10B);
  if not eof(tfile) or (erstat(tfile) <> 600077B)
    then writeln(tty,'Failure 93')
    else ok;
  rewrite(tfile,'foo.bar',spec);
  rewrite(file1,'foo.bar','/o',0,0,mode*10000B);
  if eof(file1) or (erstat(file1) <> 600130B)
    then writeln(tty,'Failure 94',eof(file1),erstat(file1))
    else ok;
{interactive, upper case and see end of line}
  rewrite(tfile,'foo.bar',spec);
  writeln(tfile,'abc');
  reset(tfile,'',true,0,0,mode*10000B + 21B);
  if not eoln(tfile)
    then writeln(tty,'Failure 95');
  readln(tfile);
  read(tfile,ar3);
  if (ar3[1] <> 'A') or (tfile^ <> chr(15B))
    then writeln(tty,'Failure 96')
    else ok;
  reset(tfile,'','/i/e/u',0,0,mode*10000B);
  if not eoln(tfile)
    then writeln(tty,'Failure 97');
  readln(tfile);
  read(tfile,ar3);
  if (ar3[1] <> 'A') or (tfile^ <> chr(15B))
    then writeln(tty,'Failure 98')
    else ok;
{byte size}
  rewrite(tfile,'foo.bar',spec,0,440000000000B);
  writeln(tfile,'ABCDEFGHIJKLMNOP');
  close(tfile);
  reset(file1,'foo.bar',spec);
  if file1^ <> 101B
    then writeln(tty,'Failure 99')
    else ok;
  rewrite(tfile,'foo.bar','/b:36',0,0,mode*10000B);
  writeln(tfile,'ABCDEFGHIJKLMNOP');
  close(tfile);
  reset(file1,'foo.bar',spec);
  if file1^ <> 101B
    then writeln(tty,'Failure 100')
    else ok;
  rewrite(file1,'foo.bar',spec,0,070000000000B);
  write(file1,141B,142B,143B);
  close(file1);
  reset(tfile,'foo.bar',spec);
  read(tfile,str3);
  if str3 <> 'abc'
    then writeln(tty,'Failure 101')
    else ok;
  rewrite(file1,'foo.bar','/b:7',0,0,mode*10000B);
  write(file1,141B,142B,143B);
  close(file1);
  reset(tfile,'foo.bar',spec);
  read(tfile,str3);
  if str3 <> 'abc'
    then writeln(tty,'Failure 102')
    else ok;
  rewrite(tfile,'foo.bar',spec);
  writeln(tfile,'ABCDEFGHIJKLMNOP');
  close(tfile);
  reset(file1,'foo.bar',spec,0,070000000000B);
  if file1^ <> 101B
    then writeln(tty,'Failure 103')
    else ok;
  rewrite(tfile,'foo.bar',spec);
  writeln(tfile,'ABCDEFGHIJKLMNOP');
  close(tfile);
  reset(file1,'foo.bar','/b:7',0,0,mode*10000B);
  if file1^ <> 101B
    then writeln(tty,'Failure 104')
    else ok;
  rewrite(file1,'foo.bar',spec);
  write(file1,141B,142B,143B);
  close(file1);
  reset(tfile,'foo.bar',spec,0,440000000000B);
  read(tfile,str3);
  if str3 <> 'abc'
    then writeln(tty,'Failure 105')
    else ok;
  rewrite(file1,'foo.bar',spec);
  write(file1,141B,142B,143B);
  close(file1);
  reset(tfile,'foo.bar','/b:36',0,0,mode*10000B);
  read(tfile,str3);
  if str3 <> 'abc'
    then writeln(tty,'Failure 106')
    else ok;
{number of buffers}
  rewrite(file1,'foo.bar',spec,0,0,300B);
  rewrite(tfile,'foo.baz',spec,0,0,300B);
  for i := 1 to 2000 do
    begin
    write(file1,i);
    writeln(tfile,i:4);
    end;
  reset(file1,'',spec,0,0,300B);
  reset(tfile,'',spec,0,0,300B);
  for i := 1 to 2000 do
    begin
    read(file1,j);
    if j <> i
      then writeln(tty,'Failure 107')
      else ok;
    read(tfile,j);
    if j <> i
      then writeln(tty,'Failure 108')
      else ok;
    end;    
{specification of GTJFN and OPENF bits}
  close(tfile);
  close(file1);
  rewrite(tfile,'FOO.BAR',spec,1);
  writeln(tfile,654);
  close(tfile);
  reset(tfile,'FOO.BAR',spec,1,2000B); {version 1, thawed}
  read(tfile,i);
  if i <> 654
    then writeln(tty,'Failure 109')
    else ok;
  rewrite(tfile,'FOO.BAR',spec,1,2000B);
  if mode in [5,6]
    then update(file1,'FOO.BAR',0,1,2000B)
    else update(file1,'FOO.BAR',spec,1,2000B);
  writeln(tfile,432);
  close(file1);
  close(tfile);
  reset(tfile,'FOO.BAR.1',spec,0,2000B);
  read(tfile,i);
  if i <> 432
    then writeln(tty,'Failure 110')
    else ok;
  close(tfile);
  end;

procedure filetest(spec:str4;mode:integer);
  begin
  writeln(tty,'>>> ',mode);
{special handling:
	mode 3 (TEXTI) - because it will try to do editing on certain
		characters!
	mode 4 (NULL) - because it will not read anything}
  rewrite(file1,'',spec);
  rewrite(tfile,'',spec);
  for i := 1 to 2000 do
    begin
    write(file1,i);
    if (mode = 3) and (((i mod 200B) in [0,15B,17B,22B,25B,32B]) or
		       ((i mod 200B) = 177B))
      then write(tfile,chr(12B))
    else write(tfile,chr(i mod 200B))
    end;
  reset(file1,'',spec);
  reset(tfile,'',spec);
  if mode = 4  {null}
   then if not (eof(file1) and eof(tfile))
	    then writeln(tty,'failure 53')
	    else ok
   else for i := 1 to 2000 do
    begin
    if file1^ <> i
      then writeln(tty,'failure 54')
      else ok;
    if  not ((i mod 200B) = 0)
      then if (mode = 3) and (((i mod 200B) in [0,12B,14B,15B,17B,22B,25B,32B,
						33B])
			      or ((i mod 200B) = 177B))
	  or (mode <> 3) and ((i mod 200B) in [12B,14B,15B,16B..32B,32B,33B])
        then if (tfile^ <> ' ') or not eoln(tfile)
	  then writeln(tty,'failure 55',i,i mod 200B:3:o,
			   ord(tfile^):3:o,eoln(tfile))
          else ok
        else if (tfile^ <> chr(i mod 200B)) or eoln(tfile)
	  then writeln(tty,'failure 56',i,i mod 200B:3:o,
			   ord(tfile^):3:o,eoln(tfile))
	  else ok;
    get(file1);
    if (mode <> 3) and not ((i mod 200B) in [0,16B..32B])
       or (mode = 3)
      then get(tfile);
    end;
  if not eof(file1) or not eof(tfile) or not eoln(tfile)
    then writeln(tty,'failure 57')
    else ok;


{Now we try the fancy features of Systems Pascal - first CURPOS and
 SETPOS}

if mode = 4  {don't try them for NUL}
  then begin
  update(file1,'',spec);
  update(tfile,'',spec);
  if (curpos(file1) <> 0) or (curpos(tfile) <> 0)
    then writeln(tty,'failure 58')
    else ok;
  setpos(file1,0);
  setpos(tfile,0);
  end
 else if mode = 5  {all mode 5 has is getx - try it}
  then begin
  rewrite(file3,'',spec);
  data3[1] := 1;
  data3[2] := 2;
  data3[3] := 3;
  for i := 1 to 25 do
    begin
    data3[1] := i;
    write(file3,data3);
    end;
  file3^[2] := 123;
  reset(file3,'','/m:5/i');
  get(file3:1);
  if not ((file3^[1] = 1) and (file3^[2] = 0))
    then writeln(tty,'Failure 59',file3^[1],file3^[2],file3^[3])
    else ok;
  getx(file3);
  if file3^[2] <> 2
    then writeln(tty,'Failure 60')
    else ok;
  end  
 else if mode in [1,2,3,6]
  then begin
  if mode in [3,6]
    then begin
    reset(file1,'',spec);
    reset(tfile,'',spec)
    end
   else begin
    update(file1,'',spec);
    update(tfile,'',spec);
    end;
  setpos(file1,3);
  if (file1^ <> 4)
    then writeln(tty,'failure 61')
    else ok;
  if (curpos(file1) <> 4)
    then writeln(tty,'failure 62')
    else ok;
  setpos(tfile,101B);
  if (tfile^ <> 'B')
    then writeln(tty,'failure 63')
    else ok;
  if (curpos(tfile) <> 102B)
    then writeln(tty,'failure 64')
    else ok;

  setpos(file1,5);
  if (file1^ <> 6)
    then writeln(tty,'failure 65')
    else ok;
  if (curpos(file1) <> 6)
    then writeln(tty,'failure 66')
    else ok;
  setpos(tfile,105B);
  if (tfile^ <> 'F')
    then writeln(tty,'failure 67')
    else ok;
  if  (curpos(tfile) <> 106B)
    then writeln(tty,'failure 68')
    else ok;

  setpos(file1,1998,true);
  if curpos(file1) <> 1998
    then writeln(tty,'failure 69')
    else ok;
  get(file1);
  if (file1^ <> 1999)
    then writeln(tty,'failure 70')
    else ok;
  if (curpos(file1) <> 1999)
    then writeln(tty,'failure 71')
    else ok;
  setpos(tfile,1920+10B,true);
  if curpos(tfile) <> (1920+10B)
    then writeln(tty,'failure 72')
    else ok;
  get(tfile);
  if tfile^ <> chr(11B)
    then writeln(tty,'failure 73')
    else ok;
  if  ( curpos(tfile) <> (1920+11B))
    then writeln(tty,'failure 74')
    else ok;
  setpos(file1,3000);
  setpos(tfile,3000);
  if not (eof(file1) and eof(tfile))
    then writeln(tty,'failure 75')
    else ok;
  clreof(file1);
  clreof(tfile);
  setpos(file1,0);
  setpos(tfile,0);
  if not (
    not eof(file1) and
    not eof(tfile) and
    (file1^ = 1) and
    (tfile^ = chr(1)))
    then writeln(tty,'failure 76')
    else ok;

  if mode in [3,6]
    then begin
    rewrite(file1,'',spec);
    rewrite(tfile,'',spec);
    end;

  setpos(file1,5,true);
  setpos(tfile,5,true);
  if (curpos(file1) <> 5) or (curpos(tfile) <> 5)
    then writeln(tty,'failure 77')
    else ok;
  write(file1,999);
  write(tfile,'#$%&');
  if (curpos(file1) <> 6) or (curpos(tfile) <> 9)
    then writeln(tty,'failure 78')
    else ok;
  
  if mode in [3,6]
    then begin
    reset(file1,'',spec);
    reset(tfile,'',spec);
    end;

  setpos(file1,5);
  setpos(tfile,5);
  if (file1^ <> 999) or (tfile^ <> '#')
    then writeln(tty,'Failure 79')
    else ok;

  if mode in [3,6]
    then rewrite(file3,'',spec)
    else update(file3,'',spec);
  data3[1] := 1;
  data3[2] := 2;
  data3[3] := 3;
  for i := 1 to 25 do
    begin
    data3[1] := i;
    write(file3,data3);
    end;
  if mode in [3,6]
    then reset(file3,'',spec);
  file3^[2] := 123;
  setpos(file3,3*5,true);
  get(file3:1);
  if not ((file3^[1] = 6) and (file3^[2] = 0))
    then writeln(tty,'Failure 80',file3^[1],file3^[2],file3^[3])
    else ok;
  getx(file3);
  if file3^[2] <> 2
    then writeln(tty,'Failure 81')
    else ok;
  file3^[1] := 111;
  file3^[2] := 222;
  file3^[3] := 333;
  if not (mode in [3,6])
    then begin
    putx(file3);
    setpos(file3,0);
    if file3^[1] <> 1
      then writeln(tty,'Failure 82')
      else ok;
    setpos(file3,3*5);
    if (file3^[1] <> 111) or (file3^[2] <> 222) or (file3^[3] <> 333)
      then writeln(tty,'Failure 83')
      else ok;
    end;


  end;

  if mode <> 4
    then begin
    writeln(tty);
    writeln(tty,'There should now be exactly one error complaining that');
    writeln(tty,'it didn''t find a digit.  CONTINUE it.');
    rewrite(tfile,'',spec);
    writeln(tfile,'123 A456');
    reset(tfile,'',spec);
    read(tfile,i,j);
    writeln(tty);
    writeln(tty,'The system should now say that it can''t find file FOO.NON-EXISTENT');
    writeln(tty,'Please tell it to use FOO.BAR');
    rewrite(tfile,'foo.bar',spec);
    writeln(tfile,'123 456');
    close(tfile);
    reset(tfile,'foo.non-existent',spec);
    read(tfile,i,j);
    if (i <> 123) or (j <> 456)
      then writeln(tty,'Failure 89')
      else ok;
    close(tfile);
    rewrite(tfile,'foo.bar',spec);
    writeln(tty,'The system should now complain invalid simultaneous access');
    writeln(tty,'for FOO.BAR - please tell it to use FOO.BAZ');
    rewrite(file1,'foo.bar',spec);
    write(file1,789);
    reset(file1,'',spec);
    read(file1,i);
    if i <> 789
      then writeln(tty,'Failure 90')
      else ok;
    close(file1);
    close(tfile);
    end;

  end;


procedure tapetest;
  begin
{This tests mode 7}
  rewrite(file7,'TAPE:;POS:1','/M:7');
  for i := 1 to 100 do
    begin
    data7[1] := i;
    write(file7,data7);
    end;
  reset(file7,'TAPE:;POS:1','/M:7');
  for i := 1 to 100 do
    begin
    if file7^[1] <> i
      then writeln(tty,'Failure 84')
      else ok;
    get(file7)
    end;
  if not eof(file7)
    then writeln(tty,'Failure 85')
    else ok;
  close(file7);
  rewrite(tfile,'TAPE:;POS:2','/M:7');
  for i := 20 to 100 do
    begin
    for j := 1 to i do
      write(tfile,chr(101B + i mod 26));
    writeln(tfile)
    end;
  reset(tfile,'TAPE:;POS:2','/M:7');
  for i := 20 to 100 do
    begin
    for j := 1 to i do
      begin
      if tfile^ <> chr(101B + i mod 26)
	then writeln(tty,'Failure 86')
        else ok;
      get(tfile);
      end;
    if not eoln(tfile)
      then writeln(tty,'Failure 87')
      else ok;
    readln(tfile)
    end;
  if not eof(tfile)
    then writeln(tty,'Failure 88')
    else ok;
  rewrite(tfile,'TAPE:;POS:1','/m:7');
  writeln(tfile,'123 A456':20);
  reset(tfile,'TAPE:;POS:1','/m:7');
  read(tfile,i,j);
  writeln(tty,j);
  end;


procedure p1(fromlevel,tolevel:integer);
  label 1;
  var p,q,oldp,oldo:integer;

  procedure p2x;
    begin
    if (p<>5) or (q <>6)
      then writeln(tty,'Failure 12')
      else ok
    end;

  procedure p2(fromlevel,tolevel:integer;procedure testp);
    label 2;
    var x,y,oldp,oldo:integer;f:text;

    procedure p3(fromlevel,tolevel:integer;procedure testp,testx;i:integer);
      label 3;
      var a,b:integer;
      begin {p3}
      if i <> 123
	then writeln(tty,'Failure 21')
	else ok;
      p2x;
      testp;
      testx;
      a := 1;
      b := 2;
      if (x <> 3) or (y <> 4) or (p <> 5) or (q <> 6)
	then writeln(tty,'Failure 2')
	else ok;
      if fromlevel = 3
	then case tolevel of
	  3: begin ok; goto 3 end;
	  2: begin ok; goto 2 end;
	  1: begin ok; goto 1 end;
	  0: begin ok;  goto 11 end;
	  others: writeln(tty,'Failure 3')
	  end;
3:
      end; {p3}

    begin {p2}
    testp;
    x := 3;
    y := 4;
    rewrite(f);
    writeln(f,'fffggg');
    oldp := getmem(17B);
    oldo := getmem(16B);
    p3(fromlevel,tolevel,testp,p2x,123);
2:  if (oldp <> getmem(17B)) or (oldo <> getmem(16B))
      then writeln(tty,'Failure 4')
      else ok;
    if (p <> 5) or (q <> 6)
      then writeln(tty,'Failure 5')
      else ok;
    reset(f);
    read(f,buf);
    if buf <> 'fffggg    '
      then writeln(tty,'Failure 10')
      else ok;
    if fromlevel = 2
      then case tolevel of
	2: begin ok; goto 666 end;
	1: begin ok; goto 1 end;
	0: begin ok; goto 11 end;
	others: writeln(tty,'Failure 6')
	end;
666:
    end; {p2}

  begin {p1}
  p := 5;
  q := 6;
  oldp := getmem(17B);
  oldo := getmem(16B);
  p2(fromlevel,tolevel,p2x);
1:
  if (oldp <> getmem(17B)) or (oldo <> getmem(16B))
    then writeln(tty,'Failure 7')
    else ok;
  checktemp;
  if fromlevel = 1
    then case tolevel of
      1: begin ok; goto 666 end;
      0: begin ok; goto 11 end;
      others: writeln(tty,'Failure 8')
      end;
666:
  end; {p1}

begin {main}

{Here we check the to make sure the files in records work, including
 the fact that DISPOSE kills the file}

reset(input,'pas-internal.*.*':*,'/o');
if not eof
  then repeat
    delete(input);
   until nextfile(input) = 0;

new(foo);
rewrite(foo^.f);
writeln(foo^.f,'abc');
reset(foo^.f);
read(foo^.f,buf);
if buf <> 'abc       '
  then writeln(tty,'Failure 1')
  else ok;
dispose(foo);
checktemp;

{test fortran linkage}

{ ### Remove the following for non-Fortran}

/*
forini;  {this is a dummy call}
data2[1] := 123;
data2[2] := -345;
fortpr(data2,789);
if (data2[1] <> -1234567) or (data2[2] <> 99999)
  then writeln(tty,'Failure 178')
  else ok;
x := fortfn(data2,123);
if (x <> 456.0) or (data2[1] <> -1234567) or (data2[2] <> 99999)
  then writeln(tty,'Failure 179')
  else ok;
*/

{Now we check to make sure that procedures work, including access to
 variables in outer blocks, and that non-local goto's do the right
 things both to the basis AC's and to files}

oldp := getmem(17B);
oldo := getmem(16B);
for i := 0 to 3 do
  for j := 0 to i do
    begin
    p1(i,j);
11: if (oldp <> getmem(17B)) or (oldo <> getmem(16B))
      then writeln(tty,'Failure 10')
      else ok;
    end;

{Now we test STRWRITE and friends}

buf := '          ';
strwrite(output,buf);
write('abc');
if buf <> 'abc       '
  then writeln(tty,'failure 13',buf)
  else ok;
strset(input,buf);
get(input);
if input^ <> 'b'
  then writeln(tty,'failure 14')
  else ok;
getindex(input,i);
if (i <> 3) or eof(input) or eoln(input)
  then writeln(tty,'failure 15')
  else ok;
read(input,buf2);
getindex(input,i);
if not eof(input) or (i <> 12)
  then writeln(tty,'failure 16')
  else ok;

  {same tests, but using random access}

buf := '          ';
strwrite(output,buf,3);
write('abc');
if buf <> '  abc     '
  then writeln(tty,'failure 17')
  else ok;
strset(input,buf,3,9);
get(input);
if input^ <> 'b'
  then writeln(tty,'failure 18')
  else ok;
getindex(input,i);
if (i <> 5) or eof(input) or eoln(input)
  then writeln(tty,'failure 19',i)
  else ok;
read(input,buf2);
getindex(input,i);
if not eof(input) or (i <> 11)
  then writeln(tty,'failure 20',i)
  else ok;

{Same test but using local files}

strtest;

{Now try UNPACK and PACK.  (Don't ask why I bother testing them.)}

buf := 'abcdefghij';
for i := 1 to 20 do
  unpacked[i] := ' ';
unpack(buf,unpacked,3);
for i := 1 to 2 do
  if unpacked[i] <> ' '
    then writeln(tty,'failure 22')
    else ok;
for i := 3 to 12 do
  if unpacked[i] <> chr(141B + i - 3)
    then writeln(tty,'failure 23')
    else ok;
for i := 13 to 20 do
  if unpacked[i] <> ' '
    then writeln(tty,'failure 24')
    else ok;
buf := '          ';
pack(unpacked,3,buf);
if buf <> 'abcdefghij'
  then writeln(tty,'failure 25')
  else ok;

{Now various string comparisons}

if not(
  ('abc' < 'cde') and
  ('abcde' <= 'cdefh') and
  ('abcdef' <= 'abcdef') and
  ('abcdef' >= 'abcdef') and
  ('abcdefghijk' > 'abcdefghijj') and
  ('abcdefghijk' >= 'abcdefghijk')
)
  then writeln(tty,'failure 26')
  else ok;

{Now we test the built-in functions}

if not(
  (2 = abs(-2)) and
  (2 = abs (2)) and
  (3.1 = abs(-3.1)) and
  (3.1 = abs(3.1)) and
  (9.0 = sqr (3.0)) and
  odd(1) and
  (not odd (2)) and
  (3 = succ(2)) and
  (1 = pred(2)) and
  ('a' = chr(141B)) and
  (141B = ord('a')) and
  (4 = trunc(4.4)) and
  (4 = round(4.4)))
    then writeln(tty,'Failure 27')
    else ok;

if not(
  near(1.0,sin(3.1415916538/2.0)) and
  near(1.0,cos(0.0)) and
  near(2.718281828,exp(1.0)) and
  near(1.0,ln(2.718281828)) and
  near(2.0,sqrt(4.0)) and
  near(3.1415916538/4.0,arctan(1.0)) and
  near(3.1415916538/2.0,arcsin(1.0)) and
  near(1.0,1.0+arccos(1.0)))
    then writeln(tty,'Failure 28')
    else ok;

if not(
  near(1.175201,sinh(1.0)) and
  near(1.54308, cosh(1.0)) and
  near(0.761594,tanh(1.0)) and
  near(1.0, sind(90.0)) and
  near(1.0, cosd(0.0)) and
  near(2.0, log(100.0)))
    then writeln(tty,'Failure 29')
    else ok;

if power(2.0,2.0) <> 4.0
  then writeln(tty,'Failure 29.1')
  else ok;
if ipower(2,3) <> 8
  then writeln(tty,'Failure 29.2')
  else ok;
if mpower(3.0,2) <> 9.0
  then writeln(tty,'Failure 29.3')
  else ok;

strwrite(output,buf);
buf := '          ';
write(123b:3:o,123b:4:o,123b:2:o);
if buf <> '123012323 '
  then writeln(tty,'Failure 30')
  else ok;

strwrite(output,buf);
buf := '          ';
write("123:3:h,"123:4:h,"123:2:h);
if buf <> '123012323 '
  then writeln(tty,'Failure 31')
  else ok;

strwrite(output,buf);
buf := '          ';
write(123:3,123:4,123:2);
if buf <> '123 123123'
  then writeln(tty,'Failure 32')
  else ok;

strwrite(output,longbuf);
longbuf := '                                                ';
write(123.0,123.0:6,123.0:2,123.0:7:2,123.0:2:2);
if longbuf <> '    1.230000E+02  1.2E+02  1.2E+02 123.00 123.00'
  then writeln(tty,'Failure 33')
  else ok;

strwrite(output,buf);
buf := '          ';
write('A':1,'A','A':0,'A':3);
if buf <> 'AAA  A    '
  then writeln(tty,'Failure 34')
  else ok;

strwrite(output,buf);
buf := '          ';
write('ABC':2,'ABC':3,'ABC':4);
if buf <> 'ABABC ABC '
  then writeln(tty,'Failure 35')
  else ok;

strwrite(output,buf);
buf := '          ';
ar3[1] := 'A'; ar3[2] := 'B'; ar3[3] := 'C';
write(ar3:2,ar3:3,ar3:4);
if buf <> 'ABABC ABC '
  then writeln(tty,'Failure 36')
  else ok;

strwrite(output,longbuf);
longbuf := '                                                ';
write(true:0,true:1,true:2,true:4,true:6);
if longbuf <> 'truetruetruetrue  true                          '
  then writeln(tty,'Failure 37')
  else ok;

buf := '13X13.0   ';
strset(input,buf);
read(i,ch,x);
if (i <> 13) or (ch <> 'X') or (x <> 13.0)
  then writeln(tty,'Failure 38')
  else ok;

buf := 'ABCDEFGHIJ';
strset(input,buf);
read(buf2);
if buf2 <> 'ABCDEFGHIJ'
  then writeln(tty,'Failure 39')
  else ok;

buf := 'ABCDEFGHIJ';
strset(input,buf,2);
read(buf2);
if buf2 <> 'BCDEFGHIJ '
  then writeln(tty,'Failure 40')
  else ok;

longbuf := 'ABCDEFGHIJ                                      ';
strset(input,longbuf);
read(buf2);
if buf2 <> 'ABCDEFGHIJ'
  then writeln(tty,'Failure 41')
  else ok;

buf := 'ABCDEFGHIJ';
strset(input,buf);
read(ar3);
if not ((ar3[1] = 'A') and (ar3[2] = 'B') and (ar3[3] = 'C'))
  then writeln(tty,'Failure 42')
  else ok;

buf := 'ABCDEFGHIJ';
strset(input,buf,9);
read(ar3);
if not ((ar3[1] = 'I') and (ar3[2] = 'J') and (ar3[3] = ' '))
  then writeln(tty,'Failure 43')
  else ok;

data2[2] := 0;
rewrite(file1);
rewrite(file2);
rewrite(file3);
for i := 1 to 1000 do
  begin
  write(file1,i);
  data2[1] := i;
  write(file2,data2);
  data3[1] := i;
  write(file3,data3)
  end;

reset(file1);
reset(file2);
reset(file3);
for i := 1 to 1000 do
  begin
  read(file1,data1);
  if data1 <> i
    then writeln(tty,'Failure 44')
    else ok;
  read(file2,data2);
  if (data2[1] <> i) or (data2[2] <> 0)
    then writeln(tty,'Failure 45')
    else ok;
  read(file3,data3);
  if (data3[1] <> i) or (data3[2] <> 0) or (data3[3] <> 0)
    then writeln(tty,'Failure 46')
    else ok;
  end;

{Now check the heap}

new(point20);

{ ### Remove the tests for HSIZE when used with Fortran}

extended := hsize > 1000;
if extended
  then writeln(tty,'For purposes of heap tests, assuming extended addressing')
  else writeln(tty,'For purposes of heap tests, assuming normal addressing');
if extended
  then i := 1000000B - 23
  else i := 1000B - 7 - 22;
if hsize <> i
  then writeln(tty,'Failure 47 ')
  else ok;

new(point20a);
new(point20b);

if extended
  then i := i - 46
  else i := i - 44;
if hsize <> i
  then writeln(tty,'Failure 48')
  else ok;

new(point513);

if extended
  then i := i - 516
  else i := i + 1024 - 515;
if hsize <> i
  then writeln(tty,'Failure 49 ')
  else ok;
if extended
  then begin
  new(point128000);
  new(pointa128000);
  new(pointb128000);
  i := i + 1000000B - 3*(128003);
  if hsize <> i
    then writeln(tty,'Failure 50 ')
    else ok;
  dispose(pointa128000);
  dispose(pointb128000);
  dispose(point128000);
  i := i + 3*(128003);
  if hsize <> i
    then writeln(tty,'Failure 51')
    else ok
  end
  else begin ok; ok end;

dispose(point20a);
dispose(point513);
dispose(point20);
dispose(point20b);

i := i + 573;
if extended 
  then i := i + 4 * 3
  else i := i + 4 * 2;
if hsize <> i
  then writeln(tty,'Failure 52 ')
  else ok;

{Now test file functions in the various modes}

writeln(tty);
writeln(tty,'WARNING:  We are about to test all 7 possible file modes.');
writeln(tty,'For each mode, we will try generating various errors.  We');
writeln(tty,'will tell you immediately before each error.  Any errors');
writeln(tty,'that you are not warned about, or any warnings that are not');
writeln(tty,'followed by errors, should be regarded as failures.');
writeln(tty);

filetest('/m:1',1);

filetest('/m:2',2);

filetest('/m:3',3);

filetest('/m:4',4);

filetest('/m:5',5);

filetest('/m:6',6);

writeln(tty,'please type something other than a number, to check error');
writeln(tty,'handling on the terminal');
write(tty,'>');
readln(tty,i);
writeln(tty,'the final number read was ',i);

write(tty,'Do you want to do magtape tests?');
readln(tty,ch);
if ch in ['Y','y']
  then begin
  writeln(tty,'MOUNT TAPE TAPE:/WRITE/LABEL:TOPS');
  writeln(tty,'Hit CR when ready');
  readln(tty);
  tapetest
  end;

writeln(tty);
writeln(tty,'We are now going to try the same errors, but we are going to');
writeln(tty,'trap them.  If you see any error messages from here on, they');
writeln(tty,'should be regarded as failures.');

writeln(tty,1);
filetest2('/m:1',1);

writeln(tty,2);
filetest2('/m:2',2);

writeln(tty,3);
filetest2('/m:3',3);

writeln(tty,5);
filetest2('/m:5',5);

writeln(tty,6);
filetest2('/m:6',6);

rewrite(output,'foo.bar.1');
writeln;
close(output);

reset(input,'foo.bar.*':*,'/i/o');
if eof then
  writeln(tty,'Failure 111')
  else ok;
repeat
  delete(input);
 until nextfile(input) = 0;
reset(input,'foo.bar.*':*,'/o');
if not eof then  {this time there shouldn't be any left}
  writeln(tty,'Failure 112')
  else ok;
writeln(tty,'You should now get an error message complaining that FOO.BAR is not present');
analysis(input);
reset(input,'foo.baz','/o');
if eof then
  writeln(tty,'Failure 113')
  else ok;
rename(input,'foo.bar');
reset(input,'foo.bar','/o');
if eof then
  writeln(tty,'Failure 114')
  else ok;
delete(input);
{At this point there should be no FOO.BAR.}
rewrite(output,'foo.bar');
writeln(output,'abcdef');
dismiss(output);  {this should not create the output}
reset(input,'foo.bar','/o');
if not eof then
  writeln(tty,'Failure 115')
  else ok;
rewrite(output,'foo.bar');
writeln(output,'abcdef');
convert.word := curjfn(output);
if (convert.rh <= 0) or (convert.rh >= 64) then
  {make sure we can see the jfn}
  writeln(tty,'Failure 116',curjfn(output))
  else ok;
rclose(output);  {this one should}
reset(input,'foo.bar','/o');
if eof then
  writeln(tty,'Failure 117')
  else ok;
if curjfn(output) <> 0 {but JFN should go away}
  then writeln(tty,'Failure 118')
  else ok;
append(output,'foo.bar');
writeln(output,'second');
rclose(output);
reset(input,'foo.bar');
read(input,buf:i);
if (i <> 6) or (buf <> 'abcdef    ') or eof or not eoln
  then writeln(tty,'Failure 119',i,buf,eof,eoln)
  else ok;
readln; readln(input,buf:i);
if (i <> 6) or (buf <> 'second    ') or not eof or not eoln
  then writeln(tty,'Failure 120')
  else ok;
delete(input);

{now we test JSYS's}

{normal multiple returns, simple args, string args and values}
buf := '123       ';
jsys(225B{nin},1,ret;buf,0,10;i,i);
if (ret <> 2) or (i <> 123)
  then writeln(tty,'Failure 121',ret,i)
  else ok;
buf := 'a123      ';
jsys(225B{nin},1,ret;buf,0,10;i,i);
if (ret <> 1)
  then writeln(tty,'Failure 121',ret)
  else ok;

{simple calls}
jsys(13B{gjinf};;i,j,j,j);  {i - user number; j - terminal no}
convert.word := i;
if (convert.lh <> 500000B) or (j <= 1) or (j >= 200)
  then writeln(tty,'Failure 122',i:12:o,j:12:o)
  else ok;

{array}
jsys(507B{getji},1,ret;-1,-2:data2,1);
if (ret <> 2) or (data2[1] <> j) or (data2[2] <> i)
  then writeln(tty,'Failure 123',ret,data2[1],data2[2])
  else ok;

{record}
jsys(507B{getji},1,ret;-1,-2:convert,1);
if (ret <> 2) or (convert.word <> j) or (convert.word2 <> i)
  then writeln(tty,'Failure 124',ret,convert.word,convert.word2)
  else ok;

{bits}
buf := '1.0       ';
jsys(234B{DFIN},1,ret;buf;i,x,j);
if (ret <> 2) or (x <> 1.0) or (j <> 0)
  then writeln(tty,'Failure 125',ret,x,j)
  else ok;
jsys(234B,1,ret;buf;i,convert.bits);
if (ret <> 2) or (convert.float <> 1.0)
  then writeln(tty,'Failure 126',ret,convert.float)
  else ok;
buf := '          ';
jsys(233B{flout},1,ret;buf,1.0,0);
if (ret <> 2) or (buf[1] <> '1') or (buf[2] <> chr(0))
  then writeln(tty,'Failure 127',ret,buf)
  else ok;
convert.float := 1.0;
jsys(233B{flout},1,ret;buf,convert.bits,0);
if (ret <> 2) or (buf[1] <> '1') or (buf[2] <> chr(0))
  then writeln(tty,'Failure 128',ret,buf)
  else ok;

{erjmp}
jsys(50B{bin},-2,ret;128);
if (ret <> 3)
  then writeln(tty,'Failure 129',ret);

{Now we test interrupt processing}

writeln(tty,'We are now going to check overflow testing.  Please continue');
writeln(tty,'each.  We will tell you before each one what kind it will be');
writeln(tty);
writeln(tty,'Integer overflow');
i := 377777777777B * 377777777777B;
writeln(tty,'Integer divide check');
i := 1 div 0;
writeln(tty,'Floating overflow');
x := 1E38 * 1E38;
writeln(tty,'Floating divide check');
x := 1.0 / 0.0;
writeln(tty,'Floating underflow');
x := 1E-37 * 1E-37;
psidisable(-1);			{disable the default handling}

psidefine(6,1,showov);		{show overflow}
psidefine(7,1,showov);		{show overflow}
psienable(6);			{turn on those interrupts}
psienable(7);			{turn on those interrupts}
writeln(tty,'You should now be told of an integer overflow and a real overflow');
dodefer := false;
ints := 0;
i := 1 div 0;
if ints <> 1
  then writeln(tty,'Failure 132')
  else ok;
x := 1.0 / 0.0;
if ints <> 2
  then writeln(tty,'Failure 133')
  else ok;
writeln(tty,'You should now be told of a deferred integer overflow');
entercrit;
i := 1 div 0;
if ints <> 2
  then writeln(tty,'Failure 134')
  else ok;
dodefer := true;
leavecrit;
if ints <> 3
  then writeln(tty,'Failure 135')
  else ok;
writeln(tty,'Please type the string 456');
read(tty,buf);		{check TTY I/O}
if (buf <> '456       ') or not eoln(tty)
  then writeln(tty,'Failure 136',buf,eoln(tty))
  else ok;
readln(tty);

psidefine(-1,1,zerovr);
psienable(-1);

i := 1 div 0;
if i <> 0
  then writeln(tty,'Failure 136.1')
  else ok;
x := 1.0 / 0.0;
if x <> 0.0
  then writeln(tty,'Failure 136.2')
  else ok;

psidisable(-1);

{The COMND jsys package}

rewrite(output,'COM-TEST.TEST');
writeln('ONE /TWO');
writeln('COM-TEST.FOO');
writeln('COM-TEST.FOO');
writeln('COM-TEST');
writeln('123 345 234XX');
writeln('111AB');
writeln('1.0 (FOO), := PS:<SUBSYS> PS:<SUBSYS*>');
writeln('OPERATOR TTY40: 1-jan-82 0');
writeln('1-jan-82 0:00:12  1-jan-82 0:00:13');
writeln('ABC XYZ PQR');
writeln('"JUNK" SYSPROG GREEN::');
writeln('X^Y');
writeln('ERROR');
writeln('ERROR');
writeln('ONE');
writeln('ERROR');
writeln('ONE');
writeln('ERROR');
writeln('TWO');
writeln;
writeln('123');
writeln('234.0');

close(output);
reset(input,'COM-TEST.TEST','/m:1/i');
convert.word := curjfn(input);
convert.lh := convert.rh;
convert.rh := 377777B;
newioj := convert.word;

comtable := tbmak(3);
tbadd(comtable,1,'ONE',0);
tbadd(comtable,2,'TWO',0);
tbadd(comtable,3,'X^Y',0);

cmini('');
i := cmioj(newioj);
convert.word := cmstat+1;
if (convert.ad.address^ <> newioj) or (cmkey(comtable) <> 1) or
   (cmswi(comtable) <> 2)
  then writeln(tty,'Failure 137')
  else ok;
cmcfm;

cmini('');
i := cmioj(newioj);
cmofi(comfile);
cmcfm;
rewrite(comfile);
writeln(comfile,'COMFILE LINE');
close(comfile);

cmini('');
i := cmioj(newioj);
cmifi(comfile2);
cmcfm;
reset(comfile2);
read(comfile2,buf);
if buf <> 'COMFILE LI'
  then writeln(tty,'Failure 138')
  else ok;
close(comfile2);

cmini('');
i := cmioj(newioj);
buf := '          ';
gjgen(100000000000B);
gjext('FOO');
cmfil(comfile);
cmcfm;
reset(comfile);
read(comfile,buf);
if buf <> 'COMFILE LI'
  then writeln(tty,'Failure 139')
  else ok;
close(comfile);

cmini('');
i := cmioj(newioj);
if (cmnum <> 123) or (cmnum8 <> 345B) or (cmnux <> 234)
  then writeln(tty,'Failure 140')
  else ok;
i := cmtxt(buf);
cmcfm;
cmini('');
i := cmioj(newioj);
if (cmnux8 <> 111B)
  then writeln(tty,'Failure 141')
  else ok;
i := cmtxt(buf);
cmcfm;

cmini('');
i := cmioj(newioj);
if cmflt <> 1.0
  then writeln(tty,'Failure 142')
  else ok;
cmnoi('FOO');
cmcma;
cmtok(':=');
if (cmdir <> 540000000003B) or (cmdirw <> 540000000003B)
  then writeln(tty,'Failure 143')
  else ok;
cmcfm;

cmini('');
i := cmioj(newioj);
if (cmusr <> 500000000005B) or (cmdev <> 600012000040B) or
   (cmtad <> 127652152525B)
  then writeln(tty,'Failure 144')
  else ok;
cmcfm;

cmini('');
i := cmioj(newioj);
cmtadn(thistime);
if (thistime.year <> 1982) or (thistime.month <> 0) or (thistime.seconds <> 12)
  then writeln(tty,'Failure 145')
  else ok;
thistime.year := 1; thistime.month := 2; thistime.seconds := 3;
cmdn(thistime);
if (thistime.year <> 1982) or (thistime.month <> 0)
  then writeln(tty,'Failure 146')
  else ok;
cmtn(thistime);
if (thistime.seconds <> 13)
  then writeln(tty,'Failure 147')
  else ok;
i := cmatom(buf);
if (i <> 7) or (buf <> '0:00:13   ')
  then writeln(tty,'Failure 148')
  else ok;
cmcfm;

cmini('');
i := cmioj(newioj);
i := cmfld(buf);
if (i <> 3) or (buf <> 'ABC       ')
  then writeln('Failure 149')
  else ok;
i := cmtxt(buf);
if (i <> 7) or (buf <> 'XYZ PQR   ')
  then writeln('Failure 150')
  else ok;
cmcfm;

cmini('');
i := cmioj(newioj);
i := cmqst(buf);
if (i <> 4) or (buf <> 'JUNK      ')
  then writeln(tty,'Failure 151')
  else ok;
i := cmact(buf);
if (i <> 7) or (buf <> 'SYSPROG   ')
  then writeln(tty,'Failure 152')
  else ok;
{i := cmnod(buf);  our system doesn't have decnet
if (i <> 5) or (buf <> 'GREEN     ')
  then writeln(tty,'Failure 153')
  else ok;}
i := cmtxt(buf);  {this is in place of CMNOD}
cmcfm;

cmini('');
i := cmioj(newioj);
brini(break,fldb0,fldb1,fldb2,fldb3);
brmsk(break,'^^','');
cmbrk(break);
if (cmkey(comtable) <> 3)
  then writeln(tty,'Failure 154')
  else ok;
cmcfm;

cmini('');
cmauto(false);
i := cmioj(newioj);
i := cmkey(comtable);
if not cmerr
  then writeln(tty,'Failure 155')
  else ok;
i := cmfld(buf);
if cmerr or (buf <> 'ERROR     ')
  then writeln(tty,'Failure 156')
  else ok;
cmcfm;

writeln(tty,'You will now get "? Does not match switch or keyword - ERROR"');
cmini('');
i := cmioj(newioj);
if cmkey(comtable) <> 1
  then writeln(tty,'Failure 157')
  else ok;
cmcfm;

cmini('');
cmauto(false);
i := cmioj(newioj);
i := cmkey(comtable);
if cmerr
  then cmagain
  else if i <> 1
    then writeln(tty,'Failure 158')
    else ok;
cmcfm;

writeln(tty,'You will now get "? Funny error"');
cmini('');
cmauto(false);
i := cmioj(newioj);
i := cmkey(comtable);
if cmerr
  then cmuerr(' Funny error')
  else if i <> 2
    then writeln(tty,'Failure 159')
    else ok;
cmcfm;

writeln(tty);
writeln(tty,'You should now get some error message, possibly "? Device is not a terminal"');
cmerrmsg;

cmini('');
cmdef('FOOBAR');
i := cmioj(newioj);
i := cmfld(buf);
if buf <> 'FOOBAR    '
  then writeln(tty,'Failure 160')
  else ok;
cmcfm;

cmini('');
i := cmioj(newioj);
cmmult;
i := cmkey(comtable);
i := cmnum;
if (cmdo <> 2) or (cmint <> 123)
  then writeln(tty,'Failure 161')
  else ok;
cmcfm;

cmini('');
i := cmioj(newioj);
cmmult;
i := cmkey(comtable);
x := cmflt;
if (cmdo <> 2) or (cmreal <> 234.0)
  then writeln(tty,'Failure 162')
  else ok;
cmcfm;

cmini('');
i := cmioj(newioj);
if not cmeof(true)
  then begin
  i := cmnum;
  writeln(tty,'Failure 163')
  end;

writeln(tty,'The help message for the following should be');
writeln(tty,'  Please type the number');
writeln(tty,'  123');
writeln(tty,'Please try ? to make sure, and then type 123');
cmini('TEST>');
cmhlp('Please type the number');
cmhlp('123');
if (cmnum <> 123) or (cmmode <> normal)
  then writeln(tty,'Failure 164')
  else ok;

buf := 'TEST one  ';
buf[9] := chr(12B);
buf[10] := chr(0);
jsys(500B,1,ret;buf);
cmfnir('',004000000000B); {raise}
if cmmode <> rescan
  then writeln(tty,'Failure 165')
  else ok;
cmtok('ONE');
cmrscn;
if cmmode <> normal
  then writeln(tty,'Failure 166')
  else ok;

thisdate := date;
writeln(tty,'Today''s date is ',thisdate);
i := time;
j := i div (60*60*1000);
i := i mod (60*60*1000);
write(tty,'Today''s time is ',j);
j := i div (60*1000);
i := i mod (60*1000);
write(tty,':',j:2);
j := i div 1000;
writeln(tty,':',j:2);

i := runtime;
j := i div (60*60*1000);
i := i mod (60*60*1000);
write(tty,'Your runtime is ',j);
j := i div (60*1000);
i := i mod (60*1000);
write(tty,':',j:2);
j := i div 1000;
writeln(tty,':',j:2);

i := 414243444546B;
from6(i,buf);
if buf <> 'ABCDEF    '
  then writeln(tty,'Failure 167')
  else ok;
if to6('ABCDEF    ') <> i
  then writeln(tty,'Failure 168')
  else ok;

setjfn(output,789);
if curjfn(output) <> 789
  then writeln(tty,'Failure 169')
  else ok;

buf := 'abcdefghij';
buf2 := 'XYZABxxxxx';
putstr(buf2,3,buf,1);
if buf <> 'XYZdefghij'
  then writeln(tty,'Failure 174',buf)
  else ok;

buf[5] := chr(0);
if findnull(buf) <> 5
  then writeln(tty,'Failure 175',findnull(buf))
  else ok;


arrwrite(output,buf);
write('pqr');
if (buf[1] <> 'p') or (buf[2] <> 'q') or (buf[3] <> 'r') or (buf[4] <> chr(0))
  then writeln(tty,'Failure 176',buf)
  else ok;
arrset(input,buf);
read(input,buf2);
if (buf2 <> 'pqr       ') or not eof
  then writeln(tty,'Failure 177',buf2,eof)
  else ok;

{179 is last used error}

writeln(tty,'Number of OK''s:',oks);


end.
