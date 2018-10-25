{This program was written for our C.E.'s to try and pull out problems
 with a disk pack on line.  It is similar to the SEKTST diagnostic.
 It seeks to cylinder 0 and somewhere near the last cylinder (for an
 RP06 - you will have to change the 240000 to something else for
 a different drive.  It is the block number (128-word blocks) of
 a block in the last cylinder.  It just seeks back and forth between
 these two blocks as fast as possible.}
var
  block:array[0:511]of integer;
  c,u,a,w:integer;
procedure rdblock(c,u,a:integer); 
    var addr:packed record case boolean of
	true:(word:integer);
	false:(atype:0..3B;chan:0..37B;unit:0..77B;bnum:0..37777777B)
	end;
  begin
  addr.atype := 1;
  addr.chan := c;
  addr.unit := u;
  addr.bnum := a;
  jsys(242B;addr.word,128,block);
  end;

function octread:integer;
    var i:integer;
  begin
1:
  i := 0;
  while (tty^ >= '0') and (tty^ <= '9') do
    if tty^ > '7'
      then begin
      writeln(tty, 'Octal number required');
      readln(tty);
      goto 1
      end
    else begin
    i := i*8 + (ord(tty^)-60B);
    get(tty)
    end;
  octread := i
  end;

begin
write(tty,'Chan: ');  readln(tty); c := octread;
write(tty,'Unit: ');  readln(tty); u := octread;
while true do
  begin
  a := 0;
  rdblock(c,u,a);
  a := 240000;
  rdblock(c,u,a);
  end
end.
