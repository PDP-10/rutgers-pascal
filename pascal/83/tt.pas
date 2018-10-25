program copy;
var buffer:packed array[1..100]of char;
	len:integer;
  begin
  loop
    writeln(tty,'foo');
    write(tty,'next: ');
   exit if eof(tty);
    readln(tty,buffer:len);
    writeln(tty,buffer:len);
  end;
  end.
