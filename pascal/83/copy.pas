program copy(input:-,output:+);
var buffer:packed array[1..100]of char;
	len:integer;
  begin
  loop
    write('next: ');
   exit if eof;
    readln(buffer:len);
    writeln(buffer:len);
  end;
  end.
