program test1;
  type pa = array[5..10]of packed array[1..6] of char;
       ua = array[5..10]of packed array[5..10] of integer;
  var i,j:pa; {array[5..10]of packed array[5..10] of char;}
      p,q:ua; {array[5..10]of packed array[5..10] of integer;}
      par:packed array[1..6]of char;
      uar,uar2:array[5..10]of char;

  procedure bar (var p:packed array[i..j:integer] of char;
		 var q,r:array[k..l:integer]of char);
    begin
    pack(q,5,p);
    unpack(p,r,5);
    end;

  procedure foo (var a,b:array[i..j:integer] of 
		         packed array [k..l:integer] of char;
		 var c,d:array[m..n:integer;o..p:integer] of integer);
    procedure foo2;
    begin
      uar[5] := 'A';
      uar[6] := 'B';
      uar[7] := 'C';
      uar[8] := 'D';
      uar[9] := 'E';
      uar[10] := 'F';
      bar(par,uar,uar2);
      if (uar2[5] <> 'A') or
         (uar2[10] <> 'F') then
        writeln(tty,108);
      if par <> 'ABCDEF' then writeln(tty,107);
      
      if a[5] <> b[5] then writeln(tty,1);
      if a[7] = b[7] then writeln(tty,2,a[7],b[7]);
      b := a;
      d[7] := c[7];
      a[7,3] := succ(a[7,3]);
      c[7,7] := succ(c[7,7]);
      a[i,l] := succ(a[i,l]);
      strwrite(output,a[10]);
      write('ABCDEF');
      a[9] := '      ';
      strwrite(output,a[9],2);
      write('ABCDE');
      a[8] := '      ';
      strwrite(output,a[8],2,5);
      write('ABCDEF');
    end;
    begin
    foo2
    end;

    begin
      i[7,3] := 'X';
      p[7,7] := 123;
      foo(i,j,p,q);
      if i[7,3] <> 'Y' then writeln(tty,100,i[7,3]);
      if p[7,7] <> 124 then writeln(tty,101,p[7,7]);
      if j[7,3] <> 'X' then writeln(tty,102);
      if q[7,7] <> 123 then writeln(tty,103);
      if i[10] <> 'ABCDEF' then writeln(tty,104,i[10]);
      if i[9]  <> ' ABCDE' then writeln(tty,105,i[9]);
      if i[8]  <> ' ABCD ' then writeln(tty,106,i[8]);
    end
.	  
