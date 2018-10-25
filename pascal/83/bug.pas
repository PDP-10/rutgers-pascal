program test;

procedure foo (a: integer);

    var
	b: integer;

    begin   (* foo *)
	b := a - 1;
	writeLn (Tty, 'a is ', a : 0, ' and b is ', b : 0);
    end;    (* foo *)

begin	(* test *)
    foo (2);
    foo (5);
end.	(* test *)
