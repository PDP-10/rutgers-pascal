File 1)	DSKP:PFORM.PAS[10,5,PASNEW]         	created: 0203 25-Jul-1981
File 2)	DSKP:PFORM.PAS[10,5,PASNEW,PROD,NEW]	created: 1426 24-Feb-1983

1)5	    include 'p:pascmd.pas','p:string.pas';
1)	const
****
2)5	    include 'p:pascmd.pas';
2)	const
**************
1)8	    Firstdef('SETRAN    ');
1)	    definitions := 0;
****
2)8	    Firstdef('SETRAN    '); Firstdef('APPEND    ');
2)	    Firstdef('CALLI     '); Firstdef('DISMISS   ');
2)	    Firstdef('GETLINER  '); Firstdef('GETINDEX  ');
2)	    Firstdef('BREAKIN   '); Firstdef('DELETE    ');
2)	    Firstdef('RENAME    '); Firstdef('RECSIZE   ');
2)	    Firstdef('MARK       '); Firstdef('PUTX      ');
2)	    Firstdef('RCLOSE    '); Firstdef('RELEASE   ');
2)	    Firstdef('SETPOS    '); Firstdef('STRSET    ');
2)	    Firstdef('STRWRITE  '); Firstdef('CLOSE     ');
2)	    Firstdef('UPDATE    ');
2)	    definitions := 0;
**************
1)13		while (ch in ['/','_','(',' ','$','?','@','%',backslash,'!','{']) and not eob do begin
1)		    case ch of
****
2)13		while (ch in ['/','_','(',' ','$','?','@','%',backslash,'!','{','|','`','~']) and not eob do begin
2)		    case ch of
**************
    