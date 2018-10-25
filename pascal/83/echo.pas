(* MRC:<PUP>ECHO.PAS.22,  7-Jan-83 23:22:12, Edit by ADMIN.LOUGHEED *)
(* For SCORE PASCAL compiler - make TEST_DATA a variable initialized
(*  at runtime *)
(* BX:<PUP>ECHO.PAS.21,  1-Jan-83 00:18:52, Edit by B.BOMBADIL *)
(*  PUPI_BITS is an octal constant *)
(* BX:<PUP>ECHO.PAS.20,  3-Dec-82 20:51:51, Edit by K.KRONJ *)
(* parse <num>#<num>#<num> as well as text site name *)
(* <PUP>ECHO.PAS.19,  3-Oct-82 18:39:13, Edit by B.BOMBADIL *)
(* don't include TCB byte when looking at pup type byte *)
(* wait only 20ms between successive PUPO% and/or PUPI% operations *) 

program echo;
    (* shower an Ethernet host with EchoMe PUPs *)

    include 'pascmd.pas';

    const

	misc_serv = 4;	    (* PUP socket for address lookup *)
	echo_port = 5;

	(* various standard PUP types *)
	echo_me = 1;		echo_pup = 2;		bad_echo = 3;
	error_pup = 4;		lookup_request = 220b;	lookup_reply = 221b;
	lookup_error = 222b;

	(* random jsyses used *)
	getab = 10b;	erstr = 11b;	sysgt = 16b;	gtjfn = 20b;
	openf = 21b;	closf = 22b;	gdsts = 145b;	disms = 167b;
	haltf = 170b;	cvskt = 275b;	esout = 313b;

	(* PUP jsyses (don't use PUPNM) *)
	pupi  = 441b;	pupo  = 442b;

	(* constants for jsys use *)
	priou = 101b;	fhslf = 400000b;

	(* data length constants *)
	min_pup   = 22;	    (* 22 8-bit bytes in a short pup *)
	max_pup   = 554;    (* 554 8-bit bytes in a large pup *)
	max_words = 138;    (* (max_pup - 1) div 4 *)
	asciz_len = 39;	    (* index of last char in asciz string *)

    type
	halfword = 0..777777b;
	byte = 0..377b;

	word = packed record
	    case integer of
		1: (full: integer);
		2: (a: byte; b: byte; c: byte; d: byte);
		3: (left: halfword; right: halfword);
	end;	(* word *)

	address   = ^addrec;
	addrec    = packed record
	    host, net: halfword;    (* reverse order for some reason *)
	    socket:    integer;
	end;	(* addrec *)

	long_pup  = ^l_pup_rec;
	l_pup_rec = packed array [0..max_words] of word;

	asciz     = packed array [0..asciz_len] of char;

    var
	test_data: asciz;
	site: address;
	pup_jfn, index: integer;

(* word manipulation *)

    function combine (first, second: integer): integer;

	begin
	    combine := first * 400b + second;
	end;

    function left_8 (num: integer): integer;

	begin
	    left_8 := (num div 400b) mod 400b;
	end;

    function right_8 (num: integer): integer;

	begin
	    right_8 := num mod 400b;
	end;

    function left_16 (num: integer): integer;

	begin
	    left_16 := (num div 200000b) mod 200000b;
	end;

    function right_16 (num: integer): integer;

	begin
	    right_16 := num mod 200000b;
	end;

(* simple jsys calls *)

    procedure dismiss (msecs: integer);

	begin
	    jsys (disms; msecs);
	end;

    procedure die;

	const
	    dead_msg = 'Can''t continue';   (* must not be multiple of 5 *)

	begin
	    while true do begin
		jsys (haltf);
		jsys (esout; dead_msg);
	    end;
	end;

    procedure js_err;

	const
	    header = 'JSYS error - ';	(* must not be multiple of 5 *)
	    err_err = 'Error within an error';

	var
	    retskp: integer;

	begin
	    jsys (esout; header);
	    jsys (erstr, 2, retskp; priou, fhslf:-1, 0);
	    if retskp <> 3 then
		jsys (esout; err_err);
	    die;
	end;

    (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
    (* socket := get_socket (jfn)			       *)
    (*   returns the local socket number of the given pup jfn  *)
    (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

    function get_socket (jfn: integer): integer;

	var
	    socket: integer;
	    ignore, retskp: integer;

	begin
	    jsys (cvskt, 1, retskp; jfn; ignore, ignore, socket);
	    if retskp = 1 then begin
		writeln (tty, '?Couldn''t get port for JFN ', jfn:0);
		js_err;
	    end;    (* if *)
	    get_socket := socket;
	end;    (* get_socket *)

    (* * * * * * * * * * * * * * * * * * * * * * * *)
    (* address := destination (jfn)		   *)
    (*   returns the foreign port to which the jfn *)
    (*   should be sent				   *)
    (* * * * * * * * * * * * * * * * * * * * * * * *)

    function destination (jfn: integer): address;

	var
	    addr: address;

	begin
	    new (addr);
	    jsys (gdsts; jfn, 0, 2:addr);
	    destination := addr;
	end;

    (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
    (* address := owner_of (jfn)				 *)
    (*   returns the local net, host, and socket for the given	 *)
    (*   pup jfn.  useful for setting outgoing pup destinations. *)
    (*   returns nil if destination is inaccessible.           . *)
    (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

    function owner_of (jfn: integer): address;

	var
	    addr, dest: address;

	function our_net_address (dest: address): address;

	    const
		puprou = 606560625765b;

	    var
		addr: address;
		done: boolean;
		tab_num, route: word;
		net, new_net, ignore, retskp: integer;

	    begin
		jsys (sysgt; puprou; ignore, tab_num.full);
		tab_num.left := 0;
		net := dest^.net;
		done := false;
		while not done do begin
		    jsys (getab, 1, retskp; (net - 1):tab_num.full;
					    route.full);
		    if retskp = 1 then js_err;
		    if route.full < 0 then begin
			done := true;
			our_net_address := nil;
		    end else begin
			new_net := (route.left div 400b) mod 200b;
			if new_net = 0 then begin
			    done := true;
			    new (addr);
			    addr^.net := net;
			    addr^.host :=  route.right;
			    our_net_address := addr;
			end else
			    net := new_net;
		    end;
		end;
	    end;

	begin
	    dest := destination (jfn);
	    addr := our_net_address (dest);
	    dispose (dest);
	    if addr <> nil then
		addr^.socket := get_socket (jfn);
	    owner_of := addr;
	end;

    (* * * * * * * * * * * * * * * * * * * * * * * * * * * *)
    (* jfn := get_port (address)			   *)
    (*   returns a jfn for a port to a given net address.  *)
    (*   the local socket number will be job-specific.	   *)
    (*   -1 is returned on failure.			   *)
    (* * * * * * * * * * * * * * * * * * * * * * * * * * * *)

    function get_port (where: address): integer;

	var
	    pup_string: asciz;
	    jfn: integer;
	    retskp: integer;

	procedure write_octal (num: integer);

	    begin
		if num > 8 then write_octal (num div 8);
		write (num:1:o);
	    end;    (* write_octal *)

	begin
	    strwrite (output, pup_string);
	    write ('PUP:.');
	    write_octal (where^.net);
	    write (chr(26b),'#');
	    write_octal (where^.host);
	    write (chr(26b),'#');
	    write_octal (where^.socket);
	    write (Chr (0));
	    jsys (gtjfn, 1, retskp; 11b:0, pup_string; jfn);
	    if retskp = 1 then
		get_port := -1
	    else
		get_port := jfn;
	end;

    (* * * * * * * * * * * * * * * * * * * * * * * * * * * *)
    (* boolean := open_raw_pup (jfn)			   *)
    (*   tries to open the given pup jfn in raw data mode. *)
    (*   returns true if it succeeds.			   *)
    (* * * * * * * * * * * * * * * * * * * * * * * * * * * *)

    function open_raw_pup (jfn: integer): boolean;

	const
	    magic_bits = 107000300000b;
		(* FLD(10,OF%BSZ)!FLD(16,OF%MOD)!OF%RD!OF%WR *)

	var
	    retskp: integer;

	begin
	    jsys (openf, 1, retskp; jfn, magic_bits);
	    if retskp = 1 then
		open_raw_pup := false
	    else
		open_raw_pup := true;
	end;

    (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
    (* long_pup := fill_long_pup (jfn, type, id)	       *)
    (*   makes a new long pup with all the fields filled out   *)
    (*   appropriately from its arguments		       *)
    (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

    function fill_long_pup (jfn, pup_type, pup_id, n_bytes: integer): long_pup;

	var
	    the_pup: long_pup;
	    from, dest: address;

	function net_host (where: address): integer;

	    begin
		net_host := combine (where^.net, where^.host);
	    end;

	function four (left, right: integer): integer;

	    var
		the_word: word;

	    begin
		the_word.a := left_8 (left);
		the_word.b := right_8 (left);
		the_word.c := left_8 (right);
		the_word.d := right_8 (right);
		four := the_word.full;
	    end;

	begin
	    new (the_pup);
	    dest := destination (jfn);
	    from := owner_of (jfn);
	    if from = nil then
		cmuerr ('You can''t get there from here');
	    the_pup^[0].full := four (min_pup + n_bytes, pup_type);
	    the_pup^[1].full := four (left_16 (pup_id), right_16 (pup_id));
	    the_pup^[2].full := four (net_host (dest),
				      left_16 (dest^.socket));
	    the_pup^[3].full := four (right_16 (dest^.socket),
				      net_host (from));
	    the_pup^[4].full := four (left_16 (from^.socket),
				      right_16 (from^.socket));
	    the_pup^[5].full := 0;  (* let monitor figure out the checksum *)
	    dispose (dest);
	    dispose (from);
	    fill_long_pup := the_pup;
	end;

    procedure fill_byte (data, byte_num: integer; pup: long_pup);

	var
	    the_word: integer;

	begin	(* fill_byte *)
	    the_word := (byte_num div 4) + 5;
	    case (byte_num mod 4) of
		0: pup^[the_word].a := data;
		1: pup^[the_word].b := data;
		2: pup^[the_word].c := data;
		3: pup^[the_word].d := data;
	    end;    (* case *)
	end;	(* fill_byte *)

    (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
    (* integer := num_bytes_in (long_pup)			     *)
    (*   returns the number of 8-bit content bytes in the given pup. *)
    (*								     *)
    (* integer := nth_byte (byte_num, long_pup)			     *)
    (*   returns the given content byte (zero-based).		     *)
    (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

    function num_bytes_in (pup: long_pup): integer;

	begin
	    num_bytes_in := combine (pup^[0].a, pup^[0].b) - min_pup;
	end;

    function nth_byte (which: integer; pup: long_pup): integer;

	var
	    the_word: word;

	begin
	    the_word := pup^[(which div 4) + 5];
	    case (which mod 4) of
		 0: nth_byte := the_word.a;
		 1: nth_byte := the_word.b;
		 2: nth_byte := the_word.c;
		 3: nth_byte := the_word.d;
	    end;
	end;

    (* * * * * * * * * * * * * * * * * * * * * * * * * *)
    (* boolean := send_long_pup (long_pup, jfn)        *)
    (*   tries to send the given pup out over the net. *)
    (*   returns true if successful.		       *)
    (* * * * * * * * * * * * * * * * * * * * * * * * * *)

    function send_long_pup (the_pup: long_pup; jfn: integer): boolean;

	const
	    pupo_bits = 200000b;    (* PU%CHK *)

	var
	    index, retskp:  integer;

	begin
	    jsys (pupo, 1, retskp; pupo_bits:jfn, (max_words + 1):the_pup);
	    send_long_pup := (retskp <> 1);
	end;

    (* * * * * * * * * * * * * * * * * * * * * * * *)
    (* pup := next_pup (jfn)			   *)
    (*   returns a pending pup for the given port. *)
    (*   returns nil if no pup is available.	   *)
    (* * * * * * * * * * * * * * * * * * * * * * * *)

    function next_pup (jfn: integer): long_pup;

	const
	    pupi_bits = 700000B;	(* PU%NOW!PU%CHK!PU%SRC *)

	var
	    pup: long_pup;
	    retskp: integer;

	begin
	    new (pup);
	    jsys (pupi, 1, retskp; pupi_bits:jfn, (max_words + 1):pup);
	    if retskp = 1 then begin
		next_pup := nil;
		dispose (pup);
	    end else
		next_pup := pup;
	end;

    function pup_type (p: long_pup): integer;

	begin	(* pup_type *)
	    pup_type := p^[0].d;
	end;	(* pup_type *)

    function pup_id (p: long_pup): integer;

	begin	(* pup_id *)
	    pup_id := (p^[1].full div 16) mod 40000000000b;
	end;	(* pup_id *)

    (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)
    (* address := local_host					 *)
    (*   finds and returns a network address associated with the *)
    (*   local host.						 *)
    (* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

    function local_host: address;

	const
	    puprou = 606560625765b;

	var
	    addr: address;
	    done: boolean;
	    tab_num, route: word;
	    net, new_net, ignore, retskp: integer;

	begin   (* local_host *)
	    jsys (sysgt; puprou; ignore, tab_num.full);
	    tab_num.left := 0;
	    net := 1;
	    done := false;
	    while not done do begin
		jsys (getab, 1, retskp; (net - 1):tab_num.full; route.full);
		if retskp = 1 then js_err;
		new_net := (route.left div 400b) mod 200b;
		if route.full < 0 then
		    net := net + 1
		else if new_net = 0 then begin
		    done := true;
		    new (addr);
		    addr^.net := net;
		    addr^.host := route.right;
		    local_host := addr;
		end (* if *) else
		    net := net + 1;
		if net > 77b then
		    cmuerr ('Couldn''t find local host');
	    end;    (* while *)
	end;    (* local_host *)

    function is_blank (c: char): boolean;

	begin   (* is_blank *)
	    is_blank := (c = ' ') or (c = '	');
	end;    (* is_blank *)

    function send_and_get_reply (foreign: address; data: asciz;
				 pup_type, pup_jfn: integer): long_pup;

	const
	    max_tries = 10; (* number of times to send before giving up *)
	    latency   = 10; (* msec after sending before checking reply *)
	    delay     = 10; (* msec between listening for pups *)
	    re_send   = 20;  (* times to listen before retransmitting *)
	    retry_delay = 100; (* msec before trying again *)

	var
	    tries, index: integer;
	    request, reply: long_pup;

	begin	(* send_and_get_reply *)
	    index := 0;
	    while (data [index] <> chr (0)) and (index < 39) do
		index := index + 1;
	    if (index = 39) and (data [index] <> chr (0)) then
		index := 40;

	    request := fill_long_pup (pup_jfn, pup_type, 0, index);
	    while index > 0 do begin
		index := index - 1;
		fill_byte (ord (data [index]), index, request);
	    end;    (* while *)

	    tries := 0;
	    repeat
		dismiss(retry_delay);
		if not send_long_pup (request, pup_jfn) then js_err;
		dismiss (latency);
		reply := next_pup (pup_jfn);
		index := 1;
		while (reply = nil) and (index <= re_send) do begin
		    dismiss (delay);
		    reply := next_pup (pup_jfn);
		    index := index + 1;
		end;	(* while *)
		tries := tries + 1;
	    until (reply <> nil) or (tries > max_tries);
	    send_and_get_reply := reply;
	    dispose (request);
	end;	(* send_and_get_reply *)

    (* convert a host name into a network address *)

    function conv_to_addr (var host: asciz; var the_addr: address): integer;

	const
	    no_server = 'No network name server available';

	var
	    retskp: integer;
	    reply: long_pup;
	    jfn: integer;

	begin   (* conv_to_addr *)
	    the_addr := local_host;
	    the_addr^.host := 0;    (* broadcast packet *)
	    the_addr^.socket := misc_serv;
	    jfn := get_port (the_addr);
	    if jfn < 0 then js_err;
	    if not open_raw_pup (jfn) then js_err;
	    reply := send_and_get_reply (the_addr, host, lookup_request, jfn);
	    if reply = nil then begin
		jsys (esout; no_server);
		die;
	    end (* if *) else if pup_type (reply) <> lookup_reply then begin
		dispose (reply);
		dispose (the_addr);
		jsys (closf, 1; jfn);
		cmuerr ('Unrecognized host name');
	    end (* if *) else begin
		the_addr^.net := nth_byte (0, reply);
		the_addr^.host := nth_byte (1, reply);
		the_addr^.socket := 0;
		dispose (reply);
	    end;    (* else *)
	    conv_to_addr := jfn;
	end;    (* conv_to_addr *)

    procedure trim_blanks (index: integer; var str: asciz);

	begin   (* trim_blanks *)
	    str [index] := chr (0);
	    if index > 0 then
		if is_blank (str [index - 1]) then
		    trim_blanks (index - 1, str);
	end;    (* trim_blanks *)

    (* read a user name / site name combination and set up the request *)

    function get_site (var foreign: address): integer;

	var
	    site_name: asciz;
	    the_net, the_host, the_socket, ignore: integer;
	    one_string: packed array [1..1] of char;
	    old_jfn, new_jfn: integer;

	begin	(* get_site *)
	    (* cminir done at top level for cmuerrs *)
/*	    cmmult;   
	    ignore := cmnum8;
	    cmhlp ('name of machine to ask status of');
	    ignore := cmtxt (site_name);
	    if cmdo = 2 then begin
		trim_blanks (cmint, site_name);
		cmcfm;
		old_jfn := conv_to_addr (site_name, foreign);
	    end (* if *) else begin
		the_net := cmint;
*/
	    begin
		the_net := cmnum8;
		one_string [1] := '#';
		cmtok (one_string);
		the_host := cmnum8;
		cmtok (one_string);
		one_string [1] := '5';
		cmdef (one_string);
		the_socket := cmnum8;
		cmcfm;
		new (foreign);
		foreign^.net := the_net;
		foreign^.host := the_host;
		foreign^.socket := the_socket;
		old_jfn := 0;
	    end;    (* else *)
	    if foreign^.socket = 0 then
		foreign^.socket := echo_port;
	    new_jfn := get_port (foreign);
	    if new_jfn < 0 then js_err;
	    if not open_raw_pup (new_jfn) then js_err;
	    get_site := new_jfn;
	    if old_jfn <> 0 then
		jsys (closf, 1; old_jfn);
	end;	(* get_site *)

    (* say what happened *)

    function print_status (reply: long_pup): boolean;

	var
	    index: integer;

	procedure write_pup (index: integer; the_pup: long_pup);

	    var
		last_char: integer;
		done: boolean;

	    begin   (* write_pup *)
		last_char := num_bytes_in (the_pup) - 1;
		if is_blank (chr (nth_byte (index, the_pup))) then
		    index := index + 1;
		if (chr (nth_byte (last_char, the_pup)) = chr (10)) then
		    last_char := last_char - 1;
		if (chr (nth_byte (last_char, the_pup)) = '.') then
		    last_char := last_char - 1;
		while index <= last_char do begin
		    write (tty, chr (nth_byte (index, the_pup)));
		    index := index + 1;
		end;	(* loop *)
	    end;    (* write_pup *)

	begin	(* print_status *)
	    print_status := false;
	    if reply = nil then
		writeln (tty, 'Host not responding.')
	    else if pup_type (reply) = error_pup then begin
		if num_bytes_in (reply) <= 24 then
		    writeln (tty, 'Network error.')
		else begin
		    write (tty, 'Network error: ');
		    write_pup (24, reply);
		    writeln (tty, '.');
		end;	(* else *)
	    end (* if *) else if pup_type (reply) = bad_echo then
		writeln (tty, 'Bad echo received.')
	    else if pup_type (reply) <> echo_pup then
		writeln (tty, 'Unexpected PUP type ',
			 pup_type (reply):3:o, ' recieved.')
	    else if num_bytes_in (reply) <> 40 then
		writeln (tty, 'Length of reply (', num_bytes_in (reply):0,
			      ') wrong.')
	    else begin
		print_status := true;
		for index := 0 to 39 do
		    if nth_byte (index, reply) <> index + ord ('@') then begin
			writeln (tty, 'Byte at position ', index:0, ' is ',
				      nth_byte (index, reply):3:o,
				      ', should be ', index + ord ('@'):3:o,
				      '.');
			print_status := false;
		    end;    (* if *)
	    end;    (* else *)
	    if reply <> nil then
		dispose (reply);
	end;	(* print_status *)

    (* the main program *)

    begin   (* mcheck *)
	for index := 0 to asciz_len do
	    test_data[index] := chr(100B+index);
	cminir ('Site: ');
	pup_jfn := get_site (site);
	while true do begin
	    if print_status (send_and_get_reply (site, test_data,
						 echo_me, pup_jfn)) then
		write (tty, '.')
	    else begin
		cmini ('[Confirm to continue] ');
		cmcfm;
	    end;	(* if *)
	end;	(* while *)
    end.    (* mcheck *)
