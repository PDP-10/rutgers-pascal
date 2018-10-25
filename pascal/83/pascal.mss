@device(file)
@make(report)
@style(indentation 0,topmargin .5inch,linewidth 7inches)
@modify(hdx,above 3,below 2,need 7)
@modify(hd0,above 3,below 2,need 7)
@modify(hd1,above 4,below 3,centered,pagebreak before)
@modify(hd2,above 3,below 2,need 7)
@modify(hd3,above 3,below 2,need 7)
@modify(hd4,above 3,below 2,need 7)
@modify(description,leftmargin 15,indent -10)



@begin(center)
PASCAL-20

Reference Manual







@end(center)

The information in this document is subject to change without
notice and should not be construed as a commitment by Charles Hedrick
or Rutgers University.  Charles Hedrick and Rutgers University assume
no responsibility for any errors that may appear in this document.

Note:  The following are trademarks of the Digital Equipment
Corporation:  DECSYSTEM-20, DECsystem-10, Tops-20, Tops-10

@NewPage(0)@Set(Page=1)@Style(PageNumber <@1>)
This document describes DECSYSTEM-20 Pascal.  This Pascal system
is the result of cooperation among a number of different people.
It was originally written at the University of Hamburg by a group of
people under the supervision of Prof. H.-H. Nagel. This version was
developed from Prof. Nagel's by Charles Hedrick, and is maintained by
him. Lee Cooprider and others at the University of Southern California
have been particularly helpful in supplying improvements, largely to the
debugger. The heap manager was originally written by Shel Kaphan at
Stanford, and adapted for Pascal by Dave Dyer at ISI. A number of
compiler bug fixes were supplied by Andrew Hisgen at Carnegie-Mellon
University.  Extended addressing was originally implemented by Norman
Samuelson at Sandia National Labs, although Charles Hedrick made major
improvements to this implementation.


This system is intended to be a complete implementation of the Pascal
language, as defined in the Revised Report (Jensen and Wirth). We 
also believe it to be in conformance with the ISO Pascal Standard,
ISO/DP 7185, as published in the second draft proposal, January, 1981.
We regard the ISO standard as normative, and are prepared to fix any
cases where programs that conform to the ISO standards do not work
properly.  There is a separate chapter describing conformance with
the ISO standard.

This implementation includes a number of extra facilities, giving you
access to the full power of the operating system. To do this, a number
of procedures were added, and optional arguments were added to several
existing procedures. These additions give you access to the full power
of the DECSYSTEM-20's input output system, as well as to other
facilities such as interrupt handling.  In the development process, there
were two design goals: (1) a good interface to the operating system, and
(2) providing good debugging and user interface facilities. As a result
of these two goals, this compiler is now appropriate for both system
programming and instructional use.  However it is still not an
optimizing compiler, and should not be used for applications where
high-quality code is important.

This manual is intended as a complete reference manual
for this implementation.  As such it contains far more
detail on extensions than many users will need. There is
a somewhat briefer manual, which is more suitable for
the average user.  Both manuals describe only features
that differ from those documented in the Revised Report.
So you should look at the Revised Report first.

@chapter(How to use PASCAL-20)

@section(How to use the compiler under release 5)

This section describes use of the compiler version that is intended
to be used with release 5 of Tops-20.  Chapter @ref(3a4) describes
two other versions of the compiler intended for use on release 3A and 4.
Pascal is normally used with the COMPIL-class commands: COMPILE, DEBUG,
EXECUTE, and LOAD.

To compile and execute a PASCAL program TEST, you would
issue the command
@begin(display)

	EXECUTE TEST.PAS

@end(display)
The usual COMPIL switches, such as /NOBIN, /LIST, and /CREF can
be used.  For other commands, such as COMPIL, DEBUG, and LOAD,
see your EXEC documentation.  If you are linking more than one
module, you must mention the main program first in the EXECUTE
command.  Otherwise the loader will complain that .XSTRT is
not defined.

If any errors occur during compilation, the compiler writes error
messages to three different places:
@begin(itemize)
your terminal

a file, in this case TEST.ERR.  The file name is constructed by
taking your source file name and changing the extension to .ERR.
If such a file already exists, it is overwritten.

your listing file, if you ask for one
@end(itemize)
We put the error messages in a special .ERR file as a convenience
to you.  Originally we just wrote the messages to your terminal.
However the messages usually scrolled off the top of the screen.
Thus we starting writing them to a file, so you could go back
and look at the file to find your errors.  To correct all your errors,
we suggest using EMACS in 2-window mode.  Visit your source file
in one window, and read the .ERR file in the other window.
If there are no errors, PASCAL deletes any old .ERR file, just to
prevent confusion.

If your program begins with a PROGRAM statement, execution
will begin by asking you for file spec's for each of the
files mentioned in the program statement.  You should type
a standard Tops-20 file spec, terminated with <CRLF>.
Recognition is used.  If the file already exists, the
most recent generation is used by default.  (It is
possible to change this default so that a new generation
is created.  See section @ref(filedec).

If you type only a carriage return, a default file spec will
be used.  For INPUT and OUTPUT this is "TTY:", normally your terminal.
For other files it is a disk file with the same name as
the Pascal file name.

If you assign the file INPUT to a terminal, you will normally be
prompted for the first line of input by the Pascal I/O system,
[INPUT, end with ^Z: ].  Because of oddities of the Pascal
language, this initial read is done before your program has
started.  Hence you cannot issue a prompt first.  This can
be avoided by specifying INPUT to be interactive (see below).

Note that the effect of listing a file in the PROGRAM statement
depends upon whether it is a predeclared file -- INPUT or OUTPUT --
or a file declared by the user.  For a user-declared file,
listing it in the PROGRAM statement  simply provides an easy way to get
a file specification for it at runtime.  It does not open the file.
That is, you must still do RESET or REWRITE on it.
And you must still declare the file identifier in the VAR
section of the program.  However for the predeclared files
INPUT and OUTPUT, listing them in the PROGRAM statement
also causes the system to open them (RESET for INPUT,
REWRITE for OUTPUT).

There are a number of switches that control various options
of the compilation process.  Switches fall into three
categories:
@begin(itemize)
Pascal compiler switches that the COMPIL-class commands know about.
Because COMPIL knows about them, you can type them directly as part
of you command.  COMPIL will simply pass them on to the Pascal compiler.
An example is /OPTIMIZE, which specifies that debugging and checking
should be turned off in the compiled program:
@begin(display)
@@EXECUTE PROG.PAS/OPTIMIZE
@end(display)

Pascal compiler switches that the COMPIL-class commands do not
know about.  These switches are similar in nature to those just described.
However because COMPIL does not know about them, you can't type them
directly as part of the command.  Instead you must use the /LANGUAGE
switch to pass them to Pascal.  The /LANGUAGE switch is a general
COMPIL switch designed to handle switches that COMPIL does not know
about.  Since it makes no assumptions about what you will be passing,
its argument is a set of switches in quotes. For example /EXTEND, the
switch to enable extended addressing is in this category.  To execute
PROG.PAS in extended addressing mode, you would type:
@begin(display)
@@EXECUTE PROG.PAS/LANG:"/EXTEND"
@end(display)

COMPIL-class switches that the Pascal compiler does not know about.
This switches generally control the compilation process, and thus
the compiler does not need to know about them.  An example is the
switch /COMPILE, which forces compilation even if the .REL file is
newer than the source file.  This switch need not be passed to
Pascal, since all it does is determine whether Pascal should be
called at all.
@end(itemize)

A list of the first two kinds of switches is given below.  The
third kind is described in the EXEC manual.

If you choose not to use COMPIL-class commands, you can call the
compiler directly.  A command to the compiler is of the form:
@begin(display)
source-file/switch/switch...
@end(display)
You can either run PASCAL and type the commands to the PASCAL>
prompt, or you can type a single command on the same line as
the PASCAL call.  For example
@begin(display)
@@PASCAL	! here we type a command directly to PASCAL
PASCAL>PROG.PAS/EXTEND
PASCAL>^C

@@PASCAL PROG.PAS/EXTEND  ! here we put it on the same line
PASCAL:  PROG
@@
@end(display)

When you call the compiler directly, rather than using a
COMPIL-class command, the following default actions are
taken unless you specify otherwise with switches:
@begin(itemize)
A relocatable object file is produced.  It is called
DSK:xxx.REL, where xxx is the name of the source file.

No listing file is produced.
@end(itemize)

Here is a list of the switches that are meaningful to the
Pascal compiler.  Any of these switches can be typed when
Pascal is called directly.  A * after the switch name means that this
switch is also defined in COMPIL-class commands.

@begin(description)

/ARITHCHECK - Turns on error messages for arithmetic errors, i.e.
divide by zero, overflow, and underflow.  

/CREF*  - generates a listing file formatted in such a way that
the system program CREF can produce a crossreference listing.  
If you call the compiler directly, you can use the /LIST
switch to specify the name of the listing file.  If you do
not, it will be called DSK:xxx.CRF, where xxx is the name of the
source file.

/DEBUG* - generate information for the DEBUG package.  This is
normally on except for production programs.
We strongly encourage people to turn this off (probably by putting
the directive (*$D-*) in their program) when they know they have
finished using PASDDT.  The debug information may double the size
of your program.  Since this is the default, there would never
be any reason to type this switch.

/EXTEND - generate code for extended addressing use.  See Chapter
@ref(extended).

/HEAP:nnn - sets the first address to be used for the heap.
This is the storage used by NEW.  It will begin at
the specified address and go down.  The only known
use for this is if you intend to load the high
segment at other than 400000.  NNN is an octal address.

/LIST:filename* - Requests a listing file.  The filename can be
supplied only if you call the compiler directly, and is optional.  If
you do not specify the file name, the name DSK:xxx.LST will be used,
where xxx is the name of the source file.  Note that the /LIST
switch to the COMPIL-class commands automatically results in
a file name of LPT:xxx, that is, the listing file will go directly
to the printer.  If you do not want that, you can use /LAN:"/LIST"
so that you get the Pascal compiler's version of the switch,
which puts the listing on disk by default.

/MACHINE-CODE* - list the generated code in symbolic form.  This
automatically turns on /LIST (with the default file name unless
you give a /LIST:filename yourself).

/NOARITHCHECK - Turns off error messages for arithmetic errors, i.e.
divide by zero, overflow, and underflow.

/NOBINARY* - No object file will be produced.  Useful if you
just want to see whether there are syntax errors in the program,
or just produce a listing.

/NOCHECK - Normally Pascal generates code to perform runtime checks for
indices and assignments to scalar and subrange variables.  Pointer
accesses will also be checked for NIL or zero pointers.  (Usually a zero
pointer is the result of a pointer variable not being initialized.)
Divide by zero, overflow, and underflow are also caught.  All of these
cases cause an error message to be printed and transfer to PASDDT or
DDT if they are loaded.  [Note that .JBOPC is set up for DDT. However the
program cannot necessarily be continued, because the AC's may be those
of the error trapper.  Also, in the case of an arithmetic error, you are
at interrupt level in DDT.]  This switch *TURNS* *OFF* the checking
just described.  If neither /ARITHCHECK nor /NOARITHCHECK has been
specified, it also has the effect of /NOARITHCHECK.  Note that there is
a /NOCHECK switch available for COMPIL-class commands, but it has no
effect when used with Pascal.  Thus this switch must be used when Pascal
is called directly, or it must be passed as /LANG:"/NOCHECK".

/NODEBUG - turns off generation of symbol table information needed
by PASDDT.  This makes your program significantly smaller, but you
will have to recompile it (without /NODEBUG) if you want to use
PASDDT with it.  Note that there is a /NODEBUG switch available for
COMPIL-class commands, but it has no effect when used with Pascal.  Thus
this switch must be used when Pascal is called directly, or it must be
passed as /LANG:"/NODEBUG".

/NOEXTEND - reverse of /EXTEND.  Since /NOEXTEND is the default, this
switch seems to be of no use.

/NOMAIN - indicates that this file is a file of external procedures,
and does not have a main program part.  This is normally specified
by {$M-} in the source itself, instead of by a switch.

/OBJECT - when given with no argument, this switch is the
/OBJECTLIST switch, and is equivalent to /MACHINE-CODE (for
compatibility with older versions of the compiler).

/OBJECT:filename - specifies the name of the file to be used for
the relocatable object output.  The default is DSK:xxx.REL, where
xxx is the name of the source file.

/OPTIMIZE* - equivalent to /NOCHECK/NODEBUG.

/STACK:nnn - sets the first location to be used for the
stack.  This should be above the high segment
(if any).  The only known use is if you intend
to do a GET to a larger segment and want to
be sure the stack doesn't get in the way. NNN is an octal address.

/VERSION:vvv - This version number will be put in .JBVER, and the
third location of the entry vector, unless overridden by a later
directive.  vvv is a full word in octal.

/ZERO - Causes code to be compiled in every procedure and
function prolog to initialize all of its local
variables to 0.  Also, NEW will initialize all
records it generates to 0.  This is useful mainly
for programs with complicated pointer manipulations,
since it guarantees that uninitialized pointers will be
0, which will be caught by the /CHECK code.  Note that
/ZERO and /CHECK can be set independently, however, and
that /ZERO applies to all local variables, not just
pointers.  /ZERO will not cause global variables to be
reinitialized, although they always start out as zero
unless an initprocedure is used to give them another
value.
@end(description)

In addition to these switches, there are two switches that are
designed for use in communicating with the EXEC.  They can be
used *IN PLACE OF* a source file name.  They cause Pascal to
run another program.  The intended use is for the EXEC to request
Pascal to run LINK.
@begin(description)
/RUN:filename - replaces the Pascal compiler with the specified
program.  The default extension is .EXE.  The specified program
will be started at an address specified by the /OFFSET switch.
If none is given, it will be started at its normal starting address.

/OFFSET:nnn - Specifies that the program is to be started at
the normal starting address + NNN.  NNN should normally be 0 or 1.
0 is used if no /OFFSET switch is used.
@end(description)

@section(Core Allocation)
@label(dispose)

PASCAL has dynamic core allocation.  This means that memory
will automatically expand if you do NEW a lot, or use a large
stack.  Thus if you get a message informing you that you have run out
of core, it means that you used all of your virtual memory space.
In such a case, you should reconsider your data structures or
algorithm.

Programs that do a lot of dynamic memory allocation should
consider returning spaced used by structures they are
finished with.  Note that PASCAL makes no
attempt to garbage collect unused structures.  This means that
the programmer must know when he is finished with a particular
record instance and DISPOSE it.  DISPOSE is a standard procedure,
described in some editions of the Revised Report.  It takes
exactly the same arguments as NEW.  However it returns the space
used by the record pointed to.  This space can then be reused
by later NEW's.  It is very important that any extra arguments
you supplied to NEW in generating the record be supplied in the
same way to DISPOSE.  (These arguments are used only for variant
records, to allow allocation of space for a particular variant.)
If you do not use the same parameters, you will get the error
message:  "DISPOSE called with clobbered or already-disposed object".
In addition to checking validity of the disposed object in this
way, the runtimes also check for disposing NIL or 0, and give
an appropriate error message.

If your program uses memory in a strictly hierarchical fashion,
you may also find it possible to use the procedures MARK and
RELEASE to deallocate memory (See section @ref(markrel)).  RELEASEing
an entire block of memory is more efficient than DISPOSEing of
records one by one, though this efficiency is balanced by the
fact that MARK and RELEASE are not part of official Pascal (though
they are present in most implementations).

Note that you get a completely different version of NEW when you
use DISPOSE and when you do not.  The system handles loading the
right version of NEW automatically.  The version used with DISPOSE
is not compatible with MARK and RELEASE.  It is also not compatible
with use of the /HEAP switch (or $H directive) to start the heap
at addresses above 377777 octal.

If you do interrupt handling, note that DISPOSE, and the version
of NEW used when DISPOSE is used, are not reentrant.  Thus
you should treat sections of code using NEW and DISPOSE as
critical sections.  (The easiest thing is to call ENTERCRIT
and LEAVECRIT around all calls to NEW and DISPOSE.)  This is
only needed if you are using DISPOSE, and if you do either
NEW or DISPOSE at interrupt level.  (That is why I have not
put a built-in critical section around NEW and DISPOSE.)

@section<How to Write a Program (Lexical Issues)>

PASCAL programs can be written using the full ASCII character
set, including lower case.  Of course some characters (e.g.
control characters) will be illegal except in character or
string constants.  Lines are ended by carriage-return/linefeed,
form feed, or altmode (escape).
Lower case letters are mapped into the equivalent upper case letters 
before being analyzed by the compiler, except in string or character
constants.  However, lower case letters will always appear in any 
listings exactly as read in. Now we shall describe language elements 
which use special characters.

Comments are enclosed in { and }, (* and *), /* and */, or % and \.
For example
@begin(display)

	{This is an official comment}
	(*This is a comment*)
	%So is this\
	(*And so \  is this *)

@end(display)
The switches mentioned above as appearing in the compiler
command line may also be set in the body of the program by
directives.  Such directives take precedence over any setting
typed in the command string.   These directives are comments
which start with a $ sign and have the form
@begin(display)

	(*$C+*) or %$C+\

@end(display)
Each switch has a corresponding letter (e.g. C represents /CHECK).
A + after the letter indicates that the corresponding
switch should be turned on, a - that it should be turned off.
More than one switch setting can be given, separating them with
a comma:
@begin(display)

	(*$T+,M-*)

@end(display)
The letters used in the directives correspond to the switches
in the following way:
@begin(display)

	A	ARITHCHECK
	C	CHECK
	D	DEBUG
	H	HEAP
	M	MAIN
	L	OBJECTLIST
	S	STACK
	V	VERSION
	X	EXTEND
	Z	ZERO

@end(display)
The form for H and S is (*$H:400000B*), etc.
The form for V is (*$V:2200000000B*), etc., i.e. the version number
in octal.

Note that setting or clearing C also sets or clears A, so order matters.
To clear C, but leave A on, you should do something like {$C-,A+}.
This is consistent with the overall approach wherein the default value
of ARITHCHECK is the same as CHECK.

Identifiers may be written using the underline character to
improve readability, e.g.:
@begin(display)

	NEW__NAME

@end(display)
Strings are character sequences enclosed in single quotes, e.g.:
@begin(display)

	'This is a string'

@end(display)
If a quote is to appear in the string it must be repeated, e.g.:
@begin(display)

	'Isn''t PASCAL fun?'

@end(display)
Note that mapping of lower case to upper case 
is not done inside strings.

An integer is represented in octal form if it consists of octal
digits followed by B.  An integer is represented in hexadecimal
form if it consists of a " followed by hexadecimal digits.  The
following representations have the same value:
@begin(display)

	63	77B	"3F

@end(display)
Several PASCAL operators have an alternate representation.  The
alternate form is provided for compatibility with older versions
of PASCAL.  The form of the operator shown in the left column
should be used in new programs.
@begin(display)

   operator    alternate form         explanation

     >=              "                greater or equal
     <=              @@                less or equal
     AND             &                logical and
     OR              !                logical or
     NOT             $                logical negation
     <>              #                not equal
     +		     OR,!	      set union
     *               AND,&            set intersection

@end(display)

@section(Extensions from the ISO Proposal)

There are a number of extensions in this version.  Many of them will
not be of interest to "normal" people, so I am delaying them until later
in this document.  However many people will probably want to know that
we have implemented the most important new features from the ISO Pascal
Proposal.  I do not claim that this compiler is complies with the ISO
standard in a technical sense.  (The deviations are probably mostly
errors that we do not catch.)  However these features will greatly
improve the ability to transport code between this compiler and real
ISO Pascals.

@subsection(Conformant Arrays)

Conformant arrays are designed to let you write general-purpose
procedures that will handle arrays of any size.  Normally a procedure
must take parameters of a definite type.  Since ARRAY[1..5] and
ARRAY[1..10] are different types, this means that you can't write
a procedure to handle both of these kinds of arrays.  Conformant
arrays solve this problem.  They allow you to say that your procedure
takes an ARRAY[I..J:INTEGER].  That is, it will match any array
that has integer bounds.  Also, it sets I and J to the lower and
upper bounds, respectively.  E.g. 
@begin(display)

    procedure invert(var inarray,outarray:array[I..J:integer,
                                  K..L:integer] of real;
		 var weights:array[M..N:integer] of integer);
       begin
	...
       end

@end(display)
There are three arguments: INARRAY, OUTARRAY, and WEIGHTS. Their bounds
must be integers. INARRAY and OUTARRAY must have the same bounds.
(Currently we do not check that things are the same when they are
supposed to be.)  I, J, K, L, M, and N are declared as local variables
in the procedure.  They will be set to the bounds of the arrays that are
actually passed.  Normal Pascal array declaration syntax is used.
ARRAY[I..J,K..L] is equivalent to ARRAY[I..J] OF ARRAY[K..L].  You may
say PACKED (but only for the last dimension).

NB:  Conformant arrays must always be passed by reference (VAR).

@subsection(Parametric Procedures)

Probably you will never want to do this, but...  It is possible
to pass a procedure as a parameter to another procedure.  E.g.
PROCEDURE FOO(PROCEDURE BAR); BEGIN ... END.  When you call FOO
you must pass an actual procedure:  FOO(P1).  Inside FOO, you
can use BAR as if it were a procedure - you will get P1.  This is
all documented in Jensen and Wirth.  The one thing that Jensen
and Wirth did not do is define a way for you to tell the compiler
what arguments you are going to use for BAR.  If you use the
old conventions, but not declaring the parameters for BAR, you
can pass FOO absolutely any procedure you want, and when you get
around to calling it, you can pass any arguments you want.  No
type checking can be done, because the compiler can't tell what
procedure you are going to pass at runtime.  Also, the compiler
can't handle call by reference to a procedure passed this way,
nor can it handle more than about 5 arguments.  It is just impossible
to generate nice code without knowing what type of arguments that
procedure is going to want.  The ISO standard requires you to declare the
arguments of procedures passed like this:
@begin(display)

	procedure foo(procedure bar(i,j:integer)); ...

@end(display)
This solves most of the problems.  The compiler can now check what
you are doing, and generate code correctly.  In order to avoid
breaking old code, we still allow the old method.  If you specify
any parameters, they will be used as in the ISO standard.  If you
don't, you can pass any you like.

@chapter(Input/Output)

Input/Output is done with the standard procedures READ, READLN, WRITE,
and WRITELN as described in the Revised Report on PASCAL [1,2].

@section(Standard Files)

In addition to the standard files INPUT and OUTPUT the standard
file TTY is available in Tops-20 PASCAL.  This file is used to
communicate with the terminal.  The standard files can be directly
used to read or write without having to use the standard procedures
RESET or REWRITE.  Note that these files are logically declared in
a block global to all of your code.  Specifically, if you use external
procedures, those procedures may also refer to INPUT, OUTPUT, and TTY,
and the same files will be used as in the main program.

As described in the Revised Report, the files INPUT and OUTPUT
are opened for you automatically if you mention them in your
PROGRAM statement.  The file TTY does not need to be opened,
since it is "hardwired" to the terminal.  (Indeed mentioning TTY
in the program statement is completely useless.  Doing RESET or REWRITE on
TTY is also almost completely useless,
except that RESET can be used to establish
lower to upper case conversion or to let you see end of line
characters.  However any file specification given in RESET will be
ignored.)

@section(File Declaration)
@label(filedec)

Files that you declare follow the normal scope rules.  That is, they
are local to the block in which they are declared.  This means that
a file F declared in the main program is a different file than a file
F declared in a file of external procedures, or in a different
block.  To use the same file
in an external procedure, you should pass it as a parameter to the
procedure.  (It must be passed by reference, i.e. declared with
VAR in the procedure header.)

You have two opportunities to specify what external file name you
want associated with a Pascal file variable (e.g. that you want
INPUT to refer to "TTY:").  One is by listing the file variable
in the PROGRAM statement.  This has been described above.  The other
is by supplying a file name as a string when you use RESET or
REWRITE.  If you do not supply a file name in one of these ways,
the file is considered "internal".  That is, Pascal will choose
a filename for you, and will do its best to see to it that you
never see the file.  When you exit from the block in which the file
variable was declared, Pascal will delete the file.  Such files
are useful for temporary working storage, but obviously should
not be used for the major input and output of the program.

The syntax of the PROGRAM statement has been extended to allow
you to declare some extra attributes of the file.  These
allow the initial file name dialog to be made slightly
more intelligent than otherwise.  The attributes are specified
by using a colon after the file name in the list.  For example
@begin(display)

     PROGRAM TEST(INPUT:-*/,OUTPUT:+);

@end(display)
The character - implies that this is an input file.  When
the user is prompted for the file name, an error message will
be typed if it does not exist, and he will be asked to try again.
The character + implies that this is an output file.  This means
that if the file exists and the user defaults the version
number, a new version will be used.  The character * allows
wildcards in the file specification.  If this extra syntax is
not used, no assumption is made about whether the file is input
or output, and the most recent generation is used for any existing
file.

The / is effective only for the file INPUT.  It specifies
that the compiler-generated RESET should be done in interactive
mode, i.e. that the initial implicit GET should not be done.
This is useful for files that may be associated with terminals.
It eliminates the initial [INPUT, end with ^Z: ] that would
otherwise be generated by the Pascal I/O system.


@section<RESET and REWRITE (simple form)>
@label(simopen)

Except for the standard files, a file must be "opened" with the 
standard procedure RESET when it is to be used for reading.  It must 
be "opened" with the standard procedure REWRITE when it is to be used 
for writing.

RESET and REWRITE may have up to 6 parameters in Tops-20 PASCAL.  However,
most users will need at most 2  of them, so the others are deferred
until section @ref(wiz).
@begin(display)

     RESET (<file identifier>,<file spec>)

@end(display)
Only the first parameter is required.  If <file spec> is specified,
it must be of type PACKED ARRAY of CHAR.  Any length
is acceptable, and string constants may also be used.
The parameter is expected to be the usual Tops-20 file spec.
A GTJFN is done on it.  Simple programs need not use this
parameter, since they will get the file name via the starting
dialog.

If you omit this parameter, the jfn currently associated with this
file will be used again.
Usually this means that the same file (and version)
used before will be used.  The opening dialog does a GTJFN,
so in practice you usually end up with the file specified
in that dialog.
If the file was not listed in the PROGRAM
statement,  and you do not specify a file name some time when you
open the file, it will be considered "internal", as described
above. To omit the file spec parameter when further
parameters are specified, use a null string, i.e. ''.

In the following example REWRITE is used to give the file OUTPUT
the actual file name TEST.LST, with protection 775252

Example:    REWRITE(OUTPUT,'TEST.LST;P775252')

Note that RESET and REWRITE can fail.  The most common cause is
something wrong with your file spec., but various other problems
with the program or hardware can also cause failure.  Unless
you have specified user error handling (see below), you will
get the official error message returned by the monitor.

@section(Formatted Output)
@label(write)

Parameters of the standard procedure WRITE (and WRITELN) may be
followed by a "format specification".  A parameter with format
has one of the following forms:
@begin(display)

	X : E1
	X : E1 : E2
	X : E1 : O
	X : E1 : H

@end(display)
E1 is called the field width.  It must be an expression of type
INTEGER yielding a nonnegative value.  If no format is given
then the default value for E1 is for type
@begin(display)

	INTEGER		12
	BOOLEAN		 6
	CHAR		 1
	REAL		16
	STRING		length of string
@end(display)
Blanks precede the value to be printed if the field width is
larger than necessary to print the value.  Depending on the type
involved, the following is printed if the field width is smaller
than necessary to print the value:
@begin(display)

	INTEGER(normal)	field width increased to fit
	INTEGER(octal)	least significant digits
	INTEGER(hex)	least significant digits
	REAL		field width increased to fit
	BOOLEAN		field width increased to fit
	STRING		leftmost characters

@end(display)
A maximum of 7 significant digits will be printed for
real numbers.  Rounding is done at the seventh digit
(or the rightmost digit, if the format does not allow
a full seven digits to be displayed).  Because of the
automatic expansion of formats for normal integers and
reals, a field width of zero is a convenient way to get
a free format output.

The minimal field width for values of type REAL is 9.
The representation used for a field width of 9 is
b-d.dE+dd, where b is a blank, - a minus sign or blank,
d a digit, and + a plus or minus sign.  As the field
width is increased, more digits are used after the
period, until a maximum of 6 such digits is used.  After
than point, any increased field width is used for leading
blanks.
@begin(display)

Example:   WRITELN('STR':4, 'STR', 'STR':2, -12.0:10);
	   WRITELN(15:9, TRUE, FALSE:4, 'X':3);

@end(display)
The following character sequence will be printed (colons
represent blanks):
@begin(display)

	:STRSTRST -1.20E+01
	:::::::15::truefalse::X

@end(display)
(Note that the field width for FALSE has been expanded in
order to fit in the output.)

A value of type REAL can be printed as a fixed point number if
the format with expression E2 is used.  E2 must be of type
INTEGER and yield a nonnegative value.  It specifies the number
of digits following the decimal point.  Exactly E2 digits
will always be printed after the point.  The minimal field width
for this format is E2 + D + S + 2, where D represents the
number of digits in front of the decimal place, and S is 1 if the
number is negative and 0 otherwise.  The extra 2 places are for
the decimal point and a leading blank.  There is always at least
one leading blank, as required by the Revised Report.  Extra
field width will be used for leading blanks.
@begin(display)

Example:      WRITELN(1.23:5:2, 1.23:4:1, 1.23:6:0);
	      WRITELN(1.23:4:3, 123456123456:0:0);

@end(display)
The following character sequence will be printed (colons
represent blanks):
@begin(display)

	:1.23:1.2::::1.
	:1.230:123456100000.

@end(display)
The :1.230 is a result of automatic format expansion, since the
specified 4 spaces was not enough.  The 123456100000 shows that
numbers will be rounded after 7 significant digits.

A value of type INTEGER can be printed in octal representation if
the format with letter O is used.  The octal representation
consists of 12 digits.  If the field width is smaller than 12,
the rightmost digits are used to fill the field width.  If the
field width is larger than 12, the appropriate number of blanks
preceded the digits.
@begin(display)

Example:   WRITE(12345B:2:O, 12345B:6:O, 12345B:15:O);

@end(display)
The following character sequence will be printed (colons
represent blanks):
@begin(display)

	45012345:::000000012345

@end(display)
A value of type INTEGER can also be printed in hexadecimal
representation if the format with letter H is used.  The
hexadecimal representation consists of 9 digits.  Using the
format with letter H, the following character sequence will be
printed for the example above (colons indicate blanks):
@begin(display)

	E50014E5::::::0000014E5

@end(display)

@section(Reading characters)

In official Pascal one cannot use READ or READLN to read into arrays of
CHAR.  Thus one sees many programs full of many loops reading characters
into arrays of CHAR, cluttering up essentially simple algorithms.  I
have implemented READ into arrays and packed arrays of CHAR, with
capabilities similar to SAIL's very fine string input routines.  An example
of the full syntax is
@begin(display)

	read(input,array1:howmany:['@ ',':'])

@end(display)
This will read characters into the  array array1 until one of three things
happens:
@begin(itemize)
One of the characters mentioned in the "break set" (in this case
blank or colon) is read.  This character (the "break character") is
not put into the array.  You can find it by looking at INPUT^, since
this always contains the next character that will be read by READ.
Howmany (which can be any integer variable) is set to the number of
characters actually put into the array.

End of line is reached in the input.  Again, howmany is set to the
number of characters put into the array.  You can test for this outcome
by looking at EOLN(INPUT).

The array is filled.  In this case, INPUT^ is the character that would
have overflowed the array.  Howmany is set to one more than the size of
the array, in order to allow you to detect this case uniquely.
@end(itemize)
If filling of the array is terminated by a break character or end of
line, the rest of the array is cleared to blanks.

The break set can be omitted, in which case input breaks only on
end of line or when the array fills up.  The integer variable can also
be omitted, in which case the count is not given to the user.  Thus the
actual syntax permitted is
@begin(display)

	read(<array-name>[:<integer-var>[:<set-expression>]])

@end(display)
The user is cautioned not to confuse this syntax with the field width
specification for output:  READ(X:I) does not specify a field width
of I.  Rather I is set after the input is done to tell how many
characters were actually read.

A number of users have been confused as to how to read from the
terminal.  The following would seem to be appropriate:
@begin(display)

	write(tty,'Please enter I and J as integers:');
	readln(tty,i,j);

@end(display)
However if you do all of your input that way, you will discover that
in each case the system will go into terminal input wait trying to
read the data BEFORE the question is printed.  The reason has to do with
the exact definition of readln:  Readln reads characters until it comes
to the first character AFTER the next end of line.  When used with
a terminal, this means that readln throws away anything else on the
current line and waits for you to type the next one.
Readln(tty,i,j)
is equivalent to read(tty,i,j); readln(tty).  So it reads the 
data and then asks for the next line, before the next question can
be asked.

The correct sequence of statements is
@begin(display)

	write(tty,'Please enter I and J as integers:');
	readln(tty); read(tty,i,j);

@end(display)
Note that the first time this sequence is used in your program, the
readln will pass the null that is automatically inserted before
your first real character (see @ref(stanfile)).  For files other than
TTY, you will have to declare them as interactive in order to get
the same effect.

@section(The Standard Files)
@label(stanfile)

There are three files which may be initialized by PASCAL
for the user automatically.  These are INPUT,
OUTPUT, and TTY.  If you list it in the program statement, INPUT is
initialized by an implicit RESET.
If you list it, OUTPUT is initialized by an implicit
REWRITE.  TTY is always initialized on the
user's terminal.  For most purposes one may assume that
TTY is both RESET and REWRITTEN, i.e. that
it can be used for both read and write operations.  As in
standard PASCAL, the default file for those
standard procedures that read is INPUT, and for those
that write, OUTPUT.  If I/O is to be done on the
file TTY, it must be explicitly mentioned as the first
argument to READ, WRITE, etc.  Of course the files INPUT
and OUTPUT may be assigned to the terminal by specifying
TTY: as the file spec in the initial dialog.

In general TTY can be used with any of the read or write procedures.
Actually, however, this is somewhat of an illusion.
Internally, the file TTY is only usable for input, and the file TTYOUTPUT
is used for output.  The user need not normally be aware of this, as
all mentions of TTY in output procedures are automatically transformed
into TTYOUTPUT.  However, for obvious reasons, such
mapping cannot be done with buffer variables.  Thus should one wish
to work with the buffer directly, TTYOUTPUT^ should be
used for output.  TTYOUTPUT must also be used explicitly
with PUT and REWRITE.
Note however that TTY is directly connected with the user's
terminal via RDTTY and PBOUT.  REWRITE and RESET cannot be used to
alter this.

In standard PASCAL, RESET(file) does an implicit GET(file), so
that file^ contains the first character of the file immediately
after the RESET is done.  This is fine for disk files, but for
a terminal it makes things difficult.  The problem is that
RESET(TTY) is done automatically at the beginning of the program,
so the program would go into TTY input wait before you had a
chance to prompt the user for input.  To solve such problems, many
implementations allow you to specify a file as interactive.  Such
a specification keeps RESET from doing the implicit GET.
In this implementation, TTY is always interactive.  Other files
can be made interactive by specifying a nonzero third argument
in the RESET.  (The distinction is irrelevant for REWRITE, and
the third argument is ignored for REWRITE.)

For an interactive file, file^ will not contain anything useful until
you do an explicit GET.  To indicate this fact, the system
automatically sets EOLN(file) true after RESET.  Thus any program that
checks for EOLN and does READLN if it is true will work
correctly.  (This is done automatically by READ
with numerical and Boolean arguments.)

@section(Character Processing)
@label(charproc)

Any character except null (0) can be read or written by a PASCAL
program.  In the normal case, end of line characters appear in
the buffer (e.g. INPUT^) as blanks.  This is required by the
specifications of the Pascal language.  To tell whether the file
is currently positioned at an end of line,
EOLN should be used.  When EOLN is true, the
buffer should contain some end of line character (although 
what actually appears there is a blank).  To get to the first
character on the next line do READLN.  (If the next line is empty,
of course EOLN will be true again.)  This is done by the system
routine READ when it is looking for numerical input.

Note that carriage return, line feed, form feed, altmode, and control-Z
are considered to be end of line characters.  However, if the
end of line was a carriage return, the carriage return and everything
up to the next end of line (typically a line feed) is considered
a single character.

If it is necessary to know which end of line character actually appeared,
the user can RESET the file in a special mode.  When this mode is used,
the end of line character appears in the buffer unchanged.  You can still
tell whether the buffer is an end of line character by using EOLN (indeed
this is the recommended practice).  In this mode, carriage return is
seen as a single character, separate from the line feed.  However READLN
will still treat a carriage return and line feed as a single end of
line.  To be precise, READLN will skip to the
next line feed, form feed, altmode, or control-Z before returning. 
To open a file in the mode where you see the end of line character,
specify /E in the options string used in the RESET  (see section @ref(wiz)),
or in the case of INPUT being implicitly opened by the PROGRAM statement,
specify INPUT:#.  You may request the special file TTY to be opened in
this mode by listing TTY:# in your program statement.

Control-Z is also considered the end of file character for
normal files opened on terminals (but not the special file TTY, which
has no end of file condition).

Terminal I/O is done in such a way that control does not return
to the program
until ^G, ^L, ^Z, <alt>, <cr>, or <lf> is typed.  This
allows the normal editing characters ^U, ^R, <del>, etc., to
be used.  This is true with normal files open on terminals
as well as the file TTY.

It is possible to cause all lower case letters to be turned into
the equivalent upper case when they are read by your program.  To
set up this process,  specify  /U in the options string used in
the reset.  (See section @ref(wiz).)

@chapter(Extensions to PASCAL)

We have tried (somewhat unsuccessfully) to avoid the usual temptation of
Pascal implementors to add lots of features to the language.
The actual language extensions are limited to those
built into the base version as it came from Hamburg.  However
we have added many library procedures, mostly to
allow a wider variety of I/O and better access to the
system.  Only the most common of these have actually been
put into the compiler as predeclared procedures.  So several
of the procedures described below are noted as being external.
This means that you must include an explicit procedure declaration
for them, with the token EXTERN replacing the procedure body.
For your convenience, these EXTERN declarations have been
collected into a file called EXTERN.PAS.  It may be
included in your program by using the statement
@begin(display)

	INCLUDE '<PASCAL>EXTERN.PAS';

@end(display)
immediately after your PROGRAM statement.  <PASCAL> should be
replaced by whatever directory Pascal files usually reside on.
(<PASCAL> at Sri-KL, S:<PASCAL> at Rutgers)

@section(Input/Output to strings)
@label(strio)

It is often convenient to be able to use the number-scanning abilities
of READ to process a string of characters in an array of CHAR.
Similarly, it may be useful to use the formatting capabilities of
WRITE to make up a string of characters.  To allow these operations,
this implementation provides a facility to treat a packed array of
CHAR as if it were a file, allowing READ from it and WRITE to it.
This facility is equivalent to the REREAD and REWRITE functions
present in many implementations of FORTRAN.

To make use of this, you must use a file that has been declared
FILE OF CHAR.  Rather than using RESET or REWRITE to initialize I/O, you
use STRSET or STRWRITE instead.  These associate a string with the
file and set the internal file pointer to the beginning of the string
(in the simplest case).  A typical call would be STRSET(FILE1,MYARRAY).
After that call is issued FILE1 can be used with READ, etc., and will take
successive characters out of the array MYARRAY.
Similarly, one might do STRWRITE(FILE2,YOURARRAY), and then use
WRITE(FILE2,...) to write things into YOURARRAY.  Note that as with a
RESET, an implicit GET is done as part of the STRSET.  Thus immediately
after the STRSET, the first character of the string is in the file
buffer.

It is possible to start I/O at a location other than the beginning of the
array.  To do so, use a third argument, which is the index of the first
element to be transferred.  E.g. STRSET(FILE1,MYARRAY,5) means that the
GET will retrieve element 5 from MYARRAY.  (This is MYARRAY[5].  It
is not necessarily the fifth element, since the index might be -20..6
or something.)

There is a procedure to see where you currently are
in the string.  It is GETINDEX(file,variable).  Variable is set to the
current index into the array.  This is the index of the thing that will
be read by the next GET (or written by the next PUT).

Note that no 
runtime error messages will ever result from string I/O.  Should you run
over the end of the string, PASCAL will simply set EOF (or clear it if you
are doing output).  It will also set EOF if you read an illegal format
number.  (GETINDEX will allow you to discriminate these two cases, if you
care.)

There is also a fourth optional argument to STRSET and STRWRITE.
This sets a limit on how much of the array will be used.  It thus
gives you the effect of the substring operator in PL/I.
For example, STRWRITE(F1,AR1,3,6) will make it possible to change
characters 3 to 6 inclusive.  If absent, the fourth argument
defaults to the last location in the array.

Note that arrays of types other than CHAR can be used.  They
must be packed arrays, however.  (In order for an array to be
considered packed, the elements must take up a half word or
less.  You can declare an array PACKED ARRAY[..]OF INTEGER, but
it is not really considered packed.)

Of course the file and the array must have the same
underlying type.  (This is checked.)

Beware that it is possible to set a file to an array, and then
exit the block in which the array is defined.  The file is
then pointing out into nowhere.  This is not currently detected.


@section(Monitor calls)
@label(jsys)

For those daring souls who want to  have access to all the facilities of
the machine, it is possible to insert JSYS's into your program.
Although this is syntactically simply a call to a predeclared
runtime, it compiles code in line.  There are so many options that
this is best thought of as a sort of macro.

It has the following syntax:
jsys(jsysnum, extralocs, return; arg1, arg2, ...; result1, result2, ...)
The only required argument is jsysnum.  This must be an integer
constant, and is the number of the jsys to be compiled.
When arguments are left out, the associated commas should also
be omitted.  If an entire group of arguments is left out,
the semicolons should also be omitted, except that an extra
semicolon is necessary when there are no argi's and there are
resulti's, for example jsys(3;;i);

Extralocs should be nonzero to insert dummy instructions after
the jsys, in case it skips.  In the simplest case, the specified
number of jfcl's are inserted.  If extralocs is negative, the
first inserted instruction is an erjmp, and abs(extralocs)
instructions (counting the erjmp) are inserted.

Return allows you to know how the jsys returned.  It makes sense
only with nonzero extralocs.  Return must be an integer variable,
and will be set to a number indicating which return was taken,
1 for non-skip, 2 for one skip, etc.  If an erjmp is activated,
return will be set to abs(extralocs)+1, i.e. one larger than the
largest value normally possible.

Arg1 ... are expressions which will be put into registers 1 up.
There are a number of special cases:
@begin(itemize)
Simple variables and
expressions have their values loaded.

Sets have the first
word only loaded. (This lets you use the first 36 entries of
a set to set arbitrary bits.)

Files have their jfn loaded,
with index bits in the left half in the normal case.
(So use 0:file if you just want the jfn.)

Packed
arrays have a byte pointer to the first element loaded.

Complex variables (records, arrays, etc.) have their address
loaded.

To specify half words, use a colon between the
left and right half.
@end(itemize)
E.g. to print the most recent error on
the terminal you might use
jsys(11B,2;101B,400000B:-1,0)  The 400000B:-1 specifies that
register 2 will have 400000 octal in the left half and -1 in
the right half.  All the special cases defined above may be
used this way, though only the low order half word of the
values will be used, obviously.

Result1 ... are variables specifying where the contents of
registers 1 up should be put after the jsys is executed.
These are treated as above, except that "complex variables"
are not allowed.  (It is not in general clear what they would mean.)
The code may not work correctly if you try to return
registers above 4.  (5 and up are used as temporaries for
handling certain obscure cases.)

This monstrosity is compiled inline.  It is best thought
of as a macro, since the code generated depends quite
heavily on which arguments are supplied, and to a certain
extent on their values and types.  The code has a few
inelegancies, but is probably faster than calling even
a well-coded assembly language subroutine.  If you are
going to do I/O with jsys on a pascal file, you should
probably be sure that the file was opened in byte mode,
unless you really know what you are doing.  Needless to
say, this construct is "escape from Pascal".  I.e. there
is no protection against doing something with a jsys
that will clobber your program or the Pascal runtimes.


@section(INITPROCEDURE)

Variables of type scalar, subrange, pointer, array or record
declared in the main program may be initialized by an
INITPROCEDURE.  The body of an INITPROCEDURE contains only
assignment statements.  Indices as well as the assigned values
must be constants.  Assignment to components of packed
structures is possible if the components occupy a full word.

The syntax of an INITPROCEDUE is as follows (the parts enclosed
in [ and ] may appear 0 or more times):
@begin(display)


   <initprocedure> ::= INITPROCEDURE ;
		       BEGIN  <assignments> END;

   <init part> ::= <initprocedure> [ <initprocedure> ]

@end(display)
The <init part> must follow the variable declaration part and
precede the procedure declaration part of the main program.

Note that INITPROCEDURES do not compile into code.  Instead they
put the values specified into appropriate places in the .REL file,
so that the variables are initialized by loading the program.  This
means that you should not attempt to call an INITPROCEDURE.  It
also means that if you restart a program (e.g. by ^C-START),
the INITPROCEDURES will not be redone.  We recommend very
strongly that INITPROCEDURES only be used for constant data.

@section(Extended CASE Statement)

The CASE statement may be extended with the case OTHERS which
then appears as the last case in the CASE statement.  This case
will be executed if the expression of the CASE statement does
not evaluate to one of the case labels.

In the following example it is assumed that the variable X is of
type CHAR:
@begin(display)

   CASE X OF
	'A' : WRITELN('VALUE IS A');
	'B' : WRITELN('VALUE IS B');
	OTHERS : WRITELN('VALUE IS NEITHER A NOR B')
   END   %CASE STATEMENT\

@end(display)

@section(LOOP Statement)

The LOOP statement is an additional control statement which
combines the advantages of the WHILE and the REPEAT statement.

The LOOP statement has the following syntax:
@begin(display)

   <loop statement> ::= LOOP
		          <statement> [; <statement> ]
			EXIT IF <expression> ;
			  <statement> [; <statement> ]
			END

@end(display)
The expression must result in a Boolean value.  Note that there must
be exactly one EXIT IF in each LOOP.


@section(CLOSE, RCLOSE, and DISMISS)
@label(close)

It is often desirable to close a file during execution of a
program.  This may be needed to make it available to other
programs (under certain circumstances), or simply to
remove the possibility of accidentally changing it if you
know you are not going to use it again.  Thus the procedure
CLOSE is available.  The normal call is
@begin(display)

	CLOSE(file)

@end(display)
This closes the file, but does not release the jfn.  Thus
any future RESET, REWRITE, etc., of this file will use
the same jfn (i.e. name, etc.) as the one just closed,
unless a new file spec is given explicitly.  An optional
second argument may be given, CLOSE(file,bits).  It is an
integer, which will be used for accumulator 2 in the CLOSF
jsys.  (Only experts will need this.  Note that the bit
CO.NRJ is set automatically to keep the jfn from being
released.)  If the Pascal file variable is omitted for CLOSE, OUTPUT is
used as a default.

RCLOSE works just like CLOSE, but releases the jfn.  It
is not obvious what this is good for.  It also takes an
optional second parameter.

DISMISS closes the file, but rather than finishing out
the normal writing, it aborts the creation of the file.
If the file is on disk, it is expunged.  If it is on
tape, the normal writing of EOF's, etc., is skipped.

@section(MARK and RELEASE)
@label(markrel)

MARK and RELEASE can be used to organize the heap like a stack.
Both have one parameter which must be of type INTEGER.

MARK(X) assigns to X the current top of the heap.  The value
of X should not be altered until the corresponding RELEASE(X).

RELEASE(X) sets the top of the heap to X.  This releases all the
items which were created by NEW since the corresponding MARK(X).
Use of release is dangerous if any of the records released
contains a file.  DISPOSE of a record containing a file will
correctly close the file.  However RELEASE is a bit more
wholesale, and files will not get closed.

Note that MARK and RELEASE are probably not useful with programs
that use DISPOSE, since DISPOSE invokes a dynamic memory manager
that does not allocate the heap as a simple stack.

@section(I/O facilities for wizards only)
@label(wiz)

PASCAL has the ability to use the full I/O capabilities of
Tops-20.  This includes wildcards, file updating, etc.

Before we discuss the facilities available for I/O, it will
be helpful for the reader to understand the relation between
Pascal files and jfn's (the "handles" used to refer to physical
files).
A Pascal file variable secretly is associated with a jfn.
The jfn is assigned at startup if the file is listed in the
program statement.  It may be changed by supplying a new file
spec in RESET, etc., or by doing explicit jsys's.  If RESET, etc.,
is done without specifying any file spec, the jfn currently
associated with that file is used (if there is one - otherwise
a default file spec is used to generate a jfn, and the file is marked as
internal). This means that sequences of reset, rewrite, etc., all refer
to the same jfn, i.e. to the same version of the file, unless
a new file spec is given.  A few functions change the jfn
associated with the file.  RENAME replaces the old jfn with
a jfn associated with the new name.  NEXTFILE releases the old
jfn completely when you come to the end of the files associated
with the file spec.  There is nothing I can do about this, because
of the way the monitor is built.

@subsection(Extra arguments to RESET, etc.)
@label(open)

Most of the options available for I/O are specified in arguments to
RESET and REWRITE.  The full form of these procedures includes
the following arguments:

@begin(display)

     RESET (<file identifier>,<file spec>,<interactive?>,
		<gtjfn bits>,<openf bits>,<flags>)

@end(display)

You may omit trailing parameters.  (They are taken as 0).
As a convenience for typing bits and flags, a set may be used
as the argument for <gtjfn bits>, <openf bits>, or <flags>.
The first word of the set is passed.  Fortunately, the Pascal
representation of a set of integer is such that [n] produces
a word with bit n turned on.  E.g. to specify bits 3 and 5
(1B3!1B5 in Macro), one can use [3,5].  

If a file spec is given, any existing jfn will be
released, and a new one gotten for that file spec.
The file spec may be followed by :*@@.  The *@@ are
each optional, and may appear in either order.
* indicates that wildcards should be allowed in the
spec.  @@ indicates that the runtimes should do a
gtjfn from the terminal.  Confirmation and messages
will not be turned on by default, but you may set the bits gj%cfm
and gj%msg yourself in the gtjfn bits.  + and - are not
needed as in the program statement, since the runtimes
know whether the file is input or output.  Effectively
+ is used for rewrite and - for the others.  To omit the
file spec, type ''.  (To get a spec from the tty:,
you normally supply '':@@ )

The form shown above is the old, full form of the RESET.  Because
no one (including me) could remember which bit is which, a
new form is provided which allows you to set the most useful
bits by the use of switches.  To do this, pass a string for
the third parameter, e.g.

@begin(display)

	RESET(F,'A.B','/I/E')

@end(display)

Here are the meaning of the switches.  For details on what they
do, you will have to look below where the bits that they set are
described.  Note that you can mix the two notations, i.e. use a
string for the third parameter and then go on to set bits in the
later parameters.  The bits set in the two ways are or'ed together.

@begin(description)
/B:nn	Byte size specification.  The number specified goes into the byte 
size field of the OPENF word.  It is mainly useful for handling
industry-compatible magtape, wherein 8 bit bytes are useful. For details
about the meaning of the byte size, see section @ref(iomode).

/D	Data transmission errors will be handled by the user.  See
the section below on error handling (section @ref(errors)).  A data
transmission error is usually a physical problem of some sort.
See /F for problems with the format of the data.

/E	End of line characters will be visible to the program.  Normally
Pascal programs put a blank in the input buffer at the end of line.
If this flag is set, the actual end of line character appears in the
buffer.  Normally a single GET will read past both a carriage return
and a line feed, since they are a single line terminator.  But if /E
is set, the carriage return and the line feed will be treated as
separate characters, although READLN will still skip them both.

/F	Format errors in the data will be handled by the user.  See
the section below on error handling (section @ref(errors)).  A format
error occurs when the data is readable, but is not what READ wants.
E.g. when trying to read a number, a letter is found.

/I	Interactive file.  The meaning of this has been discussed above.
It keeps the system from reading the first component in the file, as
it normally does whenever a file is opened.

/M:nn	Mode.  This allows you to specify the internal software mode
that will be used in processing the file.  See section @ref(IOMode).

/O	Open errors will be handled by the user.  See
the section below on error handling (section @ref(errors)).  An open
error is an error that occurs during the RESET, REWRITE, etc.  Most
commonly it is when the specified file is not present or a protection
problem (e.g. you aren't allowed to read the file).

/U	Upper case the file.  All lower case letters will be turned
into the equivalent upper case.  Only letters are affected.
@end(description)

The normal user may skip to the end of this section now.
The other parameters are intended mainly for use by
hackers.

The 3rd parameter suppresses implicit Gets, as for Tops-10.
It is not used for rewrite, as protection is put in the
file spec following usual Tops-20 conventions.  Normally
you should specify 0 for the gtjfn and openf bits.  The
runtimes will supply those bits needed to carry out the
operation.  However if you do specify nonzero bits, they
will be xor'ed into the bits supplied by the runtimes, except
that certain bits will be ignored (e.g. those that control
whether it a long or short form gtfjn).  The bits supplied
by default are: for reset, update, and append: gj%old, gj%flg, gj%sht;
for rewrite:  gj%fou, gj%flg,  gj%sht.  Those bits
that may not be changed by the user are gj%flg, gj%sht, gj%jfn,
gj%ofg, and gj%xtn.

Currently the flag word is used for the following:
@begin(description)
left half - buffer or record size, in bytes.  See Section @ref(IOmode)

bit 1 - map lower case to upper

bits 16 - control of error processing.
Error processing will be explained in the next section.

bits 7700 - number of buffers or pages for buffering.  See section
@ref(IOmode)

bits 770000 - I/O method.  This allows you to override Pascal's
usual method of handling I/O.  See Section @ref(IOmode)
@end(description)

@subsection(Labelled tape processing)

Tops-20 Pascal has fairly powerful facilities for dealing with labelled
tapes.  These facilities use Tops-20's special labelled tape support,
so they are not yet available for Tops-10.  To read a labelled tape,
you normally do not need to do anything special.  However when you
want to write a labelled tape, you often may want to specify exactly
how the file is to be written.  To do this, you include "attributes"
as part of the file name.  Here is a typical file name that
uses attributes to control the format of a file on tape:
@begin(display)
	MT0:DATA;FORMAT:F;RECORD:80;BLOCK:8000
@end(display)
This entire string is considered to be the file name.  You
can type such a string when you are asked about the files
in the program statement.  Or you can supply such a string
as the second argument to a REWRITE statement.  This particular
string asks for a file called DATA to be written in fixed format,
with records of length 80, blocked 100 per block.  Pascal will
fill lines that are shorter than 80 characters with blanks.

The record format is described by ;FORMAT:x, where x is a single
letter.  The following formats are supported by Pascal:
@begin(description)
U (undefined)	This is the default for output files.  If this is
what you want, you do not need to use any attributes at
all.  "MT0:" alone would be sufficient. Pascal
assumes that a file in format U has the same structure as
a disk file.  That is, end of lines are denoted by carriage
returns, etc.  Pascal will ignore physical record boundaries on the
tape.  If you do not do anything special, such files can be
copied back and forth between disk and tape using a simple
COPY command in the EXEC.  You might want to specify a block
size in order to make more efficient use of the tape.  E.g.
"MT0:;BLOCK:5120".  Tapes written in format U will probably not
be readable on computers other than a DECsystem-10 or DECSYSTEM-20.

D (variable)	This format is useful for text files that you want
to be able to move to other computers.  This format uses special
record headers to show where lines begin and end.
Most other computers understand these headers, but do not
understand the normal DEC convention of using carriage returns to end
a line.  Also, tapes are coded one character per tape frame, which is
what other systems expect.  Unless you really know what you are
doing, you will only be able to use D format for text files
(files declared TEXT or FILE OF CHAR).  To use this mode, you
should specify something like "MT0:;FORMAT:D;BLOCK:5000".
The block size should be chosen large enough to make reasonably efficient use
of tape, but not to waste memory.

F (fixed)	This format is also usable to communicate with
other computers.  In it there is nothing on the tape
to show where lines end.  However all lines are same length.  Thus
system can find the end of a line by counting characters.  These
tapes are also coded one character per tape frame.  The example
above showed how to specify format F.  You should specify both a
block size and a record size.  Pascal will fill out all lines
to match the record size, by putting blanks at the end of each line
that would be too short.  The system will put as many records into
one physical tape block as it can fit there. The block size must be
an even multiple of the record size.  Again, the block size should
be big enough not to waste tape.  Unless you are an expert you
will only be able to use this mode for text files also.

S (spanned)	This is a somewhat unusual mode.  It is very
similar to mode D.  However the record headers are written in
such a way that records can cross block boundaries.  This mode
makes somewhat more efficient use of tape, but is more complex
to process.  Many other computers do not support it.
@end(description)

When you are reading a labelled tape, Pascal can tell the
structure of the tape from the label.  Thus you should not
have to specify any attributes for an input file.

In addition to the tape format, which is describe by the
FORMAT attribute, there is also a distinction between
"stream I/O" and "record-oriented I/O".  Stream I/O is
the normal style on Tops-20.  With it, line and record
boundaries are indicated in the data, e.g. with carriage
returns.  The tape records are completely ignored by
the program.  Record-oriented I/O is used when the
tape records should match the line or record structure of
the file.  In record-oriented I/O, there would usually
be no carriage returns in the file, since the end of
line would be indicated by the record structure.  By default,
Pascal uses stream I/O for format U and record-oriented
I/O for the other formats.  You can override this default
by specifying '/M:7' in the option string for record-oriented
I/O and '/M:5' or '/M:6' for stream-oriented I/O.  See
section @ref(IOMode) for the difference between these two
modes.  

@subsection(I/O Error processing )
@label(errors)

Errors may occur at three different times.  Each of the resulting
three error types has a bit associated with it in the
flag parameter to RESET, etc.  The types (and associated bits) are
@begin(description)
@ 2B errors during actual I/O operations (get, put, ...)

@ 4B errors during number conversion (read, write)

10B errors during file opening (reset, rewrite, ...)
@end(description)

When an error occurs, the appropriate bit in the flag word is
checked.  If it is 0, a fatal error message is printed, using the
erstr string.  If it is 1, the error number (from geter) is stored
where you can get it, EOF (and EOL) is set to show you that you
can no longer read or write, and the program continues.  Normally
future I/O operations become no-ops until you recover the error.
The runtimes are constructed so that the high-level read  routines (READ
and READLN) return immediately with no effect when EOF is set.  Other
routines proceed as usual, but usually the monitor sees to it that
they have no effect.  

The moral is, if you set one of the magic bits, you had better check
EOF (at least, probably also ERSTAT) now and then.  Note that if you
set bit 10B (open errors) but not bit 2 (I/O errors), the open error
will generate a fatal I/O error at the first I/O operation, since
the monitor refuses to do I/O with a file that has not been successfully
opened!  The EXTERN function ERSTAT(file) [section @ref(miscIO)]
may be used to look at the most recent
monitor error code for a given file.

PASCAL does not attempt to do any error recovery beyond printing
error messages and saving error numbers.  It is assumed that users
who specify user error processing do not want us interfering.  In most
cases I/O operations in which an error occurs are no-ops.  Thus if a
get fails, a succeeding get will try to read the same character.  Of
course in most cases this will again fail, so the user will want to
advance the file (SETPOS [section @ref(randac)] for disk,
some mtopr function using JSYS
for tape).  Note however that in the case of record I/O, some of the
record may have been read successfully.  In this case GETX [section
@ref(varrec)] will
attempt to reread the tail of the record (and again will usually fail).
I.e. reading will begin with the byte for which it blew up before.

The monitor has a restriction that no I/O is possible when an error bit
is set in the file status word.  Thus in general one will want to clear
this error indication before doing any retry or other recovery.  Thus
the EXTERN routine CLREOF(file) [section @ref(miscIO)]
both clears the internal Pascal EOF indication
and does a STSTS jsys to clear the error and EOF flags in the monitor.
Note that if the error is a real end of file, it can be cleared by
doing a random access SETPOS [section @ref(randac)]
to a byte number within the file.  CLREOF
is not necessary to clear an end of file condition before random
access.  (SETPOS will not clear an error other than end of file,
however.)

@subsection(I/O implementation )
@label(IOMode)

In order to get both efficiency and convenient operation, Pascal
has to have a set of device-dependent I/O routines.  Unless you
override it, Pascal will use a routine that gives the best performance
for the device type that each file is on.  However, you can
explicitly specify which routine you want used for a given file
by putting the number of the routine in bits 770000B of the flag
word in Reset, etc.  If these bits are 0, Pascal will use its
own choice.  In the sections below, we describe each of the routines,
by number.

In all cases, a file has associated with it a "logical byte size".
If the user specifies a byte size in the openf flag word, that size
is used.  Otherwise, 7 bits is used for text files, and 36 bits for
record files.  In most cases the file is opened using the logical
byte size.  However in a few cases full word I/O is done and the
logical byte size is simulated by getting bytes of the specified
size from Pascal's I/O buffer.

Note that several of these routines use buffering.  The overhead for
a monitor call is sufficiently high that it is very expensive to
get each byte separately.  So buffering involves getting a large
number from the monitor at once and then giving them to the user
one by one.
Or for output, collecting bytes in a buffer and sending them to the
monitor at once when the buffer is full.  While it improves efficiency,
this method introduces two kinds of timing problems.

First, sometimes
the user will want to synchronize logical and physical I/O.  For
example, if you rewind the tape, you will want to be sure that any
bytes built up in the buffer are put out before the rewind takes
place.  Similarly, after a rewind, you will want to be sure that any
bytes read come from the beginning of the tape, rather than being
left over in the buffer from before you did the rewind.  Thus there
are two routines for clearing the buffer:
@begin(description)
BREAK(file) - forces out any bytes in the buffer for output

BREAKIN(file) - clears any leftover bytes in the buffer for input.
Afterwards, does an implicit GET, unless it is suppressed by
BREAKIN(file,true)
@end(description)
The details about these routines depend upon the exact method used
for buffering, so they are described in detail below for each of the
buffered modes.  Note that BREAK and BREAKIN should not be used
with SETPOS (random access movement [section @ref(randac)]), as SETPOS
does its own synchronization.

The other synchronization problem involves errors.  In byte by byte
I/O you get an error exactly when it happens.  Then there is no
timing problem - the routines simply set EOF or print an error
message immediately.  However in buffered
I/O, errors will usually happen when the routines are in the middle
of filling a buffer.  The error cannot be acted on immediately,
because some valid data has been gotten.  So the end of file, or
other error, is delayed until all of the valid bytes have been given
to the user.  The problem is that BREAKIN and SETPOS (random access
positioning[section @ref(randac)]) essentially throw away the
remaining bytes in the buffer.  Thus they have to cause any saved
error to be triggered immediately.  (It would be misleading to
wait and give an old error when you tried read a completely different
file after a rewind.  But you have to be told about the error,
since further processing is impossible with some devices until the
error is cleared.)  Thus you should not be shocked to find that
BREAKIN and SETPOS, which are essentially bookkeeping operations,
can cause I/O errors to be triggered and set EOF.

Certain I/O methods allow you to specify the size buffer you want
used.  This will be mentioned in the description below if it is
applicable.  The specification takes bits 7700B in the flag word.
This argument is ignored by methods that do not implement it.
Details are given under the descriptions of those methods that
implement this option.  (It is ignored for other methods.)
At the moment it is implemented only for Pmap'ed mode.  Note that
this specification affects only the efficiency of your program.

@paragraph(Byte mode[1])

This routine does bin and bout jsys's for text I/O, and sin and
sout jsys's for record I/O.  No internal buffering or other
processing is done: each GET of a character is a BIN, etc.  This
avoid the various timing problems mentioned, but is unbelievably
slow for text files and small records.  For large records it may
not be too bad.  This
method is used only for devices for which Pascal can't figure out
anything better, i.e. all devices for which no special routine is
listed below.  You should specify this mode explicitly whenever
you wish to do your own I/O jsys's intermixed with Pascal I/O, unless
you are prepared to give some care to transition between Pascal
I/O and your own.  (I.e. BREAK, BREAKIN, etc.)

With this routine, the file is opened using the
logical byte size and all operations utilize byte pointers of  that
size.  Break and breakin are meaningless.

@paragraph(Pmap'ed mode[2])

This routine attempts to handle disk I/O is the optimal way.  Pages
from the file are Pmap'ed directly into a buffer within Pascal.
This makes performance particularly good for random access updating,
but is always the best way to handle the disk.  When it can, Pascal
will always use this routine for disk files.  However it will not be
able to do so when you are writing or appending to a file to which
you do not have read/write access.  (In that case buffered mode, by
bytes, is used.)  This routine will work only for disk.

Should you
wish to do some jsys of your own with a file open in pmap mode,
you may want to do BREAK(file) or BREAKIN(file).  Break will
unmap the file from the buffer, and would normally be used after
doing Pascal I/O to allow you to play with the file more freely.
Breakin clears Pascal's internal entries about the file, and should
be done before doing more Pascal I/O.
(Breakin also returns you to 
the beginning of the file, so be careful.)  Note that these are
somewhat odd definitions of BREAK and BREAKIN.

In this mode, the file is opened using the logical byte size.  However
the pmap jsys, which this uses, ignores the byte size, giving whole
words.  The logical byte size is simulated by the routines that
remove and add bytes in the buffer.

The efficiency of I/O is affected remarkably by the size of the buffer,
i.e. the number of pages mapped at one time.  By default, PASCAL will
use 4 pages.  We believe that almost all users will be satisfied
with this default.
However you can specify the buffer size yourself, 
using the field 7700B in the flag argument to RESET, REWRITE, etc.
What goes in this field is the number of pages used for the buffer.
For example to open INPUT with 16 pages of buffering, you might
use RESET(INPUT,'',0,0,0,16*100B).  Multiplying by 100B is a
convenient way to move the value into this field.

If you are doing a lot of random access it might make sense to specify
1 or 2 pages, and if you are doing sequential I/O with very large
record sizes, it might make sense to specify up to 16 pages.  (Sizes
over 16 pages do not improve performance noticeably.)  
There is an absolute maximum
of 36 pages.  If you declare a buffer larger than 36 pages, 36 will
be used.  

Note that very large buffer sizes only make sense in very special
cases, usually involving binary files with very large records.
Programs that process text files will probably not benefit from
buffer sizes larger than the default.  There is enough CPU time
involved in handling individual characters that such is program
will never be heavily I/O bound.  Programs that do only random
access I/O may perform slightly better with 1 or 2 page buffers,
since preloading a bigger buffer is just a waste of time.  Also,
DEC suggests that very small machines (smaller than 256K?) may
be better off in using small buffer sizes.  (If you find this
to be the case, you can change MAPBFS at the beginning of PASIO.MAC
to 1 for your site.)

@paragraph(TTY mode[3])

DEC has supplied a standard "line editor" for use with the
terminal.  This is what carries out ^U, ^R, rubout, etc.  Most
users will want this line editor activated when a program is
reading from the terminal.  Thus Pascal will handle all terminals
in TTY mode.  This routine uses the TEXTI jsys, which calls the
standard DEC line editor.  With this routine in effect, you can
use rubout, etc., until you type an "activation character"
(usually carriage return).  At that point the entire line is
passed to the program.  The complete list of activation characters
is: <cr>, <lf>, altmode, ^G, ^L, and ^Z.  Also in this mode when you
type a ^Z, end of file is generated.  For output, the bin jsys is
used, so that characters appear on the screen immediately when you
output them.  (I.e. no buffering is done.)  This is somewhat
inefficient, but avoids requiring BREAK whenever you want output to
show up immediately.
This routine can be used for any device, but there is no obvious use
except for a device where TEXTI provides editing.

For this routine the file is opened with the logical byte size.
However the bytes are packed into the buffer as 7 bit bytes, so it
is not clear what effect a byte size other than 7 bits has.  If
this mode is specified for a non-text file (i.e. record I/O),
byte I/O is actually used. 

Since input is buffered, the comments above about buffering hold
for the input side.  I.e. BREAKIN would be needed if you were using
this with tape and did a rewind, and errors are delayed as described
above.

@paragraph(Null mode[4])

This routine is used only with device NUL:.  It does nothing on output,
and simulates end of file on input.  It is far faster than actually
using the I/O jsys's for device NUL:.  It should not be used for
other devices unless you wish them to do no I/O.  Random access is
legal, but has no effect.  CURPOS always returns position 0.

@paragraph(Buffered mode, by word[5])

This routine is designed to handle devices that are capable of binary
I/O as efficiently as possible (except for disks, which are handled
a bit more efficiently by Pmap mode).  It is legal only for transfer
in a single direction.  I.e. Update is illegal.  It uses a one page
internal buffer to store up characters as they are being input or
output by the program.  When the buffer fills (output) or is
emptied (input), a single SIN or SOUT is done to transfer the whole
buffer.  The comments above about buffered I/O all apply (BREAK,
BREAKIN, and delay of errors on input).
Random access is illegal in this mode.

Note that the device is opened for 36 bit bytes in this mode.  This
makes transfers a bit more efficient, and allows the runtimes to
detect line numbers (which cannot be seen in modes where the file is
open for 7 bit bytes).  The byte size specified by the user is
simulated.  Thus as far as the user is concerned his byte size was
used.  However this means that if the user has specified a record
size for a magtape, his number will be taken as the number of words per
record.  It also means that transfers will be rounded up to the
next integral word.

This mode is used by default for magnetic tape drives.

@paragraph(Buffered mode, by bytes[6])

This mode is similar to buffered mode by words.  Transfers are still
buffered by a one page buffer.  However the file is opened using
the byte size specified by the user, and the actual transfers are
done in terms of such bytes.  In most cases this is not an advantage,
so this mode is used by default only for devices that I suspect
may act strangely when a byte size of 36 is specified: line printer
and card reader.  It is possible to do random access in this mode
if it is specified for disk files, however you are limited to
unidirectional transfers (no Update), so it seems to have no
advantage for disk.  The comments above about buffering apply to
this mode (BREAK, BREAKIN, and delay of errors).

This mode is used for disk files when append
or rewrite is specified and the user does not have read/write access
to the file.  

@paragraph(Record mode [7])

This mode is intended mostly for use with variable-size records on
magtapes, or with labelled tapes.  What it does depends upon whether
you are dealing with a text file or a binary file.  With a text
file, each line is a record on tape.  With a binary file, each
Pascal record is a record on tape.

With binary files, this is the same as byte mode except
that SINR and SOUTR are used for record I/O rather than SIN and SOUT.  
Each GET reads one
record from the tape into <filename>^.  If the record on the tape is
shorter than the size of the Pascal record into which it is being read,
the extra bytes in the Pascal record are not changed.  If it is longer,
the extra bytes on the tape are lost.  In any case, each GET reads a
new record.  To see how many bytes were transferred, call the function
LSTREC(file) [See @ref[lstrec].]

Note that the record size specified to the monitor (in
SET RECORD command or mtopr jsys) sets a maximum to the record size you
can transmit.  As long as that maximum is large enough, it has no other
effect.  

Each PUT writes one record from the Pascal file buffer 
<filename>^ onto the tape.  See @ref[varrec] for how to specify the
size of the record.  If you do nothing special, the record size will
be that of the Pascal record.  (If variants are involved, it will be
the size of the biggest variant.)  Note that if a byte size other than
36 is specified, the monitor will pack the bytes into full words, and
round the number of bytes written up to the next highest full word.
Also, there is a minimum record size of 4 words.  

If you are dealing with text files, each line will be made into
a record on tape.  Thus WRITELN will force out a record, and
EOLN will be true when you reach the end of an input record.
Note that no explicit end of line characters occur when using
this mode.  Thus '/E' as an option has no effect in this mode.
To specify the structure of a tape file, use "attributes". For
example, "MT0:;FORMAT:F;RECORD:80;BLOCK:800" will result in fixed
records of 80 bytes, blocked 10 to a block. Pascal makes every attempt
to implement all meaningful combinations of attributes.  You can often
use attributes that will not work in the EXEC's COPY command.
When format F is in use, Pascal will fill records with blanks to
reach the specified record size if necessary.

Note that even in Pascal you cannot write an EBCDIC tape.  This
is a limitation of the monitor about which Pascal can do nothing.

Note that the I/O procedures GETX and PUTX have no obvious meaning
for tape I/O and are illegal in this mode.

The mode for tape I/O is chosen as follows:  If you
have specified an I/O mode, that mode will be used.  No other
defaulting will be done.  In this case, the record format will
be chosen by the monitor if you have not specified it in an
attribute.   If you have not specified an I/O mode, then
the Pascal magtape default module is called.  The defaulter
must first find the record format.
For an input file, the format is gotten from the tape label.
For an output file,  Pascal first checks to see if you have specified
;FORMAT in the filename.  If so, the format you specified is
used. If you do not, output files are always written with
format U.  Once the format is known, the defaulter can then
choose the I/O mode.  For format U, a different mode is used for
output and input.  Output uses buffered mode, by words.  Input
uses buffered mode, by bytes.  The usual byte size is used 
(7 for text, 36 for binary, or whatever you specify by '/B:nn'.)
For all other formats, record mode is used.  Note that
record formats other than U may be a bit hard to use with binary
files.  DEC forces 8 bit bytes internally for formats other than
U.  Pascal uses bytes in a way that may have turned out to be
wrong:  each word of a binary data structure is written as
one byte.  This is fine with format U, with a byte size of
36.  But if you try to use it for one of the other formats,
the low-order 8 bits of each word will get written in a
tape frame.  This may not be what you want.

@paragraph(Other modes)

I am currently considering how tapes should be handled.  I suspect
that some additional modes for IBM format tapes will
be added eventually.  Note that if you specify a number corresponding
to an unimplemented mode, byte mode will be used.

@subsection(A note on byte sizes in files)

The documentation above describes I/O as occurring in bytes.
On the DecSystem-20 a word contains 36 bits.  It may be divided
into smaller units called bytes.  The bytes will be left justified,
and will not be split across words.  Thus 7-bit ASCII text is
stored in 7-bit bytes, 5 to a word, left justified.  I/O gets
bytes from the monitor (possibly with intermediate buffering by
the runtimes, for efficiency)
one at a time.

There are two types of PASCAL I/O: text and record.  Text I/O is
what you get when you use TEXT or FILE OF CHAR.  The PASCAL
runtimes assume that every GET from a text file returns one
ASCII character (and that every PUT puts out one character).
Internally GET just gets one byte from the file.
Thus everything works nicely for the usual kind of file, assuming
you accept the default byte size of 7 bits.  Since the usual file
has characters packed 5 to a word, getting one byte out of the
file does indeed get one character.  However, you can change
the byte size.  If you used a byte size of 8 bits with a normal
file, there would obviously be trouble, since the 5 characters
stored in each word would be distributed over 4 bytes of 8 bits each.
The usefulness of a byte size of 8 is for industry-compatible magnetic
tapes.  Since these tapes in general contains 8-bit ASCII or EBCDIC,
the monitor packs 8-bit bytes 4 to a word in the monitor buffer.
In the case of ASCII the high-order bit of the byte is parity, and
may be ignored.  So to read such a tape one must (explicitly or
implicitly) specify a byte size of 8 bits, so that when GET gets
a byte the byte is really one character.  A byte size of 36 bits
would make sense for text files only in some weird case
where you have data packed one character per word in the file.  That is
because a byte size of 36 bits means that each GET returns one
word, with no unpacking.  Because text I/O is assumed to involve
ASCII characters, each byte is truncated to the 7 low order
bits immediately after input.  Thus in case parity is included as
an 8th bit, it will not mess up the runtimes.  Should you need to
see the parity, you will have to make other arrangements,
probably by using record I/O with a record type PACKED ARRAY OF 0..377B.

Record I/O is any I/O not involving a FILE OF CHAR.  In this case
data is transferred from the monitor buffer to the PASCAL file
buffer (FILE^) by putting each byte gotten from the monitor
into a separate word in the PASCAL record.  Thus a byte size of
36 bits causes the PASCAL record to have the same structure as
the file.  If a byte size of 7 bits were used, each 7-bit byte
in the file would be moved to a separate word in the PASCAL
record.  Thus it would be appropriate if the file is a usual text
file, but the PASCAL record is an unpacked array of CHAR.
Note that the I/O runtimes do not check the byte size to be sure
it makes sense.  So it would be perfectly possible for you to
use a byte size of 7 bits to read into a PACKED ARRAY OF CHAR, even
though that would make no sense.  It makes no sense because it
causes one 7-bit byte from the file to be put in each word of
the PACKED ARRAY.  But a PACKED ARRAY OF CHAR should have 5 characters
in each word.  Except for special effects, one usually uses a byte
size of 36 for record I/O.  Then each input word is moved into
a word in the record, and you can do what you like with it.

You can now understand why the default I/O modes use 7 bit bytes
for text I/O and 36 bit bytes for record I/O.


@subsection(Variable Record Formats)
@label(varrec)


Standard PASCAL has a problem when it tries to read files created by
non-PASCAL programs.  Every call to GET or PUT transfers a fixed number of
words and puts it into the buffer variable.  This is fine for files
whose records are all the same length and format, but for other files
it is a mess.

To avoid these problems, we have extended the format of the GET and PUT, to
allow GET(<file>[,<variant>]*[:<array size>]), or the equivalent for PUT.
If the file type is a variant record, you may use FILE,VAR1,VAR2... to
specify the exact variants.  This is exactly like the syntax for NEW, as
documented in the Revised Report.  Furthermore, if the file is an array,
or the selected variant ends in an array, you may specify the number of
elements in the array to be used.  For example we might have
@begin(display)

  TYPE REC=RECORD CASE BOOLEAN OF
	    TRUE: (INT:INTEGER);
	    FALSE: (J:BOOLEAN,K:ARRAY[1:100]OF INTEGER)
	   END;
  VAR F:FILE OF REC;
  BEGIN
  RESET(F,TRUE);  %TRUE to prevent the implicit GET\
  GET(F,TRUE);  %TRUE to select the variant "TRUE"\
  GET(F,FALSE,5);
  GET(F)
  END.

@end(display)
The first GET would read one word of the file, since the variant
TRUE requires only one word.  The second GET would read 6 words of the
file.  One word is for the Boolean J, and 5 for the first five elements
of K, since the argument 5 specifies that only 5 are to be used.
The final GET would read 101 words from the file, which is the space
required for the longest possible variant.  Note that the argument after the colon is an index into the array. (I.e. if the array is [-5:1], 1 means all 7 members of the array.)

If you do not know in advance the size of a record being read, there are
two ways to handle the problem.  If you know you will always be using
mag tape, you can open it in mode 7 (record mode).  Then each GET will
get exactly one record, and you can then look at it to see what kind it
is.  (You can use LSTREC [See @ref[LSTREC].] if you can't tell the size
of the record by looking at it.)  However on other devices there is no
builtin record structure, so you must usually depend upon reading an
initial piece of the record that tells you either the record type or
length.  Then you know how long the record is, and can read in the rest.

For example, the record might begin with a type code.  Obviously we have
to read the code before we can specify how to read the rest of the record.
Thus there is a procedure, GETX, to continue reading a single record.
With GETX, the data transfer begins when the previous GET stopped, rather than
at the beginning of the record.  Suppose our record is a simple array of
integers.  Then we might do GET(file:1) to read a length code, and GETX(file:L)
to read the rest of the record.  Note that after the GETX the code is still
present in the buffer as the first member, so the L counts it.
GETX is an alternative to using "record mode".  It is not meaningful to
use both methods of detecting record length at the same time.  (Indeed
GETX will result in an "Illegal function" error in record mode.)

Also, sometimes you will need to know how much space a given
variant will take up.  Of course you can calculate this if you
know the way PASCAL allocates memory.  But it is more elegant to
let PASCAL do the calculation for you.  RECSIZE(file) will return
the number of bytes in a record for the file.  All the variant
and length options of GET and PUT can be used, so that the size of
any version of the record can be calculated.  E.g. RECSIZE(file,TRUE)
returns the size of the TRUE variant.

In principle, GET, GETX, and other functions listed later, provide all
the facilities needed to do arbitrary I/O with variable record sizes.
However it is clear that they are not very convenient.  The
runtimes have been structured in such a way that it is easy
to add what IBM would call "access methods".  I.e. one could
add a set of runtimes that know how to read and write EBCDIC
tapes, and have them called by the normal GET and PUT.  To
do so one would simply add a procedure USEEBCDIC(file) that would
change a dispatch vector so that all I/O for the specified
file used the EBCDIC routines.  No such extra methods
are currently written, but we expect they will be eventually.

@section(RENAME)
@label(rename)

RENAME(file,newname) may be used to rename an existing file.
The newname argument is treated the same way as the name
argument for RESET, etc.  It specifies the new name for the
file.  If the operation works, EOF is set to false, otherwise
to true.  Of course unless you have set user error handling
for this file, you will get a fatal error message if it fails.
This procedure does RNAMF.  The original file should at least have its jfn in
existence.  Usually you will have done RESET, etc., on it.
It may be closed or not.  (RENAME closes it if necessary.)
As a sideeffect, the jfn of the original file is released,
and the current jfn of the file is set to one
associated with the new name.

RENAME to a blank name will not delete a file (as it did in
Tops-10).  Use DELETE instead.

@section(DELETE )
@label(delete)

DELETE(file) will delete the mentioned file.  It need not
be open, but must have a jfn associated with it.  (This
will be the case if it was mentioned in the PROGRAM
statement or if RESET, REWRITE, etc., were done on it.)
It need not be closed, as DELETE will close it first
if necessary.

@section(UPDATE)
@label(update)

You may open a file for updating by using the runtime UPDATE.
When a file has been opened for UPDATE, both GET and PUT
(or READ and WRITE) may be used with it.  A single position
is maintained within the file, which is advanced by both
GET and PUT.  There is also a special routine, PUTX, which
will rewrite the last record read by GET.  It is exactly
equivalent to repositioning the file to the place it was
at the beginning of the last GET and doing a PUT.

Update has the same arguments as RESET.
However, the third argument is
ignored, as files are always opened by UPDATE in "interactive" mode.

When you have opened a file with UPDATE, you can then use
the procedure PUTX, in conjunction with GET.  To update a record,
you would do GET to read it, change the contents of the PASCAL
buffer variable corresponding to the file involved (i.e. FILE^),
and then do PUTX(FILE).  Note that PUTX takes only one argument,
the file.  It always rewrites exactly the same record as was
read by the last GET (or the whole record read by a combination
of GET and GETX  -- See Section @ref(varrec))

Note that EOF is normally false for an update file.  It is
set to true if an error occurs (and the user has specified
user error handling) or you try to read beyond the end of
file.  It is perfectly legitimate to write beyond the end
of file, however.  Doing so extends the size of the file
to include what is written.


@section(Random access)
@label(randac)

It is possible to move around a file randomly when it is on a
direct-access device (i.e. disk or some equivalent).  The procedures
that implement this use a byte serial number to keep track of their
position.  I will henceforth call this the "position".  This number
refers to the number of bytes between the beginning of the file and
the record in question, so the first record begins at position 0.
The position is absolute within the file, so if one or more pages are
missing from the file, gaps are left in the position numbers.
Note that the unit of measure is the byte.  This corresponds to one
word in the PASCAL buffer.  However in the file itself the bytes may
be packed, depending upon the I/O mode in which it was opened.
TEXT files (FILE OF CHAR) are stored 5 bytes per word by default.
Other files are one byte per word by default.

CURPOS(FILE) returns the current position index of the file.
This is the position at which the next record
to be read or written would begin.
When a file has just been opened, the position is, of
course, 0.
Note that CURPOS may give an error if used with a file not on disk.
CURPOS is a builtin function.

SETPOS(F,B) sets things up so the next GET(F) or PUT(F) will get or put the record that
starts at position B.
If the file is an input file, SETPOS does an implied GET.  To suppress
this implied get, use an extra nonzero argument, e.g.
SETPOS(F,6,TRUE). SETPOS is also a builtin procedure.

If you SETPOS to a position beyond the end of file, the next
read will set EOF.  If you really wanted to read, you should
SETPOS to a more reasonable position.  This will reset EOF.
It is perfectly legal to write
beyond the end of file, however.  Doing so extends the length
of the file.  If you SETPOS to a position beyond the end of
the file, it is possible to leave nonexistent pages between
the old end of file and the newly written part.  These should
cause no trouble to PASCAL (such pages are treated as full of
binary zeroes), but may for programs written in other languages.


@section(APPEND)
@label(append)	

Occasionally one wishes to append new data onto the end of an
existing file.  The monitor has facilities for doing this without
recopying the existing data.  Proper use of these facilities also
allows one to append data to an append-only file.  The procedure
APPEND implements this facility in PASCAL.  It has exactly the
same parameters as REWRITE.  The difference between it and REWRITE
is that the file mentioned must already exist and writing begins
at the end of the existing data.  The arguments are exactly the
same as with REWRITE.

APPEND is simulated for pmap'ed disk files by setting the file to
the end of file pointer.  For other I/O modes the openf has
the append bit set and the monitor is assumed to do something
appropriate.

@section(Wildcards )
@label(wild)

If you wish to handle file names with wildcards in them,
you should specify :* after the file name in either the
program statement or the RESET, etc., call.  This simply
allows wildcards to be typed accepted by the file name
parser.  To actually implement the wildcards, you
must have a loop in your program.  Each time through the
loop the file should be opened with the file name field
defaulted in the RESET, etc.  This causes the existing
jfn to be used.  To advance the jfn to the next file,
the predeclared function NEXTFILE(file) should be used.
It will return 0 if no more files remain in the group
described by the file spec.  Otherwise it returns the
bits returned by the GNJFN jsys.  Thus a typical
program might look as follows:

@begin(display)

   PROGRAM WILD(INFILE:*-);
   VAR
     INFILE:TEXT;
   BEGIN
   REPEAT
     RESET(INFILE);
      ...
    UNTIL NEXTFILE(INFILE) = 0
   END.

@end(display)

Note that when Nextfile returns 0 it releases the jfn.
This is done automatically by the monitor, and cannot
be prevented by Pascal.


@section(Including external text)

Users who write complex multi-file programs sometimes want
to put TYPE, CONST, and EXTERN declarations in a common file
used by all programs.  Such a file is used by putting the
INCLUDE statement at the beginning of any block in the program.
The syntax is INCLUDE 'file spec'; The quotes are required.
This statement may occur anyplace the token TYPE would be
valid.  The text in the file is included in the source of
the program, replacing the INCLUDE statement.  The syntax
analyzer is then reset so that a TYPE statement would still
be legal following the INCLUDE.  More than one file may be
included by INCLUDE 'file spec','file spec', ...; or
by more than one INCLUDE statement.

An included file consists of possible CONST, TYPE,
and EXTERN declarations, with exactly the same
syntax they would have at the beginning of a program
or block.  The file should terminate in a period.
But if the thing just before the period is an integer,
you will need a space or new line before the period
so the scanner doesn't think it is part of the number.

@section(Miscellaneous I/O Functions)
@label(miscIO)

The following are provided for completeness.  They will not be useful
for most people.
Those that are not explained below usually just do a monitor call
with the same name.  See the Monitor Calls manual for such cases.
They must be declared external, as shown
below, but they are built into the PASCAL library.  Note that
the symbol FILE is legal in the declaration of a procedure, as shown
below.  It will match a file of any type.  This sort of declaration,
which cancels some of the normal type checking, should be used with
great care.

Those functions and procedures that do not require EXTERN declarations
are listed below under Standard Functions and Procedures.

@begin(description)

PROCEDURE QUIT; EXTERN;

@ 	Causes a normal program exit immediately.  Closes all files.

FUNCTION TO6(A:ALFA):INTEGER;

@ 	Converts 6 characters in A to sixbit.

PROCEDURE FROM6(I:INTEGER;VAR A:ALFA); EXTERN;

@ 	Converts from sixbit to ascii

PROCEDURE CLREOF(VAR F:FILE); EXTERN;

@ 	Clears PASCAL's EOF indicator.  You must do this
if you want to proceed after an end
of file or some error for which you are enabled.  
Sets EOF to false for input, true
for output.   Clears
the error indication used by ERSTAT, so the next ERSTAT
will return 0.
Also does STSTS to clear the EOF and error flags
in the monitor, assuming that there is a physical file open.
Not necessary for clearing end of file in random access files,
as moving the file pointer will clear EOF.

FUNCTION ERSTAT(VAR F:FILE):INTEGER; EXTERN;

@ 	If the user specified that he wanted I/O to continue in spite of
errors, this function must be used to check for whether an error
occurred.  It will return 0 if no error has happened, otherwise
the error bits from a GETER jsys.
Only the most recent error code generated is returned.

PROCEDURE ANALYS(VAR F:FILE); EXTERN;

@ 	If an error occurred with file, prints an
official-looking error message.  No effect if no error
occurred, or if the file is connected to a string with
STRSET or STRWRITE. 

FUNCTION LSTREC(VAR F:FILE):INTEGER; EXTERN
@label(lstrec)

@ 	Returns the size of the last record read or written.  This is useful
mostly in the case of reading records in "record mode" (mode 7), as there
is no other way to find out how big the record was that you just read.
It is implemented in all modes for record I/O, however.  It might also
be useful in case an error occurs when reading a record and user error
recovery is enabled.  It will return the numbers of bytes actually read,
according to the monitor.  Experience shows that in this case the number
is often garbage, however.

FUNCTION CURJFN(VAR F:FILE):INTEGER; EXTERN

@ 	Return the indexable file handle for the file (0 if none).
This is a full-word quantity with the JFN in the right half, and
various bits in the left half.

PROCEDURE SETJFN(VAR F:FILE,I:INTEGER); EXTERN

@ 	Set an indexable file handle for the file.  First does RCLOSE if open.
Note the you may put bits in the left half if desired, though one 
would think that normally a simple JFN would be used.
If the jfn came from another file by CURJFN, that file
should be CLOSE'd first, in order to release any I/O
buffers.  (A fatal error may occur if this is ignored.)

PROCEDURE GETPAGES(N:INTEGER;VAR P:INTEGER;VAR PP:PAGEPTR); EXTERN

@begin(display)

 Gets a block of memory:
 N is the number of pages to get, 1 - 36.
 P is set to the page number, 0 - 777B.
 PP is set to a pointer to the page
  [pageptr = ^mempage;
   mempage = array[0:777B] of integer]
@end(display)
@ 	Always use this if you need to allocate memory.
 There are problems in trying to do it yourself.
 The memory comes from the I/O buffer area.

PROCEDURE RELPAGES(N:INTEGER;P:INTEGER); EXTERN;

@ 	Returns a block of memory to the runtimes:
N is the number of pages in the block, 1 - 36.
P is a page number, 0 - 777B.
The page should have been gotten from GETPAGES.
@end(description)

The procedures NEWPAGE and RETPAGE are still in the runtime library at the
moment, but should not be used by new programs.  They are exactly like
GETPAGES and RELPAGES, except that the do not have the argument N.  They
get and return a single page.

See section @ref(aritherr) for procedures relating to arithmetic errors.

See sections @ref(int) and @ref(sync) for procedures relating to interrupts
and synchronization.  (They are also in EXTERN.PAS.)


Beware that programs using these things are of course not transportable to
machines other than the DEC20!!

@section(The structure of a PASCAL program)

Some users will need to know the exact structure of a PASCAL program in
memory.  PASCAL produces two-segment programs, which are potentially
sharable.  Thus at the start of the program, the high segment contains
all of the code and certain constants, and the low segment contains
global data.  There are three other data areas which are created during
execution of the program:  the stack, the heap, and I/O buffers.

I/O buffers are created mainly for disk files.  Since the main use
is as pages for the PMAP jsys, the buffer area is always allocated
in page units.  The buffer area begins immediately above the
end of the low segment data area.  .jbff always points to the
beginning of the next page that will be allocated.  A bit map
is maintained to indicate free pages.  If you ^C and
restart the program, all pages from the end of the low segment
data to .jbff are deallocated, to guarantee that all PMAPd
disk files can be closed by the initial RESET.

The heap contains
all space allocated by the NEW function.  It is located immediately
below address 400000B, and expands downwards.  The stack contains
parameters, return address for routines calls, and all local variables
for procedures.  The stack is allocated in pages located ABOVE the
high segment.
The entry code compiled into every PASCAL main program does some
things that you may find useful to know about.  Normally Pascal
programs have an entry vector with three entries:
@begin(description)
0@\main entry

1@\reenter address - jumps directly to the routine QUIT, which closes
all files

2@\version number
@end(description)
You can add additional entries by using a second argument to $V, e.g.
if you put the comment {$V:1000000B:2 } at the start of your program,
you will cause a version number of 1000000 octal, and 2 extra
entries in the entry vector.  These entries will have offset 3 and
4.  These extra entries will start the program, but will set a
global variable, %CCLSW to the values 1 and 2, respectively.  There
is a function, CCLSW, that returns the value of %CCLSW.  To use it,
declare
@begin(display)
   FUNCTION CCLSW:INTEGER; EXTERN;
@end(displaY)
The code is set up so that if your program is started with a Tops-10 run
offset of 1, %CCLSW is also set to 1.  To facilitate passing parameters
from a superior, when your program is started, the contents of registers
1, 2 and 3 are saved at %CCLSW+1, +2, and +3.  There is a procedure
CCLACS, that will return these values:  
@begin(display)
   PROCEDURE CCLACS(VAR AC1,AC2,AC3:INTEGER); EXTERN;
@end(display)

It should be possible to control-C a PASCAL program and restart
it.  However, the user should be aware that global variables will
not be reinitialized in this case.  (In particular INITPROCEDURE's
will NOT be done again, as they are not really executable code at
all.)


@chapter<PASCAL Debug System (PASDDT)>

A PASCAL program may be run with the PASCAL Debug System by
using the monitor command DEBUG instead of EXECUTE.  (Successful
debugging also requires that the program be assembled with
/DEBUG, but this is on by default.)  The system
can be used to set breakpoints at specified line numbers.  When
a breakpoint is encountered, program execution is suspended and
variables (using normal PASCAL notation) may be examined and new
values assigned to them.  Also additional breakpoints may be set
or breakpoints may be cleared.  It is helpful to have a listing
of the program available as the system is line number oriented.

The previous paragraph assumes that your EXEC has been modified
to handle DEBUG properly for PASCAL.  If it has not, you must
use EXEC SYS:PASDDT,<your program>.  To modify the EXEC,
you should either make it add sys:pasddt for the debug command,
or put in /DEBUG:PASCAL and modify LINK to load (but not start) 
PASDDT in this case.

Should you need to run LINK explicitly, rather than using the monitor
DEBUG command, PASDDT is included by loading the file SYS:PASDDT.

@section(Commands)

The commands described here can be given when the system enters a
breakpoint.  When the program is executed an initial breakpoint will be
entered before the main program begins.  This will be shown by
a message
@begin(display)

     > STOP AT MAIN BEGIN
     >>

@end(display)
Additional breakpoints are set by
@begin(display)

     STOP <line>

@end(display)
where <line> is of the form line#/page# or just line# which is
equivalent to line# on the current page.
An example is 120/3, line 120 on page 3.  A maximum of 20 breakpoints
may be set.

PASDDT keeps track of the "current line".  The current line is the
one most recently printed out.  (In the case of printouts showing
a range, it is the first one in the printout.)  This line can
be referred to by a simple star (*)  Hence "STOP *" will be
equivalent to "STOP 3/5" if line 3 on page 5 is the current line.
If you type a line number with no page number, the current page
is supplied.  So if the current line is 3/5, then "STOP 100"
refers to 100/5.  To find out what the current line is, type
@begin(display)

     * =

@end(display)

The breakpoint is cleared by
@begin(display)

     STOP NOT <line>

@end(display)
STOP NOT ALL will clear all of them.  The breakpoints set may be listed by
@begin(display)

     STOP LIST

@end(display)
Variables may be examined by the command
@begin(display)

     <variable> =

@end(display)
<variable> may be any variable as given by the PASCAL definition
(except files).  In particular it may be just a component of a
structured type, or the whole structure itself.  In the case of
arrays, adjacent elements that are identical are displayed in a
compressed form.  Integer variables can be examined in hexadecimal
or octal by the two commands
@begin(display)

     <variable> =h

     <variable> =o

@end(display)
A new value may be assigned to a variable by
@begin(display)

     <variable> := <variable or constant>

@end(display)
The assignment follows the usual type rules of PASCAL.

PASDDT has access to your source file (assuming that it is still
there when you get around to running the program).  Whenever you
reach a breakpoint, the portion of your source file around the
breakpoint will be displayed.  Often it is useful to look at
other parts of your program, in order to decide where to place
breakpoints, or for other reasons.  To look at your program,
there are two commands:  
@begin(display)

     TYPE  <line> [<line>]
     FIND  [<count>] [<string>]

@end(display)
TYPE allows you to type a line or range of lines in the
currently open file.  (Use OPEN to change which file you are
talking about, as described below.)  FIND allows you to search
for any text string in the current open program.  E.g.
@begin(display)

	>> find 'foo'

@end(display)
will look for the next appearance of foo in your file.  To
find the second appearance of foo, use
@begin(display)

	>> find 2 'foo'

@end(display)
Note that the FIND search starts at the line after the current
line (.).

PASDDT can be used to follow execution of your program line by
line.  This is called "single stepping".  Once you start
this mode of execution, each time you hit the carriage return
key, one line of your program will be executed.  The commands
relevant to single-stepping are:
@begin(display)

     STEP

@end(display)
STEP causes the next line of your program to be executed.  Since
you often want to do this for many lines, it is rather 
inconvenient to type the word "STEP" for each line.  Thus once
you have done one step command, PASDDT enters a special mode
where a simple <CR> will cause the next line to be executed.
@begin(display)
 
     <cr> - do one line in single-step mode

@end(display)
This mode is announced by changing the prompt from the usual
">>" to "S>".  Note that all the normal PASDDT commands are
available as usual. The main difference that S> mode makes is that
<CR> is available as an abbreviation for STEP.  

You get out of
single step mode by doing a normal END, i.e. by proceeding
your program in the normal way.  

One other command is available
in single step mode:
@begin(display)

     <esc> - continue until end of procedure

@end(display)
When you are single-stepping and come to a procedure call, the
single-stepper will show you everything that goes on within the procedure.
Sometimes you really don't want to see the inner workings of
the procedure.  You just want to continue watching the program from
the point where the procedure returns.  
An <esc> (sometimes labelled <alt>) in single-step mode
will cause the stepper to 
finish the current procedure silently.  The next time you
hear from the debugger will be when the procedure exits (unless
of course you have placed a breakpoint within the procedure).

We advise all users to experiment with the STEP command, since
single-stepping is the single most effective debugging tool
we know.

The current active call sequence of procedures and functions is
obtained by
@begin(display)

     TRACE

@end(display)
The names of the procedures and functions together with their
line numbers are printed in reverse order of their activation.
TRACE may optionally be followed by a number, which will be the
number of levels for which information is printed.

You can display the values of all variables current active in
the program by using the command
@begin(display)

     STACKDUMP

@end(display)
This will give the same information as TRACE, and additionally
at each level display the names and values of all local variables.
As with TRACE, you may follow STACKDUMP by a number, and only
that many levels will be displayed.  You may also follow it with
a filename in quotes.  The information will be put in that file,
instead of dumped to your terminal.

Program execution is continued by the command
@begin(display)

     END

@end(display)
The program will run until another breakpoint is encountered.
The breakpoint is announced by
@begin(display)

     > STOP AT <line>
     >>

@end(display)

Should you have more than one module (presumably because you
have loaded both a main program and a file of external procedures),
special care is required.  At any given moment only
one module is accessible to the user.  That means that attempts
to refer to variables or line numbers in another module will
meet with errors.  To change modules use the command
@begin(display)

     OPEN <module>

@end(display)
The module name is the name in the program statement for the
corresponding file.  If no program statement occurs, it is the
name of the .REL file.  Whenever PASDDT is entered, it will
tell you the name of the module that is open initially.  In
the case of a break, the module in which the broken line occurs
is opened.

Sometimes there will be variables of the same name at several
levels.  In this case you may find it impossible to refer to
a variable at a higher lexical level than the one where the
break occurs.  The command
@begin(display)

     OPEN <depth> <module>

@end(display)
will set the context in which names are interpreted to any
depth desired.  The depth you type is the name as shown on
TRACE or STACKDUMP.

If you want to stop debugging, the command
@begin(display)

     QUIT

@end(display)
is sometimes useful.  It is somewhat cleaner than control C-ing
out of the debugger, as it closes all files and does other normal
cleanup.  Note that if you QUIT, partially written files are
closed, and thus made to exist.  Control C will not make such
files visible.

You can control the verbosity of PASDDT with the command
@begin(display)

     SHOW <integer>

@end(display)
This controls the number of source lines shown when you enter a
break.  0 is legal if you don't want to see any.

@section(Asynchronous Interrupt)

If a program goes into an infinite loop it may be stopped temporarily by
typing ^D (possibly twice).  This
will enter the PASCAL Debug System.  This interrupt is announced
with the message
@begin(display)

     > STOP BY DDT COMMAND
     > STOP IN <line1>:<line2>
     >>

@end(display)
If you happened to stop the program when it is in the runtimes,
you will get an invalid result, probably line 0.  However the
other functions of PASDDT should still work in this case.
In particular TRACE will tell you what procedure you are in.
The END command will resume your program.  

The only case I can think of where this does not work is if you
are doing your own interrupt handling and get stuck in a priority
1 interrupt handler, or somehow disable the ^D interrupt (e.g.
by supplying your own interrupt handler for channel 35).
In this case you can halt the program by typing ^C^C, and enter
PASDDT with the DDT command.  However in order for everything to
work properly PASDDT has to be told where you are in the program.
So after typing ^C^C, type ^T and look at the PC it gives you.
Deposit the PC in location 130, and then type the DDT command.
Everything should now work right.  However the END command may
not properly resume your program if it was in the middle of doing
a JSYS when you interrupted it.

@section(Standard Procedures and Functions)

The following standard procedures and functions  (described in the
Revised PASCAL Report) are implemented.  
@begin(display)


	Standard Functions	Standard Procedures

	ABS			GET (See @ref(stanfile) and @ref(varrec))
	SQR			PUT (See @ref(varrec))
	ODD			RESET (See @ref(simopen) and @ref(open))
	SUCC			REWRITE (See @ref(simopen) and @ref(open))
	PRED			NEW
	ORD			READ
	CHR			READLN (See @ref(charproc))
	TRUNC			WRITE (See @ref(write))
	ROUND			WRITELN (See @ref(write))
	EOLN			PAGE
	EOF                     PACK
	SIN			UNPACK
	COS
	EXP
	LN
	SQRT
	ARCTAN


@end(display)
Additional mathematical functions are available:
@begin(display)

	ARCSIN			SIND
	ARCCOS			COSD
	SINH			LOG
	COSH
	TANH


@end(display)

The following functions may be used to simulate the missing
** operator.  They must be declared EXTERN.

@begin(description)
FUNCTION POWER(X,Y:REAL):REAL; EXTERN; - X ** Y

FUNCTION IPOWER(I,J:INTEGER):INTEGER;EXTERN -  I ** J

FUNCTION MPOWER(X:REAL;I:INTEGER):REAL;EXTERN - X ** I
@end(description)

Additional standard functions:

@begin(description)
CURPOS(file) returns the current position in a file.  See section @ref(randac).
Only valid for files on random access device.  (type integer)

DATE  result is a PACKED ARRAY [1..9] OF CHAR.
The date is returned in the form 'DD-Mmm-YY'.

NEXTFILE(file).  Advances to the next spec for a wildcard file.
See section @ref(wild).

RANDOM(ignored).  Argument is an integer, which is ignored. Result is a
real number in the interval 0.0 .. 1.0

RECSIZE(file) returns the record size of the file.  One may also
specify a particular variant whose length is to be returned.  See
section @ref(varrec) for details.  (type integer)

RUNTIME  elapsed CPU time in msec (type integer)

TIME  current time in msec (type integer)

@end(description)

Additional standard procedures:

@begin(description)

APPEND(file,name,...).  Like REWRITE, but extends an existing file. See
section @ref(append).

BREAK(file). Forces out the output buffer of a file.
Should be used before magtape positioning.
It is not needed for terminals to force out messages.  See section
@ref(IOMode).

BREAKIN(file,noget). Clears the input buffer count.  Must be used
after magtape positioning for buffered input.
If noget is omitted or zero (FALSE), a GET is done on the file after the
buffer count is zeroed.	May also trigger delayed errors.  See section
@ref(IOMode).

CLOSE(file,bits). Close file and release its channel. See section @ref(close).

DELETE(file). Delete file.  See @ref(delete).

DISMISS(file). Abort creation of a file.  See section @ref(delete).

DISPOSE(pointer,variant,...). Return a record to the heap.  See section
@ref(dispose).  (Some editions of Jensen and Wirth include this as a
standard procedure.)

GETINDEX(file,index). If file is open on a string (STRSET or STRWRITE),
sets index to current index into the string.  (See section @ref(strio).)

GETLINENR(file,lineno).  Lineno must be a packed array of char.  It
is set to the last line number seen in the file.  If no line numbers
have been seen '-----' is returned.  '     ' is returned for a page
mark.  If file is omitted, INPUT is assumed.

JSYS(jsysnumber, ...). Arbitrary monitor call.  See section @ref(jsys).

MARK(index). Save state of the heap.  See @ref(markrel).

PUTX(file). Rewrite record in update mode.  See @ref(randac).

RCLOSE(file).  Close, releasing jfn.  See @ref(close).

RELEASE(index).  Restore saved state of the heap.  See @ref(close).

RENAME(file,name,...). Rename an open file.  See @ref(rename).

SETPOS(file,position).  Move in random access file.  See @ref(randac).

STRSET(file,array,...).  Open input file on array.  See @ref(strio).

STRWRITE(file,array,...).  Open output file on array.  See @ref(strio).

UPDATE(file,name,...). Open random access file for revising in place.
See section @ref(update).
@end(description)

Although it is not exactly a procedure or function, some explanation
should be given of the MOD operator.   X MOD Y is the remainder after
dividing X by Y, using integer division.  The sign of the result is
the same as the sign of X (unless the result is zero, of course).
Note that this is a different definition than the one used by mathematicians.
For them X MOD Y is always between 0 and Y-1.  Here it may be between
-(Y-1) and +(Y-1), depending upon the sign of X.  This implementation is
used for consistency with the Cyber implementation, which is the semi-official
standard.  Note that SAIL (and some other widely used languages) also
use this perverted definition of MOD.

@section(External Procedures and Functions)
@label(extproc)

A procedure or function heading may be followed by the word
EXTERN.  This indicates to the compiler that the routine will
be supplied at load time.  In addition it may be specified that
the routine is a PASCAL, FORTRAN, ALGOL or COBOL routine.  PASCAL
is assumed if no language is specified.  The language symbol determines
how the parameters are passed to the external routine.  The
relocatable file also contains information to direct the loader
to search the corresponding library on SYS:.

Example:  PROCEDURE TAKE(VAR X,Y: INTEGER); EXTERN FORTRAN;

[NB: ALGOL and COBOL almost certainly do not work.  I have left in
these options to give a head start to anyone who is interested in
implementing interfaces to Algol or Cobol.]

The PASCAL compiler can deal with two kinds of files:  main programs
and files of external procedures.  A main program contains a single
program with procedures local to it.  There must be exactly one
main program involved in any load (i.e. in one EXEC command).  Any
procedures not present in the main program must be declared EXTERN,
as explained above.  They must then be defined in a file of external
procedures.

A file of external procedures has the following differences from
a main program: (1) There is no top level code.  The period follows
the last top level subroutine.  For example:

@begin(display)

	var i,j:integer;
	procedure a;
 	  begin i:=1 end;
	procedure b;
	  var y:integer;
	  begin y:=1 end.

@end(display)

In a main program, there would be top level code after procedure B.
(2)  The top level procedures,
A and B in the above example (but not any procedures defined within
A or B), have their names declared in a special way.  This
makes them accessible to other programs.  Note that only the first
six characters of the name are significant to other programs that
access these procedures as EXTERN.
(3)  A file of external procedures must either have a comment
of the form (*$M-*) at the beginning, or be compiled /NOMAIN.
(These both do the same thing.)

You may combine several .REL files containing external procedures
to form a library.  If you want to search this in library search
mode, it will be necessary to specify entry points for each module.
Each source file will compile into a single module.  The module
name will be the program name specified in the PROGRAM statement, if
there is one, otherwise the file name.  If you do nothing special,
that module name will also be used as the only entry for the module.
If there is no top level procedure with the same name, that name will
be assigned as an alternate name for the first procedure in the file.
To get more than one entry point, you must use a special form of
the PROGRAM statement, e.g.
@begin(display)

	PROGRAM TEST,A,B;

@end(display)
for the above example.  This declares TEST as the module name, and
A and B as the entry points.  Usually you should list all of the
top level procedures as entry points, although this is not required.
Note that these entry points are only needed for library search
mode.  Even without this special PROGRAM statement the procedures
A and B could be accessed as EXTERN procedures by a separate main
program.

Note that the normal form of program statement, e.g. PROGRAM TEST (A, B);,
is illegal for a file of external procedures.  All files that are to
be initialized at the beginning of the program must be declared in the
program statement in the main program.  The form that declares only
the module name, e.g. PROGRAM TEST;, is legal, however.

It is possible for one file of external procedures to call procedures
defined in another file of external procedures.  As usual, they must
be declared as EXTERN in each file where they are to be called.

Assume the files TEST.REL, AUX1.REL, and AUX2.REL are to be loaded,
along with a routine from the library NEW:ALGLIB.REL.  Execution
is accomplished by:
@begin(display)

     EXEC TEST,AUX1,AUX2,NEW:ALGLIB/LIB

@end(display)
The main program must be listed first.  Otherwise you will get an error
message from the loader complaining that .XSTRT is not defined.  Note
that this command would cause any of the programs that had not been
compiled to be compiled.

@subsection(Calling Fortran procedures from Pascal)

Here are the steps needed to call Fortran procedures from Pascal:
@begin(itemize)
The main program must be written in Pascal.  

The Fortran code must consist of Fortran subroutines or functions.
If Pascal is to call a Fortran subroutine, the Pascal module that
calls it must include a procedure declaration using EXTERN FORTRAN,
as explained in the previous section.  If Pascal is to call a
Fortran function, the Pascal module must include a function
declaration using EXTERN FORTRAN.

The main program must include a declaration for either FORINI or
FOREXI, and it must call one of these routines.  We strongly
recommend declaring both of them, and calling FORINI at the
very beginning of the program and FOREXI at the very end.  However
in the current version, these are dummy routines, so it does not
matter where they are called.  One of them must appear somewhere
in the code so as to force the Fortran interface to be loaded.
Here is how to declare them:  PROCEDURE FORINI; EXTERN;  [Note
that these procedures need not be declared EXTERN FORTRAN.]
You may omit FORINI and FOREXI if your Fortran procedures do no
I/O and use no features that require memory allocation.

You can have Fortran treat a packed array of char as a Fortran
character string.  To do so, declare the argument as type
STRING or STRINGPTR.  This causes a Fortran string descriptor
to be passed.

Most sites have the Pascal runtime system in the form of a sharable .EXE
file.  It is similar to the sharable FOROTS file in its function. In the
current version it is called SYS:PASSEG.EXE. The sharable runtime file
is automatically merged with your core image when you load a Pascal
program.  Unfortunately this file does not contain the code necessary to
link to a Fortran subroutine.  In order to use the Fortran interface,
your program must be loaded with the conventional Pascal library,
not the sharable runtimes.  If your site normally uses the sharable
segment, you should be able to force the normal library by adding
SYS:PASUNS/LIBRARY to the list of files in your LOAD or EXECUTE command.
If you do not know whether your site uses the sharable runtimes, run
a Pascal program and then do INFO MEM.  If you are using the sharable
runtimes, the output will show the file SYS:PASSEG.EXE mapped into the
pages around 700.  Note that you must start the program before this
file will be mapped in.
@end(itemize)

This reset of this section will discuss certain implementation details
that may become important if you are trying to handle complex systems.
In simple cases, all that you need to know is the rules above.

When you use the Fortran interface, by including a call to FORINI or
FOREXI, runtime systems for both Fortran and Pascal are active.
Since runtime systems normally allocate resources such as memory
and PSI channels, it is clear that some coordination is needed.
The Pascal/Fortran interface coordinates resources as follows:
@begin(itemize)
Unless you do something odd, Pascal will allocate all of memory
above the code to the stack.  It will make this allocation before
calling the Fortran initialization routine.

Pascal will allow Fortran to do all other memory management. 
When the Pascal/Fortran interface is active, NEW and DISPOSE
call the Fortran memory manager (FUNCT.), and the Pascal
runtime system gets space for I/O buffers from the Fortran
memory manager.  (This means that $H has no effect when
using the Pascal/Fortran interface.)

Pascal will allow Fortran to control the interrupt system.
This means that {$A- or {$C- will not turn off arithmetic
exception handling as it normally would.  When the Pascal/Fortran
interface is in effect, the appropriate Fortran routines should
be used to control exception handling.
@end(itemize)
[These statements are not strictly true if extended addressing
is in effect.  In extended programs, Pascal allows Fortran to manage
memory allocation for the section in which the code resides.
Because of the use of multiple sections, Pascal is able to manage
all other resources, including memory used for the stack and heap,
and the interrupt system. $H, $A and $C have their normal meanings in
this case.]

As mentioned above, normally Pascal allocates all of the memory above
the code for use by the Pascal stack.  This allocation prevents any of that
memory from being used by Fortran, or by other systems that Fortran
may call (e.g. database systems).  In some cases you may not want to
allocate this much memory to the stack, or you may call a system
that is designed to use some particular piece of memory in this
area.  Thus it is possible to prevent Pascal from allocating all
of the memory above the code.  It will not do this allocation
if ANY of the following conditions occurs:
@begin(itemize)
If you specify {$S:address}

If there is no high segment.  (This might result from
specifying /SEGMENT:LOW to LINK.)

If this is an extended address program {$X+}
@end(itemize)
The first two situations indicate that you are attempting to do
detailed control over memory allocation.  Thus Pascal abandons
its own efforts at allocating memory for the stack, so as not
to interfere with whatever you are doing.  It will put the
stack wherever you specify with {$S:address}.  However you are
responsible for making sure that that area is free for the stack.
This means that you will have to understand the memory allocation
strategy used by Fortran or any other systems that you may be
calling.  This strategy may vary between different versions of
Fortran. 

In the third situation (extended addressing), this entire issue
is not relevant, since Pascal puts the stack in a separate
section or sections.

Please note: the Fortran interface described here will only
work with version 6 or later of Fortran.  Earlier versions
should use the module FORTIO.MAC.  Documentation for this
interface is given at the beginning of the file FORTIO.MAC.
The basic instructions given at the beginning of this section
still apply, as FORTIO also uses FORINI and FOREXI.  However
the more detailed descriptions of allocation strategies do
not apply to the interface defined in FORTIO.

@chapter(Extended addressing)
@label(extended)

This chapter discusses Pascal's support of extended addressing.
In a "normal" DEC-20 program, there is a limit of 256K words.
This limit applies to the total of code and data.  Except if
extraordinary precautions are taken, this means that you can't
have an array or combination of arrays with more than about 120,000
elements in it (or smaller arrays that total 120,000 elements).
Extended addressing increases the limit to 8M words.  In principle this
should allow you to have an array or combination of arrays with 7,000,000
elements.  Note however that your system is likely to run out of
swap space or some other resource before this limit is reached.  So
you should talk to your system administrator before using extended
memory.  Extended memory is implemented as an addressing mode directly
in the hardware.  It is not like RSTS "virtual arrays", where
large arrays are kept on disk.  Thus there should not be much if
any penalty for using extended addressing, except that as programs
get larger the system will tend to bog down due to paging and other
kinds of overhead.

Extended addressing mode is NOT the default.  It must be selected
with the /EXTEND switch or the {$X+} directive.  See the section below
on new directives for details.  For most users, all that you will have
to do is insert {$X+} before your PROGRAM statement. The rest should be
automatic. This chapter discusses certain implementation details and
extra options, for the benefit of those who will need to know them.

@section(Limitations on Extended Addressing)

You can't mix code compiled with {$X+} in effect with "normal"
code.  LINK will complain about multiple definitions of the
variable %PASXT if you try to do this.

Packed arrays are
currently limited to 128K words when extended addressing is in effect,
even though such objects may be put into extended sections.

Most data in extended programs is put into extended sections.  For
this data, the only aggregate limit is the 8M word limit mentioned
above.  However the following data is all put into one section.  So
all of them together must fit within 256K:
@begin(itemize)
All of the code

All of the symbols (Symbols can be eliminated if you specify /NODEBUG
or {$D-}.)

Variables, arrays, and records declared at the top level (i.e. not
inside a procedure) whose size is less than 1024 words.
@end(itemize)

INITPROCEDURES cannot be used to initialize data in objects outside
the section with the code in it.  In practice this means that
they can't be used to initialize objects larger than 1024 words.

Due to a bug in TOPS-20 release 4, it is possible (fairly likely)
to get a FLKTIM BUGCHK after running any program in extended address mode.
There is a simple patch for this, but the patch was broken by the
second AUTOPATCH tape.  The BUGCHK simply causes the user to be hung
for up to a minute when the next program is run, it causes no other
known problems.  The patch is given in the installation instructions
(see PASCAL.OPR).

The complex memory manager, used when DISPOSE is called from anywhere
in the program, cant yet handle discontiguous sections.  This could cause
problems if the user allocates and deallocates whole sections.  One
simple way around this is to NOT call RELSECTIONS, but simply re-map
the section yourself.  Code to fix this could be added by anyone familiar
with the code.  The implementor just ran out of time and didn't feel
that was a very serious restriction.

@section(Compiler Directives and Switches for Extended Addressing)

Each of these options can be selected by a directive in the source,
or by a switch to the compiler.  If you use the COMPILE or EXECUTE
commands in the EXEC to call the compiler, you can specify
compiler switches by using /LANGUAGE-SWITCHES.  E.g. to execute
PROG.PAS specifying the /EXTEND switch to Pascal, you could say
@begin(display)
EXECUTE PROG.PAS/LAN:"/EXTEND"
@end(display)
In addition, some sites may choose to implement these switches
directly in the EXEC. If a directive is found in the source it overrides
the corresponding switch given to the compiler.

@subsection(/EXTEND or {$X+})
The extend switch or X+ directive tells the compiler to generate
code which will run in extended addressing mode.  The X directive
will NOT be recognized AFTER the PROGRAM statement.

@subsection(/STACK:n or {$S:n})
The stack switch or directive is used entirely differently in
the two modes.  In both cases it is an OCTAL number if given in
the /STACK:n form, and can be octal or decimal if using the {$S:n} form.

In single section mode it tells where the stack is
to start (an absolute address).  The default is the next page
boundary above the high segment.

In extended addressing mode,
it tells HOW MANY SECTIONS are to be used for the stack.
The stack MUST be the top N sections.  The default is 10 sections.
There are 31 sections available on the KL-10.  One section is
used for the code, some unknown number for the XDATA PSECT (see below),
one section for buffers, and the other 29 for the stack, heap, and
user-allocated sections. If the stack uses the default 10 sections, the
heap can use the other 19, less any allocated by the user. If the
assembly parameter SE%BUF in PASUNV is set to zero, buffers will
be allocated in the same section as the code.  This will limit the number
of files that can be open simultaneously, but will probably make it run
slightly faster, and will leave one more section for use by the stack or heap.

@section(Source changes for extended addressing)

For most programs, no change in the source is required at all.
There are some things to look out for which may cause problems in
existing programs.  The following list is all the things I have
found that may cause such problems.

@begin(itemize)
To hold an extended address, the size of a pointer
variable is 36 bits, so it is NOT possible to have anything else in
the same word as a pointer.  In particular, any PACKED RECORD which
includes a pointer should be looked at closely.

JSYS calls using the "a:b" construct to build halfwords
may cause trouble.  However, the most common use of "a:b" is probably
in making byte pointers (ie: "-1:b").  This is not necessary if "b" is
a packed array of characters, since simply specifying "b" will actually
put a byte pointer into the accumulator anyway.

When it is necessary to have a local address (as required by some
old JSYSes, such as TBLUK) NEWL should be called, rather than NEW.
This will allocate memory in the same section as the code.  (In a
single section program, NEW and NEWL are equivalent).

Since FORTRAN does not YET generate code which will run correctly
in a nonzero section, any program which calls FORTRAN subroutines MAY
not run correctly.  DIGITAL expects to support extended addressing in
version 7.
@end(itemize)

@section(The .XDATA PSECT: Where the data is put)

Single-section programs have two sections of contiguous data,
technically known as PSECT's.  These are referred to as the "low segment"
(the PSECT .LOW.) and the "high segment" (the PSECT .HIGH.).  The
low segment contains all data the is defined at the global level
(i.e. not within a procedure), and the high segment contains all of
the code.  In an extended core image, these two PSECT's still exist.
For various reasons, they are limited to fit into a single section of
256K.  Since it is not desirable to limit your global data to what
will fit in a single section, any global data structure that is
larger than 2 pages is put into a separate PSECT: the "extended data
area" (the PSECT .XDATA).  The reason for making this distinction
is that access to data in the same section (.LOW.) is slightly
faster than access to data in other sections.  Also, such "local"
accesses create less code, and use less space in the .REL file.
Thus we try to use .LOW. for simple variables and relatively small
objects.

.XDATA is a multi-section PSECT:  Objects that are put in
it can take up as much space as necessary.  Note that the compiler
makes no attempt to see that data in this PSECT will really fit
into your core image.  It is possible that it would take up so
much space that there would be no room at runtime for the stack,
heap, and I/O buffers.  If so, you will find this out when you
first start your program.  As with all other PSECT's, data from
multiple modules do not overlap:  The data from one module is
put into the PSECT starting at the end of the data from the
previous module.  [Note:  Currently LINK does not support
multi-section PSECT's.  For this reason, the effect described above
is simulated using Polish fixups and the .ASSIGN operator.  The user
should not notice the difference.]

@section(New procedures for use with extended addressing)

@subsection(NEWL)

A new procedure NEWL has been added to allocate memory in the
same section as the code.  This should be needed ONLY by programmers
who wish to make their own JSYS calls, since some old JSYSes require
local addresses.  If NEWL is called from a program running in section 0
(ie: a single section program), it is equivalent to a call to NEW, so
it will not cause problems in running the same program in both single-section
and extended-address modes.  Pointers created with NEWL should not be
DISPOSE'd, since NEWL does not allocate out of the section that NEW and
DISPOSE maintain.

@subsection(GETSECTIONS and RELSECTIONS)

Two new procedures "GETSECTIONS" and "RELSECTIONS" have been added,
also for programmers who want to do their own JSYS calls.  In release 5
of TOPS-20 it will be possible to map whole sections of files into
memory, or to map whole sections of processes to other processes.
The calls to GETSECTIONS and RELSECTIONS look precisely like calls to
GETPAGES and RELPAGES with the following three exceptions:
@begin(itemize)
The pointer returned by GETSECTIONS is a pointer to a whole
section rather than a single page.

The number returned by GETSECTIONS and used by RLESECTIONS is
a section number, rather than a page number.

The section number argument to GETSECTIONS is used by GETSECTIONS
as a "suggested" section number.  If the section by that number is
available it is allocated, otherwise the lowest numbered available
section is allocated.
@end(itemize)
Note that like GETPAGES and RELPAGES, GETSECTIONS and RELSECTIONS
must be declared.  See EXTERN.PAS for their definitions.

@include(restri.mss)

@chapter(Miscellaneous)

@section(Use of DDT )

It is possible to use regular DDT to debug a PASCAL program.
To do so, use the monitor DEBUG command with the switch /DDT
after the first file name.  If you run LINK explicitly,
type /DEBUG as the first command, as usual.

It is also possible to have both PASDDT and DDT in core at
the same time.  To do so, you should load the file SYS:PASDEB
with your program, e.g. "EXEC SYS:PASDEB.REL,PROG.PAS".
PASDEB has the appropriate garbage in it to load the right
files in the right order.  When loading is finished, DDT
will be started.  You may examine things and set breaks using
DDT.  If you decide you will want any breaks using PASDDT,
you should then use the command "PASDEB$G" in DDT.  This
will set things up so when you start your program you will
get the usual "Stop at main BEGIN".  To start your program
type "$G".
By the way, be sure not to use the DEBUG command when loading
PASDEB, as you will get two copies of DDT!

In DDT, you will find that there are a few symbols defined
for you.  The beginning of your main program is defined as a
global symbol.  Each procedure has up to three symbols defined
for it.  Assume that your procedure is called NAME.  Then we have

@begin(description)

NAME	the first part of the procedure proper.  This is
an appropriate place to put a DDT break point.

NAME.	the first instruction of a sequence of code used
to adjust the static display pointer.  It is
located before NAME  Most procedure calls are
to NAME.+<some integer>, rather than to NAME

NAME%	the first location of a block of byte pointers
associated with this procedure.  This is located
before NAME.

.MAIN.	the first instruction in the main program

@end(description)


@section(Arithmetic errors)
@label(aritherr)

The PSI system is always turned on.  Arithmetic errors are always trapped.
If you are using a relatively recent monitor, arithmetic errors are trapped
using the SWTRP jsys.  This is a special-purpose, relatively efficient,
way to trap arithmetic errors.  If your monitor does not support this
method, then PSI channels 6 and 7 are used.  Whenever an arithmetic error
occurs, the result of the operation is set to an appropriate value:
@begin(display)
floating underflow: 0.0
floating overflow, and all division by 0:
    If the result is positive, the largest possible number.
    If the result is negative, the smallest negative number.
fixed point overflow: low-order bits of true result.
@end(display)
These results are designed to be consistent with Tops-20 Fortran.
In addition, unless you have turned off checking, Pascal prints an
error message and terminates your program.

If you do not want to treat arithmetic errors as fatal, you can
control them in either of two ways:
@begin(itemize)
Call SETARITH to define how you want them handled.

Use /NOARITH or its equivalent (/NOCHECK, {$A-}, {$C-}) to turn off
all error messages due to arithmetic errors.
@end(itemize)
SETARITH must be declared external:
@begin(display)
   PROCEDURE SETARITH(WHICH,HOWMANY:INTEGER); EXTERN;
@end(display)
HOWMANY specifies how many error messages you want Pascal to issue.
It will issue that many, and then stop issuing error messages (as
if /NOARITH were in effect).  A value of 0 is equivalent to
/NOARITH.  A value of -1 is special, and indicates that the error
is to be regarded as fatal.  This is the default behavior.  You
can specify different values for each kind of arithmetic error.
WHICH is a code indicating what kind of error you are using.
Here are the values:
@begin(description,spread 0)
-1	Set all at the same time

0	Integer overflow

1	Integer divide by 0

4	Floating overflow

5	Floating divide by 0

6	Floating underflow
@end(description)

There is also a function, ARITHERR, that tells you how many errors
have occured since the last time you called ARITHERR:
@begin(display)
   FUNCTION ARITHERR:INTEGER; EXTERN;
@end(display)
Obviously this is only useful if you have arranged for arithmetic
errors to be nonfatal.

The above description does not apply if you are using the Fortran
I/O interface.  In this case, you should use ERRSET and OVERFL.
These procedures are analogous to SETARITH and OVERFL.  (Indeed ERRSET takes
the same arguments as SETARITH, but must be declared EXTERN FORTRAN.)

For those of you who may be writing in assembly language:  If there is
an instruction that may create an error, you can prevent the error
message by following it by a JFCL.  Pascal will still set the result
as described above, but it will not issue error messages (or count the
error in the count returned by ARITHERR).  Pascal will clear all error
bits in the PC except the ones that you test with your JFCL.  (This is
to make sure that your JFCL triggers.)  JFCL 0 is useful if you want
to ignore the error, but not test it.

@section(Interrupt handling)
@label(int)

Users may declare a Pascal procedure as an interrupt handler.
To do so, call PSIDEFINE(chan,level,proc).  Chan is the
interrupt channel, -1 to 35.  Level is the interrupt priority level,
1 to 3.  Proc is the procedure to call when the interrupt
occurs.  PSIDEFINE must be declared external:
Procedure PSIDEFINE (chan, level: integer; procedure proc); EXTERN;

Interrupt channel -1 is used for arithmetic traps.  It is available
only on Tops-20 release 4 and later.  It uses the new SWRTP% JSYS
instead of the normal interrupt system.  It is supposed to be
slightly more efficient than a normal interrupt.  Except for the
implementation method, setting up an interrupt on channel -1
should be equivalent to setting an interrupt on both channels
6 and 7.  The procedures PSIDEFINE, PSIENABLE, and PSIDISABLE
are all applicable for channel "-1", just as for real PSI channels.
For channel -1, the level argument to PSIDEFINE is ignored.
Please be sure to use channel -1 rather than 6 and 7 if you monitor
supports it.  Pascal sets up a default error handler using
SWTRP% if it can.  Since SWTRP% takes precedence over PSI handling,
this default error handler would take precedence over your
interrupt handling on channels 6 and 7.

The  procedure called can do anything any Pascal procedure
could normally do, except that it should not refer to any
variables outside itself except top-level globals (variables
not in a procedure but defined at the beginning of the
program or file of external procedures).  This restriction
is not enforced, but violating it could result in disaster,
and certainly will result in junk.  The procedure may have
from 0 to 5 arguments.  If they exist, they are as follows:

The first argument
is set to the channel on which the interrupt occurred (in case
the same procedure is associated with more than one channel).

The second one is set to the value of PC when the interrupt
occurred.  The right half is the address to which control
will return, and the left half is status bits.

The third argument is a Boolean variable that is normally false.
It is true if this interrupt really occurred in a critical
section and was deferred.  If it is set, the PC is not the PC
where the interrupt really occurred, but rather is in the
routine that reinitiates the deferred interrupts when you leave
the critical section.  (See below for an explanation of
critical sections.)

The fourth argument is the old PC passed by reference.  Only
hackers should touch this.  It allows you to change the
location that the system will return to after the interrupt
routine.

The fifth arguments is the array where the AC's are stored,
passed by reference.  This would let you change the contents
of the AC's, e.g. to change the result after an arithmetic
exception.  This is also for hackers only.

Most users will use at most two arguments.
Other information may be obtained by appropriate jsys's.

If the user sets up an interrupt for channel -1, 6 or 7 (arithmetic
errors), the interrupt will be "censored" so that errors
occurring in DDT, or followed by a JFCL will not be seen.  In
the case of an error followed by a JFCL, the result will be set to 0.
The user interrupt
will preempt the normal Pascal arithmetic error handler in other cases.

There are also procedures for enabling and disabling interrupt
channels: psienable(chan) and psidisable(chan).  Psienable
must be done before  interrupts
can be received on the corresponding channel.  They do the aic
and dic jsys's respectively.  Note that the normal
Pascal initialization sequence enables the interrupt system as
a whole and sets up the vector, so only the individual channels
need to be enabled.  Psienable and Psidisable must  be declared
as EXTERN.

The Pascal runtime system is designed to be as reentrant as
possible.  This is why you can do almost anything at interrupt
level without worrying about what was going on when the
interrupt occurred.  The known exceptions are
@begin(itemize)
You should generally not do I/O on the same file at interrupt
level and in the main program, unless you know that you
will never interrupt an I/O operation on that common file.
The one exception is the predeclared file TTY, which we
take great pains to handle in a reentrant manner.  (That is,
you can do I/O with TTY in both the main program and at
interrupt level.)

You should be careful about referring to common (global)
data.

If you are using DISPOSE, you should be careful about NEW
and DISPOSE.  The memory manager used by NEW and DISPOSE
is not reentrant.  So if you happen to interrupt a DISPOSE
and do another DISPOSE at interrupt level, the heap is
likely to become corrupted.  Any of the following things
will eliminate this danger:
@begin(itemize)
If you are not using DISPOSE.  In this case you get
a simpler memory manager for NEW, and it is reentrant.

If you know that an interrupt will never occur during
NEW or DISPOSE.

If you do not use NEW or DISPOSE at interrupt level.
@end(itemize)
If none of these things is true, then you are going to
have to put the NEW and DISPOSE that occur in the main
program inside critical sections.
@end(itemize)

@section<SYNCHRONIZATION (critical sections)>
@label(sync)

Occasionally it is necessary to manipulate the same global
data in both the main program and an interrupt routine.  When
this is done, it is often necessary to be sure that the
interrupt routine does not break into the middle of the
main program's manipulation.  To attack this problem, we
have added two synchronization primitives.  These allow you
to declare any section of your program to be a critical
section.  Within a critical section any interrupts that
occur are deferred until you leave the critical section.
To begin a critical section, call ENTERCRIT.  To end a
critical section, call LEAVECRIT.  These are procedures
with no parameters, and should be declared EXTERN.

ENTERCRIT
is vaguely equivalent to turning off the interrupt system,
except that it is much faster than doing that jsys.
When you are in a critical section, all interrupts are
immediately dismissed, but the runtimes keep track of which
channels have had interrupts occur on them.  When you
do LEAVECRIT, the deferred interrupts are simulated by the IIC
jsys, higher priority level channels being done first.
If several interrupts occur on the same channel during the
critical section, only one interrupt will be generated at
the end.  This is consistent with DEC's design decision that
programs can never assume that there are the same number
of interrupts as causes of interrupts.  Your code must check
all possible causes of the interrupt, and continue to
process them as long as any show up.
Note that you probably cannot defer
interrupts on the "panic channels".  If one occurs during a
critical section, you may get the usual error message from the
EXEC.  Note that memory allocation and deallocation in the
runtimes are done within critical sections.  This is what
allows you to open and close files in interrupt routines.


@section(Warning about error messages)

If you handle your own errors, you should not assume that the
error code you get was necessarily generated by the monitor.
Occasionally the runtimes detect some problem and generate what
seems to be an appropriate code.  At least the messages seem
appropriate for the circumstances.

@section(Interfacing to external procedures in MACRO)

This section discusses the structure of MACRO routines designed to
be called from as PASCAL program.  Such routines will require a
declaration within the PASCAL program, with EXTERN used in place
of the body.  EXTERN causes the compiler to expect a routine
that uses the PASCAL calling conventions, so those will be discussed
here.  Should you prefer to use the Fortran-10 calling conventions,
the routine should be declared EXTERN FORTRAN.

The calling conventions are similar for both procedures and functions.
The only difference is that functions return values, and procedures
don't.  In both cases, the arguments are put in accumulators 2
through 6.  There is a way to pass more parameters than will fit in
these accumulators, but it is fairly complex to explain.  Should
you need to do this, you are probably best to look at the code
produced by the compiler (using /OBJECT).  What is put in the accumulators
is determined as follows:

@begin(description)

by value, one word - the value in one accumulator

by value, two words - the value in two successive accumulators

by value, more than two words - address of object in one accumulator

by reference (VAR) - address of object in one accumulator
@end(description)

Your routine may use the accumulators freely, except for 15, 16, and 17.

@begin(description)

15 - this is a holdover from the tops-10 version.  You should
probably make sure that it is not changed.

16 - pointer to the base of the local variable area.  This is in the stack
below the current value of 17.  All local variables of the calling routine
may be accessed as positive offsets off 16.  To find the offsets you will
have to look at the object code, however.  This value should be unchanged
on exit from your routine.

17 - pointer to the top of the stack.  You may use it in pushj and push.
Be sure that every push is matched by a pop, and pushj by popj.
Note that the stack is at the top of allocated core, so it can
expand almost indefinitely.
@end(description)

If your routine is to be called as a function, it should move the
result to 1(p).  [That's right, folks, one above the top of stack.]

You may call any PASCAL runtime routine with a simple pushj 17,.
You may call any normal PASCAL-compiled routine with a pushj 17,
but you should push a dummy argument on the stack first, as pascal
routines garbage -1(17).

@section(Special linkage conventions for hackers)

The following three identifiers function syntactically as if they were
predeclared types.  However they are only legal when used to describe
parameters of EXTERN procedures.  Thus they are a convenience for
those brave souls who are trying to add more runtimes but do not want
to have to modify the compiler.  

@begin(description)
FILE - a parameter declared as FILE will match a file of any type.
This is necessary for procedures such as CLOSE, RENAME, etc., which
one obviously wants to work for files of all types.

STRING or STRINGPTR - a parameter declared as STRING or STRINGPTR (they are
the same) will match a packed array of CHAR of any length.  This is used
for the file name argument in RESET, REWRITE, etc.  For Pascal procedure,
this actually puts data into two registers.  The first gets the address
of the array.  The second gets its length in characters.  For Fortran
procedures, a Fortran string descriptor is used passed.

POINTER - a parameter declared as POINTER will match a pointer of
any kind.  It is used for procedures such as NEW, which must work
for pointers to any kind of structure.  It also puts data into two
registers.  The first gets the value of the pointer (or its address
if VAR is used).  The second gets the size (in words) of the structure
that the pointer points to.  This type of parameter only works with
Pascal procedures.  You can't pass it to Fortran, Cobol, or Algol.
No error message will be generated if you try, but the results are
garbage.
@end(description)

Use of these things is strongly discouraged except by Pascal maintainers,
who are assumed to understand what is going on.


@chapter(Pascal-20 for Release 3A and 4)
@label(3a4)

This section describes two compilers intended for use on release 3A and 4.
The only way these compilers differ from those documented above is in
their command scanners.  

@section(How to use the normal release 3A/4 compiler)

The usual version of the compiler, PASCAL, follows the standard 
DECsystem-10 conventions for compilers, and can thus be invoked by 
COMPIL-class commands.  (If your EXEC does not know about PASCAL,
see the next section, where a special version of the compiler,
called PAS, is described.)

To compile and execute a PASCAL program TEST, you would
issue the command
@begin(display)

	EXECUTE TEST.PAS

@end(display)
The usual COMPIL switches, such as /NOBIN, /LIST, and /CREF can
be used.  For other commands, such as COMPIL, DEBUG, and LOAD,
see your EXEC documentation.

If you choose not to use COMPIL-class commands, you should say
@begin(display)

	@@PASCAL
	*<relfile>,<listfile>=<sourcefile>/<sw>/<sw> ...

@end(display)
Anything other than the source file may be left out.  The
defaults are:
@begin(description)

relfile: not produced if missing; if no extension: .REL

listfile: not produced if missing; if no extension: .LST

sourcefile: if no extension: .PAS
@end(description)
Since the EXEC expects to be dealing with Tops-10 compilers,
we have to supply a Tops-10 scanner.  The usual limitations
apply:
@begin(itemize)
File names must be 6 characters for file name, 3 for file type

No version numbers may be used

If the directory must be specified, you must use a PPN, not
a directory name.

No recognition is done on file names or switches
@end(itemize)

Under release 3A, if you want to supply compiler switches you
must call the compiler and use the syntax just given.  In release
4 the EXEC allows you to pass switches as part of the COMPIL-class
commands by using the syntax /LANG:"/switch/switch..".  For 
example if you want to specify /NODEBUG, you would type
@begin(display)
@@EXEC PROG.PAS/LANG:"/NODEBUG"
@end(display)
Whether you run the compiler explicitly or pass switches through the
release 4 EXEC, the following compiler switches are allowed:

@begin(description)

/ARITHCHECK - Turns on checking for arithmetic errors, i.e.
divide by zero, overflow, and underflow.  If this switch
is not specified, the setting of /CHECK is used as its default.

/CHECK - generates code to perform runtime checks for
indices and assignments to scalar and subrange
variables.  Pointer accesses will also be checked for NIL or
zero pointers.  (Usually a zero pointer is the result of a
pointer variable not being initialized.)  Divide by zero,
overflow, and underflow are also caught.  All of these cases
cause an error message to be printed and transfer to PASDDT1 or
DDT if they are loaded.  Note that .JBOPC is set up for DDT.
However the program cannot necessarily be continued, because the
AC's may be those of the error trapper.  Also, in the case of
an arithmetic error, you are at interrupt level in DDT.

/CREF  - generates information so that CREF can produce
a crossreference listing.  Changes default
extension for the listing to .CRF.

/DEBUG - generate information for the DEBUG package.  This is
normally on except for production programs.
We strongly encourage people to turn this off (probably by putting
the directive (*$D-*) in their program) when they know they have
finished using PASDDT.  The debug information may double the size
of your program.

/EXTEND - generate code for extended addressing use.  See Chapter
@ref(extended).

/HEAP:nnn - sets the first address to be used for the heap.
This is the storage used by NEW.  It will begin at
the specified address and go down.  The only known
use for this is if you intend to load the high
segment at other than 400000.

/MAIN - a main program part is present (see Section @ref(extproc))

/OBJECTLIST - list the generated code in symbolic form

/STACK:nnn - sets the first location to be used for the
stack.  This should be above the high segment
(if any).  The only known use is if you intend
to do a GET to a larger segment and want to
be sure the stack doesn't get in the way.

/VERSION:vvv - must be given on the output side.  This
version number will be
put in .JBVER unless overridden by a later
directive.  vvv is the usual DEC-10 version number, e.g.
3B(22)-4.

/ZERO - Causes code to be compiled in every procedure and
function prolog to initialize all of its local
variables to 0.  Also, NEW will initialize all
records it generates to 0.  This is useful mainly
for programs with complicated pointer manipulations,
since it guarantees that uninitialized pointers will be
0, which will be caught by the /CHECK code.  Note that
/ZERO and /CHECK can be set independently, however, and
that /ZERO applies to all local variables, not just
pointers.  /ZERO will not cause global variables to be
reinitialized, although they always start out as zero
unless an initprocedure is used to give them another
value.
@end(description)

To get the opposite effect of a listed switch, type /NO<switch>.
The default switch settings are /CHECK /DEBUG /MAIN /NOOBJECTLIST
/NOZERO.
For /STACK  and /HEAP the arguments to the PASCAL compiler can be 
nnP, nnK, nnnnnn, or #nnnnnn. This specifies a core address in pages, 
K, decimal, or octal.  The default values are 0, which causes 400000B 
to be used for the heap and the stack to be put immediately above the 
high segment.  Values will be rounded up to the nearest page boundary.  
For the PAS compiler, described below, the arguments to /STACK
and /HEAP are assumed to be octal numbers, with the same interpretation.

@section(PAS: A Special Compiler for Unmodified EXEC's on Release 3A/4)

The instructions above assume that the EXEC has been modified to
know about the PASCAL compiler.  If it has not, then under release 3A
and 4 you cannot use the EXECUTE, COMPILE, LOAD, or DEBUG commands to
compile a PASCAL program.  To simplify things for users at sites with an
unmodified EXEC, we supply a second compiler, usually called PAS.  This
compiler has the most important features of the EXEC built into it.  If
your EXEC knows about PASCAL, you will have no need for PAS, and you
should ignore this section.

PAS differs from a normal compiler in that it rescans the
command line.  Thus you can use PAS as if it were a command,
listing file names and switches on the line where you call it.
Because of design limitations in the EXEC, recognition is not
available for the command line.  So you must type file names
out, except that the file type .PAS will be assumed if you
don't type one.  Of course switches may be abbreviated.

The default when you use PAS is that it will execute the
program.  So to execute a PASCAL program TEST.PAS, you would use the 
command
@begin(display)

	PAS TEST

@end(display)
Only one source file can be mentioned in the command.
TEST.PAS would be compiled only if there has been a change since
the last time it was compiled.  Then LINK would be called to load
and start the program.  

PAS has switches to select a few options other than simple 
execution.
@begin(display)

	PAS TEST/DEBUG - debug with PASDDT
	PAS TEST/LOAD - compile and load; don't start
	PAS TEST/NOLOAD - compile only, don't load

@end(display)
Note that the switch /DEBUG has a different interpretation in this 
context that was documented above when compiler switches were 
described.  It causes the program to be compiled (if necessary), 
loaded with PASDDT, and PASDDT started. The following switches are 
also available in PAS:
@begin(display)

	/COMPIL - compile even if source is not changed
	/LIST - produce a listing file
	/CREF - produce a CREF listing file
	/OBJECT - produce listing file with object code
	/NOBIN - do not produce a .REL file

@end(display)
All other compiler switches described in the previous section 
(including /NODEBUG) are also available and have the effect 
documented.

If no arguments are typed on the line with PAS
(i.e. if you just type the command "PAS"), you will
get a prompt PASCAL>.  PAS will then expect you to type
the name of a source file to compile.  Only a compilation will be
done, i.e. no automatic execution.  Of the switches mentioned
in this section, only /LIST, /CREF, /OBJECT, and /NOBIN will be 
meaningful.  Of course the normal compiler switches documented in the 
previous section will still be available.


@appendix(References)

(1)  N. Wirth.  The Programming Language PASCAL (Revised Report)
Bericht Nr. 5, Berichte der Fachgruppe Computer-Wissenschaften,
ETH Zurich, November 1972

(2)  K. Jensen, N. Wirth.  PASCAL - User Manual and Report.
Springer Verlag, Berlin, Heidelberg, New York, 1974.
