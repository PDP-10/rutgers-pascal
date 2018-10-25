@device(pagedfile)
@make(manual)
@style(indentation 0,paperlength 60,topmargin .5inch,linewidth 7inches)
@modify(hdx,above 3,below 2,need 7)
@modify(hd0,above 3,below 2,need 7)
@modify(hd1,above 4,below 3,centered,pagebreak before)
@modify(hd2,above 3,below 2,need 7)
@modify(hd3,above 3,below 2,need 7)
@modify(hd4,above 3,below 2,need 7)
@modify(description,leftmargin 15,indent -10)
@disable(contents)
@generate(contents "cont")
@enable(contents)



@begin(center)
PASCAL-10 and PASCAL-20

Introductory User's Guide









@end(center)

The information in this document is subject to change without
notice and should not be construed as a commitment by Charles Hedrick
or Rutgers University.  Charles Hedrick and Rutgers University assume
no responsibility for any errors that may appear in this document.

Note:  The following are trademarks of the Digital Equipment
Corporation:  DECSYSTEM-20, DECsystem-10, Tops-20, Tops-10

@include(pascal-mem.cont)
@NewPage(0)@Set(Page=1)@Style(PageNumber <@1>)

This document is an introduction to DECSYSTEM-20 and DECsystem-10
Pascal.  This document is designed to be used with the book "Standard
Pascal: User Reference Manual", by Doug Cooper.  You should think of
this document as containing a few extra chapters that should be inserted
in that book to make it more complete.  Earlier drafts of this document
were designed to be used with Jensen and Wirth.  That is no longer
practical, since the ISO standard added some features to the language
and clarified other things.

This Pascal system is the result of cooperation among a number of
different people. It was originally written at the University of Hamburg
by a group of people under the supervision of Prof. H.-H. Nagel. This
version was developed from Prof. Nagel's by Charles Hedrick, and is
maintained by him. Lee Cooprider and others at the University of
Southern California have been particularly helpful in supplying
improvements, largely to the debugger. The heap manager was originally
written by Shel Kaphan at Stanford, and adapted for Pascal by Dave Dyer
at ISI. A number of compiler bug fixes were supplied by Andrew Hisgen at
Carnegie-Mellon University.   Extended addressing for the DECSYSTEM-20
(not described in this introduction) was originally implemented by
Norman Samuelson at Sandia National Labs, although Charles Hedrick made
major improvements to this implementation.

This system is intended to be a complete implementation of the Pascal
language, as defined in the Revised Report (Jensen and Wirth).  It is
also intended to be in conformance with the ISO Pascal Standard, ISO/DP
7185.   I regard the ISO standard as normative, and am prepared to fix
any cases where programs that conform to the ISO standards do not work
properly.  I have read one draft of the standard carefully, and also a
textbook based on the final draft.  However I have not yet tried any
validation suites, or  made a systematic effort to check conformance If
anyone does use a validation suite on this compiler, I would very much
appreciate being notified of the results (and being given a chance to
fix any problems before you publish the results elsewhere). There is a
separate chapter in the Reference Manual describing conformance with the
ISO standard.

This implementation includes a number of extra facilities, giving you
access to the full power of the operating system. Only the most
important of these additions will be described in this manual.  There is
a complete reference manual, which you should consult if you need
information not given here.

@chapter(How to compile and execute your program.)

@section(How to use the normal compiler)

@index(compiler)
This section describes how to use Pascal on the DECsystem-10, and
on those DECSYSTEM-20 systems where the EXEC has been modified
to know about Pascal.  If you are using a DECSYSTEM-20 whose
EXEC has not been modified, there is a special version of PASCAL
that you can use instead of the EXECUTE command.

@index(source program)
Suppose you have a Pascal program called TEST that you want to execute.
You must first put the program into the computer as a text file.
You will do this using a text editor, several of which exist.  Generally
you should choose a file name ending with .PAS, since the extension PAS
tells the system that it is a PASCAL program.  Let us assume that your
source program is stored in a file TEST.PAS

@index(EXECUTE command)
@index(DEBUG command)
@index(COMPILE command)
@index(LOAD command)
@index(executing a program)
To execute a program, use the EXECUTE command. You give this
command the name
of the file in which the source program is stored.  For example, to compile
and execute a Pascal program stored in the file TEST.PAS, you would
issue the command
@begin(display)

	EXECUTE TEST.PAS

@end(display)
You can use the usual COMPIL switches, such as /NOBIN, /LIST, and /CREF.
See the Command Reference Manual for a description of these switches, as 
well as of the COMPIL, DEBUG, and LOAD commands.  If you are using more
than one module, you must list the main program first in the EXECUTE
command.  If you don't, you will get an error message from the loader
@index(.XSTRT undefined error)
complaining that .XSTRT is undefined.

Here is what happens when you issue an EXECUTE command.
The first step is for the Pascal compiler to compile
your program.  The compiler reads your source file
and produces a relocatable binary file as output.
@index(binary file)
@index(relocatable binary file)
@index(.REL file)
For example, if your source file is called TEST.PAS, the compiler would
produce a binary file called TEST.REL.  The compiler always uses the
name of your source file, with the extension .REL in place of the
original extension .PAS.  You do not need to worry about what is in this
binary file.  It contains a form of your program that the system can
execute more easily.

If any errors occur during compilation, the compiler writes error
messages to three different places:
@begin(itemize)
your terminal

@index(.ERR file)
@index(error messages)
a file, in this case TEST.ERR.  The file name is constructed by
taking your source file name and changing the extension to .ERR.
If such a file already exists, it is overwritten.

your listing file, if you ask for one
@end(itemize)
We put the error messages in a special .ERR file as a convenience
to you.  Originally we just wrote the messages to your terminal.
However the messages usually scrolled off the top of the screen.
Thus we starting writing them to a file, so you could go back
and look at the file to find your errors.  If there are no errors,
PASCAL deletes any old .ERR file, just to prevent confusion.

The second step in the EXECUTE command is loading your program.
The linking loader (LINK) reads the binary form of your program
(the .REL file), and loads it into memory.

@index(PROGRAM statment, effect in execution)
@index(files, how to specify at runtime)
@index(data files, how to specify at runtime)
The final step in the EXECUTE command is the actual execution of
your program.  Before it begins the main body of your program,
the Pascal runtime system will ask you what files you want to use as
input and output. It will ask you about each  each Pascal file
variable listed in your PROGRAM statement.  For example, if your PROGRAM
statement looks like this 
@begin(display)

	PROGRAM TEST(INPUT,OUTPUT)

@end(display)
here is the resulting dialog.  In the following, the computer's
output is in upper case, and your input is in lower case.
@begin(display)

	INPUT   : data.in
	OUTPUT  : data.out


@end(display)
This example assumes that you want input to be from a file called
DATA.IN, and output to be to a file called DATA.OUT.  Of course
you can type any file name.

@index(terminal, how to use as I/O device)
@index(TTY, how to use as I/O device)
In the most common case, you want to
use the terminal for input and output.  You could specify this
by typing TTY: as a file name.  Because it is so common to use
the terminal for the files INPUT and OUTPUT, TTY: is the
default for these files. That is, if you simply type a carriage return
in place of a file name for INPUT and OUTPUT, the terminal
will be used.
@begin(display)

	INPUT   : 
	OUTPUT  : 

@end(display)
@index(end of file on terminal)
@index(terminal, end of file on)
@index(^Z)
@index(EOF on terminal)
When input is assigned to a terminal, you should type ^Z (control-Z) to
set the end of file (EOF) for that file.

Note that INPUT and OUTPUT are just examples of Pascal file variables.
If your PROGRAM statement lists a different set of
file variables, then Pascal will ask about them instead of INPUT and
OUTPUT.  If your program does not read any input, you should not put
any input file in the PROGRAM statement.
If you do not want Pascal to ask the user about file names at all, you
should just leave out the PROGRAM statement completely.  However in
this case you will need to build the file names into the program,
as described below.

Here is an example of the way things will look on your terminal
when you execute a Pascal program on a DECSYSTEM-20.  The at sign
shown is the system's prompt.  You do not type it.  You type only what
is in lower case.  Things after the ! are explanatory comments.
@begin(display)

    @@execute test.pas
    PASCAL: TEST	! Pascal compiler produces TEST.REL
    LINK:   LOADING     ! LINK loads TEST.REL
    [LNKXCT TEST EXECUTION]     ! Your program is now started
    INPUT   : in.file   ! You tell it what files to use
    OUTPUT  : out.file
    @@			! Your program is finished.

@end(display)
On a DECsystem-10, the prompt will be a dot instead of an at
sign, and you will see "EXIT" when your program is finished.

@section(Compiler switches)

@index(switches, compiler)
@index(directives, compiler)
@index(options, compiler)
This section discusses some of the more useful options in the
compiler.  You can specify these options when you compile
the program, or you can build the specifications into the
program itself.  For example, to turn off checking for
arithmetic exceptions you could specify /NOARITHCHECK when you
compile the program, or put the specification {$A-} into the
program.

To include an option in the program, you simply include a
comment containing the specification in your program.  The
comment must begin with $ and then contain a list of specification,
separated by commas.  + turns on an option and - turns it off.
E.g. consider the following
@begin(display)
	{$A-,Z+}
	PROGRAM TEST(INPUT,OUTPUT)
	....
@end(display)
This will turn off the A option and turn on the Z option.  In
most cases this should be done before the program statement,
although many of the specifiers can be put anywhere in the
program.

Here is an example of how you specify options 
when compiling the program:
@begin(display)
   Tops-20:
	one option, /NOARITH
	     EXEC TEST.PAS/LANG:"/NOARITH"
	more than one option, /NOARITH and /ZERO
	     EXEC TEST.PAS/LANG:"/NOARITH/ZERO"
   Tops-10:
	one option, /NOARITH
	     EXEC TEST.PAS(NOARITH)
        more than one option, /NOARITH and /ZERO
	     EXEC TEST.PAS(NOARITH/ZERO)
@end(display)
@index(/COMPILE switch)
@index(forcing recompilation)
@index(compilation, how to force when it doesn't happen)
Note that these options will only take effect if the program
is compiled.  If you do not see a message like
@begin(display)
	PASCAL:	TEST
@end(display)
it means that your program did not need to be recompiled.
A binary file left from a previous compilation was used instead,
so you end up with whatever options were in effect during that
earlier compilation.  To force a new compilation, specify
/COMPILE, e.g.
@begin(display)
    Tops-20:
	EXEC/COMPILE  TEST.PAS/LANG:"/NOARITH"
    Tops-10:
	EXEC/COMPILE  TEST.PAS(NOARITH)
@end(display)

Here is a list of the most useful of the options.  For a complete
list, see the full reference manual.  Some of these options are
noted as being the "default".  This means that they are in
effect unless you specifically turn them off.  Note that the
form with the slash can be abbreviated by using any unique abbreviation.

@begin(description)
@index(/ARITHCHECK switch)
@index(arithmetic exceptions)
@index(checking, arithmetic exceptions)
@index($A directive)
@index(overflow)
@index(underflow)
/ARITHCHECK {$A+} [DEFAULT]  This causes your program to check for
arithmetic exceptions. An arithmetic exception is
any case where an arithmetic operation gives the wrong results.
These include division by zero, and production of values too large
(overflow) or too small (underflow) for the computer to store.
When one of these problems occurs, your program will print an
error message.  If you are using a debugger, it will be activated
after the error message is printed.  If you are not, your
program will simply stop.   You can continue execution of your program
by typing CONTINUE, although you should not place too much faith in
any answer produced once an error has happened.

@index(/NOARITHCHECK switch)
/NOARITHCHECK {$A-}  Turn off checking for arithmetic exceptions, as
described above.

@index(checking, range and bounds)
@index(array, bounds checking)
@index(pointers, how to detect erroneous)
@index(NIL, how to detect attempts to follow)
@index(/CHECK switch)
@index($C directive)
@index(initialization, detecting missing pointer)
/CHECK {$C+} [DEFAULT] This causes your program to check for various
conditions that would make its execution be invalid.  It turns on
the same checks that are turned on by /ARITHCHECK.  In addition,
your program will check all array subscripts to see that they are
within the size declared for the array.  It will check assignments 
to subrange variables to see that the new value is within the limits
defined for the subrange.  And it will check use of pointer variables
to see to it that you do not attempt to follow a pointer that is
NIL or zero. (Usually a zero pointer is the result of using a 
pointer variable that has not been initialized.)  If the program
detects an error, it acts as described under /ARITHCHECK above.

@index(/NOCHECK switch)
/NOCHECK {$C-}   Turn off checking for errors, as described above.

@index(/DEBUG switch)
@index($D directive)
@index(debugger, how to remove debugger information)
/DEBUG {$D+} [DEFAULT]   This causes the binary form of your
program (.REL file) to contain information about your program
that is needed by the debugger.  You must have this option turned
on in order to use the debugger.  This information takes up space
in your program.  So large programs that are going to be used
many times will sometimes be compiled without this information,
to save space.

@index(/NODEBUG switch)
/NODEBUG {$D-}   Do not include debugging information with
your program.

@index(/ZERO switch)
@index($Z directive)
@index(zeroing, how to cause automatic initialization)
@index(initialization, how to cause automatic)
/ZERO {$Z+}  This causes your program to initialize all local
variables to 0.   Also, NEW will initialize all
records it generates to 0.  This is very helpful when you
are trying to debug a program that deals with pointers.
It guarantees that any pointer variables you forget to
initialize in your program will be zero.  This will allow any attempt
to use them to
be caught by the error-handling code compiled by the
/CHECK option.  This option affects only variables that are
local to procedures.  Global variables (variables declared
at the outer level of your program) are always initialized
to zero when your program is first loaded.  However if you
restart your program once it has been run once, global
variables will not be reinitialized.

@index(/NOZERO switch)
/NOZERO {$Z-} [DEFAULT]  Do not initialize local variables
to 0.
@end(description)

@chapter<PASCAL Debug System (PASDDT)>

@index[PASDDT (see "debugger" for subtopics)]
@index(debugger)
@index(DEBUG command)
A PASCAL program may be run with the PASCAL Debug System by
using the monitor command DEBUG instead of EXECUTE.  (Successful
debugging also requires that the program be assembled with
/DEBUG, but this is on by default.)  For example, the following
command will cause the program GAUSS to be run under the control
of PASDDT:
@begin(display)

	DEBUG GAUSS.PAS

@end(display)

PASDDT is an extra command
interpreter that allows you to control how your program is
executed.  It lets you stop your program at places that you
have specified in advance, or trace its execution line by
line.  When PASDDT is control, you can look at the current value of
the variables in your program (using normal PASCAL notation), and
you can even assign new values to them.

@section(How PASDDT works)

PASDDT commands can only be given when PASDDT is in control.  When
you start out debugging a program, PASDDT is initially in control.
PASDDT continues accepting commands from you until you tell it to
start your program.  Once you have done that, your program is in
control until something happens to return control to PASDDT.
Here are the ways that PASDDT can regain control:
@begin(itemize)
You can set a "breakpoint".  This is a specific line in your program
where you would like to have the opportunity to look at variables
or do other PASDDT commands.  When the program reaches such a line
during its execution, it is suspended, and control returns to PASDDT.
You insert breakpoints by using the STOP command, described below.

You can request "single stepping".  In this case, one line of the
program is executed at a time, with PASDDT regaining control after
each line is executed.  You enter single step mode by using the STEP
command.

When an error occurs, control goes to PASDDT, to allow you to examine
the program context when the error happened.

@index(debugger, getting to while running)
@index(^D interrupt for debugger)
@index(control-D interrupt for debugger)
If you need to get into the debugger while your program is running,
the procedure differs somewhat depending upon whether you are
running under Tops-10 or Tops-20.  On Tops-20, you simply type
^D (control-D).  When you type ^D, you program will immediately be
suspended and
PASDDT will get control.  This will work no matter what the program
happens to be doing at the time.  On Tops-10, such interrupt
characters are not available.  So there you would ^C your program,
and type the command DDT to the monitor.  This will cause you
to enter PASDDT.
@end(itemize)

Once PASDDT has regained control, you can look at and change variables,
and do other PASDDT commands.  Then you can request PASDDT to continue
your program.  When you do, your program will continue from exactly
where it was when it was stopped.  

@index(line numbers, in debugger)
@index(debugger, line numbers in)
Many PASDDT commands use line numbers to refer to a line in your
program.  If your file contains SOS line numbers, these line numbers
are used.  Otherwise, 1 refers to the first line, 2 to the second, etc.

Many files are organized into pages.  In SOS and TVEDIT there are special
commands to produce page marks.  In other editors, pages are delimited
by form feed characters (^L, control-L).  In PASDDT you can specify
line 75 on page 3 by the syntax "75/3".  If you do not specify a page
number, it is assumed that you are referring to the current page (the
page on which the most recent typeout of program text began).  Line numbers
start over again with 1 on each page (except in SOS file, where the SOS
line numbers are used).

Here are some examples of legal line number specifications.
@begin(description)
1864	line 1864 on the current page

1200/5	line 1200 on page 5

*	the current line (where the most recent typeout of program
text started)
@end(description)

@index(current line, in debugger)
You can find out what the current line and page are by typing the command
"*="

@section(Commands for controlling your program)

The section will describe commands that start and stop your program,
thus determining when PASDDT will get control.

After you issue the DEBUG command, your program will go through the
normal startup dialog.  It will ask for file names for all files
listed in the PROGRAM statement, just as it usually would.  Once this is
finished, PASDDT will get control.  You will see something like the
following:
@begin(display)

	@@debug merge.pas
	LINK:	Loading
	[LNKXCT MERGE Execution]
	OUTPUT    : tty:
	> Stop at main BEGIN - module MERGE open at 270/1
	270     for i := 1 to 36 do
	271       for j := 1 to 104 do
	272         for k := 1 to 11 do
	>>

@end(display)
@index(>> - debugger prompt)
The >> prompt shows that PASDDT is waiting for a command.  PASDDT
always shows you the line of your program that will be executed next
if you continue the program.  In this case it is line 270 on page 1
of your program.  A message like this will be printed whenever
PASDDT gets control from your program.

The following commands can be used to control when Pascal will next
get control:

@center(STOP line-number)

@index(STOP debugger command)
@index(breakpoint in debugger)
@index(debugger, inserting breakpoints)
This puts a breakpoint at the specified line.  If you continue your
program's execution, and it ever reaches this line, the program
will be suspended and PASDDT put in control.  You will then see
a message similar to the one shown above.  The >> prompt will tell
you that PASDDT is again waiting for commands.

@center(STOP LIST)

Lists the line numbers of all the breakpoints.

@center(STOP NOT line-number)

Removes the breakpoint at the specified line.

@center(STOP NOT ALL)

Removes all breakpoints.

@center(END)

@index(END debugger command)
@index(proceeding in debugger)
@index(continuing in debugger)
@index(debugger, proceeding with your program)
This ends the control by PASDDT.  It causes your program to proceed
from the point where it was most recently stopped.  The program will
continue until something happens to give control back to PASDDT.
Most commonly it will continue until it reaches a line where a breakpoint
has been set.  If you have been single-stepping, END cancels single-step
mode.

@section(Single step mode)

@index(debugger, single stepping)
@index(single stepping, in debugger)
@index(stepping, in debugger)
@index(S> as debugger prompt)
Single step mode is a convenient way of executing your program while
maintaining a maximum of control under PASDDT.  In contrast to breakpoints,
which are used when you know what part of the program you are interested
in looking at, single step mode is used when you don't know exactly
what you are looking for.  It causes only one line of your program
to be executed.  PASDDT regains control at every line.  

Here is what you will see at each line when you are in single step mode:
@begin(display)

> Stop in MERGE:270/1
270     for i := 1 to 36 do
271       for j := 1 to 104 do
272         for k := 1 to 11 do
S>

@end(display)
This indicates that the next line to be executed is line 270 on page 1.
The prompt of S> instead of >> indicates that you are in single step mode.

Here are the commands that are used to start and stop single-step mode,
and to control your program when you are in it:

@center(STEP)

@index(STEP debugger command)
Start single-stepping.  This will cause your program to regain control,
but will return control to PASDDT at the next line.  This command is
valid whether you are in single-step mode or not.  It leaves you in
single-step mode.

@center(carriage return)

When you are in single-step mode, a simple carriage return is equivalent
to the command STEP.  This is simply a convenience to allow you to
move through your program without having to type the word S-T-E-P for
each line to be done.  In fact the only difference between single step
mode and normal PASDDT is the fact that in single-step mode, carriage
return and escape are available as convenient abbreviations.  All normal
PASDDT commands are still available in single-step mode.

@center(escape)

When you are in single-step mode, you sometimes find yourself steppping
through procedures in which you are not interested.  If you hit the
escape key, execution of your program will continue until the program
exits from the procedure it is currently executing.  When the program
reaches the next line after the call to that procedure, PASDDT will
regain control.  You will probably have to try single-stepping before
you see why this is a useful feature.

@center(END)

You get out of single-step mode by continuing your program in the
normal way, i.e. by the END command.

@section(Commands for looking at and changing variables)

@index(debugger, examining and changing variables in)
@index(variables, examining and changing in debugger)
The main reason for using PASDDT is that it lets you look at what
is going on inside your program.  The commands listed in this
section are those that allow you to look at variables, and even
change their values.

@center(variable =)

@index(= debugger command)
@index(octal output in debugger)
@index(hexadecimal output in debugger)
This command shows you the value of a variable from your program.
There is some problem because of Pascal's block structure.  There
can be 25 different variables in your program all called "I".
Each one is defined in a different procedure.  PASDDT uses the
same rule for figuring out which one you mean that Pascal itself
uses.  The search starts at the location that was shown when
PASDDT gained control (the next line to be executed).  Any variable
defined in the procedure of which that line is a part is used first.
Then procedures containing that one are searched, until the search
ends with global variables.  Procedures that do not contain the
next line to be executed are not accessible.

Note that variable is any legal Pascal variable.  You can use
subscripts, dots, and pointer arrows, and even complex combinations of 
these.  If you ask for an array or structure, all of its elements will
be printed.  (For an array, several identical elements are printed in a
abbreviated form.)

Integer variables can be printed in hexadecimal or octal by
using "=h" or "=o" respectively after the variable name.

@center(variable := value)

@index(:= command in debugger)
This allows you to change the value of a variable.  Whatever value you
set it to will be used by the program if you continue it.  Value should
be a Pascal constant of a type compatible with the variable involved.

@center(TRACE)

@index(TRACE command in debugger)
@index(backtrace in debugger)
@index(debugger, finding out where you are)
Trace gives you a "backtrace", a list of what procedures are currently
active, and where they were called from.  If you do not want to see the
whole backtrace, you can use the following form:

@center(TRACE n)

which prints only the first N procedure calls from the full trace.

@center(STACKDUMP)

@index(STACKDUMP debugger command)
Stackdump prints the value of all local variables.  Needless to say,
this output can get a bit voluminous.  Thus it is possible to direct
the output of this command to a file:

@center(STACKDUMP 'filename')

It is also possible to specify that you only want to see local variables
from the N innermost procedures currently active.  (You can see what 
these are from the output of TRACE.)

@center(STACKDUMP n)

@section(Looking around in the source file)

@index(debugger, looking around in source file)
@index(searching in source file in debugger)
@index(source file, looking at in debugger)
@index(file, source, looking at in debugger)
Sometimes you will want to place a breakpoint somewhere, but not remember
the exact line number.  Or for some other reason you may need to look
around in the source file.  For this purpose, PASDDT has two commands
useful for looking around.

@section(FIND 'string')

@index(FIND debugger command)
@index(string, searching for in debugger)
This command searches forward in the source file, beginning from
the current location (denoted by a dot).  It looks for the first
occurence of the string specified.  In doing the search, upper and
lower case letters are considered equivalent.  So if you say
FIND 'MYPROC', this will match an appearance of MyProc in the source
file.  The first line found will be displayed, and will become the
new current line.

@center(TYPE start-line end-line)

@index(TYPE debugger command)
This allows you to type out any part of the source file on your
terminal.  If you specify only one line number, just that one
line will be typed.  The ending line number can be something
ridiculously big if you want to see everything to the end of
the file.

@section(A warning about confusing PASDDT)

It is possible for PASDDT to become confused about where it
is in your program.  This will generally happen if you transfer
to PASDDT by typing ^D (control-D) in the middle of your
program. (Or on Tops-10, typing ^C and then the monitor
command DDT.)  As long as the part of your program that is written
is Pascal is being executed, things will be fine.  But if you
happen to be in one of the library routines (e.g. I/O routines,
NEW, or routines written in another language and loaded with
your Pascal program), PASDDT will be unable to figure out
where you are.  In this case, it will usually claim that you
are at either line 0/0, or at the last line in your program.

The only serious problem raised by this case is that you can't
tell exactly where you are in the program.  The usual functions
of PASDDT should still work.  Note that the TRACE command will
at least tell you what procedure you are in.  And END and STEP
can still be used to continue your program.  (In fact STEP is
a convenient way to find out where you are, as it will cause
a stop at the next line in the source program.)

The problem happens most often when you stop the program while
it is waiting for input from the terminal.

@chapter(Basic Input/Output)

@index(input/output)
This section is provided for two reasons.  First, existing textbooks 
and manuals are often somewhat ambiguous about Input/Output.
Second, this version of Pascal has some special features
that will make your life a bit easier.  Actually both this chapter
and the next one discuss Input/Output.  This chapter gives more
details on standard features and how to use them.  Although it
describes some features peculiar to this implementation, generally
they are features that fit smoothly into existing Pascal I/O.
The next chapter describes facilities that are more serious
extensions.

@section(Standard Files)

@index(standard files)
@index(INPUT, standard file)
@index(OUTPUT, standard file)
@index[files, predeclared (standard)]
@index(files, how standard files are opened)
The files INPUT and OUTPUT are called "standard files", because they
are built into the language.  If you use any other files, you
must declare the file variables in the VAR section of your program.
INPUT and OUTPUT should not be declared in the VAR section, since
they are already builtin ("predeclared").  In addition to being
predeclared, INPUT and OUTPUT have the following special properties:
@begin(itemize)
INPUT and OUTPUT are default files for READ and WRITE.  E.g.
READ(X,Y) really means READ(INPUT,X,Y) and WRITE(X,Y) really
means WRITE(OUTPUT,X,Y).  If you wanted to use any other file,
you would have to mention it by name in every READ and WRITE statement
referring to it.

INPUT and OUTPUT are automatically opened if they are mentioned
in the PROGRAM statement.  If you want to use any other file, you
have to open it by RESET (for reading) or REWRITE (for writing).
@end(itemize)

The net effect of this is that you can just say READ(X) in your
program.  You do not have to do any declarations or file opening,
as long as you mention INPUT in the PROGRAM statement.

@index(TTY, standard file)
@index(TTYOUTPUT, standard file)
@index(terminal, how to use as I/O device)
In addition to the standard files INPUT and OUTPUT the standard
file TTY is available in this implementation.  You may use this file
to communicate with the terminal.  Most users will not need to use
TTY, since INPUT and OUTPUT will also default to the terminal.
TTY is provided mostly for compatibility with older versions of
the compiler, where terminal I/O via INPUT and OUTPUT was
somewhat inconvenient.

As with INPUT and OUTPUT, you do not need to declare TTY in the 
VAR section of your program.  Because it always describes the terminal,
you also do not need to (indeed you should not) mention TTY in the PROGRAM
statement.  The purpose of the program statement
is to specify which files the system should ask you about when your
program starts.  Since TTY is always associated with your terminal,
it obviously serves no purpose to ask about it.

PASCAL automatically opens TTY with a builtin RESET and REWRITE before
starting your program.  So as with INPUT and OUTPUT you need not (indeed
should not) include RESET and REWRITE statements for TTY.

When you want to READ or WRITE from TTY, you must explicitly mention
it as the first argument to READ or WRITE.  Otherwise INPUT
or OUTPUT will be used by default.

To summarize, here is a table of special properties of the
various files:
@begin(display)
			INPUT&OUTPUT	TTY	others

Must be listed in
PROGRAM statement	     Y	         N          Y
if used.

Must be declared in	     N		 N	    Y
the VAR section.

Must be RESET or	     N		 N	    Y
REWRITE'n before use

Must be mentioned as         N	         Y          Y
first arg to READ or
WRITE
@end(display)

@section(Other ways of specifying a file name)

@index(files, how to specify at runtime)
@index(data files, how to specify at runtime)
@index(PROGRAM statement, effect in execution)
Most of the time, you will list all of the files you are going to
use in the PROGRAM statement.  The effect of including a file in
the PROGRAM statement is
@begin(itemize)
to specify that Pascal should ask the user for a file name for that
file when your programs starts.

for INPUT and OUTPUT only, to specify that the system should
automatically open the file when your program starts. (TTY is always opened,
and other files must be opened by RESET or REWRITE).
@end(itemize)

@index(RESET, specifying file names with)
@index(REWRITE, specifying file names with)
Sometimes it is not desirable to have your program ask the user for file
names in that way.  In such cases, the file should not be mentioned in the
PROGRAM statement.   If you do not specify a file in the PROGRAM
statement, there are two ways of determining its file name.  The most
common is to specify the file name as the (optional) second argument
to RESET or REWRITE.  E.g. if you always want INPUT to refer to the file
BASIC.DAT, you might omit INPUT from the PROGRAM statement and then
start your program with
@begin(display)

	RESET(INPUT,'BASIC.DAT')

@end(display)
You can also use a variable as the second argument.  This variable
should be a PACKED ARRAY OF CHAR, of any size.  It will contain
the file name.

If you do not list a file in the PROGRAM statement, and you do not
specify a file name for the RESET or REWRITE, the file will be
classified as an "internal" file.  Such files are intended for
use as working storage within your program.
They are automatically deleted when the program exits from the block
in which the Pascal file variable was declared.

@section(Details about terminal I/O)

@index(terminal I/O, lazy I/O)
@index(interactive I/O, lazy I/O)
@index(lazy I/O)
@index(READLN, use of lazy I/O with)
Many older implementations of Pascal had problems with terminal I/O.
This implementation uses a modified version of "lazy I/O".  This solves
almost all of the problems.  Because of our use of lazy I/O, you should
be able to do I/O with the terminal without any particular difficulty.
The following paragraphs describe the particular way we have implemented
lazy I/O.  Now and then you will find it useful to know these details.

Lazy I/O is designed to solve problems in coordinating terminal
input and output.  In particular, in older Pascals, the following
code sequence would not work:
@begin(display)
	write('Please type X: ');
	readln(X);
	write('Please type Y: ');
	readln(Y);
@end(display)
The problem is that READLN(X) is defined as doing the following
things:
@itemize{
read X

skip the carriage return/line feed at the end of the line

read the first character on the next line into INPUT^
}
It is the third action that causes the trouble.  The effect
is that you have to type the line containing Y before the
program has a chance to print 'Please type Y: '.  

The solution to this problem is very simple.  We have
redefined what READLN(X) does slightly:
@itemize{
read X

skip the carriage return/line feed at the end of the line

set things up so the next time your program refers to the
file INPUT, the first character on the next line will be
read into INPUT^
}
Notice the change in the third action.  We delay reading
the next line until it is actually going to be used.  Once READLN
has been done, the next line will be read whenever any mention
at all is made of the file.  This includes occurences of
INPUT^, another READ or READLN, EOF(INPUT), EOLN(INPUT), etc.
This modification results in the right behavior in almost
all cases.  Note that the ISO Standard specifically allows
this modification.

Our version of lazy I/O is somewhat more conservative than
what some other systems use.  Some systems delay all input
until your program examines it.  Under this interpretation,
GET(INPUT) doesn't do anything.  It just sets things up so
that the next reference to INPUT causes the actual
character to be read.  I consider this to result in 
programs that are somewhat misleading.  As it turns out,
READLN seems to be the only case where people actually have
problems.  Thus this version of Pascal delays only the
final part of READLN.  It does not delay GET, even when
you use GET to read an end of line character.  In addition,
when you first open a file, the reading of its first
character is delayed.  That is, after you do RESET(INPUT),
the first character of the file will be read the first time
you actually refer to INPUT, INPUT^, etc.


@section(Reading characters)
@label(charproc)

@subsection(Turning end of lines into blanks)

@index(end of line, reading)
@index(blanks, why end of lines turn into)
@index(EOLN)
A Pascal program can read any character except null (0).
However the Pascal language definition requires the
system to "censor" end of line characters by showing them as blanks.
That is, when Pascal reads a carriage return from the file INPUT,
what shows up in the buffer variable, INPUT^, is a blank.
There obviously needs to be some way for you to know that what
appears to be a blank is really an end of line.  This is the purpose
of the special function EOLN.  If EOLN is true, it tells you that
what looks like a blank in the input buffer variable is really
an end of line (probably a carriage return).

@index(GET, using to skip end of line)
The Pascal language considers an end of line to be a single
character.  So when EOLN is true, a single GET will read the
first character on the next line.  This is true even though
most lines on a DECsystem-10 or DECSYSTEM-20 end with a carriage-return
followed by a line feed.  That is, when the file is positioned at
a carriage return, the next GET skips the following line feed,
and reads the first character on the next line.  (Of course when
the file is positioned at a carriage return, what you see in
the buffer is a blank.)

@index(end of line, which characters are)
@index(end of line, seeing the actual characters)
@index(/E file opening option)
There are five different characters that can end a line:
carriage return, line feed, escape, form feed (^L), and control-Z(^Z).
Sometimes you need to know which particular end of line character
actually appeared.  In this case, you can RESET the file in
a special mode.  This mode turns off Pascal's "end of line censor", and
causes the actual end of line character to appear in the input buffer,
instead of the usual blank.  When this mode is in effect, you can still
check whether the buffer contains an end of line character by using
EOLN.  Indeed this is the recommended practice.  However in this mode
carriage return is treated as a single character, separate from the line
feed.  So if a line ends with carriage-return followed by line feed,
you would need two GET's to advance to the beginning of the next
line.  For this reason READLN is recommended as the safest way to
get to the beginning of the next line.  READLN will always get to
the first character of the next line, no matter how the line
ends.  To open a file in the mode where you see the end of line character,
specify /E in the options string used in the RESET.  For example
@begin(display)

	RESET(INFILE,'FOO.BAR','/E')
	RESET(INFILE,'','/E')

@end(display)

@index(:#)
@index(#)
@index(PROGRAM statment, specifying see end of lines in)
Normally Pascal opens the file INPUT for you automatically.
To make Pascal's automatic open use '/E', you should specify
INPUT:# in the PROGRAM statement.  You may also request the special file
TTY to be opened with '/E' by listing TTY:# in your PROGRAM statement.
This is the only reason for listing TTY in a PROGRAM statment. TTY is
always opened, even if you don't list it.  So the only reason for
listing it is if you want to specify a special option.

'/E' and :# are extensions.  That is, they are not present in
implementations of Pascal on other computers.  So do not use either of them
if you want your program to be transportable to other systems.

@subsection(Reading strings)

@index(strings, reading)
@index(READ with strings)
@index(break characters for reading strings)
@index(sets, using as break set for reading strings)
@index(array, reading into array of characters)
This implementation allows you to read a whole sequence of characters
at once with a single READ or READLN.  In the simplest case, you
might ask to read an entire line into a single PACKED ARRAY of CHAR.
More complex capabilities are also available, modelled after
SAIL's very fine string input routines.  An example
of the full syntax is
@begin(display)

	read(input,array1:howmany:['@ ',':'])

@end(display)
This will read characters into the array ARRAY1 until one of three things
happens:
@begin(itemize)
One of the characters mentioned in the "break set" (in this case
blank or colon) is read.  This character (the "break character") is
not put into the array.  You can find it by looking at INPUT^, since
this always contains the next character that will be read by READ.
HOWMANY is set to the number of characters actually put into the array.
Any integer variable could be used in place of HOWMANY.

End of line is reached in the input.  Again, HOWMANY is set to the
number of characters put into the array.  You can tell that this
is what happened because EOLN(INPUT) will be true.

The array is filled.  In this case, INPUT^ is the character that would
have overflowed the array.  You can tell that this is what happened
because HOWMANY is set to one more than the size of the array in
this case.
@end(itemize)
If the read operation stopped before the end of the array,
the rest of the array is cleared to blanks.

If you don't need a break set, you can leave it out.  You would use
this simpler form of READ if you just wanted to read into the
array until it fills or you get to the end of a line.  Here is
an example of this form of READ:  READ(S:I).

If you don't want to know how many characters were actually read,
you can also leave out the integer variable.  This would leave
you with a statement that looked like READ(S).  The READ operation
works the same way whether you put in an integer variable or not.
If you don't put it in, you just don't know how many characters
were read.

WARNING:  The integer variable used with READ looks a lot like
the field width specification used with WRITE.  Please do not
confuse these two things.  READ(X:I) does not specify a field width
of I.  Rather it asks for I to be set by the read operation to show
how many characters were actually read.

@Chapter(Additional Input/Output Facilities)

In some ways this chapter is a continuation of the previous
one.  It describes additional I/O facilities available in
this implementation.   

@section(Extra options in file I/O)

@label(open)

@index(RESET, extra options)
@index(REWRITE, extra options)
@index(file opening, extra options)
This implementation supplies you with a number of optional
features that you can use when you do I/O.  Most of
these features are controlled by two extra arguments to RESET and
REWRITE.  In standard Pascal, RESET and REWRITE take only one argument,
the Pascal file identifier.   Here is an example of a RESET that
uses the extra arguments in order to take advantage of features present
only in this implementation:
@begin(display)

	RESET(INFILE,'TEST.DAT','/U/E')

@end(display)

@begin(description)
INFILE	This is the Pascal file variable.  It represents the file
throughout the rest of your program.  You must have declared
it in the VAR section as FILE OF something.

'TEST.DAT'	This is the file name.  You should specify the
file name in this way if you want the program to determine
the name of the file.  If you want to ask the user for the
name of the file, it is better to list the file in the PROGRAM
statement.  Pascal will ask the user automatically about every file
listed in the PROGRAM statement.  Usually it does not make sense
to list a file in the PROGRAM statement and to supply a file name
in RESET and REWRITE.  This argument is either
a name in quotes or a variable (of type PACKED ARRAY OF CHAR) containing
a file name.  You can use '' to indicate that you are not supplying
a file name.  This would be useful if you want to specify the
third argument but not the second.

'/U/E'	This is the option string.  It allows you to specify
various details about how the file will be processed.  It consists
of a number of letters.  Each of them is an abbreviation for an option
that you want to select.  Each letter must be preceeded by a slash.  The
most common options will be described below.
@end(description)

Here are the options that you can specify in the option string:

@index(byte size, specifying in file opening)
@index(/B file opening option)
@begin(description)
/B:nn	Byte size specification.  Input and output is done in bytes.
In a text file, Pascal uses one byte in the file for each character 
read or written.  If you do not specify the byte size, Pascal will
use a byte size of 7 for text files.  This is what other system
software uses.  In any file other than a text file,  Pascal uses
one byte in the file for each word in the file component.  If
you do specify the byte size, Pascal will use a byte size of 36 bits
for these files.  This will allow your data to be stored exactly
as it appears in memory.  If you specify a byte size, Pascal will
use it.  You should do this only if you have a clear idea of
how items of data are going to appear in the file.  8 bit bytes are
sometimes useful when dealing with industry-compatible magtape.

@index(/D file opening option)
@index(errors, handling data transmission yourself)
@index(data transmission errors, handling yourself)
/D	Data transmission errors will be handled by the user.  See
the section below on error handling.   A data transmission error is
usually a physical problem of some sort. See /F if you want to be able
to handle problems with the format of the data.

@index(/E file opening option)
@index(end of line, seeing the actual characters)
/E	End of line characters will be visible to the program.  Normally
Pascal programs put a blank in the input buffer at the end of line.
If this option is specified, the actual end of line character will
appear in the buffer.  Normally a single GET will read past both a
carriage return and a line feed, since they form a single line
terminator.  If /E is set, the carriage return and the line feed will be
treated as separate characters.  However, READLN will still skip them both.

@index(/F file opening option)
@index(errors, handling format yourself)
@index(format errors, handling yourself)
/F	Format errors in the data will be handled by the user.  See
the section below on error handling.  A format error occurs when the
data is readable, but is not what READ wants. E.g. such an error will
occur if you try to read a number, but the next character in the
file is a letter.

@index(/I file opening option)
@index(interactive files, specifying)
@index(terminals, specifying interactive mode for)
/I	Interactive file.  This disables the use of "lazy I/O", 
discussed above.  Only experts should use this.  See the Reference
Manual for a description of the details of "interactive" I/O.

@index(/M file opening option)
@index(I/O mode, specifying)
@index(mode of I/O, specifying)
/M:nn	Mode specification.  Currently you can only use this specification
on Tops-20.  It allows you to specify the internal software mode that
Pascal will use to process a file.  This option will probably not be
much use to a normal user.  It is discussed in detail in the
reference manual.

@index(/O file opening option)
@index(errors, handling file opening yourself)
@index(file opening errors, handling yourself)
@index(opening errors for files, handling yourself)
/O	Open errors will be handled by the user.  See
the section below on error handling.  An open
error is an error that occurs during the RESET or REWRITE.  The most 
common kinds of open error are for the specified file to be missing
or for it to be protected such that you are not allowed to access it.

@index(/U file opening option)
/U	Upper case the file.  All lower case letters will be turned
into the equivalent upper case.  Only letters are affected.
@end(description)

@subsection(Labelled tapes)

@index(labelled tapes)
@index(tapes, labelled)
@index(magnetic tapes, labelled)
@index(blocking factor, for labelled tapes)
@index(record size, for labelled tapes)
@index(record format, for labelled tapes)
@index(format of record, for labelled tapes)
Tops-20 Pascal has fairly powerful facilities for dealing with labelled
tapes.  These facilities use Tops-20's special labelled tape support,
so they are not yet available for Tops-10.  To read a labelled tape,
you normally do not need to do anything special.  However when you
write a labelled tape, you may want to specify exactly
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
record headers to show where lines begin and end.  Carriage returns
are not used in this format.  When dealing
with such a file, WRITELN causes the current line to written out,
with the proper header at the beginning.  It does not cause
a carriage return line feed to be written.  EOLN is turned on when
you come to the end of a line, as defined by the header at the
beginning of it.  Most other computers understand this format, but do not
know about the use of carriage returns to define lines.  Unless you really
know what you are doing, you will only be able to use D format for text files
(files declared TEXT or FILE OF CHAR).  To use this mode, you
should specify something like "MT0:;FORMAT:D;BLOCK:5000".
The block size should be chosen large enough to make reasonably efficient use
of tape, but not to waste memory.

F (fixed)	This format is also useful when you need to communicate with
other computers.  In it there is nothing on the tape
to show where lines end.  There are no carriage returns and no headers.
In order to make it work, you must declare a fixed line length.
When writing a tape, each line will be filled with blanks to be
the length you specify.  When reading, the system knows where a
line ends by counting characters.  The example
above showed how to specify format F.  You should specify both a
block size and a record size.  The record size is the length to which
you want all lines filled.  The system will put as many records
of this size as it can into one physical tape block. The block size must
be an even multiple of the record size.  Again, the block size should
be big enough not to waste tape.  Unless you are an expert you
will only be able to use this mode for text files.

S (spanned)	This is a somewhat unusual mode.  It is very
similar to mode D.  However the record headers are written in
such a way that records can cross block boundaries.  This mode
makes somewhat more efficient use of tape, but is more complex
to process.  Many other computers do not support it.
@end(description)

You do not need to worry about decoding record headers, filling
lines to be the right length, or counting characters to determine
the end of a line.  Pascal does all of these things for you.
You only need to specify what format you want by using the
appropriate attributes.  WRITELN and EOLN still work as they
always do for disk files.  However instead of dealing with
carriage returns they deal with record headers or character
counts.

When you are reading a labelled tape, Pascal can tell the
structure of the tape from the label.  Thus you should not
have to specify any attributes for an input file.

@subsection(I/O Error processing )

@index(errors, processing I/O)
@index(I/O errors, processing)
@index(input/output errors, processing)
@index(output errors, processing)
@index(data transmission errors, processing)
@index(format errors, processing)
@index(opening errors, processing)
@index(file opening errors, processing)
Errors may occur at three different times.  You may
decide separately how Pascal should handle each of these three error types.
@begin(itemize)
Data transmission errors: errors occuring during physical I/O operations
(GET and PUT).   These would normally indicate some problem with the
device or medium.

Format errors: errors occuring during number conversion (READ and WRITE).
These would be errors such as having a letter appear where you requested
that a number be read.

Opening errors: errors during file opening (RESET and REWRITE).
These would be errors having something to do with the file system,
e.g. that the file you requested is not present.
@end(itemize)

When an error occurs, the system checks to see whether you specified
that you want to handle this error.  You would have specified this by
giving the corresponding switch in the option list when you opened 
the file.  If you didn't ask to handle this kind of error,
the I/O system will take a default action.  The default actions
are as follows:
@begin(itemize)
Data transmission errors:  Print an error message and terminate the program.

Format errors:  If the file involved is assigned to a
terminal, ask the user is given to retype his input, and
then continue with the program.  If the file is not on a terminal, print
an error message and terminate the program.

Opening errors:  Ask the user for a new file name.
@end(itemize)

@index(EOF, after I/O error)
If you did specify that you want to handle the appropriate kind of error,
the I/O system will not attempt to do any error recovery of its own.
The I/O procedure that your program was executing
will be aborted, and your program will continue.  As an indication
that something odd has happened, EOF (and EOLN) will be set.  Normally,
future I/O operations will do nothing until you recover the error.
The runtimes are constructed so that the high-level read  routines (READ
and READLN) return immediately with no effect when EOF is set.  Other
routines proceed as usual, but usually the system sees to it that
they have no effect.  The moral is, if you set one of the magic bits, 
you had better check EOF (or ERSTAT, to be explained below) now and then,
or your program may be trying to process non-existent data.

@index(ERSTAT function)
@index(CLREOF procedure)
@index(end of file, clearing after error)
One would hope that you will ask to handle errors yourself only
if you intend to do something about them. To do so you need two tools: a
function to tell you what kind of error has happened, and a procedure to
clear out error conditions.  These are ERSTAT and CLREOF, respectively.
To use these, you must declare them near the beginning of your program.
Put the following declarations anywhere that procedure declarations
would be legal, at the top (global) level of your program:
@begin(display)

	function erstat(var f:file):integer; extern;
	procedure clreof(var f:file); extern;

@end(display)
They both take one argument, the name of a file.  ERSTAT returns an
internal code for the most recent error, or 0 if no error has happened.
Trying to read past the end of file is considered an error.
(Its code is 600220B on Tops-20, and 20000B on Tops-10.)
CLREOF clears an error condition.  It restores
EOF and EOLN to their normal states, and clears the error condition
in the operating system.  If you wish to continue with (or retry)
I/O, you should call CLREOF before continuing.

@index(quota exceeded error message, how to proceed)
Note that the error "Quota exceeded or disk full" cannot currently
be processed by your program.  The Pascal I/O system (Tops-20), or the monitor
(Tops-10) will print an error message and advise the user to type
CONTINUE after having freed up some disk space.

@section(CLOSE)
@label(close)

@index(CLOSE procedure)
@index(closing files)
@index(files, closing automatically or by CLOSE)
Pascal automatically closes all files when a program ends. However there
are cases where you may want to close a file in the middle of a program.
You may want to do this to make the file available to other programs, or
simply to prevent the program from changing it accidentally later on.
Thus the procedure CLOSE is available.  The normal call is
@begin(display)

	CLOSE(file)

@end(display)
@index(jfn, association with Pascal file)
This closes the file, but does not release Pascal's internal pointer
to it (the "jfn" on Tops-20, a data block containing the name
of the file on Tops-10).  Thus any future RESET or REWRITE of this
file will use the same file name, unless it includes an explict file name.
If the Pascal file variable is omitted, OUTPUT is used as a default.

@section(RENAME)
@label(rename)

@index(RENAME procedure)
@index(renaming files)
@index(files, renaming with RENAME)
You may use RENAME(file,newname) to rename an existing file.
The newname argument is treated the same way as the name
argument for RESET and REWRITE.  It specifies the new name for the
file.  If the operation works, EOF is set to false, otherwise
to true.  (On Tops-10, there may be some problem here at the
moment.)  The system must know which existing file you are referring to.
On Tops-10, the file must be open.  This means that you must have
done a RESET or REWRITE, and not closed it since.  On Tops-20 
you only need to have a jfn for the file.  Specifying the file
in the PROGRAM statement will cause Pascal to get a jfn for
it.  Opening the file with RESET or REWRITE will also get a jfn.
CLOSE will not lose the jfn unless you specifically ask for the
jfn to be returned.  (We have not told you how to do that.)  Once the
RENAME is finished, the file will be closed.

@section(DELETE )
@label(delete)

@index(DELETE procedure)
@index(deleting files)
@index(files, deleting with DELETE)
DELETE(file) will delete the mentioned file.  On Tops-10, DELETE is
valid only on a file that has been RESET and not CLOSE'd. (On some older
versions of Tops-10, you can also open it with REWRITE.)  On Tops-20
you just have to make sure that Pascal has a jfn for the file.  This
will be the case if it was mentioned in the PROGRAM statement or if
RESET, REWRITE, etc., was done on it.  A CLOSE will not lose the jfn.

@section(UPDATE)
@label(update)

@index(UPDATE procedure)
@index(files, updating)
In standard Pascal, the only way you can change a file is
by reading it and writing a new copy.  In this implementation,
you can change files without recopying them, as long as
they are stored on a random-access device (probably disk).  If you intend
to change a file in this way, you must open it by using the procedure
UPDATE instead of RESET or REWRITE. When you have opened a file with
UPDATE, you may use both GET and PUT (or READ and WRITE) with it.  There
is a single "current position" within the file, which is advanced by
both GET and PUT.

UPDATE has the same arguments as RESET.  At a few places
in this manual, we say that a file must have been opened by
RESET or REWRITE.  If a file was opened by UPDATE, that
is OK too.

Note that EOF is normally false for a file opened by UPDATE.  Pascal
will turn on EOF if you try to read beyond the end of file or if an error
occurs.  Of course you will never see EOF after an error unless you
requested user error handling in your UPDATE.  (See @ref(open).)
Note that it is perfectly legitimate to write beyond the end
of file.  Pascal simply moves the end of file to be after the
new data.

@section(Random access)
@label(randac)

@index(random access files)
@index(position in file, looking at and changing)
@index(files, random access)
@index(CURPOS function)
@index(SETPOS procedure)
@index(byte number in file, looking at and changing)
It is possible to move around a file randomly when it is on a
direct-access device (i.e. disk or some equivalent).  In order
to use this facility, you must know what a "byte" is.  Unless
you are doing something weird, a byte is one character for
normal text files, and a word for other (binary) files.
The procedures that implement random access use a byte serial number to
keep track of their position.  I will henceforth call this the
"position".  This number refers to the number of bytes between the
beginning of the file and the position in question.  Thus the beginning
of the file is position 0.

CURPOS(FILE) returns the current position of the file. This is the
position at which the next thing to be read or written would begin. When
a file has just been opened, the position is, of course, 0. Note that
CURPOS may give an error if used with a file not on disk.

SETPOS(FILE,POSITION) sets the file buffer so it points to the
position indicated.  
@begin(description)
@index(GET, automatic, with SETPOS)
If the file is open for input
(it was opened with RESET or UPDATE), SETPOS goes to the position
indicated and gets the record there into the Pascal buffer.  This
involves reading a record (effectively doing GET(file)).  Because
of this reading, the current position immediately after a SETPOS
is the position you gave it + the record size of the file.  Because
of this, SETPOS followed by a GET or PUT may not do what you 
expected.  You may prevent this builtin GET by using an extra
",true".  See below.

If the file is open for write
(it was opened with REWRITE or APPEND), SETPOS goes to the position
indicated and clears the buffer.  

SETPOS(file,position,true) is a special form that prevents reading
the record, in case the file is open for input.  The file buffer
is cleared.  After this call, the current position is the position
requested in the SETPOS call.  Unless you use ",true", SETPOS will
always do an automatic GET if the file was opened with RESET or UPDATE,
no matter what options you used in the RESET or UPDATE statement.
@end(description)

This convention of setting the buffer to the record specified
has caused some confusion.  The following code sequences will
probably not do what you had in mind.

@begin(programexample)
update(f);
setpos(f,3);
f^ := <stuff>;
put(f);
@end(programexample)

The problem here is that SETPOS reads the record at location 3,
leaving the current position somewhere later.  Thus the PUT
writes a record at a later location.  The writer should have
used SETPOS(f,3,true) to prevent reading the record at 
location 3.

@begin(programexample)
update(f);
setpos(f,3);
f^.use := f^.use + 1;
put(f)
@end(programexample)

Here the user wanted the SETPOS to read the record at position
3.  He then changed it and wrote it out again.  Unfortunately
the PUT does not put the changed record back at position 3.
Because the SETPOS read a record, it leaves the file pointer
positioned after that record.  The PUT then writes over the
next record in the file.  The writer should have used PUTX
instead of PUT.  This rewrites the most recently read record.
In this case, it is equivalent to SETPOS(f,3,true); PUT(f).

@begin(programexample)
rewrite(f);
f^.name := 'hedrick';
f^.num := 45892;
setpos(f,123);
put(f)
@end(programexample)

Here the user thought he was writing a record containing 'Hedrick'
and 45892 at position 123.  However SETPOS clears the file buffer
in the cases where it doesn't read a record.  So a blank record
was written at position 123.

On Tops-10, SETPOS is only legal
with files opened for input (i.e. with RESET or UPDATE).  On Tops-20,
it is sometimes possible to do SETPOS on files opened for output only
(i.e. with REWRITE or APPEND).

If you SETPOS to a position beyond the end of file, the next
read will set EOF.  If you really wanted to read, you should
SETPOS to a more reasonable position.  This will reset EOF.
It is perfectly legal to write
beyond the end of file, however.  Doing so extends the length
of the file.  If you SETPOS to a position beyond the end of
the file on Tops-20, it is possible to leave non-existent pages between
the old end of file and the newly written part.  These should
cause no trouble to PASCAL (such pages are treated as full of
binary zeroes), but may for programs written in other languages.

@section(I/O to strings)
@label(strio)

@index(strings, processing with I/O functions)
@index(arrays of char, processing with I/O functions)
@index(REREAD, Pascal equivalent of)
@index(REWRITE [Fortran], Pascal equivalent of)
@index(formatting, using I/O to strings)
@index(STRSET procedure)
@index(STRWRITE procedure)
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

@index(GETINDEX procedure)
There is a procedure to see where you currently are
in the string.  It is GETINDEX(file,variable).  Variable is set to the
current index into the array.  This is the index of the thing that will
be read by the next GET (or written by the next PUT).

@index(errors, none in string I/O)
Note that no 
runtime error messages will ever result from string I/O.  Should you run
over the end of the string, PASCAL will simply set EOF (or clear it if you
are doing output).  It will also set EOF if you read an illegal format
number.  (GETINDEX will allow you to discriminate these two cases, if you
care.)

@index(substring)
There is also a fourth optional argument to STRSET and STRWRITE.
This sets a limit on how much of the array will be used.  It thus
gives you the effect of the substring operator in PL/I.
For example, STRWRITE(F1,AR1,3,6) will make it possible to change
characters 3 to 6 inclusive.  If absent, the fourth argument
defaults to the last location in the array.

Beware that it is possible to set a file to an array, and then
exit the block in which the array is defined.  The file is
then pointing out into nowhere.  This is not currently detected.

@chapter(Extensions to Pascal Syntax)

This is a supplement to chapter 2 of Cooper.  It describes
the OTHERS option for the CASE statement, and the LOOP statement
(an entirely new statement type).

@section(Extended CASE Statement)

@index(CASE statement, OTHERS option)
@index(OTHERS in case statement)
In standard Pascal, you must list every case that can happen in a CASE
statement.  If a case comes up that you have not listed, the effect is
undefined.  This implementation allows you to define what will happen
when cases come up that you have not listed.  To do this, use the
special case OTHERS. This case will be executed if the  control
expression does not match any of the other cases. If you use OTHERS, it
must be the last case listed.

Here is an example of how you might use OTHERS.  X is assumed to
be a variables of type CHAR.
@begin(display)

   CASE X OF
	'A' : WRITELN('VALUE IS A');
	'B' : WRITELN('VALUE IS B');
	OTHERS : WRITELN('VALUE IS NEITHER A NOR B')
   END   %CASE STATEMENT\

@end(display)

@section(LOOP Statement)

@index(LOOP statement)
@index(EXIT IF statement)
Standard Pascal provides only two rather limited kinds of
loops.  WHILE makes its test at the beginning.  REPEAT
makes its test at the end.  This implementation provides
LOOP, a statement that allows you to put the test
anywhere within the loop.

The LOOP statement has the following syntax:
@begin(display)

   <loop statement> ::= LOOP
		          <statement> [; <statement> ]
			EXIT IF <expression> ;
			  <statement> [; <statement> ]
			END

@end(display)
The expression must result in a Boolean value.  Note that there must
be exactly one EXIT IF in each LOOP.  It must not occur
inside any other statement.

@Chapter(Standard Procedures and Functions)

The following standard procedures and functions  (described in the
Revised PASCAL Report(ref)) are implemented.  

@index(functions, standard)
@index(procedures, standard)
@index(ABS function, implemented as in Report)
@index(SQR function, implemented as in Report)
@index(ODD function, implemented as in Report)
@index(SUCC function, implemented as in Report)
@index(PRED function, implemented as in Report)
@index(ORD function, implemented as in Report)
@index(CHR function, implemented as in Report)
@index(TRUNC function, implemented as in Report)
@index(ROUND function, implemented as in Report)
@index(EOLN function, implemented as in Report)
@index(EOF function, implemented as in Report)
@index(SIN function, implemented as in Report)
@index(COS function, implemented as in Report)
@index(EXP function, implemented as in Report)
@index(LN function, implemented as in Report)
@index(SQRT function, implemented as in Report)
@index(ARCTAN function, implemented as in Report)
@index(GET procedure, implemented as in Report)
@index(PUT procedure, implemented as in Report)
@index(RESET procedure, implemented as in Report)
@index(REWRITE procedure, implemented as in Report)
@index(NEW procedure, implemented as in Report)
@index(READ procedure, implemented as in Report)
@index(READLN procedure, implemented as in Report)
@index(WRITE procedure, implemented as in Report)
@index(WRITELN procedure, implemented as in Report)
@index(PAGE procedure, implemented as in Report)
@index(PACK procedure, implemented as in Report)
@index(UNPACK procedure, implemented as in Report)
@begin(display)


  Standard Functions   Standard Procedures

	ABS	       	GET 
	SQR	       	PUT 
	ODD	       	RESET (See @ref(open))
	SUCC	       	REWRITE (See @ref(open))
	PRED	       	NEW
	ORD	       	READ
	CHR	       	READLN (See @ref(charproc))
	TRUNC	       	WRITE
	ROUND	       	WRITELN
	EOLN	       	PAGE
	EOF             PACK
	SIN	       	UNPACK
	COS
	EXP
	LN
	SQRT
	ARCTAN


@end(display)
Additional mathematical functions are available:
@index(ARCSIN function)
@index(ARCCOS function)
@index(SINH function)
@index(COSH function)
@index(TANH function)
@index(SIND function)
@index(COSD function)
@index(LOG function)
@begin(display)

	ARCSIN	       	SIND
	ARCCOS	       	COSD
	SINH	       	LOG
	COSH
	TANH


@end(display)

@index(** operator missing - use POWER)
@index(exponentiation)
@index(POWER function)
@index(IPOWER function)
@index(MPOWER function)
The following functions may be used to simulate the missing
** operator.  They must be declared EXTERN.  (** was left out of
the language intentionally by its original designer.)

@begin(description)
FUNCTION POWER(X,Y:REAL):REAL; EXTERN; - X ** Y

FUNCTION IPOWER(I,J:INTEGER):INTEGER;EXTERN -  I ** J

FUNCTION MPOWER(X:REAL;I:INTEGER):REAL;EXTERN - X ** I
@end(description)

Additional standard functions.  Those prefixed by a star are not
described in this document.  See the full manual.

@begin(description)

CURPOS(file) returns the current position in a file.  See section @ref(randac).
Only valid for files on random access device.  (type integer)

@index(DATE function)
DATE  result is a PACKED ARRAY [1..9] OF CHAR.
The date is returned in the form 'DD-Mmm-YY'.

@index(NEXTFILE procedure)
@index(wildcard, advancing to next file)
*NEXTFILE(file).  Advances to the next spec for a wildcard file.

@index(RANDOM function)
@index(random number generator)
RANDOM(dummy)  result is a real number in the interval 0.0 .. 1.0.  Ignores
its argument, but an argument is required.

@index(RECSIZE function)
@index(record size in file, finding)
@index(file, finding size of record)
*RECSIZE(file) returns the record size of the file.

@index(RUNTIME function)
@index(time, CPU time so far)
RUNTIME  elapsed CPU time in msec (type integer)

@index(TIME function)
@index(time, since midnight)
TIME  current time in msec (type integer)

@end(description)

Additional standard procedures:

@begin(description)

@index(APPEND procedure)
@index(appending to existing file)
@index(file, appending to)
*APPEND(file,name,...).  Like REWRITE, but extends an existing file. 

@index(BREAK procedure)
@index(forcing output to file)
*BREAK(file). Forces out the output buffer of a file.

@index(BREAKIN procedure)
*BREAKIN(file,noget). Clears the input buffer count.  

@index(CALLI procedure)
@index(monitor calls)
@index(operating system calls)
*CALLI(calli number ...). Arbitrary monitor call.  Tops-10 only.

CLOSE(file,bits). Close file and release its channel. See section @ref(close).

DELETE(file). Delete file.  See @ref(delete).

@index(DISMISS procedure)
@index(files, aborting creation)
@index(aborting creation of file)
*DISMISS(file). Abort creation of a file.  

GETINDEX(file,index). If file is open on a string (STRSET or STRWRITE),
sets index to current index into the string.  (See section @ref(strio).)

@index(GETLINENR procedure)
@index(line number in file, reading)
@index(files, reading line number in)
*GETLINENR(file,lineno).  Lineno must be a packed array of char.  It
is set to the last line number seen in the file.  If no line numbers
have been seen '-----' is returned.  '     ' is returned for a page
mark.  If file is omitted, INPUT is assumed.

@index(JSYS procedure)
@index(monitor calls)
@index(operating system calls)
*JSYS(jsysnumber, ...). Arbitrary monitor call.  Tops-20 only.

@index(MARK procedure)
@index(heap)
*MARK(index). Save state of the heap.  

@index(PUTX procedure)
*PUTX(file). Rewrite record in update mode.  

@index(RCLOSE procedure)
@index(jfn, releasing)
@index(releasing file)
@index(file, close releasing)
*RCLOSE(file).  Close, releasing jfn.  

@index(RELEASE procedure)
@index(heap)
*RELEASE(index).  Restore saved state of the heap.  

RENAME(file,name,...). Rename an open file.  See @ref(rename).

SETPOS(file,position).  Move in random access file.  See @ref(randac).

STRSET(file,array,...).  Open input file on array.  See @ref(strio).

STRWRITE(file,array,...).  Open output file on array.  See @ref(strio).

UPDATE(file,name,...). Open random access file for revising in place.
See section @ref(update).
@end(description)

@chapter(Things to look out for)

@index(pitfalls, general)
There are a number of things you can do in a program that will result in
trouble.  The Pascal system will look for some of them and warn you. But
it does not check for every possible error.  If you program does not work,
you might take a look at the following list, and see whether your program
might contain one of the errors shown in it.  Unless otherwise specified,
this compiler will not detect the errors listed below.

@section(Errors that this compiler won't tell you about)

You should make sure that all variables are defined before you use them.
This applies also to file buffer variables.  The most dangerous kind of
undefined variables are pointers, as anyone who has debugged pointer
programs will know.  The compiler will detect them if you specify {$Z+}
at the beginning of your program.

If you have a function, you should make sure that you always assign it a
value.

You should not use DISPOSE on a record if there is some other record, or
some pointer variable, that is still pointing to it.

If you are using variant records, you should be very careful about what
you are doing.  For example, consider the following example:
@begin(display)
   TYPE PERSON = RECORD
	SSN: INTEGER;
	CASE SEX OF
		MALE: (BATTING_AVERAGE: REAL; AGE: INTEGER; 
			BEER: BEER_BRAND);
		FEMALE: (HAIR_COLOR: COLOR)
	END;
@end(display)
You have to treat any give person as either consistently male or
consistently female.  If you first assign a value to HAIR_COLOR and then
try to look at the BATTING_AVERAGE, you will end up getting very strange
results.  This is because those two variables share the same storage.
You would get the color, which is in fact a small integer, interpreted
as a real number.  It is particularly dangerous to use NEW with an extra
argument, e.g. NEW(P,FEMALE). This will generate a new record with only
enough space to store the fields needed for the female variant, that is
HAIR_COLOR. If you try to refer to BEER, you will be refering to
something that is not in the record at all.  It is easy to do that
without intending to.  The most common way is by an assignment
statement, e.g. P^ := THISPERSON.  This copies the record THISPERSON to
P^.  The assignment statement has no way of knowing that P^ is really a
short record, so it copies the largest possible variant of the record,
in this case the male variant.  This will probably garbage some other
record that happened to be after P^.

When you assign a value to a variable or field that has a subrange type,
Pascal is supposed to check to make sure that the new value is within
the limits.  For example, if you declare A: 1..5, the compiler should
make sure that you never give A a value outside the range of 1 to 5.
This compiler will check that in simple cases, such as A := 6, but will
not in more subtle cases.  Examples of the things it will not detect are
passing A to a procedure by reference, where the procedure sets it to an
illegal value, or reading an illegal value into A by a READ or READLN
statement.

If you have a set, and that set is defined over a subrange, Pascal is
supposed to check to make sure that no value outside the subrange
shows up in the set.  For example, if you declare S: SET OF 1..5,
you should never put a number outside the range 1 to 5 into the set,
Also, you not assign a new set value to S if the new value might have a
member outside the range.

If you have more than one array listed in a single conformant array
specification, you should make sure that they have the same lower and
upper bounds.  For example, consider PROCEDURE P1(VAR
A,B:ARRAY[I..J:POS] OF REAL).  Sine I and J are supposed to represent
the bounds of both A and B, A and B must have the same bounds. Also, if
POS happens to be a subrange, you should make sure that A and B have
bounds that fit the subrange. For example if POS is 1..500, you should
make sure that A and B have bounds that are within the range 1 to 500.

If you use CHR, you should make sure that its argument corresponds to
some character.  In effect that means that it must be between 0 and 127.

@section(Extensions)

There are a number of other things you can do that will not cause
trouble with this compiler, but which may mean that your program will
not work on some other kind of computer.  The clearest examples of this
are using features that are described in this document as "extensions".
For example, RESET with more than one argument, APPEND, UPDATE, SETPOS,
the LOOP statement, and the OTHERS case of the CASE statement.  We will
not list them here, since we think it will be clear enough to you
that they will not work somewhere else.  

However there are other more minor things that this compiler accepts
which it really shouldn't.  They are present mostly for historical
reasons, and we don't recommend their use.  Thus we have not documented
them elsewhere as features.  However it is possible that you could end
up using them accidentally. We provide a list here so you will know what
to look for if you are planning to move your program to another
computer.  To repeat, the following features are *not* part of ISO
Pascal.  They apply only to this compiler.  You should avoid them if you
want to be able to move your programs.

The symbol "_" is regarded by this compiler as a letter.

Numbers can end be written in octal or hexadecimal.  A 'B' after a
number makes it octal.  A '"' before a number makes it hexadecimal.

The symbols ":" and ".." may be used interchangably.  That is, anywhere
that the Standard requires ":", you may use ".." and visa versa.

Labels are permitted to have any non-negative integer value.  (The
Standard requires them to be in the closed interval 0 to 9999.)

Comments may be written with the pairs of symbols { }, /* */, (* *),
and % \. Many other compilers accept both { } and (* *).  However the
Standard requires that they be used interchangable.  That is, { *) is a
legal comment.  This compiler treats each such pair as a separate kind
of comment, thus allowing them to nest.  For example, { This is (* legal
*) in our compiler}.  In a normal compiler, the first *) would end the
comment.  So if you want your program to be standard, you should only
use one set of comments (probably { }), or you should be careful never
to nest them.

A number of other symbols have alternate forms.  Here is a list.
The alternate forms are present only for historical reasons.  You
should not use them in new programs.
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


Any array of CHAR (or of a type compatible with CHAR) is regarded as a
string.  The Standard treats only packed arrays whose lower index bound
is 1 as strings.  Thus in this compiler, any array of CHAR can be
written using WRITE or WRITELN, and compared by the lexical order
relational operators.

This compiler does not make any distinction between PACKED and non-PACKED
sets and files.

This compiler considers any file of CHAR (or subrange of CHAR) to
be a text file.  Thus you can use the full form of READ and WRITE
on them.  According to the Standard, if you want a text file, you
must declare it TEXT.  FILE OF CHAR will not do.

Type compability checking in this compiler is somewhat looser than that
specified by the Standard.  If you have VAR A: ARRAY [1..10] OF INTEGER;
B: ARRAY [1..10] OF INTEGER, we consider A and B to be compatible.
The Standard does not.  We also consider similar files to be
compatible, and the Standard does not.  If you want your program to
be standard, you should either specify A and B together, e.g.
A,B: ARRAY [1..10] OF INTEGER, or you should declare a type, e.g.
ARRAY10 = ARRAY [1..10] OF INTEGER and then use A: ARRAY10; B: ARRAY10.

According to the Standard, it is illegal to pass by reference any member
of a packed structure or the tag field of a record. In this
implementation, most of this limitation does not apply.  

According to the Standard, the actual parameters corresponding to formal
parameters in a single conformant array parameter
specification must all have the same type.  This implementation only
requires that each actual parameter individually must be compatible with
the conformant specification. To give an example, consider a procedure
with two parameters declared
A,B: ARRAY [I..J: INTEGER] OF REAL.  Now consider the following
variables: X,Y: ARRAY[1..10] OF REAL; Z: ARRAY[1..10] OF REAL.
This compiler would allow you to pass X and Z to the procedure, since
both X and Z are compatible with the conformant array specification. The
Standard would allow you to pass X and Y but not X and Z. X and Y have
the same type, whereas X and Z do not.

A set of rules is given in the Standard specifying what labels it is
legal to go to with GOTO.  In this compiler, it is legal to go to any
label in the current block or any containing block.  Doing a GOTO into 
the middle of a loop, case statement, etc., can be dangerous with many
compilers, but the code generated by this compiler is so simple that
such things will produce the expected results here. The one questionable
case is doing a goto into the middle of a for loop.  Even there, if the
value of the control variable is defined, it will work in this compiler.

According to the Standard, it is an error if a case statement specifies
an index that does not match one of the cases.  In this implementation
such a situation is legal.  If no case matches the value of the case
index, a null case is used.  This case consists of a null statement.

According to the Standard, the controlling variable of a FOR loop
must be a simple variable, and it must be declared in the block
in which the FOR loop appears.  In this implementation the variable
may declared in an outer block, or it may be global.

