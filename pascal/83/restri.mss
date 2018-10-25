@chapter(Conformance of this implementation to ISO/DP 7185)

Note to the reader:  Because ISO standards have the status of law in
some countries, this chapter is worded with the same sort of precision
as the standard itself.  In our opinion, the ISO standard is about the
most lousy piece of English prose ever created.  That tends to make this
document almost as unintelligible.  In cases where it seems that the
normal user is probably not going to understand what we are saying, an
English paraphrase is given in brackets.  Such paraphrases should be
ignored by anyone so bold as to use this document to evaluate the
conformance of this implementation to the Standard.

We believe that the Pascal-10 and Pascal-20 compilers and associated
runtime systems comply at level 1 with the ISO Pascal standard, ISO/DP
7185, as given in the second draft proposal of January, 1981, hereinafter
refered to as "the Standard".  However it is possible that we have
missed provisions of the Standard, or that for other reasons some
specific provision is not fully implemented. In particular, we have not
yet tried the Pascal Validation Suite. We would be greatful to have any
such deficiencies pointed out to us.  We would be particularly greatful
to anyone who is willing to try out the Validation Suite and give us the
results.  We will do our best to correct any problems, within the time
we have available to spend on Pascal maintenance. However we wish to
point out that this is not a commerical product, and we must explicitly
disavow any legal obligation to support it, or any responsbility for
damage, direct or indirect, to anyone using it, whether such damage
occurs because of its lack of compliance to the Standard, or for any
other reason.

The Standard requires that every implementation must be accompanied by a
document that does the following:
@begin(itemize)
Defines all features whose details are left up to the particular
implementation.  (These are refered to as "implementation-defined" in
the Standard.)

Lists all errors that are not reported by the compiler or runtime
systems.

Lists all extensions whose use cannot be detected by the compiler
or runtime systems.
@end(itemize)

In addition, this document will include a list of limits on complexity
or size of program that this compiler will process.  The existence of
such limits is allowed by section 1.2.a of the Standard.  Although the
Standard does not specifically require that they be documented, it seem
to be within the spirit of the Standard to do so.

@section(Implementation-defined items)

The real-type consists of the values -1.7014118E+38 to -1.4693679E-39,
0.0, and 1.4693679E-39 to 1.7014118E+38. Within the two ranges, values
are stored with a precision of 27 binary digits, which represents
slightly over 8 decimal digits.  Printout is rounded to 7 significant
figures.

The char-type consist of all 128 ASCII characters, including control
characters and null.  The order and value of these characters is that
defined by ASCII.  Note however that the following characters may not
appear within string or character constants in a source program, nor may
they be read by a Pascal program from a text file unless special
provisions are made: ^@@ [NUL], ^J [LF], ^L [FF], ^M [CR], ^N [RS], ^Z
[SUB], ^[ [ESC]. However these characters may occur as values of char
variables and may be manipulated by Pascal programs.

The Standard says that the actions of the procedures get and put on
any external entities are implementation-defined.  The Reference Manual
describes these actions in detail.  However in general terms, it can
be said that when used in accordance with the Standard, get and put
always cause effects on an external (physical) file.  In general,
get reads a byte from a file and put writes a byte to a file.  This
reading and writing may be buffered, so that no physical action
occurs until a buffer is full (for writing) or empty (for reading).
In most cases this buffering will not matter to the user.  However
for the terminal it is visible.  When characters are read from the
terminal, the buffer contains a line of text.  This means that the
when eoln is true, the next get will read an entire line of text
from the terminal into the buffer.  [There is an exception for the
case of lines so long that they will not fit in the buffer.  In
this case more than one physical read operation may be needed.]
No buffering is done for terminal output, so characters written
to the terminal by put show up immediately.

MAXINT is defined as 34,359,738,367.

If you do not specify a fieldwidth in a write or writeln, the
following default values are used:
@begin(display)
	INTEGER		12
	BOOLEAN		 6
	REAL		16
@end(display)

The number of digits written in an exponent is 2.

An upper-case E is used to indicate the exponent.

The values printed for Boolean variables are in lower-case: true and
false.

The procedure page writes a form-feed character into the output file.

The only program parameters that are legal in this implementation
are file variables.  They are bound to physical (operating system)
files when the program starts.  The runtime system writes the
name of each file variable, and asks the user for the file name
to use for it.   Any file variable that is not mentioned in the program
statement is bound to a temporary physical file, assuming that
only language features mentioned in the Standard are used.  This file is
deleted when the block in whch the file variable occurs is exited. 

Reset and rewrite may be applied to the files input and output in
this implementation.  Reset and rewrite have the same effect when
applied to input and output as when applied to any other file.
If input or output is not mentioned in the program statement, then
it is bound to a temporary file, as described in the previous paragraph.

@section(Errors not reported by the compiler or runtimes)

At any place where the rule of assignment-compatibility, it shall be an
error if types T1 and T2 are compatible ordinal-types and the value of
type T2 is not in the closed interval specified by the type T1. [In
English:  when you assign to a variable or field that has a subrange
type, it is illegal for the new value to be outside the bounds of the
subrange.] This implementation does not detect such errors
except in an assignment statements and in parameters passed by value.

At any place where the rule of assignment-compatibility, it shall be an
error if types T1 and T2 are compatible set-types and any number of the
value of type T2 is not in the closed interval specified by the
base-type of the type T1.  [In English:  when you assign to a variable
or field that has a set type, and that set involves a subrange, it is
illegal for the new set to have items that are outside the bounds of the
subrange.]  This implementation does not detect such errors.

It shall be an error to remove from its pointer-type the
identifying-value of an identified variable when a reference to the
identified variable exists.  [In English: it is illegal to dispose
of a record when there are still pointers to it somewhere.]  This
implementation does not detect violations of this rule.

It shall be an error to alter the value of a file-variable f when
a reference to the buffer-variable F^ exists.  [This means that
it is illegal to advance or reposition a file by GET, PUT, or any other
procedure, while the buffer variable is in use.  For example, it you
pass F^ to a procedure by reference, it is illegal for that procedure
to advance the file F.  It would also be illegal to advance a file
as a side-effect of any assignment statement having F^ as its left hand
side.]  This implementation does not detect such errors.

It shall be an error if a function is undefined upon completion of
the algorithm of an activation of the function-block.  [In English:
every function must assign a value to be returned by the function,
unless the program exits from the function by a GOTO.]  This
implementation does not detect such errors.

The actual-parameters corresponding to formal parameters that occur in a
single conformant-array-parameter-specification must all possess the
same upper and lower bounds.  [This is itself the result of an
extension.]  This implementation does not detect violations of this
rule.

When passing an array by means of a conformant array schema,
it shall be an error if the smallest or largest value specified by
index-type of the actual parameter lies outside the closed
interval specified by the formal parameter.  This implementation
does not detect such errors.

When doing put(f), it is an error if f^ is undefined.  This
implementation does not detect such errors.

Refering to the procedure new, when given more than one argument:
It shall be an error if a variant of a variant-part within the new
variable becomes active and a different variant of the variant-part
is one of the specified variants.  [This means that once you have
use new to generate a specific variant, it is an error to refer to
fields in a different variant or to change the tag-field to a
different variant.]   This implementation does not detect such errors.

Refering to the procedure new, when given more than one argument:
It shall be an error if a variable created using this form of new
is accessed by the identified-variable of the variable-access of a
factor, or an assignment-statement, or of an actual-parameter.
[That is, if you have used new to generate a specific variant, it
is an error to do operations that require the full record to exist.
For example, if you do an assignment to a record variable, the
system has to copy the record described on the right hand side into
the variable on the left-hand side.  How many words should it copy?
It copies the number of words needed by the largest variant.  If
you have used new to create a smaller variant, this means that
the assignment statement may be copying beyond the end of the record.]
This implementation does not detect such errors.

It shall be an error to do dispose(q) where q is undefined.  This
implementation does not normally detect such errors.  However if you
specify /ZERO to the compiler or put the directive {$Z+} into your
program, uninitialized pointer variables will appear to be nil.
The system will detect dispose of nil.

In the use of the function chr(x), it shall be an error if no character
value exists with the ordinal value equal to x.  This implementation
does not detect such errors.

It is an error if an expression involves a variable that is undefined.
This implementation does not detect such errors, except that if you
specify /ZERO to the compiler or put the directive {$Z+} into your
program, it will detect initialized pointer variables.

It is an error if the field-width in a write or writeln statement is
less than 1.  This implementation will not detect such errors.

@section(Extensions not reported by the compiler or runtimes)

The non-trivial extensions are described in more detail in the Pascal-10
and Pascal-20 Reference Manuals.  However a number of these extensions
consist of minor relaxations of type rules.  They are present only for
historical reasons, and will be considered by most users to have been
ill-advised.  We strongly recommend that users not make use of these
extensions unless there is a good reason to do so.

The symbol "_" is regarded by this compiler as a letter.

The symbols ":" and ".." may be used interchangably.  That is, anywhere
that the Standard requires ":", you may use ".." and visa versa.

The directive external is used to specify procedures and functions
whose procedure-block or function-block is external to the program-block.


Numerical constants may be supplies in octal or hexadecimal
radix.  An octal number is indicated by the letter B (upper
or lower case) as a suffix.  A hexadecimal number is indicated
by " as a prefix.  For example, the following three numbers
are the same: 63, 77B, "3F.

Labels are permitted to have any non-negative integer value.  (The
Standard requires them to be in the closed interval 0 to 9999.)

Comments may be written with the pairs of symbols { }, /* */, or (* *).
These symbols are not equivalent.  That is, any comment opened with
/* must be closed with */.  For this reason this is a true extension,
and not just an implementation of alternate symbols for { }.  (The
Standard allows only { and }, or other symbols that are equivalent to them.)

This implementation allows you to include declarations from a separate
file, with the include statement.  The syntax is "include" followed
by a list of file names in quotes.  See the Reference Manual for details.

Any array of char (or of a type compatible with char) is regarded as a
string.  The Standard treats only packed arrays whose lower index bound
is 1 as strings.  This means that any array of char can be written
using write or writeln, and compared by the lexical order relational
operators.

There is only a single representation for sets and files.  This
compiler makes no distinction between packed and unpacked sets and
files.  If two sets or two files would otherwise be compatible, but
one is packed and the other unpacked, this compiler considers them
to be compatible.  (The Standard considers packed sets and files to
be separate, and incompatible types.)

Any file of char, or of a subrange of char, is considered to be
a textfile.  Thus the special features of read, readln, write, and
writeln, and the function eoln, are applicable to all files of
char.  (The Standard allows text operations only with files that
have been declared using the Standard type "text", or some other
type defined equivalent to it.)

Type compability checking is somewhat looser than that specified
by the Standard.  This allows assignments and other operations
that would not be allowed by the Standard.  Types T1 and T2 shall
be designated compatible if any of the statements that follow is
true.  Statements that are not present in the Standard are prefixed
by ***.

@begin(itemize)
T1 and T2 are the same type.

T1 is a subrange of T2 or T2 is a subrange of T1, or both T1 and T2
are subranges of the same host type.

T1 and T2 are set-types of compatible base-types, and either
both T1 and T2 are designated packed or neither T1 nor T2 is
designated packed.  *** However in this implementation the second
clause, regarding the matching of the packed attribute, does not
apply.

T1 and T2 are string-types with the same number of components.
*** Note that this rule is subsumed by the following one.

***T1 and T2 are arrays whose index types and host types are
compatible, and with identical lower and upper bounds, and
either both T1 and T2 are designated packed or neither T1 nor
T2 is designated packed.

***T1 and T2 are files whose host types are compatible.
@end(itemize)

A value of type T2 shall be designated assignment-compatible
with a type T1 if any of the statements that follow is true.
Statements that are not present in the Standard are prefixed
by ***.

@begin(itemize)
T1 and T2 are the same type which is neither a file-type nor
a structured-type with a file component (this rule is to be
interpreted recursively).  *** However in this implementation
the requirement that the type need not be a file-type nor
contain a file component does not apply.  [Note however that
assignment of files and file components is extremely dangerous
and is unlikely to do anything unless you know a lot about the
implementation.]

T1 is the real-type and T2 is the integer-type.

T1 and T2 are compatible ordinal-types and the value of type T2
is in the closed interval specified by the type T1.

T1 and T2 are compatible set-types and all the members of the value of
type T2 are in the closed interval specified by the base-type of
T1.

T1 and T2 are compatible string-types.
*** Note that this rule is subsumed by the following one.

***T1 and T2 are compatible arrays.

***T1 and T2 are compatible files.
@end(itemize)

In a variant record, the Standard requires that only one variant can be
active at a time, and that if there is a tag-field, the value of the
tag-field must specify the currently active variant. This implementation
does not have such a restriction.  Since the fields in different variants
overlap in memory, it is possible in this implementation to treat the
same bit-pattern in memory as several different fields, possibly of
different types.

There is an extra declaration type, the initprocedure.  It looks much
like a procedure, but can contain only certain kinds of assignment
statements.  It is used to initialize global variables.  See the
Reference Manual for details.

In this implementation it is not necessary for a function to include
an assignment statement that assigns a value for it to return.  Note
however that the only case where it would be valid to omit such a
statement is if the program always exits from the function by a GOTO.

According to the Standard, an actual variable parameter shall not denote
a field which is the selector of a variant part, nor a component of
a variable that possesses a type that is designated packed.  [In
English: It is illegal to pass by reference any member of a packed
structure or the tag field of a record.] In this implementation, most
of this limitation does not apply.  However it is illegal to pass
by reference any component that does not occupy a full word or more
than one full word.  Normally this means fields of packed records
and components of packed arrays.  However if the field or component
is large enough it is possible that no packing will actually have
taken place, and it will be legal to pass it.  This error will be
detected by the compiler.

The Standard specifies a set of rules to define when two  formal
parameter lists are congruous.  [This defines when you can pass a
procedure as an argument to another procedure.]  In addition, this
implementation considers two formal parameter lists to be congruous when
either of them is empty.  This is done for compatibility with the
syntax for parametric procedure defined by Jensen and Wirth.  In
Jensen and Wirth, no declaration is made of the parameters for a 
formal procedure.  When you call a formal procedure and its 
parameters are not declared, you can pass any actual parameters at
all.  The compiler will assume that the procedure being called
has the same number of formal parameters, that they are called by
value, and that they are compatible with the actual parameters you
have supplied. Because of the fact that the compiler does not
know what the arguments are supposed to be, it is unable to
do conversions.  For example, if you pass an integer to
a parametric procedure that is expecting a real, the normal conversion
will not happen.  Similarly, if you pass [] to a procedure that
is expecting a set, it will be treated as the null set of characters.
If the procedure is expecting a null set of some other type, the
results will be wrong.  This form of call is only acceptable if the actual
parameters fit in the registers.  At most 6 parameters will fit in
the registers, but certain kinds of parameters will take up more space
than usual, and thus reduce the number allowed.  The compiler will
detect such cases and issue an error message.  [The term procedure
as used in this paragraph should be construed to apply to procedures
or functions.]

According to the Standard, the actual-parameters corresponding to
formal parameters that occur in a single
conformant-array-parameter-specification shall all possess the same
type.  This implementation only requires that each actual parameter
individually must be compatible with the conformant specification.
It shall be an error if all such actual parameters do not have
the same lower and upper bounds.  To give an example, consider a
procedure with two parameters declared  A,B: ARRAY[I..J:INTEGER] OF REAL.
The standard would require two actual parameters with the same type.
This implementation only requires that the two actual parameters
both be arrays of real, and both have the same lower bound, which
is an integer and which gets assigned to I, and the same upper bound,
which is an integer and which gets set to J.  Note however that
this implementation does not currently detect the error wherein
the two arrays have different lower or upper bounds.

There are two special ways to pass parameters to procedures and
functions in this implementation.  They are described by special
keywords that function syntactically like type-identifiers:
string and pointer.  They can be used only with the
directive "extern".  String matches a string of any length.
It is now outmoded, because of the existence of conformant arrays.
Pointer matches a pointer of any type.

There is a special "type" denoted by the keyword file.  It can be
used in a formal parameter list.  It allows the corresponding
actual parameter to be a file of any type.

Get and put may have extra parameters.  They have the same syntax as
in the new and dispose.  They control reading and writing of
variable-length records.  See the Reference Manual for details.

Reset and rewrite may have up to 6 parameters in Tops-20 Pascal.
See the Reference Manual for the meanings of the additional ones.

In write and writeln, integers can be written in octal or hexadecimal.
This involves use of an extra colon and the letter O or H after the
field width.  See the Reference Manual for details.

Read and readln can be applied to arrays of char.  See the Reference
Manual for details.

There are a number of additional predefined procedures and functions.
Functions: ARCSIN, ARCCOS, SINH, COSH, TANH, SIND, COSD, LOG,
CURPOS, DATE, NEXTFILE, RANDOM, RECSIZE, RUNTIME, TIME.  Procedures:
APPEND, BREAK, BREAKIN, CLOSE, DELETE, DISMISS, GETINDEX, GETLINENR,
JSYS, MARK, NEWL, PUTX, RCLOSE, RELEASE, RENAME, SETPOS, STRSET, STRWRITE,
UPDATE.  See the Reference Manual for details.

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

A set of rules is given in the Standard specifying what labels
it is legal to go to with goto.  In this compiler, it is legal
to goto any label in the current block or any containing block.
Doing a goto into a loop, case statement, etc., can be dangerous
with many compilers, but the code generated by this compiler is
so simple that such things will produce the expected results.
The one questionable case is doing a goto into the middle of
a for loop.  Even there, if the value of the control variable
is defined, it will work.

According to the Standard, it is an error if a case statement specifies
an index that does not match one of the cases.  In this
implementation such a situation is legal.  If no case matches the
value of the case index, a null case is used.  This case consists
of a null statement.

The case statement may be extended with the case "others" which
then appears as the last case in the case statement.  This case
will be executed if the expression of the case statement does
not evaluate to one of the case labels.

According to the Standard, the controlling variable of a for loop
must be a simple variable, and it must be declared in the block
in which the for loop appears.  In this implementation the variable
may be a local from an outer block.  However it still must be a
simple scalar variable, i.e. not part of any structure and not
a formal parameter.

There is an additional kind of repetitive statement, which allows an
exit in the middle.  Its syntax is
@begin(display)
   loop-statement> ::= "loop" statement {; statement}
		       "exit if" Boolean-expression ";"
			statement {; statement} "end"
@end(display)

The syntax of the program statement has been extended to allow
you to declare some extra attributes of the file.  These
allow the initial file name dialog to be made slightly
more intelligent than otherwise.  The attributes are specified
by using a colon after the file name in the list.  See the
Reference Manual for details.

The syntax of the program statement has been modified to allow
for files that do not contain main programs.  In such a case
the syntax is "program" program-name {,entry-name}.  Entry-name
is the name of all procedures that are to be defined as
entrypoints, if this is made into a library.  A file without
a main program should be indicated by the directive {$M-}, which
must occur before the program statement.  Such a file should
not have a main body.  After the last procedure, there should be
a period.

The program statement is optional in this implementation.  

In addition to the standard files input and output the standard
file tty is available in Tops-20 Pascal.  This file is used to
communicate with the terminal.  Tty need not be mentioned in the
program statement.  Indeed doing so will have no effect.  Tty can
be used for input or output.  To implement this, there is a second
predeclared file, ttyoutput.  Output references to tty are automatically
turned into references to ttyoutput.  See the Reference Manual for
more information on tty.


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

@section(Limits on size and complexity)

@subsection(Limits that may not be changed)

The type INTEGER and types derived from it are limited to the range
-34,359,738,368 to 34,359,738,367 (- 2**35 to 2**35 - 1).

The size of the program and data are limited by the address space of
the PDP-10 computer.  Except when extended addressing is used,
the program and data together must not take up more than 262144 words.
Normally half of this space is allocated to the program code plus the stack,
and the other half to the global variables, I/O buffers, and heap.
However the directive {$P:nnnn} may be used to change this distribution,
as indicated in the Reference Manual.  When extended addressing is used,
the program code plus "small" variables is still limited to 262144 words.
However the stack, heap, and large objects are put in a separate space
that has a capacity of 7,864,320 words.  Details on distribution of
memory is given in the Reference Manual.


Sets may only be defined on types or subranges of types having
72 or fewer members.  With subranges of integers the set may only
include 0 to 71.  With enumerated types it may include only
the first 72 members of the enumeration.  Special provisions are
made to allow sets of CHAR and subranges of CHAR.


@subsection(Limits that may be changed)

The following limits are determined by constants in the program.
They may be relaxed by increasing these constants and rebuilding
the compiler.

Identifiers are limited to a maximum size of 200 characters.

A maximum of 20 labels is permitted in any one procedure.
(This is an assembly parameter.  The whole restriction may
be removed easily, at the cost of some extra local fixups in
the .REL file.)  This includes both labels declared in the
LABEL section and those not so declared.  (Labels need be
declared only if they will be the object of a nonlocal goto.)

Procedure declarations may be nested to a maximum depth of 8.  (That is,
the main program, plus 7 procedures.)

Name contexts may be nested to a maximum depth of 20.  A name context
is required every time you enter a new nested procedure declaration,
a record declaration, a with statement, or a dot.

Strings constants are limited to 120 characters.

@section(Other warnings)

The contents of unused bits in packed arrays and records is
undefined.  This should not cause trouble, except in programs
the play fast and loose with variant records, or programs that
pass arrays of type PACKED ARRAY OF CHAR to Fortran programs.
Many Fortran programmers will use integer comparisons on character
data, thus requiring the low order bit in the word to be zero.
The code compiled in Pascal to compare PACKED ARRAY OF CHAR
variables ignores the low order bit, so this does not cause a
problem in Pascal.  If you require unused fields to be zero in all
cases, you can set /ZERO or (*$Z+*).

All of the entry points in the runtime library are effectively
reserved external names.  That is, if you use one of these names
either as your program name (in the PROGRAM statement) or as the
name of a procedure in a file of external procedures, disaster
may result.  Any name whose first six characters is the same as
one of these names will cause the problem.  You can sometimes
get away with violating this rule if your program does not use
any of the features of Pascal which cause the particular runtime
routine involved to be invoked.  As of the time when this document
was prepared, the following were the entry point names.  For an
up to date list, use the command "TTY:=PASLIB/POINTS" to MAKLIB.
