This file is for version 4.  If you are running version 3 or 3A, look
at EXEC3.PAT.

This file describes how to modify your EXEC to link to PASCAL.  We 
assume that you do not have EXEC sources.  If you do, you will just
add the standard entry to the macros LANGUAGE and NAMES, and look at
the two patches given in EXEC4.DDT (to implement PASDDT, and make the
EXEC pass /RUN:LINK instead of LINK!)

This compiler is designed to be called by the version 4 EXEC.  This
EXEC assumes it is dealing with Tops-10 compilers, so we use the
official Tops-10 top level (SCAN).  The EXEC patches needed to make
this work are given in EXEC4.DDT.  

Before your apply the patches, you must decide what existing language 
PASCAL is to replace.  You should choose some lanugage supported by the DEC 
EXEC that you do not intend to use, or at least which you are willing to call
explicitly. It should also be a language which does not have special 
support in the EXEC which would have to be changed to fit PASCAL.  In the 
current EXEC the best choices appear to be SAIL, FAIL, ALGOL, and 
SNOBOL.  When you decide what language to replace, you should then
find its language index.  This is its offset in the various EXEC
tables.  The offsets in our version are:
  SAIL - 2
  FAIL - 3
  SNOBOL - 4
  ALGOL - 6
The offset can be verified by looking at LTAB plus offset, and
displaying the contents in ASCII.  This should be the extension
for the language.  E.g. for SNOBOL:
  get exec
  ddt
  ltab+4/ JUNK $7t; SNO
EXEC4.DDT was prepared using a language index of 4, i.e. replacing SNOBOL.
If you decide to use some other language, you should edit EXEC4.DDT and
change 4 to the index you want used.

EXEC4.DDT was designed to be used with the $Y command of FILDDT to
automatically insert the patches into your EXEC. Unfortunately the
current released version of FILDDT does not support the $Y command so
you will have to insert the patches shown there by hand.

The patching done by this will make PASCAL a known language, and any source
file with the extension .PAS will be handled properly.  However there should
be a switch /PASCAL that forces a file to be treated as PASCAL.  
Unfortunately we cannot simply replace the /SNOBOL switch with /PASCAL,
since the applicable table must be in alphabetical order.  Before listing
your choices, let me explain how to change the /SNOBOL switch to /PASCAL.
Although it will not work directly, the other choices are slight variations
on this change.  The table SWTAB contains a list of all the switches.
The first entry is length,,length.  The offsets are not the same as the
language index, since it is in alphabetical order.  The offset for
SNOBOL happens to be 35 in our version.  It is not hard to find the others.
The left half of each entry is the address of the switch name.  So to
examine SNOBOL, we do
  swtab+35/  JUNK = 57636,,57640
  57636/ JUNK $t; SNOBO
  57637/ L
To change it to PASCAL you would do
  57636/  JUNK "/PASCAL/
(If you do this with FAIL or SAIL, beware that FAIL and SAIL only take up
one word, so you will have to use PASC, or assign a different word from
the patch area pat..)  Anyway, your choices are as follows:
  - ignore the problem.  It is rare to have to use a switch to force
	the language choice, so you can probably survive the limitation
	that the extension must be .PAS.  If necessary, /SNOBOL can be
	used to force PASCAL!!
  - use a name that fits into the table in the same place as the existing
	entry alphabetically.  E.g. if you replace SNOBOL, you might call
	the switch /SPASCAL.  (This is the official name of the compiler
	in DECUS, for "System programmer's PASCAL".)
  - do it right.  This would involve changing the switch name to PASCAL
	(using the patch area if the compiler you are replacing has a
	name of 4 characters or less) and then reordering the table to
	keep it in alphabetical order.

We suggest that you yell long and loudly to DEC about their policy of
not distributing source, or at least .REL files.  (We could supply
clean patches for .REL files, using MAKLIB.)  Alternatively, they should
leave some extra room in their tables and provide a mechanism for
adding langauges and/or commands.  E.g.
  $^E ADD LANGUAGE 
  $$COMPILER SYS:PASCAL.EXE
  $$EXTENSION PAS
etc.  This is the least one can expect if they won't give you the sources
to make your own patches.

