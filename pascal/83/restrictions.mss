@device(pagedfile)
@make(report)
@style(indentation 0,paperlength 60,topmargin .5inch,linewidth 7inches)
@modify(hdx,above 3,below 2,need 7)
@modify(hd0,above 3,below 2,need 7)
@modify(hd1,above 4,below 3,centered,pagebreak before)
@modify(hd2,above 3,below 2,need 7)
@modify(hd3,above 3,below 2,need 7)
@modify(hd4,above 3,below 2,need 7)
@modify(description,leftmargin 15,indent -10)
@pageheading()



@begin(center)
PASCAL-10 and PASCAL-20

ISO Conformance









@end(center)

The information in this document is subject to change without
notice and should not be construed as a commitment by Charles Hedrick
or Rutgers University.  Charles Hedrick and Rutgers University assume
no responsibility for any errors that may appear in this document.

Note:  The following are trademarks of the Digital Equipment
Corporation:  DECSYSTEM-20, DECsystem-10, Tops-20, Tops-10

@NewPage(0)@Set(Page=1)@Style(PageNumber <@1>)

This document is intended to describe the conformance of the Pascal
compilers supplied by Rutgers University to the ISO Pascal standard.
We have not yet tried the Validation Suite.  This document is the
result of careful reading of the standard, but is considerably less
authoritative than it would be if the Validation Suite had been run.

@include(restri.mss)
