const
{The following are for hardware only, independent of operating system.
 For a KI-10, both are false}
klcpu=true;    {True for KL-10 or KS-10 CPU, any operating system}
kacpu=false;   {True for KA-10 CPU, any operating system}
{The following are for operating system, independent of hardware}
tops10=false;  {True for Tops-10, false for Tenex or Tops-20}
novm=false;    {True for operating system without V.M., i.e.
		Tops-10 on a KA-10 or a KI-10 that didn't buy 
		VMSER.  False for Tops-10 with VM, and Tenex 
		or Tops-20.}
paslib='PASLIB    ';
pasdev='SYS       ';
pasproj=0b;
pasprog=0B;
cixmax=40000
.
