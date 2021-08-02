

Example command lines
=====================

fortran-parser.exe dogtail.f
- parses dogtail.f and writes dogtail.f.json and dogtail.f.2.json (with more optimized structure)

fortran-parser.exe --noparse dogtail.f $.Program[2].SubroutineSubprogram[0].SubroutineStatement[0].Name
- jsonpath on output to print name of subroutine from 2nd subprogram block
  (should print IO for digtail.f)

fortran-parser.exe --noparse dogtail.f $.Program[2].SubroutineSubprogram[3].CommonStatement.CONSTNTS
- print value for COMMON CONSTNTS block (which is 3rd statement in 2nd subprog block)

fortran-parser.exe --noparse dogtail.f $.Program[2].SubroutineSubprogram[3].CommonStatement.CONSTNTS[*]
- same print value for COMMON CONSTNTS block but now as sep strings instead of array syntax

fortran-parser.exe --noparse dogtail.f $.Program[2].SubroutineSubprogram[?(@.CommonStatement.CONSTNTS)].CommonStatement.CONSTNTS[*]
- same but now with a filter so didnt have to know line index for common block (BUT a bit more repitive)

