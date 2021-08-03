

Example command lines
=====================

- parses dogtail.f and writes dogtail.f.json and dogtail.f.2.json (with more optimized structure)
	fortran-parser.exe dogtail.f

- name of subroutine from IO sub-program block
	fortran-parser.exe --noparse dogtail.f $.Program.IO[0].SubroutineStatement[0].Name
  (should print IO for digtail.f)

- List all COMMON blocks in sub IO...
	fortran-parser.exe --noparse dogtail.f $.Program.IO..CommonStatement.Name
  or
    fortran-parser.exe --noparse dogtail.f $.Program.IO[*].CommonStatement.Name

- List items in specified common block...
    fortran-parser.exe --noparse dogtail.f $.Program.IO[?(@.CommonStatement.Name=='CONSTNTS')].CommonStatement.Items[*]
  ...seems only work by filter includign commonstatemet rather than selecting commonstatement first and then filter again name

 - LIst variable on lhs of assignments
	fortran-parser.exe --noparse dogtail.f $.Program.IO[?(@.AssignmentStatement)].AssignmentStatement.lhs.VarRef -jpf0
	...note "-jpf0" to force single line output of each result token (otherwise vars with array get split onto multiple lines)

 - LIst variable on either side of assignments
	fortran-parser.exe --noparse dogtail.f $.Program.IO[?(@.AssignmentStatement)].AssignmentStatement..VarRef -jpf0


Docker help
===========

A build is available on docker hub at rpobinrottier/fortranparser

Run this image with a command like:
docker run -v "%cd%":/app/f robinrottier/fortranparser /app/f/xxx.f -n $.Program.IO

..this makes current directory in your (windows) host avilable as mount in the app directory for the container.

