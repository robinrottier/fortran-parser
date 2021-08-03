

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

A build is available on docker hub at https://hub.docker.com/repository/docker/robinrottier/fortranparser

Run this image with a command like:
docker run -v "%cd%":/app/f robinrottier/fortranparser /app/f/xxx.f -n $.Program.IO

..this makes current directory in your (windows) host avilable as mount in the app directory for the container.

JULIA Help
==========
Load this file using JSON
```julia
	import JSON
	f = JSON.parsefile(<filename>)
```
THen extract assignments in EQNS1 sub...
```julia
	a=filter(x->haskey(x,"AssignmentStatement"), f["Program"]["EQNS1"])
```
And get rid of the "assingment" outer collection
```julia
	a2 = map(x->x["AssignmentStatement"], a)
```
THen extract lhs variables...
```julia
	a3 = map(x->x["lhs"]["VarRef"], a2)
```
where a simple variable prints "x", arrays print like Any["x", 1]

Find the KE assignent statement in IO function and get the rhs expression...
```julia
	filter(x->haskey(x,"AssignmentStatement") && x["AssignmentStatement"]["lhs"]["VarRef"] == "KE", f["Program"]["IO"])[1]["AssignmentStatement"]["rhs"]
```

