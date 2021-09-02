
How to run
==========
Each tagged release on GITHUB includs a zipped file of the build. Download, unzip and copy to its own folder. The build is a cross platform .NET application, so the .NET runtime needs to be installed first. If you installed the .NET SDK then already have the runtime. To install just the runtime go to the microsoft page at https://dotnet.microsoft.com/download/dotnet/5.0/runtime and pick your platform of choice. The runtime only is not too large.

If .NET (runtime only or full sdk) is installed then the command `dotnet` should "just work" and print some help about missing arguments.

Once installed and fortran-parser zip is downloaded and unzipped then run with a command like:

dotnet fortran-parser.dll

..if in same directory you unzipped to; or simply with the full path if not

dotnet ...somepath\fortran-parser.dll
	

Install dotnet runtime on WSL
-----------------------------
Following instructions on

First add MS packages to the list...
```
wget https://packages.microsoft.com/config/ubuntu/18.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
sudo dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb
```
Then install dotnet runtime...
```
sudo apt-get update;   sudo apt-get install -y apt-transport-https &&   sudo apt-get update &&   sudo apt-get install -y dotnet-runtime-5.0
```
To build this project on WSL you need the full sdk:
```
sudo apt-get install -y dotnet-sdk-5.0
```

Example command lines
=====================

- parses dogtail.f and writes dogtail.f.json and dogtail.f.2.json (with more optimized structure)
```
	dotnet fortran-parser.dll dogtail.f
```
- name of subroutine from IO sub-program block
```
	dotnet fortran-parser.dll --noparse dogtail.f $.Program.IO[0].SubroutineStatement[0].Name
```
  (should print IO for digtail.f)

- List all COMMON blocks in sub IO...
```
	dotnet fortran-parser.dll --noparse dogtail.f $.Program.IO..CommonStatement.Name
```
  or
```
   	 dotnet fortran-parser.dll --noparse dogtail.f $.Program.IO[*].CommonStatement.Name
```

- List items in specified common block...
```
    dotnet fortran-parser.dll --noparse dogtail.f $.Program.IO[?(@.CommonStatement.Name=='CONSTNTS')].CommonStatement.Items[*]
```
  ...seems only work by filter includign commonstatemet rather than selecting commonstatement first and then filter again name

 - LIst variable on lhs of assignments
```
	dotnet fortran-parser.dll --noparse dogtail.f $.Program.IO[?(@.AssignmentStatement)].AssignmentStatement.lhs.VarRef -jpf0
```
	...note "-jpf0" to force single line output of each result token (otherwise vars with array get split onto multiple lines)

 - LIst variable on either side of assignments
```
	dotnet fortran-parser.dll --noparse dogtail.f $.Program.IO[?(@.AssignmentStatement)].AssignmentStatement..VarRef -jpf0
```


Docker help
===========

A build is available on docker hub at https://hub.docker.com/repository/docker/robinrottier/fortranparser

Run this image with a command like:
```
docker run -v "%cd%":/app/f robinrottier/fortranparser /app/f/xxx.f -n $.Program.IO
```

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

