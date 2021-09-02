@echo off
@setlocal

set f=-Dlanguage=CSharp -o csharp 
if not exist csharp md csharp

if "%1" == "" (
	echo Nohting to build!!
) else (
	if /i "%1" == "/a" (
		echo Building lexer...
		call antlr4 %f% Fortran77Lexer.g4
		echo Building parser...
		call antlr4 %f% Fortran77Parser.g4
	) else (
		echo Building %1...
		call antlr4 %f% %1
	)
)
