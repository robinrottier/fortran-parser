@setlocal

set f=-Dlanguage=CSharp -o csharp 
if not exist csharp md csharp

echo Building lexer...
call antlr4 %f% Fortran77Lexer.g4
echo Building parser...
call antlr4 %f% Fortran77Parser.g4
