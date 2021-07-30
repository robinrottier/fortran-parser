@setlocal
set d0=%~dp0

set CLASSPATH=%d0%\antlr-4.9.2-complete.jar;
java org.antlr.v4.Tool %*
