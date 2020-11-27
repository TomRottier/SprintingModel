@echo off
setlocal

if "%1" == "d" set o=/traceback /fpe:0 /Od /Qinit:snan,arrays
if "%1" == ""  set o=/fast

mpiifort 7SegSprint_opt.f ..\span.f ..\..\subs.f %o%