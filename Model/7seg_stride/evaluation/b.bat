@echo off
setlocal

if "%1" == "d" set o=/traceback /fpe:0 /Qinit:snan /C /Z7
if "%1" == ""  set o=/O2

mpiifort 7SegSprint_eval.f span.f ..\subs.f %o%