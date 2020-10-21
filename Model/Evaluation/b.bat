@echo off
setlocal

if "%2" == "d" set o=/traceback /fpe:0 /Od /Qinit:snan

mpiifort 6segSprint_eval.f ..\span.f ..\subs.f %o%