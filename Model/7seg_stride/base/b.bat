@echo off

ftn95 7segsprint.f /debug
ftn95 ..\subs.f /debug
slink
@REM lo 7segsprint
@REM lo ..\subs
@REM file