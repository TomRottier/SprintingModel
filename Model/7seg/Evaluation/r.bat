@echo off

if "%1" == "e" mpiexec -np %2 7SegSprint_eval.exe
