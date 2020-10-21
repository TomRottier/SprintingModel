@echo off

if "%1" == "e" mpiexec -np %2 6segSprint_eval.exe
