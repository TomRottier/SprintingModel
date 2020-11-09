# Execution speed test

Compare the AUTOZ feature from AUTOLEV which introduces intermediate variables to simplify expressions. Speed up persumably comes from reducing number of calls to SIN and COS functions as they are the most computationally intensive.

***

## Test 1

Same seven-segment model with same inputs; only difference is the intermediate variables. 100 function calls (single simulation which calculate RMSE in joint angles)

### Build 1

- Compiler options: /debug /traceback
- Profiled with VTune
- Results:
  - Total time:
    - ON: 33.464 s
    - OFF: 142.543 s
  - Active functions:
    - ON: EQNS1 (15.347 s), SOLVE (9.306 s), QDSPLN (3.822 s), QSPLIN (1.644 s)
    - OFF: EQNS1 (16.773 s), SOLVE (9.996 s), QDSPLN (3.190 s), QSPLIN (1.555 s)
  - COS/SIN (libm) call time:
    - ON: COS (0.892 s), SIN (0.747 s)
    - OFF: COS (65.914 s), SIN (36.380 s)

### Build 2

- Compiler options: /fast
- Timed with timecmd.bat
- Results:
  - Total time:
    - ON: 7.07 s
    - OFF: 9.78 s

***

## Test 2

Seven segment model with AUTOZ on/off vs six segment model with AUTOZ off. Seven segment model has one extra generalised coordinate/speed which calculates joint torques for another joint and forces for another contact point. 100 function calls (single simulation which calculate RMSE in joint angles). Different models so won't be exact comparison.

- Compiler options: /fast
- Timed with timecmd.bat
- Results:
  - Total time:
    - Six: 39.60 s
    - Seven OFF: 14.96 s (??)
    - Seven ON: 11.71 s
