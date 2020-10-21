C=========================================================================
C
C                    SPAN:
C  Simulated Parallel Annealing by Neighborhood
C
C                     by
C
C              Jill S. Higginson
C             jstartz@stanford.edu
C
C                     and
C
C             Frank C. Anderson
C              fca@stanford.edu
C
C SPAN was adapted from the simulated annealing algorithm
C authored by Bill Goffe (see Goffe Abstract below).
C Goffe's algorithm is essentially unaltered except for
C communication calls that distribute the workload among
C a specified number of processors.  Communications are built
C on the Message Passing Interface (MPI) and should compile
C under most all MPI implementations.
C
C=========================================================================
C   Goffe Abstract:
C   Simulated annealing is a global optimization method that distinguishes
C   between different local optima. Starting from an initial point, the
C   algorithm takes a step and the function is evaluated. When minimizing a
C   function, any downhill step is accepted and the process repeats from this
C   new point. An uphill step may be accepted. Thus, it can escape from local
C   optima. This uphill decision is made by the Metropolis criteria. As the
C   optimization process proceeds, the length of the steps decline and the
C   algorithm closes in on the global optimum. Since the algorithm makes very
C   few assumptions regarding the function to be optimized, it is quite
C   robust with respect to non-quadratic surfaces. The degree of robustness
C   can be adjusted by the user. In fact, simulated annealing can be used as
C   a local optimizer for difficult functions.
C
C   This implementation of simulated annealing was used in "Global Optimization
C   of Statistical Functions with Simulated Annealing," Goffe, Ferrier and
C   Rogers, Journal of Econometrics, vol. 60, no. 1/2, Jan./Feb. 1994, pp.
C   65-100. Briefly, we found it competitive, if not superior, to multiple
C   restarts of conventional optimization routines for difficult optimization
C   problems.
C
C   For more information on this routine, contact its author:
C   Bill Goffe, bgoffe@whale.st.usm.edu
C
C  This file is an example of the Corana et al. simulated annealing
C  algorithm for multimodal and robust optimization as implemented
C  and modified by Goffe, Ferrier and Rogers. Counting the above line
C  ABSTRACT as 1, the routine itself (SA), with its supplementary
C  routines, is on lines 232-990. A multimodal example from Judge et al.
C  (FCN) is on lines 150-231. The rest of this file (lines 1-149) is a
C  driver routine with values appropriate for the Judge example. Thus, this
C  example is ready to run.
C
C  To understand the algorithm, the documentation for SA on lines 236-
C  484 should be read along with the parts of the paper that describe
C  simulated annealing. Then the following lines will then aid the user
C  in becomming proficient with this implementation of simulated
C  annealing.
C
C  Learning to use SA:
C      Use the sample function from Judge with the following suggestions
C  to get a feel for how SA works. When you've done this, you should be
C  ready to use it on most any function with a fair amount of expertise.
C    1. Run the program as is to make sure it runs okay. Take a look at
C       the intermediate output and see how it optimizes as temperature
C       (T) falls. Notice how the optimal point is reached and how
C       falling T reduces VM.
C    2. Look through the documentation to SA so the following makes a
C       bit of sense. In line with the paper, it shouldn't be that hard
C       to figure out. The core of the algorithm is described on pp. 68-70
C       and on pp. 94-95. Also see Corana et al. pp. 264-9.
C    3. To see how it selects points and makes decisions about uphill
C       and downhill moves, set IPRINT = 3 (very detailed intermediate
C       output) and MAXEVL = 100 (only 100 function evaluations to limit
C       output).
C    4. To see the importance of different temperatures, try starting
C       with a very low one (say T = 10E-5). You'll see (i) it never
C       escapes from the local optima (in annealing terminology, it
C       quenches) & (ii) the step length (VM) will be quite small. This
C       is a key part of the algorithm: as temperature (T) falls, step
C       length does too. In a minor point here, note how VM is quickly
C       reset from its initial value. Thus, the input VM is not very
C       important. This is all the more reason to examine VM once the
C       algorithm is underway.
C    5. To see the effect of different parameters and their effect on
C       the speed of the algorithm, try RT = .95 & RT = .1. Notice the
C       vastly different speed for optimization. Also try NT = 20. Note
C       that this sample function is quite easy to optimize, so it will
C       tolerate big changes in these parameters. RT and NT are the
C       parameters one should adjust to modify the runtime of the
C       algorithm and its robustness.
C    6. Try constraining the algorithm with either LB or UB.
C
C
C=========================================================================
C
C  Version: 3.2
C  Date: 1/22/94.
C  Differences compared to Version 2.0:
C     1. If a trial is out of bounds, a point is randomly selected
C        from LB(i) to UB(i). Unlike in version 2.0, this trial is
C        evaluated and is counted in acceptances and rejections.
C        All corresponding documentation was changed as well.
C  Differences compared to Version 3.0:
C     1. If VM(i) > (UB(i) - LB(i)), VM is set to UB(i) - LB(i).
C        The idea is that if T is high relative to LB & UB, most
C        points will be accepted, causing VM to rise. But, in this
C        situation, VM has little meaning; particularly if VM is
C        larger than the acceptable region. Setting VM to this size
C        still allows all parts of the allowable region to be selected.
C  Differences compared to Version 3.1:
C     1. Test made to see if the initial temperature is positive.
C     2. WRITE statements prettied up.
C     3. References to paper updated.
C
C  Synopsis:
C  This routine implements the continuous simulated annealing global
C  optimization algorithm described in Corana et al.'s article
C  "Minimizing Multimodal Functions of Continuous Variables with the
C  "Simulated Annealing" Algorithm" in the September 1987 (vol. 13,
C  no. 3, pp. 262-280) issue of the ACM Transactions on Mathematical
C  Software.
C
C  A very quick (perhaps too quick) overview of SA:
C     SA tries to find the global optimum of an N dimensional function.
C  It moves both up and downhill and as the optimization process
C  proceeds, it focuses on the most promising area.
C     To start, it randomly chooses a trial point within the step length
C  VM (a vector of length N) of the user selected starting point. The
C  function is evaluated at this trial point and its value is compared
C  to its value at the initial point.
C     In a maximization problem, all uphill moves are accepted and the
C  algorithm continues from that trial point. Downhill moves may be
C  accepted; the decision is made by the Metropolis criteria. It uses T
C  (temperature) and the size of the downhill move in a probabilistic
C  manner. The smaller T and the size of the downhill move are, the more
C  likely that move will be accepted. If the trial is accepted, the
C  algorithm moves on from that point. If it is rejected, another point
C  is chosen instead for a trial evaluation.
C     Each element of VM periodically adjusted so that half of all
C  function evaluations in that direction are accepted.
C     A fall in T is imposed upon the system with the RT variable by
C  T(i+1) = RT*T(i) where i is the ith iteration. Thus, as T declines,
C  downhill moves are less likely to be accepted and the percentage of
C  rejections rise. Given the scheme for the selection for VM, VM falls.
C  Thus, as T declines, VM falls and SA focuses upon the most promising
C  area for optimization.
C
C  The importance of the parameter T:
C     The parameter T is crucial in using SA successfully. It influences
C  VM, the step length over which the algorithm searches for optima. For
C  a small intial T, the step length may be too small; thus not enough
C  of the function might be evaluated to find the global optima. The user
C  should carefully examine VM in the intermediate output (set IPRINT =
C  1) to make sure that VM is appropriate. The relationship between the
C  initial temperature and the resulting step length is function
C  dependent.
C     To determine the starting temperature that is consistent with
C  optimizing a function, it is worthwhile to run a trial run first. Set
C  RT = 1.5 and T = 1.0. With RT > 1.0, the temperature increases and VM
C  rises as well. Then select the T that produces a large enough VM.
C
C  For modifications to the algorithm and many details on its use,
C  (particularly for econometric applications) see Goffe, Ferrier
C  and Rogers, "Global Optimization of Statistical Functions with
C  Simulated Annealing," Journal of Econometrics, vol. 60, no. 1/2,
C  Jan./Feb. 1994, pp. 65-100.
C  For more information, contact
C              Bill Goffe
C              Department of Economics and International Business
C              University of Southern Mississippi
C              Hattiesburg, MS  39506-5072
C              (601) 266-4484 (office)
C              (601) 266-4920 (fax)
C              bgoffe@whale.st.usm.edu (Internet)
C
C  As far as possible, the parameters here have the same name as in
C  the description of the algorithm on pp. 266-8 of Corana et al.
C
C  In this description, SP is single precision, DP is double precision,
C  INT is integer, L is logical and (N) denotes an array of length n.
c  Thus, DP(N) denotes a double precision array of length n.
C
C  Input Parameters:
C    Note: The suggested values generally come from Corana et al. To
C          drastically reduce runtime, see Goffe et al., pp. 90-1 for
C          suggestions on choosing the appropriate RT and NT.
C    N - Number of variables in the function to be optimized. (INT)
C    X - The starting values for the variables of the function to be
C        optimized. (DP(N))
C    MAX - Denotes whether the function should be maximized or
C          minimized. A true value denotes maximization while a false
C          value denotes minimization. Intermediate output (see IPRINT)
C          takes this into account. (L)
C    RT - The temperature reduction factor. The value suggested by
C         Corana et al. is .85. See Goffe et al. for more advice. (DP)
C    EPS - Error tolerance for termination. If the final function
C          values from the last neps temperatures differ from the
C          corresponding value at the current temperature by less than
C          EPS and the final function value at the current temperature
C          differs from the current optimal function value by less than
C          EPS, execution terminates and IER = 0 is returned. (EP)
C    NS - Number of cycles. After NS*N function evaluations, each
C         element of VM is adjusted so that approximately half of
C         all function evaluations are accepted. The suggested value
C         is 20. (INT)
C    NT - Number of iterations before temperature reduction. After
C         NT*NS*N function evaluations, temperature (T) is changed
C         by the factor RT. Value suggested by Corana et al. is
C         MAX(100, 5*N). See Goffe et al. for further advice. (INT)
C    NEPS - Number of final function values used to decide upon termi-
C           nation. See EPS. Suggested value is 4. (INT)
C    MAXEVL - The maximum number of function evaluations. If it is
C             exceeded, IER = 1. (INT)
C    LB - The lower bound for the allowable solution variables. (DP(N))
C    UB - The upper bound for the allowable solution variables. (DP(N))
C         If the algorithm chooses X(I) .LT. LB(I) or X(I) .GT. UB(I),
C         I = 1, N, a point is from inside is randomly selected. This
C         This focuses the algorithm on the region inside UB and LB.
C         Unless the user wishes to concentrate the search to a par-
C         ticular region, UB and LB should be set to very large positive
C         and negative values, respectively. Note that the starting
C         vector X should be inside this region. Also note that LB and
C         UB are fixed in position, while VM is centered on the last
C         accepted trial set of variables that optimizes the function.
C    C - Vector that controls the step length adjustment. The suggested
C        value for all elements is 2.0. (DP(N))
C    IPRINT - controls printing inside SA. (INT)
C             Values: 0 - Nothing printed.
C                     1 - Function value for the starting value and
C                         summary results before each temperature
C                         reduction. This includes the optimal
C                         function value found so far, the total
C                         number of moves (broken up into uphill,
C                         downhill, accepted and rejected), the
C                         number of out of bounds trials, the
C                         number of new optima found at this
C                         temperature, the current optimal X and
C                         the step length VM. Note that there are
C                         N*NS*NT function evalutations before each
C                         temperature reduction. Finally, notice is
C                         is also given upon achieveing the termination
C                         criteria.
C                     2 - Each new step length (VM), the current optimal
C                         X (XOPT) and the current trial X (X). This
C                         gives the user some idea about how far X
C                         strays from XOPT as well as how VM is adapting
C                         to the function.
C                     3 - Each function evaluation, its acceptance or
C                         rejection and new optima. For many problems,
C                         this option will likely require a small tree
C                         if hard copy is used. This option is best
C                         used to learn about the algorithm. A small
C                         value for MAXEVL is thus recommended when
C                         using IPRINT = 3.
C             Suggested value: 1
C             Note: For a given value of IPRINT, the lower valued
C                   options (other than 0) are utilized.
C    ISEED1 - The first seed for the random number generator RANMAR.
C             0 .LE. ISEED1 .LE. 31328. (INT)
C    ISEED2 - The second seed for the random number generator RANMAR.
C             0 .LE. ISEED2 .LE. 30081. Different values for ISEED1
C             and ISEED2 will lead to an entirely different sequence
C             of trial points and decisions on downhill moves (when
C             maximizing). See Goffe et al. on how this can be used
C             to test the results of SA. (INT)
C
C  Input/Output Parameters:
C    T - On input, the initial temperature. See Goffe et al. for advice.
C        On output, the final temperature. (DP)
C    VM - The step length vector. On input it should encompass the
C         region of interest given the starting value X. For point
C         X(I), the next trial point is selected is from X(I) - VM(I)
C         to  X(I) + VM(I). Since VM is adjusted so that about half
C         of all points are accepted, the input value is not very
C         important (i.e. is the value is off, SA adjusts VM to the
C         correct value). (DP(N))
C
C  Output Parameters:
C    XOPT - The variables that optimize the function. (DP(N))
C    FOPT - The optimal value of the function. (DP)
C    NACC - The number of accepted function evaluations. (INT)
C    NFCNEV - The total number of function evaluations. In a minor
C             point, note that the first evaluation is not used in the
C             core of the algorithm; it simply initializes the
C             algorithm. (INT).
C    NOBDS - The total number of trial function evaluations that
C            would have been out of bounds of LB and UB. Note that
C            a trial point is randomly selected between LB and UB.
C            (INT)
C    IER - The error return number. (INT)
C          Values: 0 - Normal return; termination criteria achieved.
C                  1 - Number of function evaluations (NFCNEV) is
C                      greater than the maximum number (MAXEVL).
C                  2 - The starting value (X) is not inside the
C                      bounds (LB and UB).
C                  3 - The initial temperature is not positive.
C                  99 - Should not be seen; only used internally.
C
C  Work arrays that must be dimensioned in the calling routine:
C       RWK1 (DP(NEPS))  (FSTAR in SA)
C       RWK2 (DP(N))     (XP    "  " )
C       IWK  (INT(N))    (NACP  "  " )
C
C  Required Functions (included):
C    EXPREP - Replaces the function EXP to avoid under- and overflows.
C             It may have to be modified for non IBM-type main-
C             frames. (DP)
C    RMARIN - Initializes the random number generator RANMAR.
C    RANMAR - The actual random number generator. Note that
C             RMARIN must run first (SA does this). It produces uniform
C             random numbers on [0,1]. These routines are from
C             Usenet's comp.lang.fortran. For a reference, see
C             "Toward a Universal Random Number Generator"
C             by George Marsaglia and Arif Zaman, Florida State
C             University Report: FSU-SCRI-87-50 (1987).
C             It was later modified by F. James and published in
C             "A Review of Pseudo-random Number Generators." For
C             further information, contact stuart@ads.com. These
C             routines are designed to be portable on any machine
C             with a 24-bit or more mantissa. I have found it produces
C             identical results on a IBM 3081 and a Cray Y-MP.
C
C  Required Subroutines (included):
C    PRTVEC - Prints vectors.
C    PRT1 ... PRT10 - Prints intermediate output.
C    FCN - Function to be optimized. The form is
C            SUBROUTINE FCN(N,X,F)
C            INTEGER N
C            DOUBLE PRECISION  X(N), F
C            ...
C            function code with F = F(X)
C            ...
C            RETURN
C            END
C          Note: This is the same form used in the multivariable
C          minimization algorithms in the IMSL edition 10 library.
C
C  Machine Specific Features:
C    1. EXPREP may have to be modified if used on non-IBM type main-
C       frames. Watch for under- and overflows in EXPREP.
C    2. Some FORMAT statements use G25.18; this may be excessive for
C       some machines.
C    3. RMARIN and RANMAR are designed to be protable; they should not
C       cause any problems.
C
C***********************************************************************
      SUBROUTINE SPAN(N,X,MAX,RT,EPS,NS,NT,NEPS,MAXEVL,LB,UB,C,IPRINT,
     1              ISEED1,ISEED2,T,VM,XOPT,FOPT,NACC,NFCNEV,NOBDS,IER,
     2              FSTAR,XP,NACP,work)


C**** Type all argument variables.
      DOUBLE PRECISION  X(*), LB(*), UB(*), C(*), VM(*), FSTAR(*),
     1                  XOPT(*), XP(*), T, EPS, RT, FOPT
      INTEGER  NACP(*), work(*), N, NS, NT, NEPS, NACC, MAXEVL,
     1         IPRINT, NOBDS, IER, NFCNEV, ISEED1, ISEED2
      LOGICAL  MAX

C**** Type all internal variables.
      DOUBLE PRECISION  F, FP, P, PP, RATIO
      INTEGER  NUP, NDOWN, NREJ, NNEW, LNOBDS, H, I, J, M
      LOGICAL  QUIT

C**** Include all internal CPU variables
      INTEGER ncpu, thiscpu, totalcalcs, ncalcpcpu, ncalcleft
      INTEGER ncalc
      INTEGER RANK, IERROR, tag


C**** Type all functions.
      DOUBLE PRECISION  EXPREP
      REAL  RANMAR

C**** Debugging variables
      INTEGER CYCLES


C**** Initialize parallel code
      INCLUDE 'mpif.h'
      CALL MPI_INIT(IERROR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, ncpu, IERROR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, RANK, IERROR)
      thiscpu = RANK + 1
      cycles = 1

C**** Write initial parameters to file
      if (thiscpu .eq. 1) then
         open(10,file='span.out',status='unknown')
         WRITE(10,1000) N, MAX, T, RT, EPS, NS, NT, NEPS, MAXEVL,
     1              IPRINT, ISEED1, ISEED2

         CALL PRTVEC(X,N,'STARTING VALUES')
         CALL PRTVEC(VM,N,'INITIAL STEP LENGTH')
         CALL PRTVEC(LB,N,'LOWER BOUND')
         CALL PRTVEC(UB,N,'UPPER BOUND')
         CALL PRTVEC(C,N,'C VECTOR')
         WRITE(10,'(/,''  ****   END OF DRIVER ROUTINE OUTPUT   ****''
     1          /,''  ****   BEFORE CALL TO SA.             ****'')')
         WRITE(*,*) 'Number of proccessors: ', ncpu
      endif 
      
C**** Determine how many function evaluations each cpu will make
      totalcalcs = NS
      ncalcpcpu = totalcalcs / ncpu
      ncalcleft = totalcalcs - (ncalcpcpu * ncpu)
      if (thiscpu .le. ncalcleft) then
         ncalc = ncalcpcpu + 1
      else
         ncalc = ncalcpcpu
      end if

C**** Initialize the random number generator RANMAR.
      CALL RMARIN(ISEED1+RANK,ISEED2+2*RANK+1)

C**** Set initial values.
      NACC = 0
      NOBDS = 0
      NFCNEV = 0
      IER = 99

      DO 10, I = 1, N
         XOPT(I) = X(I)
         NACP(I) = 0
10    CONTINUE

      DO 20, I = 1, NEPS
         FSTAR(I) = 1.0D+20
20    CONTINUE 

C**** If the initial temperature is not positive, notify the user and 
C*    return to the calling routine.  
      IF (T .LE. 0.0) THEN
         WRITE(*,'(/,''  THE INITIAL TEMPERATURE IS NOT POSITIVE. ''
     1             /,''  RESET THE VARIABLE T. ''/)')
         IER = 3
         CALL FINALIZE(thiscpu,N,XOPT,VM,FOPT,NFCNEV,NACC,
     1                 NOBDS,T,IER) 
         RETURN
      END IF

C**** If the initial value is out of bounds, notify the user and return
C*    to the calling routine.
      DO 30, I = 1, N
         IF ((X(I) .GT. UB(I)) .OR. (X(I) .LT. LB(I))) THEN
            CALL PRT1
            IER = 2
            CALL FINALIZE(thiscpu,N,XOPT,VM,FOPT,NFCNEV,NACC,
     1                    NOBDS,T,IER) 
            RETURN
         END IF
30    CONTINUE


C**** Evaluate the function with input X and return value as F.
      CALL FCN(N,X,F)

C**** If the function is to be minimized, switch the sign of the function.
C*    Note that all intermediate and final output switches the sign back
C*    to eliminate any possible confusion for the user.
      IF(.NOT. MAX) F = -F
      NFCNEV = NFCNEV + 1
      FOPT = F
      FSTAR(1) = F
      IF(THISCPU .EQ. 1 .AND. IPRINT .GE. 1) CALL PRT2(MAX,N,X,F)


C**** Start the main loop. Note that it terminates if (i) the algorithm
C*    succesfully optimizes the function or (ii) there are too many
C*    function evaluations (more than MAXEVL).
100   NUP = 0
      NREJ = 0
      NNEW = 0
      NDOWN = 0
      LNOBDS = 0


C**** Each cpu expects ncalc runs and runs independently from other
C**** cpu's until communication is done after a neighborhood search.
C**** ncalc may vary between cpus.  
      DO 400, M = 1, NT
         DO 300, J = 1, ncalc
            DO 200, H = 1, N

C**** Generate XP, the trial value of X. Note use of VM to choose XP.
               DO 110, I = 1, N
                  IF (I .EQ. H) THEN
                     XP(I) = X(I) + (RANMAR()*2.- 1.) * VM(I)
                  ELSE
                     XP(I) = X(I)
                  END IF

C**** If XP is out of bounds, select a point in bounds for the trial.
                  IF((XP(I) .LT. LB(I)) .OR. (XP(I) .GT. UB(I))) THEN
                     XP(I) = LB(I) + (UB(I) - LB(I))*RANMAR()
                     LNOBDS = LNOBDS + 1
                     NOBDS = NOBDS + 1
                     IF(IPRINT .GE. 3) CALL PRT3(MAX,N,XP,X,FP,F)
                  END IF
110            CONTINUE


C**** Evaluate the function with the trial point XP and return as FP.
               CALL FCN(N,XP,FP)
               IF(.NOT. MAX) FP = -FP
               NFCNEV = NFCNEV + 1
               IF(IPRINT .GE. 3) CALL PRT4(MAX,N,XP,X,FP,F)

C**** If too many function evaluations occur, terminate the algorithm.
               IF(NFCNEV .GE. MAXEVL) THEN
                  CALL PRT5
                  IF (.NOT. MAX) FOPT = -FOPT
                  IER = 1
                  CALL FINALIZE(thiscpu,N,XOPT,VM,FOPT,NFCNEV,NACC,
     1                    NOBDS,T,IER) 
                  RETURN
               END IF

C**** Accept the new point if the function value increases.
               IF(FP .GE. F) THEN
                  IF(IPRINT .GE. 3) THEN
                     WRITE(*,'(''  POINT ACCEPTED'')')
                  END IF
                  DO 120, I = 1, N
                     X(I) = XP(I)
120               CONTINUE
                  F = FP
                  NACC = NACC + 1
                  NACP(H) = NACP(H) + 1
                  NUP = NUP + 1

C**** If greater than any other point, record as new optimum.
                  IF (FP .GT. FOPT) THEN
                     IF(IPRINT .GE. 3) THEN
                        WRITE(*,'(''  NEW OPTIMUM'')')
                     END IF
                     DO 130, I = 1, N
                        XOPT(I) = XP(I)
130                  CONTINUE
                     FOPT = FP
                     NNEW = NNEW + 1
                  END IF

C**** If the point is lower, use the Metropolis criteria to decide on
C*    acceptance or rejection.
               ELSE
                  P = EXPREP((FP - F)/T)
                  PP = RANMAR()
                  IF (PP .LT. P) THEN
                     IF(IPRINT .GE. 3) CALL PRT6(MAX)
                     DO 140, I = 1, N
                        X(I) = XP(I)
140                  CONTINUE
                     F = FP
                     NACC = NACC + 1
                     NACP(H) = NACP(H) + 1
                     NDOWN = NDOWN + 1
                  ELSE
                     NREJ = NREJ + 1
                     IF(IPRINT .GE. 3) CALL PRT7(MAX)
                  END IF
               END IF

200         CONTINUE
300      CONTINUE


C**** Communicate among processors to refine the neighborhood.
      CALL SPAN_COMMUNICATE(ncpu, thiscpu, UB, LB, C, NACP,
     1 	            work, M, N, X, XOPT, FOPT, VM, NT, NS, F)

      IF(IPRINT .GE. 2) THEN
         CALL PRT8(N,VM,XOPT,X)
      END IF

      if (thiscpu .eq. 1) write(*,*) 'NT cycles:', cycles
      cycles = cycles + 1
400   CONTINUE

      IF(THISCPU .EQ. 1 .AND. IPRINT .GE. 1) THEN
         CALL PRT9(MAX,N,T,XOPT,VM,FOPT,NUP,NDOWN,NREJ,LNOBDS,NNEW)
      END IF

C**** Check termination criteria.
      QUIT = .FALSE.
      FSTAR(1) = F
      IF ((FOPT - FSTAR(1)) .LE. EPS) QUIT = .TRUE.
      DO 410, I = 1, NEPS
         IF (ABS(F - FSTAR(I)) .GT. EPS) THEN
            QUIT = .FALSE.
            GO TO 415
         ENDIF
410   CONTINUE
415   CONTINUE


C**** Terminate SA if appropriate.
      IF (QUIT) THEN
         DO 420, I = 1, N
            X(I) = XOPT(I)
420      CONTINUE
         IER = 0
         IF (.NOT. MAX) FOPT = -FOPT
         IF(THISCPU .EQ. 1 .AND. IPRINT .GE. 1) CALL PRT10
         CALL FINALIZE(thiscpu,N,XOPT,VM,FOPT,NFCNEV,NACC,
     1                 NOBDS,T,IER) 
         RETURN
      END IF


C**** If termination criteria is not met, prepare for another loop.
      T = RT*T
      DO 430, I = NEPS, 2, -1
         FSTAR(I) = FSTAR(I-1)
430   CONTINUE
      F = FOPT
      DO 440, I = 1, N
         X(I) = XOPT(I)
440   CONTINUE


C  Loop again.
      GO TO 100

1000  FORMAT(/,' SIMULATED PARALLEL ANNEALING by NEIGHBORHOOD EXAMPLE',/,
     1       /,' NUMBER OF PARAMETERS: ',I3,'   MAXIMAZATION: ',L5,
     2       /,' INITIAL TEMP: ', G8.2, '   RT: ',G8.2, '   EPS: ',G8.2,
     3       /,' NS: ',I3, '   NT: ',I3, '   NEPS: ',I2,
     4       /,' MAXEVL: ',I10, '   IPRINT: ',I1, '   ISEED1: ',I4,
     5       ' ISEED2: ',I4)




      END



C***********************************************************************
C**** Communicate among processors to refine the neighborhood.
C
C
C
      SUBROUTINE SPAN_COMMUNICATE(ncpu, thiscpu, UB, LB, C, NACP,
     1           total_NACP, M, N, X, XOPT, FOPT, VM, NT, NS, F)

      IMPLICIT none
      INCLUDE 'mpif.h'

C**** Arguments
      INTEGER N, ncpu, thiscpu, NACP(*),total_NACP(*), M, NT, NS
      DOUBLE PRECISION UB(*),LB(*),C(*),X(*),XOPT(*),FOPT,VM(*),F     


C**** Local variables
      INTEGER size, I, j, k, best_cpu
      INTEGER tag, IERROR, STATUS(MPI_STATUS_SIZE)
      DOUBLE PRECISION RATIO, best_FOPT 


C**** DETERMINE FOPT AND CPU THAT HAD BEST FOPT
      best_cpu = 0
      tag = 1
C**** Send data to cpu 1.
      if (thiscpu .gt. 1) then

         CALL MPI_SSEND(FOPT,1,MPI_REAL8,0,tag,MPI_COMM_WORLD,
     *     IERROR)

C**** Receive data from all other cpu's.
      else

         best_cpu = thiscpu
         best_FOPT = FOPT
         DO 520, j = 2, ncpu

            CALL MPI_RECV(FOPT,1,MPI_REAL8,j-1,tag,MPI_COMM_WORLD,
     *         STATUS,IERROR)
            if (IERROR.ne.0) then
               print*,'Error receiving FOPT from cpu ',j
               stop
            endif

C****       Compare value of FOPT from cpu j to best_FOPT
            if (FOPT .gt. best_FOPT) then
               best_cpu = j 
               best_FOPT = FOPT
            endif

520      CONTINUE
      FOPT = best_FOPT
      endif


C**** COMMUNICATE WHICH CPU WAS BEST
      tag = 2
      if (thiscpu .eq. 1) then

         do j = 2,ncpu 
           CALL MPI_SSEND(best_cpu,1,MPI_INTEGER,j-1,tag,
     *     MPI_COMM_WORLD,IERROR)
         enddo

      else

          CALL MPI_RECV(best_cpu,1,MPI_INTEGER,0,tag,MPI_COMM_WORLD,
     1       STATUS,IERROR)
          if (IERROR.ne.0) then
             print*,'Error receiving best_cpu from master'
             stop
          endif

      endif 


C**** ADJUST STEP LENGTH
      tag = 3
C**** Send nacp (number of accepted points) to best cpu
      if (thiscpu .ne. best_cpu) then

         CALL MPI_SSEND(NACP,N,MPI_INTEGER,best_cpu-1,tag,
     *                  MPI_COMM_WORLD,IERROR)

C**** Receive nacp from cpu's other than the best cpu
      else

C****    Initialize total_NACP
         DO j = 1, N
            total_NACP(j) = NACP(j)
         ENDDO

         DO 530, j = 1, ncpu
            if (j.ne.best_cpu) then

              CALL MPI_RECV(NACP,N,MPI_INTEGER,j-1,tag,
     *                      MPI_COMM_WORLD,STATUS,IERROR)
              if (IERROR.ne.0) then
                 print*,'Error receiving NACP from cpu ',j
                 stop
              endif

              DO k = 1, N
                 total_NACP(k) = total_NACP(k) + NACP(k)
              ENDDO

            endif 
530      CONTINUE

C****    Update NACP
         DO j = 1,N
            NACP(j) = total_NACP(j)
         ENDDO

C****    Adjust VM so that approximately half of all evaluations 
C****    are accepted.  
         DO 310, I = 1, N
            RATIO = DFLOAT(NACP(I)) /DFLOAT(NS)
            IF (RATIO .GT. .6) THEN
               VM(I) = VM(I)*(1. + C(I)*(RATIO - .6)/.4)
            ELSE IF (RATIO .LT. .4) THEN
               VM(I) = VM(I)/(1. + C(I)*((.4 - RATIO)/.4))
            END IF

            IF (VM(I) .GT. (UB(I)-LB(I))) THEN
               VM(I) = UB(I) - LB(I)
            END IF
310      CONTINUE

      endif


C**** UPDATE ALL CPU'S
      tag = 4
C**** Best cpu sends new FOPT, F, VM, X, and XOPT to other cpu's
      if (thiscpu .eq. best_cpu) then
         DO 595, j = 1, ncpu
            if(j.ne.best_cpu) then
               CALL MPI_SSEND(FOPT,1,MPI_REAL8,j-1,tag,
     1            MPI_COMM_WORLD,IERROR)
               CALL MPI_SSEND(F,1,MPI_REAL8,j-1,tag,
     1            MPI_COMM_WORLD,IERROR)
               CALL MPI_SSEND(VM,N,MPI_REAL8,j-1,tag,
     1            MPI_COMM_WORLD,IERROR)
               CALL MPI_SSEND(X,N,MPI_REAL8,j-1,tag,
     1            MPI_COMM_WORLD,IERROR)
               CALL MPI_SSEND(XOPT,N,MPI_REAL8,j-1,tag,
     1            MPI_COMM_WORLD,IERROR)
            endif
595      CONTINUE

C**** Receive FOPT, F, VM, X, and XOPT from best cpu.
      else if (thiscpu .ne. best_cpu) then

         CALL MPI_RECV(FOPT,1,MPI_REAL8,best_cpu-1,tag,
     1   MPI_COMM_WORLD,STATUS,IERROR)
         if (IERROR.ne.0) then
            print*,'Error receiving FOPT from best cpu'
            stop
         endif

         CALL MPI_RECV(F,1,MPI_REAL8,best_cpu-1,tag,
     1   MPI_COMM_WORLD,STATUS,IERROR)
         if (IERROR.ne.0) then
            print*,'Error receiving FOPT from best cpu'
            stop
         endif

         CALL MPI_RECV(VM,N,MPI_REAL8,best_cpu-1,tag,
     1   MPI_COMM_WORLD,STATUS,IERROR)
         if (IERROR.ne.0) then
            print*,'Error receiving VM from best cpu'
            stop
         endif

         CALL MPI_RECV(X,N,MPI_REAL8,best_cpu-1,tag,
     1   MPI_COMM_WORLD,STATUS,IERROR)
         if (IERROR.ne.0) then
            print*,'Error receiving X from best cpu'
            stop
         endif

         CALL MPI_RECV(XOPT,N,MPI_REAL8,best_cpu-1,tag,
     1   MPI_COMM_WORLD,STATUS,IERROR)
         if (IERROR.ne.0) then
            print*,'Error receiving FOPT from best cpu'
            stop
         endif
      endif

C**** Reset NACP
      DO I = 1, N
         NACP(I) = 0
      ENDDO


      RETURN
      END




C***********************************************************************
      FUNCTION  EXPREP(RDUM)
C  This function replaces exp to avoid under- and overflows and is
C  designed for IBM 370 type machines. It may be necessary to modify
C  it for other machines. Note that the maximum and minimum values of
C  EXPREP are such that they has no effect on the algorithm.

      DOUBLE PRECISION  RDUM, EXPREP

      IF (RDUM .GT. 174.) THEN
         EXPREP = 3.69D+75
      ELSE IF (RDUM .LT. -180.) THEN
         EXPREP = 0.0
      ELSE
         EXPREP = EXP(RDUM)
      END IF

      RETURN
      END

      subroutine RMARIN(IJ,KL)
C  This subroutine and the next function generate random numbers. See
C  the comments for SA for more information. The only changes from the
C  orginal code is that (1) the test to make sure that RMARIN runs first
C  was taken out since SA assures that this is done (this test didn't
C  compile under IBM's VS Fortran) and (2) typing ivec as integer was
C  taken out since ivec isn't used. With these exceptions, all following
C  lines are original.

C This is the initialization routine for the random number generator
C     RANMAR()
C NOTE: The seed variables can have values between:    0 <= IJ <= 31328
C                                                      0 <= KL <= 30081
      real U(97), C, CD, CM
      integer I97, J97
      common /raset1/ U, C, CD, CM, I97, J97
      if( IJ .lt. 0  .or.  IJ .gt. 31328  .or.
     *    KL .lt. 0  .or.  KL .gt. 30081 ) then
          print '(A)', ' The first random number seed must have a value
     *between 0 and 31328'
          print '(A)',' The second seed must have a value between 0 and
     *30081'
            stop
      endif
      i = mod(IJ/177, 177) + 2
      j = mod(IJ    , 177) + 2
      k = mod(KL/169, 178) + 1
      l = mod(KL,     169)
      do 2 ii = 1, 97
         s = 0.0
         t = 0.5
         do 3 jj = 1, 24
            m = mod(mod(i*j, 179)*k, 179)
            i = j
            j = k
            k = m
            l = mod(53*l+1, 169)
            if (mod(l*m, 64) .ge. 32) then
               s = s + t
            endif
            t = 0.5 * t
3        continue
         U(ii) = s
2     continue
      C = 362436.0 / 16777216.0
      CD = 7654321.0 / 16777216.0
      CM = 16777213.0 /16777216.0
      I97 = 97
      J97 = 33
      return
      end

      function ranmar()
      real U(97), C, CD, CM
      integer I97, J97
      common /raset1/ U, C, CD, CM, I97, J97
         uni = U(I97) - U(J97)
         if( uni .lt. 0.0 ) uni = uni + 1.0
         U(I97) = uni
         I97 = I97 - 1
         if(I97 .eq. 0) I97 = 97
         J97 = J97 - 1
         if(J97 .eq. 0) J97 = 97
         C = C - CD
         if( C .lt. 0.0 ) C = C + CM
         uni = uni - C
         if( uni .lt. 0.0 ) uni = uni + 1.0
         RANMAR = uni
      return
      END

      SUBROUTINE PRT1
C  This subroutine prints intermediate output, as does PRT2 through
C  PRT10. Note that if SA is minimizing the function, the sign of the
C  function value and the directions (up/down) are reversed in all
C  output to correspond with the actual function optimization. This
C  correction is because SA was written to maximize functions and
C  it minimizes by maximizing the negative a function.

      WRITE(10,'(/,''  THE STARTING VALUE (X) IS OUTSIDE THE BOUNDS ''
     1          /,''  (LB AND UB). EXECUTION TERMINATED WITHOUT ANY''
     2          /,''  OPTIMIZATION. RESPECIFY X, UB OR LB SO THAT  ''
     3          /,''  LB(I) .LT. X(I) .LT. UB(I), I = 1, N. ''/)')

      RETURN
      END

      SUBROUTINE PRT2(MAX,N,X,F)

      DOUBLE PRECISION  X(*), F
      INTEGER  N
      LOGICAL  MAX

      WRITE(10,'(''  '')')
      CALL PRTVEC(X,N,'INITIAL X')
      IF (MAX) THEN
         WRITE(10,'(''  INITIAL F: '',/, G25.18)') F
      ELSE
         WRITE(10,'(''  INITIAL F: '',/, G25.18)') -F
      END IF

      RETURN
      END

      SUBROUTINE PRT3(MAX,N,XP,X,FP,F)

      DOUBLE PRECISION  XP(*), X(*), FP, F
      INTEGER  N
      LOGICAL  MAX

      WRITE(10,'(''  '')')
      CALL PRTVEC(X,N,'CURRENT X')
      IF (MAX) THEN
         WRITE(10,'(''  CURRENT F: '',G25.18)') F
      ELSE
         WRITE(10,'(''  CURRENT F: '',G25.18)') -F
      END IF
      CALL PRTVEC(XP,N,'TRIAL X')
      WRITE(10,'(''  POINT REJECTED SINCE OUT OF BOUNDS'')')

      RETURN
      END

      SUBROUTINE PRT4(MAX,N,XP,X,FP,F)

      DOUBLE PRECISION  XP(*), X(*), FP, F
      INTEGER  N
      LOGICAL  MAX

      WRITE(10,'(''  '')')
      CALL PRTVEC(X,N,'CURRENT X')
      IF (MAX) THEN
         WRITE(10,'(''  CURRENT F: '',G25.18)') F
         CALL PRTVEC(XP,N,'TRIAL X')
         WRITE(10,'(''  RESULTING F: '',G25.18)') FP
      ELSE
         WRITE(10,'(''  CURRENT F: '',G25.18)') -F
         CALL PRTVEC(XP,N,'TRIAL X')
         WRITE(10,'(''  RESULTING F: '',G25.18)') -FP
      END IF

      RETURN
      END

      SUBROUTINE PRT5

      WRITE(10,'(/,''  TOO MANY FUNCTION EVALUATIONS; CONSIDER ''
     1          /,''  INCREASING MAXEVL OR EPS, OR DECREASING ''
     2          /,''  NT OR RT. THESE RESULTS ARE LIKELY TO BE ''
     3          /,''  POOR.'',/)')

      RETURN
      END

      SUBROUTINE PRT6(MAX)

      LOGICAL  MAX

      IF (MAX) THEN
         WRITE(10,'(''  THOUGH LOWER, POINT ACCEPTED'')')
      ELSE
         WRITE(10,'(''  THOUGH HIGHER, POINT ACCEPTED'')')
      END IF

      RETURN
      END

      SUBROUTINE PRT7(MAX)

      LOGICAL  MAX

      IF (MAX) THEN
         WRITE(10,'(''  LOWER POINT REJECTED'')')
      ELSE
         WRITE(10,'(''  HIGHER POINT REJECTED'')')
      END IF

      RETURN
      END

      SUBROUTINE PRT8(N,VM,XOPT,X)

      DOUBLE PRECISION  VM(*), XOPT(*), X(*)
      INTEGER  N

      WRITE(10,'(/,
     1  '' INTERMEDIATE RESULTS AFTER STEP LENGTH ADJUSTMENT'',/)')
      CALL PRTVEC(VM,N,'NEW STEP LENGTH (VM)')
      CALL PRTVEC(XOPT,N,'CURRENT OPTIMAL X')
      CALL PRTVEC(X,N,'CURRENT X')
      WRITE(10,'('' '')')

      RETURN
      END

      SUBROUTINE PRT9(MAX,N,T,XOPT,VM,FOPT,NUP,NDOWN,NREJ,LNOBDS,NNEW)

      DOUBLE PRECISION  XOPT(*), VM(*), T, FOPT
      INTEGER  N, NUP, NDOWN, NREJ, LNOBDS, NNEW, TOTMOV
      LOGICAL  MAX

      TOTMOV = NUP + NDOWN + NREJ

      WRITE(10,'(/,
     1  '' INTERMEDIATE RESULTS BEFORE NEXT TEMPERATURE REDUCTION'',/)')
      WRITE(10,'(''  CURRENT TEMPERATURE:            '',G12.5)') T
      IF (MAX) THEN
         WRITE(10,'(''  MAX FUNCTION VALUE SO FAR:  '',G25.18)') FOPT
         WRITE(10,'(''  TOTAL MOVES:                '',I8)') TOTMOV
         WRITE(10,'(''     UPHILL:                  '',I8)') NUP
         WRITE(10,'(''     ACCEPTED DOWNHILL:       '',I8)') NDOWN
         WRITE(10,'(''     REJECTED DOWNHILL:       '',I8)') NREJ
         WRITE(10,'(''  OUT OF BOUNDS TRIALS:       '',I8)') LNOBDS
         WRITE(10,'(''  NEW MAXIMA THIS TEMPERATURE:'',I8)') NNEW
      ELSE
         WRITE(10,'(''  MIN FUNCTION VALUE SO FAR:  '',G25.18)') -FOPT
         WRITE(10,'(''  TOTAL MOVES:                '',I8)') TOTMOV
         WRITE(10,'(''     DOWNHILL:                '',I8)')  NUP
         WRITE(10,'(''     ACCEPTED UPHILL:         '',I8)')  NDOWN
         WRITE(10,'(''     REJECTED UPHILL:         '',I8)')  NREJ
         WRITE(10,'(''  TRIALS OUT OF BOUNDS:       '',I8)')  LNOBDS
         WRITE(10,'(''  NEW MINIMA THIS TEMPERATURE:'',I8)')  NNEW
      END IF
      CALL PRTVEC(XOPT,N,'CURRENT OPTIMAL X')
      CALL PRTVEC(VM,N,'STEP LENGTH (VM)')
      WRITE(10,'('' '')')

      RETURN
      END

      SUBROUTINE PRT10

      WRITE(10,'(/,''  SA ACHIEVED TERMINATION CRITERIA. 
     *IER = 0. '',/)')

      RETURN
      END


C========================================================================
C Finalize the run. 
C========================================================================
      SUBROUTINE FINALIZE(thiscpu,N,XOPT,VM,FOPT,NFCNEV,NACC,NOBDS,T,
     1                    IER)   

      implicit none
      integer IERROR
      integer thiscpu,N,NFCNEV,NACC,NOBDS,IER
      double precision XOPT(*),VM(*),FOPT,T

      if (thiscpu .eq. 1) then
         WRITE(10,'(/,''  ****   RESULTS AFTER SA   ****   '')')
         CALL PRTVEC(XOPT,N,'SOLUTION')
         CALL PRTVEC(VM,N,'FINAL STEP LENGTH')
         WRITE(10,1001) FOPT, NFCNEV, NACC, NOBDS, T, IER
      endif

      CALL MPI_FINALIZE(IERROR)

1001  FORMAT(/,' OPTIMAL FUNCTION VALUE: ',G20.13
     1       /,' NUMBER OF FUNCTION EVALUATIONS:     ',I10,
     2       /,' NUMBER OF ACCEPTED EVALUATIONS:     ',I10,
     3       /,' NUMBER OF OUT OF BOUND EVALUATIONS: ',I10,
     4       /,' FINAL TEMP: ', G20.13,'  IER: ', I3)

      RETURN
      END



c========================================================================
      SUBROUTINE PRTVEC(VECTOR,NCOLS,NAME)
C  This subroutine prints the double precision vector named VECTOR.
C  Elements 1 thru NCOLS will be printed. NAME is a character variable
C  that describes VECTOR. Note that if NAME is given in the call to
C  PRTVEC, it must be enclosed in quotes. If there are more than 10
C  elements in VECTOR, 10 elements will be printed on each line.

      INTEGER NCOLS
      DOUBLE PRECISION VECTOR(NCOLS)
      CHARACTER *(*) NAME

      WRITE(10,1001) NAME

      IF (NCOLS .GT. 10) THEN
         LINES = INT(NCOLS/10.)

         DO 100, I = 1, LINES
            LL = 10*(I - 1)
            WRITE(10,1000) (VECTOR(J),J = 1+LL, 10+LL)
  100    CONTINUE

         WRITE(10,1000) (VECTOR(J),J = 11+LL, NCOLS)
      ELSE
         WRITE(10,1000) (VECTOR(J),J = 1, NCOLS)
      END IF

 1000 FORMAT( 10(G20.10,1X))
 1001 FORMAT(/,25X,A)

      RETURN
      END
