C Subroutines for sprinting model, includes:
C   - MUSCLEMODEL: calculates the torque generated for a given level of 
C                  activation and joint kinematics
C   - ACITVATION:  activation for MUSCLEMODEL, currently 2 ramps
C   - INITCCANG:   estimates intial CC angle for MUSCLEMODEL
C   - TQVEL:       toruqe-angular velocity relationship for MUSCLEMODEL
C   - INITCOND:    converts model inputs to generalised coordinates 
C                  (will need to change for each model)
C   - TORQUE9:     calculates torque given an activation, angle and 
C                  angular velocity. No SEC
C   - KUTTA:       numerical integrator using variable step size
C   - SOLVE:       solves a system of equations
C   - EVALSPLINE:  evaluates a quintic spline at a specified time point
C
C   Tom Rottier 2020
C***********************************************************************
      SUBROUTINE MUSCLEMODEL(T,TQP,ACTP,K,THETA,OMEGA,TQ,SECANG,SECANGVE
     &L,CCANG,CCANGVEL,DT,N)
C Torque generator model subroutines used for simulating sprinting
C   Tom Rottier 2020
C
C Inputs:
C   - T:          time
C   - TQP:        parameters of the torque generator (9 parameters)
C   - ACTP:       parameters for the activation profile (3*N+1 params)
C   - K:          SEC stiffness
C   - THETA:      joint angle
C   - OMEGA:      joint angular velocity
C   - DT:         integrator step
C   - CCANG:      CC angle (input current angle, output next time step)
C   - N:          number of ramps for activation
C
C Outputs:
C   - TQ:         torque produced at the joint
C
C
C Optional outputs:
C   - SECANG:     SEC angle
C   - CCANGVEL:   CC angular velocity
C   - SECANGVEL:  SEC angular velocity (input current, output next)
C
C Subroutines:
C   - ACTIVATION: calculates the activation value based on a quintic
C                 function, parameters from ACTP
C   - INITCCANG:  finds initial CC angle
C   - TQVEL:      calculates either the torque-angular velocity
C                 or CC angular velocity depending on input
C
C Local varibles:
C   - T0:         maximum isometric torque
C   - ACT:        activation
C   - TA:         normalised torque-angle value
C   - TV:         normalised torque-angular velocity value
C
C Parameters:
C - Torque:
C       TMAX  = TQP(1)
C       T0    = TQP(2)
C       WMAX  = TQP(3)
C       WC    = TQP(4)
C       K     = 4.3D0
C       AMIN  = TQP(5)
C       W1    = TQP(6)
C       WR    = TQP(7)
C       AMAX  = 1.0D0
C       THOPT = TQP(8)
C       R     = TQP(9)
C      
C - Activation (T0 - A1 repeated N times):
C       A0    = ACTP(1)
C       T0    = ACTP(3)
C       TR    = ACTP(4)
C       A1    = ACTP(2)
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER N
      DIMENSION TQP(9),ACTP(1+N*3),TQWP(7)
            
C Assign torque-angular velocity parameters to new array and normalise 
      TQWP    = TQP(1:7)
      T0      = TQWP(2)
      TQMAX   = TQWP(1)
      TQWP(1) = TQP(1) / T0
      TQWP(2) = 1.0D0
    
C Calculate activation
      CALL ACTIVATION(T,ACTP,ACT,N)

C Max SEC angle
      SECANGMAX = TQMAX/K  
    
C Initial CC angle if T = 0
C Joint angular velocity opposite to CC angular velocity
      IF (ABS(T) .LT. DT) THEN
        CCANGVEL = -OMEGA
        SECANGVEL = 0.0D0
C Set TV to < 0 to get it as output         
        TV = -1.0D0
        CALL TQVEL(TQWP,CCANGVEL,TV)
        CALL INITCCANG(T0,ACT,TV,THETA,K,TQWP(1),TQP(8),TQP(9),CCANG,SEC
     &                 ANG)
        TQ = MAX(0.0D0, MIN(TQMAX, K*SECANG)) 
        CCANGVEL = -CCANGVEL

C Non-intial time        
      ELSEIF (T .GT. 0.0D0) THEN
            IF (CCANG .GT. THETA) CCANG = THETA
            SECANG = THETA - CCANG
            IF (SECANG .GT. SECANGMAX) THEN
              SECANG = SECANGMAX
              CCANG = THETA - SECANG
            ENDIF
            TQ = K*SECANG
            TA = EXP((-(CCANG - TQP(8))**2) / (2.0D0*TQP(9)**2))
            IF(TA .LT. 0.001D0) TA = 0.001D0
C If activation 0, set torques and SECANG/ANGVEL to 0 (stops fluctuations)
            IF (ACT .GE. 0.01D0) THEN
              TV = TQ/(T0*ACT*TA)
            ELSE
              CCANGVEL = OMEGA
              CCANG = THETA
              SECANG = 0.0D0
              SECANGVEL = 0.0D0
              TQ = 0.0D0
              GOTO 4000
            ENDIF
            IF (TV .GE. TQWP(1)) THEN
              CCANGVEL2 = -TQWP(3)
            ELSEIF (TV .LT. TQWP(1)) THEN
              IF (TV .LE. 0.0D0) THEN
                CCANGVEL2 = TQWP(3)
              ELSE
                CALL TQVEL(TQWP,CCANGVEL2,TV)
              ENDIF
            ENDIF

C Integrate CC angular velocity to get CC ang at next time step
C Negate CCANGVEL when using to integrate (shortening = negative)
            CCANGVEL2 = -CCANGVEL2
            IF (CCANG .GE. THETA) CCANGVEL2 = OMEGA
            CCANG = CCANG + 0.5D0*(CCANGVEL + CCANGVEL2)*DT
            CCANGVEL = CCANGVEL2
            SECANGVEL = OMEGA - CCANGVEL
      ENDIF
      
4000  RETURN
      END SUBROUTINE

C***********************************************************************
      SUBROUTINE ACTIVATION(T,P,A,N)
C Calculates the activation based on a series on quintic functions from
C Yeadon and Hiley (2000).
C
C Inputs:
C   - T: time
C   - P: actiavation function parameters (3*N + 1):
C                    A0 - inital activation level
C                    T0 - ramp time on (since start/previous ramp)
C                    TR - ramp time for ramp
C                    A1 - final activation level (will be A0 for next ramp)
C   - N: number of ramps
C
C Outputs:
C   - A: activation
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER N,I,J
      DIMENSION P(1+N*3)
        
C Loop through parameters and calculate activation
      T0 = 0.0D0
      TR = 0.0D0
      DO I = 1, N
        J = (I-1)*3
        A0 = P(J+1)
        T0 = T0 + TR + P(J+2)
        TR = P(J+3)
        A1 = P(J+4)

        AFUN = A0 + (A1-A0) * ((T-T0)/TR)**3 * (6*((T-T0)/TR)**2 - 15*((
     &            T-T0)/TR) + 10)
     
        IF (T .LE. T0) THEN
          A = A0
          RETURN
        ELSEIF ((T .GE. T0) .AND. (T .LE. T0 + TR)) THEN
          A = AFUN
          RETURN
        ELSEIF (T .GE. T0 + TR) THEN
          A = A1
        ENDIF
      ENDDO
      


!       A0 = P(1)
!       T0 = P(2)
!       TR1 =  P(3)
!       A1 = P(4)
!       T1 = P(5) + T0 + TR1
!       TR2 = P(6)
!       A2 = P(7)
!       T2 = P(8) + T1 + TR2
!       TR3 = P(9)
!       A3 = P(10)
!       T3 = P(11) + T2 + TR3
!       TR4 = P(12)
!       A4 = P(13)

!         AFUN1 = A0 + (A1-A0) * ((T-T0)/TR1)**3 * (6*((T-T0)/TR1)**2 - 15
!      &            *((T-T0)/TR1) + 10)
!         AFUN2 = A1 + (A2-A1) * ((T-T1)/TR2)**3 * (6*((T-T1)/TR2)**2 - 15
!      &           *((T-T1)/TR2) + 10)
!         AFUN3 = A2 + (A3-A2) * ((T-T2)/TR3)**3 * (6*((T-T2)/TR3)**2 - 15
!      &           *((T-T2)/TR3) + 10)
!         AFUN4 = A3 + (A4-A3) * ((T-T3)/TR4)**3 * (6*((T-T3)/TR4)**2 - 15
!      &            *((T-T3)/TR4) + 10)


!       IF (T .LT. T0) A = A0
!       IF (T .GT. T0 .AND. T .LT. T0+TR1) A = AFUN1
!       IF (T .GE. T0+TR1 .AND. T .LT. T1) A = A1
!       IF (T .GT. T1 .AND. T .LT. T1+TR2) A = AFUN2
!       IF (T .GE. T1+TR2 .AND. T .LT. T2) A = A2
!       IF (T .GT. T2 .AND. T .LT. T2+TR3) A = AFUN3
!       IF (T .GE. T2+TR3 .AND. T .LT. T3) A = A3
!       IF (T .GT. T3 .AND. T .LT. T3+TR4) A = AFUN4
!       IF (T .GE. T3+TR4) A = A4


      RETURN
      END SUBROUTINE

C***********************************************************************
      SUBROUTINE INITCCANG(T0,ACT,TV,THETA,K,TMAX,THETAOPT,R,CCANG,SECAN
     &G)
C Finds a CC angle and SEC angle that satisfy the constraint equation:
C         T0 * ACT * TA(CCANG) * TV(CCANGVEL) - k*SECANG = 0
C using binary split method. From Sam Allen.
C
C Inputs:
C   - T0:       max isometric torque
C   - ACT:      activation
C   - TV:       normalised torque-angular velocity value
C   - THETA:    joint angle
C   - K:        SEC stifness
C   - TMAX:     Eccentric torque plateau
C   - THETAOPT: optimal CC angle
C   - R:        width paramter for torque-angle relationship
C
C Outputs:
C   - CCANG:    CC angle
C   - SECANG:   SEC angle
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER NUM

C Set tolerance
      TOL = 1.0D-06
      NUM = 0
      
C Ensure CC ang positive      
      THETA2 = ABS(THETA)
      
C Set up min and max bound of CC angle, midpoint and initial interval
      TQMAX = T0*TMAX
      MIN = THETA2 - (TQMAX/K)
      IF (MIN .LT. 0.0D0) MIN = 0.0D0
      MAX = THETA2
      DIFF = (MAX-MIN) / 2.0D0
      CCANG = MIN + DIFF
      
C Evaluate torque-angle function at midpoint
      TA = EXP((-(CCANG-THETAOPT)**2) / (2.0D0*R**2))
      
C Calculate SEC torque based on angle and evalute constraint equation
      SECANG = ABS(THETA2-CCANG)
      GUESS = T0*ACT*TA*TV - SECANG*K
      
C Start iteration loop
3000  CONTINUE
      NUM = NUM + 1
      DIFF = DIFF/2.0D0
      
C Orient search in relevant direction
      IF (GUESS .GT. 0.0D0) THEN
        CCANG = CCANG - DIFF
      ELSEIF (GUESS .LT. 0.0D0) THEN
        CCANG = CCANG + DIFF
      ENDIF
      
      TA = EXP((-(CCANG-THETAOPT)**2) / (2.0D0*R**2))
      SECANG = ABS(THETA2-CCANG)
      GUESS = T0*ACT*TA*TV - SECANG*K

      IF (NUM .GT. 50) THEN
        DIFF = (MAX-MIN) / 2.0D0
        NUM = 0
      ENDIF
      
C Check if within tolerance
      IF (ABS(GUESS) .GT. TOL) GOTO 3000
      
C Reset sign
      IF (THETA .LT. 0.0D0) CCANG = -CCANG
           
      RETURN
      END SUBROUTINE

C***********************************************************************
      SUBROUTINE TQVEL(P,CCANGVEL,TQV)
C Calculates either the torque-angular velocity value based on CC
C angular velocity or finds the CC angular velocity if torque-angular  
C velocity entered.
C If calculating CC angular velocity input torque as negative number
C CC torque-angular velocity relationship modelled using equations from
C Yeadon et al. (2006) differential activation from Forrester et al.
C (2011)
C
C Inputs:
C   - P: 7 paramaters for torque generator.
C
C Inputs/Outputs:
C   - CCANGVEL: CC angular velocity
C   - TQV:      torque   
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-Z)
      DIMENSION P(7)
      LOGICAL FINDTQ,DIFFACT

C Assume finding CCANGVEL and with differential activation      
      FINDTQ  = .FALSE.
      DIFFACT = .TRUE.
      
C Assign parameters      
      TMAX = P(1)
      T0   = P(2)
      WMAX = P(3)
      WC   = P(4)
      K    = 4.3D0
      AMIN = P(5)
      W1   = P(6)
      WR   = P(7)
      AMAX = 1.0D0
      
C Check whether calculating CCANGVEL or TQV
      IF (TQV .LT. 0.00D0) FINDTQ = .TRUE.
      
C Check if no differential activation      
      IF (AMIN .GT. 0.99D0) DIFFACT = .FALSE.
      
C If finding torque
      IF (FINDTQ) THEN
        IF (CCANGVEL .GT. 0) THEN
C Concentric velocity hyperbola    
          TC    = T0 * WC / WMAX
          C     = TC * (WMAX + WC)
          TQW   = C / (WC + CCANGVEL) - TC
          IF (TQW .LT. 0.0D0) TQW = 0.0D0
        ELSEIF (CCANGVEL .LE. 0) THEN
C Eccentric velocity hyperbola
          WE    = ((TMAX - T0) / (K * T0)) * ((WMAX * WC) / (WMAX + WC))
          E     = -WE * (TMAX - T0)
          TQW   = E / (WE - CCANGVEL) + TMAX
        ENDIF
C Differential activation
        A = AMIN + (AMAX - AMIN) / (1 + EXP(-(CCANGVEL - W1) / WR))
        TQV = A*TQW
        
C If finding CC angular velocity
      ELSEIF (.NOT. FINDTQ) THEN
C Closed-form solution if no differential activation
        IF (.NOT. DIFFACT) THEN
          IF (TQV .LT. 1.0D0) THEN
            TC    = T0 * WC / WMAX
            C     = TC * (WMAX + WC)
		CCANGVEL = C / (TQV + TC) - WC
          ELSEIF (TQV .GT. 1.0D0) THEN
            WE = ((TMAX - T0) / (K * T0)) * ((WMAX * WC) / (WMAX + WC))
            E  = -WE * (TMAX - T0)
            CCANGVEL = WE - E / (TQV - TMAX)
          ENDIF
        ELSEIF (DIFFACT) THEN
          WRITE(*,*) 'Cant account for differential activation... yet'
          STOP
        ENDIF
      ENDIF      

      RETURN
      END SUBROUTINE

C**********************************************************************
      SUBROUTINE TORQUE9(W,TH,P,ACT,TQ)
C Calculates the torque from the angle and angular velocity of the CC
C from Yeadon et al. (2006) and differential activation from Forrester 
C et al. (2011). Does not account for SEC component
C
C Inputs:
C  - W:   angular velocity
C  - TH:  angle
C  - P:   7 parameters for torque generator.
C  - ACT: activation
C
C Outputs:
C   - TQ:      torque   
C
C**********************************************************************
      IMPLICIT DOUBLE PRECISION (A -Z)
      DIMENSION P(9)
C Parameters
      TMAX  = P(1)
      T0    = P(2)
      WMAX  = P(3)
      WC    = P(4)
      K     = 4.3D0
      AMIN  = P(5)
      W1    = P(6)
      WR    = P(7)
      AMAX  = 1.0D0
      THOPT = P(8)
      R     = P(9)

C Torque-angular velocity relationship (Yeadon et al., 2006)      
      IF (W .GT. 0) THEN
C       Concentric velocity hyperbola    
        TC    = T0 * WC / WMAX
        C     = TC * (WMAX + WC)
        TQW   = C / (WC + W) - TC
        IF (TQW .LT. 0.0D0) TQW = 0.0D0
      ELSEIF (W .LE. 0) THEN
C       Eccentric velocity hyperbola
        WE    = ((TMAX - T0) / (K * T0)) * ((WMAX * WC) / (WMAX + WC))
        E     = -WE * (TMAX - T0)
        TQW   = E / (WE - W) + TMAX
      ENDIF

C Differential activation (Forrester et al., 2011)
      A     = AMIN + (AMAX - AMIN) / (1 + EXP(-(W - W1) / WR))
      
C Torque-angle (normalised) relationship in Forrester et al. (2011)
      TQTH  = EXP((-(THOPT - TH)**2) / (2*R**2))
      
C Multiply for torque
       TQ   = ACT * A * TQW * TQTH
       
      END SUBROUTINE

C***********************************************************************
      SUBROUTINE INITCOND(CM)      
C Subroutines used to convert model inputs into generalised coordinates.
C Input file (.in) contains CM position and velocities and joint angles
C and velocities.
C  Tom Rottier 2020
C
C Inputs:
C   - CM: LOGICAL. If true, CM position specified in input file and Q1,
C         Q2 calculated. If false, Q1, Q2 specified in input file 
C         (usually zero) and CM calculated.
C   - Done through COMMON blocks
C Outputs:
C   - Done through COMMON blocks
C
C***********************************************************************
!       IMPLICIT DOUBLE PRECISION (A-Z)
!       LOGICAL CM
!       COMMON/CONSTNTS/ G,IA,IB,IC,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6,MA,MB,MC
!      &,MD
!       COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,U1,U2,U3,U4,U5,Q1I,Q2I,Q3I,Q4I,Q5I
!      &,U1I,U2I,U3I,U4I,U5I
!       COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(5,5),RHS(5)
          
!       Q3 = Q3*DEGtoRAD
!       Q4 = Q4*DEGtoRAD
!       Q5 = Q5*DEGtoRAD
!       U3 = U3*DEGtoRAD
!       U4 = U4*DEGtoRAD
!       U5 = U5*DEGtoRAD

!       IF (CM) THEN            
!         POCMX   = Q1
!         POCMY   = Q2
!         Q1 = POCMX - ((L5*MC+L6*MD)*COS(Q5)+(L3*MB+L4*MC+L4*MD)*COS(Q4)+
!      &(L1*MA+L2*MB+L2*MC+L2*MD)*COS(Q3))/(MA+MB+MC+MD)
!         Q2 = POCMY - ((L5*MC+L6*MD)*SIN(Q5)+(L3*MB+L4*MC+L4*MD)*SIN(Q4)+
!      &(L1*MA+L2*MB+L2*MC+L2*MD)*SIN(Q3))/(MA+MB+MC+MD)        
!       ENDIF
        
!       VOCMX   = U1
!       VOCMY   = U2
!       HANG    = Q5
!       KANG    = Q4
!       AANG    = Q3
!       HANGVEL = U5
!       KANGVEL = U4
!       AANGVEL = U3
      
!       Q5 = 1.5D0*PI - HANG
!       Q4 = KANG + Q5 - PI
!       Q3 = PI + Q4 - AANG
      
!       U5 = -HANGVEL
!       U4 = KANGVEL + U5
!       U3 = U4 - AANGVEL
!       U1 = VOCMX + ((L5*MC+L6*MD)*SIN(Q5)*U5+(L3*MB+L4*MC+L4*MD)*SIN(Q4)
!      &*U4+(L1*MA+L2*MB+L2*MC+L2*MD)*SIN(Q3)*U3)/(MA+MB+MC+MD)
!       U2 = VOCMY - ((L5*MC+L6*MD)*COS(Q5)*U5+(L3*MB+L4*MC+L4*MD)*COS(Q4)
!      &*U4+(L1*MA+L2*MB+L2*MC+L2*MD)*COS(Q3)*U3)/(MA+MB+MC+MD)
     
      RETURN
      END SUBROUTINE
      
C*****************************************************************************
C**                                                                         **
C** PURPOSE  Solves a set of first order ordinary differential equations    **
C**          of the form dy(i)/dt = F(t,y(1), ..., y(numeqns) (i = 1,       **
C**          ..., numeqns)                                                  **
C**                                                                         **
C** INPUT                                                                   **
C**    eqns: Subroutine that evaluates dy(i)/dt (i = 1, ..., numeqns), the  **
C**          first derivatives of y(1), ..., y(numeqns) with respect to t   **
C**                                                                         **
C** numeqns: The number of differential equations to be solved              **
C**                                                                         **
C**       y: One-dimensional array whose elements are y(1), ..., y(numeqns) **
C**                                                                         **
C**       t: Independent variable                                           **
C**                                                                         **
C** integstp: Maximum integration stepsize                                  **
C**                                                                         **
C**  abserr: Allowable absolute error in y(i)  (i=1, ..., numeqns)          **
C**                                                                         **
C**  relerr: Allowable relative error in y(i)  (i=1, ..., numeqns)          **
C**                                                                         **
C**     com: When com = 2, the Kutta-Merson algorithm (L. Fox, Numerical    **
C**          Solutions of Ordinary and Partial Differential Equations,      **
C**          Palo Alto: Addison-Wesley, 1962, pp. 24-25) is employed to     **
C**          perform the numerical solution of the differential equations.  **
C**          Accordingly, dy(i)/dt (i = 1, ..., numeqns) are evaluated at   **
C**          every integration boundary, including those at Tinitial,       **
C**          Tfinal, and ones created when integstp is halved to satisfy    **
C**          the requirements imposed by abserr and relerr.  Integration    **
C**          is self-starting at each boundary, and the occurrence, at      **
C**          boundaries, of discontinuities in derivatives does not lead    **
C**          to failure of the integration process.                         **
C**                                                                         **
C**          When com = 1, a modified form of the Kutta-Merson algorithm    **
C**          is employed.  It is nearly 20% faster than the one used when   **
C**          com = 2 because no recalculation of derivatives at inte-       **
C**          gration boundaries between Tinitial and Tfinal takes place.    **
C**          Integration is self-starting at Tinitial and Tfinal only.      **
C**          Integration may fail if any of dy(i)/dt (i = 1, ..., numeqns)  **
C**          is discontinuous between Tinitial and Tfinal.                  **
C**                                                                         **
C**          When com = 0, the function eqns is called and dy(i)/dt         **
C**          (i = 1, ..., numeqns) are evaluated, but no integration        **
C**          is performed.                                                  **
C**                                                                         **
C** OUTPUT                                                                  **
C**          The value of t+integstp is returned in t, and the values of    **
C**          y(i) at t+integstp are returned in y.                          **
C**                                                                         **
C** SOURCE                                                                  **
C**          Copyright 1995 by Paul C. Mitiguy, Thomas R. Kane, David A.    **
C**          Levinson, and David B. Schaechter.  Permission is granted      **
C**          to copy, modify, and distribute this subroutine, provided      **
C**          that this copyright notice appear.                             **
C**                                                                         **
C*****************************************************************************
      SUBROUTINE KUTTA (EQNS,NUMY,Y,T,INTEGSTP,ABSERR,RELERR,COM,*)
      EXTERNAL         EQNS
      INTEGER          NUMY, COM, NUMCUTS, I
      LOGICAL          STEPDBL, ENTRY
      DOUBLE PRECISION Y(NUMY), F0, F1, F2, Y1, Y2
      DOUBLE PRECISION T, INTEGSTP, ABSERR, RELERR, ERROR, TEST
      DOUBLE PRECISION TFINAL, TT, HC, H, H2, H3, H6, H8
      COMMON/CKUTTA/   F0(100),F1(100),F2(100),Y1(100),Y2(100)
      ! DATA             HC, NUMCUTS / 0.0D0, 20 /

      HC = 0.0D0
      NUMCUTS = 20
C**   If COM=0, call EQNS subroutine and return.
      IF( COM .EQ. 0) THEN
        CALL EQNS(T, Y, F0, 1)
        RETURN
      ENDIF

C**   Check for initial entry and adjust current value of stepsize.
      IF(NUMY .EQ. 0) THEN
        HC = INTEGSTP
        RETURN
      ENDIF
      IF(INTEGSTP .EQ. 0) RETURN 1
      IF(HC*INTEGSTP .LT. 0) HC = -HC
      IF(HC .EQ. 0)          HC = INTEGSTP

C**   Set local variables
      H = HC
      TT = T + H
      TFINAL = T + INTEGSTP
      T  = TFINAL
      ENTRY = .TRUE.

C**   Check round-off problems.
100   IF( TT+H .EQ. TT ) THEN
        T = TT
C         WRITE(*,2010) H, T
        CALL EQNS(T, Y, F0, 0)
        RETURN 1
      ENDIF
C**   Main Kutta-Merson step
      H2 = H * 0.5D0
      H3 = H / 3.0D0
      H6 = H / 6.0D0
      H8 = H * 0.125D0
      IF( COM .EQ. 2 .OR. ENTRY )  CALL EQNS(TT-H, Y, F0, 1)
      ENTRY = .FALSE.
      DO 110  I=1,NUMY
110     Y1(I) = Y(I) + H3*F0(I)
      CALL EQNS(TT-2.0*H3, Y1, F1, 0)
      DO 120  I=1,NUMY
120     Y1(I) = Y(I) + H6*(F0(I) + F1(I))
      CALL EQNS(TT-2.0*H3, Y1, F1, 0)
      DO 130  I=1,NUMY
130     Y1(I) = Y(I) + H8*(F0(I) + 3.0D0*F1(I) )
      CALL EQNS(TT-H2,     Y1, F2, 0)
      DO 140  I=1,NUMY
140     Y1(I) = Y(I) + H2*(F0(I) - 3.0D0*F1(I)+ 4.0D0*F2(I) )
      CALL EQNS(TT,        Y1, F1, 0)
      DO 150  I=1,NUMY
150     Y2(I) = Y(I) + H6*(F0(I) +  4.0D0*F2(I) + F1(I) )
C**   Assume that step needs to be doubled.  Check error criterion
      STEPDBL = .TRUE.
      DO 160 I=1,NUMY
        ERROR = DABS(Y1(I) - Y2(I)) * 0.2D0
        TEST  = DABS(Y1(I)) * RELERR
        IF(ERROR .GE. TEST .AND. ERROR .GE. ABSERR) THEN
          HC = H2
          H  = HC
          TT = TT - H2
          NUMCUTS = NUMCUTS - 1
          IF(NUMCUTS .GE. 0) GO TO 100
          T = TT - H
C           WRITE(*,2000) T
          CALL EQNS(T, Y, F0, 0)
          RETURN 1
        ENDIF
      IF(STEPDBL .AND. 64.0D0*ERROR .GT. TEST
     &           .AND. 64.0D0*ERROR .GT. ABSERR) STEPDBL=.FALSE.
160   CONTINUE
      DO 170  I = 1,NUMY
170     Y(I) = Y2(I)
C**   Double the STEPSIZE, maybe.
      IF( STEPDBL .AND. DABS(H+H) .LE. DABS(INTEGSTP) .AND.
     &     DABS(TT+H+H) .LE. DABS(TFINAL) )  THEN
        HC = H + H
        H  = HC
        NUMCUTS = NUMCUTS + 1
      ENDIF
      IF( TT .EQ. TFINAL ) THEN
        CALL EQNS(TFINAL, Y, F0, 2)
        RETURN
      ENDIF
      TT = TT + H
      IF( (H .GT. 0 .AND. TT .GT. TFINAL-0.1D0*H) .OR.
     &    (H .LT. 0 .AND. TT .LT. TFINAL-0.1D0*H)  )  THEN
        H  = TFINAL - (TT-H)
        TT = TFINAL
      ENDIF
      IF( COM .EQ. 1 ) THEN
        DO 180  I = 1,NUMY
180       F0(I) = F1(I)
      ENDIF
      GOTO 100 

2000  FORMAT(/1X,'THE STEPSIZE HAS BEEN HALVED TOO MANY TIMES; T = ',
     &1PD12.4,/1X,'ERROR: NUMERICAL INTEGRATION FAILED TO CONVERGE.',//)
2010  FORMAT(/1X,'THE STEPSIZE OF ',1PD22.14,' IS TOO SMALL RELATIVE ', 
     &'TO THE TERMINAL TIME OF',/1PD22.14,'.  INTEGRATION HALTED BECA',
     &'USE OF NUMERICAL ROUND-OFF.',/,'THE STEPSIZE MAY HAVE BEEN CUT ',
     &'TOO MANY TIMES.'//)
      END SUBROUTINE
      
C**************************************************************************** 
C**                                                                        ** 
C** PURPOSE  The matrix equation a x = b is solved for x, where a is an    ** 
C**          n by n matrix, and x and b are n by 1 matrices.               ** 
C**                                                                        ** 
C** INPUT                                                                  **
C**       N: n                                                             ** 
C**                                                                        ** 
C**       A: an N by N double precision array whose elements are those     **      
C**          of the matrix a                                               ** 
C**                                                                        ** 
C**       B: an N by 1 double precision array whose elements are those     **      
C**          of the matrix b                                               ** 
C**                                                                        ** 
C** OUTPUT                                                                 ** 
C**       X: an N by 1 double precision array whose elements are those     **
C**          of the matrix x                                               ** 
C**                                                                        ** 
C**************************************************************************** 
        SUBROUTINE SOLVE(N, A, B, X)
        IMPLICIT DOUBLE PRECISION (A - Z)
        INTEGER N,IPS(100),I,J,K,IP,KP,KP1,NM1,IDXPIV,IP1,IM1,NP1,IBACK
        DIMENSION A(N,N),SCALES(100),B(N),X(N)

C*************** Beginning of LU decomposition of A ********************
        ZERO = 0.0D0
        DO 5 I=1,N
        IPS(I) = I
        ROWNRM = 0.0D0
        DO 20 J=1,N
        ROWNRM = DMAX1(ROWNRM,DABS(A(I,J)))
   20   CONTINUE
        IF(ROWNRM.EQ.ZERO) GOTO 500
        SCALES(I) = 1.0D0 / ROWNRM
    5   CONTINUE
        NM1 = N-1
        DO 17 K=1,NM1
        BIG = 0.0D0
        DO 11 I=K,N
        IP = IPS(I)
        SIZE = DABS(A(IP,K))*SCALES(IP)
        IF(SIZE .LE. BIG) GO TO 11
        BIG = SIZE
        IDXPIV = I
   11   CONTINUE
        IF(BIG .EQ. ZERO) GOTO 520
        IF(IDXPIV .EQ. K) GO TO 15
        J = IPS(K)
        IPS(K) = IPS(IDXPIV)
        IPS(IDXPIV) = J
   15   KP = IPS(K)
        PIVOT = A(KP,K)
        KP1 = K+1
        DO 16 I=KP1,N
        IP = IPS(I)
        EM = A(IP,K)/PIVOT
        A(IP,K) = EM
        DO 16 J = KP1,N
        A(IP,J) = A(IP,J) - EM*A(KP,J)
   16   CONTINUE
   17   CONTINUE
        IF(A(IPS(N),N) .EQ. ZERO) GOTO 520

C**     Note: The LU decomposition of A is returned in A
C***************** Beginning of back substitution **********************
        NP1 = N+1
        X(1) = B(IPS(1))
        DO 2 I=2,N
        IP = IPS(I)
        IM1 = I-1
        SUM = 0.0D0
        DO 1 J=1,IM1
        SUM = SUM + A(IP,J)*X(J)
    1   CONTINUE
        X(I) = B(IP) - SUM
    2   CONTINUE
        X(N) = X(N)/A(IPS(N),N)
        DO 4 IBACK=2,N
        I = NP1-IBACK
        IP = IPS(I)
        IP1 = I+1
        SUM = 0.0D0
        DO 3 J=IP1,N
        SUM = SUM + A(IP,J)*X(J)
    3   CONTINUE
    4   X(I) = (X(I)-SUM)/A(IP,I)
        RETURN

  500  WRITE(*,600) I
       STOP
  520  WRITE(*,620)
       STOP
  600  FORMAT(/1X,'ALL ELEMENTS IN ROW ',I3,'   OF COEF ARE ZEROS'/)
  620  FORMAT(/1X,'A PIVOT ELEMENT ENCOUNTERED IN THE DECOMPOSITION',
     & ' OF COEF IS ZERO',/15X,'COEFFICIENT MATRIX IS SINGULAR')
        END SUBROUTINE

c**************************************************************************
c
c     subroutine: evalspline
c
c     evaluates splines at time t
c
c     calculates: position
c                 velocity 
c                 acceleration
c
c**************************************************************************
c
c
      subroutine evalspline(t,nf,kk,ccswlegx,ccswlegy,swlegxp,swlegxv,
     &                      swlegxa,swlegyp,swlegyv,swlegya)

      IMPLICIT         DOUBLE PRECISION (A - Z)
      DIMENSION        swlegx(3),swlegy(3)
c     
      integer nf
c
      call VALQ3(swlegx,t,ccswlegx,nf,kk)
      call VALQ3(swlegy,t,ccswlegy,nf,kk)
c
      swlegxp = swlegx(1)
      swlegxv = swlegx(2)
      swlegxa = swlegx(3)
c
      swlegyp = swlegy(1)
      swlegyv = swlegy(2)
      swlegya = swlegy(3)
c
      return
      end
c
***************************************************************
c
      Subroutine VALQ3(SPY,T,CCF,NU,k)

      parameter (n = 1000)
c
      double precision CCF(6,n),k(n),T,SPY(3),QSPLIN,QDSPLN
c
      integer NU

      SPY(1)=QSPLIN(NU,k,CCF,T)
      SPY(2)=QDSPLN(NU,k,CCF,T,1)
      SPY(3)=QDSPLN(NU,k,CCF,T,2)

      return
      end
c
c********************************************************************
c
      DOUBLE PRECISION FUNCTION QSPLIN (N,X,COEF,T)
      IMPLICIT DOUBLE PRECISION(A-H,P-Z)
C
C       FUNCTION TO EVALUATE THE SPLINE AT THE POINT T.
C
      DIMENSION COEF(6,N),X(N)
      DO1I=1,N
      IF(T.LT.X(I)) GOTO2
    1 CONTINUE
      I=N+1
    2 I=I-1
      IF(I.EQ.0) GOTO3
      TT=T-X(I)
      QSPLIN=COEF(6,I)
      DO4J=1,5
    4 QSPLIN=QSPLIN*TT+COEF(6-J,I)
      RETURN
    3 QSPLIN=(COEF(3,1)*(T-X(1))+COEF(2,1))*(T-X(1))+COEF(1,1)
      RETURN
      END
c
C*******************************************************************
c
      DOUBLE PRECISION FUNCTION QDSPLN (N,X,COEF,T,K)
      IMPLICIT DOUBLE PRECISION(A-H,P-Z)
      DIMENSION COEF(6,N),X(N),FACT(15)
      DATA FACT/5.,4.,3.,2.,1.,20.,12.,6.,2.,60.,24.,6.,120.,24.,120./
C
C       FUNCTION TO EVALUATE DERIVATIVES OF QUINTIC SPLINE.
C
      DO1I=1,N
      IF(T.LT.X(I)) GOTO2
    1 CONTINUE
      I=N+1
    2 I=I-1
      IF(I.EQ.0) GOTO3
      TT=T-X(I)
      IF(K.GT.0) GOTO5
    9 QDSPLN=QSPLIN(N,X,COEF,T)
      RETURN
    5 IF(K.GE.6) GOTO8
      KCOL=(K-1)*(12-K)/2+1
      R=COEF(6,I)*FACT(KCOL)
      IF(K.EQ.5) GOTO6
      JJ=5-K
      DO4J=1,JJ
    4 R=R*TT+COEF(6-J,I)*FACT(KCOL+J)
    6 QDSPLN=R
      RETURN
    3 TT=T-X(1)
      IF(K.LE.0) GOTO9
      IF(K.GE.3) GOTO8
      QDSPLN=COEF(3,1)*2.0
      IF(K.EQ.2) RETURN
      QDSPLN=QDSPLN*TT+COEF(2,1)
      RETURN
    8 QDSPLN=0.0
      RETURN
      END