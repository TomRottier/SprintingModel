C Six segment model evaluation script
C Minimises cost function measuring RMSE between orientation and 
C configuration angles along with CoM kinematics
C
C Parameters to optimise:
C   - Activation timings: 42, 7 per torque generator (6)
C   - K1,K2,K3,K4:        stiffness and damping parameters for
C                         viscoelastic contact model
C
C
C   Tom Rottier 2020
C***********************************************************************
      PROGRAM MAIN
      IMPLICIT         DOUBLE PRECISION (A - Z)
C** Model variables
      INTEGER          ILOOP,PRINTINT
      INTEGER          I,J,NACTP,NROW,NCOL
      PARAMETER        (NACTP=7)
      CHARACTER        MESSAGE(99)
      DIMENSION        Y(500,9)
      DIMENSION        TT(500),CCHIP(6,500),CCKNEE(6,500),CCHAT(6,500)
      DIMENSION        HETQP(10),HFTQP(10),KETQP(10),KFTQP(10),AETQP(10)
     &,AFTQP(10),HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(NACTP)
     &,AEACTP(NACTP),AFACTP(NACTP)
      COMMON/CONSTNTS/ G,IA,IB,IC,ID,IE,IF,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6
     &,L7,MA,MB,MC,MD,ME,MF
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,U1I,U2I,U3I,U4I,U5I,U6I
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(6,6),RHS(6)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW
      COMMON/DATAIN  / Y,AERIALTIME,SWINGTIME
C** SPAN variables
      INTEGER N, NEPS
      PARAMETER(N=46,NEPS=4)
      DOUBLE PRECISION  LB(N), UB(N), X(N), XOPT(N), C(N), VM(N),
     &                  FSTAR(NEPS), XP(N), T, EPS, RT, FOPT
      INTEGER  NACP(N), WORK(N), NS, NT, NFCNEV, IER, ISEED1, ISEED2,
     &         MAXEVL, IPRINT, NACC, NOBDS
      LOGICAL  MAX
      EXTERNAL SPAN

C**   Open input file
      OPEN(UNIT=20, FILE='6segSprint.in', STATUS='OLD')

C**   Read message from input file
      READ(20,7000,END=7100,ERR=7101) (MESSAGE(ILOOP),ILOOP = 1,99)

C**   Read values of constants from input file
      READ(20,7010,END=7100,ERR=7101) G,IA,IB,IC,ID,IE,IF,K1,K2,K3,K4,L1
     &,L2,L3,L4,L5,L6,L7,MA,MB,MC,MD,ME,MF

C**   Read the initial value of each variable from input file
      READ(20,7010,END=7100,ERR=7101) Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,U1I,U2I,U3
     &I,U4I,U5I,U6I

C**   Read integration parameters from input file
      READ(20,7011,END=7100,ERR=7101) TINITIAL,TFINAL,INTEGSTP,PRINTINT,
     &ABSERR,RELERR
      CLOSE(UNIT=20)

C** Read torque parameters
      OPEN(UNIT=30, FILE='torque.in', STATUS='OLD')
      READ(30, 7200, ERR=7210) HETQP,KETQP,AETQP,HFTQP,KFTQP,AFTQP
      CLOSE(UNIT=30)

C** Read activation parameters
      OPEN(UNIT=31, FILE='activation.in', STATUS='OLD')
      READ(31, 7300, ERR=7310) HEACTP,KEACTP,AEACTP,HFACTP,KFACTP,AFACTP
      CLOSE(UNIT=31)

C** Read spline coefficients for angles and HAT CoM location
      OPEN(UNIT=32, FILE='angles_coef.csv', STATUS='OLD')
      READ(32,*) NROW
      READ(32,*) (TT(I), I=1, NROW)
      READ(32,*) ((CCHIP(J,I), J=1, 6), I=1, NROW)
      READ(32,*) ((CCKNEE(J,I), J=1, 6), I=1, NROW)
      CLOSE(UNIT=32)
      OPEN(UNIT=33, FILE='HAT_coef.csv', STATUS='OLD')
      READ(33,*)
      READ(33,*)
      READ(33,*) ((CCHAT(J,I), J=1, 6), I=1, NROW)
      CLOSE(UNIT=33)

C** Read matching data
      OPEN(UNIT=34, FILE='matchingData2.csv', STATUS='OLD')
      READ(34,*) NROW, NCOL
      READ(34,*)
      READ(34,*, ERR=7410) ((Y(I,J), J=1, NCOL), I=1, NROW)
      CLOSE(UNIT=34)
      AERIALTIME = 0.132D0
      SWINGTIME  = 0.374D0

C**   Convert to generalised coordinates
      CALL INITCOND()

C**   Set input parameters for SPAN
C*    Recommended values: NT = 100, NS = even multiple of ncpu
      MAX = .FALSE.
      EPS = 1.0D-03
      RT = 0.75
      ISEED1 = 7
      ISEED2 = 8
      NS = 24
      NT = 20
      MAXEVL = 100000000
      IPRINT = 1

C** Set upper and lower bounds on parameters
      DO I = 1, N-4, NACTP
        LB(I)    = 0.0D0
        LB(I+1)  = 0.0D0
        LB(I+2)  = 0.1D0
        LB(I+3)  = 0.0D0
        LB(I+4)  = 0.0D0
        LB(I+5)  = 0.1D0
        LB(I+6)  = 0.0D0
      ENDDO

      DO I = 1, N-4, NACTP
        UB(I)    = 1.1D0
        UB(I+1)  = 0.1D0
        UB(I+2)  = 0.3D0
        UB(I+3)  = 1.1D0
        UB(I+4)  = 0.1D0
        UB(I+5)  = 0.3D0
        UB(I+6)  = 1.1D0
      ENDDO

      LB(N-3) = 1.0D0
      LB(N-2) = 1.0D0
      LB(N-1) = 10000.0D0
      LB(N)   = 10000.0D0
      UB(N-3) = 1000.0D0
      UB(N-2) = 1000.0D0
      UB(N-1) = 1000000.0D0
      UB(N)   = 1000000.0D0

C***  Set input values of the input/output parameters
      T = 5.0

      DO 20, I = 1, N
         VM(I) = UB(I) - LB(I)
         C(I)  = 2.0D0
         X(I) = LB(I) + VM(I)*0.5D0
20    CONTINUE

      X(1:7)   = HEACTP
      X(8:14)  = KEACTP
      X(15:21) = AEACTP
      X(22:28) = HFACTP
      X(29:35) = KFACTP
      X(36:42) = AFACTP
      X(43) = K1
      X(44) = K2
      X(45) = K3
      X(46) = K4

C**** Call SPAN
      CALL SPAN(N,X,MAX,RT,EPS,NS,NT,NEPS,MAXEVL,LB,UB,C,IPRINT,ISEED1,
     &        ISEED2,T,VM,XOPT,FOPT,NACC,NFCNEV,NOBDS,IER,
     &        FSTAR,XP,NACP,WORK)
      
      ! DO I = 1, 10
      ! CALL FCN(N,X,COST)
      ! PRINT*, COST
      ! ENDDO

      STOP
7000  FORMAT(//,99A1,///)
7010  FORMAT( 1000(59X,E30.0,/) )
7011  FORMAT( 3(59X,E30.0,/), 1(59X,I30,/), 2(59X,E30.0,/) )
      STOP
7100  WRITE(*,*) 'Premature end of file while reading 6segsprint.in '
7101  WRITE(*,*) 'Error while reading file 6segsprint.in'
      STOP
7200  FORMAT(//, 6(///, 10(8X, F7.2, /)))
7210  WRITE(*,*) 'Error reading torque parameters'
      STOP
7300  FORMAT(//, 6(///, 7(5X, G30.10, /)))
7310  WRITE(*,*) 'Error reading activation parameters'
7410  WRITE(*,*) 'Error while reading matching data'
      STOP
      END PROGRAM MAIN


C***********************************************************************
      SUBROUTINE FCN(N,X,COST)
C Simulates the model with input parameters then calculates cost
C
C Inputs:
C    - N: number of parameters to optimise
C    - X: input parameters
C
C Outputs:
C    - COST:   cost function for given parameters
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A - Z)
      INTEGER          N,IDX,NACTP,NROW
      LOGICAL          EXIT
      PARAMETER        (NACTP=7)
      EXTERNAL         EQNS1
      DIMENSION        VAR(12)
      DIMENSION        X(N),Y(500,9)
      DIMENSION        TT(500),CCHIP(6,500),CCKNEE(6,500),CCHAT(6,500)
      DIMENSION        HETQP(10),HFTQP(10),KETQP(10),KFTQP(10),AETQP(10)
     &,AFTQP(10),HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(NACTP)
     &,AEACTP(NACTP),AFACTP(NACTP)
      COMMON/CONSTNTS/ G,IA,IB,IC,ID,IE,IF,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6
     &,L7,MA,MB,MC,MD,ME,MF
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,U1I,U2I,U3I,U4I,U5I,U6I
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,Q6,U1,U2,U3,U4,U5,U6
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,HANG,HANGVEL,HETOR,
     &HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,PECM,PX,PY,RX,RY,
     &SHANG,SHANGVEL,SHTOR,SKANG,SKANGVEL,SKTOR,TE,U7,U8,Q1p,Q2p,Q3p,Q4p
     &,Q5p,Q6p,U1p,U2p,U3p,U4p,U5p,U6p,AEACT,AECCANG,AECCANGVEL,AESECANG
     &,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,DA,EA,F
     &S,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,HFCC
     &ANGVEL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,KESE
     &CANGVEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,DAp,EAp,FSp,
     &DApp,EApp,FSpp,POCMX,POCMY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP
     &4X,POP4Y,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POPFX,POPFY,VOCMX,VOC
     &MY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(6,6),RHS(6)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW
      COMMON/DATAIN  / Y,AERIALTIME,SWINGTIME

C** Initialise parameters
      HEACTP = X(1:7)
      KEACTP = X(8:14)
      AEACTP = X(15:21)
      HFACTP = X(22:28)
      KFACTP = X(29:35)
      AFACTP = X(36:42)
      K1 = X(43)
      K2 = X(44)
      K3 = X(45)
      K4 = X(46)

C** Initialise variables
      Q1 = Q1I
      Q2 = Q2I
      Q3 = Q3I
      Q4 = Q4I
      Q5 = Q5I
      Q6 = Q6I
      U1 = U1I
      U2 = U2I
      U3 = U3I
      U4 = U4I
      U5 = U5I
      U6 = U6I 

C**   Evaluate constants
      U7 = 0
      U8 = 0

C**   Initialize time, print counter, variables array for integrator
      EXIT = .FALSE.
      T      = TINITIAL
      VAR(1) = Q1
      VAR(2) = Q2
      VAR(3) = Q3
      VAR(4) = Q4
      VAR(5) = Q5
      VAR(6) = Q6
      VAR(7) = U1
      VAR(8) = U2
      VAR(9) = U3
      VAR(10) = U4
      VAR(11) = U5
      VAR(12) = U6

C** Initialise variables for COST
      IDX = 2
      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCHAT,FS,FSp,FSpp)
      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCHIP,DA,DAp,DApp)
      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCKNEE,EA,EAp,EApp)
      DA   = DA  *DEGtoRAD 
      DAp  = DAp *DEGtoRAD 
      DApp = DApp*DEGtoRAD 
      EA   = EA  *DEGtoRAD 
      EAp  = EAp *DEGtoRAD 
      EApp = EApp*DEGtoRAD 
      CMYTD = Q2 - (ME*(L3-L4)*SIN(DA-EA-Q3)+(L6*ME-MD*(L5-L6))*SIN(DA-Q
     &3)-MF*FS*SIN(Q3)-(L5*MC+L6*MD+L6*ME+L6*MF)*SIN(Q3-Q6)-(L3*MB+L4*MC
     &+L4*MD+L4*ME+L4*MF)*SIN(Q3-Q5-Q6)-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L
     &2*MF)*SIN(Q3-Q4-Q5-Q6))/(MA+MB+MC+MD+ME+MF)
      VCMXI = U1 + (MF*FSp*COS(Q3)-MF*FS*SIN(Q3)*U3-(L5*MC+L6*MD+L6*ME+L
     &6*MF)*SIN(Q3-Q6)*(U3-U6)-(L6*ME-MD*(L5-L6))*SIN(DA-Q3)*(DAp-U3-U7)
     &-ME*(L3-L4)*SIN(DA-EA-Q3)*(DAp-EAp-U3-U7-U8)-(L3*MB+L4*MC+L4*MD+L4
     &*ME+L4*MF)*SIN(Q3-Q5-Q6)*(U3-U5-U6)-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME
     &+L2*MF)*SIN(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6))/(MA+MB+MC+MD+ME+MF)

      HATS = 0.0D0
      HIPS = 0.0D0
      KNEES = 0.0D0
      ANKLES  = 0.0D0

C** Initialise torques for integration
      CALL UPDATE(T)

C**   Initalize numerical integrator with call to EQNS1 at T=TINITIAL
      CALL KUTTA(EQNS1, 12, VAR, T, INTEGSTP, ABSERR, RELERR, 0, *5920)

C**   Check exit conditions
5900  IF(TFINAL.GE.TINITIAL.AND.T+.01D0*INTEGSTP.GE.TFINAL) EXIT=.TRUE.
      IF(TFINAL.LE.TINITIAL.AND.T+.01D0*INTEGSTP.LE.TFINAL) EXIT=.TRUE.
      IF (Q2 .GT. 0.0D0) EXIT = .TRUE.

      IF (EXIT) THEN
        IDX = IDX - 1
        HATJ   = HATS   / IDX
        HIPJ   = HIPS   / IDX
        KNEEJ  = KNEES  / IDX
        ANKLEJ = ANKLES / IDX
        CMYTO = Q2 - (ME*(L3-L4)*SIN(DA-EA-Q3)+(L6*ME-MD*(L5-L6))*SIN(DA
     &  -Q3)-MF*FS*SIN(Q3)-(L5*MC+L6*MD+L6*ME+L6*MF)*SIN(Q3-Q6)-(L3*MB+L
     &  4*MC+L4*MD+L4*ME+L4*MF)*SIN(Q3-Q5-Q6)-(L1*MA+L2*MB+L2*MC+L2*MD+L
     &  2*ME+L2*MF)*SIN(Q3-Q4-Q5-Q6))/(MA+MB+MC+MD+ME+MF)
        VCMXF = U1 + (MF*FSp*COS(Q3)-MF*FS*SIN(Q3)*U3-(L5*MC+L6*MD+L6*ME
     &  +L6*MF)*SIN(Q3-Q6)*(U3-U6)-(L6*ME-MD*(L5-L6))*SIN(DA-Q3)*(DAp-U3
     &  -U7)-ME*(L3-L4)*SIN(DA-EA-Q3)*(DAp-EAp-U3-U7-U8)-(L3*MB+L4*MC+L4
     &  *MD+L4*ME+L4*MF)*SIN(Q3-Q5-Q6)*(U3-U5-U6)-(L1*MA+L2*MB+L2*MC+L2*
     &  MD+L2*ME+L2*MF)*SIN(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6))/(MA+MB+MC+MD+ME+
     &  MF)
        VCMYF = U2 - ((L6*ME-MD*(L5-L6))*COS(DA-Q3)*(DAp-U3-U7)+ME*(L3-L
     &  4)*COS(DA-EA-Q3)*(DAp-EAp-U3-U7-U8)-MF*FSp*SIN(Q3)-MF*FS*COS(Q3)
     &  *U3-(L5*MC+L6*MD+L6*ME+L6*MF)*COS(Q3-Q6)*(U3-U6)-(L3*MB+L4*MC+L4
     &  *MD+L4*ME+L4*MF)*COS(Q3-Q5-Q6)*(U3-U5-U6)-(L1*MA+L2*MB+L2*MC+L2*
     &  MD+L2*ME+L2*MF)*COS(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6))/(MA+MB+MC+MD+ME+
     &  MF)
        DS = CMYTD - CMYTO

C** Check if quadratic has solution
C** If VCMYF negative then a negative aerial is mathematically possible
        IF (VCMYF .LT. 0.0D0) THEN
          COST = 15000.0D0
          RETURN
        ENDIF
        DISCRIM = VCMYF**2 - (4.0D0)*(G/2.0D0)*(-DS)
        IF (DISCRIM .LT. 0.0D0) THEN
          COST = 10000.0D0
          RETURN
        ELSE
          TA = (-VCMYF-SQRT(DISCRIM)) / G
        ENDIF
  
        TSW = T + 2.0D0*TA
        TAJ = ABS(TA - AERIALTIME)
        TSWJ = ABS(TSW - SWINGTIME)
        VCMJ = ABS(VCMXF-VCMXI)
  
        COST = 10*HATJ+HIPJ+KNEEJ+ANKLEJ+1000.0D0*TAJ+100.0D0*VCMJ
      !   COST = TAJ+VCMJ
        IF (T .LT. 0.095D0) COST = 3000.0D0
        RETURN
      ENDIF

C** Integrate      
      CALL KUTTA(EQNS1, 12, VAR, T, INTEGSTP, ABSERR, RELERR, 1, *5920)

C** Update torques after integration
      CALL UPDATE(T)

C** Intermediate cost      
      HATS   = HATS   + (Y(IDX,3) - Q3*RADtoDEG)**2
      HIPS   = HIPS   + (Y(IDX,5) - HANG*RADtoDEG)**2
      KNEES  = KNEES  + (Y(IDX,7) - KANG*RADtoDEG)**2
      ANKLES = ANKLES + (Y(IDX,9) - AANG*RADtoDEG)**2
      IDX = IDX + 1
      GOTO 5900


C**   Print message if numerical integration fails to converge
5920  COST = 20000.0D0

      END SUBROUTINE FCN

C**********************************************************************
      SUBROUTINE       EQNS1(T, VAR, VARp, BOUNDARY)
      IMPLICIT         DOUBLE PRECISION (A - Z)
      INTEGER          BOUNDARY,NROW
      DIMENSION        VAR(*), VARp(*)
      DIMENSION        TT(500),CCHIP(6,500),CCKNEE(6,500),CCHAT(6,500)
      COMMON/CONSTNTS/ G,IA,IB,IC,ID,IE,IF,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6
     &,L7,MA,MB,MC,MD,ME,MF
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,Q6,U1,U2,U3,U4,U5,U6
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,HANG,HANGVEL,HETOR,
     &HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,PECM,PX,PY,RX,RY,
     &SHANG,SHANGVEL,SHTOR,SKANG,SKANGVEL,SKTOR,TE,U7,U8,Q1p,Q2p,Q3p,Q4p
     &,Q5p,Q6p,U1p,U2p,U3p,U4p,U5p,U6p,AEACT,AECCANG,AECCANGVEL,AESECANG
     &,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,DA,EA,F
     &S,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,HFCC
     &ANGVEL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,KESE
     &CANGVEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,DAp,EAp,FSp,
     &DApp,EApp,FSpp,POCMX,POCMY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP
     &4X,POP4Y,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POPFX,POPFY,VOCMX,VOC
     &MY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(6,6),RHS(6)
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW

C**   Update variables after integration step
      Q1 = VAR(1)
      Q2 = VAR(2)
      Q3 = VAR(3)
      Q4 = VAR(4)
      Q5 = VAR(5)
      Q6 = VAR(6)
      U1 = VAR(7)
      U2 = VAR(8)
      U3 = VAR(9)
      U4 = VAR(10)
      U5 = VAR(11)
      U6 = VAR(12)

      Q1p = U1
      Q2p = U2
      Q3p = U3
      Q4p = U4
      Q5p = U5
      Q6p = U6

C** Calculate forces
        IF (Q2 .LT. 0.0D0) THEN
          RY = -K3*Q2 - K4*U2*ABS(Q2)
          RX = (-K1*Q1 - K2*U1)*RY
        ELSE
          RX = 0.0D0
          RY = 0.0D0
        ENDIF     

C** Specified variables
      CALL EVALSPLINE2(T,NROW,TT,CCHAT,FS,FSp,FSpp)
      CALL EVALSPLINE2(T,NROW,TT,CCHIP,DA,DAp,DApp)
      CALL EVALSPLINE2(T,NROW,TT,CCKNEE,EA,EAp,EApp)
      DA   = DA  *DEGtoRAD 
      DAp  = DAp *DEGtoRAD 
      DApp = DApp*DEGtoRAD 
      EA   = EA  *DEGtoRAD 
      EAp  = EAp *DEGtoRAD 
      EApp = EApp*DEGtoRAD 

C** Solve for accelearations
      COEF(1,1) = -MA - MB - MC - MD - ME - MF
      COEF(1,2) = 0
      COEF(1,3) = L1*MA*SIN(Q3-Q4-Q5-Q6) + MB*(L3*SIN(Q3-Q5-Q6)+L2*SIN(Q
     &3-Q4-Q5-Q6)) + MC*(L5*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-
     &Q6)) + MF*(FS*SIN(Q3)+L6*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-
     &Q5-Q6)) + MD*(L6*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+(L5-L6)*SIN(DA-Q3)+L2
     &*SIN(Q3-Q4-Q5-Q6)) + ME*(L6*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-
     &Q4-Q5-Q6)-L6*SIN(DA-Q3)-(L3-L4)*SIN(DA-EA-Q3))
      COEF(1,4) = -(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)*SIN(Q3-Q4-Q5-Q6
     &)
      COEF(1,5) = -L1*MA*SIN(Q3-Q4-Q5-Q6) - MB*(L3*SIN(Q3-Q5-Q6)+L2*SIN(
     &Q3-Q4-Q5-Q6)) - MC*(L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6)) - MD*(L4
     &*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6)) - ME*(L4*SIN(Q3-Q5-Q6)+L2*SIN(
     &Q3-Q4-Q5-Q6)) - MF*(L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6))
      COEF(1,6) = -L1*MA*SIN(Q3-Q4-Q5-Q6) - MB*(L3*SIN(Q3-Q5-Q6)+L2*SIN(
     &Q3-Q4-Q5-Q6)) - MC*(L5*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5
     &-Q6)) - MD*(L6*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6)) - 
     &ME*(L6*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6)) - MF*(L6*S
     &IN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6))
      COEF(2,1) = 0
      COEF(2,2) = -MA - MB - MC - MD - ME - MF
      COEF(2,3) = MD*((L5-L6)*COS(DA-Q3)-L6*COS(Q3-Q6)-L4*COS(Q3-Q5-Q6)-
     &L2*COS(Q3-Q4-Q5-Q6)) - L1*MA*COS(Q3-Q4-Q5-Q6) - MB*(L3*COS(Q3-Q5-Q
     &6)+L2*COS(Q3-Q4-Q5-Q6)) - MC*(L5*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*CO
     &S(Q3-Q4-Q5-Q6)) - MF*(FS*COS(Q3)+L6*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2
     &*COS(Q3-Q4-Q5-Q6)) - ME*(L6*COS(DA-Q3)+L6*COS(Q3-Q6)+L4*COS(Q3-Q5-
     &Q6)+L2*COS(Q3-Q4-Q5-Q6)+(L3-L4)*COS(DA-EA-Q3))
      COEF(2,4) = (L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)*COS(Q3-Q4-Q5-Q6)
      COEF(2,5) = L1*MA*COS(Q3-Q4-Q5-Q6) + MB*(L3*COS(Q3-Q5-Q6)+L2*COS(Q
     &3-Q4-Q5-Q6)) + MC*(L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + MD*(L4*
     &COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + ME*(L4*COS(Q3-Q5-Q6)+L2*COS(Q
     &3-Q4-Q5-Q6)) + MF*(L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6))
      COEF(2,6) = L1*MA*COS(Q3-Q4-Q5-Q6) + MB*(L3*COS(Q3-Q5-Q6)+L2*COS(Q
     &3-Q4-Q5-Q6)) + MC*(L5*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-
     &Q6)) + MD*(L6*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + M
     &E*(L6*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + MF*(L6*CO
     &S(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6))
      COEF(3,1) = L1*MA*SIN(Q3-Q4-Q5-Q6) + MB*(L3*SIN(Q3-Q5-Q6)+L2*SIN(Q
     &3-Q4-Q5-Q6)) + MC*(L5*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-
     &Q6)) + MF*(FS*SIN(Q3)+L6*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-
     &Q5-Q6)) + MD*(L6*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+(L5-L6)*SIN(DA-Q3)+L2
     &*SIN(Q3-Q4-Q5-Q6)) + ME*(L6*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-
     &Q4-Q5-Q6)-L6*SIN(DA-Q3)-(L3-L4)*SIN(DA-EA-Q3))
      COEF(3,2) = MD*((L5-L6)*COS(DA-Q3)-L6*COS(Q3-Q6)-L4*COS(Q3-Q5-Q6)-
     &L2*COS(Q3-Q4-Q5-Q6)) - L1*MA*COS(Q3-Q4-Q5-Q6) - MB*(L3*COS(Q3-Q5-Q
     &6)+L2*COS(Q3-Q4-Q5-Q6)) - MC*(L5*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*CO
     &S(Q3-Q4-Q5-Q6)) - MF*(FS*COS(Q3)+L6*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2
     &*COS(Q3-Q4-Q5-Q6)) - ME*(L6*COS(DA-Q3)+L6*COS(Q3-Q6)+L4*COS(Q3-Q5-
     &Q6)+L2*COS(Q3-Q4-Q5-Q6)+(L3-L4)*COS(DA-EA-Q3))
      COEF(3,3) = -IA - IB - IC - ID - IE - IF - MA*L1**2 - MB*(L2**2+L3
     &**2+2*L2*L3*COS(Q4)) - MC*(L2**2+L4**2+L5**2+2*L2*L4*COS(Q4)+2*L4*
     &L5*COS(Q5)+2*L2*L5*COS(Q4+Q5)) - MF*(L2**2+L4**2+L6**2+FS**2+2*L2*
     &L4*COS(Q4)+2*L4*L6*COS(Q5)+2*L6*FS*COS(Q6)+2*L2*L6*COS(Q4+Q5)+2*L4
     &*FS*COS(Q5+Q6)+2*L2*FS*COS(Q4+Q5+Q6)) - MD*(L2**2+L4**2+L6**2+(L5-
     &L6)**2+2*L2*L4*COS(Q4)+2*L4*L6*COS(Q5)+2*L2*L6*COS(Q4+Q5)-2*L6*(L5
     &-L6)*COS(DA-Q6)-2*L4*(L5-L6)*COS(DA-Q5-Q6)-2*L2*(L5-L6)*COS(DA-Q4-
     &Q5-Q6)) - ME*(L2**2+L4**2+2*L6**2+(L3-L4)**2+2*L6*(L3-L4)*COS(EA)+
     &2*L2*L4*COS(Q4)+2*L4*L6*COS(Q5)+2*L2*L6*COS(Q4+Q5)+2*L6**2*COS(DA-
     &Q6)+2*L4*L6*COS(DA-Q5-Q6)+2*L2*L6*COS(DA-Q4-Q5-Q6)+2*L6*(L3-L4)*CO
     &S(DA-EA-Q6)+2*L4*(L3-L4)*COS(DA-EA-Q5-Q6)+2*L2*(L3-L4)*COS(DA-EA-Q
     &4-Q5-Q6))
      COEF(3,4) = IA + MA*L1**2 + L2*MB*(L2+L3*COS(Q4)) + L2*MC*(L2+L4*C
     &OS(Q4)+L5*COS(Q4+Q5)) + L2*MF*(L2+L4*COS(Q4)+L6*COS(Q4+Q5)+FS*COS(
     &Q4+Q5+Q6)) + L2*MD*(L2+L4*COS(Q4)+L6*COS(Q4+Q5)-(L5-L6)*COS(DA-Q4-
     &Q5-Q6)) + L2*ME*(L2+L4*COS(Q4)+L6*COS(Q4+Q5)+L6*COS(DA-Q4-Q5-Q6)+(
     &L3-L4)*COS(DA-EA-Q4-Q5-Q6))
      COEF(3,5) = IA + IB + MA*L1**2 + MB*(L2**2+L3**2+2*L2*L3*COS(Q4)) 
     &+ MC*(L2**2+L4**2+L4*L5*COS(Q5)+2*L2*L4*COS(Q4)+L2*L5*COS(Q4+Q5)) 
     &+ MF*(L2**2+L4**2+L4*L6*COS(Q5)+2*L2*L4*COS(Q4)+L2*L6*COS(Q4+Q5)+L
     &4*FS*COS(Q5+Q6)+L2*FS*COS(Q4+Q5+Q6)) + MD*(L2**2+L4**2+L4*L6*COS(Q
     &5)+2*L2*L4*COS(Q4)+L2*L6*COS(Q4+Q5)-L4*(L5-L6)*COS(DA-Q5-Q6)-L2*(L
     &5-L6)*COS(DA-Q4-Q5-Q6)) + ME*(L2**2+L4**2+L4*L6*COS(Q5)+2*L2*L4*CO
     &S(Q4)+L2*L6*COS(Q4+Q5)+L4*L6*COS(DA-Q5-Q6)+L2*L6*COS(DA-Q4-Q5-Q6)+
     &L4*(L3-L4)*COS(DA-EA-Q5-Q6)+L2*(L3-L4)*COS(DA-EA-Q4-Q5-Q6))
      COEF(3,6) = IA + IB + IC + MA*L1**2 + MB*(L2**2+L3**2+2*L2*L3*COS(
     &Q4)) + MC*(L2**2+L4**2+L5**2+2*L2*L4*COS(Q4)+2*L4*L5*COS(Q5)+2*L2*
     &L5*COS(Q4+Q5)) + MF*(L2**2+L4**2+L6**2+L6*FS*COS(Q6)+2*L2*L4*COS(Q
     &4)+2*L4*L6*COS(Q5)+L4*FS*COS(Q5+Q6)+2*L2*L6*COS(Q4+Q5)+L2*FS*COS(Q
     &4+Q5+Q6)) + MD*(L2**2+L4**2+L6**2+2*L2*L4*COS(Q4)+2*L4*L6*COS(Q5)+
     &2*L2*L6*COS(Q4+Q5)-L6*(L5-L6)*COS(DA-Q6)-L4*(L5-L6)*COS(DA-Q5-Q6)-
     &L2*(L5-L6)*COS(DA-Q4-Q5-Q6)) + ME*(L2**2+L4**2+L6**2+2*L2*L4*COS(Q
     &4)+2*L4*L6*COS(Q5)+2*L2*L6*COS(Q4+Q5)+L6**2*COS(DA-Q6)+L4*L6*COS(D
     &A-Q5-Q6)+L2*L6*COS(DA-Q4-Q5-Q6)+L6*(L3-L4)*COS(DA-EA-Q6)+L4*(L3-L4
     &)*COS(DA-EA-Q5-Q6)+L2*(L3-L4)*COS(DA-EA-Q4-Q5-Q6))
      COEF(4,1) = -(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)*SIN(Q3-Q4-Q5-Q6
     &)
      COEF(4,2) = (L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)*COS(Q3-Q4-Q5-Q6)
      COEF(4,3) = IA + MA*L1**2 + L2*MB*(L2+L3*COS(Q4)) + L2*MC*(L2+L4*C
     &OS(Q4)+L5*COS(Q4+Q5)) + L2*MF*(L2+L4*COS(Q4)+L6*COS(Q4+Q5)+FS*COS(
     &Q4+Q5+Q6)) + L2*MD*(L2+L4*COS(Q4)+L6*COS(Q4+Q5)-(L5-L6)*COS(DA-Q4-
     &Q5-Q6)) + L2*ME*(L2+L4*COS(Q4)+L6*COS(Q4+Q5)+L6*COS(DA-Q4-Q5-Q6)+(
     &L3-L4)*COS(DA-EA-Q4-Q5-Q6))
      COEF(4,4) = -IA - MA*L1**2 - MB*L2**2 - MC*L2**2 - MD*L2**2 - ME*L
     &2**2 - MF*L2**2
      COEF(4,5) = -IA - MA*L1**2 - L2*MB*(L2+L3*COS(Q4)) - L2*MC*(L2+L4*
     &COS(Q4)) - L2*MD*(L2+L4*COS(Q4)) - L2*ME*(L2+L4*COS(Q4)) - L2*MF*(
     &L2+L4*COS(Q4))
      COEF(4,6) = -IA - MA*L1**2 - L2*MB*(L2+L3*COS(Q4)) - L2*MC*(L2+L4*
     &COS(Q4)+L5*COS(Q4+Q5)) - L2*MD*(L2+L4*COS(Q4)+L6*COS(Q4+Q5)) - L2*
     &ME*(L2+L4*COS(Q4)+L6*COS(Q4+Q5)) - L2*MF*(L2+L4*COS(Q4)+L6*COS(Q4+
     &Q5))
      COEF(5,1) = -L1*MA*SIN(Q3-Q4-Q5-Q6) - MB*(L3*SIN(Q3-Q5-Q6)+L2*SIN(
     &Q3-Q4-Q5-Q6)) - MC*(L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6)) - MD*(L4
     &*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6)) - ME*(L4*SIN(Q3-Q5-Q6)+L2*SIN(
     &Q3-Q4-Q5-Q6)) - MF*(L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6))
      COEF(5,2) = L1*MA*COS(Q3-Q4-Q5-Q6) + MB*(L3*COS(Q3-Q5-Q6)+L2*COS(Q
     &3-Q4-Q5-Q6)) + MC*(L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + MD*(L4*
     &COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + ME*(L4*COS(Q3-Q5-Q6)+L2*COS(Q
     &3-Q4-Q5-Q6)) + MF*(L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6))
      COEF(5,3) = IA + IB + MA*L1**2 + MB*(L2**2+L3**2+2*L2*L3*COS(Q4)) 
     &+ MC*(L2**2+L4**2+L4*L5*COS(Q5)+2*L2*L4*COS(Q4)+L2*L5*COS(Q4+Q5)) 
     &+ MF*(L2**2+L4**2+L4*L6*COS(Q5)+2*L2*L4*COS(Q4)+L2*L6*COS(Q4+Q5)+L
     &4*FS*COS(Q5+Q6)+L2*FS*COS(Q4+Q5+Q6)) + MD*(L2**2+L4**2+L4*L6*COS(Q
     &5)+2*L2*L4*COS(Q4)+L2*L6*COS(Q4+Q5)-L4*(L5-L6)*COS(DA-Q5-Q6)-L2*(L
     &5-L6)*COS(DA-Q4-Q5-Q6)) + ME*(L2**2+L4**2+L4*L6*COS(Q5)+2*L2*L4*CO
     &S(Q4)+L2*L6*COS(Q4+Q5)+L4*L6*COS(DA-Q5-Q6)+L2*L6*COS(DA-Q4-Q5-Q6)+
     &L4*(L3-L4)*COS(DA-EA-Q5-Q6)+L2*(L3-L4)*COS(DA-EA-Q4-Q5-Q6))
      COEF(5,4) = -IA - MA*L1**2 - L2*MB*(L2+L3*COS(Q4)) - L2*MC*(L2+L4*
     &COS(Q4)) - L2*MD*(L2+L4*COS(Q4)) - L2*ME*(L2+L4*COS(Q4)) - L2*MF*(
     &L2+L4*COS(Q4))
      COEF(5,5) = -IA - IB - MA*L1**2 - MB*(L2**2+L3**2+2*L2*L3*COS(Q4))
     & - MC*(L2**2+L4**2+2*L2*L4*COS(Q4)) - MD*(L2**2+L4**2+2*L2*L4*COS(
     &Q4)) - ME*(L2**2+L4**2+2*L2*L4*COS(Q4)) - MF*(L2**2+L4**2+2*L2*L4*
     &COS(Q4))
      COEF(5,6) = -IA - IB - MA*L1**2 - MB*(L2**2+L3**2+2*L2*L3*COS(Q4))
     & - MC*(L2**2+L4**2+L4*L5*COS(Q5)+2*L2*L4*COS(Q4)+L2*L5*COS(Q4+Q5))
     & - MD*(L2**2+L4**2+L4*L6*COS(Q5)+2*L2*L4*COS(Q4)+L2*L6*COS(Q4+Q5))
     & - ME*(L2**2+L4**2+L4*L6*COS(Q5)+2*L2*L4*COS(Q4)+L2*L6*COS(Q4+Q5))
     & - MF*(L2**2+L4**2+L4*L6*COS(Q5)+2*L2*L4*COS(Q4)+L2*L6*COS(Q4+Q5))
      COEF(6,1) = -L1*MA*SIN(Q3-Q4-Q5-Q6) - MB*(L3*SIN(Q3-Q5-Q6)+L2*SIN(
     &Q3-Q4-Q5-Q6)) - MC*(L5*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5
     &-Q6)) - MD*(L6*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6)) - 
     &ME*(L6*SIN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6)) - MF*(L6*S
     &IN(Q3-Q6)+L4*SIN(Q3-Q5-Q6)+L2*SIN(Q3-Q4-Q5-Q6))
      COEF(6,2) = L1*MA*COS(Q3-Q4-Q5-Q6) + MB*(L3*COS(Q3-Q5-Q6)+L2*COS(Q
     &3-Q4-Q5-Q6)) + MC*(L5*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-
     &Q6)) + MD*(L6*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + M
     &E*(L6*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + MF*(L6*CO
     &S(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6))
      COEF(6,3) = IA + IB + IC + MA*L1**2 + MB*(L2**2+L3**2+2*L2*L3*COS(
     &Q4)) + MC*(L2**2+L4**2+L5**2+2*L2*L4*COS(Q4)+2*L4*L5*COS(Q5)+2*L2*
     &L5*COS(Q4+Q5)) + MF*(L2**2+L4**2+L6**2+L6*FS*COS(Q6)+2*L2*L4*COS(Q
     &4)+2*L4*L6*COS(Q5)+L4*FS*COS(Q5+Q6)+2*L2*L6*COS(Q4+Q5)+L2*FS*COS(Q
     &4+Q5+Q6)) + MD*(L2**2+L4**2+L6**2+2*L2*L4*COS(Q4)+2*L4*L6*COS(Q5)+
     &2*L2*L6*COS(Q4+Q5)-L6*(L5-L6)*COS(DA-Q6)-L4*(L5-L6)*COS(DA-Q5-Q6)-
     &L2*(L5-L6)*COS(DA-Q4-Q5-Q6)) + ME*(L2**2+L4**2+L6**2+2*L2*L4*COS(Q
     &4)+2*L4*L6*COS(Q5)+2*L2*L6*COS(Q4+Q5)+L6**2*COS(DA-Q6)+L4*L6*COS(D
     &A-Q5-Q6)+L2*L6*COS(DA-Q4-Q5-Q6)+L6*(L3-L4)*COS(DA-EA-Q6)+L4*(L3-L4
     &)*COS(DA-EA-Q5-Q6)+L2*(L3-L4)*COS(DA-EA-Q4-Q5-Q6))
      COEF(6,4) = -IA - MA*L1**2 - L2*MB*(L2+L3*COS(Q4)) - L2*MC*(L2+L4*
     &COS(Q4)+L5*COS(Q4+Q5)) - L2*MD*(L2+L4*COS(Q4)+L6*COS(Q4+Q5)) - L2*
     &ME*(L2+L4*COS(Q4)+L6*COS(Q4+Q5)) - L2*MF*(L2+L4*COS(Q4)+L6*COS(Q4+
     &Q5))
      COEF(6,5) = -IA - IB - MA*L1**2 - MB*(L2**2+L3**2+2*L2*L3*COS(Q4))
     & - MC*(L2**2+L4**2+L4*L5*COS(Q5)+2*L2*L4*COS(Q4)+L2*L5*COS(Q4+Q5))
     & - MD*(L2**2+L4**2+L4*L6*COS(Q5)+2*L2*L4*COS(Q4)+L2*L6*COS(Q4+Q5))
     & - ME*(L2**2+L4**2+L4*L6*COS(Q5)+2*L2*L4*COS(Q4)+L2*L6*COS(Q4+Q5))
     & - MF*(L2**2+L4**2+L4*L6*COS(Q5)+2*L2*L4*COS(Q4)+L2*L6*COS(Q4+Q5))
      COEF(6,6) = -IA - IB - IC - MA*L1**2 - MB*(L2**2+L3**2+2*L2*L3*COS
     &(Q4)) - MC*(L2**2+L4**2+L5**2+2*L2*L4*COS(Q4)+2*L4*L5*COS(Q5)+2*L2
     &*L5*COS(Q4+Q5)) - MD*(L2**2+L4**2+L6**2+2*L2*L4*COS(Q4)+2*L4*L6*CO
     &S(Q5)+2*L2*L6*COS(Q4+Q5)) - ME*(L2**2+L4**2+L6**2+2*L2*L4*COS(Q4)+
     &2*L4*L6*COS(Q5)+2*L2*L6*COS(Q4+Q5)) - MF*(L2**2+L4**2+L6**2+2*L2*L
     &4*COS(Q4)+2*L4*L6*COS(Q5)+2*L2*L6*COS(Q4+Q5))
      RHS(1) = MF*(COS(Q3)*(FSpp-FS*U3**2)-2*FSp*SIN(Q3)*U3-L6*COS(Q3-Q6
     &)*(U3-U6)**2-L4*COS(Q3-Q5-Q6)*(U3-U5-U6)**2-L2*COS(Q3-Q4-Q5-Q6)*(U
     &3-U4-U5-U6)**2) + MD*((L5-L6)*DApp*SIN(DA-Q3)+(L5-L6)*COS(DA-Q3)*(
     &DAp-U3)**2-L6*COS(Q3-Q6)*(U3-U6)**2-L4*COS(Q3-Q5-Q6)*(U3-U5-U6)**2
     &-L2*COS(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6)**2) - RX - L1*MA*COS(Q3-Q4-Q5-Q
     &6)*(U3-U4-U5-U6)**2 - MB*(L3*COS(Q3-Q5-Q6)*(U3-U5-U6)**2+L2*COS(Q3
     &-Q4-Q5-Q6)*(U3-U4-U5-U6)**2) - MC*(L5*COS(Q3-Q6)*(U3-U6)**2+L4*COS
     &(Q3-Q5-Q6)*(U3-U5-U6)**2+L2*COS(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6)**2) - M
     &E*(L6*DApp*SIN(DA-Q3)+(L3-L4)*(DApp-EApp)*SIN(DA-EA-Q3)+L6*COS(DA-
     &Q3)*(DAp-U3)**2+L6*COS(Q3-Q6)*(U3-U6)**2+L4*COS(Q3-Q5-Q6)*(U3-U5-U
     &6)**2+(L3-L4)*COS(DA-EA-Q3)*(DAp-EAp-U3)**2+L2*COS(Q3-Q4-Q5-Q6)*(U
     &3-U4-U5-U6)**2)
      RHS(2) = MF*(2*FSp*COS(Q3)*U3+SIN(Q3)*(FSpp-FS*U3**2)-L6*SIN(Q3-Q6
     &)*(U3-U6)**2-L4*SIN(Q3-Q5-Q6)*(U3-U5-U6)**2-L2*SIN(Q3-Q4-Q5-Q6)*(U
     &3-U4-U5-U6)**2) + MD*((L5-L6)*DApp*COS(DA-Q3)-L6*SIN(Q3-Q6)*(U3-U6
     &)**2-(L5-L6)*SIN(DA-Q3)*(DAp-U3)**2-L4*SIN(Q3-Q5-Q6)*(U3-U5-U6)**2
     &-L2*SIN(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6)**2) + ME*(L6*SIN(DA-Q3)*(DAp-U3
     &)**2+(L3-L4)*SIN(DA-EA-Q3)*(DAp-EAp-U3)**2-L6*DApp*COS(DA-Q3)-(L3-
     &L4)*(DApp-EApp)*COS(DA-EA-Q3)-L6*SIN(Q3-Q6)*(U3-U6)**2-L4*SIN(Q3-Q
     &5-Q6)*(U3-U5-U6)**2-L2*SIN(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6)**2) - G*MA -
     & G*MB - G*MC - G*MD - G*ME - G*MF - RY - L1*MA*SIN(Q3-Q4-Q5-Q6)*(U
     &3-U4-U5-U6)**2 - MB*(L3*SIN(Q3-Q5-Q6)*(U3-U5-U6)**2+L2*SIN(Q3-Q4-Q
     &5-Q6)*(U3-U4-U5-U6)**2) - MC*(L5*SIN(Q3-Q6)*(U3-U6)**2+L4*SIN(Q3-Q
     &5-Q6)*(U3-U5-U6)**2+L2*SIN(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6)**2)
      RHS(3) = G*MD*((L5-L6)*COS(DA-Q3)-L6*COS(Q3-Q6)-L4*COS(Q3-Q5-Q6)-L
     &2*COS(Q3-Q4-Q5-Q6)) + ME*(L4*L6*SIN(Q5)*(U3-U5-U6)**2+L6**2*SIN(DA
     &-Q6)*(DAp-U3)**2+L6*(L3-L4)*SIN(EA)*(DAp-U3)**2+L2*L4*SIN(Q4)*(U3-
     &U4-U5-U6)**2+L4*L6*SIN(DA-Q5-Q6)*(DAp-U3)**2+L2*L6*SIN(Q4+Q5)*(U3-
     &U4-U5-U6)**2+L2*L6*SIN(DA-Q4-Q5-Q6)*(DAp-U3)**2+L6*(L3-L4)*SIN(DA-
     &EA-Q6)*(DAp-EAp-U3)**2+L4*(L3-L4)*SIN(DA-EA-Q5-Q6)*(DAp-EAp-U3)**2
     &+L2*(L3-L4)*SIN(DA-EA-Q4-Q5-Q6)*(DAp-EAp-U3)**2-L6**2*DApp-L6*(L3-
     &L4)*COS(EA)*DApp-(L3-L4)**2*(DApp-EApp)-L6*(L3-L4)*COS(EA)*(DApp-E
     &App)-L6**2*DApp*COS(DA-Q6)-L4*L6*DApp*COS(DA-Q5-Q6)-L2*L6*DApp*COS
     &(DA-Q4-Q5-Q6)-L6*(L3-L4)*(DApp-EApp)*COS(DA-EA-Q6)-L4*(L3-L4)*(DAp
     &p-EApp)*COS(DA-EA-Q5-Q6)-L2*(L3-L4)*(DApp-EApp)*COS(DA-EA-Q4-Q5-Q6
     &)-L4*L6*SIN(Q5)*(U3-U6)**2-L2*L6*SIN(Q4+Q5)*(U3-U6)**2-L2*L4*SIN(Q
     &4)*(U3-U5-U6)**2-L6**2*SIN(DA-Q6)*(U3-U6)**2-L6*(L3-L4)*SIN(EA)*(D
     &Ap-EAp-U3)**2-L4*L6*SIN(DA-Q5-Q6)*(U3-U5-U6)**2-L6*(L3-L4)*SIN(DA-
     &EA-Q6)*(U3-U6)**2-L2*L6*SIN(DA-Q4-Q5-Q6)*(U3-U4-U5-U6)**2-L4*(L3-L
     &4)*SIN(DA-EA-Q5-Q6)*(U3-U5-U6)**2-L2*(L3-L4)*SIN(DA-EA-Q4-Q5-Q6)*(
     &U3-U4-U5-U6)**2) - ID*DApp - IE*(DApp-EApp) - G*L1*MA*COS(Q3-Q4-Q5
     &-Q6) - G*MB*(L3*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) - G*MC*(L5*COS(
     &Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) - G*MF*(FS*COS(Q3)+L6
     &*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) - G*ME*(L6*COS(D
     &A-Q3)+L6*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)+(L3-L4)*C
     &OS(DA-EA-Q3)) - L2*L3*MB*SIN(Q4)*((U3-U5-U6)**2-(U3-U4-U5-U6)**2) 
     &- MC*(L4*L5*SIN(Q5)*(U3-U6)**2+L2*L5*SIN(Q4+Q5)*(U3-U6)**2+L2*L4*S
     &IN(Q4)*(U3-U5-U6)**2-L4*L5*SIN(Q5)*(U3-U5-U6)**2-L2*L4*SIN(Q4)*(U3
     &-U4-U5-U6)**2-L2*L5*SIN(Q4+Q5)*(U3-U4-U5-U6)**2) - MF*(L4*L6*SIN(Q
     &5)*(U3-U6)**2+L2*L6*SIN(Q4+Q5)*(U3-U6)**2+L2*L4*SIN(Q4)*(U3-U5-U6)
     &**2-2*FS*FSp*U3-2*L6*FSp*COS(Q6)*U3-2*L4*FSp*COS(Q5+Q6)*U3-2*L2*FS
     &p*COS(Q4+Q5+Q6)*U3-L6*SIN(Q6)*(FSpp-FS*U3**2)-L6*FS*SIN(Q6)*(U3-U6
     &)**2-L4*SIN(Q5+Q6)*(FSpp-FS*U3**2)-L2*SIN(Q4+Q5+Q6)*(FSpp-FS*U3**2
     &)-L4*L6*SIN(Q5)*(U3-U5-U6)**2-L4*FS*SIN(Q5+Q6)*(U3-U5-U6)**2-L2*L4
     &*SIN(Q4)*(U3-U4-U5-U6)**2-L2*L6*SIN(Q4+Q5)*(U3-U4-U5-U6)**2-L2*FS*
     &SIN(Q4+Q5+Q6)*(U3-U4-U5-U6)**2) - MD*((L5-L6)**2*DApp+L4*L6*SIN(Q5
     &)*(U3-U6)**2+L2*L6*SIN(Q4+Q5)*(U3-U6)**2+L2*L4*SIN(Q4)*(U3-U5-U6)*
     &*2+L6*(L5-L6)*SIN(DA-Q6)*(DAp-U3)**2+L4*(L5-L6)*SIN(DA-Q5-Q6)*(DAp
     &-U3)**2+L2*(L5-L6)*SIN(DA-Q4-Q5-Q6)*(DAp-U3)**2-L6*(L5-L6)*DApp*CO
     &S(DA-Q6)-L4*(L5-L6)*DApp*COS(DA-Q5-Q6)-L2*(L5-L6)*DApp*COS(DA-Q4-Q
     &5-Q6)-L4*L6*SIN(Q5)*(U3-U5-U6)**2-L2*L4*SIN(Q4)*(U3-U4-U5-U6)**2-L
     &2*L6*SIN(Q4+Q5)*(U3-U4-U5-U6)**2-L6*(L5-L6)*SIN(DA-Q6)*(U3-U6)**2-
     &L4*(L5-L6)*SIN(DA-Q5-Q6)*(U3-U5-U6)**2-L2*(L5-L6)*SIN(DA-Q4-Q5-Q6)
     &*(U3-U4-U5-U6)**2)
      RHS(4) = G*L1*MA*COS(Q3-Q4-Q5-Q6) + G*L2*MB*COS(Q3-Q4-Q5-Q6) + G*L
     &2*MC*COS(Q3-Q4-Q5-Q6) + G*L2*MD*COS(Q3-Q4-Q5-Q6) + G*L2*ME*COS(Q3-
     &Q4-Q5-Q6) + G*L2*MF*COS(Q3-Q4-Q5-Q6) - ATOR - L2*(MF*(2*FSp*COS(Q4
     &+Q5+Q6)*U3+SIN(Q4+Q5+Q6)*(FSpp-FS*U3**2)-L6*SIN(Q4+Q5)*(U3-U6)**2-
     &L4*SIN(Q4)*(U3-U5-U6)**2)+MD*((L5-L6)*DApp*COS(DA-Q4-Q5-Q6)-L6*SIN
     &(Q4+Q5)*(U3-U6)**2-L4*SIN(Q4)*(U3-U5-U6)**2-(L5-L6)*SIN(DA-Q4-Q5-Q
     &6)*(DAp-U3)**2)-L3*MB*SIN(Q4)*(U3-U5-U6)**2-MC*(L5*SIN(Q4+Q5)*(U3-
     &U6)**2+L4*SIN(Q4)*(U3-U5-U6)**2)-ME*(L6*DApp*COS(DA-Q4-Q5-Q6)+(L3-
     &L4)*(DApp-EApp)*COS(DA-EA-Q4-Q5-Q6)+L6*SIN(Q4+Q5)*(U3-U6)**2+L4*SI
     &N(Q4)*(U3-U5-U6)**2-L6*SIN(DA-Q4-Q5-Q6)*(DAp-U3)**2-(L3-L4)*SIN(DA
     &-EA-Q4-Q5-Q6)*(DAp-EAp-U3)**2))
      RHS(5) = KTOR + G*L1*MA*COS(Q3-Q4-Q5-Q6) + G*MB*(L3*COS(Q3-Q5-Q6)+
     &L2*COS(Q3-Q4-Q5-Q6)) + G*MC*(L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6))
     & + G*MD*(L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + G*ME*(L4*COS(Q3-Q
     &5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + G*MF*(L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q
     &5-Q6)) + L2*L3*MB*SIN(Q4)*((U3-U5-U6)**2-(U3-U4-U5-U6)**2) + MC*(L
     &4*L5*SIN(Q5)*(U3-U6)**2+L2*L5*SIN(Q4+Q5)*(U3-U6)**2+L2*L4*SIN(Q4)*
     &(U3-U5-U6)**2-L2*L4*SIN(Q4)*(U3-U4-U5-U6)**2) + MF*(L4*L6*SIN(Q5)*
     &(U3-U6)**2+L2*L6*SIN(Q4+Q5)*(U3-U6)**2+L2*L4*SIN(Q4)*(U3-U5-U6)**2
     &-2*L4*FSp*COS(Q5+Q6)*U3-2*L2*FSp*COS(Q4+Q5+Q6)*U3-L4*SIN(Q5+Q6)*(F
     &Spp-FS*U3**2)-L2*SIN(Q4+Q5+Q6)*(FSpp-FS*U3**2)-L2*L4*SIN(Q4)*(U3-U
     &4-U5-U6)**2) + ME*(L4*L6*DApp*COS(DA-Q5-Q6)+L2*L6*DApp*COS(DA-Q4-Q
     &5-Q6)+L4*(L3-L4)*(DApp-EApp)*COS(DA-EA-Q5-Q6)+L2*(L3-L4)*(DApp-EAp
     &p)*COS(DA-EA-Q4-Q5-Q6)+L4*L6*SIN(Q5)*(U3-U6)**2+L2*L6*SIN(Q4+Q5)*(
     &U3-U6)**2+L2*L4*SIN(Q4)*(U3-U5-U6)**2-L2*L4*SIN(Q4)*(U3-U4-U5-U6)*
     &*2-L4*L6*SIN(DA-Q5-Q6)*(DAp-U3)**2-L2*L6*SIN(DA-Q4-Q5-Q6)*(DAp-U3)
     &**2-L4*(L3-L4)*SIN(DA-EA-Q5-Q6)*(DAp-EAp-U3)**2-L2*(L3-L4)*SIN(DA-
     &EA-Q4-Q5-Q6)*(DAp-EAp-U3)**2) - MD*(L4*(L5-L6)*DApp*COS(DA-Q5-Q6)+
     &L2*(L5-L6)*DApp*COS(DA-Q4-Q5-Q6)+L2*L4*SIN(Q4)*(U3-U4-U5-U6)**2-L4
     &*L6*SIN(Q5)*(U3-U6)**2-L2*L6*SIN(Q4+Q5)*(U3-U6)**2-L2*L4*SIN(Q4)*(
     &U3-U5-U6)**2-L4*(L5-L6)*SIN(DA-Q5-Q6)*(DAp-U3)**2-L2*(L5-L6)*SIN(D
     &A-Q4-Q5-Q6)*(DAp-U3)**2)
      RHS(6) = G*L1*MA*COS(Q3-Q4-Q5-Q6) + G*MB*(L3*COS(Q3-Q5-Q6)+L2*COS(
     &Q3-Q4-Q5-Q6)) + G*MC*(L5*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-
     &Q5-Q6)) + G*MD*(L6*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)
     &) + G*ME*(L6*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + G*
     &MF*(L6*COS(Q3-Q6)+L4*COS(Q3-Q5-Q6)+L2*COS(Q3-Q4-Q5-Q6)) + L2*L3*MB
     &*SIN(Q4)*((U3-U5-U6)**2-(U3-U4-U5-U6)**2) + MC*(L4*L5*SIN(Q5)*(U3-
     &U6)**2+L2*L5*SIN(Q4+Q5)*(U3-U6)**2+L2*L4*SIN(Q4)*(U3-U5-U6)**2-L4*
     &L5*SIN(Q5)*(U3-U5-U6)**2-L2*L4*SIN(Q4)*(U3-U4-U5-U6)**2-L2*L5*SIN(
     &Q4+Q5)*(U3-U4-U5-U6)**2) + MF*(L4*L6*SIN(Q5)*(U3-U6)**2+L2*L6*SIN(
     &Q4+Q5)*(U3-U6)**2+L2*L4*SIN(Q4)*(U3-U5-U6)**2-2*L6*FSp*COS(Q6)*U3-
     &2*L4*FSp*COS(Q5+Q6)*U3-2*L2*FSp*COS(Q4+Q5+Q6)*U3-L6*SIN(Q6)*(FSpp-
     &FS*U3**2)-L4*SIN(Q5+Q6)*(FSpp-FS*U3**2)-L2*SIN(Q4+Q5+Q6)*(FSpp-FS*
     &U3**2)-L4*L6*SIN(Q5)*(U3-U5-U6)**2-L2*L4*SIN(Q4)*(U3-U4-U5-U6)**2-
     &L2*L6*SIN(Q4+Q5)*(U3-U4-U5-U6)**2) + ME*(L6**2*DApp*COS(DA-Q6)+L4*
     &L6*DApp*COS(DA-Q5-Q6)+L2*L6*DApp*COS(DA-Q4-Q5-Q6)+L6*(L3-L4)*(DApp
     &-EApp)*COS(DA-EA-Q6)+L4*(L3-L4)*(DApp-EApp)*COS(DA-EA-Q5-Q6)+L2*(L
     &3-L4)*(DApp-EApp)*COS(DA-EA-Q4-Q5-Q6)+L4*L6*SIN(Q5)*(U3-U6)**2+L2*
     &L6*SIN(Q4+Q5)*(U3-U6)**2+L2*L4*SIN(Q4)*(U3-U5-U6)**2-L4*L6*SIN(Q5)
     &*(U3-U5-U6)**2-L6**2*SIN(DA-Q6)*(DAp-U3)**2-L2*L4*SIN(Q4)*(U3-U4-U
     &5-U6)**2-L4*L6*SIN(DA-Q5-Q6)*(DAp-U3)**2-L2*L6*SIN(Q4+Q5)*(U3-U4-U
     &5-U6)**2-L2*L6*SIN(DA-Q4-Q5-Q6)*(DAp-U3)**2-L6*(L3-L4)*SIN(DA-EA-Q
     &6)*(DAp-EAp-U3)**2-L4*(L3-L4)*SIN(DA-EA-Q5-Q6)*(DAp-EAp-U3)**2-L2*
     &(L3-L4)*SIN(DA-EA-Q4-Q5-Q6)*(DAp-EAp-U3)**2) - HTOR - MD*(L6*(L5-L
     &6)*DApp*COS(DA-Q6)+L4*(L5-L6)*DApp*COS(DA-Q5-Q6)+L2*(L5-L6)*DApp*C
     &OS(DA-Q4-Q5-Q6)+L4*L6*SIN(Q5)*(U3-U5-U6)**2+L2*L4*SIN(Q4)*(U3-U4-U
     &5-U6)**2+L2*L6*SIN(Q4+Q5)*(U3-U4-U5-U6)**2-L4*L6*SIN(Q5)*(U3-U6)**
     &2-L2*L6*SIN(Q4+Q5)*(U3-U6)**2-L2*L4*SIN(Q4)*(U3-U5-U6)**2-L6*(L5-L
     &6)*SIN(DA-Q6)*(DAp-U3)**2-L4*(L5-L6)*SIN(DA-Q5-Q6)*(DAp-U3)**2-L2*
     &(L5-L6)*SIN(DA-Q4-Q5-Q6)*(DAp-U3)**2)
      CALL SOLVE(6,COEF,RHS,VARp)

C**   Update variables after uncoupling equations
      U1p = VARp(1)
      U2p = VARp(2)
      U3p = VARp(3)
      U4p = VARp(4)
      U5p = VARp(5)
      U6p = VARp(6)

C**   Update derivative array prior to integration step
      VARp(1) = Q1p
      VARp(2) = Q2p
      VARp(3) = Q3p
      VARp(4) = Q4p
      VARp(5) = Q5p
      VARp(6) = Q6p
      VARp(7) = U1p
      VARp(8) = U2p
      VARp(9) = U3p
      VARp(10) = U4p
      VARp(11) = U5p
      VARp(12) = U6p

      RETURN
      END

C***********************************************************************
      SUBROUTINE INITCOND()      
C Subroutines used to convert model inputs into generalised coordinates.
C Input file (.in) contains CM velocities and joint angles/angular 
C velocities.
C
C Input/Output all through COMMON blocks
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER          NROW
      DIMENSION        TT(500),CCHIP(6,500),CCKNEE(6,500),CCHAT(6,500)
      COMMON/CONSTNTS/ G,IA,IB,IC,ID,IE,IF,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6
     &,L7,MA,MB,MC,MD,ME,MF
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,U1I,U2I,U3I,U4I,U5I,U6I
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(6,6),RHS(6)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW

C** Global variables 
      PI       = 4*ATAN(1.0D0)
      DEGtoRAD = PI/180.0D0
      RADtoDEG = 180.0D0/PI

C** Local variables
      AANG    = Q4I
      KANG    = Q5I
      HANG    = Q6I 
      KANGVEL = U5I
      VOCMX   = U1I
      VOCMY   = U2I
      U7      = 0.0D0
      U8      = 0.0D0

C** Convert joint angles to generalised coordinates/speeds
      Q4I = AANG - 180.0D0
      Q5I = 180.0D0 - KANG
      Q6I = HANG - 180.0D0
      U5I = -KANGVEL

C** Convert CoM velocity to generalised speeds
      Q3 = Q3I*DEGtoRAD
      Q4 = Q4I*DEGtoRAD
      Q5 = Q5I*DEGtoRAD
      Q6 = Q6I*DEGtoRAD
      U3 = U3I*DEGtoRAD
      U4 = U4I*DEGtoRAD
      U5 = U5I*DEGtoRAD
      U6 = U6I*DEGtoRAD
      
      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCHAT,FS,FSp,FSpp)
      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCHIP,DA,DAp,DApp)
      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCKNEE,EA,EAp,EApp)
      DA   = DA  *DEGtoRAD 
      DAp  = DAp *DEGtoRAD 
      DApp = DApp*DEGtoRAD 
      EA   = EA  *DEGtoRAD 
      EAp  = EAp *DEGtoRAD 
      EApp = EApp*DEGtoRAD 

      U1I = VOCMX -(MF*FSp*COS(Q3)-MF*FS*SIN(Q3)*U3-(L5*MC+L6*MD+L6*ME+L
     &6*MF)*SIN(Q3-Q6)*(U3-U6)-(L6*ME-MD*(L5-L6))*SIN(DA-Q3)*(DAp-U3-U7)
     &-ME*(L3-L4)*SIN(DA-EA-Q3)*(DAp-EAp-U3-U7-U8)-(L3*MB+L4*MC+L4*MD+L4
     &*ME+L4*MF)*SIN(Q3-Q5-Q6)*(U3-U5-U6)-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME
     &+L2*MF)*SIN(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6))/(MA+MB+MC+MD+ME+MF)
      U2I = VOCMY +((L6*ME-MD*(L5-L6))*COS(DA-Q3)*(DAp-U3-U7)+ME*(L3-L4)
     &*COS(DA-EA-Q3)*(DAp-EAp-U3-U7-U8)-MF*FSp*SIN(Q3)-MF*FS*COS(Q3)*U3-
     &(L5*MC+L6*MD+L6*ME+L6*MF)*COS(Q3-Q6)*(U3-U6)-(L3*MB+L4*MC+L4*MD+L4
     &*ME+L4*MF)*COS(Q3-Q5-Q6)*(U3-U5-U6)-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME
     &+L2*MF)*COS(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6))/(MA+MB+MC+MD+ME+MF)

      Q3I = Q3
      Q4I = Q4
      Q5I = Q5
      Q6I = Q6
      U3I = U3
      U4I = U4
      U5I = U5
      U6I = U6

      RETURN
      END SUBROUTINE INITCOND

C***********************************************************************
      SUBROUTINE UPDATE(T)
C Wrapper to calculate joint torques for each timestep 
C Input/Output all through COMMON blocks
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A -Z)

      INTEGER          NACTP
      PARAMETER        (NACTP=7)
      DIMENSION        HETQP(10),HFTQP(10),KETQP(10),KFTQP(10),AETQP(10)
     &,AFTQP(10),HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(NACTP)
     &,AEACTP(NACTP),AFACTP(NACTP)
      COMMON/CONSTNTS/ G,IA,IB,IC,ID,IE,IF,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6
     &,L7,MA,MB,MC,MD,ME,MF
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,Q6,U1,U2,U3,U4,U5,U6
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,HANG,HANGVEL,HETOR,
     &HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,PECM,PX,PY,RX,RY,
     &SHANG,SHANGVEL,SHTOR,SKANG,SKANGVEL,SKTOR,TE,U7,U8,Q1p,Q2p,Q3p,Q4p
     &,Q5p,Q6p,U1p,U2p,U3p,U4p,U5p,U6p,AEACT,AECCANG,AECCANGVEL,AESECANG
     &,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,DA,EA,F
     &S,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,HFCC
     &ANGVEL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,KESE
     &CANGVEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,DAp,EAp,FSp,
     &DApp,EApp,FSpp,POCMX,POCMY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP
     &4X,POP4Y,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POPFX,POPFY,VOCMX,VOC
     &MY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(6,6),RHS(6)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP

      HANG = 3.141592653589793D0 + Q6
      KANG = 3.141592653589793D0 - Q5
      AANG = 3.141592653589793D0 + Q4
      HANGVEL = U6
      KANGVEL = -U5
      AANGVEL = U4

      CALL MUSCLEMODEL(T,HETQP(1:9),HEACTP,HETQP(10),2*PI-HANG,-HANGVEL,
     &HETOR,HESECANG,HESECANGVEL,HECCANG,HECCANGVEL,INTEGSTP,2)
      CALL MUSCLEMODEL(T,KETQP(1:9),KEACTP,KETQP(10),2*PI-KANG,-KANGVEL,
     &KETOR,KESECANG,KESECANGVEL,KECCANG,KECCANGVEL,INTEGSTP,2)
      CALL MUSCLEMODEL(T,AETQP(1:9),AEACTP,AETQP(10),2*PI-AANG,-AANGVEL,
     &AETOR,AESECANG,AESECANGVEL,AECCANG,AECCANGVEL,INTEGSTP,2)
      CALL MUSCLEMODEL(T,HFTQP(1:9),HFACTP,HFTQP(10),HANG,HANGVEL,HFTOR,
     &HFSECANG,HFSECANGVEL,HFCCANG,HFCCANGVEL,INTEGSTP,2)
      CALL MUSCLEMODEL(T,KFTQP(1:9),KFACTP,KFTQP(10),KANG,KANGVEL,KFTOR,
     &KFSECANG,KFSECANGVEL,KFCCANG,KFCCANGVEL,INTEGSTP,2)
      CALL MUSCLEMODEL(T,AFTQP(1:9),AFACTP,AFTQP(10),AANG,AANGVEL,AFTOR,
     &AFSECANG,AFSECANGVEL,AFCCANG,AFCCANGVEL,INTEGSTP,2)

      HTOR = HETOR - HFTOR
      KTOR = KETOR - KFTOR
      ATOR = AETOR - AFTOR 

      END SUBROUTINE UPDATE