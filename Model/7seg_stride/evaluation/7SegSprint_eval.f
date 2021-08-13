C Seven segment model evaluation script
C Parameters to optimise:
C     - activation timings of each torque generator - 3n+1 paramaters
C       where n is the number of ramps (3 here)
C   Tom Rottier 2020
C***********************************************************************
      PROGRAM MAIN
      IMPLICIT         DOUBLE PRECISION (A - Z)
C** Model variables
      INTEGER          ILOOP,PRINTINT
      INTEGER          I,J,NACTP,NROW,NCOL
      PARAMETER        (NACTP=10)
      CHARACTER        MESSAGE(99)
      DIMENSION        Y(5000,11)
      DIMENSION        TT(500),CCHIP(6,500),CCKNEE(6,500),CCHAT(6,500)
      DIMENSION        HETQP(10),HFTQP(10),KETQP(10),KFTQP(10),AETQP(10)
     &,AFTQP(10),HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(NACTP)
     &,AEACTP(NACTP),AFACTP(NACTP),METQP(10),MFTQP(10)
      COMMON/CONSTNTS/ FOOTANG,G,IA,IB,IC,ID,IE,IF,IG,K1,K2,K3,K4,K5,K6,
     &K7,K8,L1,L10,L11,L12,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,
     &MTPB,MTPK,POP1XI,POP2XI
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,Q7I,U1I,U2I,U3I,U4I,U5I,U
     &6I,U7I
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(435),COEF(7,7),RHS(7)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP,METQP,MFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW
      COMMON/DATAIN  / Y,AERIALTIME,SWINGTIME,CONTACTTIME,VCMXI

C** SPAN variables
      INTEGER N, NEPS
      PARAMETER(N=60,NEPS=4)
      DOUBLE PRECISION  LB(N), UB(N), X(N), XOPT(N), C(N), VM(N),
     &                  FSTAR(NEPS), XP(N), T, EPS, RT, FOPT
      INTEGER  NACP(N), WORK(N), NS, NT, NFCNEV, IER, ISEED1, ISEED2,
     &         MAXEVL, IPRINT, NACC, NOBDS
      LOGICAL  MAX
      EXTERNAL SPAN

C**   Open input file
      OPEN(UNIT=20, FILE='7segSprint.in', STATUS='OLD')

C**   Read message from input file
      READ(20,7000,END=7100,ERR=7101) (MESSAGE(ILOOP),ILOOP = 1,99)

C**   Read values of constants from input file
      READ(20,7010,END=7100,ERR=7101) FOOTANG,G,IA,IB,IC,ID,IE,IF,IG,K1,
     &K2,K3,K4,K5,K6,K7,K8,L1,L10,L11,L12,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,
     &MC,MD,ME,MF,MG,MTPB,MTPK,POP1XI,POP2XI

C**   Read the initial value of each variable from input file
      READ(20,7010,END=7100,ERR=7101) Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,Q7I,U1I,U2
     &I,U3I,U4I,U5I,U6I,U7I

C**   Read integration parameters from input file
      READ(20,7011,END=7100,ERR=7101) TINITIAL,TFINAL,INTEGSTP,PRINTINT,
     &ABSERR,RELERR
      CLOSE(UNIT=20)

C** Read torque parameters
      OPEN(UNIT=40, FILE='torque.in', STATUS='OLD')
      READ(40, 7200, ERR=7210) HETQP,KETQP,AETQP,HFTQP,KFTQP,AFTQP,METQP
     &,MFTQP
      CLOSE(UNIT=40)

C** Read activation parameters
      OPEN(UNIT=41, FILE='activation.in', STATUS='OLD')
      READ(41, 7300, ERR=7310) HEACTP,KEACTP,AEACTP,HFACTP,KFACTP,AFACTP
      CLOSE(UNIT=41)

C** Read spline coefficients for angles and HAT CoM location
      OPEN(UNIT=42, FILE='angles_coef.csv', STATUS='OLD')
      READ(42,*) NROW
      READ(42,*) (TT(I), I=1, NROW)
      READ(42,*) ((CCHIP(J,I), J=1, 6), I=1, NROW)
      READ(42,*) ((CCKNEE(J,I), J=1, 6), I=1, NROW)
      CLOSE(UNIT=42)
      OPEN(UNIT=43, FILE='HAT_coef.csv', STATUS='OLD')
      READ(43,*)
      READ(43,*)
      READ(43,*) ((CCHAT(J,I), J=1, 6), I=1, NROW)
      CLOSE(UNIT=43)

C** Read matching data
      OPEN(UNIT=44, FILE='matchingData2.csv', STATUS='OLD')
      READ(44,*) NROW, NCOL
      READ(44,*)
      READ(44,*, ERR=7410) ((Y(I,J), J=1, NCOL), I=1, NROW)
      CLOSE(UNIT=44)
      AERIALTIME = 0.132D0
      SWINGTIME  = 0.374D0
      CONTACTTIME = 0.110D0
      VCMXI = U1I

C**   Convert to generalised coordinates
      CALL INITCOND()

C**   Set input parameters for SPAN
C*    Recommended values: NT = 100, NS = even multiple of ncpu
      MAX = .FALSE.
      EPS = 1.0D-03
      RT = 0.75
      ISEED1 = 3
      ISEED2 = 4
      NS = 24
      NT = 5
      MAXEVL = 100000000
      IPRINT = 1

C** Set upper and lower bounds on parameters
      DO I = 1, N, NACTP
        LB(I)    = 0.0D0
        LB(I+1)  = 0.0D0
        LB(I+2)  = 0.1D0
        LB(I+3)  = 0.0D0
        LB(I+4)  = 0.0D0
        LB(I+5)  = 0.1D0
        LB(I+6)  = 0.0D0
      ENDDO

      DO I = 1, N, NACTP
        UB(I)    = 1.0D0
        UB(I+1)  = 0.1D0
        UB(I+2)  = 0.3D0
        UB(I+3)  = 1.0D0
        UB(I+4)  = 0.1D0
        UB(I+5)  = 0.3D0
        UB(I+6)  = 1.0D0
      ENDDO


C***  Set input values of the input/output parameters
      T = 5.0

      DO 20, I = 1, N
         VM(I) = UB(I) - LB(I)
         C(I)  = 2.0D0
         X(I) = LB(I) + VM(I)*0.5D0
20    CONTINUE

      X(1:10)  = HEACTP
      X(11:20) = KEACTP
      X(21:30) = AEACTP
      X(31:40) = HFACTP
      X(41:50) = KFACTP
      X(51:60) = AFACTP


!      do i = 1, n
!        if ( x(i) .le. lb(i) ) print*, i, "lb"
!        if ( x(i) .ge. ub(i) ) print*, i, "ub"
!      enddo
!      stop

C**** Call SPAN
      CALL SPAN(N,X,MAX,RT,EPS,NS,NT,NEPS,MAXEVL,LB,UB,C,IPRINT,ISEED1,
     &        ISEED2,T,VM,XOPT,FOPT,NACC,NFCNEV,NOBDS,IER,
     &        FSTAR,XP,NACP,WORK)
      
!      DO I = 1, 1
!      CALL FCN(N,X,COST)
!      PRINT*, COST
!      ENDDO

      STOP
7000  FORMAT(//,99A1,///)
7010  FORMAT( 1000(59X,E30.0,/) )
7011  FORMAT( 3(59X,E30.0,/), 1(59X,I30,/), 2(59X,E30.0,/) )
      STOP
7100  WRITE(*,*) 'Premature end of file while reading 6segsprint.in '
7101  WRITE(*,*) 'Error while reading file 6segsprint.in'
      STOP
7200  FORMAT(//, 8(///, 10(8X, F7.2, /)))
7210  WRITE(*,*) 'Error reading torque parameters'
      STOP
7300  FORMAT(//, 6(///, 10(5X, G30.10, /)))
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
      INTEGER          N,IDX,NACTP,NROW,PRINTINT,IPRINT,FORCE_IDX
      LOGICAL          EXIT,STEP1,STEP2,STEP
      PARAMETER        (NACTP=10)
      EXTERNAL         EQNS1
      DIMENSION        VAR(14)
      DIMENSION        X(N),Y(5000,11)
      DIMENSION        TT(500),CCHIP(6,500),CCKNEE(6,500),CCHAT(6,500)
      DIMENSION        HETQP(10),HFTQP(10),KETQP(10),KFTQP(10),AETQP(10)
      DIMENSION        FORCE(1000,2)
     &,AFTQP(10),HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(NACTP)
     &,AEACTP(NACTP),AFACTP(NACTP),METQP(10),MFTQP(10)
      COMMON/CONSTNTS/ FOOTANG,G,IA,IB,IC,ID,IE,IF,IG,K1,K2,K3,K4,K5,K6,
     &K7,K8,L1,L10,L11,L12,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,
     &MTPB,MTPK,POP1XI,POP2XI
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,Q7I,U1I,U2I,U3I,U4I,U5I,U
     &6I,U7I
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,Q6,Q7,U1,U2,U3,U4,U5,U6,U7
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,COP,GRF,HANG,HANGVE
     &L,HETOR,HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,MANG,MANG
     &VEL,METOR,MFTOR,MTOR,PECM,PX,PY,RX,RX1,RX2,RY,RY1,RY2,SHANG,SHANGV
     &EL,SHTOR,SKANG,SKANGVEL,SKTOR,TE,U8,U9,VRX,VRY,Q1p,Q2p,Q3p,Q4p,Q5p
     &,Q6p,Q7p,U1p,U2p,U3p,U4p,U5p,U6p,U7p,AEACT,AECCANG,AECCANGVEL,AESE
     &CANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,EA,
     &FA,GS,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,
     &HFCCANGVEL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,
     &KESECANGVEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,MEACT,ME
     &CCANG,MECCANGVEL,MESECANG,MESECANGVEL,MFACT,MFCCANG,MFCCANGVEL,MFS
     &ECANG,MFSECANGVEL,EAp,FAp,GSp,EApp,FApp,GSpp,DP1X,DP2X,MT,POCMSTAN
     &CEX,POCMSTANCEY,POCMSWINGX,POCMSWINGY,POCMX,POCMY,POGOX,POGOY,POP1
     &X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,POP5X,POP5Y,POP6X,POP6
     &Y,POP7X,POP7Y,POP8X,POP8Y,POP9X,POP9Y,PSTANCEX,PSTANCEY,PSWINGX,PS
     &WINGY,VOCMSTANCEX,VOCMSTANCEY,VOCMSWINGX,VOCMSWINGY,VOCMX,VOCMY,VO
     &P2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(435),COEF(7,7),RHS(7)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP,METQP,MFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW
      COMMON/DATAIN  / Y,AERIALTIME,SWINGTIME,CONTACTTIME,VCMXI

C** Initialise parameters
      HEACTP = X(1:10) 
      KEACTP = X(11:20)
      AEACTP = X(21:30)
      HFACTP = X(31:40)
      KFACTP = X(41:50)
      AFACTP = X(51:60)


C** Initialise variables
      Q1 = Q1I
      Q2 = Q2I
      Q3 = Q3I
      Q4 = Q4I
      Q5 = Q5I
      Q6 = Q6I
      Q7 = Q7I
      U1 = U1I
      U2 = U2I
      U3 = U3I
      U4 = U4I
      U5 = U5I
      U6 = U6I 
      U7 = U7I

C**   Initialize time, print counter, variables array for integrator
      IPRINT = 0
      EXIT = .FALSE.
      T      = TINITIAL
      VAR(1) = Q1
      VAR(2) = Q2
      VAR(3) = Q3
      VAR(4) = Q4
      VAR(5) = Q5
      VAR(6) = Q6
      VAR(7) = Q7
      VAR(8) = U1
      VAR(9) = U2
      VAR(10) = U3
      VAR(11) = U4
      VAR(12) = U5
      VAR(13) = U6
      VAR(14) = U7

C Virtual forces
      FORCE_IDX = 1
      STEP1 = .TRUE.
      STEP2 = .FALSE.
      STEP = .TRUE.
      VRX = 0.0D0
      VRY = 0.0D0

C** Initialise torques for integration
      CALL UPDATE(T)

C**   Initalize numerical integrator with call to EQNS1 at T=TINITIAL
      CALL KUTTA(EQNS1, 14, VAR, T, INTEGSTP, ABSERR, RELERR, 0, *5920)

C** Initialise variables for COST
      IDX = 1
      CMYTD = Q2 + Z(56)*Z(25) + Z(57)*Z(44) + Z(58)*Z(48) + Z(59)*Z(51)
     & + Z(60)*Z(2) + 0.5D0*Z(55)*Z(40) + 0.5D0*Z(61)*Z(36) - Z(54)*Z(29
     &)
      HATS = 0.0D0
      HIPS = 0.0D0
      KNEES = 0.0D0
      ANKLES  = 0.0D0
      MTPS = 0.0D0

C** Main loop
5900  IF(TFINAL.GE.TINITIAL.AND.T+.01D0*INTEGSTP.GE.TFINAL) EXIT=.TRUE.
      IF(TFINAL.LE.TINITIAL.AND.T+.01D0*INTEGSTP.LE.TFINAL) EXIT=.TRUE.
      IF(STEP2 .AND. (Q2 .LE. 0.0D0 .OR. POP2Y .LE. 0.0D0)) EXIT=.TRUE.

      IF (EXIT) THEN
        IDX = IDX
        HATJ   = HATS   / IDX
        HIPJ   = HIPS   / IDX
        KNEEJ  = KNEES  / IDX
        ANKLEJ = ANKLES / IDX
        MTPJ   = MTPS   / IDX
        PERIODICITY = (Q2-Q2I)**2+(Q3-Q3I)**2+(Q4-Q4I)**2+(Q5-Q5I)**2+(
     &  Q6-Q6I)**2+(Q7-Q7I)**2+(U1-U1I)**2+(U2-U2I)**2+(U3-U3I)**2+(U4-
     &  U4I)**2+(U5-U5I)**2+(U6-U6I)**2+(U7-U7I)**2
  
        COST = 10*HATJ+HIPJ+KNEEJ+ANKLEJ+MTPJ+PERIODICITY
        RETURN
      ENDIF

C** Intermediate cost
      IF (IPRINT .EQ. 0) THEN
        HATS   = HATS   + (Y(IDX,3)  - Q3*RADtoDEG)**2
        HIPS   = HIPS   + (Y(IDX,5)  - HANG*RADtoDEG)**2
        KNEES  = KNEES  + (Y(IDX,7)  - KANG*RADtoDEG)**2
        ANKLES = ANKLES + (Y(IDX,9)  - AANG*RADtoDEG)**2
        MTPS   = MTPS   + (Y(IDX,11) - MANG*RADtoDEG)**2
        IDX = IDX + 1
        IPRINT = PRINTINT
      ENDIF

C CoM height
      POCMY = Q2 + Z(56)*Z(25) + Z(57)*Z(44) + Z(58)*Z(48) + Z(59)*Z(51)
     & + Z(60)*Z(2) + 0.5D0*Z(55)*Z(40) + 0.5D0*Z(61)*Z(36) - Z(54)*Z(29
     &)


C Check when second contact occuring
      IF ( .NOT. STEP1 .AND. POCMY .LE. CMYTD .AND. .NOT. STEP2) THEN
        STEP2 = .TRUE.
        FORCE_IDX = 1      
      ENDIF

C Apply force if second stance phase
      IF ( STEP2 .AND. STEP ) THEN
        VRX = FORCE(FORCE_IDX,1)
        VRY = FORCE(FORCE_IDX,2)
      ENDIF


C** Integrate      
      CALL KUTTA(EQNS1, 14, VAR, T, INTEGSTP, ABSERR, RELERR, 1, *5920)

C Save force if first stance phase      
      IF ( STEP1 ) THEN
        FORCE(FORCE_IDX,1) = RX
        FORCE(FORCE_IDX,2) = RY
      ENDIF

C** Update torques and forces after integration
      CALL UPDATE(T)

C Check if first contact over
      IF ( STEP1 .AND. RY .LT. 1.0D-8) STEP1 = .FALSE.
C Check if second contact over
      IF ( STEP2 .AND. VRY .LT. 1.0D-8) THEN 
        STEP2 = .FALSE.
        STEP = .FALSE.
      ENDIF

      IPRINT = IPRINT - 1
      FORCE_IDX = FORCE_IDX + 1
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
      COMMON/CONSTNTS/ FOOTANG,G,IA,IB,IC,ID,IE,IF,IG,K1,K2,K3,K4,K5,K6,
     &K7,K8,L1,L10,L11,L12,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,
     &MTPB,MTPK,POP1XI,POP2XI
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,Q6,Q7,U1,U2,U3,U4,U5,U6,U7
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,COP,GRF,HANG,HANGVE
     &L,HETOR,HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,MANG,MANG
     &VEL,METOR,MFTOR,MTOR,PECM,PX,PY,RX,RX1,RX2,RY,RY1,RY2,SHANG,SHANGV
     &EL,SHTOR,SKANG,SKANGVEL,SKTOR,TE,U8,U9,VRX,VRY,Q1p,Q2p,Q3p,Q4p,Q5p
     &,Q6p,Q7p,U1p,U2p,U3p,U4p,U5p,U6p,U7p,AEACT,AECCANG,AECCANGVEL,AESE
     &CANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,EA,
     &FA,GS,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,
     &HFCCANGVEL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,
     &KESECANGVEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,MEACT,ME
     &CCANG,MECCANGVEL,MESECANG,MESECANGVEL,MFACT,MFCCANG,MFCCANGVEL,MFS
     &ECANG,MFSECANGVEL,EAp,FAp,GSp,EApp,FApp,GSpp,DP1X,DP2X,MT,POCMSTAN
     &CEX,POCMSTANCEY,POCMSWINGX,POCMSWINGY,POCMX,POCMY,POGOX,POGOY,POP1
     &X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,POP5X,POP5Y,POP6X,POP6
     &Y,POP7X,POP7Y,POP8X,POP8Y,POP9X,POP9Y,PSTANCEX,PSTANCEY,PSWINGX,PS
     &WINGY,VOCMSTANCEX,VOCMSTANCEY,VOCMSWINGX,VOCMSWINGY,VOCMX,VOCMY,VO
     &P2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(435),COEF(7,7),RHS(7)
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW

C**   Update variables after integration step
      Q1 = VAR(1)
      Q2 = VAR(2)
      Q3 = VAR(3)
      Q4 = VAR(4)
      Q5 = VAR(5)
      Q6 = VAR(6)
      Q7 = VAR(7)
      U1 = VAR(8)
      U2 = VAR(9)
      U3 = VAR(10)
      U4 = VAR(11)
      U5 = VAR(12)
      U6 = VAR(13)
      U7 = VAR(14)

      Q1p = U1
      Q2p = U2
      Q3p = U3
      Q4p = U4
      Q5p = U5
      Q6p = U6
      Q7p = U7


C** Specified variables
      CALL EVALSPLINE2(T,NROW,TT,CCHAT,GS,GSp,GSpp)
      CALL EVALSPLINE2(T,NROW,TT,CCHIP,EA,EAp,EApp)
      CALL EVALSPLINE2(T,NROW,TT,CCKNEE,FA,FAp,FApp)
      EA   = EA  *DEGtoRAD 
      EAp  = EAp *DEGtoRAD 
      EApp = EApp*DEGtoRAD 
      FA   = FA  *DEGtoRAD 
      FAp  = FAp *DEGtoRAD 
      FApp = FApp*DEGtoRAD 

C** Intermediate variables
      Z(3) = COS(Q4)
      Z(5) = COS(Q5)
      Z(4) = SIN(Q4)
      Z(6) = SIN(Q5)
      Z(18) = Z(3)*Z(5) - Z(4)*Z(6)
      Z(1) = COS(Q3)
      Z(7) = COS(Q6)
      Z(10) = SIN(Q7)
      Z(8) = SIN(Q6)
      Z(9) = COS(Q7)
      Z(22) = -Z(7)*Z(10) - Z(8)*Z(9)
      Z(2) = SIN(Q3)
      Z(21) = Z(7)*Z(9) - Z(8)*Z(10)
      Z(25) = Z(1)*Z(22) + Z(2)*Z(21)
      Z(19) = -Z(3)*Z(6) - Z(4)*Z(5)
      Z(23) = Z(7)*Z(10) + Z(8)*Z(9)
      Z(27) = Z(1)*Z(21) + Z(2)*Z(23)
      Z(29) = Z(18)*Z(25) + Z(19)*Z(27)
      Z(20) = Z(3)*Z(6) + Z(4)*Z(5)
      Z(31) = Z(18)*Z(27) + Z(20)*Z(25)
      Z(24) = Z(1)*Z(21) - Z(2)*Z(22)
      Z(26) = Z(1)*Z(23) - Z(2)*Z(21)
      Z(28) = Z(18)*Z(24) + Z(19)*Z(26)
      Z(30) = Z(18)*Z(26) + Z(20)*Z(24)

C** Calculate forces
      POP2X = Q1 - L2*Z(28)
      VOP2X = U1 - L2*Z(30)*(U3-U4-U5-U6-U7)
      POP2Y = Q2 - L2*Z(29)
      VOP2Y = U2 - L2*Z(31)*(U3-U4-U5-U6-U7)
      
      IF (Q2 .LT. 0.0D0) THEN
        DP1X = Q1 - POP1XI
        RY1 = -K3*(Q2) - K4*ABS(Q2)*U2
        RX1 = (-K1*DP1X - K2*U1)*RY1
      ELSE
        RX1 = 0.0D0
        RY1 = 0.0D0
      ENDIF
      IF (POP2Y .LT. 0.0D0) THEN
        DP2X = POP2X - POP2XI
        RY2 = -K7*(POP2Y) - K8*ABS(POP2Y)*VOP2Y
        RX2 = (-K5*DP2X - K6*VOP2X)*RY2
      ELSE
        RX2 = 0.0D0
        RY2 = 0.0D0
      ENDIF
C Total force      
      RX = RX1 + RX2
      RY = RY1 + RY2


      Z(32) = Z(15)*Z(3) - Z(16)*Z(4)
      Z(33) = Z(15)*Z(4) + Z(16)*Z(3)
      Z(34) = -Z(15)*Z(4) - Z(16)*Z(3)
      Z(35) = Z(28)*Z(32) + Z(30)*Z(33)
      Z(36) = Z(29)*Z(32) + Z(31)*Z(33)
      Z(37) = Z(28)*Z(34) + Z(30)*Z(32)
      Z(38) = Z(29)*Z(34) + Z(31)*Z(32)
      Z(39) = Z(3)*Z(28) + Z(4)*Z(30)
      Z(40) = Z(3)*Z(29) + Z(4)*Z(31)
      Z(41) = Z(3)*Z(30) - Z(4)*Z(28)
      Z(42) = Z(3)*Z(31) - Z(4)*Z(29)
      Z(43) = Z(7)*Z(24) + Z(8)*Z(26)
      Z(44) = Z(7)*Z(25) + Z(8)*Z(27)
      Z(45) = Z(7)*Z(26) - Z(8)*Z(24)
      Z(46) = Z(7)*Z(27) - Z(8)*Z(25)
      Z(77) = Z(1)*Z(30) + Z(2)*Z(31)
      Z(78) = Z(1)*Z(29) - Z(2)*Z(28)
      Z(79) = Z(1)*Z(31) - Z(2)*Z(30)
      Z(82) = Z(1)*Z(41) + Z(2)*Z(42)
      Z(83) = Z(1)*Z(40) - Z(2)*Z(39)
      Z(84) = Z(1)*Z(42) - Z(2)*Z(41)
      Z(106) = L1*(U3-U4-U5-U6-U7)
      Z(107) = (U3-U4-U5-U6-U7)*Z(106)
      Z(108) = L2*(U3-U4-U5-U6-U7)
      Z(109) = (U3-U4-U5-U6-U7)*Z(108)
      Z(110) = L4*(U3-U5-U6-U7)
      Z(111) = L3*(U3-U5-U6-U7)
      Z(112) = (U3-U5-U6-U7)*Z(110)
      Z(113) = (U3-U5-U6-U7)*Z(111)
      Z(116) = L6*(U3-U5-U6-U7)
      Z(117) = (U3-U5-U6-U7)*Z(116)
      Z(118) = L7*(U3-U6-U7)
      Z(119) = (U3-U6-U7)*Z(118)
      Z(120) = L8*(U3-U6-U7)
      Z(121) = (U3-U6-U7)*Z(120)
      Z(122) = L9*(U3-U7)
      Z(123) = (U3-U7)*Z(122)
      Z(124) = L10*(U3-U7)
      Z(125) = (U3-U7)*Z(124)
      Z(157) = Z(7)*Z(18) + Z(8)*Z(19)
      Z(158) = Z(7)*Z(19) - Z(8)*Z(18)
      Z(159) = Z(7)*Z(20) + Z(8)*Z(18)
      Z(160) = Z(7)*Z(18) - Z(8)*Z(20)
      Z(162) = Z(3)*Z(158) + Z(4)*Z(160)
      Z(163) = Z(3)*Z(159) - Z(4)*Z(157)
      Z(164) = Z(3)*Z(160) - Z(4)*Z(158)
      Z(303) = RX1 + RX2 + VRX
      Z(305) = Z(304) + RY1 + RY2 + VRY
      Z(314) = MTOR + Z(306)*Z(31) + Z(308)*Z(31) + Z(309)*Z(31) + Z(310
     &)*Z(31) + Z(311)*Z(31) + Z(312)*Z(31) + Z(313)*Z(31) + L2*(RX2*Z(3
     &0)+RY2*Z(31)) + Z(54)*(VRX*Z(30)+VRY*Z(31))
      Z(336) = Z(335)*Z(30)
      Z(337) = Z(273)*Z(30) + MC*(L2*Z(30)-L6*Z(41)) + MD*(L2*Z(30)-L6*Z
     &(41)) + ME*(L2*Z(30)-L6*Z(41)) + MF*(L2*Z(30)-L6*Z(41)) + MG*(L2*Z
     &(30)-L6*Z(41)) + 0.5D0*MB*(2*L2*Z(30)-L3*Z(37)-L4*Z(41))
      Z(338) = Z(273)*Z(30) + MC*(L2*Z(30)-L6*Z(41)-L7*Z(26)) + MD*(L2*Z
     &(30)-L6*Z(41)-L8*Z(26)) + ME*(L2*Z(30)-L6*Z(41)-L8*Z(26)) + MF*(L2
     &*Z(30)-L6*Z(41)-L8*Z(26)) + MG*(L2*Z(30)-L6*Z(41)-L8*Z(26)) + 0.5D
     &0*MB*(2*L2*Z(30)-L3*Z(37)-L4*Z(41))
      Z(339) = Z(273)*Z(30) + MC*(L2*Z(30)-L6*Z(41)-L7*Z(26)) + 0.5D0*MB
     &*(2*L2*Z(30)-L3*Z(37)-L4*Z(41)) + MD*(L2*Z(30)-L6*Z(41)-L8*Z(26)-L
     &9*Z(45)) + ME*(L2*Z(30)-L10*Z(45)-L6*Z(41)-L8*Z(26)) + MF*(L2*Z(30
     &)-L10*Z(45)-L6*Z(41)-L8*Z(26)) + MG*(L2*Z(30)-L10*Z(45)-L6*Z(41)-L
     &8*Z(26))
      Z(342) = Z(335)*Z(31)
      Z(343) = Z(273)*Z(31) + MC*(L2*Z(31)-L6*Z(42)) + MD*(L2*Z(31)-L6*Z
     &(42)) + ME*(L2*Z(31)-L6*Z(42)) + MF*(L2*Z(31)-L6*Z(42)) + MG*(L2*Z
     &(31)-L6*Z(42)) + 0.5D0*MB*(2*L2*Z(31)-L3*Z(38)-L4*Z(42))
      Z(344) = Z(273)*Z(31) + MC*(L2*Z(31)-L6*Z(42)-L7*Z(27)) + MD*(L2*Z
     &(31)-L6*Z(42)-L8*Z(27)) + ME*(L2*Z(31)-L6*Z(42)-L8*Z(27)) + MF*(L2
     &*Z(31)-L6*Z(42)-L8*Z(27)) + MG*(L2*Z(31)-L6*Z(42)-L8*Z(27)) + 0.5D
     &0*MB*(2*L2*Z(31)-L3*Z(38)-L4*Z(42))
      Z(345) = Z(273)*Z(31) + MC*(L2*Z(31)-L6*Z(42)-L7*Z(27)) + 0.5D0*MB
     &*(2*L2*Z(31)-L3*Z(38)-L4*Z(42)) + MD*(L2*Z(31)-L6*Z(42)-L8*Z(27)-L
     &9*Z(46)) + ME*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)) + MF*(L2*Z(31
     &)-L10*Z(46)-L6*Z(42)-L8*Z(27)) + MG*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L
     &8*Z(27))
      Z(390) = Z(389) + Z(277)*(L2-L6*Z(3)) + Z(280)*(L2-L6*Z(3)) + Z(28
     &4)*(L2-L6*Z(3)) + Z(290)*(L2-L6*Z(3)) + Z(297)*(L2-L6*Z(3)) + 0.5D
     &0*Z(274)*(2*L2-L3*Z(32)-L4*Z(3))
      Z(391) = Z(389) + Z(277)*(L2-L6*Z(3)-L7*Z(18)) + Z(280)*(L2-L6*Z(3
     &)-L8*Z(18)) + Z(284)*(L2-L6*Z(3)-L8*Z(18)) + Z(290)*(L2-L6*Z(3)-L8
     &*Z(18)) + Z(297)*(L2-L6*Z(3)-L8*Z(18)) + 0.5D0*Z(274)*(2*L2-L3*Z(3
     &2)-L4*Z(3))
      Z(392) = Z(389) + Z(277)*(L2-L6*Z(3)-L7*Z(18)) + 0.5D0*Z(274)*(2*L
     &2-L3*Z(32)-L4*Z(3)) + Z(280)*(L2-L6*Z(3)-L8*Z(18)-L9*Z(160)) + Z(2
     &84)*(L2-L10*Z(160)-L6*Z(3)-L8*Z(18)) + Z(290)*(L2-L10*Z(160)-L6*Z(
     &3)-L8*Z(18)) + Z(297)*(L2-L10*Z(160)-L6*Z(3)-L8*Z(18))
      Z(396) = Z(394) + MC*(Z(395)-2*Z(351)*Z(3)) + MD*(Z(395)-2*Z(351)*
     &Z(3)) + ME*(Z(395)-2*Z(351)*Z(3)) + MF*(Z(395)-2*Z(351)*Z(3)) + MG
     &*(Z(395)-2*Z(351)*Z(3)) + 0.25D0*MB*(Z(348)-4*Z(349)*Z(32)-4*Z(350
     &)*Z(3))
      Z(397) = Z(394) + 0.25D0*MB*(Z(348)-4*Z(349)*Z(32)-4*Z(350)*Z(3)) 
     &- MC*(Z(352)*Z(18)+2*Z(351)*Z(3)-Z(353)-Z(354)-Z(356)*Z(5)) - MD*(
     &Z(357)*Z(18)+2*Z(351)*Z(3)-Z(353)-Z(354)-Z(361)*Z(5)) - ME*(Z(357)
     &*Z(18)+2*Z(351)*Z(3)-Z(353)-Z(354)-Z(361)*Z(5)) - MF*(Z(357)*Z(18)
     &+2*Z(351)*Z(3)-Z(353)-Z(354)-Z(361)*Z(5)) - MG*(Z(357)*Z(18)+2*Z(3
     &51)*Z(3)-Z(353)-Z(354)-Z(361)*Z(5))
      Z(398) = Z(394) + 0.25D0*MB*(Z(348)-4*Z(349)*Z(32)-4*Z(350)*Z(3)) 
     &- MC*(Z(352)*Z(18)+2*Z(351)*Z(3)-Z(353)-Z(354)-Z(356)*Z(5)) - MD*(
     &Z(357)*Z(18)+Z(358)*Z(160)+2*Z(351)*Z(3)-Z(353)-Z(354)-Z(361)*Z(5)
     &-Z(362)*Z(164)) - ME*(Z(357)*Z(18)+Z(364)*Z(160)+2*Z(351)*Z(3)-Z(3
     &53)-Z(354)-Z(361)*Z(5)-Z(368)*Z(164)) - MF*(Z(357)*Z(18)+Z(364)*Z(
     &160)+2*Z(351)*Z(3)-Z(353)-Z(354)-Z(361)*Z(5)-Z(368)*Z(164)) - MG*(
     &Z(357)*Z(18)+Z(364)*Z(160)+2*Z(351)*Z(3)-Z(353)-Z(354)-Z(361)*Z(5)
     &-Z(368)*Z(164))
      Z(401) = Z(400) + 0.25D0*MB*(Z(348)-4*Z(349)*Z(32)-4*Z(350)*Z(3)) 
     &- MC*(2*Z(351)*Z(3)+2*Z(352)*Z(18)-Z(353)-Z(354)-Z(355)-2*Z(356)*Z
     &(5)) - MD*(2*Z(351)*Z(3)+2*Z(357)*Z(18)-Z(353)-Z(354)-Z(359)-2*Z(3
     &61)*Z(5)) - ME*(2*Z(351)*Z(3)+2*Z(357)*Z(18)-Z(353)-Z(354)-Z(359)-
     &2*Z(361)*Z(5)) - MF*(2*Z(351)*Z(3)+2*Z(357)*Z(18)-Z(353)-Z(354)-Z(
     &359)-2*Z(361)*Z(5)) - MG*(2*Z(351)*Z(3)+2*Z(357)*Z(18)-Z(353)-Z(35
     &4)-Z(359)-2*Z(361)*Z(5))
      Z(402) = Z(400) + 0.25D0*MB*(Z(348)-4*Z(349)*Z(32)-4*Z(350)*Z(3)) 
     &- MC*(2*Z(351)*Z(3)+2*Z(352)*Z(18)-Z(353)-Z(354)-Z(355)-2*Z(356)*Z
     &(5)) - MD*(Z(358)*Z(160)+2*Z(351)*Z(3)+2*Z(357)*Z(18)-Z(353)-Z(354
     &)-Z(359)-2*Z(361)*Z(5)-Z(362)*Z(164)-Z(363)*Z(7)) - ME*(Z(364)*Z(1
     &60)+2*Z(351)*Z(3)+2*Z(357)*Z(18)-Z(353)-Z(354)-Z(359)-2*Z(361)*Z(5
     &)-Z(368)*Z(164)-Z(369)*Z(7)) - MF*(Z(364)*Z(160)+2*Z(351)*Z(3)+2*Z
     &(357)*Z(18)-Z(353)-Z(354)-Z(359)-2*Z(361)*Z(5)-Z(368)*Z(164)-Z(369
     &)*Z(7)) - MG*(Z(364)*Z(160)+2*Z(351)*Z(3)+2*Z(357)*Z(18)-Z(353)-Z(
     &354)-Z(359)-2*Z(361)*Z(5)-Z(368)*Z(164)-Z(369)*Z(7))
      Z(405) = Z(404) + 0.25D0*MB*(Z(348)-4*Z(349)*Z(32)-4*Z(350)*Z(3)) 
     &- MC*(2*Z(351)*Z(3)+2*Z(352)*Z(18)-Z(353)-Z(354)-Z(355)-2*Z(356)*Z
     &(5)) - MD*(2*Z(351)*Z(3)+2*Z(357)*Z(18)+2*Z(358)*Z(160)-Z(353)-Z(3
     &54)-Z(359)-Z(360)-2*Z(361)*Z(5)-2*Z(362)*Z(164)-2*Z(363)*Z(7)) - M
     &E*(2*Z(351)*Z(3)+2*Z(357)*Z(18)+2*Z(364)*Z(160)-Z(353)-Z(354)-Z(35
     &9)-Z(366)-2*Z(361)*Z(5)-2*Z(368)*Z(164)-2*Z(369)*Z(7)) - MF*(2*Z(3
     &51)*Z(3)+2*Z(357)*Z(18)+2*Z(364)*Z(160)-Z(353)-Z(354)-Z(359)-Z(366
     &)-2*Z(361)*Z(5)-2*Z(368)*Z(164)-2*Z(369)*Z(7)) - MG*(2*Z(351)*Z(3)
     &+2*Z(357)*Z(18)+2*Z(364)*Z(160)-Z(353)-Z(354)-Z(359)-Z(366)-2*Z(36
     &1)*Z(5)-2*Z(368)*Z(164)-2*Z(369)*Z(7))
      Z(11) = COS(EA)
      Z(12) = SIN(EA)
      Z(13) = COS(FA)
      Z(14) = SIN(FA)
      Z(47) = Z(11)*Z(1) + Z(12)*Z(2)
      Z(48) = Z(11)*Z(2) - Z(12)*Z(1)
      Z(49) = Z(12)*Z(1) - Z(11)*Z(2)
      Z(50) = -Z(13)*Z(47) - Z(14)*Z(49)
      Z(51) = -Z(13)*Z(48) - Z(14)*Z(47)
      Z(52) = Z(14)*Z(47) - Z(13)*Z(49)
      Z(53) = Z(14)*Z(48) - Z(13)*Z(47)
      Z(60) = MG*GS/MT
      Z(86) = U8 - EAp
      Z(88) = FApp - EApp
      Z(97) = Z(17)*EAp
      Z(98) = L10*EAp
      Z(99) = L12*(EAp-FAp)
      Z(126) = Z(17)*U3 + Z(17)*U8 - Z(97)
      Z(127) = Z(17)*EApp
      Z(128) = Z(126)*(U3+Z(86))
      Z(129) = L10*U3 + L10*U8 - Z(98)
      Z(130) = L10*EApp
      Z(131) = Z(129)*(U3+Z(86))
      Z(132) = L12*U3 + L12*U8 + L12*U9 - Z(99)
      Z(133) = L12*(EApp-FApp)
      Z(134) = FAp + U9
      Z(135) = Z(132)*(U3+Z(86)+Z(134))
      Z(139) = GS*U3
      Z(140) = GSp*U3
      Z(141) = GSpp - U3*Z(139)
      Z(142) = GSp*U3 + Z(140)
      Z(152) = MTOR - ATOR
      Z(153) = ATOR + KTOR
      Z(154) = -HTOR - KTOR
      Z(165) = Z(28)*Z(47) + Z(29)*Z(48)
      Z(166) = Z(28)*Z(49) + Z(29)*Z(47)
      Z(167) = Z(30)*Z(47) + Z(31)*Z(48)
      Z(168) = Z(30)*Z(49) + Z(31)*Z(47)
      Z(169) = Z(3)*Z(165) + Z(4)*Z(167)
      Z(170) = Z(3)*Z(166) + Z(4)*Z(168)
      Z(171) = Z(3)*Z(167) - Z(4)*Z(165)
      Z(172) = Z(3)*Z(168) - Z(4)*Z(166)
      Z(173) = Z(5)*Z(169) + Z(6)*Z(171)
      Z(174) = Z(5)*Z(170) + Z(6)*Z(172)
      Z(175) = Z(5)*Z(171) - Z(6)*Z(169)
      Z(176) = Z(5)*Z(172) - Z(6)*Z(170)
      Z(178) = Z(7)*Z(174) + Z(8)*Z(176)
      Z(179) = Z(7)*Z(175) - Z(8)*Z(173)
      Z(180) = Z(7)*Z(176) - Z(8)*Z(174)
      Z(181) = Z(28)*Z(50) + Z(29)*Z(51)
      Z(182) = Z(28)*Z(52) + Z(29)*Z(53)
      Z(183) = Z(30)*Z(50) + Z(31)*Z(51)
      Z(184) = Z(30)*Z(52) + Z(31)*Z(53)
      Z(185) = Z(3)*Z(181) + Z(4)*Z(183)
      Z(186) = Z(3)*Z(182) + Z(4)*Z(184)
      Z(187) = Z(3)*Z(183) - Z(4)*Z(181)
      Z(188) = Z(3)*Z(184) - Z(4)*Z(182)
      Z(189) = Z(5)*Z(185) + Z(6)*Z(187)
      Z(190) = Z(5)*Z(186) + Z(6)*Z(188)
      Z(191) = Z(5)*Z(187) - Z(6)*Z(185)
      Z(192) = Z(5)*Z(188) - Z(6)*Z(186)
      Z(194) = Z(7)*Z(190) + Z(8)*Z(192)
      Z(195) = Z(7)*Z(191) - Z(8)*Z(189)
      Z(196) = Z(7)*Z(192) - Z(8)*Z(190)
      Z(315) = MTOR + Z(306)*Z(31) + Z(54)*VRX*Z(30) + Z(54)*VRY*Z(31) +
     & L2*(RX2*Z(30)+RY2*Z(31)) + Z(147)*(L2*Z(31)-L6*Z(42)) + Z(148)*(L
     &2*Z(31)-L6*Z(42)) + Z(149)*(L2*Z(31)-L6*Z(42)) + Z(150)*(L2*Z(31)-
     &L6*Z(42)) + Z(151)*(L2*Z(31)-L6*Z(42)) + 0.5D0*Z(146)*(2*L2*Z(31)-
     &L3*Z(38)-L4*Z(42)) - Z(152) - 0.5D0*Z(55)*VRX*Z(41) - 0.5D0*Z(55)*
     &VRY*Z(42) - 0.5D0*Z(61)*VRX*Z(37) - 0.5D0*Z(61)*VRY*Z(38)
      Z(316) = MTOR + Z(306)*Z(31) + Z(54)*VRX*Z(30) + Z(54)*VRY*Z(31) +
     & L2*(RX2*Z(30)+RY2*Z(31)) + Z(147)*(L2*Z(31)-L6*Z(42)-L7*Z(27)) + 
     &Z(148)*(L2*Z(31)-L6*Z(42)-L8*Z(27)) + Z(149)*(L2*Z(31)-L6*Z(42)-L8
     &*Z(27)) + Z(150)*(L2*Z(31)-L6*Z(42)-L8*Z(27)) + Z(151)*(L2*Z(31)-L
     &6*Z(42)-L8*Z(27)) + 0.5D0*Z(146)*(2*L2*Z(31)-L3*Z(38)-L4*Z(42)) - 
     &Z(152) - Z(153) - Z(56)*VRX*Z(26) - Z(56)*VRY*Z(27) - 0.5D0*Z(55)*
     &VRX*Z(41) - 0.5D0*Z(55)*VRY*Z(42) - 0.5D0*Z(61)*VRX*Z(37) - 0.5D0*
     &Z(61)*VRY*Z(38)
      Z(317) = MTOR + Z(306)*Z(31) + Z(54)*VRX*Z(30) + Z(54)*VRY*Z(31) +
     & L2*(RX2*Z(30)+RY2*Z(31)) + Z(147)*(L2*Z(31)-L6*Z(42)-L7*Z(27)) + 
     &0.5D0*Z(146)*(2*L2*Z(31)-L3*Z(38)-L4*Z(42)) + Z(148)*(L2*Z(31)-L6*
     &Z(42)-L8*Z(27)-L9*Z(46)) + Z(149)*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*
     &Z(27)) + Z(150)*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)) + Z(151)*(L
     &2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)) - Z(152) - Z(153) - Z(154) - 
     &Z(56)*VRX*Z(26) - Z(56)*VRY*Z(27) - Z(57)*VRX*Z(45) - Z(57)*VRY*Z(
     &46) - 0.5D0*Z(55)*VRX*Z(41) - 0.5D0*Z(55)*VRY*Z(42) - 0.5D0*Z(61)*
     &VRX*Z(37) - 0.5D0*Z(61)*VRY*Z(38)
      Z(331) = IE*EApp
      Z(333) = IF*Z(88)
      Z(334) = MG*(L10*Z(45)+L6*Z(41)+L8*Z(26)-L2*Z(30)-GS*Z(2)) - Z(273
     &)*Z(30) - MC*(L2*Z(30)-L6*Z(41)-L7*Z(26)) - 0.5D0*MB*(2*L2*Z(30)-L
     &3*Z(37)-L4*Z(41)) - MD*(L2*Z(30)-L6*Z(41)-L8*Z(26)-L9*Z(45)) - ME*
     &(L2*Z(30)-L10*Z(45)-L6*Z(41)-L8*Z(26)-Z(17)*Z(49)) - MF*(L2*Z(30)-
     &L10*Z(45)-L10*Z(49)-L12*Z(52)-L6*Z(41)-L8*Z(26))
      Z(340) = MA*Z(28)*Z(107) + MC*(Z(28)*Z(109)-Z(24)*Z(119)-Z(39)*Z(1
     &17)) + 0.5D0*MB*(2*Z(28)*Z(109)-Z(35)*Z(113)-Z(39)*Z(112)) + MD*(Z
     &(28)*Z(109)-Z(24)*Z(121)-Z(39)*Z(117)-Z(43)*Z(123)) + MG*(Z(1)*Z(1
     &41)+Z(28)*Z(109)-Z(2)*Z(142)-Z(24)*Z(121)-Z(39)*Z(117)-Z(43)*Z(125
     &)) + ME*(Z(28)*Z(109)-Z(127)*Z(49)-Z(24)*Z(121)-Z(39)*Z(117)-Z(43)
     &*Z(125)-Z(47)*Z(128)) + MF*(Z(28)*Z(109)-Z(130)*Z(49)-Z(133)*Z(52)
     &-Z(24)*Z(121)-Z(39)*Z(117)-Z(43)*Z(125)-Z(47)*Z(131)-Z(50)*Z(135))
      Z(341) = -Z(273)*Z(31) - MC*(L2*Z(31)-L6*Z(42)-L7*Z(27)) - 0.5D0*M
     &B*(2*L2*Z(31)-L3*Z(38)-L4*Z(42)) - MD*(L2*Z(31)-L6*Z(42)-L8*Z(27)-
     &L9*Z(46)) - ME*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)-Z(17)*Z(47)) 
     &- MG*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)-GS*Z(1)) - MF*(L2*Z(31)
     &-L10*Z(46)-L10*Z(47)-L12*Z(53)-L6*Z(42)-L8*Z(27))
      Z(346) = MA*Z(29)*Z(107) + MC*(Z(29)*Z(109)-Z(25)*Z(119)-Z(40)*Z(1
     &17)) + 0.5D0*MB*(2*Z(29)*Z(109)-Z(36)*Z(113)-Z(40)*Z(112)) + MD*(Z
     &(29)*Z(109)-Z(25)*Z(121)-Z(40)*Z(117)-Z(44)*Z(123)) + MG*(Z(1)*Z(1
     &42)+Z(2)*Z(141)+Z(29)*Z(109)-Z(25)*Z(121)-Z(40)*Z(117)-Z(44)*Z(125
     &)) + ME*(Z(29)*Z(109)-Z(127)*Z(47)-Z(25)*Z(121)-Z(40)*Z(117)-Z(44)
     &*Z(125)-Z(48)*Z(128)) + MF*(Z(29)*Z(109)-Z(130)*Z(47)-Z(133)*Z(53)
     &-Z(25)*Z(121)-Z(40)*Z(117)-Z(44)*Z(125)-Z(48)*Z(131)-Z(51)*Z(135))
      Z(378) = Z(347) + 0.25D0*MB*(Z(348)-4*Z(349)*Z(32)-4*Z(350)*Z(3)) 
     &- MC*(2*Z(351)*Z(3)+2*Z(352)*Z(18)-Z(353)-Z(354)-Z(355)-2*Z(356)*Z
     &(5)) - MD*(2*Z(351)*Z(3)+2*Z(357)*Z(18)+2*Z(358)*Z(160)-Z(353)-Z(3
     &54)-Z(359)-Z(360)-2*Z(361)*Z(5)-2*Z(362)*Z(164)-2*Z(363)*Z(7)) - M
     &E*(2*Z(351)*Z(3)+2*Z(357)*Z(18)+2*Z(364)*Z(160)+2*Z(365)*Z(168)-Z(
     &353)-Z(354)-Z(359)-Z(366)-Z(367)-2*Z(361)*Z(5)-2*Z(368)*Z(164)-2*Z
     &(369)*Z(7)-2*Z(370)*Z(180)-2*Z(371)*Z(172)-2*Z(372)*Z(176)) - MG*(
     &2*Z(351)*Z(3)+2*Z(357)*Z(18)+2*Z(364)*Z(160)+2*L2*GS*Z(79)-Z(353)-
     &Z(354)-Z(359)-Z(366)-GS**2-2*Z(361)*Z(5)-2*Z(368)*Z(164)-2*Z(369)*
     &Z(7)-2*L10*GS*Z(9)-2*L6*GS*Z(84)-2*L8*GS*Z(21)) - MF*(2*Z(373)*Z(1
     &3)+2*Z(351)*Z(3)+2*Z(357)*Z(18)+2*Z(364)*Z(160)+2*Z(364)*Z(168)+2*
     &Z(374)*Z(184)-2*Z(366)-Z(353)-Z(354)-Z(359)-Z(375)-2*Z(361)*Z(5)-2
     &*Z(366)*Z(180)-2*Z(368)*Z(164)-2*Z(368)*Z(172)-2*Z(369)*Z(7)-2*Z(3
     &69)*Z(176)-2*Z(373)*Z(196)-2*Z(376)*Z(188)-2*Z(377)*Z(192))
      Z(380) = Z(379) - Z(277)*(L2-L6*Z(3)-L7*Z(18)) - 0.5D0*Z(274)*(2*L
     &2-L3*Z(32)-L4*Z(3)) - Z(280)*(L2-L6*Z(3)-L8*Z(18)-L9*Z(160)) - Z(2
     &84)*(L2-L10*Z(160)-L6*Z(3)-L8*Z(18)-Z(17)*Z(168)) - Z(297)*(L2-L10
     &*Z(160)-L6*Z(3)-L8*Z(18)-GS*Z(79)) - Z(290)*(L2-L10*Z(160)-L10*Z(1
     &68)-L12*Z(184)-L6*Z(3)-L8*Z(18))
      Z(382) = MC*(Z(352)*Z(18)+2*Z(351)*Z(3)-Z(353)-Z(354)-Z(356)*Z(5))
     & + MD*(Z(357)*Z(18)+Z(358)*Z(160)+2*Z(351)*Z(3)-Z(353)-Z(354)-Z(36
     &1)*Z(5)-Z(362)*Z(164)) + ME*(Z(357)*Z(18)+Z(364)*Z(160)+Z(365)*Z(1
     &68)+2*Z(351)*Z(3)-Z(353)-Z(354)-Z(361)*Z(5)-Z(368)*Z(164)-Z(371)*Z
     &(172)) + MG*(Z(357)*Z(18)+Z(364)*Z(160)+2*Z(351)*Z(3)+L2*GS*Z(79)-
     &Z(353)-Z(354)-Z(361)*Z(5)-Z(368)*Z(164)-L6*GS*Z(84)) + MF*(Z(357)*
     &Z(18)+Z(364)*Z(160)+Z(364)*Z(168)+Z(374)*Z(184)+2*Z(351)*Z(3)-Z(35
     &3)-Z(354)-Z(361)*Z(5)-Z(368)*Z(164)-Z(368)*Z(172)-Z(376)*Z(188)) -
     & IA - IB - Z(381) - 0.25D0*MB*(Z(348)-4*Z(349)*Z(32)-4*Z(350)*Z(3)
     &)
      Z(383) = MC*(2*Z(351)*Z(3)+2*Z(352)*Z(18)-Z(353)-Z(354)-Z(355)-2*Z
     &(356)*Z(5)) + MD*(Z(358)*Z(160)+2*Z(351)*Z(3)+2*Z(357)*Z(18)-Z(353
     &)-Z(354)-Z(359)-2*Z(361)*Z(5)-Z(362)*Z(164)-Z(363)*Z(7)) + ME*(Z(3
     &64)*Z(160)+Z(365)*Z(168)+2*Z(351)*Z(3)+2*Z(357)*Z(18)-Z(353)-Z(354
     &)-Z(359)-2*Z(361)*Z(5)-Z(368)*Z(164)-Z(369)*Z(7)-Z(371)*Z(172)-Z(3
     &72)*Z(176)) + MG*(Z(364)*Z(160)+2*Z(351)*Z(3)+2*Z(357)*Z(18)+L2*GS
     &*Z(79)-Z(353)-Z(354)-Z(359)-2*Z(361)*Z(5)-Z(368)*Z(164)-Z(369)*Z(7
     &)-L6*GS*Z(84)-L8*GS*Z(21)) + MF*(Z(364)*Z(160)+Z(364)*Z(168)+Z(374
     &)*Z(184)+2*Z(351)*Z(3)+2*Z(357)*Z(18)-Z(353)-Z(354)-Z(359)-2*Z(361
     &)*Z(5)-Z(368)*Z(164)-Z(368)*Z(172)-Z(369)*Z(7)-Z(369)*Z(176)-Z(376
     &)*Z(188)-Z(377)*Z(192)) - IA - IB - IC - Z(381) - 0.25D0*MB*(Z(348
     &)-4*Z(349)*Z(32)-4*Z(350)*Z(3))
      Z(384) = MC*(2*Z(351)*Z(3)+2*Z(352)*Z(18)-Z(353)-Z(354)-Z(355)-2*Z
     &(356)*Z(5)) + MD*(2*Z(351)*Z(3)+2*Z(357)*Z(18)+2*Z(358)*Z(160)-Z(3
     &53)-Z(354)-Z(359)-Z(360)-2*Z(361)*Z(5)-2*Z(362)*Z(164)-2*Z(363)*Z(
     &7)) + ME*(Z(365)*Z(168)+2*Z(351)*Z(3)+2*Z(357)*Z(18)+2*Z(364)*Z(16
     &0)-Z(353)-Z(354)-Z(359)-Z(366)-2*Z(361)*Z(5)-2*Z(368)*Z(164)-2*Z(3
     &69)*Z(7)-Z(370)*Z(180)-Z(371)*Z(172)-Z(372)*Z(176)) + MG*(2*Z(351)
     &*Z(3)+2*Z(357)*Z(18)+2*Z(364)*Z(160)+L2*GS*Z(79)-Z(353)-Z(354)-Z(3
     &59)-Z(366)-2*Z(361)*Z(5)-2*Z(368)*Z(164)-2*Z(369)*Z(7)-L10*GS*Z(9)
     &-L6*GS*Z(84)-L8*GS*Z(21)) + MF*(Z(364)*Z(168)+Z(374)*Z(184)+2*Z(35
     &1)*Z(3)+2*Z(357)*Z(18)+2*Z(364)*Z(160)-Z(353)-Z(354)-Z(359)-Z(366)
     &-2*Z(361)*Z(5)-2*Z(368)*Z(164)-2*Z(369)*Z(7)-Z(366)*Z(180)-Z(368)*
     &Z(172)-Z(369)*Z(176)-Z(373)*Z(196)-Z(376)*Z(188)-Z(377)*Z(192)) - 
     &IA - IB - IC - ID - Z(381) - 0.25D0*MB*(Z(348)-4*Z(349)*Z(32)-4*Z(
     &350)*Z(3))
      Z(387) = Z(333) + 0.25D0*MB*(Z(385)*Z(112)+2*L2*Z(4)*Z(112)+2*L2*Z
     &(33)*Z(113)+2*L3*Z(34)*Z(109)-Z(386)*Z(113)-2*L4*Z(4)*Z(109)) + MD
     &*(L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(159)*Z(123)+L8*Z(6)*Z(117)+L
     &8*Z(19)*Z(109)+L9*Z(8)*Z(121)+L9*Z(158)*Z(109)-L6*Z(4)*Z(109)-L6*Z
     &(6)*Z(121)-L6*Z(163)*Z(123)-L8*Z(8)*Z(123)-L9*Z(162)*Z(117)) + MG*
     &(GS*Z(142)+L10*Z(8)*Z(121)+L10*Z(9)*Z(142)+L10*Z(10)*Z(141)+L10*Z(
     &158)*Z(109)+L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(159)*Z(125)+L6*Z(8
     &2)*Z(141)+L6*Z(84)*Z(142)+L8*Z(6)*Z(117)+L8*Z(19)*Z(109)+L8*Z(21)*
     &Z(142)+L8*Z(23)*Z(141)+GS*Z(10)*Z(125)+GS*Z(78)*Z(109)-L10*Z(162)*
     &Z(117)-L2*Z(77)*Z(141)-L2*Z(79)*Z(142)-L6*Z(4)*Z(109)-L6*Z(6)*Z(12
     &1)-L6*Z(163)*Z(125)-L8*Z(8)*Z(125)-GS*Z(22)*Z(121)-GS*Z(83)*Z(117)
     &) + ME*(L2*Z(127)*Z(168)+L10*Z(8)*Z(121)+L10*Z(158)*Z(109)+L2*Z(4)
     &*Z(117)+L2*Z(20)*Z(121)+L2*Z(159)*Z(125)+L2*Z(167)*Z(128)+L8*Z(6)*
     &Z(117)+L8*Z(19)*Z(109)+Z(17)*Z(166)*Z(109)-Z(17)*Z(127)-L10*Z(127)
     &*Z(180)-L6*Z(127)*Z(172)-L8*Z(127)*Z(176)-L10*Z(162)*Z(117)-L10*Z(
     &179)*Z(128)-L6*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(163)*Z(125)-L6*Z(17
     &1)*Z(128)-L8*Z(8)*Z(125)-L8*Z(175)*Z(128)-Z(17)*Z(170)*Z(117)-Z(17
     &)*Z(174)*Z(121)-Z(17)*Z(178)*Z(125)) + MF*(L10*Z(13)*Z(133)+L12*Z(
     &13)*Z(130)+L2*Z(130)*Z(168)+L2*Z(133)*Z(184)+L10*Z(14)*Z(135)+L10*
     &Z(8)*Z(121)+L10*Z(158)*Z(109)+L10*Z(166)*Z(109)+L12*Z(182)*Z(109)+
     &L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(159)*Z(125)+L2*Z(167)*Z(131)+L
     &2*Z(183)*Z(135)+L8*Z(6)*Z(117)+L8*Z(19)*Z(109)-L10*Z(130)-L12*Z(13
     &3)-L10*Z(130)*Z(180)-L10*Z(133)*Z(196)-L6*Z(130)*Z(172)-L6*Z(133)*
     &Z(188)-L8*Z(130)*Z(176)-L8*Z(133)*Z(192)-L10*Z(162)*Z(117)-L10*Z(1
     &70)*Z(117)-L10*Z(174)*Z(121)-L10*Z(178)*Z(125)-L10*Z(179)*Z(131)-L
     &10*Z(195)*Z(135)-L12*Z(14)*Z(131)-L12*Z(186)*Z(117)-L12*Z(190)*Z(1
     &21)-L12*Z(194)*Z(125)-L6*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(163)*Z(12
     &5)-L6*Z(171)*Z(131)-L6*Z(187)*Z(135)-L8*Z(8)*Z(125)-L8*Z(175)*Z(13
     &1)-L8*Z(191)*Z(135)) - Z(331) - MC*(L6*Z(4)*Z(109)+L6*Z(6)*Z(119)-
     &L2*Z(4)*Z(117)-L2*Z(20)*Z(119)-L7*Z(6)*Z(117)-L7*Z(19)*Z(109))
      Z(393) = L2*(2*MG*(Z(77)*Z(141)+Z(79)*Z(142)-Z(4)*Z(117)-Z(20)*Z(1
     &21)-Z(159)*Z(125))-2*MC*(Z(4)*Z(117)+Z(20)*Z(119))-MB*(Z(4)*Z(112)
     &+Z(33)*Z(113))-2*MD*(Z(4)*Z(117)+Z(20)*Z(121)+Z(159)*Z(123))-2*ME*
     &(Z(127)*Z(168)+Z(4)*Z(117)+Z(20)*Z(121)+Z(159)*Z(125)+Z(167)*Z(128
     &))-2*MF*(Z(130)*Z(168)+Z(133)*Z(184)+Z(4)*Z(117)+Z(20)*Z(121)+Z(15
     &9)*Z(125)+Z(167)*Z(131)+Z(183)*Z(135)))
      Z(399) = -MC*(L2*Z(4)*Z(117)+L2*Z(20)*Z(119)-L6*Z(4)*Z(109)-L6*Z(6
     &)*Z(119)) - MD*(L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(159)*Z(123)-L6
     &*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(163)*Z(123)) - 0.25D0*MB*(Z(385)*
     &Z(112)+2*L2*Z(4)*Z(112)+2*L2*Z(33)*Z(113)+2*L3*Z(34)*Z(109)-Z(386)
     &*Z(113)-2*L4*Z(4)*Z(109)) - ME*(L2*Z(127)*Z(168)+L2*Z(4)*Z(117)+L2
     &*Z(20)*Z(121)+L2*Z(159)*Z(125)+L2*Z(167)*Z(128)-L6*Z(127)*Z(172)-L
     &6*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(163)*Z(125)-L6*Z(171)*Z(128)) - 
     &MG*(L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(159)*Z(125)+L6*Z(82)*Z(141
     &)+L6*Z(84)*Z(142)-L2*Z(77)*Z(141)-L2*Z(79)*Z(142)-L6*Z(4)*Z(109)-L
     &6*Z(6)*Z(121)-L6*Z(163)*Z(125)) - MF*(L2*Z(130)*Z(168)+L2*Z(133)*Z
     &(184)+L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(159)*Z(125)+L2*Z(167)*Z(
     &131)+L2*Z(183)*Z(135)-L6*Z(130)*Z(172)-L6*Z(133)*Z(188)-L6*Z(4)*Z(
     &109)-L6*Z(6)*Z(121)-L6*Z(163)*Z(125)-L6*Z(171)*Z(131)-L6*Z(187)*Z(
     &135))
      Z(403) = MC*(L6*Z(4)*Z(109)+L6*Z(6)*Z(119)-L2*Z(4)*Z(117)-L2*Z(20)
     &*Z(119)-L7*Z(6)*Z(117)-L7*Z(19)*Z(109)) + MD*(L6*Z(4)*Z(109)+L6*Z(
     &6)*Z(121)+L6*Z(163)*Z(123)+L8*Z(8)*Z(123)-L2*Z(4)*Z(117)-L2*Z(20)*
     &Z(121)-L2*Z(159)*Z(123)-L8*Z(6)*Z(117)-L8*Z(19)*Z(109)) + MG*(L2*Z
     &(77)*Z(141)+L2*Z(79)*Z(142)+L6*Z(4)*Z(109)+L6*Z(6)*Z(121)+L6*Z(163
     &)*Z(125)+L8*Z(8)*Z(125)-L2*Z(4)*Z(117)-L2*Z(20)*Z(121)-L2*Z(159)*Z
     &(125)-L6*Z(82)*Z(141)-L6*Z(84)*Z(142)-L8*Z(6)*Z(117)-L8*Z(19)*Z(10
     &9)-L8*Z(21)*Z(142)-L8*Z(23)*Z(141)) - 0.25D0*MB*(Z(385)*Z(112)+2*L
     &2*Z(4)*Z(112)+2*L2*Z(33)*Z(113)+2*L3*Z(34)*Z(109)-Z(386)*Z(113)-2*
     &L4*Z(4)*Z(109)) - ME*(L2*Z(127)*Z(168)+L2*Z(4)*Z(117)+L2*Z(20)*Z(1
     &21)+L2*Z(159)*Z(125)+L2*Z(167)*Z(128)+L8*Z(6)*Z(117)+L8*Z(19)*Z(10
     &9)-L6*Z(127)*Z(172)-L8*Z(127)*Z(176)-L6*Z(4)*Z(109)-L6*Z(6)*Z(121)
     &-L6*Z(163)*Z(125)-L6*Z(171)*Z(128)-L8*Z(8)*Z(125)-L8*Z(175)*Z(128)
     &) - MF*(L2*Z(130)*Z(168)+L2*Z(133)*Z(184)+L2*Z(4)*Z(117)+L2*Z(20)*
     &Z(121)+L2*Z(159)*Z(125)+L2*Z(167)*Z(131)+L2*Z(183)*Z(135)+L8*Z(6)*
     &Z(117)+L8*Z(19)*Z(109)-L6*Z(130)*Z(172)-L6*Z(133)*Z(188)-L8*Z(130)
     &*Z(176)-L8*Z(133)*Z(192)-L6*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(163)*Z
     &(125)-L6*Z(171)*Z(131)-L6*Z(187)*Z(135)-L8*Z(8)*Z(125)-L8*Z(175)*Z
     &(131)-L8*Z(191)*Z(135))
      Z(406) = MC*(L6*Z(4)*Z(109)+L6*Z(6)*Z(119)-L2*Z(4)*Z(117)-L2*Z(20)
     &*Z(119)-L7*Z(6)*Z(117)-L7*Z(19)*Z(109)) + MG*(L10*Z(162)*Z(117)+L2
     &*Z(77)*Z(141)+L2*Z(79)*Z(142)+L6*Z(4)*Z(109)+L6*Z(6)*Z(121)+L6*Z(1
     &63)*Z(125)+L8*Z(8)*Z(125)-L10*Z(8)*Z(121)-L10*Z(9)*Z(142)-L10*Z(10
     &)*Z(141)-L10*Z(158)*Z(109)-L2*Z(4)*Z(117)-L2*Z(20)*Z(121)-L2*Z(159
     &)*Z(125)-L6*Z(82)*Z(141)-L6*Z(84)*Z(142)-L8*Z(6)*Z(117)-L8*Z(19)*Z
     &(109)-L8*Z(21)*Z(142)-L8*Z(23)*Z(141)) - 0.25D0*MB*(Z(385)*Z(112)+
     &2*L2*Z(4)*Z(112)+2*L2*Z(33)*Z(113)+2*L3*Z(34)*Z(109)-Z(386)*Z(113)
     &-2*L4*Z(4)*Z(109)) - MD*(L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(159)*
     &Z(123)+L8*Z(6)*Z(117)+L8*Z(19)*Z(109)+L9*Z(8)*Z(121)+L9*Z(158)*Z(1
     &09)-L6*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(163)*Z(123)-L8*Z(8)*Z(123)-
     &L9*Z(162)*Z(117)) - ME*(L2*Z(127)*Z(168)+L10*Z(8)*Z(121)+L10*Z(158
     &)*Z(109)+L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(159)*Z(125)+L2*Z(167)
     &*Z(128)+L8*Z(6)*Z(117)+L8*Z(19)*Z(109)-L10*Z(127)*Z(180)-L6*Z(127)
     &*Z(172)-L8*Z(127)*Z(176)-L10*Z(162)*Z(117)-L10*Z(179)*Z(128)-L6*Z(
     &4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(163)*Z(125)-L6*Z(171)*Z(128)-L8*Z(8)
     &*Z(125)-L8*Z(175)*Z(128)) - MF*(L2*Z(130)*Z(168)+L2*Z(133)*Z(184)+
     &L10*Z(8)*Z(121)+L10*Z(158)*Z(109)+L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L
     &2*Z(159)*Z(125)+L2*Z(167)*Z(131)+L2*Z(183)*Z(135)+L8*Z(6)*Z(117)+L
     &8*Z(19)*Z(109)-L10*Z(130)*Z(180)-L10*Z(133)*Z(196)-L6*Z(130)*Z(172
     &)-L6*Z(133)*Z(188)-L8*Z(130)*Z(176)-L8*Z(133)*Z(192)-L10*Z(162)*Z(
     &117)-L10*Z(179)*Z(131)-L10*Z(195)*Z(135)-L6*Z(4)*Z(109)-L6*Z(6)*Z(
     &121)-L6*Z(163)*Z(125)-L6*Z(171)*Z(131)-L6*Z(187)*Z(135)-L8*Z(8)*Z(
     &125)-L8*Z(175)*Z(131)-L8*Z(191)*Z(135))
      Z(425) = Z(303) - Z(340)
      Z(426) = Z(305) - Z(346)
      Z(428) = Z(314) - 0.5D0*Z(393)
      Z(429) = Z(315) - Z(399)
      Z(430) = Z(316) - Z(403)
      Z(431) = Z(317) - Z(406)
      Z(435) = HTOR + Z(152) + Z(153) + Z(154) + Z(56)*VRX*Z(26) + Z(56)
     &*VRY*Z(27) + Z(57)*VRX*Z(45) + Z(57)*VRY*Z(46) + Z(58)*VRX*Z(49) +
     & Z(58)*VRY*Z(47) + Z(59)*VRX*Z(52) + Z(59)*VRY*Z(53) + Z(60)*VRY*Z
     &(1) + 0.5D0*Z(55)*VRX*Z(41) + 0.5D0*Z(55)*VRY*Z(42) + 0.5D0*Z(61)*
     &VRX*Z(37) + 0.5D0*Z(61)*VRY*Z(38) - MTOR - Z(306)*Z(31) - Z(54)*VR
     &X*Z(30) - Z(54)*VRY*Z(31) - Z(60)*VRX*Z(2) - L2*(RX2*Z(30)+RY2*Z(3
     &1)) - Z(147)*(L2*Z(31)-L6*Z(42)-L7*Z(27)) - 0.5D0*Z(146)*(2*L2*Z(3
     &1)-L3*Z(38)-L4*Z(42)) - Z(148)*(L2*Z(31)-L6*Z(42)-L8*Z(27)-L9*Z(46
     &)) - Z(149)*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)-Z(17)*Z(47)) - Z
     &(151)*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)-GS*Z(1)) - Z(150)*(L2*
     &Z(31)-L10*Z(46)-L10*Z(47)-L12*Z(53)-L6*Z(42)-L8*Z(27)) - Z(387)

      COEF(1,1) = -MT
      COEF(1,2) = 0
      COEF(1,3) = -Z(334)
      COEF(1,4) = -Z(336)
      COEF(1,5) = -Z(337)
      COEF(1,6) = -Z(338)
      COEF(1,7) = -Z(339)
      COEF(2,1) = 0
      COEF(2,2) = -MT
      COEF(2,3) = -Z(341)
      COEF(2,4) = -Z(342)
      COEF(2,5) = -Z(343)
      COEF(2,6) = -Z(344)
      COEF(2,7) = -Z(345)
      COEF(3,1) = -Z(334)
      COEF(3,2) = -Z(341)
      COEF(3,3) = -Z(378)
      COEF(3,4) = -Z(380)
      COEF(3,5) = -Z(382)
      COEF(3,6) = -Z(383)
      COEF(3,7) = -Z(384)
      COEF(4,1) = -Z(336)
      COEF(4,2) = -Z(342)
      COEF(4,3) = -Z(380)
      COEF(4,4) = -Z(388)
      COEF(4,5) = -Z(390)
      COEF(4,6) = -Z(391)
      COEF(4,7) = -Z(392)
      COEF(5,1) = -Z(337)
      COEF(5,2) = -Z(343)
      COEF(5,3) = -Z(382)
      COEF(5,4) = -Z(390)
      COEF(5,5) = -Z(396)
      COEF(5,6) = -Z(397)
      COEF(5,7) = -Z(398)
      COEF(6,1) = -Z(338)
      COEF(6,2) = -Z(344)
      COEF(6,3) = -Z(383)
      COEF(6,4) = -Z(391)
      COEF(6,5) = -Z(397)
      COEF(6,6) = -Z(401)
      COEF(6,7) = -Z(402)
      COEF(7,1) = -Z(339)
      COEF(7,2) = -Z(345)
      COEF(7,3) = -Z(384)
      COEF(7,4) = -Z(392)
      COEF(7,5) = -Z(398)
      COEF(7,6) = -Z(402)
      COEF(7,7) = -Z(405)
      RHS(1) = -Z(425)
      RHS(2) = -Z(426)
      RHS(3) = -Z(435)
      RHS(4) = -Z(428)
      RHS(5) = -Z(429)
      RHS(6) = -Z(430)
      RHS(7) = -Z(431)
      CALL SOLVE(7,COEF,RHS,VARp)

C**   Update variables after uncoupling equations
      U1p = VARp(1)
      U2p = VARp(2)
      U3p = VARp(3)
      U4p = VARp(4)
      U5p = VARp(5)
      U6p = VARp(6)
      U7p = VARp(7)

C**   Update derivative array prior to integration step
      VARp(1) = Q1p
      VARp(2) = Q2p
      VARp(3) = Q3p
      VARp(4) = Q4p
      VARp(5) = Q5p
      VARp(6) = Q6p
      VARp(7) = Q7p
      VARp(8) = U1p
      VARp(9) = U2p
      VARp(10) = U3p
      VARp(11) = U4p
      VARp(12) = U5p
      VARp(13) = U6p
      VARp(14) = U7p

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
      COMMON/CONSTNTS/ FOOTANG,G,IA,IB,IC,ID,IE,IF,IG,K1,K2,K3,K4,K5,K6,
     &K7,K8,L1,L10,L11,L12,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,
     &MTPB,MTPK,POP1XI,POP2XI
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,Q7I,U1I,U2I,U3I,U4I,U5I,U
     &6I,U7I
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,COP,GRF,HANG,HANGVE
     &L,HETOR,HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,MANG,MANG
     &VEL,METOR,MFTOR,MTOR,PECM,PX,PY,RX,RX1,RX2,RY,RY1,RY2,SHANG,SHANGV
     &EL,SHTOR,SKANG,SKANGVEL,SKTOR,TE,U8,U9,VRX,VRY,Q1p,Q2p,Q3p,Q4p,Q5p
     &,Q6p,Q7p,U1p,U2p,U3p,U4p,U5p,U6p,U7p,AEACT,AECCANG,AECCANGVEL,AESE
     &CANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,EA,
     &FA,GS,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,
     &HFCCANGVEL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,
     &KESECANGVEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,MEACT,ME
     &CCANG,MECCANGVEL,MESECANG,MESECANGVEL,MFACT,MFCCANG,MFCCANGVEL,MFS
     &ECANG,MFSECANGVEL,EAp,FAp,GSp,EApp,FApp,GSpp,DP1X,DP2X,MT,POCMSTAN
     &CEX,POCMSTANCEY,POCMSWINGX,POCMSWINGY,POCMX,POCMY,POGOX,POGOY,POP1
     &X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,POP5X,POP5Y,POP6X,POP6
     &Y,POP7X,POP7Y,POP8X,POP8Y,POP9X,POP9Y,PSTANCEX,PSTANCEY,PSWINGX,PS
     &WINGY,VOCMSTANCEX,VOCMSTANCEY,VOCMSWINGX,VOCMSWINGY,VOCMX,VOCMY,VO
     &P2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(435),COEF(7,7),RHS(7)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW

C** Global variables
      PI       = 4*ATAN(1.0D0)
      DEGtoRAD = PI/180.0D0
      RADtoDEG = 180.0D0/PI
      FOOTANG = FOOTANG*DEGtoRAD
      U8      = 0.0D0
      U9      = 0.0D0
      MT = MA + MB + MC + MD + ME + MF + MG

      Z(295) = L12*MF
      Z(15) = COS(FOOTANG)
      Z(16) = SIN(FOOTANG)
      Z(17) = L10 - L9
      Z(54) = (L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF+L2*MG)/MT
      Z(55) = (L4*MB+2*L6*MC+2*L6*MD+2*L6*ME+2*L6*MF+2*L6*MG)/MT
      Z(56) = (L7*MC+L8*MD+L8*ME+L8*MF+L8*MG)/MT
      Z(57) = (L10*ME+L10*MF+L10*MG+L9*MD)/MT
      Z(58) = (L10*MF+ME*Z(17))/MT
      Z(59) = L12*MF/MT
      Z(61) = L3*MB/MT
      Z(62) = MA + MB + MC + MD
      Z(63) = (L1*MA+L2*MB+L2*MC+L2*MD)/Z(62)
      Z(64) = (L4*MB+2*L6*MC+2*L6*MD)/Z(62)
      Z(65) = (L7*MC+L8*MD)/Z(62)
      Z(66) = L9*MD/Z(62)
      Z(67) = L3*MB/Z(62)
      Z(68) = ME + MF
      Z(69) = L2*(ME+MF)/Z(68)
      Z(70) = L6*(ME+MF)/Z(68)
      Z(71) = L8*(ME+MF)/Z(68)
      Z(72) = L10*(ME+MF)/Z(68)
      Z(73) = (L10*MF+ME*Z(17))/Z(68)
      Z(74) = L12*MF/Z(68)
      Z(145) = G*MA
      Z(146) = G*MB
      Z(147) = G*MC
      Z(148) = G*MD
      Z(149) = G*ME
      Z(150) = G*MF
      Z(151) = G*MG
      Z(200) = Z(54) - L1
      Z(201) = Z(54) - L2
      Z(202) = 0.5D0*L4 - 0.5D0*Z(55)
      Z(203) = 0.5D0*L3 - 0.5D0*Z(61)
      Z(219) = L6 - 0.5D0*Z(55)
      Z(220) = L7 - Z(56)
      Z(221) = L8 - Z(56)
      Z(222) = L9 - Z(57)
      Z(223) = L10 - Z(57)
      Z(224) = Z(17) - Z(58)
      Z(225) = L10 - Z(58)
      Z(226) = L12 - Z(59)
      Z(232) = Z(202) + Z(15)*Z(203)
      Z(234) = Z(203) + Z(15)*Z(202)
      Z(239) = Z(15)*Z(61)
      Z(273) = L1*MA
      Z(274) = L2*MB
      Z(275) = L4*MB
      Z(276) = L3*MB
      Z(277) = L2*MC
      Z(278) = L6*MC
      Z(279) = L7*MC
      Z(280) = L2*MD
      Z(281) = L6*MD
      Z(282) = L8*MD
      Z(283) = L9*MD
      Z(284) = L2*ME
      Z(285) = L6*ME
      Z(286) = L8*ME
      Z(287) = L10*ME
      Z(288) = ME*Z(17)
      Z(290) = L2*MF
      Z(291) = L6*MF
      Z(292) = L8*MF
      Z(293) = L10*MF
      Z(297) = L2*MG
      Z(298) = L6*MG
      Z(299) = L8*MG
      Z(300) = L10*MG
      Z(304) = Z(145) + Z(146) + Z(147) + Z(148) + Z(149) + Z(150) + Z(1
     &51)
      Z(306) = L1*Z(145)
      Z(308) = L2*Z(146)
      Z(309) = L2*Z(147)
      Z(310) = L2*Z(148)
      Z(311) = L2*Z(149)
      Z(312) = L2*Z(150)
      Z(313) = L2*Z(151)
      Z(318) = Z(17)*Z(149)
      Z(320) = L12*Z(150)
      Z(335) = L1*MA + L2*MB + L2*MC + L2*MD + L2*ME + L2*MF + L2*MG
      Z(347) = IA + IB + IC + ID + IE + IF + IG + MA*L1**2
      Z(348) = L3**2 + L4**2 + 4*L2**2 + 2*L3*L4*Z(15)
      Z(349) = L2*L3
      Z(350) = L2*L4
      Z(351) = L2*L6
      Z(352) = L2*L7
      Z(353) = L2**2
      Z(354) = L6**2
      Z(355) = L7**2
      Z(356) = L6*L7
      Z(357) = L2*L8
      Z(358) = L2*L9
      Z(359) = L8**2
      Z(360) = L9**2
      Z(361) = L6*L8
      Z(362) = L6*L9
      Z(363) = L8*L9
      Z(364) = L10*L2
      Z(365) = L2*Z(17)
      Z(366) = L10**2
      Z(367) = Z(17)**2
      Z(368) = L10*L6
      Z(369) = L10*L8
      Z(370) = L10*Z(17)
      Z(371) = L6*Z(17)
      Z(372) = L8*Z(17)
      Z(373) = L10*L12
      Z(374) = L12*L2
      Z(375) = L12**2
      Z(376) = L12*L6
      Z(377) = L12*L8
      Z(379) = -IA - MA*L1**2
      Z(381) = MA*L1**2
      Z(385) = L3*Z(16)
      Z(386) = L4*Z(16)
      Z(388) = IA + MA*L1**2 + MB*L2**2 + MC*L2**2 + MD*L2**2 + ME*L2**2
     & + MF*L2**2 + MG*L2**2
      Z(389) = IA + MA*L1**2
      Z(394) = IA + IB + MA*L1**2
      Z(395) = L2**2 + L6**2
      Z(400) = IA + IB + IC + MA*L1**2
      Z(404) = IA + IB + IC + ID + MA*L1**2
      Z(407) = IE + IF
      Z(422) = L12*L2*MF

C** Local variables
      Q1 = Q1I
      Q2 = Q2I
      Q3 = Q3I*DEGtoRAD
      Q4 = Q4I*DEGtoRAD
      Q5 = Q5I*DEGtoRAD
      Q6 = Q6I*DEGtoRAD
      Q7 = Q7I*DEGtoRAD
      U1 = U1I
      U2 = U2I
      U3 = U3I*DEGtoRAD
      U4 = U4I*DEGtoRAD
      U5 = U5I*DEGtoRAD
      U6 = U6I*DEGtoRAD
      U7 = U7I*DEGtoRAD

      POP2X   = Q1
      POP2Y   = Q2
      MANG    = Q4
      AANG    = Q5
      KANG    = Q6
      HANG    = Q7 
      MANGVEL = U4
      AANGVEL = U5
      KANGVEL = U6
      HANGVEL = U7
      VOCMX   = U1
      VOCMY   = U2

C** Convert joint angles to generalised coordinates/speeds
      Q4 = MANG
      Q5 = AANG - PI
      Q6 = PI - KANG
      Q7 = HANG - PI
      U4 = MANGVEL
      U5 = AANGVEL
      U6 = -KANGVEL
      U7 = HANGVEL

C** Calculate Q1,Q2 given that MTP contacts first (and therefore at 0,0)
      Q1 = POP2X + L2*COS(Q3-Q4-Q5-Q6-Q7)
      Q2 = POP2Y + L2*SIN(Q3-Q4-Q5-Q6-Q7)

      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCHIP,EA,EAp,EApp)
      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCKNEE,FA,FAp,FApp)
      EA   = EA  *DEGtoRAD 
      EAp  = EAp *DEGtoRAD 
      EApp = EApp*DEGtoRAD 
      FA   = FA  *DEGtoRAD 
      FAp  = FAp *DEGtoRAD 
      FApp = FApp*DEGtoRAD 
      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCHAT,GS,GSp,GSpp)

C** Convert CoM velocities to generalised speeds
      U1 = VOCMX + 0.5D0*(2*MG*GS*SIN(Q3)*U3+2*(L10*ME+L10*MF+L10*MG+L9*
     &MD)*SIN(Q3-Q7)*(U3-U7)+2*(L10*MF+ME*(L10-L9))*SIN(EA-Q3)*(EAp-U3-U
     &8)+L3*MB*SIN(FOOTANG+Q3-Q5-Q6-Q7)*(U3-U5-U6-U7)+2*(L7*MC+L8*MD+L8*
     &ME+L8*MF+L8*MG)*SIN(Q3-Q6-Q7)*(U3-U6-U7)+(L4*MB+2*L6*MC+2*L6*MD+2*
     &L6*ME+2*L6*MF+2*L6*MG)*SIN(Q3-Q5-Q6-Q7)*(U3-U5-U6-U7)-2*MG*GSp*COS
     &(Q3)-2*L12*MF*SIN(EA-FA-Q3)*(EAp-FAp-U3-U8-U9)-2*(L1*MA+L2*MB+L2*M
     &C+L2*MD+L2*ME+L2*MF+L2*MG)*SIN(Q3-Q4-Q5-Q6-Q7)*(U3-U4-U5-U6-U7))/(
     &MA+MB+MC+MD+ME+MF+MG)

      U2 = VOCMY - 0.5D0*(2*MG*GSp*SIN(Q3)+2*MG*GS*COS(Q3)*U3+2*(L10*ME+
     &L10*MF+L10*MG+L9*MD)*COS(Q3-Q7)*(U3-U7)+2*L12*MF*COS(EA-FA-Q3)*(EA
     &p-FAp-U3-U8-U9)+L3*MB*COS(FOOTANG+Q3-Q5-Q6-Q7)*(U3-U5-U6-U7)+2*(L7
     &*MC+L8*MD+L8*ME+L8*MF+L8*MG)*COS(Q3-Q6-Q7)*(U3-U6-U7)+(L4*MB+2*L6*
     &MC+2*L6*MD+2*L6*ME+2*L6*MF+2*L6*MG)*COS(Q3-Q5-Q6-Q7)*(U3-U5-U6-U7)
     &-2*(L10*MF+ME*(L10-L9))*COS(EA-Q3)*(EAp-U3-U8)-2*(L1*MA+L2*MB+L2*M
     &C+L2*MD+L2*ME+L2*MF+L2*MG)*COS(Q3-Q4-Q5-Q6-Q7)*(U3-U4-U5-U6-U7))/(
     &MA+MB+MC+MD+ME+MF+MG)

C** Global intial variables
      Q1I = Q1
      Q2I = Q2
      Q3I = Q3
      Q4I = Q4
      Q5I = Q5
      Q6I = Q6
      Q7I = Q7
      U1I = U1
      U2I = U2
      U3I = U3
      U4I = U4
      U5I = U5
      U6I = U6
      U7I = U7
      POP1XI = Q1
      POP2XI = POP2X


      END SUBROUTINE INITCOND

C***********************************************************************
      SUBROUTINE UPDATE(T)
C Wrapper to calculate joint torques for each timestep 
C Input/Output all through COMMON blocks
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A -Z)
      INTEGER          NACTP
      PARAMETER        (NACTP=10)
      DIMENSION        HETQP(10),HFTQP(10),KETQP(10),KFTQP(10),AETQP(10)
     &,AFTQP(10),HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(NACTP)
     &,AEACTP(NACTP),AFACTP(NACTP),METQP(10),MFTQP(10)
      COMMON/CONSTNTS/ FOOTANG,G,IA,IB,IC,ID,IE,IF,IG,K1,K2,K3,K4,K5,K6,
     &K7,K8,L1,L10,L11,L12,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,
     &MTPB,MTPK,POP1XI,POP2XI
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,Q6,Q7,U1,U2,U3,U4,U5,U6,U7
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,COP,GRF,HANG,HANGVE
     &L,HETOR,HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,MANG,MANG
     &VEL,METOR,MFTOR,MTOR,PECM,PX,PY,RX,RX1,RX2,RY,RY1,RY2,SHANG,SHANGV
     &EL,SHTOR,SKANG,SKANGVEL,SKTOR,TE,U8,U9,VRX,VRY,Q1p,Q2p,Q3p,Q4p,Q5p
     &,Q6p,Q7p,U1p,U2p,U3p,U4p,U5p,U6p,U7p,AEACT,AECCANG,AECCANGVEL,AESE
     &CANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,EA,
     &FA,GS,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,
     &HFCCANGVEL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,
     &KESECANGVEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,MEACT,ME
     &CCANG,MECCANGVEL,MESECANG,MESECANGVEL,MFACT,MFCCANG,MFCCANGVEL,MFS
     &ECANG,MFSECANGVEL,EAp,FAp,GSp,EApp,FApp,GSpp,DP1X,DP2X,MT,POCMSTAN
     &CEX,POCMSTANCEY,POCMSWINGX,POCMSWINGY,POCMX,POCMY,POGOX,POGOY,POP1
     &X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,POP5X,POP5Y,POP6X,POP6
     &Y,POP7X,POP7Y,POP8X,POP8Y,POP9X,POP9Y,PSTANCEX,PSTANCEY,PSWINGX,PS
     &WINGY,VOCMSTANCEX,VOCMSTANCEY,VOCMSWINGX,VOCMSWINGY,VOCMX,VOCMY,VO
     &P2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(435),COEF(7,7),RHS(7)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP,METQP,MFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP

      HANG = 3.141592653589793D0 + Q7
      KANG = 3.141592653589793D0 - Q6
      AANG = 3.141592653589793D0 + Q5
      MANG = Q4
      HANGVEL = U7
      KANGVEL = -U6
      AANGVEL = U5
      MANGVEL = U4

      CALL MUSCLEMODEL(T,HETQP(1:9),HEACTP,HETQP(10),2*PI-HANG,-HANGVEL,
     &HETOR,HESECANG,HESECANGVEL,HECCANG,HECCANGVEL,INTEGSTP,3)
      CALL MUSCLEMODEL(T,KETQP(1:9),KEACTP,KETQP(10),2*PI-KANG,-KANGVEL,
     &KETOR,KESECANG,KESECANGVEL,KECCANG,KECCANGVEL,INTEGSTP,3)
      CALL MUSCLEMODEL(T,AETQP(1:9),AEACTP,AETQP(10),2*PI-AANG,-AANGVEL,
     &AETOR,AESECANG,AESECANGVEL,AECCANG,AECCANGVEL,INTEGSTP,3)
      CALL MUSCLEMODEL(T,HFTQP(1:9),HFACTP,HFTQP(10),HANG,HANGVEL,HFTOR,
     &HFSECANG,HFSECANGVEL,HFCCANG,HFCCANGVEL,INTEGSTP,3)
      CALL MUSCLEMODEL(T,KFTQP(1:9),KFACTP,KFTQP(10),KANG,KANGVEL,KFTOR,
     &KFSECANG,KFSECANGVEL,KFCCANG,KFCCANGVEL,INTEGSTP,3)
      CALL MUSCLEMODEL(T,AFTQP(1:9),AFACTP,AFTQP(10),AANG,AANGVEL,AFTOR,
     &AFSECANG,AFSECANGVEL,AFCCANG,AFCCANGVEL,INTEGSTP,3)
!       CALL MUSCLEMODEL(T,METQP(1:9),AEACTP,METQP(10),MANG,MANGVEL,METOR,
!      &MESECANG,MESECANGVEL,MECCANG,MECCANGVEL,INTEGSTP,2)
!       CALL MUSCLEMODEL(T,MFTQP(1:9),AFACTP,MFTQP(10),MANG,MANGVEL,MFTOR,
!      &MFSECANG,MFSECANGVEL,MFCCANG,MFCCANGVEL,INTEGSTP,2)

      HTOR = HETOR - HFTOR
      KTOR = KETOR - KFTOR
      ATOR = AETOR - AFTOR 
      ! MTOR = METOR - MFTOR
      MTOR = MTPK*(3.141592653589793D0-MANG) - MTPB*MANGVEL

      END SUBROUTINE UPDATE