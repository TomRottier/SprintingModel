C Main program for optimisation of seven segment simulation model of
C sprinting. 
C
C Parameters to optimise:
C   - Activation timings: 42 (7 per torque generator)
C
C Cost function minimising loss in horizontal velocity while maintaining 
C sufficient swing time.
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
      DIMENSION        TT(500),CCHIP(6,500),CCKNEE(6,500),CCHAT(6,500)
      DIMENSION        HETQP(10),HFTQP(10),KETQP(10),KFTQP(10),AETQP(10)
     &,AFTQP(10),HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(NACTP)
     &,AEACTP(NACTP),AFACTP(NACTP),METQP(10),MFTQP(10)
      COMMON/CONSTNTS/ FOOTANG,G,IA,IB,IC,ID,IE,IF,IG,K1,K2,K3,K4,K5,K6,
     &K7,K8,L1,L10,L11,L12,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,
     &MTPB,MTPK
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,Q7I,U1I,U2I,U3I,U4I,U5I,U
     &6I,U7I,POP1XI,POP1YI,POP2XI,POP2YI
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(431),COEF(7,7),RHS(7)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP,METQP,MFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW
      COMMON/DATAIN  / AERIALTIME,SWINGTIME,VCMXI

C** SPAN variables
      INTEGER N, NEPS
      PARAMETER(N=42,NEPS=4)
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
     &MC,MD,ME,MF,MG,MTPB,MTPK

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
      OPEN(UNIT=42, FILE='teamsport_coef.csv', STATUS='OLD')
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

C** Matching data
      AERIALTIME = 0.132D0
      SWINGTIME  = 0.374D0
      VCMXI = U1I

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

      X(1:7)   = HEACTP
      X(8:14)  = KEACTP
      X(15:21) = AEACTP
      X(22:28) = HFACTP
      X(29:35) = KFACTP
      X(36:42) = AFACTP

      DO I = 1, N
        LB(I) = X(I) - X(I)*0.2D0
        UB(I) = X(I) + X(I)*0.2D0
        IF (LB(I) .LT. 0.0D0) LB(I) = 0.0D0
        IF (UB(I) .GT. 1.0D0) UB(I) = 1.0D0
        VM(I) = UB(I) - LB(I)
      ENDDO

      DO I = 3, N, 7
        IF (LB(I) .LT. 0.1D0) LB(I) = 0.1D0
      ENDDO

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
7200  FORMAT(//, 8(///, 10(8X, F7.2, /)))
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
      DIMENSION        VAR(14)
      DIMENSION        X(N)
      DIMENSION        TT(500),CCHIP(6,500),CCKNEE(6,500),CCHAT(6,500)
      DIMENSION        HETQP(10),HFTQP(10),KETQP(10),KFTQP(10),AETQP(10)
     &,AFTQP(10),HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(NACTP)
     &,AEACTP(NACTP),AFACTP(NACTP),METQP(10),MFTQP(10)
      COMMON/CONSTNTS/ FOOTANG,G,IA,IB,IC,ID,IE,IF,IG,K1,K2,K3,K4,K5,K6,
     &K7,K8,L1,L10,L11,L12,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,
     &MTPB,MTPK
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,Q7I,U1I,U2I,U3I,U4I,U5I,U
     &6I,U7I,POP1XI,POP1YI,POP2XI,POP2YI
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,Q6,Q7,U1,U2,U3,U4,U5,U6,U7
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,COP,GRF,HANG,HANGVE
     &L,HETOR,HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,MANG,MANG
     &VEL,METOR,MFTOR,MTOR,PECM,PX,PY,RX,RX1,RX2,RY,RY1,RY2,SHANG,SHANGV
     &EL,SHTOR,SKANG,SKANGVEL,SKTOR,TE,U8,U9,Q1p,Q2p,Q3p,Q4p,Q5p,Q6p,Q7p
     &,U1p,U2p,U3p,U4p,U5p,U6p,U7p,AEACT,AECCANG,AECCANGVEL,AESECANG,AES
     &ECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,EA,FA,GS,HE
     &ACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,HFCCANGV
     &EL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,KESECANG
     &VEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,MEACT,MECCANG,ME
     &CCANGVEL,MESECANG,MESECANGVEL,MFACT,MFCCANG,MFCCANGVEL,MFSECANG,MF
     &SECANGVEL,EAp,FAp,GSp,EApp,FApp,GSpp,MT,POCMSTANCEX,POCMSTANCEY,PO
     &CMSWINGX,POCMSWINGY,POCMX,POCMY,POGOX,POGOY,POP1X,POP1Y,POP2X,POP2
     &Y,POP3X,POP3Y,POP4X,POP4Y,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POP8
     &X,POP8Y,POP9X,POP9Y,PSTANCEX,PSTANCEY,PSWINGX,PSWINGY,VOCMSTANCEX,
     &VOCMSTANCEY,VOCMSWINGX,VOCMSWINGY,VOCMX,VOCMY,VOP2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(431),COEF(7,7),RHS(7)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP,METQP,MFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW
      COMMON/DATAIN  / AERIALTIME,SWINGTIME,VCMXI

C** Initialise parameters
      HEACTP = X(1:7)
      KEACTP = X(8:14)
      AEACTP = X(15:21)
      HFACTP = X(22:28)
      KFACTP = X(29:35)
      AFACTP = X(36:42)

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

C** Initialise torques for integration
      CALL UPDATE(T)

C**   Initalize numerical integrator with call to EQNS1 at T=TINITIAL
      CALL KUTTA(EQNS1, 14, VAR, T, INTEGSTP, ABSERR, RELERR, 0, *5920)

C** Initialise variables for COST
      IDX = 2
      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCHAT,GS,GSp,GSpp)
      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCHIP,EA,EAp,EApp)
      CALL EVALSPLINE2(TINITIAL,NROW,TT,CCKNEE,FA,FAp,FApp)
      EA   = EA  *DEGtoRAD 
      EAp  = EAp *DEGtoRAD 
      EApp = EApp*DEGtoRAD 
      FA   = FA  *DEGtoRAD 
      FAp  = FAp *DEGtoRAD 
      FApp = FApp*DEGtoRAD 
      Z(60) = MG*GS/MT
      CMYTD = Q2 + Z(56)*Z(25) + Z(57)*Z(44) + Z(58)*Z(48) + Z(59)*Z(51)
     & + Z(60)*Z(2) + 0.5D0*Z(55)*Z(40) + 0.5D0*Z(61)*Z(36) - Z(54)*Z(29
     &)
      CMXTD = Q1 + Z(56)*Z(24) + Z(57)*Z(43) + Z(58)*Z(47) + Z(59)*Z(50)
     & + Z(60)*Z(1) + 0.5D0*Z(55)*Z(39) + 0.5D0*Z(61)*Z(35) - Z(54)*Z(28
     &)


C** Main loop
5900  IF(TFINAL.GE.TINITIAL.AND.T+.01D0*INTEGSTP.GE.TFINAL) EXIT=.TRUE.
      IF(TFINAL.LE.TINITIAL.AND.T+.01D0*INTEGSTP.LE.TFINAL) EXIT=.TRUE.
      IF (Q2 .GT. 1.0D-05 .AND. POP2Y .GT. 1.0D-05) EXIT = .TRUE.

      IF (EXIT) THEN
        IDX = IDX - 1
        Z(60) = MG*GS/MT
        Z(101) = MG*GSp/MT
        Z(102) = Z(58)*EAp
        Z(103) = Z(59)*(EAp-FAp)
        CMYTO = Q2+Z(56)*Z(25) + Z(57)*Z(44) + Z(58)*Z(48) + Z(59)*Z(51)
     & + Z(60)*Z(2) + 0.5D0*Z(55)*Z(40) + 0.5D0*Z(61)*Z(36) - Z(54)*Z(29
     &)
        CMXTO = Q1+Z(56)*Z(24) + Z(57)*Z(43) + Z(58)*Z(47) + Z(59)*Z(50)
     & + Z(60)*Z(1) + 0.5D0*Z(55)*Z(39) + 0.5D0*Z(61)*Z(35) - Z(54)*Z(28
     &)
        VCMXF=Z(101)*Z(1) + U1 + Z(57)*Z(45)*(U3-U7) + Z(56)*Z(26)*(U3-U
     &6-U7) + 0.5D0*Z(55)*Z(41)*(U3-U5-U6-U7) + 0.5D0*Z(61)*Z(37)*(U3-U5
     &-U6-U7) - Z(60)*Z(2)*U3 - Z(49)*(Z(102)-Z(58)*U3-Z(58)*U8) - Z(54)
     &*Z(30)*(U3-U4-U5-U6-U7) - Z(52)*(Z(103)-Z(59)*U3-Z(59)*U8-Z(59)*U9
     &)
        VCMYF=Z(101)*Z(2) + U2 + Z(60)*Z(1)*U3 + Z(57)*Z(46)*(U3-U7) + Z
     &(56)*Z(27)*(U3-U6-U7) + 0.5D0*Z(55)*Z(42)*(U3-U5-U6-U7) + 0.5D0*Z(
     &61)*Z(38)*(U3-U5-U6-U7) - Z(47)*(Z(102)-Z(58)*U3-Z(58)*U8) - Z(54)
     &*Z(31)*(U3-U4-U5-U6-U7) - Z(53)*(Z(103)-Z(59)*U3-Z(59)*U8-Z(59)*U9
     &)      
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
        VCMX = ((CMXTO - CMXTD) + VCMXF*TA) / (T + TA)
        VCMJ = ABS(VCMX - VCMXI)
  
        COST = 10*TSWJ+VCMJ
        RETURN
      ENDIF

C** Integrate      
      CALL KUTTA(EQNS1, 14, VAR, T, INTEGSTP, ABSERR, RELERR, 1, *5920)

C** Update torques after integration
      CALL UPDATE(T)

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
      COMMON/CONSTNTS/ FOOTANG,G,IA,IB,IC,ID,IE,IF,IG,K1,K2,K3,K4,K5,K6,
     &K7,K8,L1,L10,L11,L12,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,
     &MTPB,MTPK
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,Q6,Q7,U1,U2,U3,U4,U5,U6,U7
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,COP,GRF,HANG,HANGVE
     &L,HETOR,HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,MANG,MANG
     &VEL,METOR,MFTOR,MTOR,PECM,PX,PY,RX,RX1,RX2,RY,RY1,RY2,SHANG,SHANGV
     &EL,SHTOR,SKANG,SKANGVEL,SKTOR,TE,U8,U9,Q1p,Q2p,Q3p,Q4p,Q5p,Q6p,Q7p
     &,U1p,U2p,U3p,U4p,U5p,U6p,U7p,AEACT,AECCANG,AECCANGVEL,AESECANG,AES
     &ECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,EA,FA,GS,HE
     &ACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,HFCCANGV
     &EL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,KESECANG
     &VEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,MEACT,MECCANG,ME
     &CCANGVEL,MESECANG,MESECANGVEL,MFACT,MFCCANG,MFCCANGVEL,MFSECANG,MF
     &SECANGVEL,EAp,FAp,GSp,EApp,FApp,GSpp,MT,POCMSTANCEX,POCMSTANCEY,PO
     &CMSWINGX,POCMSWINGY,POCMX,POCMY,POGOX,POGOY,POP1X,POP1Y,POP2X,POP2
     &Y,POP3X,POP3Y,POP4X,POP4Y,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POP8
     &X,POP8Y,POP9X,POP9Y,PSTANCEX,PSTANCEY,PSWINGX,PSWINGY,VOCMSTANCEX,
     &VOCMSTANCEY,VOCMSWINGX,VOCMSWINGY,VOCMX,VOCMY,VOP2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(431),COEF(7,7),RHS(7)
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,Q7I,U1I,U2I,U3I,U4I,U5I,U
     &6I,U7I,POP1XI,POP1YI,POP2XI,POP2YI

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
        RY1 = -K3*(Q2) - K4*ABS(Q2)*U2
        RX1 = (-K1*(Q1-POP1XI) - K2*U1)*RY1
      ELSE
        RX1 = 0.0D0
        RY1 = 0.0D0
      ENDIF
      IF (POP2Y .LT. 0.0D0) THEN
        RY2 = -K7*(POP2Y) - K8*ABS(POP2Y)*VOP2Y
        RX2 = (-K5*(POP2X-POP2XI) - K6*VOP2X)*RY2
      ELSE
        RX2 = 0.0D0
        RY2 = 0.0D0
      ENDIF
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
      Z(155) = Z(7)*Z(18) + Z(8)*Z(19)
      Z(156) = Z(7)*Z(19) - Z(8)*Z(18)
      Z(157) = Z(7)*Z(20) + Z(8)*Z(18)
      Z(158) = Z(7)*Z(18) - Z(8)*Z(20)
      Z(160) = Z(3)*Z(156) + Z(4)*Z(158)
      Z(161) = Z(3)*Z(157) - Z(4)*Z(155)
      Z(162) = Z(3)*Z(158) - Z(4)*Z(156)
      Z(301) = RX1 + RX2
      Z(303) = Z(302) + RY1 + RY2
      Z(312) = MTOR + Z(304)*Z(31) + Z(306)*Z(31) + Z(307)*Z(31) + Z(308
     &)*Z(31) + Z(309)*Z(31) + Z(310)*Z(31) + Z(311)*Z(31) + L2*(RX2*Z(3
     &0)+RY2*Z(31))
      Z(334) = Z(333)*Z(30)
      Z(335) = Z(271)*Z(30) + MC*(L2*Z(30)-L6*Z(41)) + MD*(L2*Z(30)-L6*Z
     &(41)) + ME*(L2*Z(30)-L6*Z(41)) + MF*(L2*Z(30)-L6*Z(41)) + MG*(L2*Z
     &(30)-L6*Z(41)) + 0.5D0*MB*(2*L2*Z(30)-L3*Z(37)-L4*Z(41))
      Z(336) = Z(271)*Z(30) + MC*(L2*Z(30)-L6*Z(41)-L7*Z(26)) + MD*(L2*Z
     &(30)-L6*Z(41)-L8*Z(26)) + ME*(L2*Z(30)-L6*Z(41)-L8*Z(26)) + MF*(L2
     &*Z(30)-L6*Z(41)-L8*Z(26)) + MG*(L2*Z(30)-L6*Z(41)-L8*Z(26)) + 0.5D
     &0*MB*(2*L2*Z(30)-L3*Z(37)-L4*Z(41))
      Z(337) = Z(271)*Z(30) + MC*(L2*Z(30)-L6*Z(41)-L7*Z(26)) + 0.5D0*MB
     &*(2*L2*Z(30)-L3*Z(37)-L4*Z(41)) + MD*(L2*Z(30)-L6*Z(41)-L8*Z(26)-L
     &9*Z(45)) + ME*(L2*Z(30)-L10*Z(45)-L6*Z(41)-L8*Z(26)) + MF*(L2*Z(30
     &)-L10*Z(45)-L6*Z(41)-L8*Z(26)) + MG*(L2*Z(30)-L10*Z(45)-L6*Z(41)-L
     &8*Z(26))
      Z(340) = Z(333)*Z(31)
      Z(341) = Z(271)*Z(31) + MC*(L2*Z(31)-L6*Z(42)) + MD*(L2*Z(31)-L6*Z
     &(42)) + ME*(L2*Z(31)-L6*Z(42)) + MF*(L2*Z(31)-L6*Z(42)) + MG*(L2*Z
     &(31)-L6*Z(42)) + 0.5D0*MB*(2*L2*Z(31)-L3*Z(38)-L4*Z(42))
      Z(342) = Z(271)*Z(31) + MC*(L2*Z(31)-L6*Z(42)-L7*Z(27)) + MD*(L2*Z
     &(31)-L6*Z(42)-L8*Z(27)) + ME*(L2*Z(31)-L6*Z(42)-L8*Z(27)) + MF*(L2
     &*Z(31)-L6*Z(42)-L8*Z(27)) + MG*(L2*Z(31)-L6*Z(42)-L8*Z(27)) + 0.5D
     &0*MB*(2*L2*Z(31)-L3*Z(38)-L4*Z(42))
      Z(343) = Z(271)*Z(31) + MC*(L2*Z(31)-L6*Z(42)-L7*Z(27)) + 0.5D0*MB
     &*(2*L2*Z(31)-L3*Z(38)-L4*Z(42)) + MD*(L2*Z(31)-L6*Z(42)-L8*Z(27)-L
     &9*Z(46)) + ME*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)) + MF*(L2*Z(31
     &)-L10*Z(46)-L6*Z(42)-L8*Z(27)) + MG*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L
     &8*Z(27))
      Z(388) = Z(387) + Z(275)*(L2-L6*Z(3)) + Z(278)*(L2-L6*Z(3)) + Z(28
     &2)*(L2-L6*Z(3)) + Z(288)*(L2-L6*Z(3)) + Z(295)*(L2-L6*Z(3)) + 0.5D
     &0*Z(272)*(2*L2-L3*Z(32)-L4*Z(3))
      Z(389) = Z(387) + Z(275)*(L2-L6*Z(3)-L7*Z(18)) + Z(278)*(L2-L6*Z(3
     &)-L8*Z(18)) + Z(282)*(L2-L6*Z(3)-L8*Z(18)) + Z(288)*(L2-L6*Z(3)-L8
     &*Z(18)) + Z(295)*(L2-L6*Z(3)-L8*Z(18)) + 0.5D0*Z(272)*(2*L2-L3*Z(3
     &2)-L4*Z(3))
      Z(390) = Z(387) + Z(275)*(L2-L6*Z(3)-L7*Z(18)) + 0.5D0*Z(272)*(2*L
     &2-L3*Z(32)-L4*Z(3)) + Z(278)*(L2-L6*Z(3)-L8*Z(18)-L9*Z(158)) + Z(2
     &82)*(L2-L10*Z(158)-L6*Z(3)-L8*Z(18)) + Z(288)*(L2-L10*Z(158)-L6*Z(
     &3)-L8*Z(18)) + Z(295)*(L2-L10*Z(158)-L6*Z(3)-L8*Z(18))
      Z(394) = Z(392) + MC*(Z(393)-2*Z(349)*Z(3)) + MD*(Z(393)-2*Z(349)*
     &Z(3)) + ME*(Z(393)-2*Z(349)*Z(3)) + MF*(Z(393)-2*Z(349)*Z(3)) + MG
     &*(Z(393)-2*Z(349)*Z(3)) + 0.25D0*MB*(Z(346)-4*Z(347)*Z(32)-4*Z(348
     &)*Z(3))
      Z(395) = Z(392) + 0.25D0*MB*(Z(346)-4*Z(347)*Z(32)-4*Z(348)*Z(3)) 
     &- MC*(Z(350)*Z(18)+2*Z(349)*Z(3)-Z(351)-Z(352)-Z(354)*Z(5)) - MD*(
     &Z(355)*Z(18)+2*Z(349)*Z(3)-Z(351)-Z(352)-Z(359)*Z(5)) - ME*(Z(355)
     &*Z(18)+2*Z(349)*Z(3)-Z(351)-Z(352)-Z(359)*Z(5)) - MF*(Z(355)*Z(18)
     &+2*Z(349)*Z(3)-Z(351)-Z(352)-Z(359)*Z(5)) - MG*(Z(355)*Z(18)+2*Z(3
     &49)*Z(3)-Z(351)-Z(352)-Z(359)*Z(5))
      Z(396) = Z(392) + 0.25D0*MB*(Z(346)-4*Z(347)*Z(32)-4*Z(348)*Z(3)) 
     &- MC*(Z(350)*Z(18)+2*Z(349)*Z(3)-Z(351)-Z(352)-Z(354)*Z(5)) - MD*(
     &Z(355)*Z(18)+Z(356)*Z(158)+2*Z(349)*Z(3)-Z(351)-Z(352)-Z(359)*Z(5)
     &-Z(360)*Z(162)) - ME*(Z(355)*Z(18)+Z(362)*Z(158)+2*Z(349)*Z(3)-Z(3
     &51)-Z(352)-Z(359)*Z(5)-Z(366)*Z(162)) - MF*(Z(355)*Z(18)+Z(362)*Z(
     &158)+2*Z(349)*Z(3)-Z(351)-Z(352)-Z(359)*Z(5)-Z(366)*Z(162)) - MG*(
     &Z(355)*Z(18)+Z(362)*Z(158)+2*Z(349)*Z(3)-Z(351)-Z(352)-Z(359)*Z(5)
     &-Z(366)*Z(162))
      Z(399) = Z(398) + 0.25D0*MB*(Z(346)-4*Z(347)*Z(32)-4*Z(348)*Z(3)) 
     &- MC*(2*Z(349)*Z(3)+2*Z(350)*Z(18)-Z(351)-Z(352)-Z(353)-2*Z(354)*Z
     &(5)) - MD*(2*Z(349)*Z(3)+2*Z(355)*Z(18)-Z(351)-Z(352)-Z(357)-2*Z(3
     &59)*Z(5)) - ME*(2*Z(349)*Z(3)+2*Z(355)*Z(18)-Z(351)-Z(352)-Z(357)-
     &2*Z(359)*Z(5)) - MF*(2*Z(349)*Z(3)+2*Z(355)*Z(18)-Z(351)-Z(352)-Z(
     &357)-2*Z(359)*Z(5)) - MG*(2*Z(349)*Z(3)+2*Z(355)*Z(18)-Z(351)-Z(35
     &2)-Z(357)-2*Z(359)*Z(5))
      Z(400) = Z(398) + 0.25D0*MB*(Z(346)-4*Z(347)*Z(32)-4*Z(348)*Z(3)) 
     &- MC*(2*Z(349)*Z(3)+2*Z(350)*Z(18)-Z(351)-Z(352)-Z(353)-2*Z(354)*Z
     &(5)) - MD*(Z(356)*Z(158)+2*Z(349)*Z(3)+2*Z(355)*Z(18)-Z(351)-Z(352
     &)-Z(357)-2*Z(359)*Z(5)-Z(360)*Z(162)-Z(361)*Z(7)) - ME*(Z(362)*Z(1
     &58)+2*Z(349)*Z(3)+2*Z(355)*Z(18)-Z(351)-Z(352)-Z(357)-2*Z(359)*Z(5
     &)-Z(366)*Z(162)-Z(367)*Z(7)) - MF*(Z(362)*Z(158)+2*Z(349)*Z(3)+2*Z
     &(355)*Z(18)-Z(351)-Z(352)-Z(357)-2*Z(359)*Z(5)-Z(366)*Z(162)-Z(367
     &)*Z(7)) - MG*(Z(362)*Z(158)+2*Z(349)*Z(3)+2*Z(355)*Z(18)-Z(351)-Z(
     &352)-Z(357)-2*Z(359)*Z(5)-Z(366)*Z(162)-Z(367)*Z(7))
      Z(403) = Z(402) + 0.25D0*MB*(Z(346)-4*Z(347)*Z(32)-4*Z(348)*Z(3)) 
     &- MC*(2*Z(349)*Z(3)+2*Z(350)*Z(18)-Z(351)-Z(352)-Z(353)-2*Z(354)*Z
     &(5)) - MD*(2*Z(349)*Z(3)+2*Z(355)*Z(18)+2*Z(356)*Z(158)-Z(351)-Z(3
     &52)-Z(357)-Z(358)-2*Z(359)*Z(5)-2*Z(360)*Z(162)-2*Z(361)*Z(7)) - M
     &E*(2*Z(349)*Z(3)+2*Z(355)*Z(18)+2*Z(362)*Z(158)-Z(351)-Z(352)-Z(35
     &7)-Z(364)-2*Z(359)*Z(5)-2*Z(366)*Z(162)-2*Z(367)*Z(7)) - MF*(2*Z(3
     &49)*Z(3)+2*Z(355)*Z(18)+2*Z(362)*Z(158)-Z(351)-Z(352)-Z(357)-Z(364
     &)-2*Z(359)*Z(5)-2*Z(366)*Z(162)-2*Z(367)*Z(7)) - MG*(2*Z(349)*Z(3)
     &+2*Z(355)*Z(18)+2*Z(362)*Z(158)-Z(351)-Z(352)-Z(357)-Z(364)-2*Z(35
     &9)*Z(5)-2*Z(366)*Z(162)-2*Z(367)*Z(7))

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
      Z(163) = Z(28)*Z(47) + Z(29)*Z(48)
      Z(164) = Z(28)*Z(49) + Z(29)*Z(47)
      Z(165) = Z(30)*Z(47) + Z(31)*Z(48)
      Z(166) = Z(30)*Z(49) + Z(31)*Z(47)
      Z(167) = Z(3)*Z(163) + Z(4)*Z(165)
      Z(168) = Z(3)*Z(164) + Z(4)*Z(166)
      Z(169) = Z(3)*Z(165) - Z(4)*Z(163)
      Z(170) = Z(3)*Z(166) - Z(4)*Z(164)
      Z(171) = Z(5)*Z(167) + Z(6)*Z(169)
      Z(172) = Z(5)*Z(168) + Z(6)*Z(170)
      Z(173) = Z(5)*Z(169) - Z(6)*Z(167)
      Z(174) = Z(5)*Z(170) - Z(6)*Z(168)
      Z(176) = Z(7)*Z(172) + Z(8)*Z(174)
      Z(177) = Z(7)*Z(173) - Z(8)*Z(171)
      Z(178) = Z(7)*Z(174) - Z(8)*Z(172)
      Z(179) = Z(28)*Z(50) + Z(29)*Z(51)
      Z(180) = Z(28)*Z(52) + Z(29)*Z(53)
      Z(181) = Z(30)*Z(50) + Z(31)*Z(51)
      Z(182) = Z(30)*Z(52) + Z(31)*Z(53)
      Z(183) = Z(3)*Z(179) + Z(4)*Z(181)
      Z(184) = Z(3)*Z(180) + Z(4)*Z(182)
      Z(185) = Z(3)*Z(181) - Z(4)*Z(179)
      Z(186) = Z(3)*Z(182) - Z(4)*Z(180)
      Z(187) = Z(5)*Z(183) + Z(6)*Z(185)
      Z(188) = Z(5)*Z(184) + Z(6)*Z(186)
      Z(189) = Z(5)*Z(185) - Z(6)*Z(183)
      Z(190) = Z(5)*Z(186) - Z(6)*Z(184)
      Z(192) = Z(7)*Z(188) + Z(8)*Z(190)
      Z(193) = Z(7)*Z(189) - Z(8)*Z(187)
      Z(194) = Z(7)*Z(190) - Z(8)*Z(188)
      Z(305) = HTOR + Z(152) + Z(153) + Z(154) - MTOR - Z(304)*Z(31) - L
     &2*(RX2*Z(30)+RY2*Z(31)) - Z(147)*(L2*Z(31)-L6*Z(42)-L7*Z(27)) - 0.
     &5D0*Z(146)*(2*L2*Z(31)-L3*Z(38)-L4*Z(42)) - Z(148)*(L2*Z(31)-L6*Z(
     &42)-L8*Z(27)-L9*Z(46)) - Z(149)*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(
     &27)-Z(17)*Z(47)) - Z(151)*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)-GS
     &*Z(1)) - Z(150)*(L2*Z(31)-L10*Z(46)-L10*Z(47)-L12*Z(53)-L6*Z(42)-L
     &8*Z(27))
      Z(313) = MTOR + Z(304)*Z(31) + L2*(RX2*Z(30)+RY2*Z(31)) + Z(147)*(
     &L2*Z(31)-L6*Z(42)) + Z(148)*(L2*Z(31)-L6*Z(42)) + Z(149)*(L2*Z(31)
     &-L6*Z(42)) + Z(150)*(L2*Z(31)-L6*Z(42)) + Z(151)*(L2*Z(31)-L6*Z(42
     &)) + 0.5D0*Z(146)*(2*L2*Z(31)-L3*Z(38)-L4*Z(42)) - Z(152)
      Z(314) = MTOR + Z(304)*Z(31) + L2*(RX2*Z(30)+RY2*Z(31)) + Z(147)*(
     &L2*Z(31)-L6*Z(42)-L7*Z(27)) + Z(148)*(L2*Z(31)-L6*Z(42)-L8*Z(27)) 
     &+ Z(149)*(L2*Z(31)-L6*Z(42)-L8*Z(27)) + Z(150)*(L2*Z(31)-L6*Z(42)-
     &L8*Z(27)) + Z(151)*(L2*Z(31)-L6*Z(42)-L8*Z(27)) + 0.5D0*Z(146)*(2*
     &L2*Z(31)-L3*Z(38)-L4*Z(42)) - Z(152) - Z(153)
      Z(315) = MTOR + Z(304)*Z(31) + L2*(RX2*Z(30)+RY2*Z(31)) + Z(147)*(
     &L2*Z(31)-L6*Z(42)-L7*Z(27)) + 0.5D0*Z(146)*(2*L2*Z(31)-L3*Z(38)-L4
     &*Z(42)) + Z(148)*(L2*Z(31)-L6*Z(42)-L8*Z(27)-L9*Z(46)) + Z(149)*(L
     &2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)) + Z(150)*(L2*Z(31)-L10*Z(46)-
     &L6*Z(42)-L8*Z(27)) + Z(151)*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27))
     & - Z(152) - Z(153) - Z(154)
      Z(329) = IE*EApp
      Z(331) = IF*Z(88)
      Z(332) = MG*(L10*Z(45)+L6*Z(41)+L8*Z(26)-L2*Z(30)-GS*Z(2)) - Z(271
     &)*Z(30) - MC*(L2*Z(30)-L6*Z(41)-L7*Z(26)) - 0.5D0*MB*(2*L2*Z(30)-L
     &3*Z(37)-L4*Z(41)) - MD*(L2*Z(30)-L6*Z(41)-L8*Z(26)-L9*Z(45)) - ME*
     &(L2*Z(30)-L10*Z(45)-L6*Z(41)-L8*Z(26)-Z(17)*Z(49)) - MF*(L2*Z(30)-
     &L10*Z(45)-L10*Z(49)-L12*Z(52)-L6*Z(41)-L8*Z(26))
      Z(338) = MA*Z(28)*Z(107) + MC*(Z(28)*Z(109)-Z(24)*Z(119)-Z(39)*Z(1
     &17)) + 0.5D0*MB*(2*Z(28)*Z(109)-Z(35)*Z(113)-Z(39)*Z(112)) + MD*(Z
     &(28)*Z(109)-Z(24)*Z(121)-Z(39)*Z(117)-Z(43)*Z(123)) + MG*(Z(1)*Z(1
     &41)+Z(28)*Z(109)-Z(2)*Z(142)-Z(24)*Z(121)-Z(39)*Z(117)-Z(43)*Z(125
     &)) + ME*(Z(28)*Z(109)-Z(127)*Z(49)-Z(24)*Z(121)-Z(39)*Z(117)-Z(43)
     &*Z(125)-Z(47)*Z(128)) + MF*(Z(28)*Z(109)-Z(130)*Z(49)-Z(133)*Z(52)
     &-Z(24)*Z(121)-Z(39)*Z(117)-Z(43)*Z(125)-Z(47)*Z(131)-Z(50)*Z(135))
      Z(339) = -Z(271)*Z(31) - MC*(L2*Z(31)-L6*Z(42)-L7*Z(27)) - 0.5D0*M
     &B*(2*L2*Z(31)-L3*Z(38)-L4*Z(42)) - MD*(L2*Z(31)-L6*Z(42)-L8*Z(27)-
     &L9*Z(46)) - ME*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)-Z(17)*Z(47)) 
     &- MG*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)-GS*Z(1)) - MF*(L2*Z(31)
     &-L10*Z(46)-L10*Z(47)-L12*Z(53)-L6*Z(42)-L8*Z(27))
      Z(344) = MA*Z(29)*Z(107) + MC*(Z(29)*Z(109)-Z(25)*Z(119)-Z(40)*Z(1
     &17)) + 0.5D0*MB*(2*Z(29)*Z(109)-Z(36)*Z(113)-Z(40)*Z(112)) + MD*(Z
     &(29)*Z(109)-Z(25)*Z(121)-Z(40)*Z(117)-Z(44)*Z(123)) + MG*(Z(1)*Z(1
     &42)+Z(2)*Z(141)+Z(29)*Z(109)-Z(25)*Z(121)-Z(40)*Z(117)-Z(44)*Z(125
     &)) + ME*(Z(29)*Z(109)-Z(127)*Z(47)-Z(25)*Z(121)-Z(40)*Z(117)-Z(44)
     &*Z(125)-Z(48)*Z(128)) + MF*(Z(29)*Z(109)-Z(130)*Z(47)-Z(133)*Z(53)
     &-Z(25)*Z(121)-Z(40)*Z(117)-Z(44)*Z(125)-Z(48)*Z(131)-Z(51)*Z(135))
      Z(376) = Z(345) + 0.25D0*MB*(Z(346)-4*Z(347)*Z(32)-4*Z(348)*Z(3)) 
     &- MC*(2*Z(349)*Z(3)+2*Z(350)*Z(18)-Z(351)-Z(352)-Z(353)-2*Z(354)*Z
     &(5)) - MD*(2*Z(349)*Z(3)+2*Z(355)*Z(18)+2*Z(356)*Z(158)-Z(351)-Z(3
     &52)-Z(357)-Z(358)-2*Z(359)*Z(5)-2*Z(360)*Z(162)-2*Z(361)*Z(7)) - M
     &E*(2*Z(349)*Z(3)+2*Z(355)*Z(18)+2*Z(362)*Z(158)+2*Z(363)*Z(166)-Z(
     &351)-Z(352)-Z(357)-Z(364)-Z(365)-2*Z(359)*Z(5)-2*Z(366)*Z(162)-2*Z
     &(367)*Z(7)-2*Z(368)*Z(178)-2*Z(369)*Z(170)-2*Z(370)*Z(174)) - MG*(
     &2*Z(349)*Z(3)+2*Z(355)*Z(18)+2*Z(362)*Z(158)+2*L2*GS*Z(79)-Z(351)-
     &Z(352)-Z(357)-Z(364)-GS**2-2*Z(359)*Z(5)-2*Z(366)*Z(162)-2*Z(367)*
     &Z(7)-2*L10*GS*Z(9)-2*L6*GS*Z(84)-2*L8*GS*Z(21)) - MF*(2*Z(371)*Z(1
     &3)+2*Z(349)*Z(3)+2*Z(355)*Z(18)+2*Z(362)*Z(158)+2*Z(362)*Z(166)+2*
     &Z(372)*Z(182)-2*Z(364)-Z(351)-Z(352)-Z(357)-Z(373)-2*Z(359)*Z(5)-2
     &*Z(364)*Z(178)-2*Z(366)*Z(162)-2*Z(366)*Z(170)-2*Z(367)*Z(7)-2*Z(3
     &67)*Z(174)-2*Z(371)*Z(194)-2*Z(374)*Z(186)-2*Z(375)*Z(190))
      Z(378) = Z(377) - Z(275)*(L2-L6*Z(3)-L7*Z(18)) - 0.5D0*Z(272)*(2*L
     &2-L3*Z(32)-L4*Z(3)) - Z(278)*(L2-L6*Z(3)-L8*Z(18)-L9*Z(158)) - Z(2
     &82)*(L2-L10*Z(158)-L6*Z(3)-L8*Z(18)-Z(17)*Z(166)) - Z(295)*(L2-L10
     &*Z(158)-L6*Z(3)-L8*Z(18)-GS*Z(79)) - Z(288)*(L2-L10*Z(158)-L10*Z(1
     &66)-L12*Z(182)-L6*Z(3)-L8*Z(18))
      Z(380) = MC*(Z(350)*Z(18)+2*Z(349)*Z(3)-Z(351)-Z(352)-Z(354)*Z(5))
     & + MD*(Z(355)*Z(18)+Z(356)*Z(158)+2*Z(349)*Z(3)-Z(351)-Z(352)-Z(35
     &9)*Z(5)-Z(360)*Z(162)) + ME*(Z(355)*Z(18)+Z(362)*Z(158)+Z(363)*Z(1
     &66)+2*Z(349)*Z(3)-Z(351)-Z(352)-Z(359)*Z(5)-Z(366)*Z(162)-Z(369)*Z
     &(170)) + MG*(Z(355)*Z(18)+Z(362)*Z(158)+2*Z(349)*Z(3)+L2*GS*Z(79)-
     &Z(351)-Z(352)-Z(359)*Z(5)-Z(366)*Z(162)-L6*GS*Z(84)) + MF*(Z(355)*
     &Z(18)+Z(362)*Z(158)+Z(362)*Z(166)+Z(372)*Z(182)+2*Z(349)*Z(3)-Z(35
     &1)-Z(352)-Z(359)*Z(5)-Z(366)*Z(162)-Z(366)*Z(170)-Z(374)*Z(186)) -
     & IA - IB - Z(379) - 0.25D0*MB*(Z(346)-4*Z(347)*Z(32)-4*Z(348)*Z(3)
     &)
      Z(381) = MC*(2*Z(349)*Z(3)+2*Z(350)*Z(18)-Z(351)-Z(352)-Z(353)-2*Z
     &(354)*Z(5)) + MD*(Z(356)*Z(158)+2*Z(349)*Z(3)+2*Z(355)*Z(18)-Z(351
     &)-Z(352)-Z(357)-2*Z(359)*Z(5)-Z(360)*Z(162)-Z(361)*Z(7)) + ME*(Z(3
     &62)*Z(158)+Z(363)*Z(166)+2*Z(349)*Z(3)+2*Z(355)*Z(18)-Z(351)-Z(352
     &)-Z(357)-2*Z(359)*Z(5)-Z(366)*Z(162)-Z(367)*Z(7)-Z(369)*Z(170)-Z(3
     &70)*Z(174)) + MG*(Z(362)*Z(158)+2*Z(349)*Z(3)+2*Z(355)*Z(18)+L2*GS
     &*Z(79)-Z(351)-Z(352)-Z(357)-2*Z(359)*Z(5)-Z(366)*Z(162)-Z(367)*Z(7
     &)-L6*GS*Z(84)-L8*GS*Z(21)) + MF*(Z(362)*Z(158)+Z(362)*Z(166)+Z(372
     &)*Z(182)+2*Z(349)*Z(3)+2*Z(355)*Z(18)-Z(351)-Z(352)-Z(357)-2*Z(359
     &)*Z(5)-Z(366)*Z(162)-Z(366)*Z(170)-Z(367)*Z(7)-Z(367)*Z(174)-Z(374
     &)*Z(186)-Z(375)*Z(190)) - IA - IB - IC - Z(379) - 0.25D0*MB*(Z(346
     &)-4*Z(347)*Z(32)-4*Z(348)*Z(3))
      Z(382) = MC*(2*Z(349)*Z(3)+2*Z(350)*Z(18)-Z(351)-Z(352)-Z(353)-2*Z
     &(354)*Z(5)) + MD*(2*Z(349)*Z(3)+2*Z(355)*Z(18)+2*Z(356)*Z(158)-Z(3
     &51)-Z(352)-Z(357)-Z(358)-2*Z(359)*Z(5)-2*Z(360)*Z(162)-2*Z(361)*Z(
     &7)) + ME*(Z(363)*Z(166)+2*Z(349)*Z(3)+2*Z(355)*Z(18)+2*Z(362)*Z(15
     &8)-Z(351)-Z(352)-Z(357)-Z(364)-2*Z(359)*Z(5)-2*Z(366)*Z(162)-2*Z(3
     &67)*Z(7)-Z(368)*Z(178)-Z(369)*Z(170)-Z(370)*Z(174)) + MG*(2*Z(349)
     &*Z(3)+2*Z(355)*Z(18)+2*Z(362)*Z(158)+L2*GS*Z(79)-Z(351)-Z(352)-Z(3
     &57)-Z(364)-2*Z(359)*Z(5)-2*Z(366)*Z(162)-2*Z(367)*Z(7)-L10*GS*Z(9)
     &-L6*GS*Z(84)-L8*GS*Z(21)) + MF*(Z(362)*Z(166)+Z(372)*Z(182)+2*Z(34
     &9)*Z(3)+2*Z(355)*Z(18)+2*Z(362)*Z(158)-Z(351)-Z(352)-Z(357)-Z(364)
     &-2*Z(359)*Z(5)-2*Z(366)*Z(162)-2*Z(367)*Z(7)-Z(364)*Z(178)-Z(366)*
     &Z(170)-Z(367)*Z(174)-Z(371)*Z(194)-Z(374)*Z(186)-Z(375)*Z(190)) - 
     &IA - IB - IC - ID - Z(379) - 0.25D0*MB*(Z(346)-4*Z(347)*Z(32)-4*Z(
     &348)*Z(3))
      Z(385) = Z(331) + 0.25D0*MB*(Z(383)*Z(112)+2*L2*Z(4)*Z(112)+2*L2*Z
     &(33)*Z(113)+2*L3*Z(34)*Z(109)-Z(384)*Z(113)-2*L4*Z(4)*Z(109)) + MD
     &*(L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(157)*Z(123)+L8*Z(6)*Z(117)+L
     &8*Z(19)*Z(109)+L9*Z(8)*Z(121)+L9*Z(156)*Z(109)-L6*Z(4)*Z(109)-L6*Z
     &(6)*Z(121)-L6*Z(161)*Z(123)-L8*Z(8)*Z(123)-L9*Z(160)*Z(117)) + MG*
     &(GS*Z(142)+L10*Z(8)*Z(121)+L10*Z(9)*Z(142)+L10*Z(10)*Z(141)+L10*Z(
     &156)*Z(109)+L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(157)*Z(125)+L6*Z(8
     &2)*Z(141)+L6*Z(84)*Z(142)+L8*Z(6)*Z(117)+L8*Z(19)*Z(109)+L8*Z(21)*
     &Z(142)+L8*Z(23)*Z(141)+GS*Z(10)*Z(125)+GS*Z(78)*Z(109)-L10*Z(160)*
     &Z(117)-L2*Z(77)*Z(141)-L2*Z(79)*Z(142)-L6*Z(4)*Z(109)-L6*Z(6)*Z(12
     &1)-L6*Z(161)*Z(125)-L8*Z(8)*Z(125)-GS*Z(22)*Z(121)-GS*Z(83)*Z(117)
     &) + ME*(L2*Z(127)*Z(166)+L10*Z(8)*Z(121)+L10*Z(156)*Z(109)+L2*Z(4)
     &*Z(117)+L2*Z(20)*Z(121)+L2*Z(157)*Z(125)+L2*Z(165)*Z(128)+L8*Z(6)*
     &Z(117)+L8*Z(19)*Z(109)+Z(17)*Z(164)*Z(109)-Z(17)*Z(127)-L10*Z(127)
     &*Z(178)-L6*Z(127)*Z(170)-L8*Z(127)*Z(174)-L10*Z(160)*Z(117)-L10*Z(
     &177)*Z(128)-L6*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(161)*Z(125)-L6*Z(16
     &9)*Z(128)-L8*Z(8)*Z(125)-L8*Z(173)*Z(128)-Z(17)*Z(168)*Z(117)-Z(17
     &)*Z(172)*Z(121)-Z(17)*Z(176)*Z(125)) + MF*(L10*Z(13)*Z(133)+L12*Z(
     &13)*Z(130)+L2*Z(130)*Z(166)+L2*Z(133)*Z(182)+L10*Z(14)*Z(135)+L10*
     &Z(8)*Z(121)+L10*Z(156)*Z(109)+L10*Z(164)*Z(109)+L12*Z(180)*Z(109)+
     &L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(157)*Z(125)+L2*Z(165)*Z(131)+L
     &2*Z(181)*Z(135)+L8*Z(6)*Z(117)+L8*Z(19)*Z(109)-L10*Z(130)-L12*Z(13
     &3)-L10*Z(130)*Z(178)-L10*Z(133)*Z(194)-L6*Z(130)*Z(170)-L6*Z(133)*
     &Z(186)-L8*Z(130)*Z(174)-L8*Z(133)*Z(190)-L10*Z(160)*Z(117)-L10*Z(1
     &68)*Z(117)-L10*Z(172)*Z(121)-L10*Z(176)*Z(125)-L10*Z(177)*Z(131)-L
     &10*Z(193)*Z(135)-L12*Z(14)*Z(131)-L12*Z(184)*Z(117)-L12*Z(188)*Z(1
     &21)-L12*Z(192)*Z(125)-L6*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(161)*Z(12
     &5)-L6*Z(169)*Z(131)-L6*Z(185)*Z(135)-L8*Z(8)*Z(125)-L8*Z(173)*Z(13
     &1)-L8*Z(189)*Z(135)) - Z(329) - MC*(L6*Z(4)*Z(109)+L6*Z(6)*Z(119)-
     &L2*Z(4)*Z(117)-L2*Z(20)*Z(119)-L7*Z(6)*Z(117)-L7*Z(19)*Z(109))
      Z(391) = L2*(2*MG*(Z(77)*Z(141)+Z(79)*Z(142)-Z(4)*Z(117)-Z(20)*Z(1
     &21)-Z(157)*Z(125))-2*MC*(Z(4)*Z(117)+Z(20)*Z(119))-MB*(Z(4)*Z(112)
     &+Z(33)*Z(113))-2*MD*(Z(4)*Z(117)+Z(20)*Z(121)+Z(157)*Z(123))-2*ME*
     &(Z(127)*Z(166)+Z(4)*Z(117)+Z(20)*Z(121)+Z(157)*Z(125)+Z(165)*Z(128
     &))-2*MF*(Z(130)*Z(166)+Z(133)*Z(182)+Z(4)*Z(117)+Z(20)*Z(121)+Z(15
     &7)*Z(125)+Z(165)*Z(131)+Z(181)*Z(135)))
      Z(397) = -MC*(L2*Z(4)*Z(117)+L2*Z(20)*Z(119)-L6*Z(4)*Z(109)-L6*Z(6
     &)*Z(119)) - MD*(L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(157)*Z(123)-L6
     &*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(161)*Z(123)) - 0.25D0*MB*(Z(383)*
     &Z(112)+2*L2*Z(4)*Z(112)+2*L2*Z(33)*Z(113)+2*L3*Z(34)*Z(109)-Z(384)
     &*Z(113)-2*L4*Z(4)*Z(109)) - ME*(L2*Z(127)*Z(166)+L2*Z(4)*Z(117)+L2
     &*Z(20)*Z(121)+L2*Z(157)*Z(125)+L2*Z(165)*Z(128)-L6*Z(127)*Z(170)-L
     &6*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(161)*Z(125)-L6*Z(169)*Z(128)) - 
     &MG*(L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(157)*Z(125)+L6*Z(82)*Z(141
     &)+L6*Z(84)*Z(142)-L2*Z(77)*Z(141)-L2*Z(79)*Z(142)-L6*Z(4)*Z(109)-L
     &6*Z(6)*Z(121)-L6*Z(161)*Z(125)) - MF*(L2*Z(130)*Z(166)+L2*Z(133)*Z
     &(182)+L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(157)*Z(125)+L2*Z(165)*Z(
     &131)+L2*Z(181)*Z(135)-L6*Z(130)*Z(170)-L6*Z(133)*Z(186)-L6*Z(4)*Z(
     &109)-L6*Z(6)*Z(121)-L6*Z(161)*Z(125)-L6*Z(169)*Z(131)-L6*Z(185)*Z(
     &135))
      Z(401) = MC*(L6*Z(4)*Z(109)+L6*Z(6)*Z(119)-L2*Z(4)*Z(117)-L2*Z(20)
     &*Z(119)-L7*Z(6)*Z(117)-L7*Z(19)*Z(109)) + MD*(L6*Z(4)*Z(109)+L6*Z(
     &6)*Z(121)+L6*Z(161)*Z(123)+L8*Z(8)*Z(123)-L2*Z(4)*Z(117)-L2*Z(20)*
     &Z(121)-L2*Z(157)*Z(123)-L8*Z(6)*Z(117)-L8*Z(19)*Z(109)) + MG*(L2*Z
     &(77)*Z(141)+L2*Z(79)*Z(142)+L6*Z(4)*Z(109)+L6*Z(6)*Z(121)+L6*Z(161
     &)*Z(125)+L8*Z(8)*Z(125)-L2*Z(4)*Z(117)-L2*Z(20)*Z(121)-L2*Z(157)*Z
     &(125)-L6*Z(82)*Z(141)-L6*Z(84)*Z(142)-L8*Z(6)*Z(117)-L8*Z(19)*Z(10
     &9)-L8*Z(21)*Z(142)-L8*Z(23)*Z(141)) - 0.25D0*MB*(Z(383)*Z(112)+2*L
     &2*Z(4)*Z(112)+2*L2*Z(33)*Z(113)+2*L3*Z(34)*Z(109)-Z(384)*Z(113)-2*
     &L4*Z(4)*Z(109)) - ME*(L2*Z(127)*Z(166)+L2*Z(4)*Z(117)+L2*Z(20)*Z(1
     &21)+L2*Z(157)*Z(125)+L2*Z(165)*Z(128)+L8*Z(6)*Z(117)+L8*Z(19)*Z(10
     &9)-L6*Z(127)*Z(170)-L8*Z(127)*Z(174)-L6*Z(4)*Z(109)-L6*Z(6)*Z(121)
     &-L6*Z(161)*Z(125)-L6*Z(169)*Z(128)-L8*Z(8)*Z(125)-L8*Z(173)*Z(128)
     &) - MF*(L2*Z(130)*Z(166)+L2*Z(133)*Z(182)+L2*Z(4)*Z(117)+L2*Z(20)*
     &Z(121)+L2*Z(157)*Z(125)+L2*Z(165)*Z(131)+L2*Z(181)*Z(135)+L8*Z(6)*
     &Z(117)+L8*Z(19)*Z(109)-L6*Z(130)*Z(170)-L6*Z(133)*Z(186)-L8*Z(130)
     &*Z(174)-L8*Z(133)*Z(190)-L6*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(161)*Z
     &(125)-L6*Z(169)*Z(131)-L6*Z(185)*Z(135)-L8*Z(8)*Z(125)-L8*Z(173)*Z
     &(131)-L8*Z(189)*Z(135))
      Z(404) = MC*(L6*Z(4)*Z(109)+L6*Z(6)*Z(119)-L2*Z(4)*Z(117)-L2*Z(20)
     &*Z(119)-L7*Z(6)*Z(117)-L7*Z(19)*Z(109)) + MG*(L10*Z(160)*Z(117)+L2
     &*Z(77)*Z(141)+L2*Z(79)*Z(142)+L6*Z(4)*Z(109)+L6*Z(6)*Z(121)+L6*Z(1
     &61)*Z(125)+L8*Z(8)*Z(125)-L10*Z(8)*Z(121)-L10*Z(9)*Z(142)-L10*Z(10
     &)*Z(141)-L10*Z(156)*Z(109)-L2*Z(4)*Z(117)-L2*Z(20)*Z(121)-L2*Z(157
     &)*Z(125)-L6*Z(82)*Z(141)-L6*Z(84)*Z(142)-L8*Z(6)*Z(117)-L8*Z(19)*Z
     &(109)-L8*Z(21)*Z(142)-L8*Z(23)*Z(141)) - 0.25D0*MB*(Z(383)*Z(112)+
     &2*L2*Z(4)*Z(112)+2*L2*Z(33)*Z(113)+2*L3*Z(34)*Z(109)-Z(384)*Z(113)
     &-2*L4*Z(4)*Z(109)) - MD*(L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(157)*
     &Z(123)+L8*Z(6)*Z(117)+L8*Z(19)*Z(109)+L9*Z(8)*Z(121)+L9*Z(156)*Z(1
     &09)-L6*Z(4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(161)*Z(123)-L8*Z(8)*Z(123)-
     &L9*Z(160)*Z(117)) - ME*(L2*Z(127)*Z(166)+L10*Z(8)*Z(121)+L10*Z(156
     &)*Z(109)+L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L2*Z(157)*Z(125)+L2*Z(165)
     &*Z(128)+L8*Z(6)*Z(117)+L8*Z(19)*Z(109)-L10*Z(127)*Z(178)-L6*Z(127)
     &*Z(170)-L8*Z(127)*Z(174)-L10*Z(160)*Z(117)-L10*Z(177)*Z(128)-L6*Z(
     &4)*Z(109)-L6*Z(6)*Z(121)-L6*Z(161)*Z(125)-L6*Z(169)*Z(128)-L8*Z(8)
     &*Z(125)-L8*Z(173)*Z(128)) - MF*(L2*Z(130)*Z(166)+L2*Z(133)*Z(182)+
     &L10*Z(8)*Z(121)+L10*Z(156)*Z(109)+L2*Z(4)*Z(117)+L2*Z(20)*Z(121)+L
     &2*Z(157)*Z(125)+L2*Z(165)*Z(131)+L2*Z(181)*Z(135)+L8*Z(6)*Z(117)+L
     &8*Z(19)*Z(109)-L10*Z(130)*Z(178)-L10*Z(133)*Z(194)-L6*Z(130)*Z(170
     &)-L6*Z(133)*Z(186)-L8*Z(130)*Z(174)-L8*Z(133)*Z(190)-L10*Z(160)*Z(
     &117)-L10*Z(177)*Z(131)-L10*Z(193)*Z(135)-L6*Z(4)*Z(109)-L6*Z(6)*Z(
     &121)-L6*Z(161)*Z(125)-L6*Z(169)*Z(131)-L6*Z(185)*Z(135)-L8*Z(8)*Z(
     &125)-L8*Z(173)*Z(131)-L8*Z(189)*Z(135))
      Z(423) = Z(301) - Z(338)
      Z(424) = Z(303) - Z(344)
      Z(425) = Z(305) - Z(385)
      Z(426) = Z(312) - 0.5D0*Z(391)
      Z(427) = Z(313) - Z(397)
      Z(428) = Z(314) - Z(401)
      Z(429) = Z(315) - Z(404)

      COEF(1,1) = -MT
      COEF(1,2) = 0
      COEF(1,3) = -Z(332)
      COEF(1,4) = -Z(334)
      COEF(1,5) = -Z(335)
      COEF(1,6) = -Z(336)
      COEF(1,7) = -Z(337)
      COEF(2,1) = 0
      COEF(2,2) = -MT
      COEF(2,3) = -Z(339)
      COEF(2,4) = -Z(340)
      COEF(2,5) = -Z(341)
      COEF(2,6) = -Z(342)
      COEF(2,7) = -Z(343)
      COEF(3,1) = -Z(332)
      COEF(3,2) = -Z(339)
      COEF(3,3) = -Z(376)
      COEF(3,4) = -Z(378)
      COEF(3,5) = -Z(380)
      COEF(3,6) = -Z(381)
      COEF(3,7) = -Z(382)
      COEF(4,1) = -Z(334)
      COEF(4,2) = -Z(340)
      COEF(4,3) = -Z(378)
      COEF(4,4) = -Z(386)
      COEF(4,5) = -Z(388)
      COEF(4,6) = -Z(389)
      COEF(4,7) = -Z(390)
      COEF(5,1) = -Z(335)
      COEF(5,2) = -Z(341)
      COEF(5,3) = -Z(380)
      COEF(5,4) = -Z(388)
      COEF(5,5) = -Z(394)
      COEF(5,6) = -Z(395)
      COEF(5,7) = -Z(396)
      COEF(6,1) = -Z(336)
      COEF(6,2) = -Z(342)
      COEF(6,3) = -Z(381)
      COEF(6,4) = -Z(389)
      COEF(6,5) = -Z(395)
      COEF(6,6) = -Z(399)
      COEF(6,7) = -Z(400)
      COEF(7,1) = -Z(337)
      COEF(7,2) = -Z(343)
      COEF(7,3) = -Z(382)
      COEF(7,4) = -Z(390)
      COEF(7,5) = -Z(396)
      COEF(7,6) = -Z(400)
      COEF(7,7) = -Z(403)
      RHS(1) = -Z(423)
      RHS(2) = -Z(424)
      RHS(3) = -Z(425)
      RHS(4) = -Z(426)
      RHS(5) = -Z(427)
      RHS(6) = -Z(428)
      RHS(7) = -Z(429)
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


C**********************************************************************
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
     &MTPB,MTPK
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,Q7I,U1I,U2I,U3I,U4I,U5I,U
     &6I,U7I,POP1XI,POP1YI,POP2XI,POP2YI
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,COP,GRF,HANG,HANGVE
     &L,HETOR,HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,MANG,MANG
     &VEL,METOR,MFTOR,MTOR,PECM,PX,PY,RX,RX1,RX2,RY,RY1,RY2,SHANG,SHANGV
     &EL,SHTOR,SKANG,SKANGVEL,SKTOR,TE,U8,U9,Q1p,Q2p,Q3p,Q4p,Q5p,Q6p,Q7p
     &,U1p,U2p,U3p,U4p,U5p,U6p,U7p,AEACT,AECCANG,AECCANGVEL,AESECANG,AES
     &ECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,EA,FA,GS,HE
     &ACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,HFCCANGV
     &EL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,KESECANG
     &VEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,MEACT,MECCANG,ME
     &CCANGVEL,MESECANG,MESECANGVEL,MFACT,MFCCANG,MFCCANGVEL,MFSECANG,MF
     &SECANGVEL,EAp,FAp,GSp,EApp,FApp,GSpp,MT,POCMSTANCEX,POCMSTANCEY,PO
     &CMSWINGX,POCMSWINGY,POCMX,POCMY,POGOX,POGOY,POP1X,POP1Y,POP2X,POP2
     &Y,POP3X,POP3Y,POP4X,POP4Y,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POP8
     &X,POP8Y,POP9X,POP9Y,PSTANCEX,PSTANCEY,PSWINGX,PSWINGY,VOCMSTANCEX,
     &VOCMSTANCEY,VOCMSWINGX,VOCMSWINGY,VOCMX,VOCMY,VOP2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(431),COEF(7,7),RHS(7)
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

      Z(17) = L10 - L9
      Z(149) = G*ME
      Z(316) = Z(17)*Z(149)
      Z(293) = L12*MF
      Z(15) = COS(FOOTANG)
      Z(16) = SIN(FOOTANG)
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
      Z(150) = G*MF
      Z(151) = G*MG
      Z(198) = Z(54) - L1
      Z(199) = Z(54) - L2
      Z(200) = 0.5D0*L4 - 0.5D0*Z(55)
      Z(201) = 0.5D0*L3 - 0.5D0*Z(61)
      Z(217) = L6 - 0.5D0*Z(55)
      Z(218) = L7 - Z(56)
      Z(219) = L8 - Z(56)
      Z(220) = L9 - Z(57)
      Z(221) = L10 - Z(57)
      Z(222) = Z(17) - Z(58)
      Z(223) = L10 - Z(58)
      Z(224) = L12 - Z(59)
      Z(230) = Z(200) + Z(15)*Z(201)
      Z(232) = Z(201) + Z(15)*Z(200)
      Z(237) = Z(15)*Z(61)
      Z(271) = L1*MA
      Z(272) = L2*MB
      Z(273) = L4*MB
      Z(274) = L3*MB
      Z(275) = L2*MC
      Z(276) = L6*MC
      Z(277) = L7*MC
      Z(278) = L2*MD
      Z(279) = L6*MD
      Z(280) = L8*MD
      Z(281) = L9*MD
      Z(282) = L2*ME
      Z(283) = L6*ME
      Z(284) = L8*ME
      Z(285) = L10*ME
      Z(286) = ME*Z(17)
      Z(288) = L2*MF
      Z(289) = L6*MF
      Z(290) = L8*MF
      Z(291) = L10*MF
      Z(295) = L2*MG
      Z(296) = L6*MG
      Z(297) = L8*MG
      Z(298) = L10*MG
      Z(302) = Z(145) + Z(146) + Z(147) + Z(148) + Z(149) + Z(150) + Z(1
     &51)
      Z(304) = L1*Z(145)
      Z(306) = L2*Z(146)
      Z(307) = L2*Z(147)
      Z(308) = L2*Z(148)
      Z(309) = L2*Z(149)
      Z(310) = L2*Z(150)
      Z(311) = L2*Z(151)
      Z(318) = L12*Z(150)
      Z(333) = L1*MA + L2*MB + L2*MC + L2*MD + L2*ME + L2*MF + L2*MG
      Z(345) = IA + IB + IC + ID + IE + IF + IG + MA*L1**2
      Z(346) = L3**2 + L4**2 + 4*L2**2 + 2*L3*L4*Z(15)
      Z(347) = L2*L3
      Z(348) = L2*L4
      Z(349) = L2*L6
      Z(350) = L2*L7
      Z(351) = L2**2
      Z(352) = L6**2
      Z(353) = L7**2
      Z(354) = L6*L7
      Z(355) = L2*L8
      Z(356) = L2*L9
      Z(357) = L8**2
      Z(358) = L9**2
      Z(359) = L6*L8
      Z(360) = L6*L9
      Z(361) = L8*L9
      Z(362) = L10*L2
      Z(363) = L2*Z(17)
      Z(364) = L10**2
      Z(365) = Z(17)**2
      Z(366) = L10*L6
      Z(367) = L10*L8
      Z(368) = L10*Z(17)
      Z(369) = L6*Z(17)
      Z(370) = L8*Z(17)
      Z(371) = L10*L12
      Z(372) = L12*L2
      Z(373) = L12**2
      Z(374) = L12*L6
      Z(375) = L12*L8
      Z(377) = -IA - MA*L1**2
      Z(379) = MA*L1**2
      Z(383) = L3*Z(16)
      Z(384) = L4*Z(16)
      Z(386) = IA + MA*L1**2 + MB*L2**2 + MC*L2**2 + MD*L2**2 + ME*L2**2
     & + MF*L2**2 + MG*L2**2
      Z(387) = IA + MA*L1**2
      Z(392) = IA + IB + MA*L1**2
      Z(393) = L2**2 + L6**2
      Z(398) = IA + IB + IC + MA*L1**2
      Z(402) = IA + IB + IC + ID + MA*L1**2
      Z(405) = IE + IF
      Z(420) = L12*L2*MF

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
      POP1YI = Q2
      POP2XI = POP2X
      POP2YI = POP2Y

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
     &,AEACTP(NACTP),AFACTP(NACTP),METQP(10),MFTQP(10)
      COMMON/CONSTNTS/ FOOTANG,G,IA,IB,IC,ID,IE,IF,IG,K1,K2,K3,K4,K5,K6,
     &K7,K8,L1,L10,L11,L12,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,
     &MTPB,MTPK
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,Q6,Q7,U1,U2,U3,U4,U5,U6,U7
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,COP,GRF,HANG,HANGVE
     &L,HETOR,HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,MANG,MANG
     &VEL,METOR,MFTOR,MTOR,PECM,PX,PY,RX,RX1,RX2,RY,RY1,RY2,SHANG,SHANGV
     &EL,SHTOR,SKANG,SKANGVEL,SKTOR,TE,U8,U9,Q1p,Q2p,Q3p,Q4p,Q5p,Q6p,Q7p
     &,U1p,U2p,U3p,U4p,U5p,U6p,U7p,AEACT,AECCANG,AECCANGVEL,AESECANG,AES
     &ECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,EA,FA,GS,HE
     &ACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,HFCCANGV
     &EL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,KESECANG
     &VEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,MEACT,MECCANG,ME
     &CCANGVEL,MESECANG,MESECANGVEL,MFACT,MFCCANG,MFCCANGVEL,MFSECANG,MF
     &SECANGVEL,EAp,FAp,GSp,EApp,FApp,GSpp,MT,POCMSTANCEX,POCMSTANCEY,PO
     &CMSWINGX,POCMSWINGY,POCMX,POCMY,POGOX,POGOY,POP1X,POP1Y,POP2X,POP2
     &Y,POP3X,POP3Y,POP4X,POP4Y,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POP8
     &X,POP8Y,POP9X,POP9Y,PSTANCEX,PSTANCEY,PSWINGX,PSWINGY,VOCMSTANCEX,
     &VOCMSTANCEY,VOCMSWINGX,VOCMSWINGY,VOCMX,VOCMY,VOP2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(431),COEF(7,7),RHS(7)
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