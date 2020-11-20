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
     &K7,K8,L1,L10,L11,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,MTPB
     &,MTPK
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,Q7I,U1I,U2I,U3I,U4I,U5I,U
     &6I,U7I
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(430),COEF(7,7),RHS(7)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP,METQP,MFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW
      COMMON/DATAIN  / AERIALTIME,SWINGTIME
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
     &K2,K3,K4,K5,K6,K7,K8,L1,L10,L11,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,M
     &D,ME,MF,MG,MTPB,MTPK

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
     &K7,K8,L1,L10,L11,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,MTPB
     &,MTPK
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,Q7I,U1I,U2I,U3I,U4I,U5I,U
     &6I,U7I
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
     &X,POP8Y,POP9X,POP9Y,VOCMX,VOCMY,VOP2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(430),COEF(7,7),RHS(7)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP,METQP,MFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW
      COMMON/DATAIN  / AERIALTIME,SWINGTIME

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
      CMYTD = Q2 - 0.5*(2*MF*(L7-L8)*SIN(EA-FA-Q3)+2*(L10*MF+ME*(L10-L9)
     &)*SIN(EA-Q3)+2*(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF+L2*MG)*SIN(Q3-
     &Q4-Q5-Q6-Q7)-2*MG*GS*SIN(Q3)-L3*MB*SIN(FOOTANG+Q3-Q5-Q6-Q7)-2*(L10
     &*ME+L10*MF+L10*MG+L9*MD)*SIN(Q3-Q7)-2*(L7*MC+L8*MD+L8*ME+L8*MF+L8*
     &MG)*SIN(Q3-Q6-Q7)-(L4*MB+2*L6*MC+2*L6*MD+2*L6*ME+2*L6*MF+2*L6*MG)*
     &SIN(Q3-Q5-Q6-Q7))/(MA+MB+MC+MD+ME+MF+MG)
      VCMXI = U1 - 0.5*(2*MG*GS*SIN(Q3)*U3+2*(L10*ME+L10*MF+L10*MG+L9*MD
     &)*SIN(Q3-Q7)*(U3-U7)+2*(L10*MF+ME*(L10-L9))*SIN(EA-Q3)*(EAp-U3-U8)
     &+L3*MB*SIN(FOOTANG+Q3-Q5-Q6-Q7)*(U3-U5-U6-U7)+2*MF*(L7-L8)*SIN(EA-
     &FA-Q3)*(EAp-FAp-U3-U8-U9)+2*(L7*MC+L8*MD+L8*ME+L8*MF+L8*MG)*SIN(Q3
     &-Q6-Q7)*(U3-U6-U7)+(L4*MB+2*L6*MC+2*L6*MD+2*L6*ME+2*L6*MF+2*L6*MG)
     &*SIN(Q3-Q5-Q6-Q7)*(U3-U5-U6-U7)-2*MG*GSp*COS(Q3)-2*(L1*MA+L2*MB+L2
     &*MC+L2*MD+L2*ME+L2*MF+L2*MG)*SIN(Q3-Q4-Q5-Q6-Q7)*(U3-U4-U5-U6-U7))
     &/(MA+MB+MC+MD+ME+MF+MG)


C** Initialise torques for integration
      CALL UPDATE(T)

C**   Initalize numerical integrator with call to EQNS1 at T=TINITIAL
      CALL KUTTA(EQNS1, 14, VAR, T, INTEGSTP, ABSERR, RELERR, 0, *5920)

C**   Check exit conditions
5900  IF(TFINAL.GE.TINITIAL.AND.T+.01D0*INTEGSTP.GE.TFINAL) EXIT=.TRUE.
      IF(TFINAL.LE.TINITIAL.AND.T+.01D0*INTEGSTP.LE.TFINAL) EXIT=.TRUE.
      IF (Q2 .GT. 1.0D-05 .AND. POP2Y .GT. 1.0D-05) EXIT = .TRUE.

      IF (EXIT) THEN
        IDX = IDX - 1
        CMYTO = Q2 - 0.5*(2*MF*(L7-L8)*SIN(EA-FA-Q3)+2*(L10*MF+ME*(L10-L
     &  9))*SIN(EA-Q3)+2*(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF+L2*MG)*SIN
     &  (Q3-Q4-Q5-Q6-Q7)-2*MG*GS*SIN(Q3)-L3*MB*SIN(FOOTANG+Q3-Q5-Q6-Q7)-
     &  2*(L10*ME+L10*MF+L10*MG+L9*MD)*SIN(Q3-Q7)-2*(L7*MC+L8*MD+L8*ME+L
     &  8*MF+L8*MG)*SIN(Q3-Q6-Q7)-(L4*MB+2*L6*MC+2*L6*MD+2*L6*ME+2*L6*MF
     &  +2*L6*MG)*SIN(Q3-Q5-Q6-Q7))/(MA+MB+MC+MD+ME+MF+MG)
        VCMXF = U1 - 0.5*(2*MG*GS*SIN(Q3)*U3+2*(L10*ME+L10*MF+L10*MG+L9*
     &  MD)*SIN(Q3-Q7)*(U3-U7)+2*(L10*MF+ME*(L10-L9))*SIN(EA-Q3)*(EAp-U3
     &  -U8)+L3*MB*SIN(FOOTANG+Q3-Q5-Q6-Q7)*(U3-U5-U6-U7)+2*MF*(L7-L8)*S
     &  IN(EA-FA-Q3)*(EAp-FAp-U3-U8-U9)+2*(L7*MC+L8*MD+L8*ME+L8*MF+L8*MG
     &  )*SIN(Q3-Q6-Q7)*(U3-U6-U7)+(L4*MB+2*L6*MC+2*L6*MD+2*L6*ME+2*L6*M
     &  F+2*L6*MG)*SIN(Q3-Q5-Q6-Q7)*(U3-U5-U6-U7)-2*MG*GSp*COS(Q3)-2*(L1
     &  *MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF+L2*MG)*SIN(Q3-Q4-Q5-Q6-Q7)*(U3
     &  -U4-U5-U6-U7))/(MA+MB+MC+MD+ME+MF+MG)
        VCMYF =  U2 + 0.5*(2*MG*GSp*SIN(Q3)+2*MG*GS*COS(Q3)*U3+2*(L10*ME
     &  +L10*MF+L10*MG+L9*MD)*COS(Q3-Q7)*(U3-U7)+L3*MB*COS(FOOTANG+Q3-Q5
     &  -Q6-Q7)*(U3-U5-U6-U7)+2*(L7*MC+L8*MD+L8*ME+L8*MF+L8*MG)*COS(Q3-Q
     &  6-Q7)*(U3-U6-U7)+(L4*MB+2*L6*MC+2*L6*MD+2*L6*ME+2*L6*MF+2*L6*MG)
     &  *COS(Q3-Q5-Q6-Q7)*(U3-U5-U6-U7)-2*(L10*MF+ME*(L10-L9))*COS(EA-Q3
     &  )*(EAp-U3-U8)-2*MF*(L7-L8)*COS(EA-FA-Q3)*(EAp-FAp-U3-U8-U9)-2*(L
     &  1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF+L2*MG)*COS(Q3-Q4-Q5-Q6-Q7)*(U
     &  3-U4-U5-U6-U7))/(MA+MB+MC+MD+ME+MF+MG)
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
     &K7,K8,L1,L10,L11,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,MTPB
     &,MTPK
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
     &X,POP8Y,POP9X,POP9Y,VOCMX,VOCMY,VOP2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(430),COEF(7,7),RHS(7)
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

C** Calculate forces
      POP2X = Q1 - L2*COS(Q3-Q4-Q5-Q6-Q7)
      POP2Y = Q2 - L2*SIN(Q3-Q4-Q5-Q6-Q7)
      VOP2X = U1 + L2*SIN(Q3-Q4-Q5-Q6-Q7)*(U3-U4-U5-U6-U7)
      VOP2Y = U2 - L2*COS(Q3-Q4-Q5-Q6-Q7)*(U3-U4-U5-U6-U7)  

      IF (Q2 .LT. 0.0D0) THEN
        RY1 = -K3*Q2 - K4*ABS(Q2)*U2
        RX1 = (-K1*Q1 - K2*U1)*RY1
      ELSE
        RX1 = 0.0D0
        RY1 = 0.0D0
      ENDIF
      IF (POP2Y .LT. 0.0D0) THEN
        RY2 = -K7*POP2Y - K8*ABS(POP2Y)*VOP2Y
        RX2 = (-K5*POP2X - K6*VOP2X)*RY2
      ELSE
        RX2 = 0.0D0
        RY2 = 0.0D0
      ENDIF

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
      Z(19) = Z(3)*Z(5) - Z(4)*Z(6)
      Z(1) = COS(Q3)
      Z(7) = COS(Q6)
      Z(10) = SIN(Q7)
      Z(8) = SIN(Q6)
      Z(9) = COS(Q7)
      Z(23) = -Z(7)*Z(10) - Z(8)*Z(9)
      Z(2) = SIN(Q3)
      Z(22) = Z(7)*Z(9) - Z(8)*Z(10)
      Z(26) = Z(1)*Z(23) + Z(2)*Z(22)
      Z(20) = -Z(3)*Z(6) - Z(4)*Z(5)
      Z(24) = Z(7)*Z(10) + Z(8)*Z(9)
      Z(28) = Z(1)*Z(22) + Z(2)*Z(24)
      Z(30) = Z(19)*Z(26) + Z(20)*Z(28)
      Z(21) = Z(3)*Z(6) + Z(4)*Z(5)
      Z(32) = Z(19)*Z(28) + Z(21)*Z(26)
      Z(25) = Z(1)*Z(22) - Z(2)*Z(23)
      Z(27) = Z(1)*Z(24) - Z(2)*Z(22)
      Z(29) = Z(19)*Z(25) + Z(20)*Z(27)
      Z(31) = Z(19)*Z(27) + Z(21)*Z(25)
      Z(33) = Z(15)*Z(3) - Z(16)*Z(4)
      Z(34) = Z(15)*Z(4) + Z(16)*Z(3)
      Z(35) = -Z(15)*Z(4) - Z(16)*Z(3)
      Z(36) = Z(29)*Z(33) + Z(31)*Z(34)
      Z(37) = Z(30)*Z(33) + Z(32)*Z(34)
      Z(38) = Z(29)*Z(35) + Z(31)*Z(33)
      Z(39) = Z(30)*Z(35) + Z(32)*Z(33)
      Z(40) = Z(3)*Z(29) + Z(4)*Z(31)
      Z(41) = Z(3)*Z(30) + Z(4)*Z(32)
      Z(42) = Z(3)*Z(31) - Z(4)*Z(29)
      Z(43) = Z(3)*Z(32) - Z(4)*Z(30)
      Z(44) = Z(7)*Z(25) + Z(8)*Z(27)
      Z(45) = Z(7)*Z(26) + Z(8)*Z(28)
      Z(46) = Z(7)*Z(27) - Z(8)*Z(25)
      Z(47) = Z(7)*Z(28) - Z(8)*Z(26)
      Z(78) = Z(1)*Z(31) + Z(2)*Z(32)
      Z(79) = Z(1)*Z(30) - Z(2)*Z(29)
      Z(80) = Z(1)*Z(32) - Z(2)*Z(31)
      Z(83) = Z(1)*Z(42) + Z(2)*Z(43)
      Z(84) = Z(1)*Z(41) - Z(2)*Z(40)
      Z(85) = Z(1)*Z(43) - Z(2)*Z(42)
      Z(105) = L1*(U3-U4-U5-U6-U7)
      Z(106) = (U3-U4-U5-U6-U7)*Z(105)
      Z(107) = L2*(U3-U4-U5-U6-U7)
      Z(108) = (U3-U4-U5-U6-U7)*Z(107)
      Z(109) = L4*(U3-U5-U6-U7)
      Z(110) = L3*(U3-U5-U6-U7)
      Z(111) = (U3-U5-U6-U7)*Z(109)
      Z(112) = (U3-U5-U6-U7)*Z(110)
      Z(115) = L6*(U3-U5-U6-U7)
      Z(116) = (U3-U5-U6-U7)*Z(115)
      Z(117) = L7*(U3-U6-U7)
      Z(118) = (U3-U6-U7)*Z(117)
      Z(119) = L8*(U3-U6-U7)
      Z(120) = (U3-U6-U7)*Z(119)
      Z(121) = L9*(U3-U7)
      Z(122) = (U3-U7)*Z(121)
      Z(123) = L10*(U3-U7)
      Z(124) = (U3-U7)*Z(123)
      Z(154) = Z(7)*Z(19) + Z(8)*Z(20)
      Z(155) = Z(7)*Z(20) - Z(8)*Z(19)
      Z(156) = Z(7)*Z(21) + Z(8)*Z(19)
      Z(157) = Z(7)*Z(19) - Z(8)*Z(21)
      Z(159) = Z(3)*Z(155) + Z(4)*Z(157)
      Z(160) = Z(3)*Z(156) - Z(4)*Z(154)
      Z(161) = Z(3)*Z(157) - Z(4)*Z(155)
      Z(300) = RX1 + RX2
      Z(302) = Z(301) + RY1 + RY2
      Z(333) = Z(332)*Z(31)
      Z(334) = Z(270)*Z(31) + MC*(L2*Z(31)-L6*Z(42)) + MD*(L2*Z(31)-L6*Z
     &(42)) + ME*(L2*Z(31)-L6*Z(42)) + MF*(L2*Z(31)-L6*Z(42)) + MG*(L2*Z
     &(31)-L6*Z(42)) + 0.5D0*MB*(2*L2*Z(31)-L3*Z(38)-L4*Z(42))
      Z(335) = Z(270)*Z(31) + MC*(L2*Z(31)-L6*Z(42)-L7*Z(27)) + MD*(L2*Z
     &(31)-L6*Z(42)-L8*Z(27)) + ME*(L2*Z(31)-L6*Z(42)-L8*Z(27)) + MF*(L2
     &*Z(31)-L6*Z(42)-L8*Z(27)) + MG*(L2*Z(31)-L6*Z(42)-L8*Z(27)) + 0.5D
     &0*MB*(2*L2*Z(31)-L3*Z(38)-L4*Z(42))
      Z(336) = Z(270)*Z(31) + MC*(L2*Z(31)-L6*Z(42)-L7*Z(27)) + 0.5D0*MB
     &*(2*L2*Z(31)-L3*Z(38)-L4*Z(42)) + MD*(L2*Z(31)-L6*Z(42)-L8*Z(27)-L
     &9*Z(46)) + ME*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)) + MF*(L2*Z(31
     &)-L10*Z(46)-L6*Z(42)-L8*Z(27)) + MG*(L2*Z(31)-L10*Z(46)-L6*Z(42)-L
     &8*Z(27))
      Z(339) = Z(332)*Z(32)
      Z(340) = Z(270)*Z(32) + MC*(L2*Z(32)-L6*Z(43)) + MD*(L2*Z(32)-L6*Z
     &(43)) + ME*(L2*Z(32)-L6*Z(43)) + MF*(L2*Z(32)-L6*Z(43)) + MG*(L2*Z
     &(32)-L6*Z(43)) + 0.5D0*MB*(2*L2*Z(32)-L3*Z(39)-L4*Z(43))
      Z(341) = Z(270)*Z(32) + MC*(L2*Z(32)-L6*Z(43)-L7*Z(28)) + MD*(L2*Z
     &(32)-L6*Z(43)-L8*Z(28)) + ME*(L2*Z(32)-L6*Z(43)-L8*Z(28)) + MF*(L2
     &*Z(32)-L6*Z(43)-L8*Z(28)) + MG*(L2*Z(32)-L6*Z(43)-L8*Z(28)) + 0.5D
     &0*MB*(2*L2*Z(32)-L3*Z(39)-L4*Z(43))
      Z(342) = Z(270)*Z(32) + MC*(L2*Z(32)-L6*Z(43)-L7*Z(28)) + 0.5D0*MB
     &*(2*L2*Z(32)-L3*Z(39)-L4*Z(43)) + MD*(L2*Z(32)-L6*Z(43)-L8*Z(28)-L
     &9*Z(47)) + ME*(L2*Z(32)-L10*Z(47)-L6*Z(43)-L8*Z(28)) + MF*(L2*Z(32
     &)-L10*Z(47)-L6*Z(43)-L8*Z(28)) + MG*(L2*Z(32)-L10*Z(47)-L6*Z(43)-L
     &8*Z(28))
      Z(387) = Z(386) + Z(274)*(L2-L6*Z(3)) + Z(277)*(L2-L6*Z(3)) + Z(28
     &1)*(L2-L6*Z(3)) + Z(287)*(L2-L6*Z(3)) + Z(294)*(L2-L6*Z(3)) + 0.5D
     &0*Z(271)*(2*L2-L3*Z(33)-L4*Z(3))
      Z(388) = Z(386) + Z(274)*(L2-L6*Z(3)-L7*Z(19)) + Z(277)*(L2-L6*Z(3
     &)-L8*Z(19)) + Z(281)*(L2-L6*Z(3)-L8*Z(19)) + Z(287)*(L2-L6*Z(3)-L8
     &*Z(19)) + Z(294)*(L2-L6*Z(3)-L8*Z(19)) + 0.5D0*Z(271)*(2*L2-L3*Z(3
     &3)-L4*Z(3))
      Z(389) = Z(386) + Z(274)*(L2-L6*Z(3)-L7*Z(19)) + 0.5D0*Z(271)*(2*L
     &2-L3*Z(33)-L4*Z(3)) + Z(277)*(L2-L6*Z(3)-L8*Z(19)-L9*Z(157)) + Z(2
     &81)*(L2-L10*Z(157)-L6*Z(3)-L8*Z(19)) + Z(287)*(L2-L10*Z(157)-L6*Z(
     &3)-L8*Z(19)) + Z(294)*(L2-L10*Z(157)-L6*Z(3)-L8*Z(19))
      Z(393) = Z(391) + MC*(Z(392)-2*Z(348)*Z(3)) + MD*(Z(392)-2*Z(348)*
     &Z(3)) + ME*(Z(392)-2*Z(348)*Z(3)) + MF*(Z(392)-2*Z(348)*Z(3)) + MG
     &*(Z(392)-2*Z(348)*Z(3)) + 0.25D0*MB*(Z(345)-4*Z(346)*Z(33)-4*Z(347
     &)*Z(3))
      Z(394) = Z(391) + 0.25D0*MB*(Z(345)-4*Z(346)*Z(33)-4*Z(347)*Z(3)) 
     &- MC*(Z(349)*Z(19)+2*Z(348)*Z(3)-Z(350)-Z(351)-Z(353)*Z(5)) - MD*(
     &Z(354)*Z(19)+2*Z(348)*Z(3)-Z(350)-Z(351)-Z(358)*Z(5)) - ME*(Z(354)
     &*Z(19)+2*Z(348)*Z(3)-Z(350)-Z(351)-Z(358)*Z(5)) - MF*(Z(354)*Z(19)
     &+2*Z(348)*Z(3)-Z(350)-Z(351)-Z(358)*Z(5)) - MG*(Z(354)*Z(19)+2*Z(3
     &48)*Z(3)-Z(350)-Z(351)-Z(358)*Z(5))
      Z(395) = Z(391) + 0.25D0*MB*(Z(345)-4*Z(346)*Z(33)-4*Z(347)*Z(3)) 
     &- MC*(Z(349)*Z(19)+2*Z(348)*Z(3)-Z(350)-Z(351)-Z(353)*Z(5)) - MD*(
     &Z(354)*Z(19)+Z(355)*Z(157)+2*Z(348)*Z(3)-Z(350)-Z(351)-Z(358)*Z(5)
     &-Z(359)*Z(161)) - ME*(Z(354)*Z(19)+Z(361)*Z(157)+2*Z(348)*Z(3)-Z(3
     &50)-Z(351)-Z(358)*Z(5)-Z(365)*Z(161)) - MF*(Z(354)*Z(19)+Z(361)*Z(
     &157)+2*Z(348)*Z(3)-Z(350)-Z(351)-Z(358)*Z(5)-Z(365)*Z(161)) - MG*(
     &Z(354)*Z(19)+Z(361)*Z(157)+2*Z(348)*Z(3)-Z(350)-Z(351)-Z(358)*Z(5)
     &-Z(365)*Z(161))
      Z(398) = Z(397) + 0.25D0*MB*(Z(345)-4*Z(346)*Z(33)-4*Z(347)*Z(3)) 
     &- MC*(2*Z(348)*Z(3)+2*Z(349)*Z(19)-Z(350)-Z(351)-Z(352)-2*Z(353)*Z
     &(5)) - MD*(2*Z(348)*Z(3)+2*Z(354)*Z(19)-Z(350)-Z(351)-Z(356)-2*Z(3
     &58)*Z(5)) - ME*(2*Z(348)*Z(3)+2*Z(354)*Z(19)-Z(350)-Z(351)-Z(356)-
     &2*Z(358)*Z(5)) - MF*(2*Z(348)*Z(3)+2*Z(354)*Z(19)-Z(350)-Z(351)-Z(
     &356)-2*Z(358)*Z(5)) - MG*(2*Z(348)*Z(3)+2*Z(354)*Z(19)-Z(350)-Z(35
     &1)-Z(356)-2*Z(358)*Z(5))
      Z(399) = Z(397) + 0.25D0*MB*(Z(345)-4*Z(346)*Z(33)-4*Z(347)*Z(3)) 
     &- MC*(2*Z(348)*Z(3)+2*Z(349)*Z(19)-Z(350)-Z(351)-Z(352)-2*Z(353)*Z
     &(5)) - MD*(Z(355)*Z(157)+2*Z(348)*Z(3)+2*Z(354)*Z(19)-Z(350)-Z(351
     &)-Z(356)-2*Z(358)*Z(5)-Z(359)*Z(161)-Z(360)*Z(7)) - ME*(Z(361)*Z(1
     &57)+2*Z(348)*Z(3)+2*Z(354)*Z(19)-Z(350)-Z(351)-Z(356)-2*Z(358)*Z(5
     &)-Z(365)*Z(161)-Z(366)*Z(7)) - MF*(Z(361)*Z(157)+2*Z(348)*Z(3)+2*Z
     &(354)*Z(19)-Z(350)-Z(351)-Z(356)-2*Z(358)*Z(5)-Z(365)*Z(161)-Z(366
     &)*Z(7)) - MG*(Z(361)*Z(157)+2*Z(348)*Z(3)+2*Z(354)*Z(19)-Z(350)-Z(
     &351)-Z(356)-2*Z(358)*Z(5)-Z(365)*Z(161)-Z(366)*Z(7))
      Z(402) = Z(401) + 0.25D0*MB*(Z(345)-4*Z(346)*Z(33)-4*Z(347)*Z(3)) 
     &- MC*(2*Z(348)*Z(3)+2*Z(349)*Z(19)-Z(350)-Z(351)-Z(352)-2*Z(353)*Z
     &(5)) - MD*(2*Z(348)*Z(3)+2*Z(354)*Z(19)+2*Z(355)*Z(157)-Z(350)-Z(3
     &51)-Z(356)-Z(357)-2*Z(358)*Z(5)-2*Z(359)*Z(161)-2*Z(360)*Z(7)) - M
     &E*(2*Z(348)*Z(3)+2*Z(354)*Z(19)+2*Z(361)*Z(157)-Z(350)-Z(351)-Z(35
     &6)-Z(363)-2*Z(358)*Z(5)-2*Z(365)*Z(161)-2*Z(366)*Z(7)) - MF*(2*Z(3
     &48)*Z(3)+2*Z(354)*Z(19)+2*Z(361)*Z(157)-Z(350)-Z(351)-Z(356)-Z(363
     &)-2*Z(358)*Z(5)-2*Z(365)*Z(161)-2*Z(366)*Z(7)) - MG*(2*Z(348)*Z(3)
     &+2*Z(354)*Z(19)+2*Z(361)*Z(157)-Z(350)-Z(351)-Z(356)-Z(363)-2*Z(35
     &8)*Z(5)-2*Z(365)*Z(161)-2*Z(366)*Z(7))
      Z(11) = COS(EA)
      Z(12) = SIN(EA)
      Z(13) = COS(FA)
      Z(14) = SIN(FA)
      Z(48) = Z(11)*Z(1) + Z(12)*Z(2)
      Z(49) = Z(11)*Z(2) - Z(12)*Z(1)
      Z(50) = Z(12)*Z(1) - Z(11)*Z(2)
      Z(51) = -Z(13)*Z(48) - Z(14)*Z(50)
      Z(52) = -Z(13)*Z(49) - Z(14)*Z(48)
      Z(53) = Z(14)*Z(48) - Z(13)*Z(50)
      Z(54) = Z(14)*Z(49) - Z(13)*Z(48)
      Z(87) = U8 - EAp
      Z(89) = FApp - EApp
      Z(98) = Z(17)*EAp
      Z(99) = L10*EAp
      Z(100) = Z(18)*(EAp-FAp)
      Z(125) = Z(17)*U3 + Z(17)*U8 - Z(98)
      Z(126) = Z(17)*EApp
      Z(127) = Z(125)*(U3+Z(87))
      Z(128) = L10*U3 + L10*U8 - Z(99)
      Z(129) = L10*EApp
      Z(130) = Z(128)*(U3+Z(87))
      Z(131) = Z(18)*U3 + Z(18)*U8 + Z(18)*U9 - Z(100)
      Z(132) = Z(18)*(EApp-FApp)
      Z(133) = FAp + U9
      Z(134) = Z(131)*(U3+Z(87)+Z(133))
      Z(138) = GS*U3
      Z(139) = GSp*U3
      Z(140) = GSpp - U3*Z(138)
      Z(141) = GSp*U3 + Z(139)
      Z(151) = MTOR - ATOR
      Z(152) = ATOR + KTOR
      Z(153) = -HTOR - KTOR
      Z(162) = Z(29)*Z(48) + Z(30)*Z(49)
      Z(163) = Z(29)*Z(50) + Z(30)*Z(48)
      Z(164) = Z(31)*Z(48) + Z(32)*Z(49)
      Z(165) = Z(31)*Z(50) + Z(32)*Z(48)
      Z(166) = Z(3)*Z(162) + Z(4)*Z(164)
      Z(167) = Z(3)*Z(163) + Z(4)*Z(165)
      Z(168) = Z(3)*Z(164) - Z(4)*Z(162)
      Z(169) = Z(3)*Z(165) - Z(4)*Z(163)
      Z(170) = Z(5)*Z(166) + Z(6)*Z(168)
      Z(171) = Z(5)*Z(167) + Z(6)*Z(169)
      Z(172) = Z(5)*Z(168) - Z(6)*Z(166)
      Z(173) = Z(5)*Z(169) - Z(6)*Z(167)
      Z(175) = Z(7)*Z(171) + Z(8)*Z(173)
      Z(176) = Z(7)*Z(172) - Z(8)*Z(170)
      Z(177) = Z(7)*Z(173) - Z(8)*Z(171)
      Z(178) = Z(29)*Z(51) + Z(30)*Z(52)
      Z(179) = Z(29)*Z(53) + Z(30)*Z(54)
      Z(180) = Z(31)*Z(51) + Z(32)*Z(52)
      Z(181) = Z(31)*Z(53) + Z(32)*Z(54)
      Z(182) = Z(3)*Z(178) + Z(4)*Z(180)
      Z(183) = Z(3)*Z(179) + Z(4)*Z(181)
      Z(184) = Z(3)*Z(180) - Z(4)*Z(178)
      Z(185) = Z(3)*Z(181) - Z(4)*Z(179)
      Z(186) = Z(5)*Z(182) + Z(6)*Z(184)
      Z(187) = Z(5)*Z(183) + Z(6)*Z(185)
      Z(188) = Z(5)*Z(184) - Z(6)*Z(182)
      Z(189) = Z(5)*Z(185) - Z(6)*Z(183)
      Z(191) = Z(7)*Z(187) + Z(8)*Z(189)
      Z(192) = Z(7)*Z(188) - Z(8)*Z(186)
      Z(193) = Z(7)*Z(189) - Z(8)*Z(187)
      Z(304) = HTOR + Z(151) + Z(152) + Z(153) - MTOR - Z(303)*Z(32) - L
     &2*(RX2*Z(31)+RY2*Z(32)) - Z(146)*(L2*Z(32)-L6*Z(43)-L7*Z(28)) - 0.
     &5D0*Z(145)*(2*L2*Z(32)-L3*Z(39)-L4*Z(43)) - Z(147)*(L2*Z(32)-L6*Z(
     &43)-L8*Z(28)-L9*Z(47)) - Z(148)*(L2*Z(32)-L10*Z(47)-L6*Z(43)-L8*Z(
     &28)-Z(17)*Z(48)) - Z(150)*(L2*Z(32)-L10*Z(47)-L6*Z(43)-L8*Z(28)-GS
     &*Z(1)) - Z(149)*(L2*Z(32)-L10*Z(47)-L10*Z(48)-L6*Z(43)-L8*Z(28)-Z(
     &18)*Z(54))
      Z(311) = MTOR + Z(303)*Z(32) + Z(305)*Z(32) + Z(306)*Z(32) + Z(307
     &)*Z(32) + Z(308)*Z(32) + Z(309)*Z(32) + Z(310)*Z(32) + L2*(RX2*Z(3
     &1)+RY2*Z(32))
      Z(312) = MTOR + Z(303)*Z(32) + L2*(RX2*Z(31)+RY2*Z(32)) + Z(146)*(
     &L2*Z(32)-L6*Z(43)) + Z(147)*(L2*Z(32)-L6*Z(43)) + Z(148)*(L2*Z(32)
     &-L6*Z(43)) + Z(149)*(L2*Z(32)-L6*Z(43)) + Z(150)*(L2*Z(32)-L6*Z(43
     &)) + 0.5D0*Z(145)*(2*L2*Z(32)-L3*Z(39)-L4*Z(43)) - Z(151)
      Z(313) = MTOR + Z(303)*Z(32) + L2*(RX2*Z(31)+RY2*Z(32)) + Z(146)*(
     &L2*Z(32)-L6*Z(43)-L7*Z(28)) + Z(147)*(L2*Z(32)-L6*Z(43)-L8*Z(28)) 
     &+ Z(148)*(L2*Z(32)-L6*Z(43)-L8*Z(28)) + Z(149)*(L2*Z(32)-L6*Z(43)-
     &L8*Z(28)) + Z(150)*(L2*Z(32)-L6*Z(43)-L8*Z(28)) + 0.5D0*Z(145)*(2*
     &L2*Z(32)-L3*Z(39)-L4*Z(43)) - Z(151) - Z(152)
      Z(314) = MTOR + Z(303)*Z(32) + L2*(RX2*Z(31)+RY2*Z(32)) + Z(146)*(
     &L2*Z(32)-L6*Z(43)-L7*Z(28)) + 0.5D0*Z(145)*(2*L2*Z(32)-L3*Z(39)-L4
     &*Z(43)) + Z(147)*(L2*Z(32)-L6*Z(43)-L8*Z(28)-L9*Z(47)) + Z(148)*(L
     &2*Z(32)-L10*Z(47)-L6*Z(43)-L8*Z(28)) + Z(149)*(L2*Z(32)-L10*Z(47)-
     &L6*Z(43)-L8*Z(28)) + Z(150)*(L2*Z(32)-L10*Z(47)-L6*Z(43)-L8*Z(28))
     & - Z(151) - Z(152) - Z(153)
      Z(328) = IE*EApp
      Z(330) = IF*Z(89)
      Z(331) = MG*(L10*Z(46)+L6*Z(42)+L8*Z(27)-L2*Z(31)-GS*Z(2)) - Z(270
     &)*Z(31) - MC*(L2*Z(31)-L6*Z(42)-L7*Z(27)) - 0.5D0*MB*(2*L2*Z(31)-L
     &3*Z(38)-L4*Z(42)) - MD*(L2*Z(31)-L6*Z(42)-L8*Z(27)-L9*Z(46)) - ME*
     &(L2*Z(31)-L10*Z(46)-L6*Z(42)-L8*Z(27)-Z(17)*Z(50)) - MF*(L2*Z(31)-
     &L10*Z(46)-L10*Z(50)-L6*Z(42)-L8*Z(27)-Z(18)*Z(53))
      Z(337) = MA*Z(29)*Z(106) + MC*(Z(29)*Z(108)-Z(25)*Z(118)-Z(40)*Z(1
     &16)) + 0.5D0*MB*(2*Z(29)*Z(108)-Z(36)*Z(112)-Z(40)*Z(111)) + MD*(Z
     &(29)*Z(108)-Z(25)*Z(120)-Z(40)*Z(116)-Z(44)*Z(122)) + MG*(Z(1)*Z(1
     &40)+Z(29)*Z(108)-Z(2)*Z(141)-Z(25)*Z(120)-Z(40)*Z(116)-Z(44)*Z(124
     &)) + ME*(Z(29)*Z(108)-Z(126)*Z(50)-Z(25)*Z(120)-Z(40)*Z(116)-Z(44)
     &*Z(124)-Z(48)*Z(127)) + MF*(Z(29)*Z(108)-Z(129)*Z(50)-Z(132)*Z(53)
     &-Z(25)*Z(120)-Z(40)*Z(116)-Z(44)*Z(124)-Z(48)*Z(130)-Z(51)*Z(134))
      Z(338) = -Z(270)*Z(32) - MC*(L2*Z(32)-L6*Z(43)-L7*Z(28)) - 0.5D0*M
     &B*(2*L2*Z(32)-L3*Z(39)-L4*Z(43)) - MD*(L2*Z(32)-L6*Z(43)-L8*Z(28)-
     &L9*Z(47)) - ME*(L2*Z(32)-L10*Z(47)-L6*Z(43)-L8*Z(28)-Z(17)*Z(48)) 
     &- MG*(L2*Z(32)-L10*Z(47)-L6*Z(43)-L8*Z(28)-GS*Z(1)) - MF*(L2*Z(32)
     &-L10*Z(47)-L10*Z(48)-L6*Z(43)-L8*Z(28)-Z(18)*Z(54))
      Z(343) = MA*Z(30)*Z(106) + MC*(Z(30)*Z(108)-Z(26)*Z(118)-Z(41)*Z(1
     &16)) + 0.5D0*MB*(2*Z(30)*Z(108)-Z(37)*Z(112)-Z(41)*Z(111)) + MD*(Z
     &(30)*Z(108)-Z(26)*Z(120)-Z(41)*Z(116)-Z(45)*Z(122)) + MG*(Z(1)*Z(1
     &41)+Z(2)*Z(140)+Z(30)*Z(108)-Z(26)*Z(120)-Z(41)*Z(116)-Z(45)*Z(124
     &)) + ME*(Z(30)*Z(108)-Z(126)*Z(48)-Z(26)*Z(120)-Z(41)*Z(116)-Z(45)
     &*Z(124)-Z(49)*Z(127)) + MF*(Z(30)*Z(108)-Z(129)*Z(48)-Z(132)*Z(54)
     &-Z(26)*Z(120)-Z(41)*Z(116)-Z(45)*Z(124)-Z(49)*Z(130)-Z(52)*Z(134))
      Z(375) = Z(344) + 0.25D0*MB*(Z(345)-4*Z(346)*Z(33)-4*Z(347)*Z(3)) 
     &- MC*(2*Z(348)*Z(3)+2*Z(349)*Z(19)-Z(350)-Z(351)-Z(352)-2*Z(353)*Z
     &(5)) - MD*(2*Z(348)*Z(3)+2*Z(354)*Z(19)+2*Z(355)*Z(157)-Z(350)-Z(3
     &51)-Z(356)-Z(357)-2*Z(358)*Z(5)-2*Z(359)*Z(161)-2*Z(360)*Z(7)) - M
     &E*(2*Z(348)*Z(3)+2*Z(354)*Z(19)+2*Z(361)*Z(157)+2*Z(362)*Z(165)-Z(
     &350)-Z(351)-Z(356)-Z(363)-Z(364)-2*Z(358)*Z(5)-2*Z(365)*Z(161)-2*Z
     &(366)*Z(7)-2*Z(367)*Z(177)-2*Z(368)*Z(169)-2*Z(369)*Z(173)) - MG*(
     &2*Z(348)*Z(3)+2*Z(354)*Z(19)+2*Z(361)*Z(157)+2*L2*GS*Z(80)-Z(350)-
     &Z(351)-Z(356)-Z(363)-GS**2-2*Z(358)*Z(5)-2*Z(365)*Z(161)-2*Z(366)*
     &Z(7)-2*L10*GS*Z(9)-2*L6*GS*Z(85)-2*L8*GS*Z(22)) - MF*(2*Z(370)*Z(1
     &3)+2*Z(348)*Z(3)+2*Z(354)*Z(19)+2*Z(361)*Z(157)+2*Z(361)*Z(165)+2*
     &Z(371)*Z(181)-2*Z(363)-Z(350)-Z(351)-Z(356)-Z(372)-2*Z(358)*Z(5)-2
     &*Z(363)*Z(177)-2*Z(365)*Z(161)-2*Z(365)*Z(169)-2*Z(366)*Z(7)-2*Z(3
     &66)*Z(173)-2*Z(370)*Z(193)-2*Z(373)*Z(185)-2*Z(374)*Z(189))
      Z(377) = Z(376) - Z(274)*(L2-L6*Z(3)-L7*Z(19)) - 0.5D0*Z(271)*(2*L
     &2-L3*Z(33)-L4*Z(3)) - Z(277)*(L2-L6*Z(3)-L8*Z(19)-L9*Z(157)) - Z(2
     &81)*(L2-L10*Z(157)-L6*Z(3)-L8*Z(19)-Z(17)*Z(165)) - Z(294)*(L2-L10
     &*Z(157)-L6*Z(3)-L8*Z(19)-GS*Z(80)) - Z(287)*(L2-L10*Z(157)-L10*Z(1
     &65)-L6*Z(3)-L8*Z(19)-Z(18)*Z(181))
      Z(379) = MC*(Z(349)*Z(19)+2*Z(348)*Z(3)-Z(350)-Z(351)-Z(353)*Z(5))
     & + MD*(Z(354)*Z(19)+Z(355)*Z(157)+2*Z(348)*Z(3)-Z(350)-Z(351)-Z(35
     &8)*Z(5)-Z(359)*Z(161)) + ME*(Z(354)*Z(19)+Z(361)*Z(157)+Z(362)*Z(1
     &65)+2*Z(348)*Z(3)-Z(350)-Z(351)-Z(358)*Z(5)-Z(365)*Z(161)-Z(368)*Z
     &(169)) + MG*(Z(354)*Z(19)+Z(361)*Z(157)+2*Z(348)*Z(3)+L2*GS*Z(80)-
     &Z(350)-Z(351)-Z(358)*Z(5)-Z(365)*Z(161)-L6*GS*Z(85)) + MF*(Z(354)*
     &Z(19)+Z(361)*Z(157)+Z(361)*Z(165)+Z(371)*Z(181)+2*Z(348)*Z(3)-Z(35
     &0)-Z(351)-Z(358)*Z(5)-Z(365)*Z(161)-Z(365)*Z(169)-Z(373)*Z(185)) -
     & IA - IB - Z(378) - 0.25D0*MB*(Z(345)-4*Z(346)*Z(33)-4*Z(347)*Z(3)
     &)
      Z(380) = MC*(2*Z(348)*Z(3)+2*Z(349)*Z(19)-Z(350)-Z(351)-Z(352)-2*Z
     &(353)*Z(5)) + MD*(Z(355)*Z(157)+2*Z(348)*Z(3)+2*Z(354)*Z(19)-Z(350
     &)-Z(351)-Z(356)-2*Z(358)*Z(5)-Z(359)*Z(161)-Z(360)*Z(7)) + ME*(Z(3
     &61)*Z(157)+Z(362)*Z(165)+2*Z(348)*Z(3)+2*Z(354)*Z(19)-Z(350)-Z(351
     &)-Z(356)-2*Z(358)*Z(5)-Z(365)*Z(161)-Z(366)*Z(7)-Z(368)*Z(169)-Z(3
     &69)*Z(173)) + MG*(Z(361)*Z(157)+2*Z(348)*Z(3)+2*Z(354)*Z(19)+L2*GS
     &*Z(80)-Z(350)-Z(351)-Z(356)-2*Z(358)*Z(5)-Z(365)*Z(161)-Z(366)*Z(7
     &)-L6*GS*Z(85)-L8*GS*Z(22)) + MF*(Z(361)*Z(157)+Z(361)*Z(165)+Z(371
     &)*Z(181)+2*Z(348)*Z(3)+2*Z(354)*Z(19)-Z(350)-Z(351)-Z(356)-2*Z(358
     &)*Z(5)-Z(365)*Z(161)-Z(365)*Z(169)-Z(366)*Z(7)-Z(366)*Z(173)-Z(373
     &)*Z(185)-Z(374)*Z(189)) - IA - IB - IC - Z(378) - 0.25D0*MB*(Z(345
     &)-4*Z(346)*Z(33)-4*Z(347)*Z(3))
      Z(381) = MC*(2*Z(348)*Z(3)+2*Z(349)*Z(19)-Z(350)-Z(351)-Z(352)-2*Z
     &(353)*Z(5)) + MD*(2*Z(348)*Z(3)+2*Z(354)*Z(19)+2*Z(355)*Z(157)-Z(3
     &50)-Z(351)-Z(356)-Z(357)-2*Z(358)*Z(5)-2*Z(359)*Z(161)-2*Z(360)*Z(
     &7)) + ME*(Z(362)*Z(165)+2*Z(348)*Z(3)+2*Z(354)*Z(19)+2*Z(361)*Z(15
     &7)-Z(350)-Z(351)-Z(356)-Z(363)-2*Z(358)*Z(5)-2*Z(365)*Z(161)-2*Z(3
     &66)*Z(7)-Z(367)*Z(177)-Z(368)*Z(169)-Z(369)*Z(173)) + MG*(2*Z(348)
     &*Z(3)+2*Z(354)*Z(19)+2*Z(361)*Z(157)+L2*GS*Z(80)-Z(350)-Z(351)-Z(3
     &56)-Z(363)-2*Z(358)*Z(5)-2*Z(365)*Z(161)-2*Z(366)*Z(7)-L10*GS*Z(9)
     &-L6*GS*Z(85)-L8*GS*Z(22)) + MF*(Z(361)*Z(165)+Z(371)*Z(181)+2*Z(34
     &8)*Z(3)+2*Z(354)*Z(19)+2*Z(361)*Z(157)-Z(350)-Z(351)-Z(356)-Z(363)
     &-2*Z(358)*Z(5)-2*Z(365)*Z(161)-2*Z(366)*Z(7)-Z(363)*Z(177)-Z(365)*
     &Z(169)-Z(366)*Z(173)-Z(370)*Z(193)-Z(373)*Z(185)-Z(374)*Z(189)) - 
     &IA - IB - IC - ID - Z(378) - 0.25D0*MB*(Z(345)-4*Z(346)*Z(33)-4*Z(
     &347)*Z(3))
      Z(384) = Z(330) + 0.25D0*MB*(Z(382)*Z(111)+2*L2*Z(4)*Z(111)+2*L2*Z
     &(34)*Z(112)+2*L3*Z(35)*Z(108)-Z(383)*Z(112)-2*L4*Z(4)*Z(108)) + MD
     &*(L2*Z(4)*Z(116)+L2*Z(21)*Z(120)+L2*Z(156)*Z(122)+L8*Z(6)*Z(116)+L
     &8*Z(20)*Z(108)+L9*Z(8)*Z(120)+L9*Z(155)*Z(108)-L6*Z(4)*Z(108)-L6*Z
     &(6)*Z(120)-L6*Z(160)*Z(122)-L8*Z(8)*Z(122)-L9*Z(159)*Z(116)) + MG*
     &(GS*Z(141)+L10*Z(8)*Z(120)+L10*Z(9)*Z(141)+L10*Z(10)*Z(140)+L10*Z(
     &155)*Z(108)+L2*Z(4)*Z(116)+L2*Z(21)*Z(120)+L2*Z(156)*Z(124)+L6*Z(8
     &3)*Z(140)+L6*Z(85)*Z(141)+L8*Z(6)*Z(116)+L8*Z(20)*Z(108)+L8*Z(22)*
     &Z(141)+L8*Z(24)*Z(140)+GS*Z(10)*Z(124)+GS*Z(79)*Z(108)-L10*Z(159)*
     &Z(116)-L2*Z(78)*Z(140)-L2*Z(80)*Z(141)-L6*Z(4)*Z(108)-L6*Z(6)*Z(12
     &0)-L6*Z(160)*Z(124)-L8*Z(8)*Z(124)-GS*Z(23)*Z(120)-GS*Z(84)*Z(116)
     &) + ME*(L2*Z(126)*Z(165)+L10*Z(8)*Z(120)+L10*Z(155)*Z(108)+L2*Z(4)
     &*Z(116)+L2*Z(21)*Z(120)+L2*Z(156)*Z(124)+L2*Z(164)*Z(127)+L8*Z(6)*
     &Z(116)+L8*Z(20)*Z(108)+Z(17)*Z(163)*Z(108)-Z(17)*Z(126)-L10*Z(126)
     &*Z(177)-L6*Z(126)*Z(169)-L8*Z(126)*Z(173)-L10*Z(159)*Z(116)-L10*Z(
     &176)*Z(127)-L6*Z(4)*Z(108)-L6*Z(6)*Z(120)-L6*Z(160)*Z(124)-L6*Z(16
     &8)*Z(127)-L8*Z(8)*Z(124)-L8*Z(172)*Z(127)-Z(17)*Z(167)*Z(116)-Z(17
     &)*Z(171)*Z(120)-Z(17)*Z(175)*Z(124)) + MF*(L10*Z(13)*Z(132)+Z(18)*
     &Z(13)*Z(129)+L2*Z(129)*Z(165)+L2*Z(132)*Z(181)+L10*Z(14)*Z(134)+L1
     &0*Z(8)*Z(120)+L10*Z(155)*Z(108)+L10*Z(163)*Z(108)+L2*Z(4)*Z(116)+L
     &2*Z(21)*Z(120)+L2*Z(156)*Z(124)+L2*Z(164)*Z(130)+L2*Z(180)*Z(134)+
     &L8*Z(6)*Z(116)+L8*Z(20)*Z(108)+Z(18)*Z(179)*Z(108)-L10*Z(129)-Z(18
     &)*Z(132)-L10*Z(129)*Z(177)-L10*Z(132)*Z(193)-L6*Z(129)*Z(169)-L6*Z
     &(132)*Z(185)-L8*Z(129)*Z(173)-L8*Z(132)*Z(189)-L10*Z(159)*Z(116)-L
     &10*Z(167)*Z(116)-L10*Z(171)*Z(120)-L10*Z(175)*Z(124)-L10*Z(176)*Z(
     &130)-L10*Z(192)*Z(134)-L6*Z(4)*Z(108)-L6*Z(6)*Z(120)-L6*Z(160)*Z(1
     &24)-L6*Z(168)*Z(130)-L6*Z(184)*Z(134)-L8*Z(8)*Z(124)-L8*Z(172)*Z(1
     &30)-L8*Z(188)*Z(134)-Z(18)*Z(14)*Z(130)-Z(18)*Z(183)*Z(116)-Z(18)*
     &Z(187)*Z(120)-Z(18)*Z(191)*Z(124)) - Z(328) - MC*(L6*Z(4)*Z(108)+L
     &6*Z(6)*Z(118)-L2*Z(4)*Z(116)-L2*Z(21)*Z(118)-L7*Z(6)*Z(116)-L7*Z(2
     &0)*Z(108))
      Z(390) = L2*(2*MG*(Z(78)*Z(140)+Z(80)*Z(141)-Z(4)*Z(116)-Z(21)*Z(1
     &20)-Z(156)*Z(124))-2*MC*(Z(4)*Z(116)+Z(21)*Z(118))-MB*(Z(4)*Z(111)
     &+Z(34)*Z(112))-2*MD*(Z(4)*Z(116)+Z(21)*Z(120)+Z(156)*Z(122))-2*ME*
     &(Z(126)*Z(165)+Z(4)*Z(116)+Z(21)*Z(120)+Z(156)*Z(124)+Z(164)*Z(127
     &))-2*MF*(Z(129)*Z(165)+Z(132)*Z(181)+Z(4)*Z(116)+Z(21)*Z(120)+Z(15
     &6)*Z(124)+Z(164)*Z(130)+Z(180)*Z(134)))
      Z(396) = -MC*(L2*Z(4)*Z(116)+L2*Z(21)*Z(118)-L6*Z(4)*Z(108)-L6*Z(6
     &)*Z(118)) - MD*(L2*Z(4)*Z(116)+L2*Z(21)*Z(120)+L2*Z(156)*Z(122)-L6
     &*Z(4)*Z(108)-L6*Z(6)*Z(120)-L6*Z(160)*Z(122)) - 0.25D0*MB*(Z(382)*
     &Z(111)+2*L2*Z(4)*Z(111)+2*L2*Z(34)*Z(112)+2*L3*Z(35)*Z(108)-Z(383)
     &*Z(112)-2*L4*Z(4)*Z(108)) - ME*(L2*Z(126)*Z(165)+L2*Z(4)*Z(116)+L2
     &*Z(21)*Z(120)+L2*Z(156)*Z(124)+L2*Z(164)*Z(127)-L6*Z(126)*Z(169)-L
     &6*Z(4)*Z(108)-L6*Z(6)*Z(120)-L6*Z(160)*Z(124)-L6*Z(168)*Z(127)) - 
     &MG*(L2*Z(4)*Z(116)+L2*Z(21)*Z(120)+L2*Z(156)*Z(124)+L6*Z(83)*Z(140
     &)+L6*Z(85)*Z(141)-L2*Z(78)*Z(140)-L2*Z(80)*Z(141)-L6*Z(4)*Z(108)-L
     &6*Z(6)*Z(120)-L6*Z(160)*Z(124)) - MF*(L2*Z(129)*Z(165)+L2*Z(132)*Z
     &(181)+L2*Z(4)*Z(116)+L2*Z(21)*Z(120)+L2*Z(156)*Z(124)+L2*Z(164)*Z(
     &130)+L2*Z(180)*Z(134)-L6*Z(129)*Z(169)-L6*Z(132)*Z(185)-L6*Z(4)*Z(
     &108)-L6*Z(6)*Z(120)-L6*Z(160)*Z(124)-L6*Z(168)*Z(130)-L6*Z(184)*Z(
     &134))
      Z(400) = MC*(L6*Z(4)*Z(108)+L6*Z(6)*Z(118)-L2*Z(4)*Z(116)-L2*Z(21)
     &*Z(118)-L7*Z(6)*Z(116)-L7*Z(20)*Z(108)) + MD*(L6*Z(4)*Z(108)+L6*Z(
     &6)*Z(120)+L6*Z(160)*Z(122)+L8*Z(8)*Z(122)-L2*Z(4)*Z(116)-L2*Z(21)*
     &Z(120)-L2*Z(156)*Z(122)-L8*Z(6)*Z(116)-L8*Z(20)*Z(108)) + MG*(L2*Z
     &(78)*Z(140)+L2*Z(80)*Z(141)+L6*Z(4)*Z(108)+L6*Z(6)*Z(120)+L6*Z(160
     &)*Z(124)+L8*Z(8)*Z(124)-L2*Z(4)*Z(116)-L2*Z(21)*Z(120)-L2*Z(156)*Z
     &(124)-L6*Z(83)*Z(140)-L6*Z(85)*Z(141)-L8*Z(6)*Z(116)-L8*Z(20)*Z(10
     &8)-L8*Z(22)*Z(141)-L8*Z(24)*Z(140)) - 0.25D0*MB*(Z(382)*Z(111)+2*L
     &2*Z(4)*Z(111)+2*L2*Z(34)*Z(112)+2*L3*Z(35)*Z(108)-Z(383)*Z(112)-2*
     &L4*Z(4)*Z(108)) - ME*(L2*Z(126)*Z(165)+L2*Z(4)*Z(116)+L2*Z(21)*Z(1
     &20)+L2*Z(156)*Z(124)+L2*Z(164)*Z(127)+L8*Z(6)*Z(116)+L8*Z(20)*Z(10
     &8)-L6*Z(126)*Z(169)-L8*Z(126)*Z(173)-L6*Z(4)*Z(108)-L6*Z(6)*Z(120)
     &-L6*Z(160)*Z(124)-L6*Z(168)*Z(127)-L8*Z(8)*Z(124)-L8*Z(172)*Z(127)
     &) - MF*(L2*Z(129)*Z(165)+L2*Z(132)*Z(181)+L2*Z(4)*Z(116)+L2*Z(21)*
     &Z(120)+L2*Z(156)*Z(124)+L2*Z(164)*Z(130)+L2*Z(180)*Z(134)+L8*Z(6)*
     &Z(116)+L8*Z(20)*Z(108)-L6*Z(129)*Z(169)-L6*Z(132)*Z(185)-L8*Z(129)
     &*Z(173)-L8*Z(132)*Z(189)-L6*Z(4)*Z(108)-L6*Z(6)*Z(120)-L6*Z(160)*Z
     &(124)-L6*Z(168)*Z(130)-L6*Z(184)*Z(134)-L8*Z(8)*Z(124)-L8*Z(172)*Z
     &(130)-L8*Z(188)*Z(134))
      Z(403) = MC*(L6*Z(4)*Z(108)+L6*Z(6)*Z(118)-L2*Z(4)*Z(116)-L2*Z(21)
     &*Z(118)-L7*Z(6)*Z(116)-L7*Z(20)*Z(108)) + MG*(L10*Z(159)*Z(116)+L2
     &*Z(78)*Z(140)+L2*Z(80)*Z(141)+L6*Z(4)*Z(108)+L6*Z(6)*Z(120)+L6*Z(1
     &60)*Z(124)+L8*Z(8)*Z(124)-L10*Z(8)*Z(120)-L10*Z(9)*Z(141)-L10*Z(10
     &)*Z(140)-L10*Z(155)*Z(108)-L2*Z(4)*Z(116)-L2*Z(21)*Z(120)-L2*Z(156
     &)*Z(124)-L6*Z(83)*Z(140)-L6*Z(85)*Z(141)-L8*Z(6)*Z(116)-L8*Z(20)*Z
     &(108)-L8*Z(22)*Z(141)-L8*Z(24)*Z(140)) - 0.25D0*MB*(Z(382)*Z(111)+
     &2*L2*Z(4)*Z(111)+2*L2*Z(34)*Z(112)+2*L3*Z(35)*Z(108)-Z(383)*Z(112)
     &-2*L4*Z(4)*Z(108)) - MD*(L2*Z(4)*Z(116)+L2*Z(21)*Z(120)+L2*Z(156)*
     &Z(122)+L8*Z(6)*Z(116)+L8*Z(20)*Z(108)+L9*Z(8)*Z(120)+L9*Z(155)*Z(1
     &08)-L6*Z(4)*Z(108)-L6*Z(6)*Z(120)-L6*Z(160)*Z(122)-L8*Z(8)*Z(122)-
     &L9*Z(159)*Z(116)) - ME*(L2*Z(126)*Z(165)+L10*Z(8)*Z(120)+L10*Z(155
     &)*Z(108)+L2*Z(4)*Z(116)+L2*Z(21)*Z(120)+L2*Z(156)*Z(124)+L2*Z(164)
     &*Z(127)+L8*Z(6)*Z(116)+L8*Z(20)*Z(108)-L10*Z(126)*Z(177)-L6*Z(126)
     &*Z(169)-L8*Z(126)*Z(173)-L10*Z(159)*Z(116)-L10*Z(176)*Z(127)-L6*Z(
     &4)*Z(108)-L6*Z(6)*Z(120)-L6*Z(160)*Z(124)-L6*Z(168)*Z(127)-L8*Z(8)
     &*Z(124)-L8*Z(172)*Z(127)) - MF*(L2*Z(129)*Z(165)+L2*Z(132)*Z(181)+
     &L10*Z(8)*Z(120)+L10*Z(155)*Z(108)+L2*Z(4)*Z(116)+L2*Z(21)*Z(120)+L
     &2*Z(156)*Z(124)+L2*Z(164)*Z(130)+L2*Z(180)*Z(134)+L8*Z(6)*Z(116)+L
     &8*Z(20)*Z(108)-L10*Z(129)*Z(177)-L10*Z(132)*Z(193)-L6*Z(129)*Z(169
     &)-L6*Z(132)*Z(185)-L8*Z(129)*Z(173)-L8*Z(132)*Z(189)-L10*Z(159)*Z(
     &116)-L10*Z(176)*Z(130)-L10*Z(192)*Z(134)-L6*Z(4)*Z(108)-L6*Z(6)*Z(
     &120)-L6*Z(160)*Z(124)-L6*Z(168)*Z(130)-L6*Z(184)*Z(134)-L8*Z(8)*Z(
     &124)-L8*Z(172)*Z(130)-L8*Z(188)*Z(134))
      Z(422) = Z(300) - Z(337)
      Z(423) = Z(302) - Z(343)
      Z(424) = Z(304) - Z(384)
      Z(425) = Z(311) - 0.5D0*Z(390)
      Z(426) = Z(312) - Z(396)
      Z(427) = Z(313) - Z(400)
      Z(428) = Z(314) - Z(403)

      COEF(1,1) = -MT
      COEF(1,2) = 0
      COEF(1,3) = -Z(331)
      COEF(1,4) = -Z(333)
      COEF(1,5) = -Z(334)
      COEF(1,6) = -Z(335)
      COEF(1,7) = -Z(336)
      COEF(2,1) = 0
      COEF(2,2) = -MT
      COEF(2,3) = -Z(338)
      COEF(2,4) = -Z(339)
      COEF(2,5) = -Z(340)
      COEF(2,6) = -Z(341)
      COEF(2,7) = -Z(342)
      COEF(3,1) = -Z(331)
      COEF(3,2) = -Z(338)
      COEF(3,3) = -Z(375)
      COEF(3,4) = -Z(377)
      COEF(3,5) = -Z(379)
      COEF(3,6) = -Z(380)
      COEF(3,7) = -Z(381)
      COEF(4,1) = -Z(333)
      COEF(4,2) = -Z(339)
      COEF(4,3) = -Z(377)
      COEF(4,4) = -Z(385)
      COEF(4,5) = -Z(387)
      COEF(4,6) = -Z(388)
      COEF(4,7) = -Z(389)
      COEF(5,1) = -Z(334)
      COEF(5,2) = -Z(340)
      COEF(5,3) = -Z(379)
      COEF(5,4) = -Z(387)
      COEF(5,5) = -Z(393)
      COEF(5,6) = -Z(394)
      COEF(5,7) = -Z(395)
      COEF(6,1) = -Z(335)
      COEF(6,2) = -Z(341)
      COEF(6,3) = -Z(380)
      COEF(6,4) = -Z(388)
      COEF(6,5) = -Z(394)
      COEF(6,6) = -Z(398)
      COEF(6,7) = -Z(399)
      COEF(7,1) = -Z(336)
      COEF(7,2) = -Z(342)
      COEF(7,3) = -Z(381)
      COEF(7,4) = -Z(389)
      COEF(7,5) = -Z(395)
      COEF(7,6) = -Z(399)
      COEF(7,7) = -Z(402)
      RHS(1) = -Z(422)
      RHS(2) = -Z(423)
      RHS(3) = -Z(424)
      RHS(4) = -Z(425)
      RHS(5) = -Z(426)
      RHS(6) = -Z(427)
      RHS(7) = -Z(428)
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
     &K7,K8,L1,L10,L11,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,MTPB
     &,MTPK
      COMMON/INITIAL / Q1I,Q2I,Q3I,Q4I,Q5I,Q6I,Q7I,U1I,U2I,U3I,U4I,U5I,U
     &6I,U7I
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
     &X,POP8Y,POP9X,POP9Y,VOCMX,VOCMY,VOP2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(430),COEF(7,7),RHS(7)
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
      Z(148) = G*ME
      Z(315) = Z(17)*Z(148)
      Z(18) = L8 - L7
      Z(292) = MF*Z(18)
      Z(15) = COS(FOOTANG)
      Z(16) = SIN(FOOTANG)
      Z(55) = (L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF+L2*MG)/MT
      Z(56) = (L4*MB+2*L6*MC+2*L6*MD+2*L6*ME+2*L6*MF+2*L6*MG)/MT
      Z(57) = (L7*MC+L8*MD+L8*ME+L8*MF+L8*MG)/MT
      Z(58) = (L10*ME+L10*MF+L10*MG+L9*MD)/MT
      Z(59) = (L10*MF+ME*Z(17))/MT
      Z(60) = MF*Z(18)/MT
      Z(62) = L3*MB/MT
      Z(63) = MA + MB + MC + MD
      Z(64) = (L1*MA+L2*MB+L2*MC+L2*MD)/Z(63)
      Z(65) = (L4*MB+2*L6*MC+2*L6*MD)/Z(63)
      Z(66) = (L7*MC+L8*MD)/Z(63)
      Z(67) = L9*MD/Z(63)
      Z(68) = L3*MB/Z(63)
      Z(69) = ME + MF
      Z(70) = L2*(ME+MF)/Z(69)
      Z(71) = L6*(ME+MF)/Z(69)
      Z(72) = L8*(ME+MF)/Z(69)
      Z(73) = L10*(ME+MF)/Z(69)
      Z(74) = (L10*MF+ME*Z(17))/Z(69)
      Z(75) = MF*Z(18)/Z(69)
      Z(144) = G*MA
      Z(145) = G*MB
      Z(146) = G*MC
      Z(147) = G*MD
      Z(149) = G*MF
      Z(150) = G*MG
      Z(197) = Z(55) - L1
      Z(198) = Z(55) - L2
      Z(199) = 0.5D0*L4 - 0.5D0*Z(56)
      Z(200) = 0.5D0*L3 - 0.5D0*Z(62)
      Z(216) = L6 - 0.5D0*Z(56)
      Z(217) = L7 - Z(57)
      Z(218) = L8 - Z(57)
      Z(219) = L9 - Z(58)
      Z(220) = L10 - Z(58)
      Z(221) = Z(17) - Z(59)
      Z(222) = L10 - Z(59)
      Z(223) = Z(18) - Z(60)
      Z(229) = Z(199) + Z(15)*Z(200)
      Z(231) = Z(200) + Z(15)*Z(199)
      Z(236) = Z(15)*Z(62)
      Z(270) = L1*MA
      Z(271) = L2*MB
      Z(272) = L4*MB
      Z(273) = L3*MB
      Z(274) = L2*MC
      Z(275) = L6*MC
      Z(276) = L7*MC
      Z(277) = L2*MD
      Z(278) = L6*MD
      Z(279) = L8*MD
      Z(280) = L9*MD
      Z(281) = L2*ME
      Z(282) = L6*ME
      Z(283) = L8*ME
      Z(284) = L10*ME
      Z(285) = ME*Z(17)
      Z(287) = L2*MF
      Z(288) = L6*MF
      Z(289) = L8*MF
      Z(290) = L10*MF
      Z(294) = L2*MG
      Z(295) = L6*MG
      Z(296) = L8*MG
      Z(297) = L10*MG
      Z(301) = Z(144) + Z(145) + Z(146) + Z(147) + Z(148) + Z(149) + Z(1
     &50)
      Z(303) = L1*Z(144)
      Z(305) = L2*Z(145)
      Z(306) = L2*Z(146)
      Z(307) = L2*Z(147)
      Z(308) = L2*Z(148)
      Z(309) = L2*Z(149)
      Z(310) = L2*Z(150)
      Z(317) = Z(18)*Z(149)
      Z(332) = L1*MA + L2*MB + L2*MC + L2*MD + L2*ME + L2*MF + L2*MG
      Z(344) = IA + IB + IC + ID + IE + IF + IG + MA*L1**2
      Z(345) = L3**2 + L4**2 + 4*L2**2 + 2*L3*L4*Z(15)
      Z(346) = L2*L3
      Z(347) = L2*L4
      Z(348) = L2*L6
      Z(349) = L2*L7
      Z(350) = L2**2
      Z(351) = L6**2
      Z(352) = L7**2
      Z(353) = L6*L7
      Z(354) = L2*L8
      Z(355) = L2*L9
      Z(356) = L8**2
      Z(357) = L9**2
      Z(358) = L6*L8
      Z(359) = L6*L9
      Z(360) = L8*L9
      Z(361) = L10*L2
      Z(362) = L2*Z(17)
      Z(363) = L10**2
      Z(364) = Z(17)**2
      Z(365) = L10*L6
      Z(366) = L10*L8
      Z(367) = L10*Z(17)
      Z(368) = L6*Z(17)
      Z(369) = L8*Z(17)
      Z(370) = L10*Z(18)
      Z(371) = L2*Z(18)
      Z(372) = Z(18)**2
      Z(373) = L6*Z(18)
      Z(374) = L8*Z(18)
      Z(376) = -IA - MA*L1**2
      Z(378) = MA*L1**2
      Z(382) = L3*Z(16)
      Z(383) = L4*Z(16)
      Z(385) = IA + MA*L1**2 + MB*L2**2 + MC*L2**2 + MD*L2**2 + ME*L2**2
     & + MF*L2**2 + MG*L2**2
      Z(386) = IA + MA*L1**2
      Z(391) = IA + IB + MA*L1**2
      Z(392) = L2**2 + L6**2
      Z(397) = IA + IB + IC + MA*L1**2
      Z(401) = IA + IB + IC + ID + MA*L1**2
      Z(404) = IE + IF
      Z(419) = L2*MF*Z(18)


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
      U1 = VOCMX + 0.5*(2*MG*GS*SIN(Q3)*U3+2*(L10*ME+L10*MF+L10*MG+L9*MD
     &)*SIN(Q3-Q7)*(U3-U7)+2*(L10*MF+ME*(L10-L9))*SIN(EA-Q3)*(EAp-U3-U8)
     &+L3*MB*SIN(FOOTANG+Q3-Q5-Q6-Q7)*(U3-U5-U6-U7)+2*MF*(L7-L8)*SIN(EA-
     &FA-Q3)*(EAp-FAp-U3-U8-U9)+2*(L7*MC+L8*MD+L8*ME+L8*MF+L8*MG)*SIN(Q3
     &-Q6-Q7)*(U3-U6-U7)+(L4*MB+2*L6*MC+2*L6*MD+2*L6*ME+2*L6*MF+2*L6*MG)
     &*SIN(Q3-Q5-Q6-Q7)*(U3-U5-U6-U7)-2*MG*GSp*COS(Q3)-2*(L1*MA+L2*MB+L2
     &*MC+L2*MD+L2*ME+L2*MF+L2*MG)*SIN(Q3-Q4-Q5-Q6-Q7)*(U3-U4-U5-U6-U7))
     &/(MA+MB+MC+MD+ME+MF+MG)
      U2 = VOCMY - 0.5*(2*MG*GSp*SIN(Q3)+2*MG*GS*COS(Q3)*U3+2*(L10*ME+L1
     &0*MF+L10*MG+L9*MD)*COS(Q3-Q7)*(U3-U7)+L3*MB*COS(FOOTANG+Q3-Q5-Q6-Q
     &7)*(U3-U5-U6-U7)+2*(L7*MC+L8*MD+L8*ME+L8*MF+L8*MG)*COS(Q3-Q6-Q7)*(
     &U3-U6-U7)+(L4*MB+2*L6*MC+2*L6*MD+2*L6*ME+2*L6*MF+2*L6*MG)*COS(Q3-Q
     &5-Q6-Q7)*(U3-U5-U6-U7)-2*(L10*MF+ME*(L10-L9))*COS(EA-Q3)*(EAp-U3-U
     &8)-2*MF*(L7-L8)*COS(EA-FA-Q3)*(EAp-FAp-U3-U8-U9)-2*(L1*MA+L2*MB+L2
     &*MC+L2*MD+L2*ME+L2*MF+L2*MG)*COS(Q3-Q4-Q5-Q6-Q7)*(U3-U4-U5-U6-U7))
     &/(MA+MB+MC+MD+ME+MF+MG)

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
     &K7,K8,L1,L10,L11,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,MD,ME,MF,MG,MTPB
     &,MTPK
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
     &X,POP8Y,POP9X,POP9Y,VOCMX,VOCMY,VOP2X,VOP2Y
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,Z(430),COEF(7,7),RHS(7)
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