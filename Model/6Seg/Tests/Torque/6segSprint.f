C Six segment model used to examine the effects of the swinging limb
C on sprint performance during the stance phase.
C
C Trunk, stance thigh, swing thigh, stance shank, swing shank and stance
C foot. Position of HAT CoM from hip joint specified as a function of 
C time.
C
C Torque generators in series with rotational spring for flexion and 
C extension at stance joint. Angle-driven swing leg.
C
C Inertia parameters taken from McErlain-Naylor (2017), on which the 
C experimental data was collected, albeit two years apart. Torque 
C parameters from Allen (2009) and scaled by body mass to the subject.
C
C Initial conditions are segment angles and angular velocities and CoM
C velocities, specified in the .in file. Activation function parameters
C define how the activation or the torque generators varies with time
C read in from activation7.in.
C
C   Tom Rottier 2020
C***********************************************************************
      PROGRAM MAIN
      IMPLICIT         DOUBLE PRECISION (A - Z)
      INTEGER          ILOOP, IPRINT, PRINTINT
      INTEGER          I,J,NACTP,NROW
      PARAMETER        (NACTP=7)
      CHARACTER        MESSAGE(99)
      EXTERNAL         EQNS1
      DIMENSION        VAR(12)
      DIMENSION        TT(500),CCHIP(6,500),CCKNEE(6,500),CCHAT(6,500)
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
     &DApp,EApp,FSpp,POCMSTANCEX,POCMSTANCEY,POCMSWINGX,POCMSWINGY,POCMX
     &,POCMY,POFOX,POFOY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y
     &,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,VOCMX,VOCMY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(6,6),RHS(6)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW

C**   Open input and output files
      OPEN(UNIT=20, FILE='6segSprint.in', STATUS='OLD')
      OPEN(UNIT=21, FILE='6segSprint.1',  STATUS='UNKNOWN')
      OPEN(UNIT=22, FILE='6segSprint.2',  STATUS='UNKNOWN')
      OPEN(UNIT=23, FILE='6segSprint.3',  STATUS='UNKNOWN')
      OPEN(UNIT=24, FILE='6segSprint.4',  STATUS='UNKNOWN')
      OPEN(UNIT=25, FILE='6segSprint.5',  STATUS='UNKNOWN')
      OPEN(UNIT=26, FILE='6segSprint.6',  STATUS='UNKNOWN')
      OPEN(UNIT=27, FILE='6segSprint.7',  STATUS='UNKNOWN')
      OPEN(UNIT=28, FILE='6segSprint.8',  STATUS='UNKNOWN')
      OPEN(UNIT=29, FILE='6segSprint.9',  STATUS='UNKNOWN')

C**   Read message from input file
      READ(20,7000,END=7100,ERR=7101) (MESSAGE(ILOOP),ILOOP = 1,99)

C**   Read values of constants from input file
      READ(20,7010,END=7100,ERR=7101) G,IA,IB,IC,ID,IE,IF,K1,K2,K3,K4,L1
     &,L2,L3,L4,L5,L6,L7,MA,MB,MC,MD,ME,MF

C**   Read the initial value of each variable from input file
      READ(20,7010,END=7100,ERR=7101) Q1,Q2,Q3,Q4,Q5,Q6,U1,U2,U3,U4,U5,U
     &6

C**   Read integration parameters from input file
      READ(20,7011,END=7100,ERR=7101) TINITIAL,TFINAL,INTEGSTP,PRINTINT,
     &ABSERR,RELERR

C**   Write heading(s) to output file(s)
      ! WRITE(*, 6021) (MESSAGE(ILOOP), ILOOP = 1,99) 
      WRITE(21,6021) (MESSAGE(ILOOP), ILOOP = 1,99) 
      WRITE(22,6022) (MESSAGE(ILOOP), ILOOP = 1,99) 
      WRITE(23,6023) (MESSAGE(ILOOP), ILOOP = 1,99) 
      WRITE(24,6024) (MESSAGE(ILOOP), ILOOP = 1,99) 
      WRITE(25,6025) (MESSAGE(ILOOP), ILOOP = 1,99) 
      WRITE(26,6026) (MESSAGE(ILOOP), ILOOP = 1,99) 
      WRITE(27,6027) (MESSAGE(ILOOP), ILOOP = 1,99) 
      WRITE(28,6028) (MESSAGE(ILOOP), ILOOP = 1,99) 
      WRITE(29,6029) (MESSAGE(ILOOP), ILOOP = 1,99) 

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

C**   Convert to generalised coordinates
      PI       = 4*ATAN(1.0D0)
      DEGtoRAD = PI/180.0D0
      RADtoDEG = 180.0D0/PI
      CALL INITCOND()

C**   Evaluate constants
      U7 = 0
      U8 = 0

C**   Initialize time, print counter, variables array for integrator
      T      = TINITIAL
      IPRINT = 0
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

C** Initialise torques for integration
      CALL UPDATE(T)

C**   Initalize numerical integrator with call to EQNS1 at T=TINITIAL
      CALL KUTTA(EQNS1, 12, VAR, T, INTEGSTP, ABSERR, RELERR, 0, *5920)

C**   Check exit conditions
5900  IF( TFINAL.GE.TINITIAL .AND. T+.01D0*INTEGSTP.GE.TFINAL) IPRINT=-7
      IF( TFINAL.LE.TINITIAL .AND. T+.01D0*INTEGSTP.LE.TFINAL) IPRINT=-7
C** Print      
      IF( IPRINT .LE. 0 ) THEN
        CALL IO(T)
        IF( IPRINT .EQ. -7 ) GOTO 5930
        IPRINT = PRINTINT
      ENDIF

C** Integrate      
      CALL KUTTA(EQNS1, 12, VAR, T, INTEGSTP, ABSERR, RELERR, 1, *5920)


C** Update torques after integration
      CALL UPDATE(T)

      IPRINT = IPRINT - 1
      GOTO 5900

C**   Print message if numerical integration fails to converge
5920  CALL IO(T)
      WRITE(*, 6997)
      WRITE(21,6997)
      WRITE(22,6997)
      WRITE(23,6997)
      WRITE(24,6997)
      WRITE(25,6997)
      WRITE(26,6997)
      WRITE(27,6997)
      WRITE(28,6997)
      WRITE(29,6997)

C**   Inform user of input and output filename(s)
5930  WRITE(*,6999)

6021  FORMAT(1X,'FILE: 6segsprint.1 ',//1X,'*** ',99A1,///,8X,'T',12X,'P
     &OP1X',10X,'POP1Y',10X,'POP2X',10X,'POP2Y',10X,'POP3X',10X,'POP3Y',
     &10X,'POP4X',10X,'POP4Y',10X,'POP5X',10X,'POP5Y',10X,'POP6X',10X,'P
     &OP6Y',10X,'POP7X',10X,'POP7Y',10X,'POFOX',10X,'POFOY',7X,'POCMSTAN
     &CEX',4X,'POCMSTANCEY',4X,'POCMSWINGX',5X,'POCMSWINGY',8X,'POCMX',1
     &0X,'POCMY',10X,'VOCMX',10X,'VOCMY',/,7X,'(S)',12X,'(M)',12X,'(M)',
     &12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(
     &M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12
     &X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)
     &',11X,'(M/S)',10X,'(M/S)',/)
6022  FORMAT(1X,'FILE: 6segsprint.2 ',//1X,'*** ',99A1,///,8X,'T',13X,'Q
     &1',13X,'Q2',13X,'Q3',13X,'Q4',13X,'Q5',13X,'Q6',13X,'U1',13X,'U2',
     &13X,'U3',13X,'U4',13X,'U5',13X,'U6',/,7X,'(S)',12X,'(M)',12X,'(M)'
     &,11X,'(DEG)',10X,'(DEG)',10X,'(DEG)',10X,'(DEG)',10X,'(M/S)',10X,'
     &(M/S)',9X,'(DEG/S)',8X,'(DEG/S)',8X,'(DEG/S)',8X,'(DEG/S)',/)
6023  FORMAT(1X,'FILE: 6segsprint.3 ',//1X,'*** ',99A1,///,8X,'T',13X,'R
     &X',13X,'RY',12X,'HTOR',11X,'KTOR',11X,'ATOR',11X,'SHTOR',10X,'SKTO
     &R',/,7X,'(S)',12X,'(N)',12X,'(N)',11X,'(N/M)',10X,'(N/M)',10X,'(N/
     &M)',10X,'(N/M)',10X,'(N/M)',/)
6024  FORMAT(1X,'FILE: 6segsprint.4 ',//1X,'*** ',99A1,///,8X,'T',13X,'Q
     &3',12X,'HANG',11X,'KANG',11X,'AANG',11X,'SHANG',10X,'SKANG',11X,'U
     &3',11X,'HANGVEL',8X,'KANGVEL',8X,'AANGVEL',7X,'SHANGVEL',7X,'SKANG
     &VEL',/,7X,'(S)',11X,'(DEG)',10X,'(DEG)',10X,'(DEG)',10X,'(DEG)',10
     &X,'(DEG)',10X,'(DEG)',9X,'(DEG/S)',8X,'(DEG/S)',8X,'(DEG/S)',8X,'(
     &DEG/S)',8X,'(DEG/S)',8X,'(DEG/S)',/)
6025  FORMAT(1X,'FILE: 6segsprint.5 ',//1X,'*** ',99A1,///,8X,'T',12X,'K
     &ECM',11X,'PECM',12X,'TE',13X,'HZ',13X,'PX',13X,'PY',/,7X,'(S)',12X
     &,'(J)',12X,'(J)',12X,'(J)',8X,'(KG.M^2/S)',6X,'(KG.M/S)',7X,'(KG.M
     &/S)',/)
6026  FORMAT(1X,'FILE: 6segsprint.6 ',//1X,'*** ',99A1,///,8X,'T',12X,'H
     &EACT',10X,'HFACT',10X,'KEACT',10X,'KFACT',10X,'AEACT',10X,'AFACT',
     &/,7X,'(S)',10X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X
     &,'(UNITS)',8X,'(UNITS)',/)
6027  FORMAT(1X,'FILE: 6segsprint.7 ',//1X,'*** ',99A1,///,8X,'T',12X,'H
     &ETOR',10X,'HEACT',9X,'HECCANG',6X,'HECCANGVEL',6X,'HESECANG',6X,'H
     &ESECANGVEL',7X,'HFTOR',10X,'HFACT',9X,'HFCCANG',6X,'HFCCANGVEL',6X
     &,'HFSECANG',6X,'HFSECANGVEL',/,7X,'(S)',11X,'(N/M)',9X,'(UNITS)',9
     &X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',9X,'(N/M)',9X,'(UNI
     &TS)',9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',/)
6028  FORMAT(1X,'FILE: 6segsprint.8 ',//1X,'*** ',99A1,///,8X,'T',12X,'K
     &ETOR',10X,'KEACT',9X,'KECCANG',6X,'KECCANGVEL',6X,'KESECANG',6X,'K
     &ESECANGVEL',7X,'KFTOR',10X,'KFACT',9X,'KFCCANG',6X,'KFCCANGVEL',6X
     &,'KFSECANG',6X,'KFSECANGVEL',/,7X,'(S)',11X,'(N/M)',9X,'(UNITS)',9
     &X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',9X,'(N/M)',9X,'(UNI
     &TS)',9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',/)
6029  FORMAT(1X,'FILE: 6segsprint.9 ',//1X,'*** ',99A1,///,8X,'T',12X,'A
     &ETOR',10X,'AEACT',9X,'AECCANG',6X,'AECCANGVEL',6X,'AESECANG',6X,'A
     &ESECANGVEL',7X,'AFTOR',10X,'AFACT',9X,'AFCCANG',6X,'AFCCANGVEL',6X
     &,'AFSECANG',6X,'AFSECANGVEL',/,7X,'(S)',11X,'(N/M)',9X,'(UNITS)',9
     &X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',9X,'(N/M)',9X,'(UNI
     &TS)',9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',/)
6997  FORMAT(/7X,'Error: Numerical integration failed to converge',/)
6999  FORMAT(//1X,'Input is in the file 6segsprint.in',//1X,'Output is i
     &n the file(s) 6segsprint.i  (i=1, ..., 9)',//1X,'The output quanti
     &ties and associated files are listed in file 6segsprint.dir',/)
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
7300  FORMAT(//, 6(///, 7(6X, F11.6, /)))
7310  WRITE(*,*) 'Error reading activation parameters'
      STOP
      END PROGRAM MAIN

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
     &DApp,EApp,FSpp,POCMSTANCEX,POCMSTANCEY,POCMSWINGX,POCMSWINGY,POCMX
     &,POCMY,POFOX,POFOY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y
     &,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,VOCMX,VOCMY
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
      ! CALL EVALSPLINE2(T,NROW,TT,CCHIP,DA,DAp,DApp)
      ! CALL EVALSPLINE2(T,NROW,TT,CCKNEE,EA,EAp,EApp)
      ! DA   = DA  *DEGtoRAD 
      ! DAp  = DAp *DEGtoRAD 
      ! DApp = DApp*DEGtoRAD 
      ! EA   = EA  *DEGtoRAD 
      ! EAp  = EAp *DEGtoRAD 
      ! EApp = EApp*DEGtoRAD 
      DApp = -100
      DAp  = -100*T
      DA   = -50*T**2 + PI
      EApp = 0.0
      EAp  = 0.0
      EA   = pi

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


C**********************************************************************
      SUBROUTINE       IO(T)
      IMPLICIT         DOUBLE PRECISION (A - Z)
      INTEGER          ILOOP,NACTP
      PARAMETER        (NACTP=7)
      DIMENSION        HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(
     &NACTP),AEACTP(NACTP),AFACTP(NACTP)
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
     &DApp,EApp,FSpp,POCMSTANCEX,POCMSTANCEY,POCMSWINGX,POCMSWINGY,POCMX
     &,POCMY,POFOX,POFOY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y
     &,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,VOCMX,VOCMY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(6,6),RHS(6)
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP

C**   Evaluate output quantities
      KECM = 0.5D0*IF*U3**2 + 0.5D0*IC*(U3-U6)**2 + 0.5D0*IB*(U3-U5-U6)*
     &*2 + 0.5D0*ID*(DAp-U3-U7)**2 + 0.5D0*IA*(U3-U4-U5-U6)**2 + 0.5D0*I
     &E*(DAp-EAp-U3-U7-U8)**2 + 0.5D0*ME*(U1**2+U2**2+L6**2*(U3-U6)**2+L
     &4**2*(U3-U5-U6)**2+L6**2*(DAp-U3-U7)**2+L2**2*(U3-U4-U5-U6)**2+2*L
     &6*COS(Q3-Q6)*U2*(U3-U6)+2*L4*COS(Q3-Q5-Q6)*U2*(U3-U5-U6)+2*L4*L6*C
     &OS(Q5)*(U3-U6)*(U3-U5-U6)+(L3-L4)**2*(DAp-EAp-U3-U7-U8)**2+2*L2*L6
     &*COS(Q4+Q5)*(U3-U6)*(U3-U4-U5-U6)+2*L2*COS(Q3-Q4-Q5-Q6)*U2*(U3-U4-
     &U5-U6)+2*L2*L4*COS(Q4)*(U3-U5-U6)*(U3-U4-U5-U6)+2*L6*(L3-L4)*COS(E
     &A)*(DAp-U3-U7)*(DAp-EAp-U3-U7-U8)-2*L6*SIN(Q3-Q6)*U1*(U3-U6)-2*L6*
     &SIN(DA-Q3)*U1*(DAp-U3-U7)-2*L6*COS(DA-Q3)*U2*(DAp-U3-U7)-2*L4*SIN(
     &Q3-Q5-Q6)*U1*(U3-U5-U6)-2*L6**2*COS(DA-Q6)*(U3-U6)*(DAp-U3-U7)-2*L
     &2*SIN(Q3-Q4-Q5-Q6)*U1*(U3-U4-U5-U6)-2*L4*L6*COS(DA-Q5-Q6)*(DAp-U3-
     &U7)*(U3-U5-U6)-2*(L3-L4)*SIN(DA-EA-Q3)*U1*(DAp-EAp-U3-U7-U8)-2*(L3
     &-L4)*COS(DA-EA-Q3)*U2*(DAp-EAp-U3-U7-U8)-2*L2*L6*COS(DA-Q4-Q5-Q6)*
     &(DAp-U3-U7)*(U3-U4-U5-U6)-2*L6*(L3-L4)*COS(DA-EA-Q6)*(U3-U6)*(DAp-
     &EAp-U3-U7-U8)-2*L4*(L3-L4)*COS(DA-EA-Q5-Q6)*(U3-U5-U6)*(DAp-EAp-U3
     &-U7-U8)-2*L2*(L3-L4)*COS(DA-EA-Q4-Q5-Q6)*(U3-U4-U5-U6)*(DAp-EAp-U3
     &-U7-U8)) - 0.5D0*MA*(2*L1*SIN(Q3-Q4-Q5-Q6)*U1*(U3-U4-U5-U6)-U1**2-
     &U2**2-L1**2*(U3-U4-U5-U6)**2-2*L1*COS(Q3-Q4-Q5-Q6)*U2*(U3-U4-U5-U6
     &)) - 0.5D0*MB*(2*L3*SIN(Q3-Q5-Q6)*U1*(U3-U5-U6)+2*L2*SIN(Q3-Q4-Q5-
     &Q6)*U1*(U3-U4-U5-U6)-U1**2-U2**2-L3**2*(U3-U5-U6)**2-L2**2*(U3-U4-
     &U5-U6)**2-2*L3*COS(Q3-Q5-Q6)*U2*(U3-U5-U6)-2*L2*COS(Q3-Q4-Q5-Q6)*U
     &2*(U3-U4-U5-U6)-2*L2*L3*COS(Q4)*(U3-U5-U6)*(U3-U4-U5-U6)) - 0.5D0*
     &MC*(2*L5*SIN(Q3-Q6)*U1*(U3-U6)+2*L4*SIN(Q3-Q5-Q6)*U1*(U3-U5-U6)+2*
     &L2*SIN(Q3-Q4-Q5-Q6)*U1*(U3-U4-U5-U6)-U1**2-U2**2-L5**2*(U3-U6)**2-
     &L4**2*(U3-U5-U6)**2-2*L5*COS(Q3-Q6)*U2*(U3-U6)-L2**2*(U3-U4-U5-U6)
     &**2-2*L4*COS(Q3-Q5-Q6)*U2*(U3-U5-U6)-2*L4*L5*COS(Q5)*(U3-U6)*(U3-U
     &5-U6)-2*L2*L5*COS(Q4+Q5)*(U3-U6)*(U3-U4-U5-U6)-2*L2*COS(Q3-Q4-Q5-Q
     &6)*U2*(U3-U4-U5-U6)-2*L2*L4*COS(Q4)*(U3-U5-U6)*(U3-U4-U5-U6)) - 0.
     &5D0*MF*(2*FS*SIN(Q3)*U1*U3+2*L6*SIN(Q3-Q6)*U1*(U3-U6)+2*L4*SIN(Q3-
     &Q5-Q6)*U1*(U3-U5-U6)+2*L2*SIN(Q3-Q4-Q5-Q6)*U1*(U3-U4-U5-U6)-FSp**2
     &-U1**2-U2**2-FS**2*U3**2-2*FSp*SIN(Q3)*U2-2*FSp*COS(Q3)*U1-2*FS*CO
     &S(Q3)*U2*U3-L6**2*(U3-U6)**2-2*L6*FSp*SIN(Q6)*(U3-U6)-L4**2*(U3-U5
     &-U6)**2-2*L6*FS*COS(Q6)*U3*(U3-U6)-2*L6*COS(Q3-Q6)*U2*(U3-U6)-L2**
     &2*(U3-U4-U5-U6)**2-2*L4*FSp*SIN(Q5+Q6)*(U3-U5-U6)-2*L4*FS*COS(Q5+Q
     &6)*U3*(U3-U5-U6)-2*L2*FSp*SIN(Q4+Q5+Q6)*(U3-U4-U5-U6)-2*L4*COS(Q3-
     &Q5-Q6)*U2*(U3-U5-U6)-2*L4*L6*COS(Q5)*(U3-U6)*(U3-U5-U6)-2*L2*FS*CO
     &S(Q4+Q5+Q6)*U3*(U3-U4-U5-U6)-2*L2*L6*COS(Q4+Q5)*(U3-U6)*(U3-U4-U5-
     &U6)-2*L2*COS(Q3-Q4-Q5-Q6)*U2*(U3-U4-U5-U6)-2*L2*L4*COS(Q4)*(U3-U5-
     &U6)*(U3-U4-U5-U6)) - 0.5D0*MD*(2*L6*SIN(Q3-Q6)*U1*(U3-U6)+2*L4*SIN
     &(Q3-Q5-Q6)*U1*(U3-U5-U6)+2*L2*SIN(Q3-Q4-Q5-Q6)*U1*(U3-U4-U5-U6)-U1
     &**2-U2**2-L6**2*(U3-U6)**2-L4**2*(U3-U5-U6)**2-2*L6*COS(Q3-Q6)*U2*
     &(U3-U6)-L2**2*(U3-U4-U5-U6)**2-(L5-L6)**2*(DAp-U3-U7)**2-2*L4*COS(
     &Q3-Q5-Q6)*U2*(U3-U5-U6)-2*L4*L6*COS(Q5)*(U3-U6)*(U3-U5-U6)-2*(L5-L
     &6)*SIN(DA-Q3)*U1*(DAp-U3-U7)-2*(L5-L6)*COS(DA-Q3)*U2*(DAp-U3-U7)-2
     &*L2*L6*COS(Q4+Q5)*(U3-U6)*(U3-U4-U5-U6)-2*L2*COS(Q3-Q4-Q5-Q6)*U2*(
     &U3-U4-U5-U6)-2*L2*L4*COS(Q4)*(U3-U5-U6)*(U3-U4-U5-U6)-2*L6*(L5-L6)
     &*COS(DA-Q6)*(U3-U6)*(DAp-U3-U7)-2*L4*(L5-L6)*COS(DA-Q5-Q6)*(DAp-U3
     &-U7)*(U3-U5-U6)-2*L2*(L5-L6)*COS(DA-Q4-Q5-Q6)*(DAp-U3-U7)*(U3-U4-U
     &5-U6))
      POCMY = Q2 - (ME*(L3-L4)*SIN(DA-EA-Q3)+(L6*ME-MD*(L5-L6))*SIN(DA-Q
     &3)-MF*FS*SIN(Q3)-(L5*MC+L6*MD+L6*ME+L6*MF)*SIN(Q3-Q6)-(L3*MB+L4*MC
     &+L4*MD+L4*ME+L4*MF)*SIN(Q3-Q5-Q6)-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L
     &2*MF)*SIN(Q3-Q4-Q5-Q6))/(MA+MB+MC+MD+ME+MF)
      PECM = 0.5D0*K1*Q1**2 + 0.5D0*K3*Q2**2 - G*(MA+MB+MC+MD+ME+MF)*POC
     &MY
      TE = KECM + PECM
      HZ = IA*U3 + IB*U3 + IC*U3 + ID*U3 + IE*U3 + IF*U3 + L1*MA*(L1-(L1
     &*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF+MF*FS*COS(Q4+Q5+Q6)+(L5*MC+L6*MD
     &+L6*ME+L6*MF)*COS(Q4+Q5)+(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)*COS(Q4)+M
     &E*(L3-L4)*COS(DA-EA-Q4-Q5-Q6)+(L6*ME-MD*(L5-L6))*COS(DA-Q4-Q5-Q6))
     &/(MA+MB+MC+MD+ME+MF))*(U3-U4-U5-U6) + L3*MB*(L3+(L2-(L1*MA+L2*MB+L
     &2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4)-(L3*MB+L4*MC+
     &L4*MD+L4*ME+L4*MF+MF*FS*COS(Q5+Q6)+(L5*MC+L6*MD+L6*ME+L6*MF)*COS(Q
     &5)+ME*(L3-L4)*COS(DA-EA-Q5-Q6)+(L6*ME-MD*(L5-L6))*COS(DA-Q5-Q6))/(
     &MA+MB+MC+MD+ME+MF))*(U3-U5-U6) + L5*MC*(L5+(L4-(L3*MB+L4*MC+L4*MD+
     &L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q5)+(L2-(L1*MA+L2*MB+L2*MC+L
     &2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4+Q5)-(L5*MC+L6*MD+L6*
     &ME+L6*MF+MF*FS*COS(Q6)+ME*(L3-L4)*COS(DA-EA-Q6)+(L6*ME-MD*(L5-L6))
     &*COS(DA-Q6))/(MA+MB+MC+MD+ME+MF))*(U3-U6) + L2*MB*(L2+(L3-(L3*MB+L
     &4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4)-(L1*MA+L2*MB+
     &L2*MC+L2*MD+L2*ME+L2*MF+MF*FS*COS(Q4+Q5+Q6)+(L5*MC+L6*MD+L6*ME+L6*
     &MF)*COS(Q4+Q5)+ME*(L3-L4)*COS(DA-EA-Q4-Q5-Q6)+(L6*ME-MD*(L5-L6))*C
     &OS(DA-Q4-Q5-Q6))/(MA+MB+MC+MD+ME+MF))*(U3-U4-U5-U6) + L4*MC*(L4+(L
     &5-(L5*MC+L6*MD+L6*ME+L6*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q5)+(L2-(L1*M
     &A+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4)-(L3*
     &MB+L4*MC+L4*MD+L4*ME+L4*MF+MF*FS*COS(Q5+Q6)+ME*(L3-L4)*COS(DA-EA-Q
     &5-Q6)+(L6*ME-MD*(L5-L6))*COS(DA-Q5-Q6))/(MA+MB+MC+MD+ME+MF))*(U3-U
     &5-U6) + L6*MF*(L6+(1-MF/(MA+MB+MC+MD+ME+MF))*FS*COS(Q6)+(L4-(L3*MB
     &+L4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q5)+(L2-(L1*MA+
     &L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4+Q5)-(L5
     &*MC+L6*MD+L6*ME+L6*MF+ME*(L3-L4)*COS(DA-EA-Q6)+(L6*ME-MD*(L5-L6))*
     &COS(DA-Q6))/(MA+MB+MC+MD+ME+MF))*(U3-U6) + L6*MD*(L6+(L4-(L3*MB+L4
     &*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q5)+(L6-L5-(L6*ME-
     &MD*(L5-L6))/(MA+MB+MC+MD+ME+MF))*COS(DA-Q6)+(L2-(L1*MA+L2*MB+L2*MC
     &+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4+Q5)-(L5*MC+L6*MD+L
     &6*ME+L6*MF+MF*FS*COS(Q6)+ME*(L3-L4)*COS(DA-EA-Q6))/(MA+MB+MC+MD+ME
     &+MF))*(U3-U6) + L2*MC*(L2+(L5-(L5*MC+L6*MD+L6*ME+L6*MF)/(MA+MB+MC+
     &MD+ME+MF))*COS(Q4+Q5)+(L4-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+M
     &C+MD+ME+MF))*COS(Q4)-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF+MF*FS*CO
     &S(Q4+Q5+Q6)+ME*(L3-L4)*COS(DA-EA-Q4-Q5-Q6)+(L6*ME-MD*(L5-L6))*COS(
     &DA-Q4-Q5-Q6))/(MA+MB+MC+MD+ME+MF))*(U3-U4-U5-U6) + L4*MF*(L4+(1-MF
     &/(MA+MB+MC+MD+ME+MF))*FS*COS(Q5+Q6)+(L6-(L5*MC+L6*MD+L6*ME+L6*MF)/
     &(MA+MB+MC+MD+ME+MF))*COS(Q5)+(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2
     &*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4)-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF+M
     &E*(L3-L4)*COS(DA-EA-Q5-Q6)+(L6*ME-MD*(L5-L6))*COS(DA-Q5-Q6))/(MA+M
     &B+MC+MD+ME+MF))*(U3-U5-U6) + L4*MD*(L4+(L6-(L5*MC+L6*MD+L6*ME+L6*M
     &F)/(MA+MB+MC+MD+ME+MF))*COS(Q5)+(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME
     &+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4)+(L6-L5-(L6*ME-MD*(L5-L6))/(MA
     &+MB+MC+MD+ME+MF))*COS(DA-Q5-Q6)-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF+MF*
     &FS*COS(Q5+Q6)+ME*(L3-L4)*COS(DA-EA-Q5-Q6))/(MA+MB+MC+MD+ME+MF))*(U
     &3-U5-U6) + L2*MF*(L2+(1-MF/(MA+MB+MC+MD+ME+MF))*FS*COS(Q4+Q5+Q6)+(
     &L6-(L5*MC+L6*MD+L6*ME+L6*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4+Q5)+(L4-(
     &L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4)-(L1*MA
     &+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF+ME*(L3-L4)*COS(DA-EA-Q4-Q5-Q6)+(L6*
     &ME-MD*(L5-L6))*COS(DA-Q4-Q5-Q6))/(MA+MB+MC+MD+ME+MF))*(U3-U4-U5-U6
     &) + L2*MD*(L2+(L6-(L5*MC+L6*MD+L6*ME+L6*MF)/(MA+MB+MC+MD+ME+MF))*C
     &OS(Q4+Q5)+(L4-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))
     &*COS(Q4)+(L6-L5-(L6*ME-MD*(L5-L6))/(MA+MB+MC+MD+ME+MF))*COS(DA-Q4-
     &Q5-Q6)-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF+MF*FS*COS(Q4+Q5+Q6)+ME
     &*(L3-L4)*COS(DA-EA-Q4-Q5-Q6))/(MA+MB+MC+MD+ME+MF))*(U3-U4-U5-U6) +
     & ME*(L3-L4)*(L4+(ME*(L3-L4)+MF*FS*COS(DA-EA))/(MA+MB+MC+MD+ME+MF)-
     &L3-(L6-(L6*ME-MD*(L5-L6))/(MA+MB+MC+MD+ME+MF))*COS(EA)-(L6-(L5*MC+
     &L6*MD+L6*ME+L6*MF)/(MA+MB+MC+MD+ME+MF))*COS(DA-EA-Q6)-(L4-(L3*MB+L
     &4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(DA-EA-Q5-Q6)-(L2-
     &(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(DA-
     &EA-Q4-Q5-Q6))*(DAp-EAp-U3-U7-U8) + MF*((L6-(L5*MC+L6*MD+L6*ME+L6*M
     &F)/(MA+MB+MC+MD+ME+MF))*FSp*SIN(Q6)+(L4-(L3*MB+L4*MC+L4*MD+L4*ME+L
     &4*MF)/(MA+MB+MC+MD+ME+MF))*FSp*SIN(Q5+Q6)+(L2-(L1*MA+L2*MB+L2*MC+L
     &2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*FSp*SIN(Q4+Q5+Q6)+(1-MF/(MA
     &+MB+MC+MD+ME+MF))*FS**2*U3+(L6-(L5*MC+L6*MD+L6*ME+L6*MF)/(MA+MB+MC
     &+MD+ME+MF))*FS*COS(Q6)*U3+(L4-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)/(MA+
     &MB+MC+MD+ME+MF))*FS*COS(Q5+Q6)*U3+(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*
     &ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*FS*COS(Q4+Q5+Q6)*U3-(ME*(L3-L4)*SIN
     &(DA-EA)*FSp+(L6*ME-MD*(L5-L6))*SIN(DA)*FSp+ME*(L3-L4)*FS*COS(DA-EA
     &)*U3+(L6*ME-MD*(L5-L6))*FS*COS(DA)*U3)/(MA+MB+MC+MD+ME+MF)) + MF*(
     &(1-MF/(MA+MB+MC+MD+ME+MF))*FS*COS(Q3)*U2+(L6-(L5*MC+L6*MD+L6*ME+L6
     &*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q3-Q6)*U2+(L4-(L3*MB+L4*MC+L4*MD+L4*
     &ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q3-Q5-Q6)*U2+(L2-(L1*MA+L2*MB+L
     &2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q3-Q4-Q5-Q6)*U2-(
     &1-MF/(MA+MB+MC+MD+ME+MF))*FS*SIN(Q3)*U1-(L6-(L5*MC+L6*MD+L6*ME+L6*
     &MF)/(MA+MB+MC+MD+ME+MF))*SIN(Q3-Q6)*U1-(L4-(L3*MB+L4*MC+L4*MD+L4*M
     &E+L4*MF)/(MA+MB+MC+MD+ME+MF))*SIN(Q3-Q5-Q6)*U1-(L2-(L1*MA+L2*MB+L2
     &*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*SIN(Q3-Q4-Q5-Q6)*U1-(M
     &E*(L3-L4)*SIN(DA-EA-Q3)*U1+ME*(L3-L4)*COS(DA-EA-Q3)*U2+(L6*ME-MD*(
     &L5-L6))*SIN(DA-Q3)*U1+(L6*ME-MD*(L5-L6))*COS(DA-Q3)*U2)/(MA+MB+MC+
     &MD+ME+MF)) - IC*U6 - IB*(U5+U6) - IA*(U4+U5+U6) - ID*(DAp-U7) - IE
     &*(DAp-EAp-U7-U8) - L6*ME*((L5*MC+L6*MD+L6*ME+L6*MF+MF*FS*COS(Q6))/
     &(MA+MB+MC+MD+ME+MF)-L6-(L3-L4-ME*(L3-L4)/(MA+MB+MC+MD+ME+MF))*COS(
     &DA-EA-Q6)-(L4-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))
     &*COS(Q5)-(L6-(L6*ME-MD*(L5-L6))/(MA+MB+MC+MD+ME+MF))*COS(DA-Q6)-(L
     &2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q
     &4+Q5))*(U3-U6) - MD*(L5-L6)*(L5-L6-(MD*(L5-L6)-L6*ME-MF*FS*COS(DA)
     &-ME*(L3-L4)*COS(EA))/(MA+MB+MC+MD+ME+MF)-(L6-(L5*MC+L6*MD+L6*ME+L6
     &*MF)/(MA+MB+MC+MD+ME+MF))*COS(DA-Q6)-(L4-(L3*MB+L4*MC+L4*MD+L4*ME+
     &L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(DA-Q5-Q6)-(L2-(L1*MA+L2*MB+L2*MC+L
     &2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(DA-Q4-Q5-Q6))*(DAp-U3-U
     &7) - L6*ME*(L6+(L3-L4-ME*(L3-L4)/(MA+MB+MC+MD+ME+MF))*COS(EA)+(MD*
     &(L5-L6)-L6*ME-MF*FS*COS(DA))/(MA+MB+MC+MD+ME+MF)+(L6-(L5*MC+L6*MD+
     &L6*ME+L6*MF)/(MA+MB+MC+MD+ME+MF))*COS(DA-Q6)+(L4-(L3*MB+L4*MC+L4*M
     &D+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(DA-Q5-Q6)+(L2-(L1*MA+L2*MB
     &+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(DA-Q4-Q5-Q6))*(
     &DAp-U3-U7) - L4*ME*((L3*MB+L4*MC+L4*MD+L4*ME+L4*MF+MF*FS*COS(Q5+Q6
     &))/(MA+MB+MC+MD+ME+MF)-L4-(L6-(L5*MC+L6*MD+L6*ME+L6*MF)/(MA+MB+MC+
     &MD+ME+MF))*COS(Q5)-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+M
     &B+MC+MD+ME+MF))*COS(Q4)-(L3-L4-ME*(L3-L4)/(MA+MB+MC+MD+ME+MF))*COS
     &(DA-EA-Q5-Q6)-(L6-(L6*ME-MD*(L5-L6))/(MA+MB+MC+MD+ME+MF))*COS(DA-Q
     &5-Q6))*(U3-U5-U6) - L2*ME*((L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF+MF
     &*FS*COS(Q4+Q5+Q6))/(MA+MB+MC+MD+ME+MF)-L2-(L6-(L5*MC+L6*MD+L6*ME+L
     &6*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4+Q5)-(L4-(L3*MB+L4*MC+L4*MD+L4*ME
     &+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q4)-(L3-L4-ME*(L3-L4)/(MA+MB+MC+M
     &D+ME+MF))*COS(DA-EA-Q4-Q5-Q6)-(L6-(L6*ME-MD*(L5-L6))/(MA+MB+MC+MD+
     &ME+MF))*COS(DA-Q4-Q5-Q6))*(U3-U4-U5-U6) - MA*((L1-(L1*MA+L2*MB+L2*
     &MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*SIN(Q3-Q4-Q5-Q6)*U1-(L1
     &-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q3
     &-Q4-Q5-Q6)*U2-(MF*FS*SIN(Q3)*U1+(L5*MC+L6*MD+L6*ME+L6*MF)*SIN(Q3-Q
     &6)*U1+(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)*SIN(Q3-Q5-Q6)*U1-MF*FS*COS(Q
     &3)*U2-ME*(L3-L4)*SIN(DA-EA-Q3)*U1-ME*(L3-L4)*COS(DA-EA-Q3)*U2-(L5*
     &MC+L6*MD+L6*ME+L6*MF)*COS(Q3-Q6)*U2-(L6*ME-MD*(L5-L6))*SIN(DA-Q3)*
     &U1-(L6*ME-MD*(L5-L6))*COS(DA-Q3)*U2-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF
     &)*COS(Q3-Q5-Q6)*U2)/(MA+MB+MC+MD+ME+MF)) - MB*((L3-(L3*MB+L4*MC+L4
     &*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*SIN(Q3-Q5-Q6)*U1+(L2-(L1*MA+
     &L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*SIN(Q3-Q4-Q5-Q
     &6)*U1-(L3-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS
     &(Q3-Q5-Q6)*U2-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+
     &MD+ME+MF))*COS(Q3-Q4-Q5-Q6)*U2-(MF*FS*SIN(Q3)*U1+(L5*MC+L6*MD+L6*M
     &E+L6*MF)*SIN(Q3-Q6)*U1-MF*FS*COS(Q3)*U2-ME*(L3-L4)*SIN(DA-EA-Q3)*U
     &1-ME*(L3-L4)*COS(DA-EA-Q3)*U2-(L5*MC+L6*MD+L6*ME+L6*MF)*COS(Q3-Q6)
     &*U2-(L6*ME-MD*(L5-L6))*SIN(DA-Q3)*U1-(L6*ME-MD*(L5-L6))*COS(DA-Q3)
     &*U2)/(MA+MB+MC+MD+ME+MF)) - MC*((L5-(L5*MC+L6*MD+L6*ME+L6*MF)/(MA+
     &MB+MC+MD+ME+MF))*SIN(Q3-Q6)*U1+(L4-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)
     &/(MA+MB+MC+MD+ME+MF))*SIN(Q3-Q5-Q6)*U1+(L2-(L1*MA+L2*MB+L2*MC+L2*M
     &D+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*SIN(Q3-Q4-Q5-Q6)*U1-(L5-(L5*MC
     &+L6*MD+L6*ME+L6*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q3-Q6)*U2-(L4-(L3*MB+
     &L4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q3-Q5-Q6)*U2-(L2
     &-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q3
     &-Q4-Q5-Q6)*U2-(MF*FS*SIN(Q3)*U1-MF*FS*COS(Q3)*U2-ME*(L3-L4)*SIN(DA
     &-EA-Q3)*U1-ME*(L3-L4)*COS(DA-EA-Q3)*U2-(L6*ME-MD*(L5-L6))*SIN(DA-Q
     &3)*U1-(L6*ME-MD*(L5-L6))*COS(DA-Q3)*U2)/(MA+MB+MC+MD+ME+MF)) - MD*
     &((L6-(L5*MC+L6*MD+L6*ME+L6*MF)/(MA+MB+MC+MD+ME+MF))*SIN(Q3-Q6)*U1+
     &(L4-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*SIN(Q3-Q5
     &-Q6)*U1+(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+
     &MF))*SIN(Q3-Q4-Q5-Q6)*U1-(L6-(L5*MC+L6*MD+L6*ME+L6*MF)/(MA+MB+MC+M
     &D+ME+MF))*COS(Q3-Q6)*U2-(L6-L5-(L6*ME-MD*(L5-L6))/(MA+MB+MC+MD+ME+
     &MF))*SIN(DA-Q3)*U1-(L6-L5-(L6*ME-MD*(L5-L6))/(MA+MB+MC+MD+ME+MF))*
     &COS(DA-Q3)*U2-(L4-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+
     &MF))*COS(Q3-Q5-Q6)*U2-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(M
     &A+MB+MC+MD+ME+MF))*COS(Q3-Q4-Q5-Q6)*U2-(MF*FS*SIN(Q3)*U1-MF*FS*COS
     &(Q3)*U2-ME*(L3-L4)*SIN(DA-EA-Q3)*U1-ME*(L3-L4)*COS(DA-EA-Q3)*U2)/(
     &MA+MB+MC+MD+ME+MF)) - ME*((L6-(L5*MC+L6*MD+L6*ME+L6*MF)/(MA+MB+MC+
     &MD+ME+MF))*SIN(Q3-Q6)*U1+(L4-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)/(MA+M
     &B+MC+MD+ME+MF))*SIN(Q3-Q5-Q6)*U1+(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*M
     &E+L2*MF)/(MA+MB+MC+MD+ME+MF))*SIN(Q3-Q4-Q5-Q6)*U1-MF*FS*(SIN(Q3)*U
     &1-COS(Q3)*U2)/(MA+MB+MC+MD+ME+MF)-(L3-L4-ME*(L3-L4)/(MA+MB+MC+MD+M
     &E+MF))*SIN(DA-EA-Q3)*U1-(L3-L4-ME*(L3-L4)/(MA+MB+MC+MD+ME+MF))*COS
     &(DA-EA-Q3)*U2-(L6-(L5*MC+L6*MD+L6*ME+L6*MF)/(MA+MB+MC+MD+ME+MF))*C
     &OS(Q3-Q6)*U2-(L6-(L6*ME-MD*(L5-L6))/(MA+MB+MC+MD+ME+MF))*SIN(DA-Q3
     &)*U1-(L6-(L6*ME-MD*(L5-L6))/(MA+MB+MC+MD+ME+MF))*COS(DA-Q3)*U2-(L4
     &-(L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)/(MA+MB+MC+MD+ME+MF))*COS(Q3-Q5-Q6
     &)*U2-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)/(MA+MB+MC+MD+ME+MF)
     &)*COS(Q3-Q4-Q5-Q6)*U2)
      PX = MF*FSp*COS(Q3) + (MA+MB+MC+MD+ME+MF)*U1 - MF*FS*SIN(Q3)*U3 - 
     &(L5*MC+L6*MD+L6*ME+L6*MF)*SIN(Q3-Q6)*(U3-U6) - (L6*ME-MD*(L5-L6))*
     &SIN(DA-Q3)*(DAp-U3-U7) - ME*(L3-L4)*SIN(DA-EA-Q3)*(DAp-EAp-U3-U7-U
     &8) - (L3*MB+L4*MC+L4*MD+L4*ME+L4*MF)*SIN(Q3-Q5-Q6)*(U3-U5-U6) - (L
     &1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L2*MF)*SIN(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6)
      PY = MF*FSp*SIN(Q3) + MF*FS*COS(Q3)*U3 + (MA+MB+MC+MD+ME+MF)*U2 + 
     &(L5*MC+L6*MD+L6*ME+L6*MF)*COS(Q3-Q6)*(U3-U6) + (L3*MB+L4*MC+L4*MD+
     &L4*ME+L4*MF)*COS(Q3-Q5-Q6)*(U3-U5-U6) + (L1*MA+L2*MB+L2*MC+L2*MD+L
     &2*ME+L2*MF)*COS(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6) - (L6*ME-MD*(L5-L6))*CO
     &S(DA-Q3)*(DAp-U3-U7) - ME*(L3-L4)*COS(DA-EA-Q3)*(DAp-EAp-U3-U7-U8)
      HANG = 3.141592653589793D0 + Q6
      KANG = 3.141592653589793D0 - Q5
      AANG = 3.141592653589793D0 + Q4
      HANGVEL = U6
      KANGVEL = -U5
      AANGVEL = U4
      SHANG = DA
      SKANG = EA
      SHANGVEL = DAp
      SKANGVEL = EAp
      SHTOR = ID*DApp + IE*(DApp-EApp) + G*ME*(L6*COS(DA-Q3)+(L3-L4)*COS
     &(DA-EA-Q3)) + MD*(L5-L6)*((L5-L6)*DApp-L6*SIN(DA-Q6)*(U3-U6)**2-L4
     &*SIN(DA-Q5-Q6)*(U3-U5-U6)**2-L2*SIN(DA-Q4-Q5-Q6)*(U3-U4-U5-U6)**2)
     & + (MD*(L5-L6)*SIN(DA-Q3)-ME*(L6*SIN(DA-Q3)+(L3-L4)*SIN(DA-EA-Q3))
     &)*U1p + (MD*(L5-L6)*COS(DA-Q3)-ME*(L6*COS(DA-Q3)+(L3-L4)*COS(DA-EA
     &-Q3)))*U2p - G*MD*(L5-L6)*COS(DA-Q3) - ME*(L6*(L3-L4)*SIN(EA)*(DAp
     &-U3)**2-L6**2*DApp-L6*(L3-L4)*COS(EA)*DApp-(L3-L4)**2*(DApp-EApp)-
     &L6*(L3-L4)*COS(EA)*(DApp-EApp)-L6**2*SIN(DA-Q6)*(U3-U6)**2-L6*(L3-
     &L4)*SIN(EA)*(DAp-EAp-U3)**2-L4*L6*SIN(DA-Q5-Q6)*(U3-U5-U6)**2-L6*(
     &L3-L4)*SIN(DA-EA-Q6)*(U3-U6)**2-L2*L6*SIN(DA-Q4-Q5-Q6)*(U3-U4-U5-U
     &6)**2-L4*(L3-L4)*SIN(DA-EA-Q5-Q6)*(U3-U5-U6)**2-L2*(L3-L4)*SIN(DA-
     &EA-Q4-Q5-Q6)*(U3-U4-U5-U6)**2) - L2*(MD*(L5-L6)*COS(DA-Q4-Q5-Q6)-M
     &E*(L6*COS(DA-Q4-Q5-Q6)+(L3-L4)*COS(DA-EA-Q4-Q5-Q6)))*U4p - (MD*(L5
     &-L6)*(L4*COS(DA-Q5-Q6)+L2*COS(DA-Q4-Q5-Q6))-ME*(L4*L6*COS(DA-Q5-Q6
     &)+L2*L6*COS(DA-Q4-Q5-Q6)+L4*(L3-L4)*COS(DA-EA-Q5-Q6)+L2*(L3-L4)*CO
     &S(DA-EA-Q4-Q5-Q6)))*U5p - (MD*(L5-L6)*(L6*COS(DA-Q6)+L4*COS(DA-Q5-
     &Q6)+L2*COS(DA-Q4-Q5-Q6))-ME*(L6**2*COS(DA-Q6)+L4*L6*COS(DA-Q5-Q6)+
     &L2*L6*COS(DA-Q4-Q5-Q6)+L6*(L3-L4)*COS(DA-EA-Q6)+L4*(L3-L4)*COS(DA-
     &EA-Q5-Q6)+L2*(L3-L4)*COS(DA-EA-Q4-Q5-Q6)))*U6p - (ID+IE+MD*(L5-L6)
     &*(L5-L6-L6*COS(DA-Q6)-L4*COS(DA-Q5-Q6)-L2*COS(DA-Q4-Q5-Q6))+ME*(L6
     &**2+(L3-L4)**2+2*L6*(L3-L4)*COS(EA)+L6**2*COS(DA-Q6)+L4*L6*COS(DA-
     &Q5-Q6)+L2*L6*COS(DA-Q4-Q5-Q6)+L6*(L3-L4)*COS(DA-EA-Q6)+L4*(L3-L4)*
     &COS(DA-EA-Q5-Q6)+L2*(L3-L4)*COS(DA-EA-Q4-Q5-Q6)))*U3p
      SKTOR = ME*(L3-L4)*(L6*SIN(EA)*(DAp-U3)**2-L6*COS(EA)*DApp-(L3-L4)
     &*(DApp-EApp)-L6*SIN(DA-EA-Q6)*(U3-U6)**2-L4*SIN(DA-EA-Q5-Q6)*(U3-U
     &5-U6)**2-L2*SIN(DA-EA-Q4-Q5-Q6)*(U3-U4-U5-U6)**2) + ME*(L3-L4)*SIN
     &(DA-EA-Q3)*U1p + ME*(L3-L4)*COS(DA-EA-Q3)*U2p + (IE-ME*(L3-L4)*(L4
     &-L3-L6*COS(EA)-L6*COS(DA-EA-Q6)-L4*COS(DA-EA-Q5-Q6)-L2*COS(DA-EA-Q
     &4-Q5-Q6)))*U3p - IE*(DApp-EApp) - G*ME*(L3-L4)*COS(DA-EA-Q3) - L2*
     &ME*(L3-L4)*COS(DA-EA-Q4-Q5-Q6)*U4p - ME*(L3-L4)*(L4*COS(DA-EA-Q5-Q
     &6)+L2*COS(DA-EA-Q4-Q5-Q6))*U5p - ME*(L3-L4)*(L6*COS(DA-EA-Q6)+L4*C
     &OS(DA-EA-Q5-Q6)+L2*COS(DA-EA-Q4-Q5-Q6))*U6p
      POP1X = Q1
      POP1Y = Q2
      POP2X = Q1 + L2*COS(Q3-Q4-Q5-Q6)
      POP2Y = Q2 + L2*SIN(Q3-Q4-Q5-Q6)
      POP3X = Q1 + L4*COS(Q3-Q5-Q6) + L2*COS(Q3-Q4-Q5-Q6)
      POP3Y = Q2 + L4*SIN(Q3-Q5-Q6) + L2*SIN(Q3-Q4-Q5-Q6)
      POP4X = Q1 + L6*COS(Q3-Q6) + L4*COS(Q3-Q5-Q6) + L2*COS(Q3-Q4-Q5-Q6
     &)
      POP4Y = Q2 + L6*SIN(Q3-Q6) + L4*SIN(Q3-Q5-Q6) + L2*SIN(Q3-Q4-Q5-Q6
     &)
      POP5X = Q1 + L6*COS(DA-Q3) + L6*COS(Q3-Q6) + L4*COS(Q3-Q5-Q6) + L2
     &*COS(Q3-Q4-Q5-Q6)
      POP5Y = Q2 + L6*SIN(Q3-Q6) + L4*SIN(Q3-Q5-Q6) + L2*SIN(Q3-Q4-Q5-Q6
     &) - L6*SIN(DA-Q3)
      POP6X = Q1 + L6*COS(DA-Q3) + L6*COS(Q3-Q6) + L4*COS(Q3-Q5-Q6) + L2
     &*COS(Q3-Q4-Q5-Q6) - L4*COS(DA-EA-Q3)
      POP6Y = Q2 + L6*SIN(Q3-Q6) + L4*SIN(DA-EA-Q3) + L4*SIN(Q3-Q5-Q6) +
     & L2*SIN(Q3-Q4-Q5-Q6) - L6*SIN(DA-Q3)
      POP7X = Q1 + L7*COS(Q3) + L6*COS(Q3-Q6) + L4*COS(Q3-Q5-Q6) + L2*CO
     &S(Q3-Q4-Q5-Q6)
      POP7Y = Q2 + L7*SIN(Q3) + L6*SIN(Q3-Q6) + L4*SIN(Q3-Q5-Q6) + L2*SI
     &N(Q3-Q4-Q5-Q6)
      POFOX = Q1 + FS*COS(Q3) + L6*COS(Q3-Q6) + L4*COS(Q3-Q5-Q6) + L2*CO
     &S(Q3-Q4-Q5-Q6)
      POFOY = Q2 + FS*SIN(Q3) + L6*SIN(Q3-Q6) + L4*SIN(Q3-Q5-Q6) + L2*SI
     &N(Q3-Q4-Q5-Q6)
      POCMX = Q1 + (MF*FS*COS(Q3)+ME*(L3-L4)*COS(DA-EA-Q3)+(L5*MC+L6*MD+
     &L6*ME+L6*MF)*COS(Q3-Q6)+(L6*ME-MD*(L5-L6))*COS(DA-Q3)+(L3*MB+L4*MC
     &+L4*MD+L4*ME+L4*MF)*COS(Q3-Q5-Q6)+(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+L
     &2*MF)*COS(Q3-Q4-Q5-Q6))/(MA+MB+MC+MD+ME+MF)
      POCMSTANCEX = Q1 + (L5*MC*COS(Q3-Q6)+(L3*MB+L4*MC)*COS(Q3-Q5-Q6)+(
     &L1*MA+L2*MB+L2*MC)*COS(Q3-Q4-Q5-Q6))/(MA+MB+MC)
      POCMSTANCEY = Q2 + (L5*MC*SIN(Q3-Q6)+(L3*MB+L4*MC)*SIN(Q3-Q5-Q6)+(
     &L1*MA+L2*MB+L2*MC)*SIN(Q3-Q4-Q5-Q6))/(MA+MB+MC)
      POCMSWINGX = Q1 + L6*COS(Q3-Q6) + L4*COS(Q3-Q5-Q6) + L2*COS(Q3-Q4-
     &Q5-Q6) + (ME*(L3-L4)*COS(DA-EA-Q3)+(L6*ME-MD*(L5-L6))*COS(DA-Q3))/
     &(MD+ME)
      POCMSWINGY = Q2 + L6*SIN(Q3-Q6) + L4*SIN(Q3-Q5-Q6) + L2*SIN(Q3-Q4-
     &Q5-Q6) - (ME*(L3-L4)*SIN(DA-EA-Q3)+(L6*ME-MD*(L5-L6))*SIN(DA-Q3))/
     &(MD+ME)
      VOCMX = U1 + (MF*FSp*COS(Q3)-MF*FS*SIN(Q3)*U3-(L5*MC+L6*MD+L6*ME+L
     &6*MF)*SIN(Q3-Q6)*(U3-U6)-(L6*ME-MD*(L5-L6))*SIN(DA-Q3)*(DAp-U3-U7)
     &-ME*(L3-L4)*SIN(DA-EA-Q3)*(DAp-EAp-U3-U7-U8)-(L3*MB+L4*MC+L4*MD+L4
     &*ME+L4*MF)*SIN(Q3-Q5-Q6)*(U3-U5-U6)-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME
     &+L2*MF)*SIN(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6))/(MA+MB+MC+MD+ME+MF)
      VOCMY = U2 - ((L6*ME-MD*(L5-L6))*COS(DA-Q3)*(DAp-U3-U7)+ME*(L3-L4)
     &*COS(DA-EA-Q3)*(DAp-EAp-U3-U7-U8)-MF*FSp*SIN(Q3)-MF*FS*COS(Q3)*U3-
     &(L5*MC+L6*MD+L6*ME+L6*MF)*COS(Q3-Q6)*(U3-U6)-(L3*MB+L4*MC+L4*MD+L4
     &*ME+L4*MF)*COS(Q3-Q5-Q6)*(U3-U5-U6)-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME
     &+L2*MF)*COS(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6))/(MA+MB+MC+MD+ME+MF)


C** Update activations for write
      CALL ACTIVATION(T,HEACTP,HEACT,2)
      CALL ACTIVATION(T,HFACTP,HFACT,2)
      CALL ACTIVATION(T,KEACTP,KEACT,2)
      CALL ACTIVATION(T,KFACTP,KFACT,2)
      CALL ACTIVATION(T,AEACTP,AEACT,2)
      CALL ACTIVATION(T,AFACTP,AFACT,2)

C**   Write output to screen and to output file(s)
!       WRITE(*, 6020) T,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,P
!      &OP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POCMX,POCMY,VOCMX,VOCMY
      WRITE(21,6020) T,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,P
     &OP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POFOX,POFOY,POCMSTANCEX,POCMSTA
     &NCEY,POCMSWINGX,POCMSWINGY,POCMX,POCMY,VOCMX,VOCMY
      WRITE(22,6020) T,Q1,Q2,(Q3*RADtoDEG),(Q4*RADtoDEG),(Q5*RADtoDEG),(
     &Q6*RADtoDEG),U1,U2,(U3*RADtoDEG),(U4*RADtoDEG),(U5*RADtoDEG),(U6*D
     &EGtoRAD)
      WRITE(23,6020) T,RX,RY,HTOR,KTOR,ATOR,SHTOR,SKTOR
      WRITE(24,6020) T,(Q3*RADtoDEG),(HANG*RADtoDEG),(KANG*RADtoDEG),(AA
     &NG*RADtoDEG),(SHANG*RADtoDEG),(SKANG*RADtoDEG),(U3*RADtoDEG),(HANG
     &VEL*RADtoDEG),(KANGVEL*RADtoDEG),(AANGVEL*RADtoDEG),(SHANGVEL*RADt
     &oDEG),(SKANGVEL*RADtoDEG)
      WRITE(25,6020) T,KECM,PECM,TE,HZ,PX,PY
      WRITE(26,6020) T,HEACT,HFACT,KEACT,KFACT,AEACT,AFACT
      WRITE(27,6020) T,HETOR,HEACT,(HECCANG*RADtoDEG),(HECCANGVEL*RADtoD
     &EG),(HESECANG*RADtoDEG),(HESECANGVEL*RADtoDEG),HFTOR,HFACT,(HFCCAN
     &G*RADtoDEG),(HFCCANGVEL*RADtoDEG),(HFSECANG*RADtoDEG),(HFSECANGVEL
     &*RADtoDEG)
      WRITE(28,6020) T,KETOR,KEACT,(KECCANG*RADtoDEG),(KECCANGVEL*RADtoD
     &EG),(KESECANG*RADtoDEG),(KESECANGVEL*RADtoDEG),KFTOR,KFACT,(KFCCAN
     &G*RADtoDEG),(KFCCANGVEL*RADtoDEG),(KFSECANG*RADtoDEG),(KFSECANGVEL
     &*RADtoDEG)
      WRITE(29,6020) T,AETOR,AEACT,(AECCANG*RADtoDEG),(AECCANGVEL*RADtoD
     &EG),(AESECANG*RADtoDEG),(AESECANGVEL*RADtoDEG),AFTOR,AFACT,(AFCCAN
     &G*RADtoDEG),(AFCCANGVEL*RADtoDEG),(AFSECANG*RADtoDEG),(AFSECANGVEL
     &*RADtoDEG)

6020  FORMAT( 99(1X, 1PE14.6E3) )

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
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,Q6,U1,U2,U3,U4,U5,U6
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(6,6),RHS(6)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW

C** Local variables
      AANG    = Q4
      KANG    = Q5
      HANG    = Q6 
      KANGVEL = U5
      VOCMX   = U1
      VOCMY   = U2
      U7      = 0.0D0
      U8      = 0.0D0

C** Convert joint angles to generalised coordinates/speeds
      Q4 = AANG - 180.0D0
      Q5 = 180.0D0 - KANG
      Q6 = HANG - 180.0D0
      U5 = -KANGVEL

C** Convert CoM velocity to generalised speeds
      Q3 = Q3*DEGtoRAD
      Q4 = Q4*DEGtoRAD
      Q5 = Q5*DEGtoRAD
      Q6 = Q6*DEGtoRAD
      U3 = U3*DEGtoRAD
      U4 = U4*DEGtoRAD
      U5 = U5*DEGtoRAD
      U6 = U6*DEGtoRAD
      
      DApp = 100
      DAp  = 100*T
      DA   = 50*T**2 + PI
      EApp = 0.0
      EAp  = 0.0
      EA   = pi
      CALL EVALSPLINE2(T,NROW,TT,CCHAT,FS,FSp,FSpp)

      U1 = VOCMX - (MF*FSp*COS(Q3)-MF*FS*SIN(Q3)*U3-(L5*MC+L6*MD+L6*ME+L
     &6*MF)*SIN(Q3-Q6)*(U3-U6)-(L6*ME-MD*(L5-L6))*SIN(DA-Q3)*(DAp-U3-U7)
     &-ME*(L3-L4)*SIN(DA-EA-Q3)*(DAp-EAp-U3-U7-U8)-(L3*MB+L4*MC+L4*MD+L4
     &*ME+L4*MF)*SIN(Q3-Q5-Q6)*(U3-U5-U6)-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME
     &+L2*MF)*SIN(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6))/(MA+MB+MC+MD+ME+MF)
      U2 = VOCMY + ((L6*ME-MD*(L5-L6))*COS(DA-Q3)*(DAp-U3-U7)+ME*(L3-L4)
     &*COS(DA-EA-Q3)*(DAp-EAp-U3-U7-U8)-MF*FSp*SIN(Q3)-MF*FS*COS(Q3)*U3-
     &(L5*MC+L6*MD+L6*ME+L6*MF)*COS(Q3-Q6)*(U3-U6)-(L3*MB+L4*MC+L4*MD+L4
     &*ME+L4*MF)*COS(Q3-Q5-Q6)*(U3-U5-U6)-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME
     &+L2*MF)*COS(Q3-Q4-Q5-Q6)*(U3-U4-U5-U6))/(MA+MB+MC+MD+ME+MF)

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
     &4X,POP4Y,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,VOCMX,VOCMY
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

C** Set torques manually
      HETOR = 100.0
      HFTOR = 0.0
      KETOR = 0.0
      KFTOR = 0.0
      AETOR = 0.0
      AFTOR = 0.0

!       CALL MUSCLEMODEL(T,HETQP(1:9),HEACTP,HETQP(10),2*PI-HANG,-HANGVEL,
!      &HETOR,HESECANG,HESECANGVEL,HECCANG,HECCANGVEL,INTEGSTP,2)
!       CALL MUSCLEMODEL(T,KETQP(1:9),KEACTP,KETQP(10),2*PI-KANG,-KANGVEL,
!      &KETOR,KESECANG,KESECANGVEL,KECCANG,KECCANGVEL,INTEGSTP,2)
!       CALL MUSCLEMODEL(T,AETQP(1:9),AEACTP,AETQP(10),2*PI-AANG,-AANGVEL,
!      &AETOR,AESECANG,AESECANGVEL,AECCANG,AECCANGVEL,INTEGSTP,2)
!       CALL MUSCLEMODEL(T,HFTQP(1:9),HFACTP,HFTQP(10),HANG,HANGVEL,HFTOR,
!      &HFSECANG,HFSECANGVEL,HFCCANG,HFCCANGVEL,INTEGSTP,2)
!       CALL MUSCLEMODEL(T,KFTQP(1:9),KFACTP,KFTQP(10),KANG,KANGVEL,KFTOR,
!      &KFSECANG,KFSECANGVEL,KFCCANG,KFCCANGVEL,INTEGSTP,2)
!       CALL MUSCLEMODEL(T,AFTQP(1:9),AFACTP,AFTQP(10),AANG,AANGVEL,AFTOR,
!      &AFSECANG,AFSECANGVEL,AFCCANG,AFCCANGVEL,INTEGSTP,2)

      HTOR = HETOR - HFTOR
      KTOR = KETOR - KFTOR
      ATOR = AETOR - AFTOR 

      END SUBROUTINE UPDATE