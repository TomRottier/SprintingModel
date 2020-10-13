C Three segment model used to examine the effects of the swinging limb
C on sprint performance during the stance phase.
C
C Segments include thigh, shank and one part foot. Two point masses
C representing the CoM of the swinging limb and and the HAT. Quintic 
C splines are used to specify the position, velocity and acceleration of
C these points masses throughout a simulation.
C
C Torque generators in series with rotational spring for flexion and 
C extension at each joint.
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
      DIMENSION        VAR(10)
      DIMENSION        TT(500),CCHATX(6,500),CCHATY(6,500),CCSWINGX(6,50
     &0),CCSWINGY(6,500)
      DIMENSION        HETQP(10),HFTQP(10),KETQP(10),KFTQP(10),AETQP(10)
     &,AFTQP(10),HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(NACTP)
     &,AEACTP(NACTP),AFACTP(NACTP)
      COMMON/CONSTNTS/ G,IA,IB,IC,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6,MA,MB,MC
     &,MD,ME
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,U1,U2,U3,U4,U5
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,HANG,HANGVEL,HETOR,
     &HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,PECM,PX,PY,RX,RY,
     &TE,Q1p,Q2p,Q3p,Q4p,Q5p,U1p,U2p,U3p,U4p,U5p,AEACT,AECCANG,AECCANGVE
     &L,AESECANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGV
     &EL,DX,DY,EX,EY,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT
     &,HFCCANG,HFCCANGVEL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,
     &KESECANG,KESECANGVEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL
     &,DXp,DYp,EXp,EYp,DXpp,DYpp,EXpp,EYpp,POCMX,POCMY,PODX,PODY,POEX,PO
     &EY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,VOCMX,VOCMY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(5,5),RHS(5)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP
      COMMON/SPLNCOEF/ TT,CCHATX,CCHATY,CCSWINGX,CCSWINGY,NROW

C**   Open input and output files
      OPEN(UNIT=20, FILE='sprintmodel.in', STATUS='OLD')
      OPEN(UNIT=21, FILE='sprintmodel.1',  STATUS='UNKNOWN')
      OPEN(UNIT=22, FILE='sprintmodel.2',  STATUS='UNKNOWN')
      OPEN(UNIT=23, FILE='sprintmodel.3',  STATUS='UNKNOWN')
      OPEN(UNIT=24, FILE='sprintmodel.4',  STATUS='UNKNOWN')
      OPEN(UNIT=25, FILE='sprintmodel.5',  STATUS='UNKNOWN')
      OPEN(UNIT=26, FILE='sprintmodel.6',  STATUS='UNKNOWN')
      OPEN(UNIT=27, FILE='sprintmodel.7',  STATUS='UNKNOWN')
      OPEN(UNIT=28, FILE='sprintmodel.8',  STATUS='UNKNOWN')
      OPEN(UNIT=29, FILE='sprintmodel.9',  STATUS='UNKNOWN')

C**   Read message from input file
      READ(20,7000,END=7100,ERR=7101) (MESSAGE(ILOOP),ILOOP = 1,99)

C**   Read values of constants from input file
      READ(20,7010,END=7100,ERR=7101) G,IA,IB,IC,K1,K2,K3,K4,L1,L2,L3,L4
     &,L5,L6,MA,MB,MC,MD,ME

C**   Read the initial value of each variable from input file
      READ(20,7010,END=7100,ERR=7101) Q1,Q2,Q3,Q4,Q5,U1,U2,U3,U4,U5

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

C** Read values for swing leg and HAT CoM (first two rows headers)
      ! OPEN(UNIT=32, FILE='HAT.txt', STATUS='OLD')
      ! OPEN(UNIT=33, FILE='swing.txt', STATUS='OLD')
      ! READ(32, '(I4)', ERR=7410) NROW
      ! READ(33, *)
      ! READ(32, *)
      ! READ(33, *)
      ! READ(32, '(6E14.5)', ERR=7410) (HAT(I,:), I=1, NROW)
      ! READ(33, '(6E14.5)', ERR=7510) (SWING(I,:), I=1, NROW)

C** Read spline coefficients
      OPEN(UNIT=34, FILE='HAT_coef.csv', STATUS='OLD')
      READ(34,*) NROW
      READ(34,*) (TT(I), I=1, NROW)
      READ(34,*) ((CCHATX(J,I), J=1, 6), I=1, NROW)
      READ(34,*) ((CCHATY(J,I), J=1, 6), I=1, NROW)
      CLOSE(UNIT=34)

      OPEN(UNIT=35, FILE='swing_coef.csv', STATUS='OLD')
      READ(35,*) NROW
      READ(35,*) (TT(I), I=1, NROW)
      READ(35,*) ((CCSWINGX(J,I), J=1, 6), I=1, NROW)
      READ(35,*) ((CCSWINGY(J,I), J=1, 6), I=1, NROW)
      CLOSE(UNIT=35)

C**   Degree to radian conversion
      PI       = 4*ATAN(1.0D0)
      DEGtoRAD = PI/180.0D0
      RADtoDEG = 180.0D0/PI
      Q3 = Q3*DEGtoRAD
      Q4 = Q4*DEGtoRAD
      Q5 = Q5*DEGtoRAD
      U3 = U3*DEGtoRAD
      U4 = U4*DEGtoRAD
      U5 = U5*DEGtoRAD

C** Initial velocities of masses (swing leg D, HAT E)
      CALL EVALSPLINE(TINITIAL,NROW,TT,CCSWINGX,CCSWINGY,DX,DXp,DXpp,DY,
     &DYp,DYpp)
      CALL EVALSPLINE(TINITIAL,NROW,TT,CCHATX,CCHATY,EX,EXp,EXpp,EY,EYp,
     &EYpp)

C** Convert CoM velocity to U1,U2
      U1 = ((MA+MB+MC+MD+ME)*U1+(L5*MC+L6*MD+L6*ME)*SIN(Q5)*U5+(L3*MB+L4
     &*MC+L4*MD+L4*ME)*SIN(Q4)*U4+(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)*SIN(Q3
     &)*U3-MD*DXp-ME*EXp)/(MA+MB+MC+MD+ME)
      U2 = ((MA+MB+MC+MD+ME)*U2-(L5*MC+L6*MD+L6*ME)*COS(Q5)*U5-(L3*MB+L4
     &*MC+L4*MD+L4*ME)*COS(Q4)*U4-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)*COS(Q3
     &)*U3-MD*DYp-ME*EYp)/(MA+MB+MC+MD+ME)

C**   Initialize time, print counter, variables array for integrator
      T      = TINITIAL
      IPRINT = 0
      VAR(1) = Q1
      VAR(2) = Q2
      VAR(3) = Q3
      VAR(4) = Q4
      VAR(5) = Q5
      VAR(6) = U1
      VAR(7) = U2
      VAR(8) = U3
      VAR(9) = U4
      VAR(10) = U5

C** Initialise values for integration
      CALL UPDATE(T)

C**   Initalize numerical integrator with call to EQNS1 at T=TINITIAL
      CALL KUTTA(EQNS1, 10, VAR, T, INTEGSTP, ABSERR, RELERR, 0, *5920)

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
      CALL KUTTA(EQNS1, 10, VAR, T, INTEGSTP, ABSERR, RELERR, 1, *5920)


C** Update values after integration
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

6021  FORMAT(1X,'FILE: sprintmodel.1 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &POP1X',10X,'POP1Y',10X,'POP2X',10X,'POP2Y',10X,'POP3X',10X,'POP3Y'
     &,10X,'POP4X',10X,'POP4Y',10X,'PODX',11X,'PODY',11X,'POEX',11X,'POE
     &Y',11X,'POCMX',10X,'POCMY',10X,'VOCMX',10X,'VOCMY',/,7X,'(S)',12X,
     &'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',
     &12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(
     &M)',11X,'(M/S)',10X,'(M/S)',/)
6022  FORMAT(1X,'FILE: sprintmodel.2 ',//1X,'*** ',99A1,///,8X,'T',13X,'
     &Q1',13X,'Q2',13X,'Q3',13X,'Q4',13X,'Q5',13X,'U1',13X,'U2',13X,'U3'
     &,13X,'U4',13X,'U5',/,7X,'(S)',12X,'(M)',12X,'(M)',11X,'(DEG)',10X,
     &'(DEG)',10X,'(DEG)',10X,'(M/S)',10X,'(M/S)',9X,'(DEG/S)',8X,'(DEG/
     &S)',8X,'(DEG/S)',/)
6023  FORMAT(1X,'FILE: sprintmodel.3 ',//1X,'*** ',99A1,///,8X,'T',13X,'
     &RX',13X,'RY',12X,'HTOR',11X,'KTOR',11X,'ATOR',/,7X,'(S)',12X,'(N)'
     &,12X,'(N)',11X,'(N/M)',10X,'(N/M)',10X,'(N/M)',/)
6024  FORMAT(1X,'FILE: sprintmodel.4 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &HANG',11X,'KANG',11X,'AANG',10X,'HANGVEL',8X,'KANGVEL',8X,'AANGVEL
     &',/,7X,'(S)',11X,'(DEG)',10X,'(DEG)',10X,'(DEG)',9X,'(DEG/S)',8X,'
     &(DEG/S)',8X,'(DEG/S)',/)
6025  FORMAT(1X,'FILE: sprintmodel.5 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &KECM',11X,'PECM',12X,'TE',13X,'HZ',13X,'PX',13X,'PY',/,7X,'(S)',12
     &X,'(J)',12X,'(J)',12X,'(J)',8X,'(KG.M^2/S)',6X,'(KG.M/S)',7X,'(KG.
     &M/S)',/)
6026  FORMAT(1X,'FILE: sprintmodel.6 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &HEACT',10X,'HFACT',10X,'KEACT',10X,'KFACT',10X,'AEACT',10X,'AFACT'
     &,/,7X,'(S)',10X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8
     &X,'(UNITS)',8X,'(UNITS)',/)
6027  FORMAT(1X,'FILE: sprintmodel.7 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &HETOR',10X,'HEACT',9X,'HECCANG',6X,'HECCANGVEL',6X,'HESECANG',6X,'
     &HESECANGVEL',7X,'HFTOR',10X,'HFACT',9X,'HFCCANG',6X,'HFCCANGVEL',6
     &X,'HFSECANG',6X,'HFSECANGVEL',/,7X,'(S)',11X,'(N/M)',9X,'(UNITS)',
     &9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',9X,'(N/M)',9X,'(UN
     &ITS)',9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',/)
6028  FORMAT(1X,'FILE: sprintmodel.8 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &KETOR',10X,'KEACT',9X,'KECCANG',6X,'KECCANGVEL',6X,'KESECANG',6X,'
     &KESECANGVEL',7X,'KFTOR',10X,'KFACT',9X,'KFCCANG',6X,'KFCCANGVEL',6
     &X,'KFSECANG',6X,'KFSECANGVEL',/,7X,'(S)',11X,'(N/M)',9X,'(UNITS)',
     &9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',9X,'(N/M)',9X,'(UN
     &ITS)',9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',/)
6029  FORMAT(1X,'FILE: sprintmodel.9 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &AETOR',10X,'AEACT',9X,'AECCANG',6X,'AECCANGVEL',6X,'AESECANG',6X,'
     &AESECANGVEL',7X,'AFTOR',10X,'AFACT',9X,'AFCCANG',6X,'AFCCANGVEL',6
     &X,'AFSECANG',6X,'AFSECANGVEL',/,7X,'(S)',11X,'(N/M)',9X,'(UNITS)',
     &9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',9X,'(N/M)',9X,'(UN
     &ITS)',9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',/)
6997  FORMAT(/7X,'Error: Numerical integration failed to converge',/)
6999  FORMAT(//1X,'Input is in the file sprintmodel.in',//1X,'Output is 
     &in the file(s) sprintmodel.i  (i=1, ..., 9)',//1X,'The output quan
     &tities and associated files are listed in file sprintmodel.dir',/)
7000  FORMAT(//,99A1,///)
7010  FORMAT( 1000(59X,E30.0,/) )
7011  FORMAT( 3(59X,E30.0,/), 1(59X,I30,/), 2(59X,E30.0,/) )
      STOP
7100  WRITE(*,*) 'Premature end of file while reading sprintmodel.in '
      STOP
7101  WRITE(*,*) 'Error while reading file sprintmodel.in'
      STOP
7200  FORMAT(//, 6(///, 10(8X, F7.2, /)))
7210  WRITE(*,*) 'Error reading torque parameters'
      STOP
7300  FORMAT(//, 6(///, 7(6X, F11.6, /)))
7310  WRITE(*,*) 'Error reading activation parameters'
      STOP
7410  WRITE(*,*) 'Error while reading in HAT.txt'
      STOP
7510  WRITE(*,*) 'Error while reading in swing.txt'
      STOP
      END PROGRAM MAIN


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
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,U1,U2,U3,U4,U5
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,HANG,HANGVEL,HETOR,
     &HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,PECM,PX,PY,RX,RY,
     &TE,Q1p,Q2p,Q3p,Q4p,Q5p,U1p,U2p,U3p,U4p,U5p,AEACT,AECCANG,AECCANGVE
     &L,AESECANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGV
     &EL,DX,DY,EX,EY,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT
     &,HFCCANG,HFCCANGVEL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,
     &KESECANG,KESECANGVEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL
     &,DXp,DYp,EXp,EYp,DXpp,DYpp,EXpp,EYpp,POCMX,POCMY,PODX,PODY,POEX,PO
     &EY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,VOCMX,VOCMY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(5,5),RHS(5)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP

      HANG = 4.71238898038469D0 - Q5
      KANG = 3.141592653589793D0 + Q4 - Q5
      AANG = 3.141592653589793D0 + Q4 - Q3
      HANGVEL = -U5
      KANGVEL = U4 - U5
      AANGVEL = U4 - U3

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

      HTOR = HFTOR - HETOR
      KTOR = KETOR - KFTOR
      ATOR = AFTOR - AETOR 

      END SUBROUTINE UPDATE

C**********************************************************************
      SUBROUTINE       EQNS1(T, VAR, VARp, BOUNDARY)
      IMPLICIT         DOUBLE PRECISION (A - Z)
      INTEGER          BOUNDARY,NROW
      DIMENSION        VAR(*), VARp(*)
      DIMENSION        TT(500),CCHATX(6,500),CCHATY(6,500),CCSWINGX(6,50
     &0),CCSWINGY(6,500)
      COMMON/CONSTNTS/ G,IA,IB,IC,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6,MA,MB,MC
     &,MD,ME
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,U1,U2,U3,U4,U5
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,HANG,HANGVEL,HETOR,
     &HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,PECM,PX,PY,RX,RY,
     &TE,Q1p,Q2p,Q3p,Q4p,Q5p,U1p,U2p,U3p,U4p,U5p,AEACT,AECCANG,AECCANGVE
     &L,AESECANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGV
     &EL,DX,DY,EX,EY,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT
     &,HFCCANG,HFCCANGVEL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,
     &KESECANG,KESECANGVEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL
     &,DXp,DYp,EXp,EYp,DXpp,DYpp,EXpp,EYpp,POCMX,POCMY,PODX,PODY,POEX,PO
     &EY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,VOCMX,VOCMY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(5,5),RHS(5)
      COMMON/SPLNCOEF/ TT,CCHATX,CCHATY,CCSWINGX,CCSWINGY,NROW

C**   Update variables after integration step
      Q1 = VAR(1)
      Q2 = VAR(2)
      Q3 = VAR(3)
      Q4 = VAR(4)
      Q5 = VAR(5)
      U1 = VAR(6)
      U2 = VAR(7)
      U3 = VAR(8)
      U4 = VAR(9)
      U5 = VAR(10)

      Q1p = U1
      Q2p = U2
      Q3p = U3
      Q4p = U4
      Q5p = U5

C** Mass positions
      CALL EVALSPLINE(T,NROW,TT,CCSWINGX,CCSWINGY,DX,DXp,DXpp,DY,DYp,DYp
     &p)
      CALL EVALSPLINE(T,NROW,TT,CCHATX,CCHATY,EX,EXp,EXpp,EY,EYp,EYpp)

C** Calculate forces
        IF (Q2 .LT. 0.0D0) THEN
          RY = -K3*Q2 - K4*U2*ABS(Q2)
          RX = (-K1*Q1 - K2*U1)*RY
        ELSE
          RX = 0.0D0
          RY = 0.0D0
        ENDIF     

C** Set up inertia matrix (COEF) and force vector (RHS)
      COEF(1,1) = -MA - MB - MC - MD - ME
      COEF(1,2) = 0
      COEF(1,3) = (L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)*SIN(Q3)
      COEF(1,4) = (L3*MB+L4*MC+L4*MD+L4*ME)*SIN(Q4)
      COEF(1,5) = (L5*MC+L6*MD+L6*ME)*SIN(Q5)
      COEF(2,1) = 0
      COEF(2,2) = -MA - MB - MC - MD - ME
      COEF(2,3) = -(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)*COS(Q3)
      COEF(2,4) = -(L3*MB+L4*MC+L4*MD+L4*ME)*COS(Q4)
      COEF(2,5) = -(L5*MC+L6*MD+L6*ME)*COS(Q5)
      COEF(3,1) = (L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)*SIN(Q3)
      COEF(3,2) = -(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)*COS(Q3)
      COEF(3,3) = -IA - MA*L1**2 - MB*L2**2 - MC*L2**2 - MD*L2**2 - ME*L
     &2**2
      COEF(3,4) = -L2*(L3*MB+L4*MC+L4*MD+L4*ME)*COS(Q3-Q4)
      COEF(3,5) = -L2*(L5*MC+L6*MD+L6*ME)*COS(Q3-Q5)
      COEF(4,1) = (L3*MB+L4*MC+L4*MD+L4*ME)*SIN(Q4)
      COEF(4,2) = -(L3*MB+L4*MC+L4*MD+L4*ME)*COS(Q4)
      COEF(4,3) = -L2*(L3*MB+L4*MC+L4*MD+L4*ME)*COS(Q3-Q4)
      COEF(4,4) = -IB - MB*L3**2 - MC*L4**2 - MD*L4**2 - ME*L4**2
      COEF(4,5) = -L4*(L5*MC+L6*MD+L6*ME)*COS(Q4-Q5)
      COEF(5,1) = (L5*MC+L6*MD+L6*ME)*SIN(Q5)
      COEF(5,2) = -(L5*MC+L6*MD+L6*ME)*COS(Q5)
      COEF(5,3) = -L2*(L5*MC+L6*MD+L6*ME)*COS(Q3-Q5)
      COEF(5,4) = -L4*(L5*MC+L6*MD+L6*ME)*COS(Q4-Q5)
      COEF(5,5) = -IC - MC*L5**2 - MD*L6**2 - ME*L6**2
      RHS(1) = MD*(DXpp-L2*COS(Q3)*U3**2-L4*COS(Q4)*U4**2-L6*COS(Q5)*U5*
     &*2) + ME*(EXpp-L2*COS(Q3)*U3**2-L4*COS(Q4)*U4**2-L6*COS(Q5)*U5**2)
     & - RX - L1*MA*COS(Q3)*U3**2 - MB*(L2*COS(Q3)*U3**2+L3*COS(Q4)*U4**
     &2) - MC*(L2*COS(Q3)*U3**2+L4*COS(Q4)*U4**2+L5*COS(Q5)*U5**2)
      RHS(2) = MD*(DYpp-L2*SIN(Q3)*U3**2-L4*SIN(Q4)*U4**2-L6*SIN(Q5)*U5*
     &*2) + ME*(EYpp-L2*SIN(Q3)*U3**2-L4*SIN(Q4)*U4**2-L6*SIN(Q5)*U5**2)
     & - G*MA - G*MB - G*MC - G*MD - G*ME - RY - L1*MA*SIN(Q3)*U3**2 - M
     &B*(L2*SIN(Q3)*U3**2+L3*SIN(Q4)*U4**2) - MC*(L2*SIN(Q3)*U3**2+L4*SI
     &N(Q4)*U4**2+L5*SIN(Q5)*U5**2)
      RHS(3) = L2*(L3*MB*SIN(Q3-Q4)*U4**2+MC*(L4*SIN(Q3-Q4)*U4**2+L5*SIN
     &(Q3-Q5)*U5**2)-MD*(DXpp*SIN(Q3)-DYpp*COS(Q3)-L4*SIN(Q3-Q4)*U4**2-L
     &6*SIN(Q3-Q5)*U5**2)-ME*(EXpp*SIN(Q3)-EYpp*COS(Q3)-L4*SIN(Q3-Q4)*U4
     &**2-L6*SIN(Q3-Q5)*U5**2)) - ATOR - G*L1*MA*COS(Q3) - G*L2*MB*COS(Q
     &3) - G*L2*MC*COS(Q3) - G*L2*MD*COS(Q3) - G*L2*ME*COS(Q3)
      RHS(4) = ATOR - KTOR - G*L3*MB*COS(Q4) - G*L4*MC*COS(Q4) - G*L4*MD
     &*COS(Q4) - G*L4*ME*COS(Q4) - L2*L3*MB*SIN(Q3-Q4)*U3**2 - L4*MC*(L2
     &*SIN(Q3-Q4)*U3**2-L5*SIN(Q4-Q5)*U5**2) - L4*MD*(DXpp*SIN(Q4)+L2*SI
     &N(Q3-Q4)*U3**2-DYpp*COS(Q4)-L6*SIN(Q4-Q5)*U5**2) - L4*ME*(EXpp*SIN
     &(Q4)+L2*SIN(Q3-Q4)*U3**2-EYpp*COS(Q4)-L6*SIN(Q4-Q5)*U5**2)
      RHS(5) = KTOR + L6*MD*(DYpp*COS(Q5)-DXpp*SIN(Q5)-L2*SIN(Q3-Q5)*U3*
     &*2-L4*SIN(Q4-Q5)*U4**2) + L6*ME*(EYpp*COS(Q5)-EXpp*SIN(Q5)-L2*SIN(
     &Q3-Q5)*U3**2-L4*SIN(Q4-Q5)*U4**2) - HTOR - G*L5*MC*COS(Q5) - G*L6*
     &MD*COS(Q5) - G*L6*ME*COS(Q5) - L5*MC*(L2*SIN(Q3-Q5)*U3**2+L4*SIN(Q
     &4-Q5)*U4**2)
      CALL SOLVE(5,COEF,RHS,VARp)

C**   Update variables after uncoupling equations
      U1p = VARp(1)
      U2p = VARp(2)
      U3p = VARp(3)
      U4p = VARp(4)
      U5p = VARp(5)

C**   Update derivative array prior to integration step
      VARp(1) = Q1p
      VARp(2) = Q2p
      VARp(3) = Q3p
      VARp(4) = Q4p
      VARp(5) = Q5p
      VARp(6) = U1p
      VARp(7) = U2p
      VARp(8) = U3p
      VARp(9) = U4p
      VARp(10) = U5p

      RETURN
      END


C**********************************************************************
      SUBROUTINE       IO(T)
      IMPLICIT         DOUBLE PRECISION (A - Z)
      INTEGER          ILOOP,NACTP
      PARAMETER        (NACTP=7)
      DIMENSION        HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(
     &NACTP),AEACTP(NACTP),AFACTP(NACTP)
      COMMON/CONSTNTS/ G,IA,IB,IC,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6,MA,MB,MC
     &,MD,ME
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,U1,U2,U3,U4,U5
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,HANG,HANGVEL,HETOR,
     &HFTOR,HTOR,HZ,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,PECM,PX,PY,RX,RY,
     &TE,Q1p,Q2p,Q3p,Q4p,Q5p,U1p,U2p,U3p,U4p,U5p,AEACT,AECCANG,AECCANGVE
     &L,AESECANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGV
     &EL,DX,DY,EX,EY,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT
     &,HFCCANG,HFCCANGVEL,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,
     &KESECANG,KESECANGVEL,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL
     &,DXp,DYp,EXp,EYp,DXpp,DYpp,EXpp,EYpp,POCMX,POCMY,PODX,PODY,POEX,PO
     &EY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,VOCMX,VOCMY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(5,5),RHS(5)
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP

C**   Evaluate output quantities
      KECM = 0.5D0*IA*U3**2 + 0.5D0*IB*U4**2 + 0.5D0*IC*U5**2 - 0.5D0*MA
     &*(2*L1*SIN(Q3)*U1*U3-U1**2-U2**2-L1**2*U3**2-2*L1*COS(Q3)*U2*U3) -
     & 0.5D0*MB*(2*L2*SIN(Q3)*U1*U3+2*L3*SIN(Q4)*U1*U4-U1**2-U2**2-L2**2
     &*U3**2-L3**2*U4**2-2*L2*COS(Q3)*U2*U3-2*L3*COS(Q4)*U2*U4-2*L2*L3*C
     &OS(Q3-Q4)*U3*U4) - 0.5D0*MC*(2*L2*SIN(Q3)*U1*U3+2*L4*SIN(Q4)*U1*U4
     &+2*L5*SIN(Q5)*U1*U5-U1**2-U2**2-L2**2*U3**2-L4**2*U4**2-L5**2*U5**
     &2-2*L2*COS(Q3)*U2*U3-2*L4*COS(Q4)*U2*U4-2*L5*COS(Q5)*U2*U5-2*L2*L4
     &*COS(Q3-Q4)*U3*U4-2*L2*L5*COS(Q3-Q5)*U3*U5-2*L4*L5*COS(Q4-Q5)*U4*U
     &5) - 0.5D0*MD*(2*L2*SIN(Q3)*U3*(DXp+U1)+2*L4*SIN(Q4)*U4*(DXp+U1)+2
     &*L6*SIN(Q5)*U5*(DXp+U1)-(DXp+U1)**2-(DYp+U2)**2-L2**2*U3**2-L4**2*
     &U4**2-L6**2*U5**2-2*L2*COS(Q3)*U3*(DYp+U2)-2*L4*COS(Q4)*U4*(DYp+U2
     &)-2*L6*COS(Q5)*U5*(DYp+U2)-2*L2*L4*COS(Q3-Q4)*U3*U4-2*L2*L6*COS(Q3
     &-Q5)*U3*U5-2*L4*L6*COS(Q4-Q5)*U4*U5) - 0.5D0*ME*(2*L2*SIN(Q3)*U3*(
     &EXp+U1)+2*L4*SIN(Q4)*U4*(EXp+U1)+2*L6*SIN(Q5)*U5*(EXp+U1)-(EXp+U1)
     &**2-(EYp+U2)**2-L2**2*U3**2-L4**2*U4**2-L6**2*U5**2-2*L2*COS(Q3)*U
     &3*(EYp+U2)-2*L4*COS(Q4)*U4*(EYp+U2)-2*L6*COS(Q5)*U5*(EYp+U2)-2*L2*
     &L4*COS(Q3-Q4)*U3*U4-2*L2*L6*COS(Q3-Q5)*U3*U5-2*L4*L6*COS(Q4-Q5)*U4
     &*U5)
      POCMY = (MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2)+(L5*MC+L6*MD+L6*M
     &E)*SIN(Q5)+(L3*MB+L4*MC+L4*MD+L4*ME)*SIN(Q4)+(L1*MA+L2*MB+L2*MC+L2
     &*MD+L2*ME)*SIN(Q3))/(MA+MB+MC+MD+ME)
      PECM = 0.5D0*K1*Q1**2 + 0.5D0*K3*Q2**2 - G*(MA+MB+MC+MD+ME)*POCMY
      TE = KECM + PECM
      HZ = MB*((L3-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q4)*U
     &2+(L5*MC+L6*MD+L6*ME)*(SIN(Q5)*U1-COS(Q5)*U2)/(MA+MB+MC+MD+ME)+(L2
     &-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3)*U2+(Q1-
     &(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME))*U2-(L3
     &-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*SIN(Q4)*U1-(L2-(L1*MA
     &+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*SIN(Q3)*U1-(Q2-(MA*Q2+
     &MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))*U1) + MC*((L5
     &-(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q5)*U2+(L4-(L3*MB+L4*MC
     &+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q4)*U2+(L2-(L1*MA+L2*MB+L2*MC+
     &L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3)*U2+(Q1-(MA*Q1+MB*Q1+MC*Q1+M
     &D*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME))*U2-(L5-(L5*MC+L6*MD+L6*ME)
     &/(MA+MB+MC+MD+ME))*SIN(Q5)*U1-(L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB
     &+MC+MD+ME))*SIN(Q4)*U1-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+
     &MC+MD+ME))*SIN(Q3)*U1-(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2)
     &)/(MA+MB+MC+MD+ME))*U1) + MD*((L6-(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD
     &+ME))*COS(Q5)*(DYp+U2)+(L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+
     &ME))*COS(Q4)*(DYp+U2)+(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+M
     &C+MD+ME))*COS(Q3)*(DYp+U2)+(DX+Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME
     &*(EX+Q1))/(MA+MB+MC+MD+ME))*(DYp+U2)-(L6-(L5*MC+L6*MD+L6*ME)/(MA+M
     &B+MC+MD+ME))*SIN(Q5)*(DXp+U1)-(L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB
     &+MC+MD+ME))*SIN(Q4)*(DXp+U1)-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(
     &MA+MB+MC+MD+ME))*SIN(Q3)*(DXp+U1)-(DY+Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY
     &+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))*(DXp+U1)) + ME*((L6-(L5*MC+L6*M
     &D+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q5)*(EYp+U2)+(L4-(L3*MB+L4*MC+L4*MD
     &+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q4)*(EYp+U2)+(L2-(L1*MA+L2*MB+L2*MC+
     &L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3)*(EYp+U2)+(EX+Q1-(MA*Q1+MB*Q
     &1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME))*(EYp+U2)-(L6-(L5*
     &MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME))*SIN(Q5)*(EXp+U1)-(L4-(L3*MB+L4*M
     &C+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*SIN(Q4)*(EXp+U1)-(L2-(L1*MA+L2*MB
     &+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*SIN(Q3)*(EXp+U1)-(EY+Q2-(MA*
     &Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))*(EXp+U1)) 
     &+ (IC-L5*MC*((L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME)-L5-(L4-(L3*MB+L4
     &*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA+L2*MB+L2*
     &MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q5)-SIN(Q5)*(Q2-(MA*Q2+MB
     &*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q5)*(Q1-(MA
     &*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME)))-L6*MD*((
     &L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME)-L6-(L4-(L3*MB+L4*MC+L4*MD+L4*M
     &E)/(MA+MB+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME
     &)/(MA+MB+MC+MD+ME))*COS(Q3-Q5)-SIN(Q5)*(DY+Q2-(MA*Q2+MB*Q2+MC*Q2+M
     &D*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q5)*(DX+Q1-(MA*Q1+MB*Q
     &1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME)))-L6*ME*((L5*MC+L6
     &*MD+L6*ME)/(MA+MB+MC+MD+ME)-L6-(L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+M
     &B+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB
     &+MC+MD+ME))*COS(Q3-Q5)-SIN(Q5)*(EY+Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2
     &)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q5)*(EX+Q1-(MA*Q1+MB*Q1+MC*Q1+
     &MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME))))*U5 + (IB-L3*MB*((L3*MB+
     &L4*MC+L4*MD+L4*ME+(L5*MC+L6*MD+L6*ME)*COS(Q4-Q5))/(MA+MB+MC+MD+ME)
     &-L3-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q
     &4)-SIN(Q4)*(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC
     &+MD+ME))-COS(Q4)*(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA
     &+MB+MC+MD+ME)))-L4*MC*((L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME)-
     &L4-(L5-(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA
     &+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q4)-SIN(Q4)*(Q2
     &-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q
     &4)*(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME))
     &)-L4*MD*((L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME)-L4-(L6-(L5*MC+
     &L6*MD+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA+L2*MB+L2*MC+L
     &2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q4)-SIN(Q4)*(DY+Q2-(MA*Q2+MB*
     &Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q4)*(DX+Q1-(
     &MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME)))-L4*ME*
     &((L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME)-L4-(L6-(L5*MC+L6*MD+L6
     &*ME)/(MA+MB+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*
     &ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q4)-SIN(Q4)*(EY+Q2-(MA*Q2+MB*Q2+MC*Q2
     &+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q4)*(EX+Q1-(MA*Q1+MB
     &*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME))))*U4 + (IA+L1*M
     &A*(L1+SIN(Q3)*(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB
     &+MC+MD+ME))+COS(Q3)*(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/
     &(MA+MB+MC+MD+ME))-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+(L5*MC+L6*MD+L6*M
     &E)*COS(Q3-Q5)+(L3*MB+L4*MC+L4*MD+L4*ME)*COS(Q3-Q4))/(MA+MB+MC+MD+M
     &E))+L2*MB*(L2+(L3-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(
     &Q3-Q4)+SIN(Q3)*(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+M
     &B+MC+MD+ME))+COS(Q3)*(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))
     &/(MA+MB+MC+MD+ME))-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+(L5*MC+L6*MD+L6*
     &ME)*COS(Q3-Q5))/(MA+MB+MC+MD+ME))-L2*MC*((L1*MA+L2*MB+L2*MC+L2*MD+
     &L2*ME)/(MA+MB+MC+MD+ME)-L2-(L5-(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME
     &))*COS(Q3-Q5)-(L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(
     &Q3-Q4)-SIN(Q3)*(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+M
     &B+MC+MD+ME))-COS(Q3)*(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))
     &/(MA+MB+MC+MD+ME)))-L2*MD*((L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+
     &MC+MD+ME)-L2-(L6-(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q5)-
     &(L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q4)-SIN(Q3)
     &*(DY+Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME)
     &)-COS(Q3)*(DX+Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+
     &MC+MD+ME)))-L2*ME*((L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME
     &)-L2-(L6-(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q5)-(L4-(L3*
     &MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q4)-SIN(Q3)*(EY+Q2-
     &(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q3
     &)*(EX+Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME
     &))))*U3 - MA*((L1-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME)
     &)*SIN(Q3)*U1+(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+
     &MC+MD+ME))*U1-(L1-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME)
     &)*COS(Q3)*U2-(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+
     &MC+MD+ME))*U2-((L5*MC+L6*MD+L6*ME)*SIN(Q5)*U1+(L3*MB+L4*MC+L4*MD+L
     &4*ME)*SIN(Q4)*U1-(L5*MC+L6*MD+L6*ME)*COS(Q5)*U2-(L3*MB+L4*MC+L4*MD
     &+L4*ME)*COS(Q4)*U2)/(MA+MB+MC+MD+ME))
      PX = MA*U1 + MB*U1 + MC*U1 + MD*(DXp+U1) + ME*(EXp+U1) - (L5*MC+L6
     &*MD+L6*ME)*SIN(Q5)*U5 - (L3*MB+L4*MC+L4*MD+L4*ME)*SIN(Q4)*U4 - (L1
     &*MA+L2*MB+L2*MC+L2*MD+L2*ME)*SIN(Q3)*U3
      PY = MA*U2 + MB*U2 + MC*U2 + MD*(DYp+U2) + ME*(EYp+U2) + (L5*MC+L6
     &*MD+L6*ME)*COS(Q5)*U5 + (L3*MB+L4*MC+L4*MD+L4*ME)*COS(Q4)*U4 + (L1
     &*MA+L2*MB+L2*MC+L2*MD+L2*ME)*COS(Q3)*U3
      HANG = 4.71238898038469D0 - Q5
      KANG = 3.141592653589793D0 + Q4 - Q5
      AANG = 3.141592653589793D0 + Q4 - Q3
      HANGVEL = -U5
      KANGVEL = U4 - U5
      AANGVEL = U4 - U3
      POP1X = Q1
      POP1Y = Q2
      POP2X = Q1 + L2*COS(Q3)
      POP2Y = Q2 + L2*SIN(Q3)
      POP3X = Q1 + L2*COS(Q3) + L4*COS(Q4)
      POP3Y = Q2 + L2*SIN(Q3) + L4*SIN(Q4)
      POP4X = Q1 + L2*COS(Q3) + L4*COS(Q4) + L6*COS(Q5)
      POP4Y = Q2 + L2*SIN(Q3) + L4*SIN(Q4) + L6*SIN(Q5)
      PODX = DX + Q1 + L2*COS(Q3) + L4*COS(Q4) + L6*COS(Q5)
      PODY = DY + Q2 + L2*SIN(Q3) + L4*SIN(Q4) + L6*SIN(Q5)
      POEX = EX + Q1 + L2*COS(Q3) + L4*COS(Q4) + L6*COS(Q5)
      POEY = EY + Q2 + L2*SIN(Q3) + L4*SIN(Q4) + L6*SIN(Q5)
      POCMX = (MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1)+(L5*MC+L6*MD+L6*M
     &E)*COS(Q5)+(L3*MB+L4*MC+L4*MD+L4*ME)*COS(Q4)+(L1*MA+L2*MB+L2*MC+L2
     &*MD+L2*ME)*COS(Q3))/(MA+MB+MC+MD+ME)
      VOCMX = (MA*U1+MB*U1+MC*U1+MD*(DXp+U1)+ME*(EXp+U1)-(L5*MC+L6*MD+L6
     &*ME)*SIN(Q5)*U5-(L3*MB+L4*MC+L4*MD+L4*ME)*SIN(Q4)*U4-(L1*MA+L2*MB+
     &L2*MC+L2*MD+L2*ME)*SIN(Q3)*U3)/(MA+MB+MC+MD+ME)
      VOCMY = (MA*U2+MB*U2+MC*U2+MD*(DYp+U2)+ME*(EYp+U2)+(L5*MC+L6*MD+L6
     &*ME)*COS(Q5)*U5+(L3*MB+L4*MC+L4*MD+L4*ME)*COS(Q4)*U4+(L1*MA+L2*MB+
     &L2*MC+L2*MD+L2*ME)*COS(Q3)*U3)/(MA+MB+MC+MD+ME)

C** Update activations for write
      CALL ACTIVATION(T,HEACTP,HEACT,2)
      CALL ACTIVATION(T,HFACTP,HFACT,2)
      CALL ACTIVATION(T,KEACTP,KEACT,2)
      CALL ACTIVATION(T,KFACTP,KFACT,2)
      CALL ACTIVATION(T,AEACTP,AEACT,2)
      CALL ACTIVATION(T,AFACTP,AFACT,2)

C**   Write output to screen and to output file(s)
!       WRITE(*, 6020) T,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,P
!      &ODX,PODY,POEX,POEY,POCMX,POCMY,VOCMX,VOCMY
      WRITE(21,6020) T,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,P
     &ODX,PODY,POEX,POEY,POCMX,POCMY,VOCMX,VOCMY
      WRITE(22,6020) T,Q1,Q2,(Q3*RADtoDEG),(Q4*RADtoDEG),(Q5*RADtoDEG),U
     &1,U2,(U3*RADtoDEG),(U4*RADtoDEG),(U5*RADtoDEG)
      WRITE(23,6020) T,RX,RY,HTOR,KTOR,ATOR
      WRITE(24,6020) T,(HANG*RADtoDEG),(KANG*RADtoDEG),(AANG*RADtoDEG),(
     &HANGVEL*RADtoDEG),(KANGVEL*RADtoDEG),(AANGVEL*RADtoDEG)
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


