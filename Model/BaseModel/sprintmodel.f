C Three segment model used to examine the effects of the swinging limb
C on sprint performance.
C
C Segments include thigh, shank and one part foot. Two point masses
C representing the CoM of the swinging limb and and the HAT. The 
C position (and derivatives) of these masses relative to the hip joint
C are specified at each input from the input file.
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
      INTEGER          I,NACTP,NROW,IDX
      PARAMETER        (NACTP=7)
      CHARACTER        MESSAGE(99)
      EXTERNAL         EQNS1
      DIMENSION        VAR(10)
      DIMENSION        HAT(500,6),SWING(500,6)
      DIMENSION        HETQP(10),HFTQP(10),KETQP(10),KFTQP(10),AETQP(10)
     &,AFTQP(10),HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(NACTP)
     &,AEACTP(NACTP),AFACTP(NACTP)
      COMMON/CONSTNTS/ G,IA,IB,IC,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6,MA,MB,MC
     &,MD,ME
      COMMON/SPECFIED/ DX,DY,EX,EY,DXp,DYp,EXp,EYp,DXpp,DYpp,EXpp,EYpp
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,U1,U2,U3,U4,U5
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,H,HANG,HANGVEL,HETO
     &R,HFTOR,HTOR,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,PECM,RX,RY,TE,Q1p,
     &Q2p,Q3p,Q4p,Q5p,U1p,U2p,U3p,U4p,U5p,AEACT,AECCANG,AECCANGVEL,AESEC
     &ANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,HEAC
     &T,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,HFCCANGVEL
     &,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,KESECANGVE
     &L,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,POCMX,POCMY,PODX,P
     &ODY,POEX,POEY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,VOCM
     &X,VOCMY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(5,5),RHS(5)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP
      COMMON/MASSDATA/ HAT,SWING

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

C** Read activation parameters
      OPEN(UNIT=31, FILE='activation.in', STATUS='OLD')
      READ(31, 7300, ERR=7310) HEACTP,KEACTP,AEACTP,HFACTP,KFACTP,AFACTP


C** Read values for swing leg and HAT CoM (first two rows headers)
      OPEN(UNIT=32, FILE='HAT.txt', STATUS='OLD')
      OPEN(UNIT=33, FILE='swing.txt', STATUS='OLD')
      READ(32, '(I4)', ERR=7410) NROW
      READ(33, *)
      READ(32, *)
      READ(33, *)
      READ(32, '(6E14.5)', ERR=7410) (HAT(I,:), I=1, NROW)
      READ(33, '(6E14.5)', ERR=7510) (SWING(I,:), I=1, NROW)


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

C** Initialise integration count
      IDX = 1

C** Initial velocities of masses (swing leg D, HAT E)
      DXp = SWING(1,3)
      DYp = SWING(1,4)
      EXp = HAT(1,3)
      EYp = HAT(1,4)

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
      CALL UPDATE(T,IDX)

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
      IDX = IDX + 1
      CALL UPDATE(T,IDX)

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
     &Y',11X,'POCMX',10X,'POCMY',10X,'VOCMX',10X,'VOCMY',/,5X,'(UNITS)',
     &10X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(
     &M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12
     &X,'(M)',11X,'(M/S)',10X,'(M/S)',/)
6022  FORMAT(1X,'FILE: sprintmodel.2 ',//1X,'*** ',99A1,///,8X,'T',13X,'
     &Q1',13X,'Q2',13X,'Q3',13X,'Q4',13X,'Q5',13X,'U1',13X,'U2',13X,'U3'
     &,13X,'U4',13X,'U5',/,5X,'(UNITS)',10X,'(M)',12X,'(M)',11X,'(DEG)',
     &10X,'(DEG)',10X,'(DEG)',10X,'(M/S)',10X,'(M/S)',9X,'(DEG/S)',8X,'(
     &DEG/S)',8X,'(DEG/S)',/)
6023  FORMAT(1X,'FILE: sprintmodel.3 ',//1X,'*** ',99A1,///,8X,'T',13X,'
     &RX',13X,'RY',12X,'HTOR',11X,'KTOR',11X,'ATOR',/,5X,'(UNITS)',8X,'(
     &UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',/)
6024  FORMAT(1X,'FILE: sprintmodel.4 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &HANG',11X,'KANG',11X,'AANG',10X,'HANGVEL',8X,'KANGVEL',8X,'AANGVEL
     &',/,5X,'(UNITS)',9X,'(DEG)',10X,'(DEG)',10X,'(DEG)',9X,'(DEG/S)',8
     &X,'(DEG/S)',8X,'(DEG/S)',/)
6025  FORMAT(1X,'FILE: sprintmodel.5 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &KECM',11X,'PECM',12X,'TE',13X,'H',/,5X,'(UNITS)',8X,'(UNITS)',8X,'
     &(UNITS)',8X,'(UNITS)',8X,'(UNITS)',/)
6026  FORMAT(1X,'FILE: sprintmodel.6 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &HEACT',10X,'HFACT',10X,'KEACT',10X,'KFACT',10X,'AEACT',10X,'AFACT'
     &,/,5X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)
     &',8X,'(UNITS)',8X,'(UNITS)',/)
6027  FORMAT(1X,'FILE: sprintmodel.7 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &HETOR',10X,'HEACT',9X,'HECCANG',6X,'HECCANGVEL',6X,'HESECANG',6X,'
     &HESECANGVEL',7X,'HFTOR',10X,'HFACT',9X,'HFCCANG',6X,'HFCCANGVEL',6
     &X,'HFSECANG',6X,'HFSECANGVEL',/,5X,'(UNITS)',8X,'(UNITS)',8X,'(UNI
     &TS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNIT
     &S)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS
     &)',/)
6028  FORMAT(1X,'FILE: sprintmodel.8 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &KETOR',10X,'KEACT',9X,'KECCANG',6X,'KECCANGVEL',6X,'KESECANG',6X,'
     &KESECANGVEL',7X,'KFTOR',10X,'KFACT',9X,'KFCCANG',6X,'KFCCANGVEL',6
     &X,'KFSECANG',6X,'KFSECANGVEL',/,5X,'(UNITS)',8X,'(UNITS)',8X,'(UNI
     &TS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNIT
     &S)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS
     &)',/)
6029  FORMAT(1X,'FILE: sprintmodel.9 ',//1X,'*** ',99A1,///,8X,'T',12X,'
     &AETOR',10X,'AEACT',9X,'AECCANG',6X,'AECCANGVEL',6X,'AESECANG',6X,'
     &AESECANGVEL',7X,'AFTOR',10X,'AFACT',9X,'AFCCANG',6X,'AFCCANGVEL',6
     &X,'AFSECANG',6X,'AFSECANGVEL',/,5X,'(UNITS)',8X,'(UNITS)',8X,'(UNI
     &TS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNIT
     &S)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS
     &)',/)
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
      SUBROUTINE UPDATE(T,IDX)
C Wrapper to calculate joint torques and mass positions and derivatives 
C Input/Output all through COMMON blocks
C
C***********************************************************************
      IMPLICIT DOUBLE PRECISION(A -Z)

      INTEGER          NACTP,IDX
      PARAMETER        (NACTP=7)
      DIMENSION        HETQP(10),HFTQP(10),KETQP(10),KFTQP(10),AETQP(10)
     &,AFTQP(10),HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(NACTP)
     &,AEACTP(NACTP),AFACTP(NACTP)
      DIMENSION        HAT(500,6),SWING(500,6)
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,U1,U2,U3,U4,U5
      COMMON/SPECFIED/ DX,DY,EX,EY,DXp,DYp,EXp,EYp,DXpp,DYpp,EXpp,EYpp
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,H,HANG,HANGVEL,HETO
     &R,HFTOR,HTOR,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,PECM,RX,RY,TE,Q1p,
     &Q2p,Q3p,Q4p,Q5p,U1p,U2p,U3p,U4p,U5p,AEACT,AECCANG,AECCANGVEL,AESEC
     &ANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,HEAC
     &T,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,HFCCANGVEL
     &,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,KESECANGVE
     &L,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,POCMX,POCMY,PODX,P
     &ODY,POEX,POEY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,VOCM
     &X,VOCMY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(5,5),RHS(5)
      COMMON/INTEG   / TINITIAL,TFINAL,INTEGSTP,ABSERR,RELERR,PRINTINT
      COMMON/TQPARAMS/ HETQP,HFTQP,KETQP,KFTQP,AETQP,AFTQP
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP
      COMMON/MASSDATA/ HAT,SWING

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

      HTOR = HTORF - HTORE
      KTOR = KTORE - KTORF
      ATOR = ATORF - ATORE 

      DX   = SWING(IDX,1)
      DY   = SWING(IDX,2)
      DXp  = SWING(IDX,3)
      DYp  = SWING(IDX,4)
      DXpp = SWING(IDX,5)
      DYpp = SWING(IDX,6)
      EX   = HAT(IDX,1)
      EY   = HAT(IDX,2)
      EXp  = HAT(IDX,3)
      EYp  = HAT(IDX,4)
      EXpp = HAT(IDX,5)
      EYpp = HAT(IDX,6)

      END SUBROUTINE UPDATE

C**********************************************************************
      SUBROUTINE       EQNS1(T, VAR, VARp, BOUNDARY)
      IMPLICIT         DOUBLE PRECISION (A - Z)
      INTEGER          BOUNDARY
      DIMENSION        VAR(*), VARp(*)
      COMMON/CONSTNTS/ G,IA,IB,IC,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6,MA,MB,MC
     &,MD,ME
      COMMON/SPECFIED/ DX,DY,EX,EY,DXp,DYp,EXp,EYp,DXpp,DYpp,EXpp,EYpp
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,U1,U2,U3,U4,U5
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,H,HANG,HANGVEL,HETO
     &R,HFTOR,HTOR,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,PECM,RX,RY,TE,Q1p,
     &Q2p,Q3p,Q4p,Q5p,U1p,U2p,U3p,U4p,U5p,AEACT,AECCANG,AECCANGVEL,AESEC
     &ANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,HEAC
     &T,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,HFCCANGVEL
     &,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,KESECANGVE
     &L,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,POCMX,POCMY,PODX,P
     &ODY,POEX,POEY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,VOCM
     &X,VOCMY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(5,5),RHS(5)

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
      INTEGER          ILOOP
      COMMON/CONSTNTS/ G,IA,IB,IC,K1,K2,K3,K4,L1,L2,L3,L4,L5,L6,MA,MB,MC
     &,MD,ME
      COMMON/SPECFIED/ DX,DY,EX,EY,DXp,DYp,EXp,EYp,DXpp,DYpp,EXpp,EYpp
      COMMON/VARIBLES/ Q1,Q2,Q3,Q4,Q5,U1,U2,U3,U4,U5
      COMMON/ALGBRAIC/ AANG,AANGVEL,AETOR,AFTOR,ATOR,H,HANG,HANGVEL,HETO
     &R,HFTOR,HTOR,KANG,KANGVEL,KECM,KETOR,KFTOR,KTOR,PECM,RX,RY,TE,Q1p,
     &Q2p,Q3p,Q4p,Q5p,U1p,U2p,U3p,U4p,U5p,AEACT,AECCANG,AECCANGVEL,AESEC
     &ANG,AESECANGVEL,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL,HEAC
     &T,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFACT,HFCCANG,HFCCANGVEL
     &,HFSECANG,HFSECANGVEL,KEACT,KECCANG,KECCANGVEL,KESECANG,KESECANGVE
     &L,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL,POCMX,POCMY,PODX,P
     &ODY,POEX,POEY,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,VOCM
     &X,VOCMY
      COMMON/MISCLLNS/ PI,DEGtoRAD,RADtoDEG,COEF(5,5),RHS(5)

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
      H = MB*((L3-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q4)*U2
     &+(L5*MC+L6*MD+L6*ME)*(SIN(Q5)*U1-COS(Q5)*U2)/(MA+MB+MC+MD+ME)+(L2-
     &(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3)*U2+(Q1-(
     &MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME))*U2-(L3-
     &(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*SIN(Q4)*U1-(L2-(L1*MA+
     &L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*SIN(Q3)*U1-(Q2-(MA*Q2+M
     &B*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))*U1) + MC*((L5-
     &(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q5)*U2+(L4-(L3*MB+L4*MC+
     &L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q4)*U2+(L2-(L1*MA+L2*MB+L2*MC+L
     &2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3)*U2+(Q1-(MA*Q1+MB*Q1+MC*Q1+MD
     &*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME))*U2-(L5-(L5*MC+L6*MD+L6*ME)/
     &(MA+MB+MC+MD+ME))*SIN(Q5)*U1-(L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+
     &MC+MD+ME))*SIN(Q4)*U1-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+M
     &C+MD+ME))*SIN(Q3)*U1-(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))
     &/(MA+MB+MC+MD+ME))*U1) + MD*((L6-(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+
     &ME))*COS(Q5)*(DYp+U2)+(L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+M
     &E))*COS(Q4)*(DYp+U2)+(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC
     &+MD+ME))*COS(Q3)*(DYp+U2)+(DX+Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*
     &(EX+Q1))/(MA+MB+MC+MD+ME))*(DYp+U2)-(L6-(L5*MC+L6*MD+L6*ME)/(MA+MB
     &+MC+MD+ME))*SIN(Q5)*(DXp+U1)-(L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+
     &MC+MD+ME))*SIN(Q4)*(DXp+U1)-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(M
     &A+MB+MC+MD+ME))*SIN(Q3)*(DXp+U1)-(DY+Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+
     &Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))*(DXp+U1)) + ME*((L6-(L5*MC+L6*MD
     &+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q5)*(EYp+U2)+(L4-(L3*MB+L4*MC+L4*MD+
     &L4*ME)/(MA+MB+MC+MD+ME))*COS(Q4)*(EYp+U2)+(L2-(L1*MA+L2*MB+L2*MC+L
     &2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3)*(EYp+U2)+(EX+Q1-(MA*Q1+MB*Q1
     &+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME))*(EYp+U2)-(L6-(L5*M
     &C+L6*MD+L6*ME)/(MA+MB+MC+MD+ME))*SIN(Q5)*(EXp+U1)-(L4-(L3*MB+L4*MC
     &+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*SIN(Q4)*(EXp+U1)-(L2-(L1*MA+L2*MB+
     &L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*SIN(Q3)*(EXp+U1)-(EY+Q2-(MA*Q
     &2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))*(EXp+U1)) +
     & (IC-L5*MC*((L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME)-L5-(L4-(L3*MB+L4*
     &MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA+L2*MB+L2*M
     &C+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q5)-SIN(Q5)*(Q2-(MA*Q2+MB*
     &Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q5)*(Q1-(MA*
     &Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME)))-L6*MD*((L
     &5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME)-L6-(L4-(L3*MB+L4*MC+L4*MD+L4*ME
     &)/(MA+MB+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)
     &/(MA+MB+MC+MD+ME))*COS(Q3-Q5)-SIN(Q5)*(DY+Q2-(MA*Q2+MB*Q2+MC*Q2+MD
     &*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q5)*(DX+Q1-(MA*Q1+MB*Q1
     &+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME)))-L6*ME*((L5*MC+L6*
     &MD+L6*ME)/(MA+MB+MC+MD+ME)-L6-(L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB
     &+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+
     &MC+MD+ME))*COS(Q3-Q5)-SIN(Q5)*(EY+Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)
     &+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q5)*(EX+Q1-(MA*Q1+MB*Q1+MC*Q1+M
     &D*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME))))*U5 + (IB-L3*MB*((L3*MB+L
     &4*MC+L4*MD+L4*ME+(L5*MC+L6*MD+L6*ME)*COS(Q4-Q5))/(MA+MB+MC+MD+ME)-
     &L3-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q4
     &)-SIN(Q4)*(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+
     &MD+ME))-COS(Q4)*(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+
     &MB+MC+MD+ME)))-L4*MC*((L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME)-L
     &4-(L5-(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA+
     &L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q4)-SIN(Q4)*(Q2-
     &(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q4
     &)*(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME)))
     &-L4*MD*((L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME)-L4-(L6-(L5*MC+L
     &6*MD+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA+L2*MB+L2*MC+L2
     &*MD+L2*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q4)-SIN(Q4)*(DY+Q2-(MA*Q2+MB*Q
     &2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q4)*(DX+Q1-(M
     &A*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME)))-L4*ME*(
     &(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME)-L4-(L6-(L5*MC+L6*MD+L6*
     &ME)/(MA+MB+MC+MD+ME))*COS(Q4-Q5)-(L2-(L1*MA+L2*MB+L2*MC+L2*MD+L2*M
     &E)/(MA+MB+MC+MD+ME))*COS(Q3-Q4)-SIN(Q4)*(EY+Q2-(MA*Q2+MB*Q2+MC*Q2+
     &MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q4)*(EX+Q1-(MA*Q1+MB*
     &Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME))))*U4 + (IA+L1*MA
     &*(L1+SIN(Q3)*(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+
     &MC+MD+ME))+COS(Q3)*(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(
     &MA+MB+MC+MD+ME))-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+(L5*MC+L6*MD+L6*ME
     &)*COS(Q3-Q5)+(L3*MB+L4*MC+L4*MD+L4*ME)*COS(Q3-Q4))/(MA+MB+MC+MD+ME
     &))+L2*MB*(L2+(L3-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q
     &3-Q4)+SIN(Q3)*(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB
     &+MC+MD+ME))+COS(Q3)*(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/
     &(MA+MB+MC+MD+ME))-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME+(L5*MC+L6*MD+L6*M
     &E)*COS(Q3-Q5))/(MA+MB+MC+MD+ME))-L2*MC*((L1*MA+L2*MB+L2*MC+L2*MD+L
     &2*ME)/(MA+MB+MC+MD+ME)-L2-(L5-(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME)
     &)*COS(Q3-Q5)-(L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q
     &3-Q4)-SIN(Q3)*(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB
     &+MC+MD+ME))-COS(Q3)*(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/
     &(MA+MB+MC+MD+ME)))-L2*MD*((L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+M
     &C+MD+ME)-L2-(L6-(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q5)-(
     &L4-(L3*MB+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q4)-SIN(Q3)*
     &(DY+Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))
     &-COS(Q3)*(DX+Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+M
     &C+MD+ME)))-L2*ME*((L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME)
     &-L2-(L6-(L5*MC+L6*MD+L6*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q5)-(L4-(L3*M
     &B+L4*MC+L4*MD+L4*ME)/(MA+MB+MC+MD+ME))*COS(Q3-Q4)-SIN(Q3)*(EY+Q2-(
     &MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+MC+MD+ME))-COS(Q3)
     &*(EX+Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+MC+MD+ME)
     &)))*U3 - MA*((L1-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))
     &*SIN(Q3)*U1+(Q2-(MA*Q2+MB*Q2+MC*Q2+MD*(DY+Q2)+ME*(EY+Q2))/(MA+MB+M
     &C+MD+ME))*U1-(L1-(L1*MA+L2*MB+L2*MC+L2*MD+L2*ME)/(MA+MB+MC+MD+ME))
     &*COS(Q3)*U2-(Q1-(MA*Q1+MB*Q1+MC*Q1+MD*(DX+Q1)+ME*(EX+Q1))/(MA+MB+M
     &C+MD+ME))*U2-((L5*MC+L6*MD+L6*ME)*SIN(Q5)*U1+(L3*MB+L4*MC+L4*MD+L4
     &*ME)*SIN(Q4)*U1-(L5*MC+L6*MD+L6*ME)*COS(Q5)*U2-(L3*MB+L4*MC+L4*MD+
     &L4*ME)*COS(Q4)*U2)/(MA+MB+MC+MD+ME))
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
      WRITE(25,6020) T,KECM,PECM,TE,H
      WRITE(26,6020) T,HEACT,HFACT,KEACT,KFACT,AEACT,AFACT
      WRITE(27,6020) T,HETOR,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGV
     &EL,HFTOR,HFACT,HFCCANG,HFCCANGVEL,HFSECANG,HFSECANGVEL
      WRITE(28,6020) T,KETOR,KEACT,KECCANG,KECCANGVEL,KESECANG,KESECANGV
     &EL,KFTOR,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL
      WRITE(29,6020) T,AETOR,AEACT,AECCANG,AECCANGVEL,AESECANG,AESECANGV
     &EL,AFTOR,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL

6020  FORMAT( 99(1X, 1PE14.6E3) )

      RETURN
      END

