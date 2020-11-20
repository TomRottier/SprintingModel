C Seven segment model used to examine the effects of the swinging limb
C on sprint performance during the stance phase.
C
C Trunk, stance thigh, swing thigh, stance shank, swing shank and stance
C foot and toes. Position of HAT CoM from hip specified as a function of   
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
      DIMENSION        VAR(14)
      DIMENSION        TT(500),CCHIP(6,500),CCKNEE(6,500),CCHAT(6,500)
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
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW

C**   Open input and output files
      OPEN(UNIT=20, FILE='7segsprint.in', STATUS='OLD')
      OPEN(UNIT=21, FILE='7segsprint.1',  STATUS='UNKNOWN')
      OPEN(UNIT=22, FILE='7segsprint.2',  STATUS='UNKNOWN')
      OPEN(UNIT=23, FILE='7segsprint.3',  STATUS='UNKNOWN')
      OPEN(UNIT=24, FILE='7segsprint.4',  STATUS='UNKNOWN')
      OPEN(UNIT=25, FILE='7segsprint.5',  STATUS='UNKNOWN')
      OPEN(UNIT=26, FILE='7segsprint.6',  STATUS='UNKNOWN')
      OPEN(UNIT=27, FILE='7segsprint.7',  STATUS='UNKNOWN')
      OPEN(UNIT=28, FILE='7segsprint.8',  STATUS='UNKNOWN')
      OPEN(UNIT=29, FILE='7segsprint.9',  STATUS='UNKNOWN')
      OPEN(UNIT=30, FILE='7segsprint.10',  STATUS='UNKNOWN')
      OPEN(UNIT=31, FILE='7segsprint.11',  STATUS='UNKNOWN')

C**   Read message from input file
      READ(20,7000,END=7100,ERR=7101) (MESSAGE(ILOOP),ILOOP = 1,99)

C**   Read values of constants from input file
      READ(20,7010,END=7100,ERR=7101) FOOTANG,G,IA,IB,IC,ID,IE,IF,IG,K1,
     &K2,K3,K4,K5,K6,K7,K8,L1,L10,L11,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,MC,M
     &D,ME,MF,MG,MTPB,MTPK

C**   Read the initial value of each variable from input file
      READ(20,7010,END=7100,ERR=7101) Q1,Q2,Q3,Q4,Q5,Q6,Q7,U1,U2,U3,U4,U
     &5,U6,U7

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
      WRITE(30,6030) (MESSAGE(ILOOP), ILOOP = 1,99) 
      WRITE(31,6031) (MESSAGE(ILOOP), ILOOP = 1,99) 

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

C**   Convert to generalised coordinates
      CALL INITCOND()

C**   Initialize time, print counter, variables array for integrator
      T      = TINITIAL
      IPRINT = 0
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

C**   Check exit conditions
5900  IF( TFINAL.GE.TINITIAL .AND. T+.01D0*INTEGSTP.GE.TFINAL) IPRINT=-7
      IF( TFINAL.LE.TINITIAL .AND. T+.01D0*INTEGSTP.LE.TFINAL) IPRINT=-7
      IF (Q2 .GT. 1.0D-05 .AND. POP2Y .GT. 1.0D-05) IPRINT = -7
C** Print      
      IF( IPRINT .LE. 0 ) THEN
        CALL IO(T)
        IF( IPRINT .EQ. -7 ) GOTO 5930
        IPRINT = PRINTINT
      ENDIF

C** Integrate      
      CALL KUTTA(EQNS1, 14, VAR, T, INTEGSTP, ABSERR, RELERR, 1, *5920)

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
      WRITE(30,6997)
      WRITE(31,6997)

C**   Inform user of input and output filename(s)
5930  WRITE(*,6999)

6021  FORMAT(1X,'FILE: 7segsprint.1 ',//1X,'*** ',99A1,///,8X,'T',12X,'P
     &OP1X',10X,'POP1Y',10X,'POP2X',10X,'POP2Y',10X,'POP3X',10X,'POP3Y',
     &10X,'POP4X',10X,'POP4Y',10X,'POP5X',10X,'POP5Y',10X,'POP6X',10X,'P
     &OP6Y',10X,'POP7X',10X,'POP7Y',10X,'POP8X',10X,'POP8Y',10X,'POP9X',
     &10X,'POP9Y',10X,'POGOX',10X,'POGOY',7X,'POCMSTANCEX',4X,'POCMSTANC
     &EY',4X,'POCMSWINGX',5X,'POCMSWINGY',8X,'POCMX',10X,'POCMY',10X,'VO
     &CMX',10X,'VOCMY',/,7X,'(S)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)
     &',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,
     &'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',
     &12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(
     &M)',12X,'(M)',12X,'(M)',11X,'(M/S)',10X,'(M/S)',/)
6022  FORMAT(1X,'FILE: 7segsprint.2 ',//1X,'*** ',99A1,///,8X,'T',13X,'Q
     &1',13X,'Q2',13X,'Q3',13X,'Q4',13X,'Q5',13X,'Q6',13X,'Q7',13X,'U1',
     &13X,'U2',13X,'U3',13X,'U4',13X,'U5',13X,'U6',13X,'U7',/,7X,'(S)',1
     &2X,'(M)',12X,'(M)',11X,'(DEG)',10X,'(DEG)',10X,'(DEG)',10X,'(DEG)'
     &,10X,'(DEG)',10X,'(M/S)',10X,'(M/S)',9X,'(DEG/S)',8X,'(DEG/S)',8X,
     &'(DEG/S)',8X,'(DEG/S)',8X,'(DEG/S)',/)
6023  FORMAT(1X,'FILE: 7segsprint.3 ',//1X,'*** ',99A1,///,8X,'T',13X,'R
     &X',13X,'RY',12X,'HTOR',11X,'KTOR',11X,'ATOR',11X,'MTOR',11X,'SHTOR
     &',10X,'SKTOR',/,7X,'(S)',12X,'(N)',12X,'(N)',11X,'(N/M)',10X,'(N/M
     &)',10X,'(N/M)',10X,'(N/M)',10X,'(N/M)',10X,'(N/M)',/)
6024  FORMAT(1X,'FILE: 7segsprint.4 ',//1X,'*** ',99A1,///,8X,'T',13X,'Q
     &3',12X,'HANG',11X,'KANG',11X,'AANG',11X,'MANG',11X,'SHANG',10X,'SK
     &ANG',11X,'U3',11X,'HANGVEL',8X,'KANGVEL',8X,'AANGVEL',8X,'MANGVEL'
     &,7X,'SHANGVEL',7X,'SKANGVEL',/,7X,'(S)',11X,'(DEG)',10X,'(DEG)',10
     &X,'(DEG)',10X,'(DEG)',10X,'(DEG)',10X,'(DEG)',10X,'(DEG)',9X,'(DEG
     &/S)',8X,'(DEG/S)',8X,'(DEG/S)',8X,'(DEG/S)',8X,'(DEG/S)',8X,'(DEG/
     &S)',8X,'(DEG/S)',/)
6025  FORMAT(1X,'FILE: 7segsprint.5 ',//1X,'*** ',99A1,///,8X,'T',12X,'K
     &ECM',11X,'PECM',12X,'TE',13X,'HZ',13X,'PX',13X,'PY',/,7X,'(S)',12X
     &,'(J)',12X,'(J)',12X,'(J)',8X,'(KG.M^2/S)',6X,'(KG.M/S)',7X,'(KG.M
     &/S)',/)
6026  FORMAT(1X,'FILE: 7segsprint.6 ',//1X,'*** ',99A1,///,8X,'T',12X,'H
     &EACT',10X,'HFACT',10X,'KEACT',10X,'KFACT',10X,'AEACT',10X,'AFACT',
     &10X,'MEACT',10X,'MFACT',/,7X,'(S)',10X,'(UNITS)',8X,'(UNITS)',8X,'
     &(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(UNITS)',8X,'(
     &UNITS)',/)
6027  FORMAT(1X,'FILE: 7segsprint.7 ',//1X,'*** ',99A1,///,8X,'T',12X,'H
     &ETOR',10X,'HEACT',9X,'HECCANG',6X,'HECCANGVEL',6X,'HESECANG',6X,'H
     &ESECANGVEL',7X,'HFTOR',10X,'HFACT',9X,'HFCCANG',6X,'HFCCANGVEL',6X
     &,'HFSECANG',6X,'HFSECANGVEL',/,7X,'(S)',11X,'(N/M)',9X,'(UNITS)',9
     &X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',9X,'(N/M)',9X,'(UNI
     &TS)',9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',/)
6028  FORMAT(1X,'FILE: 7segsprint.8 ',//1X,'*** ',99A1,///,8X,'T',12X,'K
     &ETOR',10X,'KEACT',9X,'KECCANG',6X,'KECCANGVEL',6X,'KESECANG',6X,'K
     &ESECANGVEL',7X,'KFTOR',10X,'KFACT',9X,'KFCCANG',6X,'KFCCANGVEL',6X
     &,'KFSECANG',6X,'KFSECANGVEL',/,7X,'(S)',11X,'(N/M)',9X,'(UNITS)',9
     &X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',9X,'(N/M)',9X,'(UNI
     &TS)',9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',/)
6029  FORMAT(1X,'FILE: 7segsprint.9 ',//1X,'*** ',99A1,///,8X,'T',12X,'A
     &ETOR',10X,'AEACT',9X,'AECCANG',6X,'AECCANGVEL',6X,'AESECANG',6X,'A
     &ESECANGVEL',7X,'AFTOR',10X,'AFACT',9X,'AFCCANG',6X,'AFCCANGVEL',6X
     &,'AFSECANG',6X,'AFSECANGVEL',/,7X,'(S)',11X,'(N/M)',9X,'(UNITS)',9
     &X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',9X,'(N/M)',9X,'(UNI
     &TS)',9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',/)
6030  FORMAT(1X,'FILE: 7segsprint.10',//1X,'*** ',99A1,///,8X,'T',12X,'M
     &ETOR',10X,'MEACT',9X,'MECCANG',6X,'MECCANGVEL',6X,'MESECANG',6X,'M
     &ESECANGVEL',7X,'MFTOR',10X,'MFACT',9X,'MFCCANG',6X,'MFCCANGVEL',6X
     &,'MFSECANG',6X,'MFSECANGVEL',/,7X,'(S)',11X,'(N/M)',9X,'(UNITS)',9
     &X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',9X,'(N/M)',9X,'(UNI
     &TS)',9X,'(DEG)',9X,'(DEG/S)',9X,'(DEG)',9X,'(DEG/S)',/)
6031  FORMAT(1X,'FILE: 7segsprint.11',//1X,'*** ',99A1,///,8X,'T',13X,'R
     &X1',12X,'RY1',12X,'RX2',12X,'RY2',12X,'RX',13X,'RY',13X,'GRF',12X,
     &'COP',/,7X,'(S)',12X,'(N)',12X,'(N)',12X,'(N)',12X,'(N)',12X,'(N)'
     &,12X,'(N)',12X,'(N)',12X,'(M)',/)
6997  FORMAT(/7X,'Error: Numerical integration failed to converge',/)
6999  FORMAT(//1X,'Input is in the file 7segsprint.in',//1X,'Output is i
     &n the file(s) 7segsprint.i  (i=1, ..., 11)',//1X,'The output quant
     &ities and associated files are listed in file 7segsprint.dir',/)
7000  FORMAT(//,99A1,///)
7010  FORMAT( 1000(59X,E30.0,/) )
7011  FORMAT( 3(59X,E30.0,/), 1(59X,I30,/), 2(59X,E30.0,/) )
      STOP
7100  WRITE(*,*) 'Premature end of file while reading 7segsprint.in '
7101  WRITE(*,*) 'Error while reading file 7segsprint.in'
      STOP
7200  FORMAT(//, 8(///, 10(8X, F7.2, /)))
7210  WRITE(*,*) 'Error reading torque parameters'
      STOP
7300  FORMAT(//, 6(///, 7(5X, G30.10, /)))
7310  WRITE(*,*) 'Error reading activation parameters'
      STOP
      END PROGRAM MAIN


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
      SUBROUTINE       IO(T)
      IMPLICIT         DOUBLE PRECISION (A - Z)
      INTEGER          ILOOP,NACTP
      PARAMETER        (NACTP=7)
      DIMENSION        HEACTP(NACTP),HFACTP(NACTP),KEACTP(NACTP),KFACTP(
     &NACTP),AEACTP(NACTP),AFACTP(NACTP)
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
      COMMON/ACTPARAM/ HEACTP,HFACTP,KEACTP,KFACTP,AEACTP,AFACTP

C**   Evaluate output quantities
      RX = RX1 + RX2
      RY = RY1 + RY2
      GRF = SQRT(RX**2+RY**2)
      IF (GRF .GT. 0.0D0) THEN
        COP = (Q1*RY1+RY2*POP2X)/GRF
      ELSE
        COP = 0.0D0
      ENDIF
      KECM = 0.5D0*IG*U3**2 + 0.5D0*ID*(U3-U7)**2 + 0.5D0*IC*(U3-U6-U7)*
     &*2 + 0.5D0*IE*(EAp-U3-U8)**2 + 0.5D0*IB*(U3-U5-U6-U7)**2 + 0.5D0*I
     &A*(U3-U4-U5-U6-U7)**2 + 0.5D0*IF*(EAp-FAp-U3-U8-U9)**2 + 0.5D0*MC*
     &(U1**2+U2**2+L7**2*(U3-U6-U7)**2+2*L7*Z(27)*U1*(U3-U6-U7)+2*L7*Z(2
     &8)*U2*(U3-U6-U7)+L6**2*(U3-U5-U6-U7)**2+2*L6*Z(42)*U1*(U3-U5-U6-U7
     &)+2*L6*Z(43)*U2*(U3-U5-U6-U7)+L2**2*(U3-U4-U5-U6-U7)**2+2*L6*L7*Z(
     &5)*(U3-U6-U7)*(U3-U5-U6-U7)-2*L2*Z(31)*U1*(U3-U4-U5-U6-U7)-2*L2*Z(
     &32)*U2*(U3-U4-U5-U6-U7)-2*L2*L7*Z(19)*(U3-U6-U7)*(U3-U4-U5-U6-U7)-
     &2*L2*L6*Z(3)*(U3-U5-U6-U7)*(U3-U4-U5-U6-U7)) + 0.125D0*MB*(4*U1**2
     &+4*U2**2+L3**2*(U3-U5-U6-U7)**2+L4**2*(U3-U5-U6-U7)**2+4*L3*Z(38)*
     &U1*(U3-U5-U6-U7)+4*L3*Z(39)*U2*(U3-U5-U6-U7)+4*L4*Z(42)*U1*(U3-U5-
     &U6-U7)+4*L4*Z(43)*U2*(U3-U5-U6-U7)+2*L3*L4*Z(15)*(U3-U5-U6-U7)**2+
     &4*L2**2*(U3-U4-U5-U6-U7)**2-8*L2*Z(31)*U1*(U3-U4-U5-U6-U7)-8*L2*Z(
     &32)*U2*(U3-U4-U5-U6-U7)-4*L2*L3*Z(33)*(U3-U5-U6-U7)*(U3-U4-U5-U6-U
     &7)-4*L2*L4*Z(3)*(U3-U5-U6-U7)*(U3-U4-U5-U6-U7)) + 0.5D0*MD*(U1**2+
     &U2**2+L9**2*(U3-U7)**2+2*L9*Z(46)*U1*(U3-U7)+2*L9*Z(47)*U2*(U3-U7)
     &+L8**2*(U3-U6-U7)**2+2*L8*Z(27)*U1*(U3-U6-U7)+2*L8*Z(28)*U2*(U3-U6
     &-U7)+L6**2*(U3-U5-U6-U7)**2+2*L6*Z(42)*U1*(U3-U5-U6-U7)+2*L6*Z(43)
     &*U2*(U3-U5-U6-U7)+L2**2*(U3-U4-U5-U6-U7)**2+2*L8*L9*Z(7)*(U3-U7)*(
     &U3-U6-U7)+2*L6*L9*Z(161)*(U3-U7)*(U3-U5-U6-U7)+2*L6*L8*Z(5)*(U3-U6
     &-U7)*(U3-U5-U6-U7)-2*L2*Z(31)*U1*(U3-U4-U5-U6-U7)-2*L2*Z(32)*U2*(U
     &3-U4-U5-U6-U7)-2*L2*L9*Z(157)*(U3-U7)*(U3-U4-U5-U6-U7)-2*L2*L8*Z(1
     &9)*(U3-U6-U7)*(U3-U4-U5-U6-U7)-2*L2*L6*Z(3)*(U3-U5-U6-U7)*(U3-U4-U
     &5-U6-U7)) + 0.5D0*ME*(U1**2+U2**2+L10**2*(U3-U7)**2+2*L10*Z(46)*U1
     &*(U3-U7)+2*L10*Z(47)*U2*(U3-U7)+(Z(98)-Z(17)*U3-Z(17)*U8)**2+L8**2
     &*(U3-U6-U7)**2+2*L8*Z(27)*U1*(U3-U6-U7)+2*L8*Z(28)*U2*(U3-U6-U7)+L
     &6**2*(U3-U5-U6-U7)**2+2*L6*Z(42)*U1*(U3-U5-U6-U7)+2*L6*Z(43)*U2*(U
     &3-U5-U6-U7)+L2**2*(U3-U4-U5-U6-U7)**2+2*L10*L8*Z(7)*(U3-U7)*(U3-U6
     &-U7)+2*L10*L6*Z(161)*(U3-U7)*(U3-U5-U6-U7)+2*L6*L8*Z(5)*(U3-U6-U7)
     &*(U3-U5-U6-U7)+2*L2*Z(165)*(Z(98)-Z(17)*U3-Z(17)*U8)*(U3-U4-U5-U6-
     &U7)-2*Z(48)*U2*(Z(98)-Z(17)*U3-Z(17)*U8)-2*Z(50)*U1*(Z(98)-Z(17)*U
     &3-Z(17)*U8)-2*L2*Z(31)*U1*(U3-U4-U5-U6-U7)-2*L2*Z(32)*U2*(U3-U4-U5
     &-U6-U7)-2*L10*Z(177)*(U3-U7)*(Z(98)-Z(17)*U3-Z(17)*U8)-2*L8*Z(173)
     &*(U3-U6-U7)*(Z(98)-Z(17)*U3-Z(17)*U8)-2*L10*L2*Z(157)*(U3-U7)*(U3-
     &U4-U5-U6-U7)-2*L6*Z(169)*(Z(98)-Z(17)*U3-Z(17)*U8)*(U3-U5-U6-U7)-2
     &*L2*L8*Z(19)*(U3-U6-U7)*(U3-U4-U5-U6-U7)-2*L2*L6*Z(3)*(U3-U5-U6-U7
     &)*(U3-U4-U5-U6-U7)) + 0.5D0*MG*(GSp**2+U1**2+U2**2+2*GSp*Z(1)*U1+2
     &*GSp*Z(2)*U2+GS**2*U3**2+2*GS*Z(1)*U2*U3+L10**2*(U3-U7)**2+2*L10*G
     &Sp*Z(10)*(U3-U7)+2*L10*Z(46)*U1*(U3-U7)+2*L10*Z(47)*U2*(U3-U7)+2*L
     &10*GS*Z(9)*U3*(U3-U7)+L8**2*(U3-U6-U7)**2+2*L8*GSp*Z(24)*(U3-U6-U7
     &)+2*L8*Z(27)*U1*(U3-U6-U7)+2*L8*Z(28)*U2*(U3-U6-U7)+2*L8*GS*Z(22)*
     &U3*(U3-U6-U7)+L6**2*(U3-U5-U6-U7)**2+2*L6*GSp*Z(83)*(U3-U5-U6-U7)+
     &2*L6*Z(42)*U1*(U3-U5-U6-U7)+2*L6*Z(43)*U2*(U3-U5-U6-U7)+2*L6*GS*Z(
     &85)*U3*(U3-U5-U6-U7)+L2**2*(U3-U4-U5-U6-U7)**2+2*L10*L8*Z(7)*(U3-U
     &7)*(U3-U6-U7)+2*L10*L6*Z(161)*(U3-U7)*(U3-U5-U6-U7)+2*L6*L8*Z(5)*(
     &U3-U6-U7)*(U3-U5-U6-U7)-2*GS*Z(2)*U1*U3-2*L2*GSp*Z(78)*(U3-U4-U5-U
     &6-U7)-2*L2*Z(31)*U1*(U3-U4-U5-U6-U7)-2*L2*Z(32)*U2*(U3-U4-U5-U6-U7
     &)-2*L2*GS*Z(80)*U3*(U3-U4-U5-U6-U7)-2*L10*L2*Z(157)*(U3-U7)*(U3-U4
     &-U5-U6-U7)-2*L2*L8*Z(19)*(U3-U6-U7)*(U3-U4-U5-U6-U7)-2*L2*L6*Z(3)*
     &(U3-U5-U6-U7)*(U3-U4-U5-U6-U7)) - 0.5D0*MA*(2*L1*Z(31)*U1*(U3-U4-U
     &5-U6-U7)+2*L1*Z(32)*U2*(U3-U4-U5-U6-U7)-U1**2-U2**2-L1**2*(U3-U4-U
     &5-U6-U7)**2) - 0.5D0*MF*(2*Z(48)*U2*(Z(99)-L10*U3-L10*U8)+2*Z(50)*
     &U1*(Z(99)-L10*U3-L10*U8)+2*L2*Z(31)*U1*(U3-U4-U5-U6-U7)+2*L2*Z(32)
     &*U2*(U3-U4-U5-U6-U7)+2*Z(53)*U1*(Z(100)-Z(18)*U3-Z(18)*U8-Z(18)*U9
     &)+2*Z(54)*U2*(Z(100)-Z(18)*U3-Z(18)*U8-Z(18)*U9)+2*L10*Z(177)*(U3-
     &U7)*(Z(99)-L10*U3-L10*U8)+2*L8*Z(173)*(U3-U6-U7)*(Z(99)-L10*U3-L10
     &*U8)+2*L10*L2*Z(157)*(U3-U7)*(U3-U4-U5-U6-U7)+2*L10*Z(193)*(U3-U7)
     &*(Z(100)-Z(18)*U3-Z(18)*U8-Z(18)*U9)+2*L6*Z(169)*(Z(99)-L10*U3-L10
     &*U8)*(U3-U5-U6-U7)+2*L2*L8*Z(19)*(U3-U6-U7)*(U3-U4-U5-U6-U7)+2*L8*
     &Z(189)*(U3-U6-U7)*(Z(100)-Z(18)*U3-Z(18)*U8-Z(18)*U9)+2*Z(13)*(Z(9
     &9)-L10*U3-L10*U8)*(Z(100)-Z(18)*U3-Z(18)*U8-Z(18)*U9)+2*L2*L6*Z(3)
     &*(U3-U5-U6-U7)*(U3-U4-U5-U6-U7)+2*L6*Z(185)*(U3-U5-U6-U7)*(Z(100)-
     &Z(18)*U3-Z(18)*U8-Z(18)*U9)-U1**2-U2**2-2*L10*Z(46)*U1*(U3-U7)-2*L
     &10*Z(47)*U2*(U3-U7)-L10**2*(U3-U7)**2-2*L8*Z(27)*U1*(U3-U6-U7)-2*L
     &8*Z(28)*U2*(U3-U6-U7)-(Z(99)-L10*U3-L10*U8)**2-L8**2*(U3-U6-U7)**2
     &-2*L6*Z(42)*U1*(U3-U5-U6-U7)-2*L6*Z(43)*U2*(U3-U5-U6-U7)-L6**2*(U3
     &-U5-U6-U7)**2-(Z(100)-Z(18)*U3-Z(18)*U8-Z(18)*U9)**2-2*L10*L8*Z(7)
     &*(U3-U7)*(U3-U6-U7)-L2**2*(U3-U4-U5-U6-U7)**2-2*L10*L6*Z(161)*(U3-
     &U7)*(U3-U5-U6-U7)-2*L6*L8*Z(5)*(U3-U6-U7)*(U3-U5-U6-U7)-2*L2*Z(165
     &)*(Z(99)-L10*U3-L10*U8)*(U3-U4-U5-U6-U7)-2*L2*Z(181)*(U3-U4-U5-U6-
     &U7)*(Z(100)-Z(18)*U3-Z(18)*U8-Z(18)*U9))
      Z(61) = MG*GS/MT
      POCMY = Q2 + Z(57)*Z(26) + Z(58)*Z(45) + Z(59)*Z(49) + Z(60)*Z(52)
     & + Z(61)*Z(2) + 0.5D0*Z(56)*Z(41) + 0.5D0*Z(62)*Z(37) - Z(55)*Z(30
     &)
      PECM = 0.5D0*K1*Q1**2 + 0.5D0*K3*Q2**2 - G*MT*POCMY
      TE = KECM + PECM
      Z(195) = FAp - EAp
      Z(196) = IF*Z(195)
      Z(233) = MB*(Z(198)*Z(29)+Z(199)*Z(40)+Z(200)*Z(36)-Z(57)*Z(25)-Z(
     &58)*Z(44)-Z(59)*Z(48)-Z(60)*Z(51)-Z(61)*Z(1))
      Z(226) = MA*(2*Z(197)*Z(29)-2*Z(57)*Z(25)-2*Z(58)*Z(44)-2*Z(59)*Z(
     &48)-2*Z(60)*Z(51)-2*Z(61)*Z(1)-Z(56)*Z(40)-Z(62)*Z(36))
      Z(239) = MC*(2*Z(198)*Z(29)+2*Z(216)*Z(40)+2*Z(217)*Z(25)-2*Z(58)*
     &Z(44)-2*Z(59)*Z(48)-2*Z(60)*Z(51)-2*Z(61)*Z(1)-Z(62)*Z(36))
      Z(245) = MD*(2*Z(198)*Z(29)+2*Z(216)*Z(40)+2*Z(218)*Z(25)+2*Z(219)
     &*Z(44)-2*Z(59)*Z(48)-2*Z(60)*Z(51)-2*Z(61)*Z(1)-Z(62)*Z(36))
      Z(252) = ME*(2*Z(198)*Z(29)+2*Z(216)*Z(40)+2*Z(218)*Z(25)+2*Z(220)
     &*Z(44)+2*Z(221)*Z(48)-2*Z(60)*Z(51)-2*Z(61)*Z(1)-Z(62)*Z(36))
      Z(260) = MF*(2*Z(198)*Z(29)+2*Z(216)*Z(40)+2*Z(218)*Z(25)+2*Z(220)
     &*Z(44)+2*Z(222)*Z(48)+2*Z(223)*Z(51)-2*Z(61)*Z(1)-Z(62)*Z(36))
      Z(224) = GS - Z(61)
      Z(268) = MG*(Z(62)*Z(37)+2*Z(59)*Z(49)+2*Z(60)*Z(52)-2*Z(198)*Z(30
     &)-2*Z(216)*Z(41)-2*Z(218)*Z(26)-2*Z(220)*Z(45)-2*Z(224)*Z(2))
      Z(158) = Z(3)*Z(154) + Z(4)*Z(156)
      Z(82) = Z(1)*Z(40) + Z(2)*Z(41)
      Z(230) = MB*(Z(229)+Z(198)*Z(3)-Z(57)*Z(5)-Z(58)*Z(158)-Z(59)*Z(16
     &6)-Z(60)*Z(182)-Z(61)*Z(82))
      Z(201) = Z(15)*Z(5) + Z(16)*Z(6)
      Z(203) = Z(16)*Z(5) - Z(15)*Z(6)
      Z(204) = Z(7)*Z(201) + Z(8)*Z(203)
      Z(94) = Z(1)*Z(36) + Z(2)*Z(37)
      Z(96) = Z(1)*Z(37) - Z(2)*Z(36)
      Z(208) = Z(11)*Z(94) - Z(12)*Z(96)
      Z(210) = Z(11)*Z(96) + Z(12)*Z(94)
      Z(212) = -Z(13)*Z(208) - Z(14)*Z(210)
      Z(232) = MB*(Z(231)+Z(198)*Z(33)-Z(57)*Z(201)-Z(58)*Z(204)-Z(59)*Z
     &(208)-Z(60)*Z(212)-Z(61)*Z(94))
      Z(237) = MC*(2*Z(216)+2*Z(198)*Z(3)+2*Z(217)*Z(5)-Z(236)-2*Z(58)*Z
     &(158)-2*Z(59)*Z(166)-2*Z(60)*Z(182)-2*Z(61)*Z(82))
      Z(238) = MC*(2*Z(217)+2*Z(198)*Z(19)+2*Z(216)*Z(5)-2*Z(58)*Z(7)-2*
     &Z(59)*Z(170)-2*Z(60)*Z(186)-2*Z(61)*Z(22)-Z(62)*Z(201))
      Z(242) = MD*(2*Z(216)+2*Z(198)*Z(3)+2*Z(218)*Z(5)+2*Z(219)*Z(158)-
     &Z(236)-2*Z(59)*Z(166)-2*Z(60)*Z(182)-2*Z(61)*Z(82))
      Z(243) = MD*(2*Z(218)+2*Z(198)*Z(19)+2*Z(216)*Z(5)+2*Z(219)*Z(7)-2
     &*Z(59)*Z(170)-2*Z(60)*Z(186)-2*Z(61)*Z(22)-Z(62)*Z(201))
      Z(174) = Z(7)*Z(170) + Z(8)*Z(172)
      Z(190) = Z(7)*Z(186) + Z(8)*Z(188)
      Z(244) = MD*(2*Z(219)+2*Z(198)*Z(154)+2*Z(216)*Z(158)+2*Z(218)*Z(7
     &)-2*Z(59)*Z(174)-2*Z(60)*Z(190)-2*Z(61)*Z(9)-Z(62)*Z(204))
      Z(248) = ME*(2*Z(216)+2*Z(198)*Z(3)+2*Z(218)*Z(5)+2*Z(220)*Z(158)+
     &2*Z(221)*Z(166)-Z(236)-2*Z(60)*Z(182)-2*Z(61)*Z(82))
      Z(249) = ME*(2*Z(218)+2*Z(198)*Z(19)+2*Z(216)*Z(5)+2*Z(220)*Z(7)+2
     &*Z(221)*Z(170)-2*Z(60)*Z(186)-2*Z(61)*Z(22)-Z(62)*Z(201))
      Z(250) = ME*(2*Z(220)+2*Z(198)*Z(154)+2*Z(216)*Z(158)+2*Z(218)*Z(7
     &)+2*Z(221)*Z(174)-2*Z(60)*Z(190)-2*Z(61)*Z(9)-Z(62)*Z(204))
      Z(255) = MF*(2*Z(216)+2*Z(198)*Z(3)+2*Z(218)*Z(5)+2*Z(220)*Z(158)+
     &2*Z(222)*Z(166)+2*Z(223)*Z(182)-Z(236)-2*Z(61)*Z(82))
      Z(256) = MF*(2*Z(218)+2*Z(198)*Z(19)+2*Z(216)*Z(5)+2*Z(220)*Z(7)+2
     &*Z(222)*Z(170)+2*Z(223)*Z(186)-2*Z(61)*Z(22)-Z(62)*Z(201))
      Z(257) = MF*(2*Z(220)+2*Z(198)*Z(154)+2*Z(216)*Z(158)+2*Z(218)*Z(7
     &)+2*Z(222)*Z(174)+2*Z(223)*Z(190)-2*Z(61)*Z(9)-Z(62)*Z(204))
      Z(90) = Z(1)*Z(51) + Z(2)*Z(52)
      Z(259) = MF*(2*Z(223)+2*Z(198)*Z(178)+2*Z(216)*Z(182)+2*Z(218)*Z(1
     &86)+2*Z(220)*Z(190)-2*Z(222)*Z(13)-2*Z(61)*Z(90)-Z(62)*Z(212))
      Z(77) = Z(1)*Z(29) + Z(2)*Z(30)
      Z(262) = MG*(Z(62)*Z(33)+2*Z(59)*Z(162)+2*Z(60)*Z(178)-2*Z(198)-2*
     &Z(216)*Z(3)-2*Z(218)*Z(19)-2*Z(220)*Z(154)-2*Z(224)*Z(77))
      Z(194) = IE*EAp
      Z(92) = Z(1)*Z(52) - Z(2)*Z(51)
      Z(267) = MG*GSp*(2*Z(59)*Z(12)+2*Z(198)*Z(79)+2*Z(216)*Z(84)+2*Z(2
     &18)*Z(23)-2*Z(60)*Z(92)-2*Z(220)*Z(10)-Z(62)*Z(96))
      Z(234) = MB*(Z(198)*Z(30)+Z(199)*Z(41)+Z(200)*Z(37)-Z(57)*Z(26)-Z(
     &58)*Z(45)-Z(59)*Z(49)-Z(60)*Z(52)-Z(61)*Z(2))
      Z(227) = MA*(2*Z(197)*Z(30)-2*Z(57)*Z(26)-2*Z(58)*Z(45)-2*Z(59)*Z(
     &49)-2*Z(60)*Z(52)-2*Z(61)*Z(2)-Z(56)*Z(41)-Z(62)*Z(37))
      Z(240) = MC*(2*Z(198)*Z(30)+2*Z(216)*Z(41)+2*Z(217)*Z(26)-2*Z(58)*
     &Z(45)-2*Z(59)*Z(49)-2*Z(60)*Z(52)-2*Z(61)*Z(2)-Z(62)*Z(37))
      Z(246) = MD*(2*Z(198)*Z(30)+2*Z(216)*Z(41)+2*Z(218)*Z(26)+2*Z(219)
     &*Z(45)-2*Z(59)*Z(49)-2*Z(60)*Z(52)-2*Z(61)*Z(2)-Z(62)*Z(37))
      Z(253) = ME*(2*Z(198)*Z(30)+2*Z(216)*Z(41)+2*Z(218)*Z(26)+2*Z(220)
     &*Z(45)+2*Z(221)*Z(49)-2*Z(60)*Z(52)-2*Z(61)*Z(2)-Z(62)*Z(37))
      Z(261) = MF*(2*Z(198)*Z(30)+2*Z(216)*Z(41)+2*Z(218)*Z(26)+2*Z(220)
     &*Z(45)+2*Z(222)*Z(49)+2*Z(223)*Z(52)-2*Z(61)*Z(2)-Z(62)*Z(37))
      Z(269) = MG*(Z(62)*Z(36)+2*Z(59)*Z(48)+2*Z(60)*Z(51)-2*Z(198)*Z(29
     &)-2*Z(216)*Z(40)-2*Z(218)*Z(25)-2*Z(220)*Z(44)-2*Z(224)*Z(1))
      Z(228) = MB*(Z(198)+Z(199)*Z(3)+Z(200)*Z(33)-Z(57)*Z(19)-Z(58)*Z(1
     &54)-Z(59)*Z(162)-Z(60)*Z(178)-Z(61)*Z(77))
      Z(225) = MA*(2*Z(197)-2*Z(57)*Z(19)-2*Z(58)*Z(154)-2*Z(59)*Z(162)-
     &2*Z(60)*Z(178)-2*Z(61)*Z(77)-Z(56)*Z(3)-Z(62)*Z(33))
      Z(235) = MC*(2*Z(198)+2*Z(216)*Z(3)+2*Z(217)*Z(19)-2*Z(58)*Z(154)-
     &2*Z(59)*Z(162)-2*Z(60)*Z(178)-2*Z(61)*Z(77)-Z(62)*Z(33))
      Z(241) = MD*(2*Z(198)+2*Z(216)*Z(3)+2*Z(218)*Z(19)+2*Z(219)*Z(154)
     &-2*Z(59)*Z(162)-2*Z(60)*Z(178)-2*Z(61)*Z(77)-Z(62)*Z(33))
      Z(247) = ME*(2*Z(198)+2*Z(216)*Z(3)+2*Z(218)*Z(19)+2*Z(220)*Z(154)
     &+2*Z(221)*Z(162)-2*Z(60)*Z(178)-2*Z(61)*Z(77)-Z(62)*Z(33))
      Z(251) = ME*(2*Z(11)*Z(61)+Z(62)*Z(208)-2*Z(221)-2*Z(60)*Z(13)-2*Z
     &(198)*Z(162)-2*Z(216)*Z(166)-2*Z(218)*Z(170)-2*Z(220)*Z(174))
      Z(254) = MF*(2*Z(198)+2*Z(216)*Z(3)+2*Z(218)*Z(19)+2*Z(220)*Z(154)
     &+2*Z(222)*Z(162)+2*Z(223)*Z(178)-2*Z(61)*Z(77)-Z(62)*Z(33))
      Z(258) = MF*(2*Z(223)*Z(13)+2*Z(11)*Z(61)+Z(62)*Z(208)-2*Z(222)-2*
     &Z(198)*Z(162)-2*Z(216)*Z(166)-2*Z(218)*Z(170)-2*Z(220)*Z(174))
      Z(263) = MG*(Z(236)+2*Z(59)*Z(166)+2*Z(60)*Z(182)-2*Z(216)-2*Z(198
     &)*Z(3)-2*Z(218)*Z(5)-2*Z(220)*Z(158)-2*Z(224)*Z(82))
      Z(264) = MG*(Z(62)*Z(201)+2*Z(59)*Z(170)+2*Z(60)*Z(186)-2*Z(218)-2
     &*Z(198)*Z(19)-2*Z(216)*Z(5)-2*Z(220)*Z(7)-2*Z(224)*Z(22))
      Z(265) = MG*(Z(62)*Z(204)+2*Z(59)*Z(174)+2*Z(60)*Z(190)-2*Z(220)-2
     &*Z(198)*Z(154)-2*Z(216)*Z(158)-2*Z(218)*Z(7)-2*Z(224)*Z(9))
      Z(266) = MG*(2*Z(59)*Z(11)+Z(62)*Z(94)+2*Z(60)*Z(90)-2*Z(224)-2*Z(
     &198)*Z(77)-2*Z(216)*Z(82)-2*Z(218)*Z(22)-2*Z(220)*Z(9))
      HZ = Z(196) + IA*U3 + IB*U3 + IC*U3 + ID*U3 + IE*U3 + IE*U8 + IF*U
     &3 + IF*U8 + IF*U9 + IG*U3 + Z(233)*U2 + 0.5D0*Z(226)*U2 + 0.5D0*Z(
     &239)*U2 + 0.5D0*Z(245)*U2 + 0.5D0*Z(252)*U2 + 0.5D0*Z(260)*U2 + 0.
     &5D0*Z(268)*U1 + 0.5D0*Z(230)*Z(109) + 0.5D0*Z(232)*Z(110) + 0.5D0*
     &Z(237)*Z(115) + 0.5D0*Z(238)*Z(117) + 0.5D0*Z(242)*Z(115) + 0.5D0*
     &Z(243)*Z(119) + 0.5D0*Z(244)*Z(121) + 0.5D0*Z(248)*Z(115) + 0.5D0*
     &Z(249)*Z(119) + 0.5D0*Z(250)*Z(123) + 0.5D0*Z(255)*Z(115) + 0.5D0*
     &Z(256)*Z(119) + 0.5D0*Z(257)*Z(123) + 0.5D0*Z(259)*Z(131) + 0.5D0*
     &Z(262)*Z(107) - Z(194) - 0.5D0*Z(267) - ID*U7 - Z(234)*U1 - 0.5D0*
     &Z(227)*U1 - 0.5D0*Z(240)*U1 - 0.5D0*Z(246)*U1 - 0.5D0*Z(253)*U1 - 
     &0.5D0*Z(261)*U1 - 0.5D0*Z(269)*U2 - IC*(U6+U7) - IB*(U5+U6+U7) - I
     &A*(U4+U5+U6+U7) - Z(228)*Z(107) - 0.5D0*Z(225)*Z(105) - 0.5D0*Z(23
     &5)*Z(107) - 0.5D0*Z(241)*Z(107) - 0.5D0*Z(247)*Z(107) - 0.5D0*Z(25
     &1)*Z(125) - 0.5D0*Z(254)*Z(107) - 0.5D0*Z(258)*Z(128) - 0.5D0*Z(26
     &3)*Z(115) - 0.5D0*Z(264)*Z(119) - 0.5D0*Z(265)*Z(123) - 0.5D0*Z(26
     &6)*Z(138)
      Z(298) = MG*GSp
      Z(299) = MG*GS
      Z(293) = MF*Z(100)
      Z(286) = ME*Z(98)
      Z(291) = MF*Z(99)
      PX = Z(298)*Z(1) + (MA+MB+MC+MD+ME+MF+MG)*U1 + (Z(280)+Z(284)+Z(29
     &0)+Z(297))*Z(46)*(U3-U7) + 0.5D0*Z(273)*Z(38)*(U3-U5-U6-U7) + (Z(2
     &76)+Z(279)+Z(283)+Z(289)+Z(296))*Z(27)*(U3-U6-U7) + 0.5D0*(Z(272)+
     &2*Z(275)+2*Z(278)+2*Z(282)+2*Z(288)+2*Z(295))*Z(42)*(U3-U5-U6-U7) 
     &- Z(299)*Z(2)*U3 - Z(53)*(Z(293)-Z(292)*U3-Z(292)*U8-Z(292)*U9) - 
     &Z(50)*(Z(286)+Z(291)-Z(285)*U3-Z(285)*U8-Z(290)*U3-Z(290)*U8) - (Z
     &(270)+Z(271)+Z(274)+Z(277)+Z(281)+Z(287)+Z(294))*Z(31)*(U3-U4-U5-U
     &6-U7)
      PY = Z(298)*Z(2) + Z(299)*Z(1)*U3 + (MA+MB+MC+MD+ME+MF+MG)*U2 + (Z
     &(280)+Z(284)+Z(290)+Z(297))*Z(47)*(U3-U7) + 0.5D0*Z(273)*Z(39)*(U3
     &-U5-U6-U7) + (Z(276)+Z(279)+Z(283)+Z(289)+Z(296))*Z(28)*(U3-U6-U7)
     & + 0.5D0*(Z(272)+2*Z(275)+2*Z(278)+2*Z(282)+2*Z(288)+2*Z(295))*Z(4
     &3)*(U3-U5-U6-U7) - Z(54)*(Z(293)-Z(292)*U3-Z(292)*U8-Z(292)*U9) - 
     &Z(48)*(Z(286)+Z(291)-Z(285)*U3-Z(285)*U8-Z(290)*U3-Z(290)*U8) - (Z
     &(270)+Z(271)+Z(274)+Z(277)+Z(281)+Z(287)+Z(294))*Z(32)*(U3-U4-U5-U
     &6-U7)
      HANG = 3.141592653589793D0 + Q7
      KANG = 3.141592653589793D0 - Q6
      AANG = 3.141592653589793D0 + Q5
      MANG = Q4
      HANGVEL = U7
      KANGVEL = -U6
      AANGVEL = U5
      MANGVEL = U4
      SHANG = EA
      SKANG = FA
      SHANGVEL = EAp
      SKANGVEL = FAp
      Z(316) = Z(315)*Z(48) + Z(149)*(L10*Z(48)+Z(18)*Z(54))
      Z(412) = Z(330) + Z(285)*(Z(163)*Z(108)-Z(126)-Z(167)*Z(116)-Z(171
     &)*Z(120)-Z(175)*Z(124)) + MF*(L10*Z(13)*Z(132)+Z(18)*Z(13)*Z(129)+
     &L10*Z(14)*Z(134)+L10*Z(163)*Z(108)+Z(18)*Z(179)*Z(108)-L10*Z(129)-
     &Z(18)*Z(132)-L10*Z(167)*Z(116)-L10*Z(171)*Z(120)-L10*Z(175)*Z(124)
     &-Z(18)*Z(14)*Z(130)-Z(18)*Z(183)*Z(116)-Z(18)*Z(187)*Z(120)-Z(18)*
     &Z(191)*Z(124)) - Z(328)
      Z(429) = Z(316) - Z(412)
      Z(405) = Z(404) - Z(285)*(L2*Z(165)-Z(17)-L10*Z(177)-L6*Z(169)-L8*
     &Z(173)) - MF*(2*Z(370)*Z(13)+Z(361)*Z(165)+Z(371)*Z(181)-Z(363)-Z(
     &372)-Z(363)*Z(177)-Z(365)*Z(169)-Z(366)*Z(173)-Z(370)*Z(193)-Z(373
     &)*Z(185)-Z(374)*Z(189))
      Z(406) = Z(285)*Z(48) + MF*(L10*Z(48)+Z(18)*Z(54))
      Z(407) = Z(285)*Z(50) + MF*(L10*Z(50)+Z(18)*Z(53))
      Z(408) = Z(285)*(L2*Z(165)-L10*Z(177)-L6*Z(169)-L8*Z(173)) + MF*(Z
     &(361)*Z(165)+Z(371)*Z(181)-Z(363)*Z(177)-Z(365)*Z(169)-Z(366)*Z(17
     &3)-Z(370)*Z(193)-Z(373)*Z(185)-Z(374)*Z(189))
      Z(409) = Z(285)*(L2*Z(165)-L6*Z(169)-L8*Z(173)) + MF*(Z(361)*Z(165
     &)+Z(371)*Z(181)-Z(365)*Z(169)-Z(366)*Z(173)-Z(373)*Z(185)-Z(374)*Z
     &(189))
      Z(410) = Z(285)*(L2*Z(165)-L6*Z(169)) + MF*(Z(361)*Z(165)+Z(371)*Z
     &(181)-Z(365)*Z(169)-Z(373)*Z(185))
      Z(411) = L2*(Z(285)*Z(165)+MF*(L10*Z(165)+Z(18)*Z(181)))
      SHTOR = Z(429) - Z(405)*U3p - Z(406)*U2p - Z(407)*U1p - Z(408)*U7p
     & - Z(409)*U6p - Z(410)*U5p - Z(411)*U4p
      Z(413) = IF - Z(292)*(L10*Z(13)+L2*Z(181)-Z(18)-L10*Z(193)-L6*Z(18
     &5)-L8*Z(189))
      Z(414) = Z(292)*Z(53)
      Z(415) = Z(292)*Z(54)
      Z(416) = Z(292)*(L2*Z(181)-L10*Z(193)-L6*Z(185)-L8*Z(189))
      Z(417) = Z(292)*(L2*Z(181)-L6*Z(185)-L8*Z(189))
      Z(418) = Z(292)*(L2*Z(181)-L6*Z(185))
      Z(420) = Z(419)*Z(181)
      Z(318) = Z(317)*Z(54)
      Z(421) = Z(330) + Z(292)*(Z(13)*Z(129)+Z(179)*Z(108)-Z(132)-Z(14)*
     &Z(130)-Z(183)*Z(116)-Z(187)*Z(120)-Z(191)*Z(124))
      Z(430) = Z(318) - Z(421)
      SKTOR = Z(413)*U3p + Z(414)*U1p + Z(415)*U2p + Z(416)*U7p + Z(417)
     &*U6p + Z(418)*U5p + Z(420)*U4p - Z(430)
      POP1X = Q1
      POP1Y = Q2
      POP3X = Q1 + L5*Z(36) - L2*Z(29)
      POP3Y = Q2 + L5*Z(37) - L2*Z(30)
      POP4X = Q1 + L6*Z(40) - L2*Z(29)
      POP4Y = Q2 + L6*Z(41) - L2*Z(30)
      POP5X = Q1 + L6*Z(40) + L8*Z(25) - L2*Z(29)
      POP5Y = Q2 + L6*Z(41) + L8*Z(26) - L2*Z(30)
      POP6X = Q1 + L10*Z(44) + L6*Z(40) + L8*Z(25) - L2*Z(29)
      POP6Y = Q2 + L10*Z(45) + L6*Z(41) + L8*Z(26) - L2*Z(30)
      POP7X = Q1 + L10*Z(44) + L10*Z(48) + L6*Z(40) + L8*Z(25) - L2*Z(29
     &)
      POP7Y = Q2 + L10*Z(45) + L10*Z(49) + L6*Z(41) + L8*Z(26) - L2*Z(30
     &)
      POP8X = Q1 + L10*Z(44) + L10*Z(48) + L6*Z(40) + L8*Z(25) + L8*Z(51
     &) - L2*Z(29)
      POP8Y = Q2 + L10*Z(45) + L10*Z(49) + L6*Z(41) + L8*Z(26) + L8*Z(52
     &) - L2*Z(30)
      POP9X = Q1 + L10*Z(44) + L11*Z(1) + L6*Z(40) + L8*Z(25) - L2*Z(29)
      POP9Y = Q2 + L10*Z(45) + L11*Z(2) + L6*Z(41) + L8*Z(26) - L2*Z(30)
      POGOX = Q1 + L10*Z(44) + L6*Z(40) + L8*Z(25) + GS*Z(1) - L2*Z(29)
      POGOY = Q2 + L10*Z(45) + L6*Z(41) + L8*Z(26) + GS*Z(2) - L2*Z(30)
      POCMX = Q1 + Z(57)*Z(25) + Z(58)*Z(44) + Z(59)*Z(48) + Z(60)*Z(51)
     & + Z(61)*Z(1) + 0.5D0*Z(56)*Z(40) + 0.5D0*Z(62)*Z(36) - Z(55)*Z(29
     &)
      POCMSTANCEX = Q1 + Z(66)*Z(25) + Z(67)*Z(44) + 0.5D0*Z(65)*Z(40) +
     & 0.5D0*Z(68)*Z(36) - Z(64)*Z(29)
      POCMSTANCEY = Q2 + Z(66)*Z(26) + Z(67)*Z(45) + 0.5D0*Z(65)*Z(41) +
     & 0.5D0*Z(68)*Z(37) - Z(64)*Z(30)
      POCMSWINGX = Q1 + Z(71)*Z(40) + Z(72)*Z(25) + Z(73)*Z(44) + Z(74)*
     &Z(48) + Z(75)*Z(51) - Z(70)*Z(29)
      POCMSWINGY = Q2 + Z(71)*Z(41) + Z(72)*Z(26) + Z(73)*Z(45) + Z(74)*
     &Z(49) + Z(75)*Z(52) - Z(70)*Z(30)
      Z(102) = MG*GSp/MT
      Z(103) = Z(59)*EAp
      Z(104) = Z(60)*(EAp-FAp)
      VOCMX = Z(102)*Z(1) + U1 + Z(58)*Z(46)*(U3-U7) + Z(57)*Z(27)*(U3-U
     &6-U7) + 0.5D0*Z(56)*Z(42)*(U3-U5-U6-U7) + 0.5D0*Z(62)*Z(38)*(U3-U5
     &-U6-U7) - Z(61)*Z(2)*U3 - Z(50)*(Z(103)-Z(59)*U3-Z(59)*U8) - Z(55)
     &*Z(31)*(U3-U4-U5-U6-U7) - Z(53)*(Z(104)-Z(60)*U3-Z(60)*U8-Z(60)*U9
     &)
      VOCMY = Z(102)*Z(2) + U2 + Z(61)*Z(1)*U3 + Z(58)*Z(47)*(U3-U7) + Z
     &(57)*Z(28)*(U3-U6-U7) + 0.5D0*Z(56)*Z(43)*(U3-U5-U6-U7) + 0.5D0*Z(
     &62)*Z(39)*(U3-U5-U6-U7) - Z(48)*(Z(103)-Z(59)*U3-Z(59)*U8) - Z(55)
     &*Z(32)*(U3-U4-U5-U6-U7) - Z(54)*(Z(104)-Z(60)*U3-Z(60)*U8-Z(60)*U9
     &)


C** Update activations for write
      CALL ACTIVATION(T,HEACTP,HEACT,2)
      CALL ACTIVATION(T,HFACTP,HFACT,2)
      CALL ACTIVATION(T,KEACTP,KEACT,2)
      CALL ACTIVATION(T,KFACTP,KFACT,2)
      CALL ACTIVATION(T,AEACTP,AEACT,2)
      CALL ACTIVATION(T,AFACTP,AFACT,2)
      CALL ACTIVATION(T,AEACTP,MEACT,2)
      CALL ACTIVATION(T,AFACTP,MFACT,2)

C**   Write output to screen and to output file(s)
!       WRITE(*, 6020) T,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,P
!      &OP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POP8X,POP8Y,POP9X,POP9Y,POGOX,P
!      &OGOY,POCMSTANCEX,POCMSTANCEY,POCMSWINGX,POCMSWINGY,POCMX,POCMY,VOC
!      &MX,VOCMY
      WRITE(21,6020) T,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,P
     &OP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POP8X,POP8Y,POP9X,POP9Y,POGOX,P
     &OGOY,POCMSTANCEX,POCMSTANCEY,POCMSWINGX,POCMSWINGY,POCMX,POCMY,VOC
     &MX,VOCMY
      WRITE(22,6020) T,Q1,Q2,(Q3*RADtoDEG),(Q4*RADtoDEG),(Q5*RADtoDEG),(
     &Q6*RADtoDEG),(Q7*RADtoDEG),U1,U2,(U3*RADtoDEG),(U4*RADtoDEG),(U5*R
     &ADtoDEG),(U6*DEGtoRAD),(U7*RADtoDEG)
      WRITE(23,6020) T,RX,RY,HTOR,KTOR,ATOR,MTOR,SHTOR,SKTOR
      WRITE(24,6020) T,(Q3*RADtoDEG),(HANG*RADtoDEG),(KANG*RADtoDEG),(AA
     &NG*RADtoDEG),(MANG*RADtoDEG),(SHANG*RADtoDEG),(SKANG*RADtoDEG),(U3
     &*RADtoDEG),(HANGVEL*RADtoDEG),(KANGVEL*RADtoDEG),(AANGVEL*RADtoDEG
     &),(MANGVEL*RADtoDEG),(SHANGVEL*RADtoDEG),(SKANGVEL*RADtoDEG)
      WRITE(25,6020) T,KECM,PECM,TE,HZ,PX,PY
      WRITE(26,6020) T,HEACT,HFACT,KEACT,KFACT,AEACT,AFACT,MEACT,MFACT
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
      WRITE(30,6020) T,METOR,MEACT,(MECCANG*RADtoDEG),(MECCANGVEL*RADtoD
     &EG),(MESECANG*RADtoDEG),(MESECANGVEL*RADtoDEG),MFTOR,MFACT,(MFCCAN
     &G*RADtoDEG),(MFCCANGVEL*RADtoDEG),(MFSECANG*RADtoDEG),(MFSECANGVEL
     &*RADtoDEG)
      WRITE(31,6020) T,RX1,RY1,RX2,RY2,RX,RY,GRF,COP

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
      COMMON/SPLNCOEF/ TT,CCHIP,CCKNEE,CCHAT,NROW

C** Global variables
      PI       = 4*ATAN(1.0D0)
      DEGtoRAD = PI/180.0D0
      RADtoDEG = 180.0D0/PI
      FOOTANG = FOOTANG*DEGtoRAD
      Q3 = Q3*DEGtoRAD
      Q4 = Q4*DEGtoRAD
      Q5 = Q5*DEGtoRAD
      Q6 = Q6*DEGtoRAD
      Q7 = Q7*DEGtoRAD
      U3 = U3*DEGtoRAD
      U4 = U4*DEGtoRAD
      U5 = U5*DEGtoRAD
      U6 = U6*DEGtoRAD
      U7 = U7*DEGtoRAD
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