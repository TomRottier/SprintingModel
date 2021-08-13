C**   The name of this program is 7segsprint.f
C**   Created by AUTOLEV 3.2 on Wed Aug 04 17:25:25 2021

      IMPLICIT         DOUBLE PRECISION (A - Z)
      INTEGER          ILOOP, IPRINT, PRINTINT
      CHARACTER        MESSAGE(99)
      EXTERNAL         EQNS1
      DIMENSION        VAR(14)
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
     &K2,K3,K4,K5,K6,K7,K8,L1,L10,L11,L12,L2,L3,L4,L5,L6,L7,L8,L9,MA,MB,
     &MC,MD,ME,MF,MG,MTPB,MTPK,POP1XI,POP2XI

C**   Read the initial value of each variable from input file
      READ(20,7010,END=7100,ERR=7101) Q1,Q2,Q3,Q4,Q5,Q6,Q7,U1,U2,U3,U4,U
     &5,U6,U7

C**   Read integration parameters from input file
      READ(20,7011,END=7100,ERR=7101) TINITIAL,TFINAL,INTEGSTP,PRINTINT,
     &ABSERR,RELERR

C**   Write heading(s) to output file(s)
      WRITE(*, 6021) (MESSAGE(ILOOP), ILOOP = 1,99) 
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

C**   Degree to radian conversion
      PI       = 4*ATAN(1.0D0)
      DEGtoRAD = PI/180.0D0
      RADtoDEG = 180.0D0/PI
      FOOTANG = FOOTANG*DEGtoRAD
      Q3 = Q3*DEGtoRAD
      Q4 = Q4*DEGtoRAD
      Q5 = Q5*DEGtoRAD
      Q6 = Q6*DEGtoRAD
      Q7 = Q7*DEGtoRAD

C**   Evaluate constants
      MT = MA + MB + MC + MD + ME + MF + MG
      U8 = 0
      U9 = 0
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

C**   Initalize numerical integrator with call to EQNS1 at T=TINITIAL
      CALL KUTTA(EQNS1, 14, VAR, T, INTEGSTP, ABSERR, RELERR, 0, *5920)

C**   Numerically integrate; print results
5900  IF( TFINAL.GE.TINITIAL .AND. T+.01D0*INTEGSTP.GE.TFINAL) IPRINT=-7
      IF( TFINAL.LE.TINITIAL .AND. T+.01D0*INTEGSTP.LE.TFINAL) IPRINT=-7
      IF( IPRINT .LE. 0 ) THEN
        CALL IO(T)
        IF( IPRINT .EQ. -7 ) GOTO 5930
        IPRINT = PRINTINT
      ENDIF
      CALL KUTTA(EQNS1, 14, VAR, T, INTEGSTP, ABSERR, RELERR, 1, *5920)
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
     &CMX',10X,'VOCMY',8X,'PSTANCEX',7X,'PSTANCEY',8X,'PSWINGX',8X,'PSWI
     &NGY',6X,'VOCMSTANCEX',4X,'VOCMSTANCEY',4X,'VOCMSWINGX',5X,'VOCMSWI
     &NGY',/,7X,'(S)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',
     &12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(
     &M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12
     &X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)',12X,'(M)
     &',12X,'(M)',11X,'(M/S)',10X,'(M/S)',9X,'(UNITS)',8X,'(UNITS)',8X,'
     &(UNITS)',8X,'(UNITS)',9X,'(M/S)',10X,'(M/S)',10X,'(M/S)',10X,'(M/S
     &)',/)
6022  FORMAT(1X,'FILE: 7segsprint.2 ',//1X,'*** ',99A1,///,8X,'T',13X,'Q
     &1',13X,'Q2',13X,'Q3',13X,'Q4',13X,'Q5',13X,'Q6',13X,'Q7',13X,'U1',
     &13X,'U2',13X,'U3',13X,'U4',13X,'U5',13X,'U6',13X,'U7',/,7X,'(S)',1
     &2X,'(M)',12X,'(M)',11X,'(DEG)',10X,'(DEG)',10X,'(DEG)',10X,'(DEG)'
     &,10X,'(DEG)',10X,'(M/S)',10X,'(M/S)',9X,'(DEG/S)',8X,'(DEG/S)',8X,
     &'(DEG/S)',8X,'(DEG/S)',8X,'(DEG/S)',/)
6023  FORMAT(1X,'FILE: 7segsprint.3 ',//1X,'*** ',99A1,///,8X,'T',13X,'R
     &X',13X,'RY',13X,'VRX',12X,'VRY',11X,'HTOR',11X,'KTOR',11X,'ATOR',1
     &1X,'MTOR',11X,'SHTOR',10X,'SKTOR',/,7X,'(S)',12X,'(N)',12X,'(N)',1
     &0X,'(UNITS)',8X,'(UNITS)',9X,'(N/M)',10X,'(N/M)',10X,'(N/M)',10X,'
     &(N/M)',10X,'(N/M)',10X,'(N/M)',/)
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
      END


C**********************************************************************
      SUBROUTINE       EQNS1(T, VAR, VARp, BOUNDARY)
      IMPLICIT         DOUBLE PRECISION (A - Z)
      INTEGER          BOUNDARY
      DIMENSION        VAR(*), VARp(*)
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

      VRX = T
      VRY = T

      Q1p = U1
      Q2p = U2
      Q3p = U3
      Q4p = U4
      Q5p = U5
      Q6p = U6
      Q7p = U7
      RY1 = -K3*Q2 - K4*ABS(Q2)*U2
      DP1X = Q1 - POP1XI
      RX1 = -RY1*(K1*DP1X+K2*U1)
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
      POP2Y = Q2 - L2*Z(29)
      Z(20) = Z(3)*Z(6) + Z(4)*Z(5)
      Z(31) = Z(18)*Z(27) + Z(20)*Z(25)
      VOP2Y = U2 - L2*Z(31)*(U3-U4-U5-U6-U7)
      RY2 = -K7*POP2Y - K8*ABS(POP2Y)*VOP2Y
      Z(24) = Z(1)*Z(21) - Z(2)*Z(22)
      Z(26) = Z(1)*Z(23) - Z(2)*Z(21)
      Z(28) = Z(18)*Z(24) + Z(19)*Z(26)
      POP2X = Q1 - L2*Z(28)
      DP2X = POP2X - POP2XI
      Z(30) = Z(18)*Z(26) + Z(20)*Z(24)
      VOP2X = U1 - L2*Z(30)*(U3-U4-U5-U6-U7)
      RX2 = -RY2*(K5*DP2X+K6*VOP2X)
      MANG = Q4
      MANGVEL = U4
      MTOR = MTPK*(3.141592653589793D0-MANG) - MTPB*MANGVEL
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

C**   Quantities which were specified
      EA = T**3
      EAp = T**3
      EApp = T**3
      FA = T**3
      FAp = T**3
      FApp = T**3
      GS = T**3
      GSp = T**3
      GSpp = T**3
      HEACT = T
      HFACT = T
      KEACT = T
      KFACT = T
      AEACT = T
      AFACT = T
      MEACT = T
      MFACT = T
      HECCANG = T
      HECCANGVEL = T
      HESECANG = T
      HESECANGVEL = T
      HFCCANG = T
      HFCCANGVEL = T
      HFSECANG = T
      HFSECANGVEL = T
      KECCANG = T
      KECCANGVEL = T
      KESECANG = T
      KESECANGVEL = T
      KFCCANG = T
      KFCCANGVEL = T
      KFSECANG = T
      KFSECANGVEL = T
      AECCANG = T
      AECCANGVEL = T
      AESECANG = T
      AESECANGVEL = T
      AFCCANG = T
      AFCCANGVEL = T
      AFSECANG = T
      AFSECANGVEL = T
      MECCANG = T
      MECCANGVEL = T
      MESECANG = T
      MESECANGVEL = T
      MFCCANG = T
      MFCCANGVEL = T
      MFSECANG = T
      MFSECANGVEL = T

      HETOR = T*HEACT*HECCANG*HECCANGVEL*HESECANG*HESECANGVEL
      HFTOR = T*HFACT*HFCCANG*HFCCANGVEL*HFSECANG*HFSECANGVEL
      KETOR = T*KEACT*KECCANG*KECCANGVEL*KESECANG*KESECANGVEL
      KFTOR = T*KFACT*KFCCANG*KFCCANGVEL*KFSECANG*KFSECANGVEL
      AETOR = T*AEACT*AECCANG*AECCANGVEL*AESECANG*AESECANGVEL
      AFTOR = T*AFACT*AFCCANG*AFCCANGVEL*AFSECANG*AFSECANGVEL
      HTOR = HETOR - HFTOR
      KTOR = KETOR - KFTOR
      ATOR = AETOR - AFTOR
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


C**********************************************************************
      SUBROUTINE       IO(T)
      IMPLICIT         DOUBLE PRECISION (A - Z)
      INTEGER          ILOOP
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

C**   Evaluate output quantities
      RX = RX1 + RX2
      RY = RY1 + RY2
      GRF = (RX**2+RY**2)**0.5D0
      COP = (Q1*RY1+RY2*POP2X)/GRF
      KECM = 0.5D0*IG*U3**2 + 0.5D0*ID*(U3-U7)**2 + 0.5D0*IC*(U3-U6-U7)*
     &*2 + 0.5D0*IE*(EAp-U3-U8)**2 + 0.5D0*IB*(U3-U5-U6-U7)**2 + 0.5D0*I
     &A*(U3-U4-U5-U6-U7)**2 + 0.5D0*IF*(EAp-FAp-U3-U8-U9)**2 + 0.5D0*MC*
     &(U1**2+U2**2+L7**2*(U3-U6-U7)**2+2*L7*Z(26)*U1*(U3-U6-U7)+2*L7*Z(2
     &7)*U2*(U3-U6-U7)+L6**2*(U3-U5-U6-U7)**2+2*L6*Z(41)*U1*(U3-U5-U6-U7
     &)+2*L6*Z(42)*U2*(U3-U5-U6-U7)+L2**2*(U3-U4-U5-U6-U7)**2+2*L6*L7*Z(
     &5)*(U3-U6-U7)*(U3-U5-U6-U7)-2*L2*Z(30)*U1*(U3-U4-U5-U6-U7)-2*L2*Z(
     &31)*U2*(U3-U4-U5-U6-U7)-2*L2*L7*Z(18)*(U3-U6-U7)*(U3-U4-U5-U6-U7)-
     &2*L2*L6*Z(3)*(U3-U5-U6-U7)*(U3-U4-U5-U6-U7)) + 0.125D0*MB*(4*U1**2
     &+4*U2**2+L3**2*(U3-U5-U6-U7)**2+L4**2*(U3-U5-U6-U7)**2+4*L3*Z(37)*
     &U1*(U3-U5-U6-U7)+4*L3*Z(38)*U2*(U3-U5-U6-U7)+4*L4*Z(41)*U1*(U3-U5-
     &U6-U7)+4*L4*Z(42)*U2*(U3-U5-U6-U7)+2*L3*L4*Z(15)*(U3-U5-U6-U7)**2+
     &4*L2**2*(U3-U4-U5-U6-U7)**2-8*L2*Z(30)*U1*(U3-U4-U5-U6-U7)-8*L2*Z(
     &31)*U2*(U3-U4-U5-U6-U7)-4*L2*L3*Z(32)*(U3-U5-U6-U7)*(U3-U4-U5-U6-U
     &7)-4*L2*L4*Z(3)*(U3-U5-U6-U7)*(U3-U4-U5-U6-U7)) + 0.5D0*MD*(U1**2+
     &U2**2+L9**2*(U3-U7)**2+2*L9*Z(45)*U1*(U3-U7)+2*L9*Z(46)*U2*(U3-U7)
     &+L8**2*(U3-U6-U7)**2+2*L8*Z(26)*U1*(U3-U6-U7)+2*L8*Z(27)*U2*(U3-U6
     &-U7)+L6**2*(U3-U5-U6-U7)**2+2*L6*Z(41)*U1*(U3-U5-U6-U7)+2*L6*Z(42)
     &*U2*(U3-U5-U6-U7)+L2**2*(U3-U4-U5-U6-U7)**2+2*L8*L9*Z(7)*(U3-U7)*(
     &U3-U6-U7)+2*L6*L9*Z(164)*(U3-U7)*(U3-U5-U6-U7)+2*L6*L8*Z(5)*(U3-U6
     &-U7)*(U3-U5-U6-U7)-2*L2*Z(30)*U1*(U3-U4-U5-U6-U7)-2*L2*Z(31)*U2*(U
     &3-U4-U5-U6-U7)-2*L2*L9*Z(160)*(U3-U7)*(U3-U4-U5-U6-U7)-2*L2*L8*Z(1
     &8)*(U3-U6-U7)*(U3-U4-U5-U6-U7)-2*L2*L6*Z(3)*(U3-U5-U6-U7)*(U3-U4-U
     &5-U6-U7)) + 0.5D0*ME*(U1**2+U2**2+L10**2*(U3-U7)**2+2*L10*Z(45)*U1
     &*(U3-U7)+2*L10*Z(46)*U2*(U3-U7)+(Z(97)-Z(17)*U3-Z(17)*U8)**2+L8**2
     &*(U3-U6-U7)**2+2*L8*Z(26)*U1*(U3-U6-U7)+2*L8*Z(27)*U2*(U3-U6-U7)+L
     &6**2*(U3-U5-U6-U7)**2+2*L6*Z(41)*U1*(U3-U5-U6-U7)+2*L6*Z(42)*U2*(U
     &3-U5-U6-U7)+L2**2*(U3-U4-U5-U6-U7)**2+2*L10*L8*Z(7)*(U3-U7)*(U3-U6
     &-U7)+2*L10*L6*Z(164)*(U3-U7)*(U3-U5-U6-U7)+2*L6*L8*Z(5)*(U3-U6-U7)
     &*(U3-U5-U6-U7)+2*L2*Z(168)*(Z(97)-Z(17)*U3-Z(17)*U8)*(U3-U4-U5-U6-
     &U7)-2*Z(47)*U2*(Z(97)-Z(17)*U3-Z(17)*U8)-2*Z(49)*U1*(Z(97)-Z(17)*U
     &3-Z(17)*U8)-2*L2*Z(30)*U1*(U3-U4-U5-U6-U7)-2*L2*Z(31)*U2*(U3-U4-U5
     &-U6-U7)-2*L10*Z(180)*(U3-U7)*(Z(97)-Z(17)*U3-Z(17)*U8)-2*L8*Z(176)
     &*(U3-U6-U7)*(Z(97)-Z(17)*U3-Z(17)*U8)-2*L10*L2*Z(160)*(U3-U7)*(U3-
     &U4-U5-U6-U7)-2*L6*Z(172)*(Z(97)-Z(17)*U3-Z(17)*U8)*(U3-U5-U6-U7)-2
     &*L2*L8*Z(18)*(U3-U6-U7)*(U3-U4-U5-U6-U7)-2*L2*L6*Z(3)*(U3-U5-U6-U7
     &)*(U3-U4-U5-U6-U7)) + 0.5D0*MG*(GSp**2+U1**2+U2**2+2*GSp*Z(1)*U1+2
     &*GSp*Z(2)*U2+GS**2*U3**2+2*GS*Z(1)*U2*U3+L10**2*(U3-U7)**2+2*L10*G
     &Sp*Z(10)*(U3-U7)+2*L10*Z(45)*U1*(U3-U7)+2*L10*Z(46)*U2*(U3-U7)+2*L
     &10*GS*Z(9)*U3*(U3-U7)+L8**2*(U3-U6-U7)**2+2*L8*GSp*Z(23)*(U3-U6-U7
     &)+2*L8*Z(26)*U1*(U3-U6-U7)+2*L8*Z(27)*U2*(U3-U6-U7)+2*L8*GS*Z(21)*
     &U3*(U3-U6-U7)+L6**2*(U3-U5-U6-U7)**2+2*L6*GSp*Z(82)*(U3-U5-U6-U7)+
     &2*L6*Z(41)*U1*(U3-U5-U6-U7)+2*L6*Z(42)*U2*(U3-U5-U6-U7)+2*L6*GS*Z(
     &84)*U3*(U3-U5-U6-U7)+L2**2*(U3-U4-U5-U6-U7)**2+2*L10*L8*Z(7)*(U3-U
     &7)*(U3-U6-U7)+2*L10*L6*Z(164)*(U3-U7)*(U3-U5-U6-U7)+2*L6*L8*Z(5)*(
     &U3-U6-U7)*(U3-U5-U6-U7)-2*GS*Z(2)*U1*U3-2*L2*GSp*Z(77)*(U3-U4-U5-U
     &6-U7)-2*L2*Z(30)*U1*(U3-U4-U5-U6-U7)-2*L2*Z(31)*U2*(U3-U4-U5-U6-U7
     &)-2*L2*GS*Z(79)*U3*(U3-U4-U5-U6-U7)-2*L10*L2*Z(160)*(U3-U7)*(U3-U4
     &-U5-U6-U7)-2*L2*L8*Z(18)*(U3-U6-U7)*(U3-U4-U5-U6-U7)-2*L2*L6*Z(3)*
     &(U3-U5-U6-U7)*(U3-U4-U5-U6-U7)) - 0.5D0*MA*(2*L1*Z(30)*U1*(U3-U4-U
     &5-U6-U7)+2*L1*Z(31)*U2*(U3-U4-U5-U6-U7)-U1**2-U2**2-L1**2*(U3-U4-U
     &5-U6-U7)**2) - 0.5D0*MF*(2*Z(47)*U2*(Z(98)-L10*U3-L10*U8)+2*Z(49)*
     &U1*(Z(98)-L10*U3-L10*U8)+2*L2*Z(30)*U1*(U3-U4-U5-U6-U7)+2*L2*Z(31)
     &*U2*(U3-U4-U5-U6-U7)+2*Z(52)*U1*(Z(99)-L12*U3-L12*U8-L12*U9)+2*Z(5
     &3)*U2*(Z(99)-L12*U3-L12*U8-L12*U9)+2*L10*Z(180)*(U3-U7)*(Z(98)-L10
     &*U3-L10*U8)+2*L8*Z(176)*(U3-U6-U7)*(Z(98)-L10*U3-L10*U8)+2*L10*L2*
     &Z(160)*(U3-U7)*(U3-U4-U5-U6-U7)+2*L10*Z(196)*(U3-U7)*(Z(99)-L12*U3
     &-L12*U8-L12*U9)+2*L6*Z(172)*(Z(98)-L10*U3-L10*U8)*(U3-U5-U6-U7)+2*
     &L2*L8*Z(18)*(U3-U6-U7)*(U3-U4-U5-U6-U7)+2*L8*Z(192)*(U3-U6-U7)*(Z(
     &99)-L12*U3-L12*U8-L12*U9)+2*Z(13)*(Z(98)-L10*U3-L10*U8)*(Z(99)-L12
     &*U3-L12*U8-L12*U9)+2*L2*L6*Z(3)*(U3-U5-U6-U7)*(U3-U4-U5-U6-U7)+2*L
     &6*Z(188)*(U3-U5-U6-U7)*(Z(99)-L12*U3-L12*U8-L12*U9)-U1**2-U2**2-2*
     &L10*Z(45)*U1*(U3-U7)-2*L10*Z(46)*U2*(U3-U7)-L10**2*(U3-U7)**2-2*L8
     &*Z(26)*U1*(U3-U6-U7)-2*L8*Z(27)*U2*(U3-U6-U7)-(Z(98)-L10*U3-L10*U8
     &)**2-L8**2*(U3-U6-U7)**2-2*L6*Z(41)*U1*(U3-U5-U6-U7)-2*L6*Z(42)*U2
     &*(U3-U5-U6-U7)-L6**2*(U3-U5-U6-U7)**2-(Z(99)-L12*U3-L12*U8-L12*U9)
     &**2-2*L10*L8*Z(7)*(U3-U7)*(U3-U6-U7)-L2**2*(U3-U4-U5-U6-U7)**2-2*L
     &10*L6*Z(164)*(U3-U7)*(U3-U5-U6-U7)-2*L6*L8*Z(5)*(U3-U6-U7)*(U3-U5-
     &U6-U7)-2*L2*Z(168)*(Z(98)-L10*U3-L10*U8)*(U3-U4-U5-U6-U7)-2*L2*Z(1
     &84)*(U3-U4-U5-U6-U7)*(Z(99)-L12*U3-L12*U8-L12*U9))
      POCMY = Q2 + Z(56)*Z(25) + Z(57)*Z(44) + Z(58)*Z(48) + Z(59)*Z(51)
     & + Z(60)*Z(2) + 0.5D0*Z(55)*Z(40) + 0.5D0*Z(61)*Z(36) - Z(54)*Z(29
     &)
      PECM = 0.5D0*K1*Q1**2 + 0.5D0*K3*Q2**2 - G*MT*POCMY
      TE = KECM + PECM
      Z(198) = FAp - EAp
      Z(199) = IF*Z(198)
      Z(236) = MB*(Z(201)*Z(28)+Z(202)*Z(39)+Z(203)*Z(35)-Z(56)*Z(24)-Z(
     &57)*Z(43)-Z(58)*Z(47)-Z(59)*Z(50)-Z(60)*Z(1))
      Z(229) = MA*(2*Z(200)*Z(28)-2*Z(56)*Z(24)-2*Z(57)*Z(43)-2*Z(58)*Z(
     &47)-2*Z(59)*Z(50)-2*Z(60)*Z(1)-Z(55)*Z(39)-Z(61)*Z(35))
      Z(242) = MC*(2*Z(201)*Z(28)+2*Z(219)*Z(39)+2*Z(220)*Z(24)-2*Z(57)*
     &Z(43)-2*Z(58)*Z(47)-2*Z(59)*Z(50)-2*Z(60)*Z(1)-Z(61)*Z(35))
      Z(248) = MD*(2*Z(201)*Z(28)+2*Z(219)*Z(39)+2*Z(221)*Z(24)+2*Z(222)
     &*Z(43)-2*Z(58)*Z(47)-2*Z(59)*Z(50)-2*Z(60)*Z(1)-Z(61)*Z(35))
      Z(255) = ME*(2*Z(201)*Z(28)+2*Z(219)*Z(39)+2*Z(221)*Z(24)+2*Z(223)
     &*Z(43)+2*Z(224)*Z(47)-2*Z(59)*Z(50)-2*Z(60)*Z(1)-Z(61)*Z(35))
      Z(263) = MF*(2*Z(201)*Z(28)+2*Z(219)*Z(39)+2*Z(221)*Z(24)+2*Z(223)
     &*Z(43)+2*Z(225)*Z(47)+2*Z(226)*Z(50)-2*Z(60)*Z(1)-Z(61)*Z(35))
      Z(227) = GS - Z(60)
      Z(271) = MG*(Z(61)*Z(36)+2*Z(58)*Z(48)+2*Z(59)*Z(51)-2*Z(201)*Z(29
     &)-2*Z(219)*Z(40)-2*Z(221)*Z(25)-2*Z(223)*Z(44)-2*Z(227)*Z(2))
      Z(161) = Z(3)*Z(157) + Z(4)*Z(159)
      Z(81) = Z(1)*Z(39) + Z(2)*Z(40)
      Z(233) = MB*(Z(232)+Z(201)*Z(3)-Z(56)*Z(5)-Z(57)*Z(161)-Z(58)*Z(16
     &9)-Z(59)*Z(185)-Z(60)*Z(81))
      Z(204) = Z(15)*Z(5) + Z(16)*Z(6)
      Z(206) = Z(16)*Z(5) - Z(15)*Z(6)
      Z(207) = Z(7)*Z(204) + Z(8)*Z(206)
      Z(93) = Z(1)*Z(35) + Z(2)*Z(36)
      Z(95) = Z(1)*Z(36) - Z(2)*Z(35)
      Z(211) = Z(11)*Z(93) - Z(12)*Z(95)
      Z(213) = Z(11)*Z(95) + Z(12)*Z(93)
      Z(215) = -Z(13)*Z(211) - Z(14)*Z(213)
      Z(235) = MB*(Z(234)+Z(201)*Z(32)-Z(56)*Z(204)-Z(57)*Z(207)-Z(58)*Z
     &(211)-Z(59)*Z(215)-Z(60)*Z(93))
      Z(240) = MC*(2*Z(219)+2*Z(201)*Z(3)+2*Z(220)*Z(5)-Z(239)-2*Z(57)*Z
     &(161)-2*Z(58)*Z(169)-2*Z(59)*Z(185)-2*Z(60)*Z(81))
      Z(241) = MC*(2*Z(220)+2*Z(201)*Z(18)+2*Z(219)*Z(5)-2*Z(57)*Z(7)-2*
     &Z(58)*Z(173)-2*Z(59)*Z(189)-2*Z(60)*Z(21)-Z(61)*Z(204))
      Z(245) = MD*(2*Z(219)+2*Z(201)*Z(3)+2*Z(221)*Z(5)+2*Z(222)*Z(161)-
     &Z(239)-2*Z(58)*Z(169)-2*Z(59)*Z(185)-2*Z(60)*Z(81))
      Z(246) = MD*(2*Z(221)+2*Z(201)*Z(18)+2*Z(219)*Z(5)+2*Z(222)*Z(7)-2
     &*Z(58)*Z(173)-2*Z(59)*Z(189)-2*Z(60)*Z(21)-Z(61)*Z(204))
      Z(177) = Z(7)*Z(173) + Z(8)*Z(175)
      Z(193) = Z(7)*Z(189) + Z(8)*Z(191)
      Z(247) = MD*(2*Z(222)+2*Z(201)*Z(157)+2*Z(219)*Z(161)+2*Z(221)*Z(7
     &)-2*Z(58)*Z(177)-2*Z(59)*Z(193)-2*Z(60)*Z(9)-Z(61)*Z(207))
      Z(251) = ME*(2*Z(219)+2*Z(201)*Z(3)+2*Z(221)*Z(5)+2*Z(223)*Z(161)+
     &2*Z(224)*Z(169)-Z(239)-2*Z(59)*Z(185)-2*Z(60)*Z(81))
      Z(252) = ME*(2*Z(221)+2*Z(201)*Z(18)+2*Z(219)*Z(5)+2*Z(223)*Z(7)+2
     &*Z(224)*Z(173)-2*Z(59)*Z(189)-2*Z(60)*Z(21)-Z(61)*Z(204))
      Z(253) = ME*(2*Z(223)+2*Z(201)*Z(157)+2*Z(219)*Z(161)+2*Z(221)*Z(7
     &)+2*Z(224)*Z(177)-2*Z(59)*Z(193)-2*Z(60)*Z(9)-Z(61)*Z(207))
      Z(258) = MF*(2*Z(219)+2*Z(201)*Z(3)+2*Z(221)*Z(5)+2*Z(223)*Z(161)+
     &2*Z(225)*Z(169)+2*Z(226)*Z(185)-Z(239)-2*Z(60)*Z(81))
      Z(259) = MF*(2*Z(221)+2*Z(201)*Z(18)+2*Z(219)*Z(5)+2*Z(223)*Z(7)+2
     &*Z(225)*Z(173)+2*Z(226)*Z(189)-2*Z(60)*Z(21)-Z(61)*Z(204))
      Z(260) = MF*(2*Z(223)+2*Z(201)*Z(157)+2*Z(219)*Z(161)+2*Z(221)*Z(7
     &)+2*Z(225)*Z(177)+2*Z(226)*Z(193)-2*Z(60)*Z(9)-Z(61)*Z(207))
      Z(89) = Z(1)*Z(50) + Z(2)*Z(51)
      Z(262) = MF*(2*Z(226)+2*Z(201)*Z(181)+2*Z(219)*Z(185)+2*Z(221)*Z(1
     &89)+2*Z(223)*Z(193)-2*Z(225)*Z(13)-2*Z(60)*Z(89)-Z(61)*Z(215))
      Z(76) = Z(1)*Z(28) + Z(2)*Z(29)
      Z(265) = MG*(Z(61)*Z(32)+2*Z(58)*Z(165)+2*Z(59)*Z(181)-2*Z(201)-2*
     &Z(219)*Z(3)-2*Z(221)*Z(18)-2*Z(223)*Z(157)-2*Z(227)*Z(76))
      Z(197) = IE*EAp
      Z(91) = Z(1)*Z(51) - Z(2)*Z(50)
      Z(270) = MG*GSp*(2*Z(58)*Z(12)+2*Z(201)*Z(78)+2*Z(219)*Z(83)+2*Z(2
     &21)*Z(22)-2*Z(59)*Z(91)-2*Z(223)*Z(10)-Z(61)*Z(95))
      Z(237) = MB*(Z(201)*Z(29)+Z(202)*Z(40)+Z(203)*Z(36)-Z(56)*Z(25)-Z(
     &57)*Z(44)-Z(58)*Z(48)-Z(59)*Z(51)-Z(60)*Z(2))
      Z(230) = MA*(2*Z(200)*Z(29)-2*Z(56)*Z(25)-2*Z(57)*Z(44)-2*Z(58)*Z(
     &48)-2*Z(59)*Z(51)-2*Z(60)*Z(2)-Z(55)*Z(40)-Z(61)*Z(36))
      Z(243) = MC*(2*Z(201)*Z(29)+2*Z(219)*Z(40)+2*Z(220)*Z(25)-2*Z(57)*
     &Z(44)-2*Z(58)*Z(48)-2*Z(59)*Z(51)-2*Z(60)*Z(2)-Z(61)*Z(36))
      Z(249) = MD*(2*Z(201)*Z(29)+2*Z(219)*Z(40)+2*Z(221)*Z(25)+2*Z(222)
     &*Z(44)-2*Z(58)*Z(48)-2*Z(59)*Z(51)-2*Z(60)*Z(2)-Z(61)*Z(36))
      Z(256) = ME*(2*Z(201)*Z(29)+2*Z(219)*Z(40)+2*Z(221)*Z(25)+2*Z(223)
     &*Z(44)+2*Z(224)*Z(48)-2*Z(59)*Z(51)-2*Z(60)*Z(2)-Z(61)*Z(36))
      Z(264) = MF*(2*Z(201)*Z(29)+2*Z(219)*Z(40)+2*Z(221)*Z(25)+2*Z(223)
     &*Z(44)+2*Z(225)*Z(48)+2*Z(226)*Z(51)-2*Z(60)*Z(2)-Z(61)*Z(36))
      Z(272) = MG*(Z(61)*Z(35)+2*Z(58)*Z(47)+2*Z(59)*Z(50)-2*Z(201)*Z(28
     &)-2*Z(219)*Z(39)-2*Z(221)*Z(24)-2*Z(223)*Z(43)-2*Z(227)*Z(1))
      Z(231) = MB*(Z(201)+Z(202)*Z(3)+Z(203)*Z(32)-Z(56)*Z(18)-Z(57)*Z(1
     &57)-Z(58)*Z(165)-Z(59)*Z(181)-Z(60)*Z(76))
      Z(228) = MA*(2*Z(200)-2*Z(56)*Z(18)-2*Z(57)*Z(157)-2*Z(58)*Z(165)-
     &2*Z(59)*Z(181)-2*Z(60)*Z(76)-Z(55)*Z(3)-Z(61)*Z(32))
      Z(238) = MC*(2*Z(201)+2*Z(219)*Z(3)+2*Z(220)*Z(18)-2*Z(57)*Z(157)-
     &2*Z(58)*Z(165)-2*Z(59)*Z(181)-2*Z(60)*Z(76)-Z(61)*Z(32))
      Z(244) = MD*(2*Z(201)+2*Z(219)*Z(3)+2*Z(221)*Z(18)+2*Z(222)*Z(157)
     &-2*Z(58)*Z(165)-2*Z(59)*Z(181)-2*Z(60)*Z(76)-Z(61)*Z(32))
      Z(250) = ME*(2*Z(201)+2*Z(219)*Z(3)+2*Z(221)*Z(18)+2*Z(223)*Z(157)
     &+2*Z(224)*Z(165)-2*Z(59)*Z(181)-2*Z(60)*Z(76)-Z(61)*Z(32))
      Z(254) = ME*(2*Z(11)*Z(60)+Z(61)*Z(211)-2*Z(224)-2*Z(59)*Z(13)-2*Z
     &(201)*Z(165)-2*Z(219)*Z(169)-2*Z(221)*Z(173)-2*Z(223)*Z(177))
      Z(257) = MF*(2*Z(201)+2*Z(219)*Z(3)+2*Z(221)*Z(18)+2*Z(223)*Z(157)
     &+2*Z(225)*Z(165)+2*Z(226)*Z(181)-2*Z(60)*Z(76)-Z(61)*Z(32))
      Z(261) = MF*(2*Z(226)*Z(13)+2*Z(11)*Z(60)+Z(61)*Z(211)-2*Z(225)-2*
     &Z(201)*Z(165)-2*Z(219)*Z(169)-2*Z(221)*Z(173)-2*Z(223)*Z(177))
      Z(266) = MG*(Z(239)+2*Z(58)*Z(169)+2*Z(59)*Z(185)-2*Z(219)-2*Z(201
     &)*Z(3)-2*Z(221)*Z(5)-2*Z(223)*Z(161)-2*Z(227)*Z(81))
      Z(267) = MG*(Z(61)*Z(204)+2*Z(58)*Z(173)+2*Z(59)*Z(189)-2*Z(221)-2
     &*Z(201)*Z(18)-2*Z(219)*Z(5)-2*Z(223)*Z(7)-2*Z(227)*Z(21))
      Z(268) = MG*(Z(61)*Z(207)+2*Z(58)*Z(177)+2*Z(59)*Z(193)-2*Z(223)-2
     &*Z(201)*Z(157)-2*Z(219)*Z(161)-2*Z(221)*Z(7)-2*Z(227)*Z(9))
      Z(269) = MG*(2*Z(58)*Z(11)+Z(61)*Z(93)+2*Z(59)*Z(89)-2*Z(227)-2*Z(
     &201)*Z(76)-2*Z(219)*Z(81)-2*Z(221)*Z(21)-2*Z(223)*Z(9))
      HZ = Z(199) + IA*U3 + IB*U3 + IC*U3 + ID*U3 + IE*U3 + IE*U8 + IF*U
     &3 + IF*U8 + IF*U9 + IG*U3 + Z(236)*U2 + 0.5D0*Z(229)*U2 + 0.5D0*Z(
     &242)*U2 + 0.5D0*Z(248)*U2 + 0.5D0*Z(255)*U2 + 0.5D0*Z(263)*U2 + 0.
     &5D0*Z(271)*U1 + 0.5D0*Z(233)*Z(110) + 0.5D0*Z(235)*Z(111) + 0.5D0*
     &Z(240)*Z(116) + 0.5D0*Z(241)*Z(118) + 0.5D0*Z(245)*Z(116) + 0.5D0*
     &Z(246)*Z(120) + 0.5D0*Z(247)*Z(122) + 0.5D0*Z(251)*Z(116) + 0.5D0*
     &Z(252)*Z(120) + 0.5D0*Z(253)*Z(124) + 0.5D0*Z(258)*Z(116) + 0.5D0*
     &Z(259)*Z(120) + 0.5D0*Z(260)*Z(124) + 0.5D0*Z(262)*Z(132) + 0.5D0*
     &Z(265)*Z(108) - Z(197) - 0.5D0*Z(270) - ID*U7 - Z(237)*U1 - 0.5D0*
     &Z(230)*U1 - 0.5D0*Z(243)*U1 - 0.5D0*Z(249)*U1 - 0.5D0*Z(256)*U1 - 
     &0.5D0*Z(264)*U1 - 0.5D0*Z(272)*U2 - IC*(U6+U7) - IB*(U5+U6+U7) - I
     &A*(U4+U5+U6+U7) - Z(231)*Z(108) - 0.5D0*Z(228)*Z(106) - 0.5D0*Z(23
     &8)*Z(108) - 0.5D0*Z(244)*Z(108) - 0.5D0*Z(250)*Z(108) - 0.5D0*Z(25
     &4)*Z(126) - 0.5D0*Z(257)*Z(108) - 0.5D0*Z(261)*Z(129) - 0.5D0*Z(26
     &6)*Z(116) - 0.5D0*Z(267)*Z(120) - 0.5D0*Z(268)*Z(124) - 0.5D0*Z(26
     &9)*Z(139)
      Z(301) = MG*GSp
      Z(302) = MG*GS
      Z(296) = MF*Z(99)
      Z(289) = ME*Z(97)
      Z(294) = MF*Z(98)
      PX = Z(301)*Z(1) + (MA+MB+MC+MD+ME+MF+MG)*U1 + (Z(283)+Z(287)+Z(29
     &3)+Z(300))*Z(45)*(U3-U7) + 0.5D0*Z(276)*Z(37)*(U3-U5-U6-U7) + (Z(2
     &79)+Z(282)+Z(286)+Z(292)+Z(299))*Z(26)*(U3-U6-U7) + 0.5D0*(Z(275)+
     &2*Z(278)+2*Z(281)+2*Z(285)+2*Z(291)+2*Z(298))*Z(41)*(U3-U5-U6-U7) 
     &- Z(302)*Z(2)*U3 - Z(52)*(Z(296)-Z(295)*U3-Z(295)*U8-Z(295)*U9) - 
     &Z(49)*(Z(289)+Z(294)-Z(288)*U3-Z(288)*U8-Z(293)*U3-Z(293)*U8) - (Z
     &(273)+Z(274)+Z(277)+Z(280)+Z(284)+Z(290)+Z(297))*Z(30)*(U3-U4-U5-U
     &6-U7)
      PY = Z(301)*Z(2) + Z(302)*Z(1)*U3 + (MA+MB+MC+MD+ME+MF+MG)*U2 + (Z
     &(283)+Z(287)+Z(293)+Z(300))*Z(46)*(U3-U7) + 0.5D0*Z(276)*Z(38)*(U3
     &-U5-U6-U7) + (Z(279)+Z(282)+Z(286)+Z(292)+Z(299))*Z(27)*(U3-U6-U7)
     & + 0.5D0*(Z(275)+2*Z(278)+2*Z(281)+2*Z(285)+2*Z(291)+2*Z(298))*Z(4
     &2)*(U3-U5-U6-U7) - Z(53)*(Z(296)-Z(295)*U3-Z(295)*U8-Z(295)*U9) - 
     &Z(47)*(Z(289)+Z(294)-Z(288)*U3-Z(288)*U8-Z(293)*U3-Z(293)*U8) - (Z
     &(273)+Z(274)+Z(277)+Z(280)+Z(284)+Z(290)+Z(297))*Z(31)*(U3-U4-U5-U
     &6-U7)
      HANG = 3.141592653589793D0 + Q7
      KANG = 3.141592653589793D0 - Q6
      AANG = 3.141592653589793D0 + Q5
      HANGVEL = U7
      KANGVEL = -U6
      AANGVEL = U5
      SHANG = EA
      SKANG = FA
      SHANGVEL = EAp
      SKANGVEL = FAp
      METOR = T*MEACT*MECCANG*MECCANGVEL*MESECANG*MESECANGVEL
      MFTOR = T*MFACT*MFCCANG*MFCCANGVEL*MFSECANG*MFSECANGVEL
      Z(415) = Z(333) + Z(288)*(Z(166)*Z(109)-Z(127)-Z(170)*Z(117)-Z(174
     &)*Z(121)-Z(178)*Z(125)) + MF*(L10*Z(13)*Z(133)+L12*Z(13)*Z(130)+L1
     &0*Z(14)*Z(135)+L10*Z(166)*Z(109)+L12*Z(182)*Z(109)-L10*Z(130)-L12*
     &Z(133)-L10*Z(170)*Z(117)-L10*Z(174)*Z(121)-L10*Z(178)*Z(125)-L12*Z
     &(14)*Z(131)-L12*Z(186)*Z(117)-L12*Z(190)*Z(121)-L12*Z(194)*Z(125))
     & - Z(331)
      Z(434) = Z(415) - Z(318)*Z(47) - Z(58)*VRX*Z(49) - Z(58)*VRY*Z(47)
     & - Z(59)*VRX*Z(52) - Z(59)*VRY*Z(53) - Z(150)*(L10*Z(47)+L12*Z(53)
     &)
      Z(408) = Z(407) - Z(288)*(L2*Z(168)-Z(17)-L10*Z(180)-L6*Z(172)-L8*
     &Z(176)) - MF*(2*Z(373)*Z(13)+Z(364)*Z(168)+Z(374)*Z(184)-Z(366)-Z(
     &375)-Z(366)*Z(180)-Z(368)*Z(172)-Z(369)*Z(176)-Z(373)*Z(196)-Z(376
     &)*Z(188)-Z(377)*Z(192))
      Z(409) = Z(288)*Z(47) + MF*(L10*Z(47)+L12*Z(53))
      Z(410) = Z(288)*Z(49) + MF*(L10*Z(49)+L12*Z(52))
      Z(411) = Z(288)*(L2*Z(168)-L10*Z(180)-L6*Z(172)-L8*Z(176)) + MF*(Z
     &(364)*Z(168)+Z(374)*Z(184)-Z(366)*Z(180)-Z(368)*Z(172)-Z(369)*Z(17
     &6)-Z(373)*Z(196)-Z(376)*Z(188)-Z(377)*Z(192))
      Z(412) = Z(288)*(L2*Z(168)-L6*Z(172)-L8*Z(176)) + MF*(Z(364)*Z(168
     &)+Z(374)*Z(184)-Z(368)*Z(172)-Z(369)*Z(176)-Z(376)*Z(188)-Z(377)*Z
     &(192))
      Z(413) = Z(288)*(L2*Z(168)-L6*Z(172)) + MF*(Z(364)*Z(168)+Z(374)*Z
     &(184)-Z(368)*Z(172)-Z(376)*Z(188))
      Z(414) = L2*(Z(288)*Z(168)+MF*(L10*Z(168)+L12*Z(184)))
      SHTOR = -Z(434) - Z(408)*U3p - Z(409)*U2p - Z(410)*U1p - Z(411)*U7
     &p - Z(412)*U6p - Z(413)*U5p - Z(414)*U4p
      Z(416) = IF - Z(295)*(L10*Z(13)+L2*Z(184)-L12-L10*Z(196)-L6*Z(188)
     &-L8*Z(192))
      Z(417) = Z(295)*Z(52)
      Z(418) = Z(295)*Z(53)
      Z(419) = Z(295)*(L2*Z(184)-L10*Z(196)-L6*Z(188)-L8*Z(192))
      Z(420) = Z(295)*(L2*Z(184)-L6*Z(188)-L8*Z(192))
      Z(421) = Z(295)*(L2*Z(184)-L6*Z(188))
      Z(423) = Z(422)*Z(184)
      Z(321) = Z(320)*Z(53) + Z(59)*(VRX*Z(52)+VRY*Z(53))
      Z(424) = Z(333) + Z(295)*(Z(13)*Z(130)+Z(182)*Z(109)-Z(133)-Z(14)*
     &Z(131)-Z(186)*Z(117)-Z(190)*Z(121)-Z(194)*Z(125))
      Z(433) = Z(321) - Z(424)
      SKTOR = Z(416)*U3p + Z(417)*U1p + Z(418)*U2p + Z(419)*U7p + Z(420)
     &*U6p + Z(421)*U5p + Z(423)*U4p - Z(433)
      POP1X = Q1
      POP1Y = Q2
      POP3X = Q1 + L5*Z(35) - L2*Z(28)
      POP3Y = Q2 + L5*Z(36) - L2*Z(29)
      POP4X = Q1 + L6*Z(39) - L2*Z(28)
      POP4Y = Q2 + L6*Z(40) - L2*Z(29)
      POP5X = Q1 + L6*Z(39) + L8*Z(24) - L2*Z(28)
      POP5Y = Q2 + L6*Z(40) + L8*Z(25) - L2*Z(29)
      POP6X = Q1 + L10*Z(43) + L6*Z(39) + L8*Z(24) - L2*Z(28)
      POP6Y = Q2 + L10*Z(44) + L6*Z(40) + L8*Z(25) - L2*Z(29)
      POP7X = Q1 + L10*Z(43) + L10*Z(47) + L6*Z(39) + L8*Z(24) - L2*Z(28
     &)
      POP7Y = Q2 + L10*Z(44) + L10*Z(48) + L6*Z(40) + L8*Z(25) - L2*Z(29
     &)
      POP8X = Q1 + L10*Z(43) + L10*Z(47) + L6*Z(39) + L8*Z(24) + L8*Z(50
     &) - L2*Z(28)
      POP8Y = Q2 + L10*Z(44) + L10*Z(48) + L6*Z(40) + L8*Z(25) + L8*Z(51
     &) - L2*Z(29)
      POP9X = Q1 + L10*Z(43) + L11*Z(1) + L6*Z(39) + L8*Z(24) - L2*Z(28)
      POP9Y = Q2 + L10*Z(44) + L11*Z(2) + L6*Z(40) + L8*Z(25) - L2*Z(29)
      POGOX = Q1 + L10*Z(43) + L6*Z(39) + L8*Z(24) + GS*Z(1) - L2*Z(28)
      POGOY = Q2 + L10*Z(44) + L6*Z(40) + L8*Z(25) + GS*Z(2) - L2*Z(29)
      POCMX = Q1 + Z(56)*Z(24) + Z(57)*Z(43) + Z(58)*Z(47) + Z(59)*Z(50)
     & + Z(60)*Z(1) + 0.5D0*Z(55)*Z(39) + 0.5D0*Z(61)*Z(35) - Z(54)*Z(28
     &)
      POCMSTANCEX = Q1 + Z(65)*Z(24) + Z(66)*Z(43) + 0.5D0*Z(64)*Z(39) +
     & 0.5D0*Z(67)*Z(35) - Z(63)*Z(28)
      POCMSTANCEY = Q2 + Z(65)*Z(25) + Z(66)*Z(44) + 0.5D0*Z(64)*Z(40) +
     & 0.5D0*Z(67)*Z(36) - Z(63)*Z(29)
      POCMSWINGX = Q1 + Z(70)*Z(39) + Z(71)*Z(24) + Z(72)*Z(43) + Z(73)*
     &Z(47) + Z(74)*Z(50) - Z(69)*Z(28)
      POCMSWINGY = Q2 + Z(70)*Z(40) + Z(71)*Z(25) + Z(72)*Z(44) + Z(73)*
     &Z(48) + Z(74)*Z(51) - Z(69)*Z(29)
      Z(101) = MG*GSp/MT
      Z(102) = Z(58)*EAp
      Z(103) = Z(59)*(EAp-FAp)
      Z(104) = Z(73)*EAp
      Z(105) = Z(74)*(EAp-FAp)
      VOCMX = Z(101)*Z(1) + U1 + Z(57)*Z(45)*(U3-U7) + Z(56)*Z(26)*(U3-U
     &6-U7) + 0.5D0*Z(55)*Z(41)*(U3-U5-U6-U7) + 0.5D0*Z(61)*Z(37)*(U3-U5
     &-U6-U7) - Z(60)*Z(2)*U3 - Z(49)*(Z(102)-Z(58)*U3-Z(58)*U8) - Z(54)
     &*Z(30)*(U3-U4-U5-U6-U7) - Z(52)*(Z(103)-Z(59)*U3-Z(59)*U8-Z(59)*U9
     &)
      VOCMY = Z(101)*Z(2) + U2 + Z(60)*Z(1)*U3 + Z(57)*Z(46)*(U3-U7) + Z
     &(56)*Z(27)*(U3-U6-U7) + 0.5D0*Z(55)*Z(42)*(U3-U5-U6-U7) + 0.5D0*Z(
     &61)*Z(38)*(U3-U5-U6-U7) - Z(47)*(Z(102)-Z(58)*U3-Z(58)*U8) - Z(54)
     &*Z(31)*(U3-U4-U5-U6-U7) - Z(53)*(Z(103)-Z(59)*U3-Z(59)*U8-Z(59)*U9
     &)
      VOCMSTANCEX = U1 + Z(66)*Z(45)*(U3-U7) + Z(65)*Z(26)*(U3-U6-U7) + 
     &0.5D0*Z(64)*Z(41)*(U3-U5-U6-U7) + 0.5D0*Z(67)*Z(37)*(U3-U5-U6-U7) 
     &- Z(63)*Z(30)*(U3-U4-U5-U6-U7)
      VOCMSTANCEY = U2 + Z(66)*Z(46)*(U3-U7) + Z(65)*Z(27)*(U3-U6-U7) + 
     &0.5D0*Z(64)*Z(42)*(U3-U5-U6-U7) + 0.5D0*Z(67)*Z(38)*(U3-U5-U6-U7) 
     &- Z(63)*Z(31)*(U3-U4-U5-U6-U7)
      VOCMSWINGX = U1 + Z(72)*Z(45)*(U3-U7) + Z(71)*Z(26)*(U3-U6-U7) + Z
     &(70)*Z(41)*(U3-U5-U6-U7) - Z(49)*(Z(104)-Z(73)*U3-Z(73)*U8) - Z(69
     &)*Z(30)*(U3-U4-U5-U6-U7) - Z(52)*(Z(105)-Z(74)*U3-Z(74)*U8-Z(74)*U
     &9)
      VOCMSWINGY = U2 + Z(72)*Z(46)*(U3-U7) + Z(71)*Z(27)*(U3-U6-U7) + Z
     &(70)*Z(42)*(U3-U5-U6-U7) - Z(47)*(Z(104)-Z(73)*U3-Z(73)*U8) - Z(69
     &)*Z(31)*(U3-U4-U5-U6-U7) - Z(53)*(Z(105)-Z(74)*U3-Z(74)*U8-Z(74)*U
     &9)
      PSWINGX = (ME+MF)*U1 + (Z(287)+Z(293))*Z(45)*(U3-U7) + (Z(286)+Z(2
     &92))*Z(26)*(U3-U6-U7) + (Z(285)+Z(291))*Z(41)*(U3-U5-U6-U7) - Z(52
     &)*(Z(296)-Z(295)*U3-Z(295)*U8-Z(295)*U9) - (Z(284)+Z(290))*Z(30)*(
     &U3-U4-U5-U6-U7) - Z(49)*(Z(289)+Z(294)-Z(288)*U3-Z(288)*U8-Z(293)*
     &U3-Z(293)*U8)
      PSWINGY = (ME+MF)*U2 + (Z(287)+Z(293))*Z(46)*(U3-U7) + (Z(286)+Z(2
     &92))*Z(27)*(U3-U6-U7) + (Z(285)+Z(291))*Z(42)*(U3-U5-U6-U7) - Z(53
     &)*(Z(296)-Z(295)*U3-Z(295)*U8-Z(295)*U9) - (Z(284)+Z(290))*Z(31)*(
     &U3-U4-U5-U6-U7) - Z(47)*(Z(289)+Z(294)-Z(288)*U3-Z(288)*U8-Z(293)*
     &U3-Z(293)*U8)
      PSTANCEX = (MA+MB+MC+MD)*U1 + Z(283)*Z(45)*(U3-U7) + (Z(279)+Z(282
     &))*Z(26)*(U3-U6-U7) + 0.5D0*Z(276)*Z(37)*(U3-U5-U6-U7) + 0.5D0*(Z(
     &275)+2*Z(278)+2*Z(281))*Z(41)*(U3-U5-U6-U7) - (Z(273)+Z(274)+Z(277
     &)+Z(280))*Z(30)*(U3-U4-U5-U6-U7)
      PSTANCEY = (MA+MB+MC+MD)*U2 + Z(283)*Z(46)*(U3-U7) + (Z(279)+Z(282
     &))*Z(27)*(U3-U6-U7) + 0.5D0*Z(276)*Z(38)*(U3-U5-U6-U7) + 0.5D0*(Z(
     &275)+2*Z(278)+2*Z(281))*Z(42)*(U3-U5-U6-U7) - (Z(273)+Z(274)+Z(277
     &)+Z(280))*Z(31)*(U3-U4-U5-U6-U7)

C**   Write output to screen and to output file(s)
      WRITE(*, 6020) T,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,P
     &OP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POP8X,POP8Y,POP9X,POP9Y,POGOX,P
     &OGOY,POCMSTANCEX,POCMSTANCEY,POCMSWINGX,POCMSWINGY,POCMX,POCMY,VOC
     &MX,VOCMY,PSTANCEX,PSTANCEY,PSWINGX,PSWINGY,VOCMSTANCEX,VOCMSTANCEY
     &,VOCMSWINGX,VOCMSWINGY
      WRITE(21,6020) T,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,P
     &OP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POP8X,POP8Y,POP9X,POP9Y,POGOX,P
     &OGOY,POCMSTANCEX,POCMSTANCEY,POCMSWINGX,POCMSWINGY,POCMX,POCMY,VOC
     &MX,VOCMY,PSTANCEX,PSTANCEY,PSWINGX,PSWINGY,VOCMSTANCEX,VOCMSTANCEY
     &,VOCMSWINGX,VOCMSWINGY
      WRITE(22,6020) T,Q1,Q2,(Q3*RADtoDEG),(Q4*RADtoDEG),(Q5*RADtoDEG),(
     &Q6*RADtoDEG),(Q7*RADtoDEG),U1,U2,U3,U4,U5,U6,U7
      WRITE(23,6020) T,RX,RY,VRX,VRY,HTOR,KTOR,ATOR,MTOR,SHTOR,SKTOR
      WRITE(24,6020) T,(Q3*RADtoDEG),(HANG*RADtoDEG),(KANG*RADtoDEG),(AA
     &NG*RADtoDEG),(MANG*RADtoDEG),(SHANG*RADtoDEG),(SKANG*RADtoDEG),U3,
     &HANGVEL,KANGVEL,AANGVEL,MANGVEL,SHANGVEL,SKANGVEL
      WRITE(25,6020) T,KECM,PECM,TE,HZ,PX,PY
      WRITE(26,6020) T,HEACT,HFACT,KEACT,KFACT,AEACT,AFACT,MEACT,MFACT
      WRITE(27,6020) T,HETOR,HEACT,(HECCANG*RADtoDEG),HECCANGVEL,(HESECA
     &NG*RADtoDEG),HESECANGVEL,HFTOR,HFACT,(HFCCANG*RADtoDEG),HFCCANGVEL
     &,(HFSECANG*RADtoDEG),HFSECANGVEL
      WRITE(28,6020) T,KETOR,KEACT,(KECCANG*RADtoDEG),KECCANGVEL,(KESECA
     &NG*RADtoDEG),KESECANGVEL,KFTOR,KFACT,(KFCCANG*RADtoDEG),KFCCANGVEL
     &,(KFSECANG*RADtoDEG),KFSECANGVEL
      WRITE(29,6020) T,AETOR,AEACT,(AECCANG*RADtoDEG),AECCANGVEL,(AESECA
     &NG*RADtoDEG),AESECANGVEL,AFTOR,AFACT,(AFCCANG*RADtoDEG),AFCCANGVEL
     &,(AFSECANG*RADtoDEG),AFSECANGVEL
      WRITE(30,6020) T,METOR,MEACT,(MECCANG*RADtoDEG),MECCANGVEL,(MESECA
     &NG*RADtoDEG),MESECANGVEL,MFTOR,MFACT,(MFCCANG*RADtoDEG),MFCCANGVEL
     &,(MFSECANG*RADtoDEG),MFSECANGVEL
      WRITE(31,6020) T,RX1,RY1,RX2,RY2,RX,RY,GRF,COP

6020  FORMAT( 99(1X, 1PE14.6E3) )

      RETURN
      END


