% Model used to determine the effect of sprint technique on performance.
%
% Segments: 
%   - HAT with varying CoM position from hip joint
%   - Stance leg thigh, shank and foot
%   - Swing leg thigh and shank (combine with foot?)
%
% Torque-driven stance leg and angle-driven swing leg
%
% DA is the swing hip angle, EA swing knee angle
%   Tom Rottier 2020
% ------------------------------------------------------------------------------
DEGREES OFF         % Sometimes get problems with using PI if not specified
% Physical declarations
NEWTONIAN N
BODIES A,B,C,D,E,F      
POINTS O,P{7},CM
% 
% Mathematical declarations
MASS A=MA, B=MB, C=MC, D=MD, E=ME, F=MF
INERTIA A,0,0,IA
INERTIA B,0,0,IB
INERTIA C,0,0,IC
INERTIA D,0,0,ID
INERTIA E,0,0,IE    % Should be inertia of combined shank and foot but small effect
INERTIA F,0,0,IF
VARIABLES Q{6}'		% Generalised coordinates: 2 spring variables, 3 segment angles
VARIABLES U{8}'     % Generalised speeds, U7,U8 auxiliary speeds for swing leg torques
VARIABLES RX,RY		% One spring under toe
VARIABLES KECM,PECM,TE,HZ,PX,PY
VARIABLES HANG,KANG,AANG,HANGVEL,KANGVEL,AANGVEL,SHANG,SKANG,SHANGVEL,SKANGVEL
VARIABLES HETOR,HFTOR,KETOR,KFTOR,AETOR,AFTOR
VARIABLES HTOR,KTOR,ATOR,SHTOR,SKTOR
SPECIFIED HEACT=T,HFACT=T,KEACT=T,KFACT=T,AEACT=T,AFACT=T
SPECIFIED HECCANG=T,HECCANGVEL=T,HESECANG=T,HESECANGVEL=T
SPECIFIED HFCCANG=T,HFCCANGVEL=T,HFSECANG=T,HFSECANGVEL=T
SPECIFIED KECCANG=T,KECCANGVEL=T,KESECANG=T,KESECANGVEL=T
SPECIFIED KFCCANG=T,KFCCANGVEL=T,KFSECANG=T,KFSECANGVEL=T
SPECIFIED AECCANG=T,AECCANGVEL=T,AESECANG=T,AESECANGVEL=T
SPECIFIED AFCCANG=T,AFCCANGVEL=T,AFSECANG=T,AFSECANGVEL=T
CONSTANTS L{7}		% 2 lengths per segment
CONSTANTS K{4}		% Vertical and horizontal stiffness and damping
CONSTANTS G
SPECIFIED DA'',EA''    % Hip and knee angle
SPECIFIED FS''         % Distance from hip to HAT CoM
%
% ------------------------------------------------------------------------------
% Geometry relating unit vectors
SIMPROT(N,F,3,Q3)         % Global/orientation trunk angle
SIMPROT(A,B,3,Q4)         % Shank about ankle
SIMPROT(B,C,3,Q5)         % Thigh about shank
SIMPROT(C,F,3,Q6)         % HAT about thigh
SIMPROT(F,D,3,2*PI-DA)    % Swing thigh about HAT
SIMPROT(D,E,3,EA-PI)      % Swing shank about swing thigh
%
% ------------------------------------------------------------------------------
% Position vectors
P_O_P1>  = Q1*N1> + Q2*N2>		% Origin to toes
P_P1_AO> = L1*A1>			    % Toes to foot CoM
P_P1_P2> = L2*A1>			    % Toes to AJC
P_P2_BO> = L3*B1>			    % AJC to shank CoM
P_P2_P3> = L4*B1>			    % AJC to KJC
P_P3_CO> = L5*C1>			    % KJC to thigh CoM
P_P3_P4> = L6*C1>			    % KJC to HJC
P_P4_DO> = (L6-L5)*D1>          % HJC to swing CoM - prox. to dist. length
P_P4_P5> = L6*D1>               % HJC to swing KJC
P_P5_EO> = (L4-L3)*E1>          % Swing KJC to swing shank CoM - prox. to dist. length
P_P5_P6> = L4*E1>               % Swing KJC to swing AJC
P_P4_FO> = FS*F1>               % HJC to HAT CoM
P_P4_P7> = L7*F1>               % HJC to APEX - does not affect model, just plot
%
% Position of points relative to origin
P_O_AO> = P_O_P1> + P_P1_AO>
P_O_P2> = P_O_P1> + P_P1_P2>
P_O_BO> = P_O_P2> + P_P2_BO>
P_O_P3> = P_O_P2> + P_P2_P3>
P_O_CO> = P_O_P3> + P_P3_CO>
P_O_P4> = P_O_P3> + P_P3_P4>
P_O_DO> = P_O_P4> + P_P4_DO>
P_O_P5> = P_O_P4> + P_P4_P5>
P_O_EO> = P_O_P5> + P_P5_EO>
P_O_P6> = P_O_P5> + P_P5_P6>
P_O_FO> = P_O_P4> + P_P4_FO>
P_O_P7> = P_O_P4> + P_P4_P7>
%
% XY of points
POP1X = DOT(P_O_P1>,N1>)
POP1Y = DOT(P_O_P1>,N2>)
POP2X = DOT(P_O_P2>,N1>)
POP2Y = DOT(P_O_P2>,N2>)
POP3X = DOT(P_O_P3>,N1>)
POP3Y = DOT(P_O_P3>,N2>)
POP4X = DOT(P_O_P4>,N1>)
POP4Y = DOT(P_O_P4>,N2>)
POP5X = DOT(P_O_P5>,N1>)
POP5Y = DOT(P_O_P5>,N2>)
POP6X = DOT(P_O_P6>,N1>)
POP6Y = DOT(P_O_P6>,N2>)
POP7X = DOT(P_O_P7>,N1>)
POP7Y = DOT(P_O_P7>,N2>)
POPFX = DOT(P_O_FO>,N1>)
POPFY = DOT(P_O_FO>,N2>)
%
% Position of CM of system
P_O_CM> = CM(O)
POCMX = DOT(P_O_CM>,N1>)
POCMY = DOT(P_O_CM>,N2>)
%
% ------------------------------------------------------------------------------
% Kinematical differential equations
Q1' = U1
Q2' = U2
Q3' = U3
Q4' = U4
Q5' = U5
Q6' = U6
% ------------------------------------------------------------------------------
% Angular velocities and accelerations
W_F_N> = U3*N3>
W_B_A> = U4*N3>
W_C_B> = U5*N3>
W_F_C> = U6*N3>
W_D_F> = -DA'*N3> + U7*N3>
W_E_D> = EA'*N3> + U8*N3>
ALF_A_N> = DT(W_A_N>,N)
ALF_B_N> = DT(W_B_N>,N)
ALF_C_N> = DT(W_C_N>,N)
ALF_D_N> = DT(W_D_N>,N)
ALF_E_N> = DT(W_E_N>,N)
ALF_F_N> = DT(W_F_N>,N)
%
% ------------------------------------------------------------------------------
% Linear velocities and accelerations
V_O_N> = 0>
V_P1_N> = DT(P_O_P1>,N)
V_AO_N> = DT(P_O_AO>,N)
V_P2_N> = DT(P_O_P2>,N)
V_BO_N> = DT(P_O_BO>,N)
V_P3_N> = DT(P_O_P3>,N)
V_CO_N> = DT(P_O_CO>,N)
V_P4_N> = DT(P_O_P4>,N)
V_DO_N> = DT(P_O_DO>,N)
V_P5_N> = DT(P_O_P5>,N)
V_EO_N> = DT(P_O_EO>,N)
V_P6_N> = DT(P_O_P6>,N)
V_FO_N> = DT(P_O_FO>,N)
V_P7_N> = DT(P_O_P7>,N)
V_CM_N> = DT(P_O_CM>,N)
%
VOP1X = DOT(V_P1_N>,N1>)
VOP1Y = DOT(V_P1_N>,N2>)
VOP2X = DOT(V_P2_N>,N1>)
VOP2Y = DOT(V_P2_N>,N2>)
VOP3X = DOT(V_P3_N>,N1>)
VOP3Y = DOT(V_P3_N>,N2>)
VOP4X = DOT(V_P4_N>,N1>)
VOP4Y = DOT(V_P4_N>,N2>)
VOP5X = DOT(V_P5_N>,N1>)
VOP5Y = DOT(V_P5_N>,N2>)
VOP6X = DOT(V_P6_N>,N1>)
VOP6Y = DOT(V_P6_N>,N2>)
VOP7X = DOT(V_P7_N>,N1>)
VOP7Y = DOT(V_P7_N>,N2>)
VOCMX = DOT(V_CM_N>,N1>)
VOCMY = DOT(V_CM_N>,N2>)
%
A_O_N> = 0>
A_P1_N> = DT(V_P1_N>,N)
A_AO_N> = DT(V_AO_N>,N)
A_P2_N> = DT(V_P2_N>,N)
A_BO_N> = DT(V_BO_N>,N)
A_P3_N> = DT(V_P3_N>,N)
A_CO_N> = DT(V_CO_N>,N)
A_P4_N> = DT(V_P4_N>,N)
A_DO_N> = DT(V_DO_N>,N)
A_P5_N> = DT(V_P5_N>,N)
A_EO_N> = DT(V_EO_N>,N)
A_P6_N> = DT(V_P6_N>,N)
A_FO_N> = DT(V_FO_N>,N)
A_P7_N> = DT(V_P7_N>,N)
%
% ------------------------------------------------------------------------------
% Joint angles and angular velocities
AANG = PI+Q4
KANG = PI-Q5
HANG = PI+Q6
SHANG = DA
SKANG = EA
AANGVEL = U4
KANGVEL = -U5
HANGVEL = U6
SHANGVEL = DA'
SKANGVEL = EA'
%
% ------------------------------------------------------------------------------
% Specified variables
SPECIFIED HEACT=T,HFACT=T,KEACT=T,KFACT=T,AEACT=T,AFACT=T
SPECIFIED HECCANG=T,HECCANGVEL=T,HESECANG=T,HESECANGVEL=T
SPECIFIED HFCCANG=T,HFCCANGVEL=T,HFSECANG=T,HFSECANGVEL=T
SPECIFIED KECCANG=T,KECCANGVEL=T,KESECANG=T,KESECANGVEL=T
SPECIFIED KFCCANG=T,KFCCANGVEL=T,KFSECANG=T,KFSECANGVEL=T
SPECIFIED AECCANG=T,AECCANGVEL=T,AESECANG=T,AESECANGVEL=T
SPECIFIED AFCCANG=T,AFCCANGVEL=T,AFSECANG=T,AFSECANGVEL=T
%
DA   = T^3
DA'  = T^3
DA'' = T^3
EA   = T^3
EA'  = T^3
EA'' = T^3
FS   = T^3
FS'  = T^3
FS'' = T^3
%
% ------------------------------------------------------------------------------
% Generalised forces
% Calculate joint torques in fortran code
HETOR = HEACT*HECCANG*HECCANGVEL*HESECANG*HESECANGVEL*T
HFTOR = HFACT*HFCCANG*HFCCANGVEL*HFSECANG*HFSECANGVEL*T
KETOR = KEACT*KECCANG*KECCANGVEL*KESECANG*KESECANGVEL*T
KFTOR = KFACT*KFCCANG*KFCCANGVEL*KFSECANG*KFSECANGVEL*T
AETOR = AEACT*AECCANG*AECCANGVEL*AESECANG*AESECANGVEL*T
AFTOR = AFACT*AFCCANG*AFCCANGVEL*AFSECANG*AFSECANGVEL*T
%
HTOR = HFTOR-HETOR      % Positive hip extension rotates clockwise
KTOR = KETOR-KFTOR
ATOR = AFTOR-AETOR
%
% Reaction forces
RX = -K1*Q1-K2*U1
RY = -K3*Q2-K4*U2
%
% Apply forces/torques
TORQUE(B/A, ATOR*N3>)
TORQUE(C/B, KTOR*N3>)
TORQUE(F/C, HTOR*N3>)
TORQUE(F/D, SHTOR*N3>)
TORQUE(D/E, SKTOR*N3>)
FORCE(P1,RX*N1>+RY*N2>)
GRAVITY(G*N2>)
% ------------------------------------------------------------------------------
% Energy and momentum of system
KECM = KE()
PECM = -1*(MA+MB+MC+MD+ME+MF)*G*POCMY + 0.5*K1*POP1X^2 + 0.5*K3*POP1Y^2
TE = KECM + PECM
H> = MOMENTUM(ANGULAR,CM)
HZ = DOT(H>,N3>)
P> = MOMENTUM(LINEAR)
PX = DOT(P>,N1>)
PY = DOT(P>,N2>)
%
% ------------------------------------------------------------------------------
% Equations of motion
AUXILIARY[1] = U7
AUXILIARY[2] = U8
CONSTRAIN(AUXILIARY[U7,U8])
ZERO = FR() + FRSTAR()
KANE(SHTOR,SKTOR)
%
% ------------------------------------------------------------------------------
% Inputs
INPUT TINITIAL=0.0,TFINAL=0.2,INTEGSTP=0.001
INPUT ABSERR=1.0E-08,RELERR=1.0E-07
INPUT G=-9.81
INPUT L1=0.142,L2=0.224,L3=0.261,L4=0.453,L5=0.258,L6=0.447,L7=0.4
INPUT IA=0.0096,IB=0.0792,IC=0.2122,ID=0.2122,IE=0.0792,IF=1.489
INPUT MA=1.385,MB=5.271,MC=12.54,MD=12.54,ME=5.271,MF=50.935
%
% Outputs
OUTPUT T,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,POP5X,POP5Y,POP6X,POP6Y,POP7X,POP7Y,POPFX,POPFY,POCMX,POCMY,VOCMX,VOCMY
OUTPUT T,Q1,Q2,Q3,Q4,Q5,Q6,U1,U2,U3,U4,U5,U6
OUTPUT T,RX,RY,HTOR,KTOR,ATOR,SHTOR,SKTOR
OUTPUT T,Q3,HANG,KANG,AANG,SHANG,SKANG,U3,HANGVEL,KANGVEL,AANGVEL,SHANGVEL,SKANGVEL
OUTPUT T,KECM,PECM,TE,HZ,PX,PY
OUTPUT T,HEACT,HFACT,KEACT,KFACT,AEACT,AFACT
OUTPUT T,HETOR,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFTOR,HFACT,HFCCANG,HFCCANGVEL,HFSECANG,HFSECANGVEL
OUTPUT T,KETOR,KEACT,KECCANG,KECCANGVEL,KESECANG,KESECANGVEL,KFTOR,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL
OUTPUT T,AETOR,AEACT,AECCANG,AECCANGVEL,AESECANG,AESECANGVEL,AFTOR,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL
%
% Units
UNITS T=S,TINITIAL=S,TFINAL=S
UNITS L1=M,L2=M,L3=M,L4=M,L5=M,L6=M,L7=M
UNITS POP1X=M,POP1Y=M,POP2X=M,POP2Y=M,POP3X=M,POP3Y=M,POP4X=M,POP4Y=M,POP5X=M,POP5Y=M,POP6X=M,POP6Y=M,POP7X=M,POP7Y=M,POPFX=M,POPFY=M
UNITS POCMX=M,POCMY=M,VOCMX=M/S,VOCMY=M/S
UNITS Q1=M,Q2=M,Q3=DEG,Q4=DEG,Q5=DEG,Q6=DEG,U1=M/S,U2=M/S,U3=DEG/S,U4=DEG/S,U5=DEG/S,U6=DEG/S
UNITS RX=N,RY=N,HTOR=N/M,KTOR=N/M,ATOR=N/M,SHTOR=N/M,SKTOR=N/M
UNITS HANG=DEG,KANG=DEG,AANG=DEG,SHANG=DEG,SKANG=DEG,HANGVEL=DEG/S,KANGVEL=DEG/S,AANGVEL=DEG/S,SHANGVEL=DEG/S,SKANGVEL=DEG/S
UNITS KECM=J,PECM=J,TE=J,HZ=KG.M^2/S,PX=KG.M/S,PY=KG.M/S
UNITS HETOR=N/M,HECCANG=DEG,HECCANGVEL=DEG/S,HESECANG=DEG,HESECANGVEL=DEG/S
UNITS HFTOR=N/M,HFCCANG=DEG,HFCCANGVEL=DEG/S,HFSECANG=DEG,HFSECANGVEL=DEG/S
UNITS KETOR=N/M,KECCANG=DEG,KECCANGVEL=DEG/S,KESECANG=DEG,KESECANGVEL=DEG/S
UNITS KFTOR=N/M,KFCCANG=DEG,KFCCANGVEL=DEG/S,KFSECANG=DEG,KFSECANGVEL=DEG/S
UNITS AETOR=N/M,AECCANG=DEG,AECCANGVEL=DEG/S,AESECANG=DEG,AESECANGVEL=DEG/S
UNITS AFTOR=N/M,AFCCANG=DEG,AFCCANGVEL=DEG/S,AFSECANG=DEG,AFSECANGVEL=DEG/S
UNITS MA=KG,MB=KG,MC=KG,MD=KG,ME=KG,MF=KG
UNITS IA=KG.M^2,IB=KG.M^2,IC=KG.M^2,ID=KG.M^2,IE=KG.M^2,IF=KG.M^2
UNITS G=M/S^2
UNITS K1=N/M,K2=N/M/S^2,K3=N/M,K4=N/M/S^2
%--------------------------------------------------------------
CODE DYNAMICS() 6segSprint.f
