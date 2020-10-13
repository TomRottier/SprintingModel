% Model used to determine the effect of sprint technique on performance.
% Stance leg 3 segments (thigh, shank, foot) + 2 point masses representing the 
% mass of the upper body (head, arms and trunk) and the swing leg.
% Both point masses position (and derivatives) is specified at each timepoint
% Mono-articular torque generators at hip, knee and ankle with SEC
% Inertia data from McErlain-Naylor (2017) and torque parameters from Allen 
% (2009) and scaled to by bodymass to this particpant.
%
%   Tom Rottier 2020
% ------------------------------------------------------------------------------
% Physical declarations
NEWTONIAN N
BODIES A,B,C        % Foot, shank and thigh
PARTICLES D,E       % Swing leg and HAT point masses
POINTS O,P{4},CM
% 
% Mathematical declarations
MASS A = MA, B=MB, C=MC, D=MD, E=ME
INERTIA A,0,0,IA
INERTIA B,0,0,IB
INERTIA C,0,0,IC
VARIABLES Q{5}'		% 2 spring variables, 3 segment angles
VARIABLES U{5}'
VARIABLES RX,RY		% One spring under toe
VARIABLES KECM,PECM,TE,HZ,PX,PY
VARIABLES HANG,KANG,AANG,HANGVEL,KANGVEL,AANGVEL
VARIABLES HETOR,HFTOR,KETOR,KFTOR,AETOR,AFTOR
VARIABLES HTOR,KTOR,ATOR
SPECIFIED HEACT=T,HFACT=T,KEACT=T,KFACT=T,AEACT=T,AFACT=T
SPECIFIED HECCANG=T,HECCANGVEL=T,HESECANG=T,HESECANGVEL=T
SPECIFIED HFCCANG=T,HFCCANGVEL=T,HFSECANG=T,HFSECANGVEL=T
SPECIFIED KECCANG=T,KECCANGVEL=T,KESECANG=T,KESECANGVEL=T
SPECIFIED KFCCANG=T,KFCCANGVEL=T,KFSECANG=T,KFSECANGVEL=T
SPECIFIED AECCANG=T,AECCANGVEL=T,AESECANG=T,AESECANGVEL=T
SPECIFIED AFCCANG=T,AFCCANGVEL=T,AFSECANG=T,AFSECANGVEL=T
CONSTANTS L{6}		% 2 lengths per segment
CONSTANTS K{4}		% Vertical and horizontal stiffness and damping
CONSTANTS G
SPECIFIED DX'',DY'',EX'',EY''
%
% ------------------------------------------------------------------------------
% Geometry relating unit vectors
SIMPROT(N,A,3,Q3)               % Foot angle
SIMPROT(N,B,3,Q4)               % Shank angle
SIMPROT(N,C,3,Q5)               % Thigh angle
%
% ------------------------------------------------------------------------------
% Position vectors
P_O_P1>  = Q1*N1> + Q2*N2>		% From origin to end of spring
P_P1_AO> = L1*A1>			    % From end of spring to foot CM
P_P1_P2> = L2*A1>			    % From end of spring to AJC
P_P2_BO> = L3*B1>			    % From AJC to shank CM
P_P2_P3> = L4*B1>			    % From AJC to KJC
P_P3_CO> = L5*C1>			    % From KJC to thigh CM
P_P3_P4> = L6*C1>			    % From KJC to HJC
P_P4_D>  = DX*N1> + DY*N2>      % From HJC to swing leg point mass
P_P4_E>  = EX*N1> + EY*N2>		% From HJC to HAT point mass
%
% Position of points relative to origin
P_O_AO> = P_O_P1> + P_P1_AO>
P_O_P2> = P_O_P1> + P_P1_P2>
P_O_BO> = P_O_P2> + P_P2_BO>
P_O_P3> = P_O_P2> + P_P2_P3>
P_O_CO> = P_O_P3> + P_P3_CO>
P_O_P4> = P_O_P3> + P_P3_P4>
P_O_D>  = P_O_P4> + P_P4_D>
P_O_E>  = P_O_P4> + P_P4_E>
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
PODX  = DOT(P_O_D>, N1>)
PODY  = DOT(P_O_D>, N2>)
POEX  = DOT(P_O_E>, N1>)
POEY  = DOT(P_O_E>, N2>)
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
%
% ------------------------------------------------------------------------------
% Angular velocities and accelerations
W_A_N> = Q3'*N3>
W_B_N> = Q4'*N3>
W_C_N> = Q5'*N3>
ALF_A_N> = DT(W_A_N>,N)
ALF_B_N> = DT(W_B_N>,N)
ALF_C_N> = DT(W_C_N>,N)
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
V_D_N>  = DT(P_O_D>,N)
V_E_N>  = DT(P_O_E>,N)
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
VODX  = DOT(V_D_N>, N1>)
VODY  = DOT(V_D_N>, N2>)
VOEX  = DOT(V_E_N>, N1>)
VOEY  = DOT(V_E_N>, N2>)
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
A_D_N>  = DT(V_D_N>,N)
A_E_N>  = DT(V_E_N>,N)
%
% ------------------------------------------------------------------------------
% Joint angles and angular velocities
AANG = PI-Q3+Q4
KANG = PI-Q5+Q4
%HANG = PI-Q5+ATAN((POEY-POP4Y)/(POEX-POP4X))       % Relative to HAT point mass
HANG = PI-Q5+PI/2                                   % Relative to vertical
AANGVEL = U4-U3
KANGVEL = U4-U5
HANGVEL = -U5
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
DX   = T^3
DX'  = T^3
DX'' = T^3
DY   = T^3
DY'  = T^3
DY'' = T^3
EX   = T^3
EX'  = T^3
EX'' = T^3
EY   = T^3
EY'  = T^3
EY'' = T^3
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
ATOR = AFTOR-AETOR
KTOR = KETOR-KFTOR
HTOR = HFTOR-HETOR
%
% Reaction forces
RX = -K1*Q1-K2*U1
RY = -K3*Q2-K4*U2
%
% Apply forces/torques
TORQUE(B/A, ATOR*B3>)
TORQUE(C/B, KTOR*C3>)
TORQUE(C, HTOR*C3>)
FORCE(P1,RX*N1>+RY*N2>)
GRAVITY(G*N2>)
%
% ------------------------------------------------------------------------------
% Energy and momentum of system
KECM = KE()
PECM = -1*(MA+MB+MC+MD+ME)*G*POCMY + 0.5*K1*POP1X^2 + 0.5*K3*POP1Y^2
TE = KECM + PECM
H> = MOMENTUM(ANGULAR,CM)
HZ = DOT(H>,N3>)
P> = MOMENTUM(LINEAR)
PX = DOT(P>,N1>)
PY = DOT(P>,N2>)
%
% ------------------------------------------------------------------------------
% Equations of motion
ZERO = FR() + FRSTAR()
KANE()
%
% ------------------------------------------------------------------------------
% Inputs
INPUT TINITIAL=0.0,TFINAL=0.2,INTEGSTP=0.001
INPUT ABSERR=1.0E-08,RELERR=1.0E-07
INPUT G=-9.81
INPUT L1=0.142,L2=0.224,L3=0.261,L4=0.453,L5=0.258,L6=0.447
INPUT IA=0.0096,IB=0.0792,IC=0.2122,MA=1.385,MB=5.271,MC=12.54,MD=19.196,ME=50.935
%
% Outputs
OUTPUT T,POP1X,POP1Y,POP2X,POP2Y,POP3X,POP3Y,POP4X,POP4Y,PODX,PODY,POEX,POEY,POCMX,POCMY,VOCMX,VOCMY
OUTPUT T,Q1,Q2,Q3,Q4,Q5,U1,U2,U3,U4,U5
OUTPUT T,RX,RY,HTOR,KTOR,ATOR
OUTPUT T,HANG,KANG,AANG,HANGVEL,KANGVEL,AANGVEL
OUTPUT T,KECM,PECM,TE,HZ,PX,PY
OUTPUT T,HEACT,HFACT,KEACT,KFACT,AEACT,AFACT
OUTPUT T,HETOR,HEACT,HECCANG,HECCANGVEL,HESECANG,HESECANGVEL,HFTOR,HFACT,HFCCANG,HFCCANGVEL,HFSECANG,HFSECANGVEL
OUTPUT T,KETOR,KEACT,KECCANG,KECCANGVEL,KESECANG,KESECANGVEL,KFTOR,KFACT,KFCCANG,KFCCANGVEL,KFSECANG,KFSECANGVEL
OUTPUT T,AETOR,AEACT,AECCANG,AECCANGVEL,AESECANG,AESECANGVEL,AFTOR,AFACT,AFCCANG,AFCCANGVEL,AFSECANG,AFSECANGVEL
%
% Units
UNITS T=S,TINITIAL=S,TFINAL=S
UNITS L1=M,L2=M,L3=M,L4=M,L5=M,L6=M
UNITS POP1X=M,POP1Y=M,POP2X=M,POP2Y=M,POP3X=M,POP3Y=M,POP4X=M,POP4Y=M,PODX=M,PODY=M,POEX=M,POEY=M
UNITS POCMX=M,POCMY=M,VOCMX=M/S,VOCMY=M/S
UNITS Q1=M,Q2=M,Q3=DEG,Q4=DEG,Q5=DEG,U1=M/S,U2=M/S,U3=DEG/S,U4=DEG/S,U5=DEG/S
UNITS RX=N,RY=N,HTOR=N/M,KTOR=N/M,ATOR=N/M
UNITS HANG=DEG,KANG=DEG,AANG=DEG,HANGVEL=DEG/S,KANGVEL=DEG/S,AANGVEL=DEG/S
UNITS KECM=J,PECM=J,TE=J,HZ=KG.M^2/S,PX=KG.M/S,PY=KG.M/S
UNITS HETOR=N/M,HECCANG=DEG,HECCANGVEL=DEG/S,HESECANG=DEG,HESECANGVEL=DEG/S
UNITS HFTOR=N/M,HFCCANG=DEG,HFCCANGVEL=DEG/S,HFSECANG=DEG,HFSECANGVEL=DEG/S
UNITS KETOR=N/M,KECCANG=DEG,KECCANGVEL=DEG/S,KESECANG=DEG,KESECANGVEL=DEG/S
UNITS KFTOR=N/M,KFCCANG=DEG,KFCCANGVEL=DEG/S,KFSECANG=DEG,KFSECANGVEL=DEG/S
UNITS AETOR=N/M,AECCANG=DEG,AECCANGVEL=DEG/S,AESECANG=DEG,AESECANGVEL=DEG/S
UNITS AFTOR=N/M,AFCCANG=DEG,AFCCANGVEL=DEG/S,AFSECANG=DEG,AFSECANGVEL=DEG/S
UNITS MA=KG,MB=KG,MC=KG,MD=KG,ME=KG,IA=KG.M^2,IB=KG.M^2,IC=KG.M^2
UNITS G=M/S^2
UNITS K1=N/M,K2=N/M/S^2,K3=N/M,K4=N/M/S^2
%--------------------------------------------------------------
CODE DYNAMICS() sprintmodel.f
