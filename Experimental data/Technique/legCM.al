% Autolev code to generate an equation for the CoM of a three segment leg
%
%   Tom Rottier 2020
%-------------------------------------------------------------------------------
% Physical declarations
newtonian n
bodies a,b,c
points o,p{4},cm
%
% Mathematical declarations
mass a=ma, b=mb, c=mc
inertia a,0,0,Ia
inertia b,0,0,Ib
inertia c,0,0,Ic
variables q{3}
constants l{6}
%
%-------------------------------------------------------------------------------
% Geometry relating unit vectors
simprot(n,a,3,q1)
simprot(n,b,3,q2)
simprot(n,c,3,q3)
%
%-------------------------------------------------------------------------------
% Position vectors
p_o_p1> = 0>
p_p1_ao> = l1*a1>
p_p1_p2> = l2*a1>
p_p2_bo> = l3*b1>
p_p2_p3> = l4*b1>
p_p3_co> = l5*c1>
p_p3_p4> = l6*c1>
%
% Positions relative to origin
p_o_ao> = p_o_p1>+p_p1_ao>
p_o_p2> = p_o_p1>+p_p1_p2>
p_o_bo> = p_o_p2>+p_p2_bo>
p_o_p3> = p_o_p2>+p_p2_p3>
p_o_co> = p_o_p3>+p_p3_co>
p_o_p4> = p_o_p3>+p_p3_p4>
%
% CoM of system about hip joint (p4)
p_p4_cm> = cm(p4)
cmx = dot(p_p4_cm>,n1>)
cmy = dot(p_p4_cm>,n2>)
