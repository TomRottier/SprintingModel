##### Constant parameters
l1 = 0.050; l2 = 0.085; l3 = 0.121; l4 = 0.076; l5 = 0.222; l6 = 0.139; l7 = 0.261; l8 = 0.453; l9 = 0.258
l10 = 0.447; l12 = 0.275
ma = 0.239; mb = 1.385; mc = 5.271; md = 12.54; me = 12.54; mf = 6.656

##### Initial conditions
q1 = 0.0; q2 = 0.0; trunk_ang = 81.451; mtp_ang = 143.563; ankle_ang = 120.50; knee_ang = 157.074; hip_ang = 144.11
q1 = q1 + l2 * cos(q3 - q4 - q5 - q6 - q7)
q2 = q2 + l2 * sin(q3 - q4 - q5 - q6 - q7)

vocmx = 1.91; vocmy = -3.96; trunk_angvel = -24.935; mtp_angvel = 1064.1; ankle_angvel = -650.36; knee_angvel = -160.13
hip_angvel = 429.803
u8 = 0
u9 = 0
# Sprinter
swhip_ang = 160.84; swknee_ang = 68.60; swhip_angvel = -640.2; swknee_angvel = 613.8
# Teamsport
# swhip_ang = 200.89; swknee_ang = 52.60; swhip_angvel = -310.83; swknee_angvel = -951.61

#### Convert to generalised coordinates and speeds
q3 = deg2rad(q3); q4 = deg2rad(q4); q5 = deg2rad(q5); q6 = deg2rad(q6); q7 = deg2rad(q7)
u3 = deg2rad(u3); u4 = deg2rad(u4); u5 = deg2rad(u5); u6 = deg2rad(u6); u7 = deg2rad(u7)

pop2x = q1; pop2y = q2; mang  = q4; aang  = q5; kang  = q6; hang  = q7 ;
mangvel = u4; aangvel = u5; kangvel = u6; hangvel = u7; vocmx = u1; vocmy = u2
q4 = mang; q5 = aang - pi; q6 = pi - kang; q7 = hang - pi
u4 = mangvel; u5 = aangvel; u6 = -kangvel; u7 = hangvel

q1 = q1 + l2 * cos(q3 - q4 - q5 - q6 - q7)
q2 = q2 + l2 * sin(q3 - q4 - q5 - q6 - q7)

u1 = vocmx + 0.5 * (2 * mg * gs * sin(q3) * u3 + 2 * (l10 * me + l10 * mf + l10 * mg + l9 * md) * sin(q3 - q7) * (u3 - u7) + 2 * (l10 * mf + me * (l10 - l9)) * sin(ea - q3) * (eap - u3 - u8) + l3 * mb * sin(footang + q3 - q5 - q6 - q7) * (u3 - u5 - u6 - u7) + 2 * (l7 * mc + l8 * md + l8 * me + l8 * mf + l8 * mg) * sin(q3 - q6 - q7) * (u3 - u6 - u7) + (l4 * mb + 2 * l6 * mc + 2 * l6 * md + 2 * l6 * me + 2 * l6 * mf + 2 * l6 * mg) * sin(q3 - q5 - q6 - q7) * (u3 - u5 - u6 - u7) - 2 * mg * gsp * cos(q3) - 2 * l12 * mf * sin(ea - fa - q3) * (eap - fap - u3 - u8 - u9) - 2 * (l1 * ma + l2 * mb + l2 * mc + l2 * md + l2 * me + l2 * mf + l2 * mg) * sin(q3 - q4 - q5 - q6 - q7) * (u3 - u4 - u5 - u6 - u7)) / (ma + mb + mc + md + me + mf + mg)
u2 = vocmy - 0.5 * (2 * mg * gsp * sin(q3) + 2 * mg * gs * cos(q3) * u3 + 2 * (l10 * me + l10 * mf + l10 * mg + l9 * md) * cos(q3 - q7) * (u3 - u7) + 2 * l12 * mf * cos(ea - fa - q3) * (eap - fap - u3 - u8 - u9) + l3 * mb * cos(footang + q3 - q5 - q6 - q7) * (u3 - u5 - u6 - u7) + 2 * (l7 * mc + l8 * md + l8 * me + l8 * mf + l8 * mg) * cos(q3 - q6 - q7) * (u3 - u6 - u7) + (l4 * mb + 2 * l6 * mc + 2 * l6 * md + 2 * l6 * me + 2 * l6 * mf + 2 * l6 * mg) * cos(q3 - q5 - q6 - q7) * (u3 - u5 - u6 - u7) - 2 * (l10 * mf + me * (l10 - l9)) * cos(ea - q3) * (eap - u3 - u8) - 2 * (l1 * ma + l2 * mb + l2 * mc + l2 * md + l2 * me + l2 * mf + l2 * mg) * cos(q3 - q4 - q5 - q6 - q7) * (u3 - u4 - u5 - u6 - u7)) / (ma + mb + mc + md + me + mf + mg)



pswingy = @. (me + mf) * u2 + l10 * (me + mf) * cos(q3 - q7) * (u3 - u7) + l8 * (me + mf) * cos(q3 - q6 - q7) * (u3 - u6 - u7) + l12 * mf * cos(ea - fa - q3) * (eap - fap - u3 - u8 - u9) + l6 * (me + mf) * cos(q3 - q5 - q6 - q7) * (u3 - u5 - u6 - u7) - (l10 * mf + me * (l10 - l9)) * cos(ea - q3) * (eap - u3 - u8) - l2 * (me + mf) * cos(q3 - q4 - q5 - q6 - q7) * (u3 - u4 - u5 - u6 - u7)
