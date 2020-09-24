{* Vicon bodylanguage model for sprinting model evaluation

Mostly copied from Glen's assessment model
Tom Rottier 2020
*}
{* -------------------------------------------------------------------------------------*}
{* Macros *}

{*Draws segment LCS in Vicon*}
macro DRAWBONE(Bone,BoneLabel)
    BoneLabel#O = 0(Bone)+{0,0,0}*Attitude(Bone)
    BoneLabel#P = BoneLabel#O+100*3(Bone)
    BoneLabel#A = BoneLabel#O+100*1(Bone)
    BoneLabel#L = BoneLabel#O+100*2(Bone)
    OUTPUT(BoneLabel#O,BoneLabel#P,BoneLabel#A,BoneLabel#L)
endmacro

{* -------------------------------------------------------------------------------------*}
{* Create joint centres *}

RMTP = (R_1MTP + R_5MTP) / 2
LMTP = (L_1MTP + L_5MTP) / 2
RAJC = (R_AnkleL + R_AnkleM) / 2
LAJC = (L_AnkleL + L_AnkleM) / 2
RKJC = (R_KneeL + R_KneeM) / 2
LKJC = (L_KneeL + L_KneeM) / 2

RWJC = (R_WristUS + R_WristRS) / 2
LWJC = (L_WristUS + L_WristRS) / 2
REJC = (R_ElbowL + R_ElbowM) / 2
LEJC = (L_ElbowL + L_ElbowM) / 2
RSJC = (R_AS + R_PS) / 2
LSJC = (L_AS + L_PS) / 2

LTJC = (Xiphoid + T10) / 2
UTJC = (Sternum + C7) / 2
APEX = (HeadRF + HeadLF + HeadRB + HeadLB) / 4

{* Calculate hip joint centre from Davis model *}
{*Create a temporary pelvis segment*}
Sacrum = (LPSIS + RPSIS) / 2
PEL = (LASIS + RASIS)  /2
R_iliac = (RPSIS + RASIS) / 2
L_iliac = (LPSIS + LASIS) / 2
Pelvis = [PEL, PEL-Sacrum, R_iliac-L_iliac, yzx]

{*Take HJC from static .mp file (calculated from static trial)*}
RHJC = $%temp2_RHJC*Pelvis
LHJC = $%temp2_LHJC*Pelvis

HJC = (RHJC + LHJC) / 2
SJC = (RSJC + LSJC) / 2

OUTPUT(RMTP,LMTP,RAJC,LAJC,RKJC,LKJC,RHJC,LHJC,RSJC,LSJC,REJC,LEJC,RWJC,LWJC,LTJC,UTJC,APEX,HJC,SJC)

{* -------------------------------------------------------------------------------------*}
{* Create segments *}
{* 'y' is direction of travel, 'x' points to left side of body *}
{* Toe markers fall off midway through *}


Pelvis = HJC + ATTITUDE(Pelvis)
HAT    = [HJC,SJC-HJC,LHJC-RHJC,zyx]
RThigh = [RKJC, RHJC-RKJC, R_KneeM-R_KneeL, zyx]
LThigh = [LKJC, LHJC-LKJC, L_KneeL-L_KneeM, zyx]
RShank = [RAJC, RKJC-RAJC, R_AnkleM-R_AnkleL, zyx] 
LShank = [LAJC, LKJC-LAJC, L_AnkleL-L_AnkleM, zyx]
RFoot  = [RMTP, RAJC-RMTP, R_1MTP-R_5MTP, zyx]
LFoot  = [LMTP, LAJC-LMTP, L_5MTP-L_1MTP, zyx]
LTrunk = [HJC, LTJC-HJC, LHJC-RHJC, zyx]
UTrunk = [LTJC, UTJC-LTJC, Sternum-C7, zxy]
Head   = [UTJC, APEX-UTJC,HeadLF-HeadRF, zyx]

{*DrawBone(Pelvis,PLV)*}
{*DrawBone(HAT,TRNK)*}
DrawBone(RThigh,RTH)
DrawBone(LThigh,LTH)
DrawBone(RShank,RSH)
DrawBone(LShank,LSH)
DrawBone(RFoot,RFT)
DrawBone(LFoot,LFT)
DrawBone(LTrunk,LTRNK)
DrawBone(UTrunk,UTRNK)
DrawBone(Head,HD)

{* -------------------------------------------------------------------------------------*}
{* Calculate joint angles *}
{* Only checked correct definition for flexion/extension angles *}

{*Pelvis angle (root segment)*}
PelvisAngles = -<Pelvis,xyz>(-1)
PelvisAngles = <90+1(PelvisAngles),2(PelvisAngles),3(PelvisAngles)>

HATAngles = -<HAT,xyz>(-1)
HATAngles = <90+1(HATAngles),2(HATAngles),3(HATAngles)>

LTrunkAngles = -<LTrunk,xyz>(-1)
LTrunkAngles = <90+1(LTrunkAngles), 2(LTrunkAngles),3(LTrunkAngles)>

{*Hip angles (parent: pelvis)*}
RHipAngles = -<RThigh,Pelvis,xyz>
LHipAngles = -<LThigh,Pelvis,xyz>
RHipAngles = <180+1(RHipAngles),2(RHipAngles),3(RHipAngles)>
LHipAngles = <180+1(LHipAngles),2(LHipAngles),3(LHipAngles)>

{*Hip angles (parent: Lower trunk)*}
RHipAngles = -<RThigh,LTrunk,xyz>
LHipAngles = -<LThigh,LTrunk,xyz>
RHipAngles = <180+1(RHipAngles),2(RHipAngles),3(RHipAngles)>
LHipAngles = <180+1(LHipAngles),2(LHipAngles),3(LHipAngles)>

{*Knee angles (parent: thigh)*}
RKneeAngles = -<RShank,RThigh,xyz>(-1)
LKneeAngles = -<LShank,LThigh,xyz>(-1)
RKneeAngles = <180+1(RKneeAngles),2(RKneeAngles),3(RKneeAngles)>
LKneeAngles = <180+1(LKneeAngles),2(LKneeAngles),3(LKneeAngles)>

{*Ankle angles (parent: shank)*}
RAnkleAngles = -<RFoot,RShank,xyz>
LAnkleAngles = -<LFoot,LShank,xyz>
RAnkleAngles = <180+1(RAnkleAngles),2(RAnkleAngles),3(RAnkleAngles)>
LAnkleAngles = <180+1(LAnkleAngles),2(LAnkleAngles),3(LAnkleAngles)>

OUTPUT(PelvisAngles,HATAngles,RHipAngles,LHipAngles,RKneeAngles,LKneeAngles,RAnkleAngles,LAnkleAngles)

{*Global segment angles for matching simulation*}
RThighAngles = -<RThigh,xyz>(-1)
RThighAngles = <90+1(RThighAngles),2(RThighAngles),3(RThighAngles)>
LThighAngles = -<LThigh,xyz>(-1)
LThighAngles = <90+1(LThighAngles),2(LThighAngles),3(LThighAngles)>

RShankAngles = -<RShank,xyz>(-1)
RShankAngles = <90+1(RShankAngles),2(RShankAngles),3(RShankAngles)>
LShankAngles = -<LShank,xyz>(-1)
LShankAngles = <90+1(LShankAngles),2(LShankAngles),3(LShankAngles)>

RFootAngles  = -<RFoot, xyz>(-1)
RFootAngles  = <90+1(RFootAngles),2(RFootAngles),3(RFootAngles)>
LFootAngles  = -<LFoot, xyz>(-1)
LFootAngles  = <90+1(LFootAngles),2(LFootAngles),3(LFootAngles)>

OUTPUT(RThighAngles,LThighAngles,RShankAngles,LShankAngles,RFootAngles,LFootAngles)

{* -------------------------------------------------------------------------------------*}
{* Joint kinetics *}

BodyMass = 91.1

AnthropometricData
    DefaultHAT   0.449 0.491 0.191 0.166
    DefaultThigh 0.138 0.577 0.130 0.067
    DefaultShank 0.058 0.576 0.123 0.046
    DefaultFoot  0.013 0.547 0.072 0.017
EndAnthropometricData


HAT = [HAT, DefaultHAT]
RThigh = [RThigh, HAT, RHJC, DefaultThigh]
LThigh = [LThigh, HAT, LHJC, DefaultThigh]
RShank = [RShank, RThigh, RKJC, DefaultShank]
LShank = [LShank, LThigh, LKJC, DefaultShank]
RFoot = [RFoot, RShank, RAJC, DefaultFoot]
LFoot = [LFoot, LShank, LAJC, DefaultFoot]

ForceThreshold = 50
DistanceThreshold = 100
VelocityThreshold = 10000

RAnkleR = REACTION(RFoot)
LAnkleR = REACTION(LFoot)
RKneeR = REACTION(RShank)
LKneeR = REACTION(LShank)
RHipR = REACTION(RThigh)
LHipR = REACTION(LThigh)

RHipM = 2(RHipR)(-1)(-2)(-3)
LHipM = 2(LHipR)(-1)
RKneeM = 2(RKneeR)(-2)(-3)
LKneeM = 2(LKneeR)
RAnkleM = 2(RAnkleR)(-1)(-2)(-3)
LAnkleM = 2(LAnkleR)(-1)

OUTPUT(RHipM,LHipM,RKneeM,LKneeM,RAnkleM,LAnkleM)