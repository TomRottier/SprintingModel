clear; close all; clc
%% Recalculate joint angles and segment lengths from marker data
load 'C:\Users\tomro\SprintingModel\Experimental data\data.mat';
points = dout.Average.Markers.Data.Avg;
mnames = dout.Average.Markers.Names;
leg = dout.Average.Information.Leg;

% Points
origin = points(:,:,contains(mnames, [leg 'TOE']));
rTOE = points(:,:,contains(mnames, 'RTOE')) - origin; 
lTOE = points(:,:,contains(mnames, 'LTOE')) - origin;
rMTP = points(:,:,contains(mnames, 'RMTP')) - origin; 
lMTP = points(:,:,contains(mnames, 'LMTP')) - origin;
rAJC = points(:,:,contains(mnames, 'RAJC')) - origin; 
lAJC = points(:,:,contains(mnames, 'LAJC')) - origin;
rKJC = points(:,:,contains(mnames, 'RKJC')) - origin; 
lKJC = points(:,:,contains(mnames, 'LKJC')) - origin;
rHJC = points(:,:,contains(mnames, 'RHJC')) - origin; 
lHJC = points(:,:,contains(mnames, 'LHJC')) - origin;
rSJC = points(:,:,contains(mnames, 'RSJC')) - origin; 
lSJC = points(:,:,contains(mnames, 'LSJC')) - origin;
rEJC = points(:,:,contains(mnames, 'REJC')) - origin; 
lEJC = points(:,:,contains(mnames, 'LEJC')) - origin;
rWJC = points(:,:,contains(mnames, 'RWJC')) - origin; 
lWJC = points(:,:,contains(mnames, 'LWJC')) - origin;
LTJC = points(:,:,contains(mnames, 'LTJC')) - origin;
UTJC = points(:,:,contains(mnames, 'UTJC')) - origin;
APEX = points(:,:,contains(mnames, 'APEX')) - origin;
HJC = (rHJC + lHJC) ./ 2;

hatCM = dout.Average.CoM.Data.Avg(:,:,16) - origin;

% dx,dy
rd1 = rMTP - rTOE; ld1 = lMTP - lTOE;
rd2 = rAJC - rMTP; ld2 = lAJC - lMTP;
rd3 = rKJC - rAJC; ld3 = lKJC - lAJC;
rd4 = HJC - rKJC;  ld4 = HJC - lKJC;    % Combined HJC
rd5 = hatCM - rHJC; ld5 = hatCM - lHJC; % HAT CoM

% Segment angles
rFFoot  = atan2d(rd1(:,3), rd1(:,2)); lFFoot  = atan2d(ld1(:,3), ld1(:,2)); 
rRFoot  = atan2d(rd2(:,3), rd2(:,2)); lRFoot  = atan2d(ld2(:,3), ld2(:,2)); 
rShank = atan2d(rd3(:,3), rd3(:,2)); lShank = atan2d(ld3(:,3), ld3(:,2)); 
rThigh = atan2d(rd4(:,3), rd4(:,2)); lThigh = atan2d(ld4(:,3), ld4(:,2)); 
rHAT = atan2d(rd5(:,3), rd5(:,2)); lHAT = atan2d(ld5(:,3), ld5(:,2));

segs = cat(2, rHAT,lHAT,rThigh,lThigh,rShank,lShank,rRFoot,lRFoot);
segsvel = tr_diff(segs, 0.001);

% Joint angles - both hips defined relative to stance side (lHAT)
rMTP = 180 - rFFoot + rRFoot; lMTP = 180 - lFFoot + lRFoot;
rAnkle = 180 - rRFoot + rShank; lAnkle = 180 - lRFoot + lShank;
rKnee = 180 + rShank - rThigh; lKnee = 180 + lShank - lThigh;
rHip = 180 - rThigh + lHAT; lHip = 180 - lThigh + lHAT; 
joints =  cat(2, rHip,lHip,rKnee,lKnee,rAnkle,lAnkle);

for i = 1:size(joints, 2)
    joints(joints(:,i) > 360,i) = joints(joints(:,i) > 360,i) - 360;
end
jointvel = tr_diff(joints, 0.001);

% Segment lengths
rFFootL = mean(sqrt(sum(rd1.^2, 2))); lFFootL = mean(sqrt(sum(ld1.^2, 2)));
rRFootL = mean(sqrt(sum(rd2.^2, 2))); lRFootL = mean(sqrt(sum(ld2.^2, 2)));
rShankL = mean(sqrt(sum(rd3.^2, 2))); lShankL = mean(sqrt(sum(ld3.^2, 2)));
rThighL = mean(sqrt(sum(rd4.^2, 2))); lThighL = mean(sqrt(sum(ld4.^2, 2)));

% Mean across sides
FFootL = mean([rFFootL lFFootL]); 
RFootL = mean([rRFootL lRFootL]); 
ShankL = mean([rShankL lShankL]);
ThighL = mean([rThighL lThighL]);

% Output
n = size(points, 1); m = size(joints, 2) + 3;
time = dout.Average.Time.Absolute;
out = [n m strings(1,m-2); 
      ["Time","rHAT","lHAT","RHip","LHip","RKnee","LKnee","RAnkle","LAnkle"];
      time rHAT lHAT joints]; 
writematrix(out, 'matchingData2.csv');
