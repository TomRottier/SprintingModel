clear; close all; clc
%% Recalculate joint angles and segment lengths from marker data
load 'C:\Users\tomro\SprintingModel\Experimental data\data.mat';
points = dout.Average.Markers.Data.Avg;
mnames = dout.Average.Markers.Names;
hatCM = dout.Average.CoM.Data.Avg(:,:,16);

% Points
rTOE = points(:,:,contains(mnames, 'R_Toe'));
lTOE = points(:,:,contains(mnames, 'L_Toe'));
rAJC = points(:,:,contains(mnames, 'RAJC'));
lAJC = points(:,:,contains(mnames, 'LAJC'));
rKJC = points(:,:,contains(mnames, 'RKJC'));
lKJC = points(:,:,contains(mnames, 'LKJC'));
rHJC = points(:,:,contains(mnames, 'RHJC'));
lHJC = points(:,:,contains(mnames, 'LHJC'));
rSJC = points(:,:,contains(mnames, 'RSJC'));
lSJC = points(:,:,contains(mnames, 'LSJC'));
rEJC = points(:,:,contains(mnames, 'REJC'));
lEJC = points(:,:,contains(mnames, 'LEJC'));
rWJC = points(:,:,contains(mnames, 'RWJC'));
lWJC = points(:,:,contains(mnames, 'LWJC'));
LTJC = points(:,:,contains(mnames, 'LTJC'));
UTJC = points(:,:,contains(mnames, 'UTJC'));
APEX = points(:,:,contains(mnames, 'APEX'));
HJC = (rHJC + lHJC) ./ 2;

% dx,dy
rd1 = rAJC - rTOE; ld1 = lAJC - lTOE;
rd2 = rKJC - rAJC; ld2 = lKJC - lAJC;
rd3 = HJC - rKJC;  ld3 = HJC - lKJC;    % Combined HJC
rd4 = hatCM - rHJC; ld4 = hatCM - lHJC; % HAT CoM

% Segment angles
rFoot  = atan2d(rd1(:,3), rd1(:,2)); lFoot  = atan2d(ld1(:,3), ld1(:,2)); 
rShank = atan2d(rd2(:,3), rd2(:,2)); lShank = atan2d(ld2(:,3), ld2(:,2)); 
rThigh = atan2d(rd3(:,3), rd3(:,2)); lThigh = atan2d(ld3(:,3), ld3(:,2)); 
rHAT = atan2d(rd4(:,3), rd4(:,2)); lHAT = atan2d(ld4(:,3), ld4(:,2));

segs = cat(2, rHAT,lHAT,rThigh,lThigh,rShank,lShank,rFoot,lFoot);
segsvel = tr_diff(segs, 0.001);

% Joint angles
rAnkle = 180 - rFoot + rShank; lAnkle = 180 - lFoot + lShank;
rKnee = 180 + rShank - rThigh; lKnee = 180 + lShank - lThigh;
rHip = 180 - rThigh + rHAT; lHip = 180 - lThigh + lHAT;
joints =  cat(2, rHip,lHip,rKnee,lKnee,rAnkle,lAnkle);

for i = 1:size(joints, 2)
    joints(joints(:,i) > 360,i) = joints(joints(:,i) > 360,i) - 360;
end
jointvel = tr_diff(joints, 0.001);

% Segment lengths
rFootL = mean(sqrt(sum(rd1.^2, 2))); lFootL = mean(sqrt(sum(ld1.^2, 2)));
rShankL = mean(sqrt(sum(rd2.^2, 2))); lShankL = mean(sqrt(sum(ld2.^2, 2)));
rThighL = mean(sqrt(sum(rd3.^2, 2))); lThighL = mean(sqrt(sum(ld3.^2, 2)));

% Mean across sides
FootL = mean([rFootL lFootL]);
ShankL = mean([rShankL lShankL]);
ThighL = mean([rThighL lThighL]);

% Output
n = size(points, 1); m = size(joints, 2) + 3;
time = dout.Average.Time.Absolute;
out = [n m strings(1,m-2); 
      ["Time","rHAT","lHAT","RHip","LHip","RKnee","LKnee","RAnkle","LAnkle"];
      time rHAT lHAT joints]; 
writematrix(out, 'matchingData2.csv');
