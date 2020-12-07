% clear; close all; clc
%% Recalculate joint angles and segment lengths from marker data
load 'C:\Users\tomro\SprintingModel\Experimental data\data.mat';
points = dout.Average.Markers.Data.Avg;
mnames = dout.Average.Markers.Names;
leg = dout.Average.Information.Leg;
hatCM = dout.Average.CoM.Data.Avg(:,:,end-2) - origin;

% Interpolate to 0.0001 timestep
dt = 0.0001;
time = dout.Average.Time.Absolute;
time_10 = (0:dt:time(end))';
points = interp1(time, points, time_10, 'spline');
hatCM = interp1(time, hatCM, time_10, 'spline');

% Points
origin = [0 0 0]; %points(:,:,contains(mnames, [leg 'TOE']));
rTOE = points(:,:,contains(mnames, 'RTOE')) - origin; 
lTOE = points(:,:,contains(mnames, 'LTOE')) - origin;
rMTP = points(:,:,contains(mnames, 'RMTP')) - origin; 
lMTP = points(:,:,contains(mnames, 'LMTP')) - origin;
rHEL = points(:,:,contains(mnames, 'RHEL')) - origin; 
lHEL = points(:,:,contains(mnames, 'LHEL')) - origin;
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


% dx,dy
rd1 = rMTP - rTOE; ld1 = lMTP - lTOE;
rd2 = rAJC - rMTP; ld2 = lAJC - lMTP;
rd3 = rKJC - rAJC; ld3 = lKJC - lAJC;
rd4 = HJC - rKJC;  ld4 = HJC - lKJC;    % Combined HJC
rd5 = hatCM - rHJC; ld5 = hatCM - lHJC; % HAT CoM

% Segment angles
rFFoot  = atan2d(rd1(:,3), rd1(:,2)); rFFoot(rFFoot < 0) = rFFoot(rFFoot < 0) + 360;
lFFoot  = atan2d(ld1(:,3), ld1(:,2)); lFFoot(lFFoot < 0) = lFFoot(lFFoot < 0) + 360;
rRFoot  = atan2d(rd2(:,3), rd2(:,2)); rRFoot(rRFoot < 0) = rRFoot(rRFoot < 0) + 360;
lRFoot  = atan2d(ld2(:,3), ld2(:,2)); lRFoot(lRFoot < 0) = lRFoot(lRFoot < 0) + 360;
rShank = atan2d(rd3(:,3), rd3(:,2)); %rShank(rShank < 0) = rShank(rShank < 0) + 360;
lShank = atan2d(ld3(:,3), ld3(:,2)); %lShank(lShank < 0) = lShank(lShank < 0) + 360;
rThigh = atan2d(rd4(:,3), rd4(:,2)); 
lThigh = atan2d(ld4(:,3), ld4(:,2)); 
rHAT = atan2d(rd5(:,3), rd5(:,2)); 
lHAT = atan2d(ld5(:,3), ld5(:,2));

segs = cat(2, rHAT,lHAT,rThigh,lThigh,rShank,lShank,rRFoot,lRFoot,rFFoot,lFFoot);
segsvel = tr_diff(segs, dt);

% Joint angles - both hips defined relative to stance side (lHAT)
rMTPa = 180 - rFFoot + rRFoot; lMTPa = 180 - lFFoot + lRFoot;
rAnkle = 180 - rRFoot + rShank; lAnkle = 180 - lRFoot + lShank;
rKnee = 180 + rShank - rThigh; lKnee = 180 + lShank - lThigh;
rHip = 180 - rThigh + lHAT; lHip = 180 - lThigh + lHAT; 
joints =  cat(2, rHip,lHip,rKnee,lKnee,rAnkle,lAnkle,rMTPa,lMTPa);
jointvel = tr_diff(joints, dt);

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
out = [n m strings(1,m-2); 
      ["Time","rHAT","lHAT","RHip","LHip","RKnee","LKnee","RAnkle","LAnkle","RMTP","LMTP"];
      time_10 rHAT lHAT joints]; 
writematrix(out, 'matchingData2_10.csv');

%%
set(figure(1),'WindowStyle','docked'); hold on; cla
axis equal
for i = 1%:1:length(points)
    % Right leg
    line([rTOE(i,2) rMTP(i,2) rHEL(i,2) rAJC(i,2) rKJC(i,2) rHJC(i,2)], ...
         [rTOE(i,3) rMTP(i,3) rHEL(i,3) rAJC(i,3) rKJC(i,3) rHJC(i,3)],....
         'Color', 'r')
    line([rMTP(i,2) rAJC(i,2)], [rMTP(i,3) rAJC(i,3)], 'Color', 'r')
    % Left leg
    line([lTOE(i,2) lMTP(i,2) lHEL(i,2) lAJC(i,2) lKJC(i,2) lHJC(i,2)], ...
         [lTOE(i,3) lMTP(i,3) lHEL(i,3) lAJC(i,3) lKJC(i,3) lHJC(i,3)],....
         'Color', 'r')
    line([lMTP(i,2) lAJC(i,2)], [lMTP(i,3) lAJC(i,3)], 'Color', 'r')
    % Trunk
    line([HJC(i,2) LTJC(i,2) UTJC(i,2) APEX(i,2)], ...
         [HJC(i,3) LTJC(i,3) UTJC(i,3) APEX(i,3)], 'Color', 'r')

    drawnow
end