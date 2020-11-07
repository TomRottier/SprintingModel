load data.mat
points = dout.Average.Markers.Data.Avg;
mnames = dout.Average.Markers.Names;
leg = dout.Average.Information.Leg;

% Points
origin = points(:,:,contains(mnames, [leg 'TOE']));
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

% CoM
WBCM = dout.Average.CoM.Data.Avg(:,:,1)- origin;
hatCM = dout.Average.CoM.Data.Avg(:,:,end-2) - origin;
RCM = dout.Average.CoM.Data.Avg(:,:,end-1)- origin;
LCM = dout.Average.CoM.Data.Avg(:,:,end)- origin;
% hatCM = hatCM.data(:,[1 2]) + HJC(:,[2 3]);
% swingCM = swingCM.data(:,[1 2]) + HJC(:,[2 3]);

set(figure(),'WindowStyle','docked'); cla
xlim([-1 1]); ylim([-.1 2]); hold on
for i = 1%:5:length(points)
    cla
    % Right leg
    line([rTOE(i,2) rMTP(i,2) rHEL(i,2) rAJC(i,2) rKJC(i,2) rHJC(i,2)], ...
         [rTOE(i,3) rMTP(i,3) rHEL(i,3) rAJC(i,3) rKJC(i,3) rHJC(i,3)])
    line([rMTP(i,2) rAJC(i,2)], [rMTP(i,3) rAJC(i,3)])
    % Left leg
    line([lTOE(i,2) lMTP(i,2) lHEL(i,2) lAJC(i,2) lKJC(i,2) lHJC(i,2)], ...
         [lTOE(i,3) lMTP(i,3) lHEL(i,3) lAJC(i,3) lKJC(i,3) lHJC(i,3)])
    line([lMTP(i,2) lAJC(i,2)], [lMTP(i,3) lAJC(i,3)])
    % Trunk
    line([HJC(i,2) LTJC(i,2) UTJC(i,2) APEX(i,2)], ...
         [HJC(i,3) LTJC(i,3) UTJC(i,3) APEX(i,3)])
    % Right arm
    line([rSJC(i,2) rEJC(i,2) rWJC(i,2)], ...
         [rSJC(i,3) rEJC(i,3) rWJC(i,3)])
    % Left arm
    line([lSJC(i,2) lEJC(i,2) lWJC(i,2)], ...
         [lSJC(i,3) lEJC(i,3) lWJC(i,3)])
    % Whole-body CoM
    plot(WBCM(i,2), WBCM(i,3), 'ko', 'MarkerSize', 4) 
    plot(hatCM(i,2), hatCM(i,3), 'kx')
    plot(RCM(i,2), RCM(i,3), 'kx')
    plot(LCM(i,2), LCM(i,3), 'kx')
    drawnow
end

