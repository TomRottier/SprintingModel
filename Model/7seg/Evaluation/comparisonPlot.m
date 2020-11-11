% clear; close all; clc
%% Experimental data
load 'C:\Users\tomro\SprintingModel\Experimental data\data.mat'
points = dout.Average.Markers.Data.Avg;
mnames = dout.Average.Markers.Names;
mnames = strrep(mnames, '_', '');       % Remove all '_' from marker names
leg = dout.Average.Information.Leg;

% Experimental data relative to toe
origin = [points(:,[1 2],contains(mnames,[leg 'Toe']))...
          repelem(points(1,3,contains(mnames, [leg 'Toe'])), length(points), 1)];
% origin = points(:,:,contains(mnames, [leg 'Toe']));

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
WBCM = dout.Average.CoM.Data.Avg(:,:,1) - origin;
hatCM = dout.Average.CoM.Data.Avg(:,:,end-2) - origin;
RCM = dout.Average.CoM.Data.Avg(:,:,end-1) - origin;
LCM = dout.Average.CoM.Data.Avg(:,:,end) - origin;

%% Simulation data
pname = 'C:\Users\tomro\SprintingModel\Model\7Seg\Evaluation\';
fname = '7segSprint.1';
data = importdata([pname fname], ' ', 8); data = data.data;

% Simulation data relative to toe
data(:,2:2:end) = data(:,2:2:end) - data(1,2);
data(:,3:2:end) = data(:,3:2:end) - data(1,3);

%% Plot
n = length(data);
set(figure(1),'WindowStyle','docked'); cla
set(figure(1),'DefaultLineLineWidth', 1.5)
xlim([-.8 1.2]); ylim([-.2 2]); hold on
for i = 1:1:n
    cla
    % Experimental
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
    % Right arm
    line([rSJC(i,2) rEJC(i,2) rWJC(i,2)], ...
         [rSJC(i,3) rEJC(i,3) rWJC(i,3)], 'Color', 'r')
    % Left arm
    line([lSJC(i,2) lEJC(i,2) lWJC(i,2)], ...
         [lSJC(i,3) lEJC(i,3) lWJC(i,3)], 'Color', 'r')
    % Whole-body CoM
    plot(hatCM(i,2), hatCM(i,3), 'rx')
    plot(RCM(i,2), RCM(i,3), 'rx')
    plot(LCM(i,2), LCM(i,3), 'rx')
    plot(WBCM(i,2), WBCM(i,3), 'ro', 'MarkerSize', 4) 
    
    % Simulation
%     set(figure(1),'DefaultFigureColor', 'k')
    line(data(i,2:2:16), data(i,3:2:17), 'Color', 'k')    % Stance leg and HAT segments
    line(data(i,[12 18]), data(i,[13 19]), 'Color', 'k')  % Swing leg
    line(data(i,[4 8]), data(i, [5 9]), 'Color', 'k')     % Foot
    plot(data(i,end-9), data(i,end-8), 'wx')              % HAT CoM
    plot(data(i,end-7), data(i,end-6), 'kx')              % Stance leg CoM
    plot(data(i,end-5), data(i,end-4), 'kx')              % Swing leg CoM
    plot(data(i,end-3), data(i,end-2), 'ko', 'MarkerSize', 4)% CoM
    
    drawnow
    pause(0.01)
end
