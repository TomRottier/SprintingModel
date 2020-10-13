clear; close all; clc;
%% Load data
load data.mat           % Experimental data
output = 1;             % Output data or not

cm = dout.Average.CoM;  % Shortcut

% Combined HJC
HJC = dout.Average.Markers.Data.Avg(:,:,strcmp(dout.Average.Markers.Names, 'HJC'));

to = find(dout.Average.Force.Data.Avg(:,3) < 80, 1);  % Takeoff
legs = ['L', 'R'];
leg = dout.Average.Information.Leg;         % Stance leg
swleg = legs(~(legs == leg));               % Swing leg

% Inertia data
inertia = cell2mat(dout.Information.Inertia.Data);
footM = inertia(1,2); shankM = inertia(2,2); thighM = inertia(3,2);
lwTrunkM = inertia(4,2); upTrunkM = inertia(5,2); headM = inertia(6,2);
upArmM = inertia(7,2); lwArmM = inertia(8,2);
legM = footM+shankM+thighM;
hatM = lwTrunkM+upTrunkM+headM+2*upArmM+2*lwArmM;

%% CoM of trunk and arms
hatCM = (cm.Data.Avg(:,:,contains(cm.Names, 'LwTrunk')).*lwTrunkM + ...
        cm.Data.Avg(:,:,contains(cm.Names, 'UpTrunk')).*upTrunkM + ...
        cm.Data.Avg(:,:,contains(cm.Names, 'Head')).*headM + ...
        sum(cm.Data.Avg(:,:,contains(cm.Names, 'UpArm')), 3).*upArmM + ...
        sum(cm.Data.Avg(:,:,contains(cm.Names, 'LwArm')), 3).*lwArmM) ...
        ./ (hatM);

% Relative to hip joint
upperCM_HJC = hatCM - HJC;

%% CoM of swing leg
swingCM = (cm.Data.Avg(:,:,contains(cm.Names, [swleg 'Foot'])).*footM + ...
    cm.Data.Avg(:,:,contains(cm.Names, [swleg 'Shank'])).*shankM + ...
    cm.Data.Avg(:,:,contains(cm.Names, [swleg 'Thigh'])).*thighM) ./ legM;

% Relative to combined HJC
swingCM_HJC = swingCM - HJC;

%% CoM of stance leg
stanceCM = (cm.Data.Avg(:,:,contains(cm.Names, [leg 'Foot'])).*footM + ...
    cm.Data.Avg(:,:,contains(cm.Names, [leg 'Shank'])).*shankM + ...
    cm.Data.Avg(:,:,contains(cm.Names, [leg 'Thigh'])).*thighM) ./ legM;

% Relative to combined HJC
legCM_HJC = stanceCM - HJC;

%% Check WBCM
WBCM = (stanceCM.*legM + swingCM.*legM + hatCM.*hatM) / (2*legM+hatM);
WBCM_HJC = WBCM - HJC;      % Relative to HJC

% All segments CoM - slight difference maybe due to using joint centres in
% the full model compared to just CoM here
WBCM_true = cm.Data.Avg(:,:,strcmp(cm.Names, 'WBCM'));
WBCM_true_HJC = WBCM_true - HJC;        % Relative to HJC

set(figure(1),'WindowStyle','docked'); hold on; cla
plot(WBCM(:,3))
plot(WBCM_true(:,3))
title(['RMSE: ' num2str(round(tr_rmse(WBCM(:,3),WBCM_true(:,3))*1000, 2)) ' mm'])
legend('model','true', 'location', 'bestoutside')

%% Derivatives 
% HAT CoM
upperCM_HJC(:,:,2) = tr_diff(upperCM_HJC(:,:,1), 1/dout.Information.Rate);
upperCM_HJC(:,:,3) = tr_diff(upperCM_HJC(:,:,2), 1/dout.Information.Rate);

% Swing CoM
swingCM_HJC(:,:,2) = tr_diff(swingCM_HJC(:,:,1), 1/dout.Information.Rate);
swingCM_HJC(:,:,3) = tr_diff(swingCM_HJC(:,:,2), 1/dout.Information.Rate);


% Plot 
set(figure(2),'WindowStyle','docked')
set(figure(2), 'DefaultLineMarkerSize', 2)
titles = {'position','velocity','acceleration'};
for i = 1:3
    subplot(3,2,2*i-1); hold on; cla
    plot(upperCM_HJC(1:to,2,i), 's')
    title([titles{i} ' x'])
    
    subplot(3,2,2*i); hold on; cla
    plot(upperCM_HJC(1:to,3,i), 's')
    title([titles{i} ' y'])
end

set(figure(3),'WindowStyle','docked')
set(figure(3), 'DefaultLineMarkerSize', 2)
for i = 1:3
    subplot(3,2,2*i-1); hold on; cla
    plot(swingCM_HJC(1:to,2,i), 's')
    title([titles{i} ' x'])
    
    subplot(3,2,2*i); hold on; cla
    plot(swingCM_HJC(1:to,3,i), 's')
    title([titles{i} ' y'])
end

%% Export data
if output
    % Column names
    colwidth = 13;
    precision = 5;
    colnames = ["PositionX","PositionY","VelocityX","VelocityY",...
                "AccelerationX","AccelerationY"];
    nametype = ['%-' num2str(colwidth) 's'];
    datatype = ['%' num2str(colwidth) '.' num2str(precision) 'E'];
    namefmt = [repmat([nametype ' '], 1, size(colnames,2)-1) nametype '\n'];
    datafmt = [repmat([datatype ' '], 1, size(colnames,2)-1) datatype '\n'];
    
    % HAT CoM
    fid = fopen('HAT.txt', 'w');
    fprintf(fid, '%4d', size(upperCM_HJC,1));    % Data size
    fprintf(fid, '%4d', 2);                        % Header rows
    fprintf(fid, '%4d\n', colwidth+1);               % Column width
    fprintf(fid, namefmt, pad(colnames, colwidth, 'left'));
    fprintf(fid, datafmt, ...
                string(reshape(upperCM_HJC(:,[2 3],:),...
                        [size(upperCM_HJC, 1) 6])'));
    fclose(fid);
    
    % Swing CoM
    fid = fopen('swing.txt', 'w');
    fprintf(fid, '%4d', size(swingCM_HJC,1));    % Data size
    fprintf(fid, '%4d', 2);                        % Header rows
    fprintf(fid, '%4d\n', colwidth+1);               % Column width
    fprintf(fid, namefmt, pad(colnames, colwidth, 'left'));
    fprintf(fid, datafmt, ...
                string(reshape(swingCM_HJC(:,[2 3],:),...
                        [size(swingCM_HJC, 1) 6])'));
    fclose(fid);
end