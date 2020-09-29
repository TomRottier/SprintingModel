clear; close all; clc;
%% Load data
load data.mat           % Experimental data
                        % Technique comparison
                        
cm = dout.Average.CoM;  % Shortcut

to = dout.Average.Parameters.ContactTimeIndex;  % Takeoff
legs = ['L', 'R'];
leg = dout.Average.Information.Leg;         % Stance leg
swleg = legs(~(legs == leg));               % Swing leg

% Inertia data
inertia = cell2mat(dout.Information.Inertia.Data);
footM = inertia(1,2); shankM = inertia(2,2); thighM = inertia(3,2);
lwTrunkM = inertia(4,2); upTrunkM = inertia(5,2); headM = inertia(6,2);
upArmM = inertia(7,2); lwArmM = inertia(8,2);
legM = footM+shankM+thighM;
upperM = lwTrunkM+upTrunkM+headM+2*upArmM+2*lwArmM;

% Combined HJC
HJC = dout.Average.Markers.Data.Avg(:,:,strcmp(dout.Average.Markers.Names, 'HJC'));

% Tidy up
clearvars inertia legs 
%% CoM of trunk and arms
upperCM = (cm.Data.Avg(:,:,contains(cm.Names, 'LwTrunk')).*lwTrunkM + ...
        cm.Data.Avg(:,:,contains(cm.Names, 'UpTrunk')).*upTrunkM + ...
        cm.Data.Avg(:,:,contains(cm.Names, 'Head')).*headM + ...
        sum(cm.Data.Avg(:,:,contains(cm.Names, 'UpArm')), 3).*upArmM + ...
        sum(cm.Data.Avg(:,:,contains(cm.Names, 'LwArm')), 3).*lwArmM) ...
        ./ (upperM);

% Relative to hip joint
upperCM_HJC = upperCM - HJC;

% Tidy up
clearvars lwTrunkM upTrunkM headM upArmM lwArmM

%% CoM of swing leg
swingCM = (cm.Data.Avg(:,:,contains(cm.Names, [swleg 'Foot'])).*footM + ...
    cm.Data.Avg(:,:,contains(cm.Names, [swleg 'Shank'])).*shankM + ...
    cm.Data.Avg(:,:,contains(cm.Names, [swleg 'Thigh'])).*thighM) ./ legM;

% Relative to combined HJC
swingCM_HJC = swingCM - HJC;

%% CoM of stance leg
legCM = (cm.Data.Avg(:,:,contains(cm.Names, [leg 'Foot'])).*footM + ...
    cm.Data.Avg(:,:,contains(cm.Names, [leg 'Shank'])).*shankM + ...
    cm.Data.Avg(:,:,contains(cm.Names, [leg 'Thigh'])).*thighM) ./ legM;

% Relative to combined HJC
legCM_HJC = legCM - HJC;

% Tidy up
clearvars footM shankM thighM swleg leg

%% Check WBCM
WBCM = (legCM.*legM + swingCM.*legM + upperCM.*upperM) / (2*legM+upperM);
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

% Tidy up
clearvars legM upperM WBCM WBCM_HJC WBCM_true WBCM_true_HJC upperCM legCM swingCM

%% Fit function to upper body CoM data
% Filter 
upperCM_HJC_filt = tr_filterDP(upperCM_HJC, 1000, 25, 'low', 2);

% Derivatives
upperCM_HJC(:,:,2) = tr_diff(upperCM_HJC(:,:,1), 1/dout.Information.Rate);
upperCM_HJC(:,:,3) = tr_diff(upperCM_HJC(:,:,2), 1/dout.Information.Rate);

upperCM_HJC_filt(:,:,2) = tr_diff(upperCM_HJC_filt(:,:,1), 1/dout.Information.Rate);
upperCM_HJC_filt(:,:,3) = tr_diff(upperCM_HJC_filt(:,:,2), 1/dout.Information.Rate);

set(figure(2),'WindowStyle','docked')
set(figure(2), 'DefaultLineMarkerSize', 2)
for i = 1:3
    subplot(3,2,2*i-1); hold on; cla
    plot(upperCM_HJC(1:to,2,i), 's')
    plot(upperCM_HJC_filt(1:to,2,i), '-')

    
    subplot(3,2,2*i); hold on; cla
    plot(upperCM_HJC(1:to,3,i), 's')
    plot(upperCM_HJC_filt(1:to,3,i), '-')

end

%% Fit function to swing CoM data
% Derivatives
swingCM_HJC(:,:,2) = tr_diff(swingCM_HJC(:,:,1), 1/dout.Information.Rate);
swingCM_HJC(:,:,3) = tr_diff(swingCM_HJC(:,:,2), 1/dout.Information.Rate);

set(figure(3),'WindowStyle','docked')
set(figure(3), 'DefaultLineMarkerSize', 2)
for i = 1:3
    subplot(3,2,2*i-1); hold on; cla
    plot(swingCM_HJC(1:to,2,i), 's')
    
    subplot(3,2,2*i); hold on; cla
    plot(swingCM_HJC(1:to,3,i), 's')

end