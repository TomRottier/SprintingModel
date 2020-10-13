clear; close all; clc
%% Import data
% Get stance leg CoM and HJC from mssFcns.m
massFcns;
clearvars -except stanceCM HJC legM hatM to; close all

M = hatM+2*legM;

hatCM = importdata('HAT.txt', ' ', 2); hatCM = hatCM.data(:,[1 2]);
swingCM = importdata('swing.txt', ' ', 2); swingCM = swingCM.data(:,[1 2]);
sprinter = importdata('Technique/sprinter.txt', ' ', 2); 
sprinter = sprinter.data(:,[1 2]);
teamsport = importdata('Technique/teamsport.txt', ' ', 2);
teamsport = teamsport.data(:,[1 2]);

% CoM relative to origin

stanceCM = stanceCM(1:to,[2 3]);
hatCM = HJC(1:to,[2 3]) + hatCM(1:to,:);
swingCM = HJC(1:to,[2 3]) + swingCM(1:to,:);
sprinter = HJC(1:to,[2 3]) + sprinter(1:to,:);
teamsport = HJC(1:to,[2 3]) + teamsport(1:to,:);

%% WBCM using different techniques
WBCM_match = (hatCM.*hatM + swingCM.*legM + stanceCM.*legM) ./ M;
WBCM_sprinter = (hatCM.*hatM + sprinter.*legM + stanceCM.*legM) ./ M;
WBCM_teamsport = (hatCM.*hatM + teamsport.*legM + stanceCM.*legM) ./ M;

dsy_match = WBCM_match(end,2)-WBCM_match(1,2);
dsy_sprinter = WBCM_sprinter(end,2)-WBCM_sprinter(1,2);
dsy_teamsport = WBCM_teamsport(end,2)-WBCM_teamsport(1,2);
dsx_match = WBCM_match(end,1)-WBCM_match(1,1);
dsx_sprinter = WBCM_sprinter(end,1)-WBCM_sprinter(1,1);
dsx_teamsport = WBCM_teamsport(end,1)-WBCM_teamsport(1,1);

%% Plot
set(figure(1),'WindowStyle','docked'); hold on; cla
plot(WBCM_match(:,2))
plot(WBCM_sprinter(:,2))
plot(WBCM_teamsport(:,2))
legend(['match, \Delta: ' num2str(round(dsy_match, 3))],...
       ['sprinter, \Delta: ' num2str(round(dsy_sprinter, 3))],...
       ['teamsport, \Delta: ' num2str(round(dsy_teamsport, 3))],...
       'location','bestoutside')
title('CoM height')

set(figure(2),'WindowStyle','docked'); hold on; cla
plot(WBCM_match(:,1))
plot(WBCM_sprinter(:,1))
plot(WBCM_teamsport(:,1))
legend(['match, \Delta: ' num2str(round(dsx_match, 3))],...
       ['sprinter, \Delta: ' num2str(round(dsx_sprinter, 3))],...
       ['teamsport, \Delta: ' num2str(round(dsx_teamsport, 3))],...
       'location','bestoutside')
title('CoM x')