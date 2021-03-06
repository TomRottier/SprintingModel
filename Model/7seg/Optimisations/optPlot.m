clear; close all; clc
%% Import data
% Folders
p1 = 'Sprinter\10_0\';
p2 = 'Teamsport\9_2\';
name1 = 'sprinter'; name2 = 'teamsport';

datas1 = cell(1,11); datas2 = cell(size(datas1));

for i = 1:11
    fname = ['7SegSprint.' num2str(i)];
    
    file = importdata([p1 fname], ' ', 8);
    datas1{i} = file.data;
    
    file = importdata([p2 fname], ' ', 8);
    datas2{i} = file.data;
end

time1 = datas1{1}(:,1); time2 = datas2{1}(:,1);
n1 = length(time1); n2 = length(time2);

% Plot defaults
set(groot, 'DefaultLineMarkerSize', 2)
set(groot, 'DefaultLineLineWidth', 1.5)

%% Stride parameters
% Contact length
Lc1 = diff(datas1{1}([1 end],end-3)); Lc2 = diff(datas2{1}([1 end],end-3));

% Contact time
Tc1 = time1(end); Tc2 = time2(end);

% ds
ds1 = diff(datas1{1}([end 1],end-2)); ds2 = diff(datas2{1}([end 1],end-2));

% Aerial time
vcmy1 = datas1{1}(end,end); vcmy2 = datas2{1}(end,end);
Ta1 = (-vcmy1 - sqrt(vcmy1^2 - 4*-4.905*-ds1)) / -9.81;
Ta2 = (-vcmy2 - sqrt(vcmy2^2 - 4*-4.905*-ds2)) / -9.81;

% Swing time
Tsw1 = Tc1 + 2*Ta1; Tsw2 = Tc2 + 2*Ta2;

% Horizontal velocity
% vcmx1 = diff(datas1{1}([1 end],end-1)); vcmx2 = diff(datas2{1}([1 end],end-1));
vcmxf1 = datas1{1}(end,end-1); vcmxf2 = datas2{1}(end,end-1); 

% Step length
SL1 = Lc1+Ta1*vcmxf1; SL2 = Lc2+Ta2*vcmxf2;

% Step frequency
SF1 = 1/(Tc1+Ta1); SF2 = 1/(Tc2+Ta2);

% Speed
V1 = SL1*SF1; V2 = SL2*SF2;

%% Plot model
data1 = datas1{1}; data2 = datas2{1};
n = min([n1 n2]);
set(figure(),'WindowStyle','docked'); hold on; cla
xlim([-1 1.5]); ylim([-0.1 2.4]);
for i = 1:1:n
    cla
    % Data 1
    line(data1(i,2:2:16), data1(i,3:2:17), 'Color', 'k')    % Stance leg and HAT segments
    line(data1(i,[12 18]), data1(i,[13 19]), 'Color', 'k')  % Swing leg
    line(data1(i,[4 8]), data1(i, [5 9]), 'Color', 'k')     % Foot
    plot(data1(i,end-9), data1(i,end-8), 'wx')              % HAT CoM
%     plot(data1(i,end-7), data1(i,end-6), 'kx')              % Stance leg CoM
%     plot(data1(i,end-5), data1(i,end-4), 'kx')              % Swing leg CoM
    plot(data1(i,end-3), data1(i,end-2), 'ko', 'MarkerSize', 4)% CoM

    % Data 2
    line(data2(i,2:2:16), data2(i,3:2:17), 'Color', 'r')    % Stance leg and HAT segments
    line(data2(i,[12 18]), data2(i,[13 19]), 'Color', 'r')  % Swing leg
    line(data2(i,[4 8]), data2(i, [5 9]), 'Color', 'r')     % Foot
    plot(data2(i,end-9), data2(i,end-8), 'wx')              % HAT CoM
%     plot(data2(i,end-7), data2(i,end-6), 'rx')              % Stance leg CoM
%     plot(data2(i,end-5), data2(i,end-4), 'rx')              % Swing leg CoM
    plot(data2(i,end-3), data2(i,end-2), 'ro', 'MarkerSize', 4)% CoM

    drawnow 
end

%% Forces and torques
data1 = datas1{3}; data2 = datas2{3};
set(figure(),'WindowStyle','docked')
subplot(2,1,1); hold on; cla
plot(time1, data1(:,2), 'k-')
plot(time2, data2(:,2), 'r-')

subplot(2,1,2); hold on; cla
plot(time1, data1(:,3), 'k-')
plot(time2, data2(:,3), 'r-')

set(figure(),'WindowStyle','docked')
subplot(3,1,1); hold on; cla
plot(time1, data1(:,4), 'k-')
plot(time2, data2(:,4), 'r-')
title('hip'); legend(name1, name2)

subplot(3,1,2); hold on; cla
plot(time1, data1(:,5), 'k-')
plot(time2, data2(:,5), 'r-')
title('knee'); %legend(name1, name2)

subplot(3,1,3); hold on; cla
plot(time1, data1(:,6), 'k-')
plot(time2, data2(:,6), 'r-')
title('ankle'); %legend(name1, name2)

set(figure(),'WindowStyle','docked')
subplot(2,1,1); hold on; cla
plot(time1, data1(:,8), 'k--')
plot(time2, data2(:, 8), 'r--')
title('hip'); legend(name1, name2)

subplot(2,1,2); hold on; cla
plot(time1, data1(:,9), 'k--')
plot(time2, data2(:, 9), 'r--')
title('knee'); %legend(name1, name2)


%% Joint angles
data1 = datas1{4}; data2 = datas2{4};
set(figure(),'WindowStyle','docked')
subplot(3,2,1); hold on; cla
plot(time1, data1(:,2), 'k-')
plot(time2, data2(:,2), 'r-')
title('hat angle')

subplot(3,2,2); hold on; cla
plot(time1, data1(:,3), 'k-')
plot(time2, data2(:,3), 'r-')
title('hip angle' )

subplot(3,2,3); hold on; cla
plot(time1, data1(:,4), 'k-')
plot(time2, data2(:,4), 'r-')
title('knee angle')

subplot(3,2,4); hold on; cla
plot(time1, data1(:,5), 'k-')
plot(time2, data2(:,5), 'r-')
title('ankle angle')

subplot(3,1,3); hold on; cla
plot(time1, data1(:,6), 'k-')
plot(time2, data2(:,6), 'r-')
title('mtp angle'); legend(name1,name2,'location','bestoutside')

%% CoM
data1 = datas1{1}; data2 = datas2{1};
hjc1 = data1(:,[12 13]); hjc2 = data2(:,[12 13]);

% WBCM
set(figure(),'WindowStyle','docked')
subplot(1,2,1); hold on; cla
plot(time1, data1(:,end-2), 'k-')
plot(time2, data2(:,end-2), 'r-')
title('CoM height'); legend(name1,name2,'location','northwest')

subplot(1,2,2); hold on; cla
plot(time1, data1(:,end), 'k-')
plot(time2, data2(:,end), 'r-')
title('CoM vertical velocity'); %legend(name1,name2,'location','northwest')

% Segment CoM
set(figure(),'WindowStyle','docked')
subplot(2,3,1); hold on; cla
plot(time1, data1(:,end-8), 'k-')
plot(time2, data2(:,end-8), 'r-')
title('hat CoM')

subplot(2,3,2); hold on; cla
plot(time1, data1(:,end-6), 'k-')
plot(time2, data2(:,end-6), 'r-')
title('stance CoM')

subplot(2,3,3); hold on; cla
plot(time1, data1(:,end-4), 'k-')
plot(time2, data2(:,end-4), 'r-')
title('swing CoM')

% Relative to HJC
subplot(2,3,4); hold on; cla
plot(time1, data1(:,end-8) - hjc1(:,2), 'k-')
plot(time2, data2(:,end-8) - hjc2(:,2), 'r-')
title('relative to HJC')

subplot(2,3,5); hold on; cla
plot(time1, data1(:,end-6) - hjc1(:,2), 'k-')
plot(time2, data2(:,end-6) - hjc2(:,2), 'r-')
title('relative to HJC')

subplot(2,3,6); hold on; cla
plot(time1, data1(:,end-4) - hjc1(:,2), 'k-')
plot(time2, data2(:,end-4) - hjc2(:,2), 'r-')
title('relative to HJC')

%% Activations
data1 = datas1{6}; data2 = datas2{6}; 

set(figure(),'WindowStyle','docked')
% Hip
subplot(3,2,1); hold on; cla
plot(time1, data1(:,2), 'k-')
plot(time2, data2(:,2), 'r-')
title('Hip ext.')
ylim([-.1 1.1])

subplot(3,2,2); hold on; cla
plot(time1, data1(:,3), 'k-')
plot(time2, data2(:,3), 'r-')
title('Hip flx.'); legend(name1,name2)
ylim([-.1 1.1])

% Knee
subplot(3,2,3); hold on; cla
plot(time1, data1(:,4), 'k-')
plot(time2, data2(:,4), 'r-')
title('Knee ext.')
ylim([-.1 1.1])

subplot(3,2,4); hold on; cla
plot(time1, data1(:,5), 'k-')
plot(time2, data2(:,5), 'r-')
title('Knee flx.')
ylim([-.1 1.1])

% Ankle
subplot(3,2,5); hold on; cla
plot(time1, data1(:,6), 'k-')
plot(time2, data2(:,6), 'r-')
title('Ankle ext.')
ylim([-.1 1.1])

subplot(3,2,6); hold on; cla
plot(time1, data1(:,7), 'k-')
plot(time2, data2(:,7), 'r-')
title('Ankle flx.')
ylim([-.1 1.1])

%% Torque generators
titles = {'hip','knee','ankle'};

for i = 1:3
    data1 = datas1{i+6}; data2 = datas2{i+6};
    
    % Extensor
    set(figure(),'WindowStyle','docked');
    subplot(3,1,1); hold on; cla
    plot(time1, data1(:,2), 'k-')
    plot(time2, data2(:,2), 'r-')
    title([titles{i} ' extensor'])
    ylabel('torque (N.m')
    
    subplot(3,1,2); hold on; cla
    plot(time1, data1(:,4), 'k-')
    plot(time2, data2(:,4), 'r-')
    ylabel('CC angle (deg)')
    
    subplot(3,1,3); hold on; cla
    plot(time1, data1(:,5), 'k-')
    plot(time2, data2(:,5), 'r-')
    ylabel('CC angular velocity (deg/s)')
    
    % Flexor torques
    set(figure(),'WindowStyle','docked');
    subplot(3,1,1); hold on; cla
    plot(time1, data1(:,8), 'k-')
    plot(time2, data2(:,8), 'r-')
    title([titles{i} ' flexor'])
    ylabel('torque (N.m')
    
    subplot(3,1,2); hold on; cla
    plot(time1, data1(:,10), 'k-')
    plot(time2, data2(:,10), 'r-')
    ylabel('CC angle (deg)')
    
    subplot(3,1,3); hold on; cla
    plot(time1, data1(:,11), 'k-')
    plot(time2, data2(:,11), 'r-')
    ylabel('CC angular velocity (deg/s)')

end

    