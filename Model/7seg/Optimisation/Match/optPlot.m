clear; close all; clc
%% Import data
% Simulation data
[fname, p] = uigetfile('*', 'MultiSelect','on');
datas = cell(1,length(fname));
for i = 1:length(fname)
    file = importdata([p fname{i}], ' ', 8);
    datas{i} = file.data;
end


time = datas{1}(:,1); 
n = length(time);
I = 1;

% Stride parameters
vcmy = datas{1}(end,end); vcmxf = datas{1}(end,end-1); vcmxi = datas{1}(1,end-1);
cmytd = datas{1}(1,end-2); cmyto = datas{1}(end,end-2); ds = cmytd - cmyto;
ta = (-vcmy - sqrt(vcmy^2 - 4*-4.905*-ds)) / -9.81;
taj = abs(ta - 0.132);
tsw = 2*ta + time(end);
tswj = abs(tsw-0.374); vcmxj = abs(vcmxf-vcmxi);

% Cost
J = 10*tswj+vcmxj

% Plot defaults
set(groot, 'DefaultLineMarkerSize', 2)

%% Plot model
data = datas{1};
set(figure(1),'WindowStyle','docked'); hold on; cla
xlim([-1 1.5]); ylim([-0.1 2.4]);
for i = 1:1:n
    cla
    line(data(i,2:2:16), data(i,3:2:17))    % Stance leg and HAT segments
    line(data(i,[12 18]), data(i,[13 19]))  % Swing leg
    line(data(i,[4 8]), data(i, [5 9]))     % Foot
    plot(data(i,end-9), data(i,end-8), 'wx')              % HAT CoM
%     plot(data(i,end-7), data(i,end-6), 'kx')              % Stance leg CoM
%     plot(data(i,end-5), data(i,end-4), 'kx')              % Swing leg CoM
    plot(data(i,end-3), data(i,end-2), 'ko', 'MarkerSize', 4)% CoM
    drawnow 
end

I = I + 1;

%% Forces and torques
data = datas{3};
set(figure(2),'WindowStyle','docked')
subplot(2,1,1); hold on; cla
plot(time, data(:,2), '-')
% plot(time, d.Force.Data.Avg(1:n,contains(d.Force.Names,'Fy1')), 's')

subplot(2,1,2); hold on; cla
plot(time, data(:,3), '-')
% plot(time, d.Force.Data.Avg(1:n,contains(d.Force.Names,'Fz1')), 's')

I = I+1;
set(figure(4),'WindowStyle','docked'); hold on; cla
plot(time, data(:,4:7), '-')
plot(time, data(:,[8 9]), '--')
legend('hip','knee','ankle','mtp','sw. hip', 'sw. knee', ...
    'location', 'bestoutside')

%% Joint angles
data = datas{4};
set(figure(3),'WindowStyle','docked')
subplot(3,2,1); hold on; cla
plot(time, data(:,2), '-')
% plot(time, mdata2(:,3), 's')
title('HAT angle')

subplot(3,2,2); hold on; cla
plot(time, data(:,3), '-')
% plot(time, mdata2(:,5), 's')
title('hip angle' )

subplot(3,2,3); hold on; cla
plot(time, data(:,4), '-')
% plot(time, mdata2(:,7), 's')
title('knee angle')

subplot(3,2,4); hold on; cla
plot(time, data(:,5), '-')
% plot(time, mdata2(:,9), 's')
title('ankle angle')

subplot(3,2,5); hold on; cla
plot(time, data(:,6), '-')
% plot(time, mdata2(:,11), 's')
title('mtp angle')

