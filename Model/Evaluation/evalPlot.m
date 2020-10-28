clear; close all; clc
%% Import data
% Matching data
run('C:\Users\tomro\SprintingModel\Experimental data\massFcns'); 
clearvars -except swingCM swingCM_HJC stanceCM stanceCM_HJC hatCM hatCM_HJC HJC
mfile = importdata('matchingData.txt', ' ', 2);
mdata = mfile.data;

% Simulation data
[fname, p] = uigetfile('*', 'MultiSelect','on');
datas = cell(1,length(fname));
for i = 1:length(fname)
    file = importdata([p fname{i}], ' ', 8);
    datas{i} = file.data;
end


time = datas{1}(:,1); 
n = length(time);
mdata = mdata(1:n,:);       % Simulation length
I = 1;

% Stride parameters
vcmy = datas{1}(end,end); vcmx = datas{1}(end,end-1);
cmytd = datas{1}(1,end-2); cmyto = datas{1}(end,end-2); ds = cmytd - cmyto;
ta = (-vcmy - sqrt(vcmy^2 - 4*-4.905*-ds)) / -9.81;
tsw = 2*ta + time(end);
tswj = abs(tsw-0.3763); vcmxj = abs(vcmx-9.6975);

% SSE
hatj = sqrt(mean((datas{4}(:,2) - mdata(:,6)).^2));
hipj = sqrt(mean((datas{4}(:,3) - mdata(:,4)).^2));
kneej = sqrt(mean((datas{4}(:,4) - mdata(:,5)).^2));
anklej = sqrt(mean((datas{4}(:,5) - mdata(:,7)).^2));

% Cost
J = 10*hatj^2+hipj^2+kneej^2+anklej^2+500*tswj+500*vcmxj

% Plot defaults
set(groot, 'DefaultLineMarkerSize', 2)

%% Plot model
data = datas{1};
set(figure(1),'WindowStyle','docked'); hold on; cla
xlim([-1 1.5]); ylim([-0.1 2.4]);
for i = 1:1:n
    cla
    line(data(i,2:2:12), data(i,3:2:13))    % Stance leg and HAT segments
    line(data(i,[8 14]), data(i,[9 15]))    % Swing leg
    plot(data(i,16), data(i,17), 'ko')      % HAT CoM
    plot(data(i,end-3), data(i,end-2), 'kx')% CoM
    drawnow 
end

I = I + 1;

%% Forces and torques
data = datas{3};
set(figure(2),'WindowStyle','docked')
subplot(2,1,1); hold on; cla
plot(time, data(:,2), '-')
plot(time, mdata(:,2), 's')

subplot(2,1,2); hold on; cla
plot(time, data(:,3), '-')
plot(time, mdata(:,3), 's')

I = I+1;
set(figure(4),'WindowStyle','docked'); hold on; cla
plot(time, data(:,4:6), '-')
plot(time, data(:,[7 8]), '--')
legend('hip','knee','ankle','sw. hip', 'sw. knee', ...
    'location', 'bestoutside')

%% Joint angles
data = datas{4};
set(figure(3),'WindowStyle','docked')
subplot(2,2,1); hold on; cla
plot(time, data(:,2), '-')
plot(time, mdata(:,6), 's')
title(['HAT angle  rmse: ' num2str(round(hatj, 3))])

subplot(2,2,2); hold on; cla
plot(time, data(:,3), '-')
plot(time, mdata(:,4), 's')
title(['hip angle rmse: ' num2str(round(hipj, 3))])

subplot(2,2,3); hold on; cla
plot(time, data(:,4), '-')
plot(time, mdata(:,5), 's')
title(['knee angle rmse: ' num2str(round(kneej, 3))])

subplot(2,2,4); hold on; cla
plot(time, data(:,5), '-')
plot(time, mdata(:,7), 's')
title(['ankle angle rmse: ' num2str(round(anklej, 3))])

%% CoM
data = datas{1}(:,end-9:end);
hjc_mdl = datas{1}(:,9);

% RMSE
wbcmyj = tr_rmse(data(:,end-2), mdata(:,8));
wbcmvyj = tr_rmse(data(:,end), mdata(:,9));

% WBCM
set(figure(),'WindowStyle','docked')
subplot(1,2,1); hold on; cla
plot(time, data(:,end-2), '-')
plot(time, mdata(:,8), 's')
title(['wbcm y rmse: ', num2str(round(wbcmyj, 3))])

subplot(1,2,2); hold on; cla
plot(time, data(:,end), '-')
plot(time, mdata(:,9), 's')
title(['wbcmv y rmse: ', num2str(round(wbcmvyj, 3))])

% Segment CoM
set(figure(),'WindowStyle','docked')
subplot(2,3,1); hold on; cla
plot(time, data(:,2), '-')
plot(time, hatCM(1:n,3), 's')
title('hat CoM')

subplot(2,3,4); hold on; cla
plot(time, data(:,2) - hjc_mdl, '-')
plot(time, hatCM_HJC(1:n,3), 's')
title('relative to hip')

subplot(2,3,2); hold on; cla
plot(time, data(:,4), '-')
plot(time, stanceCM(1:n,3), 's')
title('stance CoM leg')

subplot(2,3,5); hold on; cla
plot(time, data(:,4), '-')
plot(time, stanceCM(1:n,3), 's')
title('stance CoM leg')

subplot(2,3,3); hold on; cla
plot(time, data(:,6), '-')
plot(time, swingCM(1:n,3), 's')
title('swing leg CoM')

subplot(2,3,6); hold on; cla
plot(time, data(:,6) - hjc_mdl, '-')
plot(time, swingCM_HJC(1:n,3), 's')
title(' relative to hip')

%% Torque generators
% Hip
ccdata = datas{8};
jdata = datas{4};
set(figure(),'WindowStyle','docked')
subplot(4,1,1); hold on; cla
title('torque')
plot(time, ccdata(:,2))

subplot(4,1,2); hold on; cla
title('activation')
plot(time, ccdata(:,3))

subplot(4,1,3); hold on; cla
title('angle')
plot(time, ccdata(:,[4 6]))
plot(time, 360-jdata(:,3))
legend('cc','sec','joint','location','bestoutside')

subplot(4,1,4); hold on; cla
title('angular velocity')
plot(time, ccdata(:,[5 7]))
plot(time, -jdata(:,9))
legend('cc','sec','joint','location','bestoutside')
