clear; close all; clc
%% Import data
% Matching data
load 'C:\Users\tomro\SprintingModel\Experimental data\data.mat'
d = dout.Average;       % Shortcut
leg = d.Information.Leg; leg2 = ('LR'); leg2(strfind(leg2,leg))=[];
WBCM = d.CoM.Data.Avg(:,:,contains(d.CoM.Names, 'WBCM'));
hatCM = d.CoM.Data.Avg(:,:,contains(d.CoM.Names, 'HAT'));
swingCM = d.CoM.Data.Avg(:,:,contains(d.CoM.Names,[leg2 'Leg']));
stanceCM = d.CoM.Data.Avg(:,:,contains(d.CoM.Names,[leg 'Leg']));
HJC = d.Markers.Data.Avg(:,:,contains(d.Markers.Names, [leg 'HJC']));
TOE = d.Markers.Data.Avg(:,:,contains(d.Markers.Names, [leg '_Toe']));

mfile = importdata('matchingData.csv', ',', 2); mdata = mfile.data;
mfile2 = importdata('matchingData2.csv', ',', 2); mdata2 = mfile2.data;

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
mdata2 = mdata2(1:n,:);
I = 1;

% Stride parameters
vcmy = datas{1}(end,end); %vcmx = datas{1}(end,end-1);
cmytd = datas{1}(1,end-2); cmyto = datas{1}(end,end-2); ds = cmytd - cmyto;
ta = (-vcmy - sqrt(vcmy^2 - 4*-4.905*-ds)) / -9.81;
taj = abs(ta - 0.132);
tcj = abs(time(end) - 0.11);
tsw = 2*ta + time(end);
tswj = abs(tsw-0.374);  %vcmxj = abs(vcmx-9.67406);
Lc = datas{1}(end,end-3) - datas{1}(1,end-3); vcmx = Lc/time(end);
vcmxj = abs(vcmx - datas{1}(1,end-1));

% SSE
hatj = tr_rmse(datas{4}(:,2), mdata2(:,3));
hipj = tr_rmse(datas{4}(:,3), mdata2(:,5));
kneej = tr_rmse(datas{4}(:,4), mdata2(:,7));
anklej = tr_rmse(datas{4}(:,5), mdata2(:,9));
mtpj = tr_rmse(datas{4}(:,6), mdata2(:,11));

% Cost
J = 10*hatj^2+hipj^2+kneej^2+anklej^2+mtpj^2+1000*tswj+100*vcmxj

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
plot(time, d.Force.Data.Avg(1:n,contains(d.Force.Names,'Fy1')), 's')

subplot(2,1,2); hold on; cla
plot(time, data(:,3), '-')
plot(time, d.Force.Data.Avg(1:n,contains(d.Force.Names,'Fz1')), 's')

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
plot(time, mdata2(:,3), 's')
title(['HAT angle  rmse: ' num2str(round(hatj, 3))])

subplot(3,2,2); hold on; cla
plot(time, data(:,3), '-')
plot(time, mdata2(:,5), 's')
title(['hip angle rmse: ' num2str(round(hipj, 3))])

subplot(3,2,3); hold on; cla
plot(time, data(:,4), '-')
plot(time, mdata2(:,7), 's')
title(['knee angle rmse: ' num2str(round(kneej, 3))])

subplot(3,2,4); hold on; cla
plot(time, data(:,5), '-')
plot(time, mdata2(:,9), 's')
title(['ankle angle rmse: ' num2str(round(anklej, 3))])

subplot(3,2,5); hold on; cla
plot(time, data(:,6), '-')
plot(time, mdata2(:,11), 's')
title(['mtp angle rmse: ' num2str(round(mtpj, 3))])

%% CoM
data = datas{1}(:,end-9:end);
hjc_mdl = datas{1}(:,13);

% RMSE
wbcmyj = tr_rmse(data(:,end-2), ...
    d.CoM.Data.Avg(1:n,3,strcmp(d.CoM.Names,'WBCM')));
wbcmvyj = tr_rmse(data(:,end), ...
    d.CoM.Data.Avg(1:n,3,strcmp(d.CoM.Names,'WBCMv')));

% WBCM
set(figure(),'WindowStyle','docked')
subplot(1,2,1); hold on; cla
plot(time, data(:,end-2) - datas{1}(1,5) , '-')
plot(time, d.CoM.Data.Avg(1:n,3,strcmp(d.CoM.Names,'WBCM')) - TOE(1,3), 's')
title(['wbcm y rmse: ', num2str(round(wbcmyj, 3))])

subplot(1,2,2); hold on; cla
plot(time, data(:,end), '-')
plot(time, d.CoM.Data.Avg(1:n,3,strcmp(d.CoM.Names,'WBCMv')), 's')
title(['wbcmv y rmse: ', num2str(round(wbcmvyj, 3))])

% Segment CoM
set(figure(),'WindowStyle','docked')
subplot(2,3,1); hold on; cla
plot(time, data(:,2), '-')
plot(time, hatCM(1:n,3) - TOE(1,3), 's')
title('hat CoM')

subplot(2,3,4); hold on; cla
plot(time, data(:,2) - hjc_mdl, '-')
plot(time, hatCM(1:n,3) - HJC(1:n,3), 's')
title('relative to hip')

subplot(2,3,2); hold on; cla
plot(time, data(:,4), '-')
plot(time, stanceCM(1:n,3)  - TOE(1,3), 's')
title('stance CoM leg')

subplot(2,3,5); hold on; cla
plot(time, data(:,4) - hjc_mdl, '-')
plot(time, stanceCM(1:n,3) - HJC(1:n,3), 's')
title('relative to hip')

subplot(2,3,3); hold on; cla
plot(time, data(:,6), '-')
plot(time, swingCM(1:n,3)  - TOE(1,3), 's')
title('swing leg CoM')

subplot(2,3,6); hold on; cla
plot(time, data(:,6) - hjc_mdl, '-')
plot(time, swingCM(1:n,3) - HJC(1:n,3), 's')
title('relative to hip')

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
