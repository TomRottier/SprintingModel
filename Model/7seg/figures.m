% Figures for paper
clear ; close all; clc
%% Import data
% Experimental data
load 'C:\Users\tomro\SprintingModel\Experimental data\data.mat';
cms = readmatrix('..\..\Experimental data\Technique\sprinter.txt', ...
    'NumHeaderLines', 2);
cmt = readmatrix('..\..\Experimental data\Technique\teamsport.txt',...
    'NumHeaderLines', 2);
d = dout.Average;       % Shortcut
angdata = readmatrix('Evaluation\matchingData2.csv', 'NumHeaderLines', 2); 
% Simulation files
pm = 'Evaluation\VCM1000\';
ps = 'Optimisations\Sprinter\10_0\';
pt = 'Optimisations\Teamsport\9_2\';

% Read
fnum = 11;
datasm = cell(1,fnum); datass = cell(1,fnum); datast = cell(1,fnum);
for i = 1:fnum
    fname = ['7SegSprint.' num2str(i)];
    % Matching
    file = importdata([pm fname], ' ', 8);
    datasm{i} = file.data;
    % Optimisation, sprinter
    file = importdata([ps fname], ' ', 8);
    datass{i} = file.data;
    % Optimisation, teamsport
    file = importdata([pt fname], ' ', 8);
    datast{i} = file.data;
end

% Time
timem = datasm{1}(:,1); times = datass{1}(:,1); timet = datast{1}(:,1);

% Lengths
nm = length(timem); ns = length(times); nt = length(timet);

% Line styles
linm = 'k-'; lins = 'k--'; lint = 'k:'; lind = 'ks';
set(groot, 'DefaultLineLineWidth', 1.5);

%% Evaluation
% Stick figures drawn elsewhere
% Angles
data = datasm{4};
skip = 5;   % Graphs look nicer
time2 = timem(1:skip:nm); angdata2 = angdata(1:skip:nm,:);

% RMSE
hatj = tr_rmse(angdata(1:nm,3), data(:,2)); 
hipj = tr_rmse(angdata(1:nm,5), data(:,3));
kneej = tr_rmse(angdata(1:nm,7), data(:,4)); 
anklej = tr_rmse(angdata(1:nm,9), data(:,5));
mtpj = tr_rmse(angdata(1:nm,11), data(:,6));

yrange = [80 200];

set(figure(),'WindowStyle','docked')
set(groot, 'DefaultLineMarkerSize', 3)

subplot(3,2,1); hold on; cla
plot(timem, data(:,2), linm)
plot(time2, angdata2(:,3), lind)
title(['HAT angle  RMSE: ' num2str(round(hatj, 3))])
ylim([40 160])

subplot(3,2,2); hold on; cla
plot(timem, data(:,3), linm)
plot(time2, angdata2(:,5), lind)
title(['Hip angle RMSE: ' num2str(round(hipj, 3))])
ylim(yrange)

subplot(3,2,3); hold on; cla
plot(timem, data(:,4), linm)
plot(time2, angdata2(:,7), lind)
title(['Knee angle RMSE: ' num2str(round(kneej, 3))])
ylim(yrange)
ylabel('Angle (degrees)')

subplot(3,2,4); hold on; cla
plot(timem, data(:,5), linm)
plot(time2, angdata2(:,9), lind)
title(['Ankle angle RMSE: ' num2str(round(anklej, 3))])
ylim(yrange)

subplot(3,2,5); hold on; cla
plot(timem, data(:,6), linm)
plot(time2, angdata2(:,11), lind)
title(['MTP angle RMSE: ' num2str(round(mtpj, 3))])
ylim(yrange)
xlabel('Time (s)')
% legend('model','matching data')


%% Optimisations
%%%%%%%% Forces and torques %%%%%%%%
datas = datass{3}; datat = datast{3};

set(figure(),'WindowStyle','docked'); hold on; cla
plot(times, datas(:,3), lins)
plot(timet, datat(:,3), lint)
xlabel('Time (s)'); ylabel('Force (N)')
title('Vertical GRF'); %legend('Sprinter','Team sport')

set(figure(),'WindowStyle','docked')
subplot(3,1,1); hold on; cla
plot(times, datas(:,4), lins)
plot(timet, datat(:,4), lint)
title('Hip'); %legend('Sprinter','Team sport')

subplot(3,1,2); hold on; cla
plot(times, datas(:,5), lins)
plot(timet, datat(:,5), lint)
title('Knee'); 
ylabel('Torque (N.m)')

subplot(3,1,3); hold on; cla
plot(times, datas(:,6), lins)
plot(timet, datat(:,6), lint)
title('Ankle'); 
xlabel('Time (s)')

% Support moment
sms = sum(datas(:,[4 5 6]), 2); smt = sum(datat(:,[4 5 6]), 2);
figure(); hold on; cla
plot(times,sms,lins)
plot(timet,smt,lint)

set(figure(),'WindowStyle','docked')
subplot(2,1,1); hold on; cla
plot(times, datas(:,8), lins)
plot(timet, datat(:, 8), lint)
title('Hip'); 

subplot(2,1,2); hold on; cla
plot(times, datas(:,9), lins)
plot(timet, datat(:, 9), lint)
title('Knee'); %legend('Sprinter','Team sport')
xlabel('Time (s)')
ylabel('Torque (N.m)')

%%%%%%%% Joint angles %%%%%%%%
datas = datass{4}; datat = datast{4};
set(figure(),'WindowStyle','docked')
subplot(3,2,1); hold on; cla
plot(times, datas(:,2), lins)
plot(timet, datat(:,2), lint)
title('hat angle')

subplot(3,2,2); hold on; cla
plot(times, datas(:,3), lins)
plot(timet, datat(:,3), lint)
title('hip angle' )

subplot(3,2,3); hold on; cla
plot(times, datas(:,4), lins)
plot(timet, datat(:,4), lint)
title('knee angle')

subplot(3,2,4); hold on; cla
plot(times, datas(:,5), lins)
plot(timet, datat(:,5), lint)
title('ankle angle')

subplot(3,1,3); hold on; cla
plot(times, datas(:,6), lins)
plot(timet, datat(:,6), lint)


%%%%%%%% CC Angular velocity %%%%%%%%
titles = {'Hip','Knee','Ankle'};
set(figure(),'WindowStyle','docked')
for i = 1:3
    datas = datass{i+6}; datat = datast{i+6};
    subplot(3,1,i); hold on; cla
    plot(times, datas(:,5), lins)
    plot(timet, datat(:,5), lint)
    title(titles{i})
end
subplot(3,1,2); ylabel('Angular velocity (deg.s^{-1})')
subplot(3,1,3); xlabel('Time (s)')