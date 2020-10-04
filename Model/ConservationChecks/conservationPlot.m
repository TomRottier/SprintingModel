clear; close all; clc;
%% Import data
file1 = importdata('sprintmodel.5', ' ', 8);
file2 = importdata('sprintmodel.1', ' ', 8);
data = file1.data;
vcmx = file2.data(:,end-1);
time = data(:,1);

%% Plot
set(figure(1),'WindowStyle','docked')
subplot(2,1,1); cla; hold on
plot(time, data(:,[2 3 4]))
legend('KECM','PECM','TE', 'location','bestoutside')
title('Energy')

subplot(2,1,2); cla; hold on
plot(time, data(:,end))
title('Angular momentum')

set(figure(2),'WindowStyle','docked')
plot(time, vcmx)