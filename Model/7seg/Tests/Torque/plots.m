clear; close all; clc;
%% Import data
f = '7SegSprint.9';
file = importdata(f, ' ', 8);
data = file.data;


%% Plot
set(figure(1),'WindowStyle','docked')
subplot(3,1,1); hold on; cla
plot(data(:,2))
title('torque')

subplot(3,1,2); hold on; cla
plot(data(:,[4 6]))
title('angle')

subplot(3,1,3); hold on; cla
plot(data(:,[5 7]))
title('angular velocity')
legend('CC','SEC')