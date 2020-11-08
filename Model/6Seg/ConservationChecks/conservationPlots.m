clear; close all; clc
% Show that model conserves energy (when no energy added) and angular
% momentum (when no external forces).
%
%% Import data
file1 = importdata('6segSprint.5', ' ', 8);
file2 = importdata('6segSprint.1', ' ', 8);
data = file1.data;
time = data(:,1);

dE = data(end,4) - data(1,4);
dH = data(end,5) - data(1,5);
dP = (data(end,6)+data(end,7)) - (data(1,6)+data(1,7));

%% Plot
set(figure(1),'WindowStyle','docked'); cla; hold on
plot(time, data(:,[2 3 4]))
legend('KECM','PECM','TE', 'location','bestoutside')
title(['\Delta energy: ' num2str(dE)])


set(figure(2),'WindowStyle','docked')
subplot(2,1,1); hold on; cla;
plot(time, data(:,5))
title(['\Delta angular momentum: ' num2str(dH)])
legend('z','location','bestoutside')

subplot(2,1,2); hold on; cla
plot(time, data(:,[6 7]))
title(['\Delta linear momentum: ' num2str(dP)])
legend('x','y','location','bestoutside')

%% Draw model
data = file2.data; n = length(data);
set(figure(3),'WindowStyle','docked'); cla; hold on
xlim([-1 1.5]); ylim([-0.1 2.4]);
for i = 1:1:n
    cla
    line(data(i,2:2:12), data(i,3:2:13))
    line(data(i,[8 14]), data(i,[9 15]))
    plot(data(i,end-3), data(i,end-2), 'kx')
    drawnow 
    pause(0.01)
end

