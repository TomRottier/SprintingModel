clear; close all; clc;
%% Import data
file1 = importdata('sprintmodel.5', ' ', 8);
file2 = importdata('sprintmodel.1', ' ', 8);
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

