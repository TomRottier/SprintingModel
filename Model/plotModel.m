clear; close all; clc;
%% Import data
[files, p] = uigetfile('*', 'MultiSelect', 'on');
data = cell(1,length(files));
for i = 1:length(files)
    file = importdata([p files{i}], ' ', 8);
    data{i} = file.data;
end

time = data{1}(:,1);
n = length(time);

%% Draw model
I = 1;
set(figure(I),'WindowStyle','docked'); cla
xlim([-1 1.5]); ylim([-.1 2.4]);
for i = 1:1:n
    cla
    line(data{I}(i,2:2:end-4), data{1}(i,3:2:end-4))
    drawnow 
    pause(0.01)
end
I = I+1;

%% Energy conservation
I = 5;
set(figure(I),'WindowStyle','docked'); hold on; cla
plot(time, data{I}(:,[2 4 5]))
plot(time, data{I}(:,[3 6]))
legend('KECM','PECM','TE','KECM2','TE2')
