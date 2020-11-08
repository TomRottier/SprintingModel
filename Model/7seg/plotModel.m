clear; close all; clc;
%% Import data
[files, p] = uigetfile('*', 'MultiSelect', 'on'); 
if ischar(files), files={files}; end        % Convert to cell
data = cell(1,length(files));
for i = 1:length(files)
    file = importdata([p files{i}], ' ', 8);
    data{i} = file.data;
end

time = data{1}(:,1);
n = length(time);

%% Draw model
% file = importdata('7SegSprint.1', ' ', 8); data{1} = file.data;
I = 1;
set(figure(I),'WindowStyle','docked'); cla; hold on
xlim([-1 1.5]); ylim([-.1 2.4]);
for i = 1%:1:n
    cla
    line(data{I}(i,2:2:16), data{I}(i,3:2:17))
    line(data{I}(i,[12 18]), data{I}(i,[13 19]))
    line(data{I}(i,[4 8]), data{I}(i,[5 9]))
%     plot(data{I}(i,10),data{I}(i,11), 'ko')
%     plot(data{I}(i,12),data{I}(i,13), 'ko')
    drawnow 
    pause(0.01)
end
I = I+1;

%% Energy conservation
I = 5;
set(figure(I),'WindowStyle','docked'); hold on; cla
plot(time, data{I}(:,[2 4 5]))
plot(time, data{I}(:,[3 6]))
legend('KECM','PECM','TE','KECM2','TE2', 'location','bestoutside')

%% 