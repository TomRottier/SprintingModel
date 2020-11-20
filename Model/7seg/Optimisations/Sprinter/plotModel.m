clear; close all; clc;
%% Import data
p = 'C:\Users\tomro\SprintingModel\Model\7Seg\Optimisation\Sprinter_sprinter\';
f = '7segsprint.1';
file = importdata([p f], ' ',8);
data = file.data;
time = data(:,1);
n = length(time);

%% Draw model
% file = importdata('7SegSprint.1', ' ', 8); data{1} = file.data;
I = 1;
set(figure(I),'WindowStyle','docked'); cla; hold on
xlim([-1 1.5]); ylim([-.1 2.4]);
for i = 1:1:n
    cla
    line(data(i,2:2:16), data(i,3:2:17))
    line(data(i,[12 18]), data(i,[13 19]))
    line(data(i,[4 8]), data(i,[5 9]))
%     plot(data(i,10),data(i,11), 'ko')
%     plot(data(i,12),data(i,13), 'ko')
    drawnow 
    pause(0.01)
end
