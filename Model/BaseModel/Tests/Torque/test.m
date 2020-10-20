clear; close all; clc
% Test torque generators
file1 = importdata('6segSprint.1', ' ', 8);
file3 = importdata('6segSprint.3', ' ', 8);
data1 = file1.data;
data3 = file3.data;

%%
set(figure(1),'WindowStyle','docked'); axis equal
n = length(data1);
for i = 1:1:n
    
    subplot(2,1,1); hold on; cla
    xlim([-1 1.5]); ylim([-0.1 2.4]);
    cla
    line(data1(i,2:2:12), data1(i,3:2:13))
    line(data1(i,[8 14]), data1(i,[9 15]))
    plot(data1(i,end-3), data1(i,end-2), 'kx')
    drawnow 
    pause(0.01)
    
    subplot(2,1,2); hold on; cla
    plot(data3(:, 5))
    plot(i, data3(i,5), 'o')
    drawnow

end


