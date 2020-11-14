clear; close all; clc
% Technique comparison using the model
sprintf = importdata('Sprinter_sprinter\7segsprint.1', ' ', 8);
teamsprtf = importdata('Teamsport_teamsport\7segsprint.1', ' ', 8);
datas = sprintf.data;
datat = teamsprtf.data;

n = min([length(datas) length(datat)]);

%%
set(figure(1),'WindowStyle','docked'); cla; hold on
xlim([-1 1.5]); ylim([-.1 2.4]);
for i = 1:1:n
    cla
    % Sprinter
    line(datas(i,2:2:16), datas(i,3:2:17), 'Color', 'k')
    line(datas(i,[12 18]), datas(i,[13 19]), 'Color', 'k')
    line(datas(i,[4 8]), datas(i,[5 9]), 'Color', 'k')
%     plot(data(i,10),data(i,11), 'ko')
%     plot(data(i,12),data(i,13), 'ko')

     % Teamsport
    line(datat(i,2:2:16), datat(i,3:2:17), 'Color', 'r')
    line(datat(i,[12 18]), datat(i,[13 19]), 'Color', 'r')
    line(datat(i,[4 8]), datat(i,[5 9]), 'Color', 'r')
%     plot(data(i,10),data(i,11), 'ko')
%     plot(data(i,12),data(i,13), 'ko')

    drawnow 
    pause(0.01)
end

