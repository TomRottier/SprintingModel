%% GIFs
pnames = 'C:\Users\tomro\SprintingModel\Model\7Seg\GIFs\Sprinter\';
pnamet = 'C:\Users\tomro\SprintingModel\Model\7Seg\GIFs\Teamsport\';
fname = '7segSprint.1';
datas = importdata([pnames fname], ' ', 8); datas = datas.data;
datat = importdata([pnamet fname], ' ', 8); datat = datat.data;

% Sprinter
set(figure(1),'WindowStyle','docked'); hold on; clf
n = min([length(datas) length(datat)]);
filename = 'sprinter.gif';
% set(groot, 'DefaultLineLineWidth', 1.5);


for i = 1:1:n
    hold on; cla
%     subplot(2,1,1); hold on; cla
    set(gca, 'XColor', 'none', 'YColor', 'none')
    set(gca,'DefaultLineLineWidth', 1.5)
    set(gcf, 'Color', 'w')
    axis equal; ylim([-.1 1.9]); xlim([-0.8 1.2]);
    
    line(datas(i,2:2:16), datas(i,3:2:17), 'Color', col2)    % Stance leg and HAT segments
    line(datas(i,[12 18]), datas(i,[13 19]), 'Color', col2)  % Swing leg
    line(datas(i,[4 8]), datas(i, [5 9]), 'Color', col2)     % Foot
%     plot(datas(i,end-9), datas(i,end-8), [col2 'x'])       % HAT CoM
%     plot(datas(i,end-7), datas(i,end-6), [col2 'x'])       % Stance leg CoM
%     plot(datas(i,end-5), datas(i,end-4), [col2 'x'])       % Swing leg CoM
    plot(datas(i,end-3), datas(i,end-2), [col2 'o'], 'MarkerSize', 4)% CoM
    line(xlim, [0 0], 'Color', 'k', 'LineWidth', .8)

    drawnow

  % Capture the plot as an image 
      frame = getframe(figure(1)); 
      im = frame2im(frame); 
      [imind,cm] = rgb2ind(im,256); 
      % Write to the GIF File 
      if i == 1 
          imwrite(imind,cm,filename,'gif', 'Loopcount',inf,'DelayTime',0); 
      else 
          imwrite(imind,cm,filename,'gif','WriteMode','append','DelayTime',0); 
      end 
end

% Teamsport
clf
filename = 'teamsport.gif';
for i = 1:1:n
    hold on; cla
    set(gca, 'XColor', 'none', 'YColor', 'none')
    set(gca,'DefaultLineLineWidth', 1.5)
    set(gcf, 'Color', 'w')
    axis equal; ylim([-.1 1.9]); xlim([-0.8 1.2]);
    
    line(datat(i,2:2:16), datat(i,3:2:17), 'Color', col2)    % Stance leg and HAT segments
    line(datat(i,[12 18]), datat(i,[13 19]), 'Color', col2)  % Swing leg
    line(datat(i,[4 8]), datat(i, [5 9]), 'Color', col2)     % Foot
%     plot(datat(i,end-9), datat(i,end-8), [col2 'x'])       % HAT CoM
%     plot(datat(i,end-7), datat(i,end-6), [col2 'x'])       % Stance leg CoM
%     plot(datat(i,end-5), datat(i,end-4), [col2 'x'])       % Swing leg CoM
    plot(datat(i,end-3), datat(i,end-2), [col2 'o'], 'MarkerSize', 4)% CoM
    line(xlim, [0 0], 'Color', 'k', 'LineWidth', .8)

    drawnow

  % Capture the plot as an image 
      frame = getframe(figure(1)); 
      im = frame2im(frame); 
      [imind,cm] = rgb2ind(im,256); 
      % Write to the GIF File 
      if i == 1 
          imwrite(imind,cm,filename,'gif', 'Loopcount',inf,'DelayTime',0); 
      else 
          imwrite(imind,cm,filename,'gif','WriteMode','append','DelayTime',0); 
      end 
end

