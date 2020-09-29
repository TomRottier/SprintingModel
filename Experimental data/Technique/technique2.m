clear; close all; clc;
%% Import data
sprinter = importdata('sprinter_xypts.csv', ',', 1);
teamsport = importdata('teamsport_xypts.csv', ',', 1);

%% Fill gaps with elite's hip marker
set(figure(1),'WindowStyle','docked'); cla
hold on
plot(sprinter.data(:,1:2), 'k-')

while ~all(~isnan(sprinter.data(:,1)))
    % Find start and end of first missing section
    idx1 = find(isnan(sprinter.data(:,1)), 1);
    idx2 = idx1 + find(~isnan(sprinter.data(idx1:end,1)), 1) -1;
    
    % Cublic spline interpolation
    sprinter.data(idx1:idx2, 1:2) = interp1([idx1-1 idx2]', ...
               [sprinter.data(idx1-1,1:2); sprinter.data(idx2,1:2)], ...
               idx1:idx2, 'spline');
                                  
   % Plot filled section
   plot(idx1:idx2, sprinter.data(idx1:idx2,1:2), 'k--')
    
end

% 3D array with sprinter 1 and teamsport 2
points = cat(3, sprinter.data, teamsport.data);

% Tidy up
clearvars idx1 idx2 sprinter teamsport

%% Process data, filter etc
% Find appropiate cutoff
% for i = 1:size(pointsG, 2)
%     col = i;
%     set(figure(2), 'WindowStyle','docked'); cla
%     hold on
%     plot(pointsG(:,col), 'k--', 'DisplayName', 'raw')
%     cutoffs = [8 10 15 20];
%     for j = 1:length(cutoffs)
%         pfilt = tr_filterDP(pointsG(:,col), 1000, cutoffs(j), 'low', 2);
%         plot(pfilt, 'DisplayName', num2str(cutoffs(j)), 'LineWidth', 1.5)
%     end
%     legend('show', 'location', 'bestoutside')
%     keyboard;
%     pause(1);
% end

% Filter
cutoff = 20;

% Reorder to 2D array then filter
points_f = reshape(...
              tr_filterDP(reshape(points, [456 16]), 1000, 20, 'low', 2),...
              [456 8 2]);

set(figure(2),'WindowStyle','docked')
hold on
plot(points(:,1,1))
plot(points_f(:,1,1))

% Tidy up
clearvars cutoff pointsB pointsG

%% Calculate segment angles
Q = nan(size(points_f, 1), 3, 2);

for i = 1:3
    n = 9-2*i;        % Index sequence
    
    % Prox and dist JC
    p1 = points_f(:,n:n+1,:); p2 = points_f(:,n-2:n-1,:);   
    d = p2 - p1;        % dx,dy
    Q(:,i,:) = atan2d(d(:,2,:), d(:,1,:));      % Segment angles
end

% Plot to check
% set(figure(3),'WindowStyle','docked')
% for i = 1:2:length(pointsG_f)
%     subplot(2,1,1)
%     axis equal
%     cla;
%     line([pointsB_f(i,7) pointsB_f(i,5) pointsB_f(i,3) pointsB_f(i,1)],...
%          [pointsB_f(i,8) pointsB_f(i,6) pointsB_f(i,4) pointsB_f(i,2)])
%          
%     subplot(2,1,2)
%     cla
%     hold on
%     plot(q_B)
%     plot(i,q_B(i,:), 'ko')
%     legend('foot','shank','thigh')
%     
%     drawnow
% end

% Tidy up
clearvars n p1 p2 d

    %% Calculate CoM
% Segment: mass, length, CMprox, CMdist
% Foot:    1.385, 0.224, 0.142, 0.082
% Shank:   5.271, 0.453, 0.261, 0.192          
% Thigh:   12.54, 0.447, 0.258, 0.189
%
% CMX = (MA*(L1-L2)*COS(Q1)-(L4*MA-MB*(L3-L4))*COS(Q2)-(L6*MA+L6*MB-MC*(L5-L6))*COS(Q3))/(MA+MB+MC)
% CMY = (MA*(L1-L2)*SIN(Q1)-(L4*MA-MB*(L3-L4))*SIN(Q2)-(L6*MA+L6*MB-MC*(L5-L6))*SIN(Q3))/(MA+MB+MC)

l1=0.082; l2=0.224; l3=0.192; l4=0.453; l5=0.189; l6=0.447;
ma=1.385; mb=5.271; mc=12.54;

% CoM location x,y
cm(:,1,:) = (ma*(l1-l2).*cosd(Q(:,1,:))-(l4*ma-mb*(l3-l4)).* ...
            cosd(Q(:,2,:))-(l6*ma+l6*mb-mc*(l5-l6)).* ...
            cosd(Q(:,3,:)))/(ma+mb+mc);
cm(:,2,:) = (ma*(l1-l2).*sind(Q(:,1,:))-(l4*ma-mb*(l3-l4)).* ...
            sind(Q(:,2,:))-(l6*ma+l6*mb-mc*(l5-l6)).* ...
            sind(Q(:,3,:)))/(ma+mb+mc);


% set(figure(4),'WindowStyle','docked'); clf
% subplot(2,1,1); hold on
% plot(cm(:,1,1)); plot(cm(:,1,2))
% title('CoM position relative to hip joint')
% ylabel('x (m)')
% legend('sprinter', 'teamsport', 'location', 'southeast')
% 
% subplot(2,1,2); hold on
% plot(cm(:,2,1)); plot(cm(:,2,2))
% ylabel('y (m)')

% Tidy up
clearvars l1 l2 l3 l4 l5 l6 ma mb mc

%% Reorder data
% Takeoff and touchdown frames for both legs estimated from video
% Videos recorded at 1000 Hz and synchronised to takeoff of markered leg
% Number of frames: 456
%                  ipsilateral | contralateral
% sprinter  to/td: 58/416        286/181
% teamsport to/td: 58/413        287/175 

% Swing leg CoM during contralateral stance
cm_stance(1) = {cm(181:286,:,1)};
cm_stance(2) = {cm(175:287,:,2)};

% Ipsilateral thigh angle during stance (assumed to be same for both legs)
q3(1) = {[Q(416:end,3,1); Q(1:58,3,1)]};
q3(2) = {[Q(413:end,3,2); Q(1:58,3,2)]};

set(figure(5),'WindowStyle','docked'); clf
set(figure(5),'DefaultLineMarkerSize', 2)
subplot(2,1,1); hold on
title('Thigh angle')
plot(Q(:,3,1),'^'); plot(Q(:,3,2), '^')
legend('sprinter','teamsport', 'location', 'southeast')
subplot(2,1,2); hold on
title('Thigh angle stance')
plot(q3{1}, 's'); plot(q3{2}, 's')

% % Tidy up
% clearvars td to cmxG cmyG cmxB cmyB q_G q_B

%% Take derivative and fit a function
% Derivatives
cm_stance(2,:) = cellfun(@(x) tr_diff(x, 0.001), cm_stance(1,:),...
    'UniformOutput', false);
cm_stance(3,:) = cellfun(@(x) tr_diff(x, 0.001), cm_stance(2,:),...
    'UniformOutput', false);

% Swing leg CoM as a function of contralateral thigh angle
n1 = min([length(q3{1,1}) length(cm_stance{1,1})]); % Fit same length
n2 = min([length(q3{1,2}) length(cm_stance{1,2})]);
n = [n1 n2];
norder = 10;
coef = nan(1,norder+1,2,3);
for i = 1:2         % x,y loop
    for j = 1:2     % sprinter,teamsport loop
        for k = 1:3 % derivative loop
            coef(i,:,j,k) = polyfit(q3{j}(1:n(j)), ...
                cm_stance{k,j}(1:n(j),i), norder);
        end
    end
end



set(figure(6),'WindowStyle','docked')
set(figure(6),'DefaultLineMarkerSize', 0.5, 'DefaultLineLineWidth', 1.2)
ylabels = {'position', 'velocity', 'acceleration'};
for i = 1:3
    % CoM x
    subplot(3,2,2*i-1); cla; hold on
    plot(q3{1}(1:n1), cm_stance{i,1}(1:n1,1), 'ks') % Raw data
    plot(q3{2}(1:n2), cm_stance{i,2}(1:n2,1), 'rs')
    plot(q3{1}(1:n1), polyval(coef(1,:,1,i),q3{1}(1:n1)),'k-') %Fitted poly
    plot(q3{2}(1:n2), polyval(coef(1,:,2,i),q3{2}(1:n2)),'r-')
    ylabel(ylabels{i})
    
    % CoM y
    subplot(3,2,2*i); cla; hold on
    plot(q3{1}(1:n1), cm_stance{i,1}(1:n1,2), 'ks') % Raw data
    plot(q3{2}(1:n2), cm_stance{i,2}(1:n2,2), 'rs')
    plot(q3{1}(1:n1), polyval(coef(2,:,1,i),q3{1}(1:n1)),'k-') %Fitted poly
    plot(q3{2}(1:n2), polyval(coef(2,:,2,i),q3{2}(1:n2)),'r-')
end
subplot(3,2,1); title('x'); subplot(3,2,2); title('y');
subplot(3,2,5); xlabel('Thigh angle'); 
subplot(3,2,6); xlabel('Thigh angle');