clear; close all; clc;
%% Import data
sprinter = importdata('sprinter_xypts.csv', ',', 1);
teamsport = importdata('teamsport_xypts.csv', ',', 1);

output = 1;     % Write to output
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
cutoff = 15;

% Reorder to 2D array then filter
points_f = reshape(...
              tr_filterDP(reshape(points, [456 16]), 1000, cutoff, 'low', 2),...
              [456 8 2]);

set(figure(2),'WindowStyle','docked')
hold on
plot(points(:,1,1))
plot(points_f(:,1,1))
title('Filtered data')
legend('raw', 'filtered')

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

%% Reorder data
% Takeoff and touchdown frames for both legs estimated from video
% Videos recorded at 1000 Hz and synchronised to takeoff of markered leg
% Number of frames: 456
%                  ipsilateral | contralateral
% sprinter  to/td: 58/416        286/181
% teamsport to/td: 58/413        287/175 

% Swing leg CoM during contralateral stance (index to end incase simulation
% takes longer)
swingCM(1) = {cm(181:end,:,1)};
swingCM(2) = {cm(175:end,:,2)};

% Ipsilateral thigh angle during stance (assumed to be same for both legs)
% q3(1) = {[Q(416:end,3,1); Q(1:58,3,1)]};
% q3(2) = {[Q(413:end,3,2); Q(1:58,3,2)]};

set(figure(4),'WindowStyle','docked')
subplot(2,1,1); hold on; cla
plot(swingCM{1}(1:105,1)); plot(swingCM{2}(1:112,1));
title('CoM position relative to hip joint')
ylabel('x')
legend('sprinter', 'teamsport', 'location', 'southeast')

subplot(2,1,2); hold on; cla
plot(swingCM{1}(1:105,2)); plot(swingCM{2}(1:112,2));
ylabel('y')

%% Derivatives 
% Swing CoM
swingCM(2,:) = cellfun(@(x) tr_diff(x, 0.001), swingCM(1,:), ...
                        'UniformOutput', false);
swingCM(3,:) = cellfun(@(x) tr_diff(x, 0.001), swingCM(2,:), ...
                        'UniformOutput', false);

% Plot 
set(figure(5),'WindowStyle','docked')
set(figure(5), 'DefaultLineMarkerSize', 1.5)
titles = {'position','velocity','acceleration'};
for i = 1:3
    subplot(3,2,2*i-1); hold on; cla
    plot(swingCM{i,1}(1:105,1), 's')
    plot(swingCM{i,2}(1:112,1), 's')
    title([titles{i} ' x'])
    
    subplot(3,2,2*i); hold on; cla
    plot(swingCM{i,1}(1:105,2), 's')
    plot(swingCM{i,2}(1:112,2), 's')
    title([titles{i} ' y'])
end
subplot(3,2,2); legend('sprinter', 'teamsport');

%% Export data
if output
    % Column names
    colwidth = 13;
    precision = 5;
    colnames = ["PositionX","PositionY","VelocityX","VelocityY",...
                "AccelerationX","AccelerationY"];
    nametype = ['%-' num2str(colwidth) 's'];
    datatype = ['%' num2str(colwidth) '.' num2str(precision) 'E'];
    namefmt = [repmat([nametype ' '], 1, size(colnames,2)-1) nametype '\n'];
    datafmt = [repmat([datatype ' '], 1, size(colnames,2)-1) datatype '\n'];
    
    % Sprinter's technique
    fid = fopen('sprinter.txt', 'w');
    fprintf(fid, '%4d', size(swingCM{1,1},1));  % Data size
    fprintf(fid, '%4d', 2);                     % Header rows
    fprintf(fid, '%4d\n', colwidth+1);          % Column width
    fprintf(fid, namefmt, pad(colnames, colwidth, 'left'));
    fprintf(fid, datafmt, ...
                string(reshape(cat(3,swingCM{:,1}), ...
                        [size(swingCM{1,1}, 1) 6])));
    fclose(fid);
    
    % Teamsports's technique
    fid = fopen('teamsport.txt', 'w');
    fprintf(fid, '%4d', size(swingCM{1,2},1));  % Data size
    fprintf(fid, '%4d', 2);                     % Header rows
    fprintf(fid, '%4d\n', colwidth+1);          % Column width
    fprintf(fid, namefmt, pad(colnames, colwidth, 'left'));
    fprintf(fid, datafmt, ...
                string(reshape(cat(3,swingCM{:,2}), ...
                        [size(swingCM{1,2}, 1) 6])));
    fclose(fid);


end

