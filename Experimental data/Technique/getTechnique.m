clear; close all; clc;
%% Import data
sprinter = importdata('sprinter_xypts.csv', ',', 1);
teamsport = importdata('teamsport_xypts.csv', ',', 1);
load ..\data.mat

output = 0;     % Write to output

%% Fill gaps with elite's hip marker
set(figure(),'WindowStyle','docked'); cla
hold on
plot(sprinter.data(:,9:10), 'k-')

while ~all(all(~isnan(sprinter.data)))
    % Find start and end of first missing section
    idx1 = find(isnan(sprinter.data(:,1)), 1);
    idx2 = idx1 + find(~isnan(sprinter.data(idx1:end,1)), 1) -1;
    
    % Cublic spline interpolation
    sprinter.data(idx1:idx2, 1:2) = interp1([idx1-1 idx2]', ...
               [sprinter.data(idx1-1,1:2); sprinter.data(idx2,1:2)], ...
               idx1:idx2, 'spline');
                                  
   % Plot filled section
   plot(idx1:idx2, sprinter.data(idx1:idx2,9:10), 'k--')
    
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
sz = size(points); sz2 = [sz(1) sz(2)*sz(3)];
points_f = reshape(...
              tr_filterDP(reshape(points, sz2), 1000, cutoff, 'low', 2),...
              sz);

set(figure(2),'WindowStyle','docked'); cla
hold on
plot(points(:,10,1))
plot(points_f(:,10,1))
title('Filtered data')
legend('raw', 'filtered')

%% Calculate segment angles
Q = nan(size(points_f, 1), 3, 2);

% Foot, shank, thigh
for i = 1:3
    n = 9-2*i;        % Index sequence
    
    % Prox and dist JC
    p1 = points_f(:,n:n+1,:); p2 = points_f(:,n-2:n-1,:);   
    d = p2 - p1;        % dx,dy
    Q(:,i,:) = atan2d(d(:,2,:), d(:,1,:));      % Segment angles
end

Qv = tr_diff(Q,0.001);

% Plot to check
% set(figure(3),'WindowStyle','docked'); cla
% for i = 1:2:length(points_f)
%     subplot(2,1,1)
%     cla;
%     xlim([0 600]); ylim([-100 500]); %axis equal
%     line([points_f(i,7,1) points_f(i,5,1) points_f(i,3,1) points_f(i,1,1) points_f(i,9,1)],...
%          [points_f(i,8,1) points_f(i,6,1) points_f(i,4,1) points_f(i,2,1) points_f(i,10,1)])
%          
%     subplot(2,1,2)
%     cla
%     hold on
% %     plot(q_B)
% %     plot(i,q_B(i,:), 'ko')
%     legend('foot','shank','thigh')
%     
%     drawnow
% end

%% Calculate CoM
% Segment: mass, length, CMprox, CMdist
% Foot:    1.147, 0.139, 0.063, 0.076
% Shank:   5.271, 0.453, 0.192, 0.261          
% Thigh:   12.54, 0.447, 0.189, 0.258
%
% CMX = (MA*(L1-L2)*COS(Q1)-(L4*MA-MB*(L3-L4))*COS(Q2)-(L6*MA+L6*MB-MC*(L5-L6))*COS(Q3))/(MA+MB+MC)
% CMY = (MA*(L1-L2)*SIN(Q1)-(L4*MA-MB*(L3-L4))*SIN(Q2)-(L6*MA+L6*MB-MC*(L5-L6))*SIN(Q3))/(MA+MB+MC)

l1=0.076; l2=0.139; l3=0.261; l4=0.453; l5=0.258; l6=0.447;
ma=1.147; mb=5.271; mc=12.54;

% CoM location x,y
cm(:,1,:) = (ma*(l1-l2).*cosd(Q(:,1,:))-(l4*ma-mb*(l3-l4)).* ...
            cosd(Q(:,2,:))-(l6*ma+l6*mb-mc*(l5-l6)).* ...
            cosd(Q(:,3,:)))/(ma+mb+mc);
cm(:,2,:) = (ma*(l1-l2).*sind(Q(:,1,:))-(l4*ma-mb*(l3-l4)).* ...
            sind(Q(:,2,:))-(l6*ma+l6*mb-mc*(l5-l6)).* ...
            sind(Q(:,3,:)))/(ma+mb+mc);
        
%% Joint angles
% Trunk angle 
p1 = points_f(:,1:2,:); p2 = points_f(:,9:10,:);   
d = p2 - p1;        % dx,dy
trunk = atan2d(d(:,2,:), d(:,1,:)); 
trunk2 = dout.Average.Angles.Data.Avg(:,1,1);
trunk2v = tr_diff(trunk2, .001);
to1_s = 58; td1_s = 416; to2_s = 286; td2_s = 181;
to1_t = 58; td1_t = 413; to2_t = 287; td2_t = 175;

hipang_s = 180 - Q(td2_s:to2_s,3,1) + trunk2(1:(to2_s-td2_s+1));
hipang_t = 180 - Q(td2_t:to2_t,3,2) + trunk2(1:(to2_t-td2_t+1));
hipangv_s = trunk2v(td1_s) - Qv(td1_s,3,1);
hipangv_t = trunk2v(td1_t) - Qv(td1_t,3,2);

% hipangs = 180 - [Q(416:end,3,1); Q(1:415,3,1)] + trunk2(1:456);
% hipangt = 180 - [Q(416:end,3,2); Q(1:415,3,2)] + trunk2(1:456);
% hipang = 180 - Q(:,3,:) + trunk;
kneang = 180 - Q(:,3,:) + Q(:,2,:);
ankang = 180 - Q(:,1,:) + Q(:,2,:);

% dout.Average.Angles.Data.Avg(1,1,contains(dout.Average.Angles.Names, 'LThighAngularVelocity'))
% Angular velocites
% hipangv = tr_diff(hipang,0.001);
kneangv = tr_diff(kneang,0.001);
ankangv = tr_diff(ankang,0.001);

% Initial conditions - assume trunk stays the same 
initcond_s = [(180-Q(td1_s,3,1)+trunk2(1)) kneang(td1_s,1,1) ankang(td1_s,1,1) ...
              hipangv_s kneangv(td1_s,1,1) ankangv(td1_s,1,1)];
initcond_t = [(180-Q(td1_t,3,2)+trunk2(1)) kneang(td1_t,1,2) ankang(td1_t,1,2) ...
              hipangv_t kneangv(td1_t,1,2) ankangv(td1_t,1,2)];

set(figure(2),'WindowStyle','docked')
subplot(3,1,1); hold on; cla
plot(hipang(:,:,1))
plot(hipang(:,:,2))
title('hip')

subplot(3,1,2); hold on; cla
plot(kneang(:,:,1))
plot(kneang(:,:,2))
title('knee')

subplot(3,1,3); hold on; cla
plot(ankang(:,:,1))
plot(ankang(:,:,2))
title('ankle')
legend('sprinter','teamsport')

% set(figure(3),'WIndowStyle','docked')
% subplot(3,1,1); hold on; cla
% plot(hipangv(:,:,1))
% plot(hipangv(:,:,2))
% title('hip')
% 
% subplot(3,1,2); hold on; cla
% plot(kneangv(:,:,1))
% plot(kneangv(:,:,2))
% title('knee')
% 
% subplot(3,1,3); hold on; cla
% plot(ankangv(:,:,1))
% plot(ankangv(:,:,2))
% title('ankle')
% legend('sprinter','teamsport')

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
hip(1) = {hipang(181:end,1,1)};
hip(2) = {hipang(175:end,1,2)};
knee(1) = {kneang(181:end,1,1)};
knee(2) = {kneang(175:end,1,2)};

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

hip(2,:) = cellfun(@(x) tr_diff(x, 0.001), hip(1,:), ...
                        'UniformOutput', false);
hip(3,:) = cellfun(@(x) tr_diff(x, 0.001), hip(2,:), ...
                        'UniformOutput', false);
knee(2,:) = cellfun(@(x) tr_diff(x, 0.001), knee(1,:), ...
                        'UniformOutput', false);
knee(3,:) = cellfun(@(x) tr_diff(x, 0.001), knee(2,:), ...
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

set(figure(6),'WindowStyle','docked'); cla
set(figure(6), 'DefaultLineMarkerSize', 1.5)
ynames = {'angle','angular velocity','angular acceleration'};
for i = 1:3
    subplot(3,2,2*i-1); hold on; cla
    plot(hip{i,1}(1:105,1), 's')
    plot(hip{i,2}(1:112,1), 's')
    ylabel(ynames{i})
    
    subplot(3,2,2*i); hold on; cla
    plot(knee{i,1}(1:105,1), 's')
    plot(knee{i,2}(1:112,1), 's')
end
subplot(3,2,1); title('hip')
subplot(3,2,2); title('knee')

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
                        [size(swingCM{1,1}, 1) 6])'));
    fclose(fid);
    % Joint angles
    n = length(hip{1,1}); time = (0:0.001:(n-1)*0.001)';
    writematrix([n 2 nan; time hip{1,1} knee{1,1}], 'sprinter.csv');
    
    % Teamsports's technique
    fid = fopen('teamsport.txt', 'w');
    fprintf(fid, '%4d', size(swingCM{1,2},1));  % Data size
    fprintf(fid, '%4d', 2);                     % Header rows
    fprintf(fid, '%4d\n', colwidth+1);          % Column width
    fprintf(fid, namefmt, pad(colnames, colwidth, 'left'));
    fprintf(fid, datafmt, ...
                string(reshape(cat(3,swingCM{:,2}), ...
                        [size(swingCM{1,2}, 1) 6])'));
    fclose(fid);
    % Joint angles
    n = length(hip{1,2}); time = (0:0.001:(n-1)*0.001)';
    writematrix([n 2 nan; time hip{1,2} knee{1,2}], 'teamsport.csv');
end

