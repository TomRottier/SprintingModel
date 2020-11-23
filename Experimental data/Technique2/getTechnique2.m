clear; close all; clc
%% Import data
sprinter = importdata('sprinter_xypts.csv', ',' , 1);
teamsport = importdata('teamsport_xypts.csv', ',', 1);

% Get index of first digitised point (same for both)
idx1 = find(~isnan(sprinter.data(:,1)), 1);
idx2 = find(~isnan(sprinter.data(:,1)), 1, 'last');

% Combine along thrid dimension
data = cat(3, sprinter.data(idx1:idx2,:), teamsport.data(idx1:idx2,:));

output = 0;

%% Filter data
% Find appropiate cutoff
% for i = 1:size(data, 2)
%     col = i;
%     set(figure(2), 'WindowStyle','docked'); cla
%     hold on
%     plot(data(:,col, 1), 'k--', 'DisplayName', 'raw')
%     cutoffs = [8 10 15 20];
%     for j = 1:length(cutoffs)
%         pfilt = tr_filterDP(data(:,col, 1), 1000, cutoffs(j), 'low', 2);
%         plot(pfilt, 'DisplayName', num2str(cutoffs(j)), 'LineWidth', 1.5)
%     end
%     legend('show', 'location', 'bestoutside')
%     keyboard;
% %     pause(1);
% end

cutoff = 15;

% Reorder to 2D array then filter
dataf = tr_filterDP(reshape(data, [],20), 1000, cutoff, 'low', 2);
dataf = reshape(dataf, [], 10, 2);

% set(figure(2),'WindowStyle','docked'); cla
% hold on
% plot(data(:,10,1))
% plot(dataf(:,10,1))
% title('Filtered data')
% legend('raw', 'filtered')

%% Segment angles
Q = nan(size(dataf, 1), 4, 2);

% Foot, shank, thigh, trunk
for i = 1:4
    n = 2*i - 1;    % Index sequence
    % Prox (p2) and dist (p1) JC
    p1 = dataf(:,[n n+1],:); p2 = dataf(:,[n+2 n+3],:);
    d = p2 - p1;
    % Segment angle
    Q(:,i,:) = atan2d(d(:,2,:), d(:,1,:));
end

% Correct angle definitions
% Q(:,[2 3],:) = Q(:,[2 3],:) + 90;

% Angular velocities
Qp = tr_diff(Q,0.001);

%% Joint angles
hipang = 180 - Q(:,3,:) + Q(:,4,:);
kneang = 180 - Q(:,3,:) + Q(:,2,:);
ankang = 180 - Q(:,1,:) + Q(:,2,:);

% Angular velocities
hipangv = tr_diff(hipang, 0.001);
kneangv = tr_diff(kneang, 0.001);
ankangv = tr_diff(ankang, 0.001);

%%  CoM
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


%% Output
if output
    td_s = 41;      % Opposite leg touchdown 41 frames from start
    td_t = 35;
    n_s = length(hipang) - td_s;
    n_t = length(hipang) - td_t;
    
    % Write to csv
    out_s = [(0:0.001:n_s/1000)' hipang(td_s:end,1,1) kneang(td_s:end,1,1)];
    out_t = [(0:0.001:n_t/1000)' hipang(td_t:end,1,2) kneang(td_t:end,1,2)];
    
    writematrix([string(n_s) string(2) string(); out_s], 'sprinter.csv')
    writematrix([string(n_t) string(2) string(); out_t], 'teamsport.csv') 
end

%% Draw
% set(figure(1),'WindowStyle','docked'); hold on
% xlim([300 600]); ylim([0 700]); axis equal
% for i = 1:length(dataf)
%     cla
%     line(dataf(i,1:2:10,1), dataf(i,2:2:10,1))
%     drawnow
% end