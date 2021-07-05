clear; close all; clc
% Comparison of torque-angle-angular velocity relationships for the two
% techniques
%% Load data
hipw = [1.2995 1.0 26.0 7.94]; hipa = [4.93 1.64];
kneew = [1.2981 1.0 36.0 5.44]; kneea = [4.31 0.74];
anklew = [1.375 1.0 30.8 15.38]; anklea = [4.22 0.37];

pw = cat(3, hipw, kneew, anklew); pa = cat(3,hipa, kneea,anklea);

color = repmat(linspace(0.15, 1.0, 256)', 1,3);

%% Torque angle angular velocity surface
n = 1000;
angvel = linspace(36, -36, n);
ang = linspace(-1, 8, n);

[W, A] = meshgrid(angvel,ang);

for i = 1:3
    Tw(:,:,i) = tqvel(-W,pw(:,:,i));
    Ta(:,:,i) = tqang(A,pa(:,:,i));
    
    T(:,:,i) = Tw(:,:,i) .* Ta(:,:,i);
end

% Plots
% Hip
set(figure(1),'WindowStyle','docked'); hold on; cla
% subplot(1,3,1); hold on; cla
% Contour
% [ck,hk] = contourf(A,W,T(:,:,1), 10);
% hk.LevelList = round(hk.LevelList, 2);
% clabel(ck,hk,'LabelSpacing', 600)

% Surface
surf(rad2deg(A),rad2deg(W),T(:,:,1), 'EdgeColor', 'interp')
% view(0,90)
% view(-51,11)
view(-31,10)

% Labels
title('Hip')
% xlabel(['Angle (' char(0176) ')']); 
% ylabel(['Angular velocity (' char(0176) '^{-1})'],...
%     'VerticalAlignment', 'bottom', 'HorizontalAlignment','center'); 
zlabel('Normalised torque')
colormap(color)
% colorbar
xlim(rad2deg([-1 8])); ylim(rad2deg([-26 26])); zlim([0 1.5])% xticks(deg2rad(0:120:400))
xticks(0:150:360)

% Knee
set(figure(2), 'WindowStyle','docked'); hold on; cla
% subplot(1,3,2); hold on; cla
% Contour
% [ca, ha] = contourf(A,W,T(:,:,2), 10);
% ha.LevelList = round(ha.LevelList, 2);
% % clabel(ca,ha,'LabelSpacing', 600)

% Surface
surf(rad2deg(A),rad2deg(W),T(:,:,2), 'EdgeColor', 'interp')
% view(0,90)
% view(-51,11)
view(-31,10)

title('Knee')
% xlabel(['Angle (' char(0176) ')']); 
% ylabel(['Angular velocity (' char(0176) '^{-1})'],...
%     'VerticalAlignment', 'bottom', 'HorizontalAlignment','center'); 
zlabel('Normalised torque')
colormap(color)
% colorbar
xlim(rad2deg([1.5 6.5])); ylim(rad2deg([-36 36])); zlim([0 1.5])% xticks(deg2rad(90:90:360))
xticks(0:150:360)

% Ankle
set(figure(3),'WindowStyle','docked'); hold on; cla
% subplot(1,3,3); hold on; cla
% Contour
% [ca, ha] = contourf(A,W,T(:,:,2), 10);
% ha.LevelList = round(ha.LevelList, 2);
% % clabel(ca,ha,'LabelSpacing', 600)

% Surface
surf(rad2deg(A),rad2deg(W),T(:,:,3), 'EdgeColor', 'interp')
% view(0,90)
% view(-51,11)
view(-31,10)

title('Ankle')
xlabel(['Angle (' char(0176) ')']);
ylabel(['Angular velocity (' char(0176) .sec'^{-1})'],...
    'VerticalAlignment', 'bottom', 'HorizontalAlignment','center'); 
zlabel('Normalised torque')
colormap(color)
% colorbar
xlim(rad2deg([3 5])); ylim(rad2deg([-31 31])); zlim([0 1.5])
xticks(180:90:360)

%% Used angles and angular velocities
pname = '';%'C:\Users\tomro\SprintingModel\Model\7Seg\Optimisations\';
fnames = 'sprinter\7segsprint.';
fnamet = 'college\7segsprint.';

% Data for take-off marker change to appear infront of surface
% Data 1: x = 212.2629; y = -430.8030; z = 0.3267
% Data 2: x = 191.9026; y = 110.1340; z = 0.6564
% Data 3: x = 218.1635; y = 630.3610; z = 0.8580
xto = [212.2629 193.2026 218.9635];
yto = [-430.8030 101.1340 630.361]; 
zto = [.3267 .6514 .858];

for i = 1:3
    % Load data
    fs = importdata([pname fnames num2str(i+6)], ' ', 8);
    ft = importdata([pname fnamet num2str(i+6)], ' ', 8);
    datas = deg2rad(fs.data(:,[4 5]));
    datat = deg2rad(ft.data(:,[4 5]));
    
    % Calculate angle/angular velocity component
    % Invert velocities as lengthening negative in function
    tas = tqang(datas(:,1), pa(:,:,i)); tat = tqang(datat(:,1), pa(:,:,i));
    tvs = tqvel(-datas(:,2), pw(:,:,i)); tvt = tqvel(-datat(:,2), pw(:,:,i));
    Ts = tas .* tvs + 0.05;
    Tt = tat .* tvt + 0.05;    % Makes plot clearer
    
    % Plot
    figure(i)
%     subplot(1,3,i)
    plot3(rad2deg(datas(:,1)), rad2deg(datas(:,2)), Ts, 'k-', 'LineWidth', 2)
    plot3(rad2deg(datat(:,1)), rad2deg(datat(:,2)), Tt, 'k:', 'LineWidth', 2)
    
    % Takeoff
    plot3(xto(i), yto(i), zto(i), ...
        'p','MarkerSize', 10 ,'MarkerFaceColor','k','MarkerEdgeColor','none')
    
end


%% Functions
function tv = tqvel(angvel, pv)
    tv = nan(size(angvel));
    
    tc = pv(2)*pv(4)/pv(3);
    c  = tc*(pv(3)+pv(4));
    we = ((pv(1)-pv(2))/(4.3*pv(2)))*((pv(3)*pv(4))/(pv(3)+pv(4)));
    e  = -we*(pv(1)-pv(2));
    
    tv(angvel < 0) = e ./ (we - angvel(angvel < 0)) + pv(1);
    tv(angvel >= 0) = c ./ (pv(4) + angvel(angvel >= 0)) - tc;


end

function ta = tqang(ang, pa)
    ta = exp((-(ang-pa(1)).^2) / (2.0*pa(2).^2));
end
