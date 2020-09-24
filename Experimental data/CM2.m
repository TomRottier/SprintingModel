% Inertia data taken from McErlain-Naylor (2017) 
% Actual body mass 88.6 kg (when measured) 91.1 kg (when running)
% [segment length, segment mass, CM location from prox end, MoI about CM]
% RFoot = [0.139 1.147 0.076 0.0059];   % Rear foot
% FFoot = [0.085 0.477 0.035 0.0003];   % Fore foot
% Foot  = [0.224 1.385 0.142 0.0096];   % Combined foot
% Shank = [0.453 5.271 0.261 0.0792];   % Shank
% Thigh = [0.447 12.54 0.258 0.2122];   % Thigh
% UpArm = [0.323 3.162 0.185 0.0588];   % Upper arm
% LwArm = [0.469 1.855 0.301 0.0568];   % Lower arm
% LTrnk = [0.418 24.43 0.323 0.5218];   % HJC to Xyphoid/T10
% UTrnk = [0.182 10.86 0.085 0.0701];   % Xyphoid/T10 to C7/sternum
% Head  = [0.269 5.611 0.136 0.0344];   % Head + neck
% HT    = [0.869 40.90 0.427 1.4890];   % Combined head and trunk
%
%--------------------------------------------------------------------------
clear; close all; clc;
%% Import data
p = [pwd '\Vicon\Vicon files\'];
f = 'run9_7_TM.c3d';
data = readC3D([p f]);
points = data.Markers.Data ./ 1000;     % In metres
mnames = data.Markers.Names;
mnames = strrep(mnames, '_', '');       % Remove all '_' from marker names

names = {'Toe', 'AJC', 'KJC', 'HJC', 'LTJC', 'UTJC', 'SJC', 'EJC'};
     
% Inertia data     
inertia =  {[0.224 1.385 0.142 0.0096]    % Combined foot
            [0.453 5.271 0.261 0.0792]     % Shank
            [0.447 12.54 0.258 0.2122]     % Thigh
            [0.418 24.43 0.323 0.5218]     % Lower trunk
            [0.182 10.86 0.085 0.0701]     % Upper trunk
            [0.269 5.611 0.136 0.0344]     % Head + neck
            [0.323 3.162 0.185 0.0588]     % Upper arm
            [0.469 1.855 0.301 0.0568]};   % Lower arm


% Change CoM location to distal end
for i = 1:length(inertia)
    inertia{i}(:,3) = inertia{i}(:,1) - inertia{i}(:,3);
end

I = containers.Map(names, inertia);

%% Get CoM
n = length(names);
CM = nan(length(points),3,n*2);
for i = 1:n-1
    
    % ======================
    % Need to change what happens when it gets to trunk segment
    % ======================
    
    
    % Right leg
    leg = 'R'; if contains(names{i+1}, 'TJC') leg = ''; end
    temp = I(names{i});
    CM(:,:,i) = points(:,:,contains(mnames, [leg names{i}])) + ...
                (points(:,:,contains(mnames,[leg names{i+1}])) - ...
                points(:,:,contains(mnames,[leg names{i}]))) .* ...
                (temp(3)/temp(1));
    % Left leg
    leg = 'L'; 
    CM(:,:,i+n) = points(:,:,contains(mnames, [leg names{i}])) + ...
                  (points(:,:,contains(mnames,[leg names{i+1}])) - ...
                  points(:,:,contains(mnames,[leg names{i}]))) .* ...
                  (temp(3)/temp(1));
               
end