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
hz = data.Parameters.POINT.RATE;
points = data.Markers.Data ./ 1000;     % In metres
mnames = data.Markers.Names;
mnames = strrep(mnames, '_', '');       % Remove all '_' from marker names
M = 89.3;
draw = 0;                               % Draw segments with CoM's

% Remove combined HJC and SJC
points(:,:, strcmp(mnames,'HJC')|strcmp(mnames,'SJC')) = [];
mnames(strcmp(mnames,'HJC')|strcmp(mnames,'SJC')) = [];


% Set up sgement objects
segments = {'foot', 'shank', 'thigh', 'lwTrunk', 'upTrunk', 'head',...
            'upArm', 'lwArm'};
proxJC = {'AJC', 'KJC', 'HJC', 'HJC', 'LTJC', 'UTJC', 'SJC', 'EJC'};
distJC = {'Toe', 'AJC', 'KJC', 'LTJC', 'UTJC', 'APEX', 'EJC', 'WJC'};
    
% Inertia 
inertia = {[0.224 1.385 0.142 0.0096];  	% Foot segment
           [0.453 5.271 0.261 0.0792];      % Shank segment
           [0.447 12.54 0.258 0.2122];      % Thigh segment
           [0.418 24.43 0.323 0.5218];      % Lower trunk segment
           [0.182 10.86 0.085 0.0701];      % Upper trunk segment
           [0.269 5.611 0.136 0.0344];      % Head + neck segment
           [0.323 3.162 0.185 0.0588];      % Upper arm segment
           [0.469 1.855 0.301 0.0568]};     % Lower arm segment
        
proxKey = containers.Map(segments, proxJC);
distKey = containers.Map(segments, distJC);
inertiaKey = containers.Map(segments, inertia);
       
%% Get CoM
CM = [];
WBCM = zeros(size(points, 1), 3);
for i = 1:length(segments)
    % Load segment information
    segment = segments{i};
    prox = contains(mnames, proxKey(segment));
    dist = contains(mnames, distKey(segment));
    I = inertiaKey(segment); r = I(3)/I(1);
    
    % Calculate CoM from proximal JC
    segCM = points(:,:,prox) + (points(:,:,dist) - points(:,:,prox)) .* r;
    if strcmp(segment, 'lwTrunk')
        segCM = mean(segCM, 3); 
    end
    CM = cat(3, CM, segCM);
        
    % Whole-body CoM
    dim = size(segCM, 3);   % Multiply segment mass by number of segments
    segCM = mean(segCM, 3);
    WBCM = WBCM + segCM.*(I(2)/M)*dim;
               
end

%% Output
CMout_names = {'WBCM', 'WBCMv', 'RFoot', 'LFoot', 'RShank', 'LShank', ...
               'RThigh', 'LThigh', 'LwTrunk', 'UpTrunk', 'Head', ...
               'RUpArm', 'LUpArm', 'RLwArm', 'LLwArm'};
WBCMv = tr_diff(WBCM, 1/hz);
CMout = cat(3, WBCM, WBCMv, CM);

%% Plot to check
if draw == 1
    rTOE = points(:,:,contains(mnames, 'RToe')); lTOE = points(:,:,contains(mnames, 'LToe'));
    rAJC = points(:,:,contains(mnames, 'RAJC')); lAJC = points(:,:,contains(mnames, 'LAJC'));
    rKJC = points(:,:,contains(mnames, 'RKJC')); lKJC = points(:,:,contains(mnames, 'LKJC'));
    rHJC = points(:,:,contains(mnames, 'RHJC')); lHJC = points(:,:,contains(mnames, 'LHJC'));
    rSJC = points(:,:,contains(mnames, 'RSJC')); lSJC = points(:,:,contains(mnames, 'LSJC'));
    rEJC = points(:,:,contains(mnames, 'REJC')); lEJC = points(:,:,contains(mnames, 'LEJC'));
    rWJC = points(:,:,contains(mnames, 'RWJC')); lWJC = points(:,:,contains(mnames, 'LWJC'));
    LTJC = points(:,:,contains(mnames, 'LTJC'));
    UTJC = points(:,:,contains(mnames, 'UTJC'));
    APEX = points(:,:,contains(mnames, 'APEX'));
    HJC = (rHJC + lHJC) ./ 2;

    set(figure(1),'WindowStyle','docked'); cla
    axis equal
    hold on
    for i = 1%:5:length(points)
        cla
        % Right leg
        line([rTOE(1,2) rAJC(1,2) rKJC(1,2) rHJC(1,2)], ...
             [rTOE(1,3) rAJC(1,3) rKJC(1,3) rHJC(1,3)])    
        % Left leg
        line([lTOE(1,2) lAJC(1,2) lKJC(1,2) lHJC(1,2)], ...
             [lTOE(1,3) lAJC(1,3) lKJC(1,3) lHJC(1,3)])
        % Trunk
        line([HJC(1,2) LTJC(1,2) UTJC(1,2) APEX(1,2)], ...
             [HJC(1,3) LTJC(1,3) UTJC(1,3) APEX(1,3)])
        % Right arm
        line([rSJC(1,2) rEJC(1,2) rWJC(1,2)], ...
             [rSJC(1,3) rEJC(1,3) rWJC(1,3)])
        % Left arm
        line([lSJC(1,2) lEJC(1,2) lWJC(1,2)], ...
             [lSJC(1,3) lEJC(1,3) lWJC(1,3)])
        % Whole-body CoM
        plot(WBCM(i,2), WBCM(i,3), 'ko') 
        drawnow
    end

    if i == 1
        for i = 1:size(CM, 3)
            plot(CM(1,2,i), CM(1,3,i), 'kx')
        end 
    end
end

%% Clear variables 
clearvars -except CMout