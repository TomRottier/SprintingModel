function CMout = getCM(data, draw)
% Returns segmental and whole-body CoM locations for a 13 segment
% rigid-body model.
% Inputs:
%   - data:  c3d file structure (from readC3D.m)
%   - draw:  logical, draw plots or not
%
% Outputs:
%   - CMout: 3D array containing whole-body CoM location (x,y,z) and 
%            velocity along with CoM locations for each segment
%
%--------------------------------------------------------------------------
% Inertia data taken from McErlain-Naylor (2017) 
% Actual body mass 88.6 kg (when measured) 91.1 kg (when running)
% length | mass | CM dist (prox end) | MoI about CoM
% Everything distal to HJC
% Combined foot ignores only accounts for length and total mass
%
% FFoot = [0.085 0.477 0.035 0.0003];   % Fore foot
% RFoot = [0.139 1.147 0.063 0.0059];   % Rear foot
% Foot  = [0.219 1.385 0.063 0.0059];   % Combined (avg length from data)
% Shank = [0.453 5.271 0.192 0.0792];   % Shank
% Thigh = [0.447 12.54 0.189 0.2122];   % Thigh
% UpArm = [0.323 3.162 0.138 0.0588];   % Upper arm
% LwArm = [0.469 1.855 0.168 0.0568];   % Lower arm
% LTrnk = [0.418 24.43 0.323 0.5218];   % HJC to Xyphoid/T10
% UTrnk = [0.182 10.86 0.085 0.0701];   % Xyphoid/T10 to C7/sternum
% Head  = [0.269 5.611 0.136 0.0344];   % Head + neck
% HT    = [0.869 40.90 0.427 1.4890];   % Combined head and trunk 
%
%--------------------------------------------------------------------------
% clear; close all; clc;
% p = [pwd '\Vicon\Vicon files\'];
% f = 'run9_7_TM.c3d';
% data = readC3D([p f]);

%% Import data
hz = data.Parameters.POINT.RATE;
points = data.Markers.Data ./ 1000;     % In metres
mnames = data.Markers.Names(134:end);   % Only modelled markers
% mnames = strrep(mnames, '_', '');       % Remove all '_' from marker names
M = 89.3;

% Output structure
CMout = struct('Names', [], 'Data', [], 'Inertia', []);

% Set up sgement objects
segments = {'FFoot','RFoot', 'Shank', 'Thigh', 'LwTrunk', 'UpTrunk', ...
            'Head', 'UpArm', 'LwArm'};
proxJC = {'MTP', 'AJC', 'KJC', 'HJC', 'HJC', 'LTJC', 'UTJC', 'SJC', 'EJC'};
distJC = {'TOE', 'MTP', 'AJC', 'KJC', 'LTJC', 'UTJC', 'APEX', 'EJC', 'WJC'};
    
% Inertia 
inertia = {[0.085 0.477 0.035 0.0003];   % Fore foot
           [0.139 1.147 0.063 0.0059];   % Rear foot
           [0.453 5.271 0.192 0.0792];   % Shank
           [0.447 12.54 0.189 0.2122];   % Thigh
           [0.418 24.43 0.323 0.5218];   % HJC to Xyphoid/T10
           [0.182 10.86 0.085 0.0701];   % Xyphoid/T10 to C7/sternum
           [0.269 5.611 0.136 0.0344];   % Head + neck
           [0.323 3.162 0.138 0.0588];   % Upper arm
           [0.469 1.855 0.168 0.0568]};  % Lower arm
       
proxKey = containers.Map(segments, proxJC);
distKey = containers.Map(segments, distJC);
inertiaKey = containers.Map(segments, inertia);
       
% Output
CMout.Names = {'WBCM', 'WBCMv', 'RFFoot', 'LFFoot','RRFoot', 'LRFoot', ...
               'RShank', 'LShank', 'RThigh', 'LThigh', 'LwTrunk',  ...
               'UpTrunk', 'Head', 'RUpArm', 'LUpArm', 'RLwArm', 'LLwArm'};

CMout.Inertia.Segments = segments;
CMout.Inertia.Information = {'Segment length', 'Segment mass', ...
    'CoM location proximal', 'MoI about CoM'}';
CMout.Inertia.Data = inertia;

%% Get CoM
CM = [];
WBCM = zeros(size(points, 1), 3);
for i = 1:length(segments)
    % Load segment information
    segment = segments{i};
    prox = find(contains(mnames, proxKey(segment))) + 133;    % All markers
    dist = find(contains(mnames, distKey(segment))) + 133;
    I = inertiaKey(segment); r = I(3)/I(1);
    
    % Calculate CoM from proximal JC
    segCM = points(:,:,prox) + (points(:,:,dist) - points(:,:,prox)) .* r;
    
    % Account for both 'sides' of lower trunk
    if strcmp(segment, 'LwTrunk')
        segCM = mean(segCM, 3); 
    end
    
    % Rear foot CoM not along MTP to AJC line
    if strcmp(segment, 'RFoot')
        prox2 = find(contains(mnames, 'HEL')) + 133;    % Heel markers
        temp1 = points(:,:,prox) + (points(:,:,dist) - points(:,:,prox)) .* r;
        temp2 = points(:,:,prox2) + (points(:,:,dist) - points(:,:,prox2)) .* r;
        segCM = (temp1 + temp2 ) ./ 2;
    end
    CM = cat(3, CM, segCM);
        
    % Whole-body CoM
    segCM = sum(segCM, 3);      % Accounts for both sides
    WBCM = WBCM + segCM.*(I(2)/M);              
end

%% Swing, stance and HAT CoM
segs = {'RFFoot', 'LFFoot','RRFoot', 'LRFoot', 'RShank', 'LShank',  ...
    'RThigh', 'LThigh','LwTrunk', 'UpTrunk', 'Head', 'RUpArm', 'LUpArm', ...
    'RLwArm', 'LLwArm'};
inertia = cell2mat(inertia);
ffootM = inertia(1,2); rfootM = inertia(2,2); shankM = inertia(3,2); thighM = inertia(4,2);
lwTrunkM = inertia(5,2); upTrunkM = inertia(6,2); headM = inertia(7,2);
upArmM = inertia(8,2); lwArmM = inertia(9,2);
legM = ffootM+rfootM+shankM+thighM;
hatM = lwTrunkM+upTrunkM+headM+2*upArmM+2*lwArmM;
          
hatCM = (CM(:,:,contains(segs, 'LwTrunk')).*lwTrunkM + ...
        CM(:,:,contains(segs, 'UpTrunk')).*upTrunkM + ...
        CM(:,:,contains(segs, 'Head')).*headM + ...
        sum(CM(:,:,contains(segs, 'UpArm')), 3).*upArmM + ...
        sum(CM(:,:,contains(segs, 'LwArm')), 3).*lwArmM) ...
        ./ (hatM);
    
RCM = (CM(:,:,contains(segs, 'RFFoot')).*ffootM + ...
    CM(:,:,contains(segs, 'RRFoot')).*rfootM + ...
    CM(:,:,contains(segs, 'RShank')).*shankM + ...
    CM(:,:,contains(segs, 'RThigh')).*thighM) ./ legM;

LCM = (CM(:,:,contains(segs, 'LFFoot')).*ffootM + ...
    CM(:,:,contains(segs, 'LRFoot')).*rfootM + ...
    CM(:,:,contains(segs, 'LShank')).*shankM + ...
    CM(:,:,contains(segs, 'LThigh')).*thighM) ./ legM;

% Relative to HJC
% hatCM_HJC = hatCM - HJC;
% stanceCM_HJC = RCM - HJC;
% swingCM_HJC = LCM - HJC;

% Add names
CMout.Names = [CMout.Names  {'HAT', 'RLeg', 'LLeg'}]; %, 'HAT_hip','RLeg_hip', 'LLeg_hip'}];
    
%% Output
% Relative to stance foot toe
% origin = points(:,:,contains(mnames,'Toe'));
% [~,tempidx] = min([origin(1,3,1) origin(1,3,2)]);
% origin = origin(:,:,tempidx);
% for i = 1:size(CM, 3)
%     CM(:,:,i) = CM(:,:,i) - origin;
% end
% WBCM = WBCM - origin;

WBCMv = tr_diff(WBCM, 1/hz);
CMout.Data = cat(3, WBCM, WBCMv, CM, hatCM, RCM, LCM);%, hatCM_HJC,stanceCM_HJC, swingCM_HJC);

%% Plot to check
if draw == 1
    mnames = data.Markers.Names;
    rTOE = points(:,:,contains(mnames, 'RTOE')); lTOE = points(:,:,contains(mnames, 'LTOE'));
    rMTP = points(:,:,contains(mnames, 'RMTP')); lMTP = points(:,:,contains(mnames, 'LMTP'));
    rHEL = points(:,:,contains(mnames, 'RHEL')); lHEL = points(:,:,contains(mnames, 'LHEL'));
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
    axis equal; xlim([-0.5 2.0]); ylim([-0.1 2.4])
    hold on
    for i = 1:3:length(points)
        cla
        % Right leg
        line([rTOE(i,2) rMTP(i,2) rHEL(i,2) rAJC(i,2) rKJC(i,2) rHJC(i,2)], ...
             [rTOE(i,3) rMTP(i,3) rHEL(i,3) rAJC(i,3) rKJC(i,3) rHJC(i,3)])
        line([rMTP(i,2) rAJC(i,2)], [rMTP(i,3) rAJC(i,3)])
        % Left leg
        line([lTOE(i,2) lMTP(i,2) lHEL(i,2) lAJC(i,2) lKJC(i,2) lHJC(i,2)], ...
             [lTOE(i,3) lMTP(i,3) lHEL(i,3) lAJC(i,3) lKJC(i,3) lHJC(i,3)])
        line([lMTP(i,2) lAJC(i,2)], [lMTP(i,3) lAJC(i,3)])
        % Trunk
        line([HJC(i,2) LTJC(i,2) UTJC(i,2) APEX(i,2)], ...
             [HJC(i,3) LTJC(i,3) UTJC(i,3) APEX(i,3)])
        % Right arm
        line([rSJC(i,2) rEJC(i,2) rWJC(i,2)], ...
             [rSJC(i,3) rEJC(i,3) rWJC(i,3)])
        % Left arm
        line([lSJC(i,2) lEJC(i,2) lWJC(i,2)], ...
             [lSJC(i,3) lEJC(i,3) lWJC(i,3)])
        % Whole-body CoM
        plot(WBCM(i,2), WBCM(i,3), 'ko') 
    

        for j = 1:size(CM, 3)
            plot(CM(i,2,j), CM(i,3,j), 'kx')
        end 
    drawnow
    end
end

end