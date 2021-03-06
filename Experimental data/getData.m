clear; close all; clc; warning off MATLAB:interp1:NaNstrip;
%% Import data
output = 1;         % Output data to .mat and .txt
p = [pwd '\Vicon\Vicon files\'];
fname = 'run9_7_TM.c3d';
din = readC3D([p fname]);
CMout = getCM(din, 0);

%% Alternate angle definitions
% Trunk angle (angle of HAT CoM relative to stance HJC)
lHJC = din.Markers.Data(:,:,contains(din.Markers.Names, 'LHJC')) ./ 1000;   % Stance leg
HATang = atan2d(CMout.Data(:,3,contains(CMout.Names, 'HAT')) - lHJC(:,3), ....
                CMout.Data(:,2,contains(CMout.Names, 'HAT')) - lHJC(:,2));
HATang = [HATang zeros(size(HATang)) zeros(size(HATang))];
din.ModelOutputs.Angles.Data = cat(3, HATang, din.ModelOutputs.Angles.Data);
din.ModelOutputs.Angles.Names = [{'HATAngles'}; din.ModelOutputs.Angles.Names ];  

clearvars HATang lHJC

%% Set up output structure
dout = struct('IndvStrides', [], 'Average', []);
si = eval('dout.IndvStrides');       % Shortcut to individual strides
sa = eval('dout.Average');           % Shortcut to average stride             
[sa.Force.Names, si.Force.Names] = deal(din.ForcePlate.Names);
[sa.Angles.Names, si.Angles.Names] = deal([din.ModelOutputs.Angles.Names; ...
    strrep(din.ModelOutputs.Angles.Names,'Angles','AngularVelocity')]);
[sa.Moments.Names, si.Moments.Names] = deal(din.ModelOutputs.Moments.Names);
[sa.Markers.Names, si.Markers.Names] = deal(din.Markers.Names);
                                        
[sa.CoM.Names, si.CoM.Names] = deal(CMout.Names);

%% Process data
% Match sampling frequency of data
hz = 1000;          % Desired frequency
fhz = din.Parameters.ANALOG.RATE; mhz = din.Parameters.POINT.RATE;
time = (0:1/hz:din.ForcePlate.Time(end))';
force = interp1(din.ForcePlate.Time, din.ForcePlate.Data, time, 'spline');
angles = interp1(din.Markers.Time, din.ModelOutputs.Angles.Data, time, 'spline');
angles = cat(3, angles, tr_diff(angles, 1/hz));     % Angular velocities 
moments = interp1(din.Markers.Time, din.ModelOutputs.Moments.Data, time, 'spline') ./ 1000;     % N.m
com = interp1(din.Markers.Time, CMout.Data, time, 'spline');
markers = interp1(din.Markers.Time, din.Markers.Data, time, 'spline') ./ 1000; % m


% Get TD,TO
threshold = 80;
contacts = tr_eventDetect(force(:,3), threshold);    % Find points where Fz > threshold {TD TO]
contacts((contacts(:,2) - contacts(:,1)) < 50 ,:) = [];    % Remove contacts lasting < 50 frames

% Filter
cutoff = 15;
angles = tr_filterDP(angles, hz, cutoff, 'low', 2);
moments = tr_filterDP(moments, hz, cutoff, 'low', 2);
com = tr_filterDP(com, hz, cutoff, 'low', 2);
markers = tr_filterDP(markers, hz, cutoff, 'low', 2);

% Remove force from aerial phase
for i = 1:length(contacts)-1
    force(contacts(i,2):contacts(i+1,1)-1,:) = 0;
end
force(1:contacts(1,1)-1,:) = 0.0;
force(contacts(end,2)+1:end,:) = 0.0;

% Info in dout
dout.Information.Rate = hz;
dout.Information.Threshold = threshold;
dout.Information.Inertia = CMout.Inertia;
dout.Information.Filter.Cutoff = cutoff;
dout.Information.Filter.Type = 'Low pass butterworth';
dout.Information.Filter.Passes = 2;

% Tidy up
clearvars fname p fhz mhz din names time CMout CMout_names jc i cutoff ;

%% Seperate out contacts and time normalise
% First contact leg
if markers(contacts(1,1),3,contains(si.Markers.Names,'RAJC')) > ...
        markers(contacts(1,1),3,contains(si.Markers.Names,'LAJC'))  % Which AJC lower at first contact
    leg1 = 'L';
else
    leg1 = 'R';
end
si.Information.Leg = leg1;

% Fields for initial conditions
fields = {'Angles', 'Markers', 'CoM'};

% Time normalised base
tnorm = linspace(0,100,1001)';

for i = 1:length(contacts)-2
    % Stride variables
    tstr = (contacts(i+2,1) - contacts(i,1))/hz;
    tc   = (contacts(i,2) - contacts(i,1))/hz;
    ttemp = linspace(0, tstr, tstr*hz)' ./ tstr .* 100;
    
    % Calculate and store stride variables e.g. contact time,impulse etc
    si.Parameters.StrideTime(i) = tstr;
    si.Parameters.ContactTime(i) = tc;
    si.Parameters.ContactTimeIndex(i) = round(tc*dout.Information.Rate);
    
    % Time
    si.Time{i} = linspace(0, tstr, tstr*hz)';
    
    % Force
    si.Force.Data{i} = force(contacts(i,1):contacts(i+2,1)-1,:);
    % Remove second contacts force
    si.Force.Data{i}(find(si.Force.Data{i}(:,3) < threshold, 1):end,:) = 0;
    si.Force.DataNorm{i} = interp1(ttemp, si.Force.Data{i}, tnorm,  'spline');
    % Remove force data below threshold 
    si.Force.DataNorm{i}((si.Force.DataNorm{i}(:,3) < threshold),:) = 0;
    
    
    % Angles
    si.Angles.Data{i} = angles(contacts(i,1):contacts(i+2,1)-1,:,:);
    si.Angles.DataNorm{i} = interp1(ttemp, si.Angles.Data{i}, tnorm,  'spline');
    
    % Moments
    si.Moments.Data{i} = moments(contacts(i,1):contacts(i+2,1)-1,:,:);
    si.Moments.DataNorm{i} = interp1(ttemp, si.Moments.Data{i}, tnorm,  'spline');
    
    % Markers
    si.Markers.Data{i} = markers(contacts(i,1):contacts(i+2,1)-1,:,:);
    si.Markers.DataNorm{i} = interp1(ttemp, si.Markers.Data{i}, tnorm,  'spline');
        
    % CoM
    si.CoM.Data{i} = com(contacts(i,1):contacts(i+2,1)-1,:,:);
    si.CoM.DataNorm{i} = interp1(ttemp, si.CoM.Data{i}, tnorm,  'spline');
    
    % Initial conditions
    for j = 1:length(fields)
        si.InitialConditions.(fields{j}) = si.(fields{j}).('Data'){i}(1,:,:);
    end 
end

% Tidy up
dout.IndvStrides = si;
clearvars force angles moments markers jointcentres com ttemp tstr tc i j
%% Average stride
n = 6;                            % Number of strides - takes middle n 
idx = 1:2:length(contacts);       
idx = idx(n-(n/2)+1:n+n/2);       % Indicies of strides used

% Average stride parameters
sa.Parameters.StrideTime = mean(si.Parameters.StrideTime(idx));
sa.Parameters.ContactTime   = mean(si.Parameters.ContactTime(idx));
sa.Parameters.ContactTimeIndex   = round(mean(si.Parameters.ContactTimeIndex(idx)));

% Average stride
fields = fieldnames(sa);
tabs = (0:1/hz:sa.Parameters.StrideTime)';
for i = 1:length(fields)-1
    sa.(fields{i}).('DataNorm').('Avg') = mean(cell2mat(reshape( ...
        si.(fields{i}).('DataNorm')(idx), 1, 1, 1, [])), 4);
    sa.(fields{i}).('DataNorm').('Std') = std(cell2mat(reshape( ...
        si.(fields{i}).('DataNorm')(idx), 1, 1, 1, [])), [], 4);
    sa.(fields{i}).('Data').('Avg') = interp1(tnorm/100*sa.Parameters.StrideTime, ...
        sa.(fields{i}).('DataNorm').('Avg'),...
        tabs, 'spline');
    sa.(fields{i}).('Data').('Std') = interp1(tnorm/100*sa.Parameters.StrideTime, ...
        sa.(fields{i}).('DataNorm').('Std'), tabs, 'spline');
end 

% Remove forces below threshold
sa.Force.Data.Avg((sa.Force.Data.Avg(:,3) < threshold),:) = 0;

% First contact leg
if sa.Markers.Data.Avg(1,3,contains(sa.Markers.Names,'RAJC')) > ...
        sa.Markers.Data.Avg(1,3,contains(sa.Markers.Names,'LAJC'))  % Which AJC lower at first contact
    leg1 = 'L';
else
    leg1 = 'R';
end
sa.Information.Leg = leg1;
sa.Information.StridesUsed = idx;
sa.Information.NStrides = length(contacts);

% Tidy up
sa.Time.Normalised = tnorm;
sa.Time.Absolute = tabs;
clearvars si tabs tnorm fields contacts n mid idx i hz leg1 leg2 threshold

%% Initial conditions
fields = {'Angles', 'Markers', 'CoM'};
for i = 1:length(fields)
    sa.InitialConditions.(fields{i}) = sa.(fields{i}).('Data').('Avg')(1,:,:);
end 

% Tidy up
clearvars fields i 

%% Output
dout.Average = sa;
clearvars sa 
if output
    clearvars output
    save data.mat           % Saves to .mat
    
    % Save to .csv
    n = size(dout.Average.Force.Data.Avg, 1);
    leg = dout.Average.Information.Leg;
    
    idx = contains(dout.Average.Angles.Names,...
        {'HATAngles', [leg 'HipAngles' ], [leg 'KneeAngles'] , ...
        [leg 'AnkleAngles'] [leg 'MTPAngles']});  
    colnames = [{'Time'}; dout.Average.Angles.Names(idx)];
    out = [num2str(n) num2str(length(colnames)) strings(1,length(colnames)-2);
        colnames'
        dout.Average.Time.Absolute permute(dout.Average.Angles.Data.Avg(:,1,idx), [1 3 2])];
    writematrix(out, 'matchingData.csv');
        
end

clearvars -except dout
