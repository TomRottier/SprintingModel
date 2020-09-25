clear; close all; clc;
%% Import data
sprinter = importdata('sprinter_xypts.csv', ',', 1);
teamsport = importdata('teamsport_xypts.csv', ',', 1);

% Hip, knee, ankle, mtp
pointsG = sprinter.data;
pointsB = teamsport.data;

% Fill gaps with elite's hip marker
set(figure(1),'WIndowStyle','docked'); cla
hold on
plot(pointsG(:,1:2), 'k-')

while ~all(~isnan(pointsG(:,1)))
    % Find start and end of first missing section
    idx1 = find(isnan(pointsG(:,1)), 1);
    idx2 = idx1 + find(~isnan(pointsG(idx1:end,1)), 1);
    
    % Cublic spline interpolation
    pointsG(idx1:idx2, 1:2) = interp1(1:length(pointsG), pointsG(:,1:2), ...
                                      idx1:idx2, 'spline');
                                  
   % Plot filled section
   plot(idx1:idx2, pointsG(idx1:idx2,1:2), 'k--')
    
end

%% Process data, filter etc
set(figure(2), 'WindowStyle','docked'); cla
hold on
for i = 1:size(pointsG, 2)
%     [power(:,i), freqs] = tr_frequencyAnalysis(pointsG(:,i), 1000, 0.99)
%     plot(freqs, power(:,i))
    [cutoff(i), rmse(:,i), freqs] = tr_residualAnalysis(pointsG(:,i), 100);
    plot(freqs, rmse(:,i))
end

pointsG_filt = tr_filterDP(pointsG, 

%% Calculate segment angles

% Calculate CoM

% Fit function