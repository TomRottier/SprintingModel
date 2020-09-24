% File name
fname = 'Run9_7.c3d';
f = [pwd '\Vicon files\' fname];

% Uniflitered force
% cornernames={'Treadmill_FL','Treadmill_BL','Treadmill_BR','Treadmill_FR'};
% c3dTM(f, cornernames, 80);

% Filtered force
% filenames = {'run8.c3d','run9.c3d','run9_5.c3d','run9_7.c3d','run9_9.c3d'};% array of filenames
cornernames={'Treadmill_FL','Treadmill_BL','Treadmill_BR','Treadmill_FR'};
threshold = 80; % threshold below which to discount CoP data
cutoff = 50; % low-pass filter cutoff frequency

c3dTM_filt(f,cornernames,threshold,cutoff)