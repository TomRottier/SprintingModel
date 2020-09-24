function c3dTM_filt(filenames,cornernames,threshold,cutoff)
%Put force data from treadmill into c3d files:

%==========================================================================
% INPUTS:
%--------------------------------------------------------------------------
%   filenames = one or more files to open (full file name and path)
% cornernames = list of markernames used to identify the corners of the
%               treadmill in a cell array in the following order:
%
%       {front left, front right, back right, back left}
%
%   threshold = force threshold (zero force below this)
%
%   cutoff = low pass filter cut-off frequency (4th order Butterworth)
%
%--------------------------------------------------------------------------
% NOTES
%--------------------------------------------------------------------------
% 1. If no input is used then a gui is opened to select the appropriate
%    files to open and try to read the voltages and get the instrumented
%    treadmill forces.
%
% 2. If no cornernames are inputted then the defaults are used. These are:
%
%      {'Treadmill_FL','Treadmill_FR','Treadmill_BR','Treadmill_BL'};
%
% 3. If no threshold is declared then 'none' is used, and centre of
%    pressure data may be problematic when little force is applied to the
%    treadmill. A value of 50-100 N is usually good to use if you're not
%    sure
%
% 4. Default channel names for the sensors are used. If these are
%    inappropriate then I suggest you edit them below.
% 
% 5. If no low pass filter cutoff is declared, 15 hz is used.
%
% 6. Functions required:    readC3D, writeC3D, forceTM, checkC3D, filtmat2
%
%==========================================================================
% OUTPUTS:
%--------------------------------------------------------------------------
%
% none (file is written but nothing is outputted to MATLAB)
%
%==========================================================================

if nargin<1
    %get files from gui:
    [fname,pname]=uigetfile('*.c3d','Select one or more C3D data files to read...','Multiselect','on');
    if ~iscell(fname)
        fname={fname};
    end
    if fname{1}==0 %selection cancelled
        return %go back to calling function
    end
    filenames=strcat(pname,fname');
end
if ~iscell(filenames)
    filenames={filenames};
end

if nargin<2
    cornernames={'Treadmill_FL','Treadmill_FR','Treadmill_BR','Treadmill_BL'};
end

if nargin < 3
    threshold='none'; %set force threshold at 25 N
end

if nargin < 4
    cutoff=15; %set filter cutoff to 15 Hz
end

%List of sensors in order: {front left, front right, back right, back left}
CH={'Fx1','Fy1','Fz1','Fx2','Fy2','Fz2','Fx3','Fy3','Fz3','Fx4','Fy4','Fz4'};

%Declare standard names for channels:
channels={'FxTM','FyTM','FzTM','MxTM','MyTM','MzTM'};
units={'N','N','N','Nmm','Nmm','Nmm'};
description=[repmat({'Force Treadmill::Force [1]'},[3,1]);...
             repmat({'Force Treadmill::Moment [1]'},[3,1])]';

UNITS=[reshape(('N  N  N  NmmNmmNmm')',[3,6])',repmat(blanks(5),[6,1])];
LABELS=[[reshape(('Force.FxTMForce.FyTMForce.FzTM')',[10,3])',repmat(blanks(6),[3,1])];...
    [reshape(('Moment.MxTMMoment.MyTMMoment.MzTM')',[11,3])',repmat(blanks(5),[3,1])]];
DESCRIPTIONS=[[reshape([description{1:3}],[26,3])',repmat(blanks(74),[3,1])];...
              [reshape([description{4:6}],[27,3])',repmat(blanks(73),[3,1])]];

%Create rotation matrix (vicon wants FP in Kistler orientation):
R=angle2dcm(0,pi,0,'xyz'); 

%loop through files:    
for n=1:length(filenames)
    %read c3d file:
    c3d=readC3D(filenames{n},'full');
    
    %create new file name:
    Savename=filenames{n};
    Savename(strfind(Savename,'.c3d'):end+3)='_TM.c3d';

    %get force data and channels:
    [T,L]=ismember(CH,c3d.Analogue.Channels);
    if any(~T)
        CHT=CH(~T);
        for m=1:length(CHT)
            warning(['The following channel is missing: ',CHT{m}])
        end
        warning(['Skipping the current file: ',filenames{n}])
        continue
    end
    DATA=c3d.Analogue.Data(:,L);
    
    %Get force plate markers:
    [~,M]=ismember(cornernames,c3d.Markers.Names);
    corners=permute(mean(c3d.Markers.Data(:,:,M)),[3,2,1]);
    
    % filter data
    dt = c3d.Analogue.Time(2)-c3d.Analogue.Time(1);
    FDATA = filtmat2(dt, cutoff, 4, DATA, 'low');
    
    %Calculate forces, moments, etc. in TM LCS
%     [TM,options]=forceTM(DATA,'threshold',threshold); %need to use some options for this
    [TM,options]=forceTM(FDATA,'threshold',threshold); %need to use some options for this

    %Create ForcePlate structure:
    c3d.ForcePlate.Data=[(R*TM.Forces')',(R*TM.Moments')'];
    c3d.ForcePlate.Names=channels;
    c3d.ForcePlate.Time=c3d.Analogue.Time;
    c3d.ForcePlate.Corners=corners;
    c3d.ForcePlate.R=options.R;
    c3d.ForcePlate.Origin=zeros(1,3);
    
    %update Analogue structure:
    c3d.Analogue.Channels=[channels,c3d.Analogue.Channels]; %put in first 6 channels
    c3d.Analogue.Units=[units,c3d.Analogue.Units]; %put in first 6 channels
    c3d.Analogue.Descriptions=[description,c3d.Analogue.Descriptions]; %put in first 6 channels
    c3d.Analogue.Data=[c3d.ForcePlate.Data,c3d.Analogue.Data]; %put in first 6 channels
    
    %update ANALOG parameters:
    c3d.Parameters.ANALOG.USED=c3d.Parameters.ANALOG.USED+6;
    c3d.Parameters.ANALOG.GAIN=[ones(6,1);c3d.Parameters.ANALOG.GAIN];
    c3d.Parameters.ANALOG.SCALE=[ones(6,1);c3d.Parameters.ANALOG.SCALE];
    c3d.Parameters.ANALOG.OFFSET=[zeros(6,1);c3d.Parameters.ANALOG.OFFSET];
    c3d.Parameters.ANALOG.UNITS=[UNITS;c3d.Parameters.ANALOG.UNITS];
    c3d.Parameters.ANALOG.LABELS=[LABELS;c3d.Parameters.ANALOG.LABELS];
    c3d.Parameters.ANALOG.DESCRIPTIONS=[DESCRIPTIONS;c3d.Parameters.ANALOG.DESCRIPTIONS];
    
    %update FORCE_PLATFORM parameters:
    c3d.Parameters.FORCE_PLATFORM.USED=1;
    c3d.Parameters.FORCE_PLATFORM.TYPE=2;
    c3d.Parameters.FORCE_PLATFORM.CORNERS=corners;
    c3d.Parameters.FORCE_PLATFORM.ORIGIN=zeros(3,1);
    c3d.Parameters.FORCE_PLATFORM.CHANNEL=(1:6)';

    %write the c3d file:
    writeC3D(c3d,Savename)
end

end
