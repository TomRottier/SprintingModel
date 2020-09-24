function writeC3D(c3d,Savename)
%writes a .c3d file

% This function will attempt to correct for any inconsistencies or missing
% inputs using other information in the structure.

%==========================================================================
% INPUTS:
%--------------------------------------------------------------------------
%
%      c3d = a single c3d structure (created from readC3D) containing:
%           - c3d.Header = Header record for file
%           - c3d.Parameters = Parameter record for file
%           - c3d.Markers = Marker data
%           - c3d.Analogue = Analogue data
%           - c3d.ForcePlate = Force Plate data
%           - c3d.ModelOutputs = Model outputs
%
% Savename = name of the file to be written
%
%--------------------------------------------------------------------------
% NOTES
%--------------------------------------------------------------------------
% 1. This function will use the checkC3D function (at bottom) to check the
%    input c3d structure is consistent and ready for writing.
%
% 2. If Header, Parameters, or Setup is missing, the function will use
%    default settings were possible with other information in the structure
%    to make sure the content is consistent.
%
% 3. If there is no marker data, analogue data, or force plate data then
%    these parts will be ignored and will not be in the output.
%
% 4. If there is force plate data present but no analogue data, the
%    function will attempt to create analogue data and make sure it is
%    consistent with the force plate settings in Parameters. The c3d file
%    does not store force plate data as it is in the c3d structure, so this
%    is required for this data to be written correctly to a c3d file using
%    the writeC3D function
%
%==========================================================================
% OUTPUTS:
%--------------------------------------------------------------------------
%
% none (file is written but nothing is outputted to MATLAB)
%
%==========================================================================

if nargin==0 %run gui to select file to copy
    [fname,pname]=uigetfile('*.c3d','Select a C3D data file to copy...');
    c3d=readC3D([pname,fname],'full'); %read with header and parameter records
end
if nargin < 2
    if ismember('Filename',fieldnames(c3d))
        Savename=c3d.Filename;
        Savename(strfind(Savename,'.c3d'):end+5)='_COPY.c3d';
    else
        [fname,pname]=uiputfile('*.c3d','Create a c3d file to write...');
        if fname==0 %input was aborted
            return %go back to calling function
        end
        Savename=[pname,fname];
    end
end

%Check the imput is correct for writing:
c3d=checkC3D(c3d);

%open a file with write access:
fid=fopen(Savename,'W','n'); %no auto flushing of buffer (faster)
% fid=fopen(Savename,'w','n');

%==========================================================================
%Write the header:
%--------------------------------------------------------------------------
fwrite(fid,c3d.Header.Pblock,'int8'); %number of parameter block [usually 2 as header is block 1]
fwrite(fid,c3d.Header.KeyValue,'int8'); %key value indicating a c3d file (should be 80)
fwrite(fid,c3d.Header.nPoints,'int16'); %number of 3D points (markers)
fwrite(fid,c3d.Header.ApF,'int16'); %total number of analogue measurements per 3D frame
fwrite(fid,c3d.Header.FirstFrame,'int16'); %first 3D frame
fwrite(fid,c3d.Header.LastFrame,'int16'); %last 3D frame (max = 32767 or 65537)
fwrite(fid,c3d.Header.MaxInt,'int16'); %maximum interpolation gap
fwrite(fid,c3d.Header.ScaleFactor,'float'); %3D scale factor (negative = scaled)
fwrite(fid,c3d.Header.DataStart,'int16'); %start of data block [depends on number of parameter blocks] **************NEED TO CHECK***************
fwrite(fid,c3d.Header.SpF,'int16'); %number of analogue samples per frame
fwrite(fid,c3d.Header.Hz,'single'); %3D frame rate
fwrite(fid,c3d.Header.NT1,'int16'); %not used
fwrite(fid,c3d.Header.LabelRange,'int16'); %key value (if label and range data present) (should be 12345)
fwrite(fid,c3d.Header.LRblock,'int16'); %first block of label and range data
fwrite(fid,c3d.Header.KeyLabel,'int16'); %key value (if file supports 4 character event labels) (should be 12345)
fwrite(fid,c3d.Header.nEvents,'int16'); %number of defined time events
fwrite(fid,c3d.Header.NT2,'int16'); %not used
fwrite(fid,c3d.Header.EventTimes,'float'); %event times
fwrite(fid,c3d.Header.EventFlags,'int8'); %event flags
fwrite(fid,c3d.Header.NT3,'int16'); %not used
fwrite(fid,c3d.Header.EventLabels,'single'); %event labels (4 characters long) *******************NEED TO CHECK***************
fwrite(fid,c3d.Header.NT4,'int16'); %not used

positionH=ftell(fid); %should be 512 at end of header block
if positionH ~=512
    fclose(fid); %Close file:
    error('The header appears to be the wrong size')
end

%==========================================================================
%Write the Parameter Record:
%--------------------------------------------------------------------------

%Write the first part of the Parameter section 
fwrite(fid,[1 80],'int8'); %first two bytes are always [1 80]
fwrite(fid,c3d.Parameters.nParamBlocks,'int8'); %number of Parameter Blocks (will check later)
fwrite(fid,c3d.Parameters.ProcessorType,'int8'); %set processor type

%save parameters with the above removed (makes it easier to loop through):
c3d.Parameters=rmfield(c3d.Parameters,{'nParamBlocks','ProcessorType'}); 

%--------------------------------------------------------------------------
%loop through groups and parameters:
GN=fieldnames(c3d.Parameters); %get all group names
for n=1:length(GN) %loop though (-n = group ID)
    fwrite(fid,length(GN{n}),'int8'); %number of characters in group name
    fwrite(fid,-n,'int8'); %group ID
    fwrite(fid,GN{n},'char*1'); %group name (might need char*1)
    if ismember('Description',fieldnames(c3d.Parameters.(GN{n})))
        tempL=length(c3d.Parameters.(GN{n}).Description); %get length of description
        fwrite(fid,tempL+3,'int16'); %enter offset
        fwrite(fid,tempL,'int8'); %set number of characters in description
        fwrite(fid,c3d.Parameters.(GN{n}).Description,'char*1'); %enter description (might need char*1)

        %remove description to avoid problems:
        c3d.Parameters.(GN{n})=rmfield(c3d.Parameters.(GN{n}),'Description');
    else
        fwrite(fid,3,'int16'); %enter offset
        fwrite(fid,0,'int8'); %set number of characters in description
    end
    if strcmp(GN{n},'PROCESSING') %this one is different
        for m=1:size(c3d.Parameters.PROCESSING.MODEL_PARAMETERS,1) %loop through list
            fwrite(fid,length(c3d.Parameters.PROCESSING.MODEL_PARAMETERS{m,1}),'int8'); %set number of characters in paramter name
            fwrite(fid,n,'int8'); %set group ID to which this parameter belongs
            fwrite(fid,c3d.Parameters.PROCESSING.MODEL_PARAMETERS{m,1},'char*1'); %enter parameter name (might need char*1)

            %setup the same each time, so all inputs are:
            fwrite(fid,10,'int16'); %enter offset
            fwrite(fid,4,'int8'); %enter length of each data element
            fwrite(fid,1,'int8'); %enter number of dimensions
            fwrite(fid,1','uint8'); %enter dimensions
            fwrite(fid,c3d.Parameters.PROCESSING.MODEL_PARAMETERS{m,2},'single'); %enter this parameter
            fwrite(fid,0,'int8'); %description length (no description)
        end
    else %normal parameter
        PN=fieldnames(c3d.Parameters.(GN{n})); %get all parameter names for this group
        for m=1:length(PN) %loop through parameters
            fwrite(fid,length(PN{m}),'int8'); %set number of characters in paramter name
            fwrite(fid,n,'int8'); %set group ID to which this parameter belongs
            fwrite(fid,PN{m},'char*1'); %enter parameter name (might need char*1)

            %get correct data element type for this parameter:
            ES=c3d.Setup.(GN{n}).(PN{m}){1}; %element size in bytes
            switch ES
                case -1
                    DE='char*1'; %might need char*1
                case 1
                    DE='int8';
                case 2
                    DE='int16';
                case 4
                    DE='single';
            end

            %determine when the next group/parameter will start:
            ND=c3d.Setup.(GN{n}).(PN{m}){2}; %number of dimensions
            PD=c3d.Setup.(GN{n}).(PN{m}){3}; %parameter dimensions
            OFFSET=5+ND+abs(ES)*prod(PD); %offset to next group/parameter

            fwrite(fid,OFFSET,'int16'); %enter offset
            fwrite(fid,ES,'int8'); %enter length of each data element
            fwrite(fid,ND,'int8'); %enter number of dimensions
            if ND > 0
                fwrite(fid,PD','uint8'); %enter dimensions
            end
            fwrite(fid,c3d.Parameters.(GN{n}).(PN{m})(:),DE); %enter this parameter
            fwrite(fid,0,'int8'); %description length (no description)
        end
    end
end

% Check position at end of parameter record:
Pend=ftell(fid); %get the position
NPend=(c3d.Header.DataStart-1)*512; %where data should start
if Pend < NPend
    fwrite(fid,zeros(NPend-Pend,1),'int8'); %pad with zeros
elseif Pend > NPend
    fclose(fid);%Close file:
    error('The end of the parameter record is beyond the postion that the data is supposed to start. Check parameter and header records')
    %could edit this afterwards

end

%==========================================================================
%Write the Data Record:
%--------------------------------------------------------------------------
DATA=c3d.OutputData';
fwrite(fid,DATA(:),'single');

%==========================================================================

%Close file:
fclose(fid);

%============================= END ========================================
end

%% ========================================================================
% External function: checkC3D
%--------------------------------------------------------------------------
function c3d=checkC3D(c3d)
%Checks the content of a c3d structure for writing to a c3d file
%==========================================================================
% Create some essential variables:
%--------------------------------------------------------------------------
if ismember('Setup',fieldnames(c3d))
%     c3d.SETUP=c3d.Setup; %store for now *********************** REMOVE ****************************************
    c3d=rmfield(c3d,'Setup');
end

ACTUAL_START_FIELD=0;
ACTUAL_END_FIELD=0;
CAMERA_RATE=0;
nPOINTS=0;
nFRAMES=0;
aRATE=0;
nCHANNELS=0;
nFP=0;
typeFP=[];
cornersFP=[];
originFP=[];
channelFP=[];
PARAM=false;	%assume no paramter record to start with
MON={'Angles','Forces','Moments','Points','Scalars','Powers','Reactions'};
M_GROUPS={'ANGLES','ANGLE';...
      'FORCES','FORCE';...
      'MOMENTS','MOMENT';...
      'MODELED_MARKERS','MODELED_MARKER';...
      'SCALARS','SCALAR';...
      'POWERS','POWER';...
      'REACTIONS','REACTION'};
MOU={'ANGLE_UNITS','FORCE_UNITS','MOMENT_UNITS','MODELED_MARKER_UNITS','SCALAR_UNITS','POWER_UNITS','REACTION_UNITS'};
MOU2={'deg','N','Nmm','mm','mm','mW','mm'};
%==========================================================================
% Check which inputs are currently in the c3d file (calculate variables):
%--------------------------------------------------------------------------
CHECK=ismember({'Header','Parameters','Markers','Analogue','ForcePlate','ModelOutputs'},fieldnames(c3d));

if CHECK(3) %marker data present, create POINTS array & adjust settings
    if ismember('Data',fieldnames(c3d.Markers))
        POINTS=c3d.Markers.Data;
        [nFRAMES,~,nPOINTS]=size(POINTS);
    end
    if ismember('Names',fieldnames(c3d.Markers))
        L=length(c3d.Markers.Names);
        NAMES=repmat(blanks(30),[L,1]);
        for m1=1:L %enter names
            NAMES(m1,1:length(c3d.Markers.Names{m1}))=c3d.Markers.Names{m1};
        end
    end
    if ismember('Frames',fieldnames(c3d.Markers))
        ACTUAL_START_FIELD=c3d.Markers.Frames(1);
        ACTUAL_END_FIELD=c3d.Markers.Frames(end);
    end
    if ismember('Time',fieldnames(c3d.Markers))
        CAMERA_RATE=round(1./diff(c3d.Markers.Time(1:2)));
    end
    
    %recreate the residuals:
    if ismember('Residuals',fieldnames(c3d.Markers))
        RESIDUALS=permute(c3d.Markers.Residuals,[1,3,2]);
    else
        RESIDUALS=zeros(nFRAMES,1,nPOINTS);
    end
end

if CHECK(6) %model outputs are present, need to put them into marker data
    MOP=ismember(MON,fieldnames(c3d.ModelOutputs));
    TYPE_GROUPS=repmat(blanks(20),[2,1,sum(MOP)]); %pre-allocate
    m_count=1;
    for m=1:7
        if MOP(m) %if this one is present
            if ismember('Data',fieldnames(c3d.ModelOutputs.(MON{m})))
                [s1,~,s3]=size(c3d.ModelOutputs.(MON{m}).Data);
                if exist('POINTS','var')
                    POINTS(:,:,end+1:end+s3)=c3d.ModelOutputs.(MON{m}).Data;
                else
                    POINTS=c3d.ModelOutputs.(MON{m}).Data;
                end
                if exist('RESIDUALS','var')
                    RESIDUALS(:,:,end+1:end+s3)=zeros(s1,1,s3);
                else
                    RESIDUALS=zeros(s1,1,s3);
                end
            end
            if ismember('Names',fieldnames(c3d.ModelOutputs.(MON{m})))
                %create label names to check later:
                LABELS.(M_GROUPS{m,1})=repmat(blanks(30),[length(c3d.ModelOutputs.(MON{m}).Names),1]);
                for m1=1:length(c3d.ModelOutputs.(MON{m}).Names) %enter names
                    LABELS.(M_GROUPS{m,1})(m1,1:length(c3d.ModelOutputs.(MON{m}).Names{m1}))=c3d.ModelOutputs.(MON{m}).Names{m1};
                end
                TYPE_GROUPS(1,1:length(M_GROUPS{m,1}),m_count)=M_GROUPS{m,1};
                TYPE_GROUPS(2,1:length(M_GROUPS{m,2}),m_count)=M_GROUPS{m,2};
                m_count=m_count+1;
                if exist('NAMES','var') %save this in NAMES
                    NAMES(end+1:end+m1,:)=LABELS.(M_GROUPS{m,1});
                else
                    NAMES=LABELS.(M_GROUPS{m,1});
                end
            end
        end
    end
    [nFRAMES,~,nPOINTS]=size(POINTS); %update these
end

if CHECK(4) %analogue data are present, create ANALOG array
    if ismember('Data',fieldnames(c3d.Analogue))
        ANALOG=c3d.Analogue.Data;
        nCHANNELS=size(ANALOG,2);
    end
    if ismember('Time',fieldnames(c3d.Analogue))
        aRATE=round(1./diff(c3d.Analogue.Time(1:2)));
    end
    if ismember('Channels',fieldnames(c3d.Analogue))
        CHANNELS=c3d.Analogue.Channels;
    end
    if ismember('Units',fieldnames(c3d.Analogue))
        UNITS=c3d.Analogue.Units;
    end
    if ismember('Descriptions',fieldnames(c3d.Analogue))
        DESC=c3d.Analogue.Descriptions;
    end
end

if CHECK(5) %force plate data present, check consistent with analogue
    %only need force and moment
    if ismember('Names',fieldnames(c3d.ForcePlate))
        [s1,s2]=size(c3d.ForcePlate.Names(:,1:6)); %only force and moment
        temp=reshape(c3d.ForcePlate.Names(:,1:6)',[1,s1*s2]);
        nFP=s1; %number of force plates
        typeFP=repmat(2,[nFP,1]); %standard vicon setup
        channelFP=reshape(1:nFP*6,[6,nFP]); %set it as this for now
    end
    if ismember('Corners',fieldnames(c3d.ForcePlate))
        cornersFP=c3d.ForcePlate.Corners;
    else %corners = [+x +y, -x +y, -x -y, +x -y]
        cornersFP=repmat([1 1 0; -1 1 0; -1 -1 0; 1 -1 0],[1,1,nFP]);
    end
    if ismember('R',fieldnames(c3d.ForcePlate))
        R=c3d.ForcePlate.R';
    elseif ismember('Corners',fieldnames(c3d.ForcePlate))
        %get orientation of force plates in vicon coordinate system:
        X=mean(mean(cornersFP([1,4],:,:))-mean(cornersFP([2,3],:,:)),3);
        X=X/norm(X);
        Y=mean(mean(cornersFP([1,2],:,:))-mean(cornersFP([3,4],:,:)),3);
        Y=Y/norm(Y);
        Z=cross(X,Y);
        R=[X;Y;Z]';
        if norm(R)~=1 %didn't work
            R=eye(3); %just use identity matrix (no rotation)
        end
    else
        R=eye(3); %just use identity matrix (no rotation)
    end
    if ismember('Origin',fieldnames(c3d.ForcePlate))
        originFP=R*[zeros(2,nFP);c3d.ForcePlate.Origin(:,3)'];
    else
        if nFP > 0
            originFP=zeros(3,nFP);
        end
    end
    if CHECK(4) %analogue data present
        test1=ismember(temp,CHANNELS);
        if ~all(test1) %some are missing, need to put them in
            test2=ismember(CHANNELS,temp);
            ANALOG(:,test2)=[]; %remove from data
            CHANNELS(test2)=[]; %remove from names
            UNITS(test2)=[]; %remove from units
            DESC(test2)=[]; %remove from descriptions
            if ismember('Data',fieldnames(c3d.ForcePlate))
                [s1,s2,s3]=size(c3d.ForcePlate.Data(:,1:6,:)); %only force and moment
                tempD=nan(s1,s2*s3);
                for n=1:s3 %rotate and store in 2D array
                    tempD(:,n*6-5:n*6)=[(R*c3d.ForcePlate.Data(:,1:3,:)')',(R*c3d.ForcePlate.Data(:,4:6,:)')'];
                end
                ANALOG=[tempD,ANALOG]; %place at start
            end
            s=size(c3d.ForcePlate.Names,1);
            UNITS=[repmat({'N','N','N','Nmm','Nmm','Nmm'},[1,s]),UNITS];
            tempD=cell(1,s*6);
            for n=1:s
                tempD(n*6-5:n*6)=repmat({strcat('Force Plate [',num2str(n),']')},[1,6]);
            end
            DESC=[tempD,DESC];
            nCHANNELS=size(ANALOG,2); %update
        else %check which channels are currently being used
            channelFP=nan(6,nFP);
            for n=1:nFP
                channelFP(:,n)=find(ismember(CHANNELS,c3d.ForcePlate.Names(n,1:6)))';
            end

        end
    else %analogue data missing
        if ismember('Data',fieldnames(c3d.ForcePlate))
            [s1,s2,s3]=size(c3d.ForcePlate.Data(:,1:6,:)); %only force and moment
            tempD=nan(s1,s2*s3);
            for n=1:s3 %rotate and store in 2D array
                tempD(:,n*6-5:n*6)=[(R*c3d.ForcePlate.Data(:,1:3,:)')',(R*c3d.ForcePlate.Data(:,4:6,:)')'];
            end
            ANALOG=tempD;
        end
        if ismember('Names',fieldnames(c3d.ForcePlate))
            CHANNELS=temp;
        end
        s=size(c3d.ForcePlate.Names,1);
        UNITS=repmat({'N','N','N','Nmm','Nmm','Nmm'},[1,s]);
        DESC=cell(1,s*6);
        for n=1:s
            DESC(n*6-5:n*6)=repmat({strcat('Force Plate [',num2str(n),']')},[1,6]);
        end
        nCHANNELS=size(ANALOG,2); %update

        %also no aRATE
        if ismember('Time',fieldnames(c3d.ForcePlate))
            aRATE=round(1./diff(c3d.ForcePlate.Time(1:2)));
        end
    end
end

%Last check of data before progressing:
if ACTUAL_START_FIELD==0 %cannot get from data
    if CHECK(2) %parameter record present
        if ismember('TRIAL',fieldnames(c3d.Parameters))
            if ismember('ACTUAL_START_FIELD',fieldnames(c3d.Parameters.TRIAL))
                ACTUAL_START_FIELD=c3d.Parameters.TRIAL.ACTUAL_START_FIELD;
            end
        end
    elseif CHECK(1) %header record present
        if ismember('FirstFrame',fieldnames(c3d.Header))
            ACTUAL_START_FIELD=c3d.Header.TRIAL.FirstFrame;
        end
    else
        ACTUAL_START_FIELD=1; %must declare as 1
    end
end
if ACTUAL_END_FIELD==0 %cannot get from data
    if CHECK(2) %parameter record present
        if ismember('TRIAL',fieldnames(c3d.Parameters))
            if ismember('ACTUAL_END_FIELD',fieldnames(c3d.Parameters.TRIAL))
                ACTUAL_END_FIELD=c3d.Parameters.TRIAL.ACTUAL_END_FIELD;
            end
        end
    elseif CHECK(1) %header record present
        if ismember('LastFrame',fieldnames(c3d.Header))
            ACTUAL_END_FIELD=c3d.Header.LastFrame;
        end
    elseif nFRAMES > 0
        ACTUAL_END_FIELD=nFRAMES;
    end
end
if nFRAMES==0 %cannot get from data
    if CHECK(2) %parameter record present
        if ismember('POINT',fieldnames(c3d.Parameters))
            if ismember('FRAMES',fieldnames(c3d.Parameters.POINT))
                nFRAMES=c3d.Parameters.POINT.FRAMES;
            end
        end
    elseif ACTUAL_END_FIELD > 0
        nFRAMES=ACTUAL_END_FIELD-ACTUAL_START_FIELD+1;
    else
        if exist('ANALOG','var') %will have to used analogue data
            nFRAMES=size(ANALOG,1);
        end
    end
end
if ACTUAL_END_FIELD==0
    ACTUAL_END_FIELD=nFRAMES;
end
if CAMERA_RATE==0 %cannot get from data
    if CHECK(2) %parameter record present
        if ismember('TRIAL',fieldnames(c3d.Parameters))
            if ismember('CAMERA_RATE',fieldnames(c3d.Parameters.TRIAL))
                CAMERA_RATE=c3d.Parameters.TRIAL.CAMERA_RATE;
            end
        elseif ismember('POINT',fieldnames(c3d.Parameters))
            if ismember('RATE',fieldnames(c3d.Parameters.POINT))
                CAMERA_RATE=c3d.Parameters.POINT.RATE;
            end
        end
    elseif CHECK(1) %header record present
        if ismember('Hz',fieldnames(c3d.Header))
            CAMERA_RATE=c3d.Header.Hz;
        end
    elseif exist('ANALOG','var') %will have to used analogue data
        CAMERA_RATE=aRATE;
    end
end

%==========================================================================
% Check parameter record, or create if not present:
%--------------------------------------------------------------------------
if CHECK(2) %paramter record present (check essential values)
    PARAM=true; %there is a parameter record
    try
        %TRIAL:
        if c3d.Parameters.TRIAL.ACTUAL_START_FIELD~=ACTUAL_START_FIELD
            warning('The first frame number in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.TRIAL.ACTUAL_START_FIELD=ACTUAL_START_FIELD;
        end
        if c3d.Parameters.TRIAL.ACTUAL_END_FIELD~=ACTUAL_END_FIELD
            warning('The last frame number in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.TRIAL.ACTUAL_END_FIELD=ACTUAL_END_FIELD;
        end
        if c3d.Parameters.TRIAL.CAMERA_RATE~=CAMERA_RATE
            warning('The 3D frame rate in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.TRIAL.CAMERA_RATE=CAMERA_RATE;
        end
        
        %POINT:
        if c3d.Parameters.POINT.RATE~=CAMERA_RATE
            warning('The 3D frame rate in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.POINT.RATE=CAMERA_RATE;
        end
        if c3d.Parameters.POINT.USED~=nPOINTS
            warning('The number of 3D points in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.POINT.USED=nPOINTS;
        end
        if c3d.Parameters.POINT.FRAMES~=nFRAMES
            warning('The number of 3D frames in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.POINT.FRAMES=nFRAMES;
        end
        if c3d.Parameters.POINT.SCALE > 0
            warning('The scale factor in the parameter record is positive, this suggests the 3D point data is stored as integers and unscaled. You should check the output carefully...')
        end
        PN=fieldnames(c3d.Parameters.POINT);
        if nPOINTS==0 %no markers present, so should be no labels too
            if ismember('LABELS',PN)
                warning('There is a POINT.LABELS parameter present but no marker data. Removing POINT.LABELS from the parameter record...')
                c3d.Parameters.POINT=rmfield(c3d.Parameters.POINT,'LABELS');
            end
            if ismember('DESCRIPTIONS',PN)
                warning('There is a POINT.DESCRIPTIONS parameter present but no marker data. Removing POINT.DESCRIPTIONS from the parameter record...')
                c3d.Parameters.POINT=rmfield(c3d.Parameters.POINT,'DESCRIPTIONS');
            end
            if ismember('TYPE_GROUPS',PN)
                warning('There are parameters for model outputs present but no model output data. Removing model outputs from the parameter record...')
                c3d.Parameters.POINT=rmfield(c3d.Parameters.POINT,'TYPE_GROUPS');
                for m=1:7
                    ThisM=ismember(M_GROUPS(m,:),fieldnames(c3d.Parameters.POINT));
                    if any(ThisM) %remove labels
                        c3d.Parameters.POINT=rmfield(c3d.Parameters.POINT,M_GROUPS{m,ThisM});
                    end
                    if ismember(MOU(m),fieldnames(c3d.Parameters.POINT))
                        c3d.Parameters.POINT=rmfield(c3d.Parameters.POINT,MOU{m});%remove units
                    end
                    
                end
            end
        else %markers are present
            if size(c3d.Parameters.POINT.LABELS,1)~=nPOINTS
                warning('The number of 3D labels in the parameter record does not match the number of 3D points. Adjusting the parameter record now...')
                c3d.Parameters.POINT.LABELS=NAMES;
            else %check the list of names are the same
                M=~all(NAMES==c3d.Parameters.POINT.LABELS,2);
                if any(M) %some don't match
                    tempM=POINTS(:,:,M); %store the problem markers
                    POINTS(:,:,M)=0; %and set them to zeros
                    tempN=cellstr(NAMES(M,:)); %store the problem names
                    tempQ=cellstr(c3d.Parameters.POINT.LABELS);
                    FQ=find(M);
                    for q1=1:length(FQ)
                        fn=find(strcmp(tempQ,tempN{q1}));
                        POINTS(:,:,fn)=tempM(:,:,q1);
                        NAMES(fn,:)=blanks(30); %clear the name fist
                        NAMES(fn,1:length(tempN{q1}))=tempN{q1};
                    end
                end
            end
            if size(c3d.Parameters.POINT.DESCRIPTIONS,1)~=nPOINTS
                warning('The number of 3D label descriptions in the parameter record does not match the number of 3D points. Adjusting the parameter record now...')
                c3d.Parameters.POINT.DESCRIPTIONS=repmat(blanks(100),[nPOINTS,1]);
            end
            if CHECK(6) %there are model outputs in this file
                if ismember('TYPE_GROUPS',PN) %need to check parameters?
                    if ~all(c3d.Parameters.POINT.TYPE_GROUPS(:)==TYPE_GROUPS(:))
                        warning('The list of model outputs in the parameter record does not match the data. Adjusting the parameter record now...')
                        c3d.Parameters.POINT.TYPE_GROUPS=TYPE_GROUPS;
                    end
                    for m=1:7 
                        ThisM=ismember(M_GROUPS(m,:),fieldnames(c3d.Parameters.POINT));
                        if any(ThisM) %this output is in parameter record
                            if MOP(m) %this output is in structure (compare)
                                if ~all(c3d.Parameters.POINT.(M_GROUPS{m,ThisM})(:)==LABELS.(M_GROUPS{m,1})(:))
                                    warning(['The list of model outputs (',M_GROUPS{m,ThisM},') in the parameter record does not match the data. Adjusting the parameter record now...'])
                                    c3d.Parameters.POINT.(MOU{m})=MOU2{m}; %update units
                                    c3d.Parameters.POINT.(M_GROUPS{m,ThisM})=LABELS.(M_GROUPS{m,1})'; %update labels
                                else
                                    c3d.Parameters.POINT.(M_GROUPS{m,ThisM})=c3d.Parameters.POINT.(M_GROUPS{m,ThisM})'; %rotate ready for writing
                                end
                            else %not in data
                                warning(['The list of model outputs (',M_GROUPS{m,ThisM},') in the parameter record does not match the data. Adjusting the parameter record now...'])
                                c3d.Parameters.POINT=rmfield(c3d.Parameters.POINT,M_GROUPS{m,ThisM});
                            end
                        else
                            if MOP(m) %this output is in structure (add to parameter)
                                warning(['The list of model outputs (',M_GROUPS{m,ThisM},') in the parameter record does not match the data. Adjusting the parameter record now...'])
                                c3d.Parameters.POINT.(MOU{m})=MOU2{m}; %update units
                                c3d.Parameters.POINT.(M_GROUPS{m,1})=LABELS.(M_GROUPS{m,1})'; %update labels
                            end
                        end
                    end
                else
                    warning('There are model outputs in the data but not in the parameter record. Adjusting the parameter record now...')
                    for m=1:7 
                        ThisM=ismember(M_GROUPS(m,:),fieldnames(c3d.Parameters.POINT));
                        if any(ThisM) %remove labels before recreating to avoid issues
                            c3d.Parameters.POINT=rmfield(c3d.Parameters.POINT,M_GROUPS{m,ThisM});
                        end
                        if MOP(m) %if this one is present
                            if ismember('Names',fieldnames(c3d.ModelOutputs.(MON{m})))
                                c3d.Parameters.POINT.(MOU{m})=MOU2{m}; %enter units
                                c3d.Parameters.POINT.(M_GROUPS{m,1})=LABELS.(M_GROUPS{m,1})'; %enter labels
                            end
                        end
                    end
                    c3d.Parameters.POINT.TYPE_GROUPS=TYPE_GROUPS;
                end
            end
        end
        
        %ANALOG
        if c3d.Parameters.ANALOG.RATE~=aRATE
            warning('The analogue frame rate in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.ANALOG.RATE=aRATE;
        end
        if c3d.Parameters.ANALOG.USED~=nCHANNELS
            warning('The number of analogue channels in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.ANALOG.USED=nCHANNELS;
        end
        if nCHANNELS > 0
            if size(c3d.Parameters.ANALOG.GAIN,1)~=nCHANNELS
                warning('The number of analogue gain values in the parameter record does not match the number of analogue channels. Adjusting the parameter record now...')
                c3d.Parameters.ANALOG.GAIN=ones(nCHANNELS,1);
            end
            if size(c3d.Parameters.ANALOG.SCALE,1)~=nCHANNELS
                warning('The number of analogue scale values in the parameter record does not match the number of analogue channels. Adjusting the parameter record now...')
                c3d.Parameters.ANALOG.SCALE=ones(nCHANNELS,1);
            end
            if size(c3d.Parameters.ANALOG.OFFSET,1)~=nCHANNELS
                warning('The number of analogue offset values in the parameter record does not match the number of analogue channels. Adjusting the parameter record now...')
                c3d.Parameters.ANALOG.OFFSET=zeros(nCHANNELS,1);
            end
            if size(c3d.Parameters.ANALOG.UNITS,1)~=nCHANNELS
                warning('The number of analogue units values in the parameter record does not match the number of analogue channels. Adjusting the parameter record now...')
                c3d.Parameters.ANALOG.UNITS=repmat(blanks(8),[nCHANNELS,1]);
                for n=1:nCHANNELS
                    c3d.Parameters.ANALOG.UNITS(n,1:length(UNITS{n}))=UNITS{n};
                end
            end
            if size(c3d.Parameters.ANALOG.LABELS,1)~=nCHANNELS
                warning('The number of analogue labels in the parameter record does not match the number of analogue channels. Adjusting the parameter record now...')
                c3d.Parameters.ANALOG.LABELS=repmat(blanks(16),[nCHANNELS,1]);
                for n=1:nCHANNELS
                    c3d.Parameters.ANALOG.LABELS(n,1:length(CHANNELS{n}))=CHANNELS{n};
                end
            end
            if size(c3d.Parameters.ANALOG.DESCRIPTIONS,1)~=nCHANNELS
                warning('The number of analogue label descriptions in the parameter record does not match the number of analogue channels. Adjusting the parameter record now...')
                c3d.Parameters.ANALOG.DESCRIPTIONS=repmat(blanks(100),[nCHANNELS,1]);
                for n=1:nCHANNELS
                    c3d.Parameters.ANALOG.DESCRIPTIONS(n,1:length(DESC{n}))=DESC{n};
                end
            end
        end
        
        %FORCE_PLATFORM:
        if c3d.Parameters.FORCE_PLATFORM.USED~=nFP
            warning('The number of force plates in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.FORCE_PLATFORM.USED=nFP;
        end
        if ~all(c3d.Parameters.FORCE_PLATFORM.TYPE==typeFP)
            warning('The type or types of force plates in the parameter record does not match expected. Adjusting the parameter record now...')
            c3d.Parameters.FORCE_PLATFORM.TYPE=typeFP;
        end
        if ~all(c3d.Parameters.FORCE_PLATFORM.CORNERS(:)==cornersFP(:))
            warning('The force plate corner coordinates in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.FORCE_PLATFORM.CORNERS=cornersFP;
        end
        if ~all(c3d.Parameters.FORCE_PLATFORM.ORIGIN(:)==originFP(:))
            warning('The force plate origin coordinates in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.FORCE_PLATFORM.ORIGIN=originFP;
        end
        if ~all(c3d.Parameters.FORCE_PLATFORM.CHANNEL(:)==channelFP(:))
            warning('The force plate channel identifiers in the parameter record does not match the data. Adjusting the parameter record now...')
            c3d.Parameters.FORCE_PLATFORM.CHANNEL=channelFP;
        end
        
        %Don't bother checking the rest as they're not important
    catch exceptionP
        warning('There are issues with the parameter record. Recreating it from the data...')
        disp([exceptionP.message,' (line ',num2str(exceptionP.stack(1).line),')'])
        tempP.nParamBlocks=0; %save temp parameter record for some things
        tempP.ProcessorType='Intel';
        if ismember('SUBJECTS',fieldnames(c3d.Parameters))
            tempP.SUBJECTS=c3d.Parameters.SUBJECTS;
        end
        if ismember('PROCESSING',fieldnames(c3d.Parameters))
            tempP.PROCESSING=c3d.Parameters.PROCESSING;
        end
        c3d=rmfield(c3d,'Parameters'); %clear the paramter record completely
        c3d.Parameters=tempP; %load up these temp things
        PARAM=false; %parameter record has problems, create from data
    end
end

if ~PARAM % need to create a parameter record
    warning('No parameter record in input. Attempting to create parameter record from data...')
    if all(~CHECK(3:6)) %no marker data, analogue data, or force plates
        error('No marker data or analogue data in input. Cannot recreate parameter record...')
    elseif any(CHECK([3,6])) && any(CHECK(4:5)) %marker data and analogue data is present
        warning('Attempting to create parameter record from marker data and analogue data...')
    elseif all(~CHECK([3,6])) && any(CHECK(4:5)) %no marker data, but analogue data is present
        warning('No marker data in input, but analogue data is. Attempting to create parameter record from analogue data only...')
    elseif any(CHECK([3,6])) && all(~CHECK(4:5)) %no analogue data, but marker data is present
        warning('No analogue data in input, but marker data is. Attempting to create parameter record from marker data only...')
    end

    %load default blank parameter record:
    c3d.Parameters.nParamBlocks=0; %need to check
    c3d.Parameters.ProcessorType='Intel';
    
    %TRIAL:
    c3d.Parameters.TRIAL.Description='';
    c3d.Parameters.TRIAL.ACTUAL_START_FIELD=ACTUAL_START_FIELD;
    c3d.Parameters.TRIAL.ACTUAL_END_FIELD=ACTUAL_END_FIELD;
    c3d.Parameters.TRIAL.CAMERA_RATE=CAMERA_RATE;
    c3d.Parameters.TRIAL.X_DIRECTION=1;
    c3d.Parameters.TRIAL.Y_DIRECTION=3;
    c3d.Parameters.TRIAL.Z_DIRECTION=5;
    
    %POINT:
    c3d.Parameters.POINT.Description='';
    c3d.Parameters.POINT.USED=nPOINTS;
    c3d.Parameters.POINT.FRAMES=nFRAMES;
    c3d.Parameters.POINT.DATA_START=0;
    c3d.Parameters.POINT.SCALE=-0.01;
    c3d.Parameters.POINT.RATE=CAMERA_RATE;
    c3d.Parameters.POINT.MOVIE_DELAY=[];
    c3d.Parameters.POINT.MOVIE_ID='';
    c3d.Parameters.POINT.X_SCREEN='+X';
    c3d.Parameters.POINT.Y_SCREEN='+Z';
    c3d.Parameters.POINT.UNITS='mm';
    if nPOINTS > 0 %markers present (enter label info)
        c3d.Parameters.POINT.LABELS=NAMES;
        c3d.Parameters.POINT.DESCRIPTIONS=repmat(blanks(100),[nPOINTS,1]);
        if CHECK(6) %model outputs are present (enter labels and units) 
            for m=1:7
                if MOP(m) %if this one is present
                    if ismember('Names',fieldnames(c3d.ModelOutputs.(MON{m})))
                        c3d.Parameters.POINT.(MOU{m})=MOU2{m}; %enter units
                        c3d.Parameters.POINT.(M_GROUPS{m,1})=LABELS.(M_GROUPS{m,1})'; %enter labels
                    end
                end
            end
            c3d.Parameters.POINT.TYPE_GROUPS=TYPE_GROUPS;
        end
    end

    %ANALOG:
    c3d.Parameters.ANALOG.Description='';
    c3d.Parameters.ANALOG.GEN_SCALE=1;
    c3d.Parameters.ANALOG.RATE=aRATE;
    c3d.Parameters.ANALOG.USED=nCHANNELS;
    if nCHANNELS > 0 %enter more info
        c3d.Parameters.ANALOG.GAIN=ones(nCHANNELS,1);
        c3d.Parameters.ANALOG.SCALE=ones(nCHANNELS,1);
        c3d.Parameters.ANALOG.OFFSET=zeros(nCHANNELS,1);
        c3d.Parameters.ANALOG.UNITS=repmat(blanks(8),[nCHANNELS,1]);
        c3d.Parameters.ANALOG.LABELS=repmat(blanks(16),[nCHANNELS,1]);
        c3d.Parameters.ANALOG.DESCRIPTIONS=repmat(blanks(100),[nCHANNELS,1]);
        for n=1:nCHANNELS %newer version uses Force.Fx1 and Moment.Mx1 notation
            c3d.Parameters.ANALOG.UNITS(n,1:length(UNITS{n}))=UNITS{n};
            c3d.Parameters.ANALOG.LABELS(n,1:length(CHANNELS{n}))=CHANNELS{n};
            c3d.Parameters.ANALOG.DESCRIPTIONS(n,1:length(DESC{n}))=DESC{n};
        end
    end

    %FORCE_PLATFORM:
    c3d.Parameters.FORCE_PLATFORM.Description='';
    c3d.Parameters.FORCE_PLATFORM.USED=nFP;
    c3d.Parameters.FORCE_PLATFORM.ZERO=[1;0];
    c3d.Parameters.FORCE_PLATFORM.TYPE=typeFP;
    c3d.Parameters.FORCE_PLATFORM.CORNERS=cornersFP;
    c3d.Parameters.FORCE_PLATFORM.ORIGIN=originFP;
    c3d.Parameters.FORCE_PLATFORM.CHANNEL=channelFP;
    
    %EVENT_CONTEXT (newer version):
    c3d.Parameters.EVENT_CONTEXT.Description='';
    c3d.Parameters.EVENT_CONTEXT.USED=4;
    c3d.Parameters.EVENT_CONTEXT.ICON_IDS=(0:3)';
    c3d.Parameters.EVENT_CONTEXT.LABELS=['General',blanks(9);'Left',blanks(12);'Right',blanks(11);'Invalid',blanks(9)];
    c3d.Parameters.EVENT_CONTEXT.DESCRIPTIONS=['For other events',blanks(16);'Left side',blanks(23);'Right side',blanks(22);'No context',blanks(22)];
    c3d.Parameters.EVENT_CONTEXT.COLOURS=[255 255 0 50; 164 0 255 50; 0 0 0 50];
    
    %EVENT (no events):
    c3d.Parameters.EVENT.Description='';
    c3d.Parameters.EVENT.USED=0;
    c3d.Parameters.EVENT.CONTEXTS='';
    c3d.Parameters.EVENT.ICON_IDS=[];
    c3d.Parameters.EVENT.LABELS='';
    c3d.Parameters.EVENT.DESCRIPTIONS='';
    c3d.Parameters.EVENT.SUBJECTS='';
    c3d.Parameters.EVENT.TIMES=[];
    c3d.Parameters.EVENT.GENERIC_FLAGS=[];
    
    %MANUFACTURER (guess):
    c3d.Parameters.MANUFACTURER.Description='';
    c3d.Parameters.MANUFACTURER.COMPANY='Vicon';
    c3d.Parameters.MANUFACTURER.SOFTWARE='Vicon Nexus';
    c3d.Parameters.MANUFACTURER.VERSION_LABEL='2.7.0.106412h';
    
    %ANALYSIS (no analysis - newer version):
    c3d.Parameters.ANALYSIS.Description='';
    c3d.Parameters.ANALYSIS.USED=0;
	
end

%re-arrange somethings ready for writing:
if ismember('LABELS',fieldnames(c3d.Parameters.POINT))
    c3d.Parameters.POINT.LABELS=c3d.Parameters.POINT.LABELS';
end
if ismember('TYPE_GROUPS',fieldnames(c3d.Parameters.POINT))
    c3d.Parameters.POINT.TYPE_GROUPS=permute(c3d.Parameters.POINT.TYPE_GROUPS,[2,1,3]);
end
if ismember('DESCRIPTIONS',fieldnames(c3d.Parameters.POINT))
    c3d.Parameters.POINT.DESCRIPTIONS=c3d.Parameters.POINT.DESCRIPTIONS';
end
if ismember('MOVIE_ID',fieldnames(c3d.Parameters.POINT))
    c3d.Parameters.POINT.MOVIE_ID=c3d.Parameters.POINT.MOVIE_ID';
end
if ismember('UNITS',fieldnames(c3d.Parameters.ANALOG))
    c3d.Parameters.ANALOG.UNITS=c3d.Parameters.ANALOG.UNITS';
end
if ismember('LABELS',fieldnames(c3d.Parameters.ANALOG))
    c3d.Parameters.ANALOG.LABELS=c3d.Parameters.ANALOG.LABELS';
end
if ismember('DESCRIPTIONS',fieldnames(c3d.Parameters.ANALOG))
    c3d.Parameters.ANALOG.DESCRIPTIONS=c3d.Parameters.ANALOG.DESCRIPTIONS';
end
if ismember('CORNERS',fieldnames(c3d.Parameters.FORCE_PLATFORM))
    c3d.Parameters.FORCE_PLATFORM.CORNERS=permute(c3d.Parameters.FORCE_PLATFORM.CORNERS,[2,1,3]);
end
if ismember('LABELS',fieldnames(c3d.Parameters.EVENT_CONTEXT))
    c3d.Parameters.EVENT_CONTEXT.LABELS=c3d.Parameters.EVENT_CONTEXT.LABELS';
end
if ismember('DESCRIPTIONS',fieldnames(c3d.Parameters.EVENT_CONTEXT))
    c3d.Parameters.EVENT_CONTEXT.DESCRIPTIONS=c3d.Parameters.EVENT_CONTEXT.DESCRIPTIONS';
end
if ismember('LABELS',fieldnames(c3d.Parameters.EVENT))
    c3d.Parameters.EVENT.LABELS=c3d.Parameters.EVENT.LABELS';
end
if ismember('DESCRIPTIONS',fieldnames(c3d.Parameters.EVENT))
    c3d.Parameters.EVENT.DESCRIPTIONS=c3d.Parameters.EVENT.DESCRIPTIONS';
end
if ismember('SUBJECTS',fieldnames(c3d.Parameters))
    if ismember('NAMES',fieldnames(c3d.Parameters.SUBJECTS))
        c3d.Parameters.SUBJECTS.NAMES=c3d.Parameters.SUBJECTS.NAMES';
    end
    if ismember('MARKER_SETS',fieldnames(c3d.Parameters.SUBJECTS))
        c3d.Parameters.SUBJECTS.MARKER_SETS=c3d.Parameters.SUBJECTS.MARKER_SETS';
    end
end


%set the processor type as a number ready for writing;
if strcmpi(c3d.Parameters.ProcessorType,'Intel') %should be this one
    c3d.Parameters.ProcessorType=84;
elseif strcmpi(c3d.Parameters.ProcessorType,'DEC')
    c3d.Parameters.ProcessorType=85;
    warning('The processor type is not Intel. There may be issues with this file')
elseif strcmpi(c3d.Parameters.ProcessorType,'MIPS')
    c3d.Parameters.ProcessorType=86;
    warning('The processor type is not Intel. There may be issues with this file')
else
    c3d.Parameters.ProcessorType=84;
    warning('The processor type could not be determined, so will set as Intel. There may be issues with this file')
end

%==========================================================================
% Create header record (from parameter record):
%--------------------------------------------------------------------------
if c3d.Parameters.ANALOG.USED > 0
    SpF=c3d.Parameters.ANALOG.RATE/c3d.Parameters.TRIAL.CAMERA_RATE;
    ApF=c3d.Parameters.ANALOG.USED*SpF;
else
    SpF=0; %zero when no analogue data
    ApF=0; %zero when no analogue data
end
c3d.Header.Pblock=2;
c3d.Header.KeyValue=80;
c3d.Header.nPoints=c3d.Parameters.POINT.USED;
c3d.Header.ApF=ApF; 
c3d.Header.FirstFrame=c3d.Parameters.TRIAL.ACTUAL_START_FIELD;
c3d.Header.LastFrame=c3d.Parameters.TRIAL.ACTUAL_END_FIELD;
c3d.Header.MaxInt=0;
c3d.Header.ScaleFactor=c3d.Parameters.POINT.SCALE;
c3d.Header.DataStart=c3d.Parameters.nParamBlocks+c3d.Header.Pblock;
c3d.Header.SpF=SpF; 
c3d.Header.Hz=c3d.Parameters.TRIAL.CAMERA_RATE;
c3d.Header.NT1=zeros(135,1);
c3d.Header.LabelRange=0;
c3d.Header.LRblock=0;
c3d.Header.KeyLabel=12345;
c3d.Header.nEvents=0;
c3d.Header.NT2=0;
c3d.Header.EventTimes=zeros(18,1);
c3d.Header.EventFlags=zeros(18,1);
c3d.Header.NT3=0;
c3d.Header.EventLabels=zeros(18,1);
c3d.Header.NT4=zeros(22,1);

%==========================================================================
% Create default setup and calculate size of parameter record:
%--------------------------------------------------------------------------
% {Byte Type, Number of Dimensions, [Dimensions] }
%TRIAL:
Setup.TRIAL.ACTUAL_START_FIELD={2 1 2};
Setup.TRIAL.ACTUAL_END_FIELD={2 1 2};
Setup.TRIAL.CAMERA_RATE={4 0 1};
Setup.TRIAL.X_DIRECTION={2 0 1};
Setup.TRIAL.Y_DIRECTION={2 0 1};
Setup.TRIAL.Z_DIRECTION={2 0 1};
Setup.TRIAL.VIDEO_RATE_DIVIDER={2 0 1}; %only in older versions

%SUBJECTS (if present):
if ismember('SUBJECTS',fieldnames(c3d.Parameters))
    Setup.SUBJECTS.USED={2 0 1};
    Setup.SUBJECTS.IS_STATIC={2 0 1};
    Setup.SUBJECTS.USES_PREFIXES={2 0 1};
    Setup.SUBJECTS.NAMES={-1 2 size(c3d.Parameters.SUBJECTS.NAMES)};
    Setup.SUBJECTS.MARKER_SETS={-1 2 size(c3d.Parameters.SUBJECTS.MARKER_SETS)};
end

%POINT:
Setup.POINT.USED={2 0 1};
Setup.POINT.FRAMES={2 0 1};
Setup.POINT.DATA_START={2 0 1};
Setup.POINT.SCALE={4 0 1};
Setup.POINT.RATE={4 0 1};
if ismember('MOVIE_DELAY',fieldnames(c3d.Parameters.POINT))
    Setup.POINT.MOVIE_DELAY={4 1 length(c3d.Parameters.POINT.MOVIE_DELAY)};
    if isempty(c3d.Parameters.POINT.MOVIE_ID)
        Setup.POINT.MOVIE_ID={-1 2 [32 0]};
    else
        Setup.POINT.MOVIE_ID={-1 2 size(c3d.Parameters.POINT.MOVIE_ID)};
    end
end
Setup.POINT.X_SCREEN={-1 1 2};
Setup.POINT.Y_SCREEN={-1 1 2};
Setup.POINT.UNITS={-1 1 2};
if c3d.Parameters.POINT.USED > 0
    Setup.POINT.LABELS={-1 2 size(c3d.Parameters.POINT.LABELS)};
    Setup.POINT.DESCRIPTIONS={-1 2 size(c3d.Parameters.POINT.DESCRIPTIONS)};
end
if CHECK(6) %model outputs are present
%     s=size(TYPE_GROUPS);
%     if length(s)==2
%         Setup.POINT.TYPE_GROUPS={-1 3 s([2,1])};
%     else
%         Setup.POINT.TYPE_GROUPS={-1 3 s([2,1,3])};
%     end

    s=size(c3d.Parameters.POINT.TYPE_GROUPS);
    Setup.POINT.TYPE_GROUPS={-1 length(s) s};
    for m=1:7
        ThisM=ismember(M_GROUPS(m,:),fieldnames(c3d.Parameters.POINT));
        if any(ThisM) %this output is in parameter record
            Setup.POINT.(MOU{m})={-1 1 length(c3d.Parameters.POINT.(MOU{m}))};
            Setup.POINT.(M_GROUPS{m,ThisM})={-1 2 size(c3d.Parameters.POINT.(M_GROUPS{m,ThisM}))};
        end
    end
end

%ANALOG:
Setup.ANALOG.GEN_SCALE={4 0 1};
Setup.ANALOG.RATE={4 0 1};
Setup.ANALOG.USED={2 0 1};
if c3d.Parameters.ANALOG.USED > 0 %only present if used > 0
    Setup.ANALOG.GAIN={2 1 length(c3d.Parameters.ANALOG.GAIN)}; 
    Setup.ANALOG.SCALE={4 1 length(c3d.Parameters.ANALOG.SCALE)};
    Setup.ANALOG.OFFSET={2 1 length(c3d.Parameters.ANALOG.OFFSET)};
    Setup.ANALOG.UNITS={-1 2 size(c3d.Parameters.ANALOG.UNITS)};
    Setup.ANALOG.LABELS={-1 2 size(c3d.Parameters.ANALOG.LABELS)};
    Setup.ANALOG.DESCRIPTIONS={-1 2 size(c3d.Parameters.ANALOG.DESCRIPTIONS)};
end

%FORCE_PLATFORM:
Setup.FORCE_PLATFORM.USED={2 0 1};
Setup.FORCE_PLATFORM.ZERO={2 1 2};
Setup.FORCE_PLATFORM.TYPE={2 1 c3d.Parameters.FORCE_PLATFORM.USED};
Setup.FORCE_PLATFORM.CORNERS={4 3 [3,4,c3d.Parameters.FORCE_PLATFORM.USED]};
Setup.FORCE_PLATFORM.ORIGIN={4 2 [3,c3d.Parameters.FORCE_PLATFORM.USED]};
Setup.FORCE_PLATFORM.CHANNEL={2 2 [6,c3d.Parameters.FORCE_PLATFORM.USED]};

%EVENT_CONTEXT:
if ismember('EVENT_CONTEXT',fieldnames(c3d.Parameters))
    if ismember('USED',fieldnames(c3d.Parameters.EVENT_CONTEXT))
        Setup.EVENT_CONTEXT.USED={2 0 1};
        Setup.EVENT_CONTEXT.ICON_IDS={2 1 c3d.Parameters.EVENT_CONTEXT.USED};
        Setup.EVENT_CONTEXT.LABELS={-1 2 [16, c3d.Parameters.EVENT_CONTEXT.USED]};
        Setup.EVENT_CONTEXT.DESCRIPTIONS={-1 2 [32, c3d.Parameters.EVENT_CONTEXT.USED]};
        Setup.EVENT_CONTEXT.COLOURS={2 2 [3, c3d.Parameters.EVENT_CONTEXT.USED]};
    end
end

%EVENT:
Setup.EVENT.USED={2 0 1};
Setup.EVENT.CONTEXTS={-1 2 [16,0]};
Setup.EVENT.ICON_IDS={2 1 0};
Setup.EVENT.LABELS={-1 2 [32,0]};
Setup.EVENT.DESCRIPTIONS={-1 2 [80,0]};
Setup.EVENT.SUBJECTS={-1 2 [32,0]};
Setup.EVENT.TIMES={4 2 [2,0]};
Setup.EVENT.GENERIC_FLAGS={1 1 0};

%MANUFACTURER:
if ismember('MANUFACTURER',fieldnames(c3d.Parameters))
    if ismember('COMPANY',fieldnames(c3d.Parameters.MANUFACTURER))
        Setup.MANUFACTURER.COMPANY={-1 1 length(c3d.Parameters.MANUFACTURER.COMPANY)};
    end
    if ismember('SOFTWARE',fieldnames(c3d.Parameters.MANUFACTURER))
        Setup.MANUFACTURER.SOFTWARE={-1 1 length(c3d.Parameters.MANUFACTURER.SOFTWARE)};
    end
    if ismember('VERSION_LABEL',fieldnames(c3d.Parameters.MANUFACTURER))
        Setup.MANUFACTURER.VERSION_LABEL={-1 1 length(c3d.Parameters.MANUFACTURER.VERSION_LABEL)};
    end
end

%ANALYSIS:
Setup.ANALYSIS.USED={2 0 1};
Setup.ANALYSIS.NAMES={-1 2 [32 0]};
Setup.ANALYSIS.DESCRIPTIONS={-1 2 [80 0]};
Setup.ANALYSIS.SUBJECTS={-1 2 [32 0]};
Setup.ANALYSIS.CONTEXTS={-1 2 [16 0]};
Setup.ANALYSIS.UNITS={-1 2 [32 0]};
Setup.ANALYSIS.VALUES={4 1 0};

%PROCESSING:
if ismember('PROCESSING',fieldnames(c3d.Parameters))
    Setup.PROCESSING.MODEL_PARAMETERS={4 1 1}; %need to check size of record
end
%--------------------------------------------------------------------------

%convert these back into two 16-bit integers:
c3d.Parameters.TRIAL.ACTUAL_START_FIELD=double(typecast(int32(c3d.Parameters.TRIAL.ACTUAL_START_FIELD),'int16'));
c3d.Parameters.TRIAL.ACTUAL_END_FIELD=double(typecast(int32(c3d.Parameters.TRIAL.ACTUAL_END_FIELD),'int16'));

%calculate size of parameter record:
BS=4; %byte size of parameter header;

GN=fieldnames(c3d.Parameters); %get group names
for n1=1:length(GN)
    if ismember((GN{n1}),fieldnames(Setup)) %check this group is in the setup
        BS=BS+5+length(GN{n1});
        PN=fieldnames(c3d.Parameters.(GN{n1})); %get parameter names
        for n2=1:length(PN)
            if ismember((PN{n2}),fieldnames(Setup.(GN{n1})))
                if strcmp('MODEL_PARAMETERS',PN{n2}) %model constants
                    B=Setup.(GN{n1}).(PN{n2});
                    C=size(c3d.Parameters.(GN{n1}).MODEL_PARAMETERS,1); %number of values
                    c3d.Setup.(GN{n1}).(PN{n2})=repmat(B,[C,1]);
                    NL=cellfun('length',c3d.Parameters.(GN{n1}).MODEL_PARAMETERS); %name lengths
                    BS=BS+8*C+sum(NL(:,1))+C*prod(B{3})*abs(B{1});
                else
                    B=Setup.(GN{n1}).(PN{n2});
                    c3d.Setup.(GN{n1}).(PN{n2})=B;
                    BS=BS+7+length(PN{n2})+prod(B{3})*abs(B{1})+B{2};
                end
            elseif ~strcmp('Description',PN{n2})
                warning(['The following parameter does not have a setup structure: ', GN{n1},'.',PN{n2}])
            end

        end
    elseif ~any(strcmp({'nParamBlocks','ProcessorType'},GN{n1}))
        warning(['The following group does not have a setup structure: ', GN{n1}])
    end
end

%update Header and Parameter records:
c3d.Parameters.nParamBlocks=ceil(BS/512);
c3d.Header.DataStart=c3d.Parameters.nParamBlocks+c3d.Header.Pblock;
c3d.Parameters.POINT.DATA_START=c3d.Header.DataStart;

%==========================================================================
% Finally arrange data ready for writing:
%--------------------------------------------------------------------------

if any(CHECK([3,6])) %marker data and/or model outputs present
    RESIDUALS=RESIDUALS/abs(c3d.Parameters.POINT.SCALE); %scale residuals
    CN=isnan(POINTS(:,1,:)); %check for gaps
    RESIDUALS(CN)=-1;
    POINTS(repmat(CN,[1,3,1]))=0; %replace NaN with zeros
    c3d.OutputData=reshape([POINTS,RESIDUALS],[nFRAMES,4*nPOINTS]);
end

if any(CHECK([4,5])) %analogue and/or force plate data present
    if ismember('OutputData',fieldnames(c3d))
        c3d.OutputData=[c3d.OutputData,reshape(ANALOG',[ApF,nFRAMES])'];
    else
        c3d.OutputData=reshape(ANALOG',[ApF,nFRAMES])';
    end
end


%Make sure everything is present:
A={'Header','Parameters','Setup','OutputData'};
CHECK1=~ismember(A,fieldnames(c3d));
if any(CHECK1)
    F=find(CHECK1);
    for n=1:sum(CHECK1)
        disp(['There is no ',A{F(n)},' ready for writing to a c3d file...'])
    end
    error('Data missing. Will not be able to write to c3d file.')
end

%remove everything except Header, Parameters, Setup and OutputData:
A={'Markers','Analogue','ForcePlate','ModelOutputs','Filename'};
CHECK2=ismember(A,fieldnames(c3d));
for n=1:length(A)
    if CHECK2(n)
        c3d=rmfield(c3d,A{n});
    end
end

%============================= END ========================================

end



