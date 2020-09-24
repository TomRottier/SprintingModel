function c3d = readC3D(filenames,output)
%Reads data from .c3d files

% The instructions for reading/creating .c3d files comes from the .pdf that
% was downloaded with C3DServer: C3DFormat.pdf

%==========================================================================
% INPUTS:
%--------------------------------------------------------------------------
%
% filenames = one or more files to open (full file name and path)
%    output = 'simple' (default) or 'full' [include header and parameters]
%
%--------------------------------------------------------------------------
% NOTES
%--------------------------------------------------------------------------
% 1. If no input is used then a gui is opened to select which file to read.
%
% 2. If output choice is not declared, 'simple' is used as the default.
%    This will mean no header or parameter record will be outputted.
%
%==========================================================================
% OUTPUTS:
%--------------------------------------------------------------------------
%
% c3d = a structure array with all relevant data, including:
%
%     c3d.Markers = Data (marker coordinates [nFrames, XYZ, nMarkers])
%                   Names (marker names in cell array {nMarkers,1})
%                   Frames and Time (column array [nFrames, 1])
%                   GapID (identification of markers with gaps in Data)
%                   GapNames (Name of markers with gaps = Names(GapID))
%    c3d.Analogue = Data (raw analogue data [nFramesA by nChannels])
%                   Channels (channel names in cell array {1,nChannels})
%                   Units (channel units in cell array {1, nChannels})
%                   Descriptions (channels descriptions {1, nChannels})
%                   Time (column array [nFramesA, 1])
%  c3d.ForcePlate = Data (forces, moments, and COP [nFramesA, 12, nPlates])
%                   Names (names data in cell array {nPlates, 12})
%                   Time (column array [nFramesA, 1])
%                   Corners (coordinates of  corners [4, 3, nPlates])
%                   R (rotation matrix from FP LCS to GCS [3, 3])
%                   Origin (FP LCS origins relative to GCS [3, nPlates])
%c3d.ModelOutputs = Angles (modelled angle data in markers format)
%                   Forces (modelled force data in markers format)
%                   Moments (modelled moment data in markers format)
%                   Points (modelled point data in markers format)
%    c3d.Filename = Original filename for this c3d file [Note 2]
%      c3d.Header = Header record needed to create this c3d file [Note 2]
%  c3d.Parameters = Parameter data needed to create this c3d file [Note 2]
%
%--------------------------------------------------------------------------
% NOTES
%--------------------------------------------------------------------------
% 1. If more than one file is selected then the output is a structure with
%    multiple stuctures within it (in the above arrangement) with each file
%    identified by it's file name, such as:
%
%       c3d.filename1.Markers
%       c3d.filename2.Markers
%
% 2. The original filename, header record (c3d.Header), and parameter
%    record (c3d.Parameter) are only outputted if the 'full' output has
%    been selected.
%
% 3. Any force data in c3d.Analogue.Data will be in the force plate local
%    coordinate system (LCS) and not the global coordinate system (GCS) of
%    Vicon. If the rotation matrix (c3d.ForcePlate.R) is an identity matrix
%    the these will be the same as in c3d.ForcePlate.Data. Force plate data
%    are usualy in reaction forces when saved from Vicon.
%
% 4. If any data is not in the file (markers, analogue, force plate,
%    model outputs) then this part of the structure will be missing.
%
%==========================================================================

if nargin==0 %no input, use gui to get filenames
    [fname,pname]=uigetfile('*.c3d','Select one or more C3D data files to read...','Multiselect','on');
    if ~iscell(fname)
        fname={fname};
    end
    if fname{1}==0 %selection cancelled
        c3d=false; %retutn something
        return %go back to calling function
    end
    filenames=strcat(pname,fname');
end
if nargin < 2
    output='full';
end

if ~iscell(filenames)
    filenames={filenames};
end

for n=1:length(filenames)
    ThisFile=filenames{n};
    fid=fopen(ThisFile,'r','n'); %open file to read in native format
    if fid==-1 %file did not open correctly
        errordlg(['Could not open file:',ThisFile,],'File error...')
        continue %more to next file
    end
    
    %create structure for storing data:
    ThisC3D=struct('Filename',filenames{n},'Header',[],'Parameters',[],...
        'Events',[],'Markers',[],'Analogue',[]);
    
%==========================================================================
% HEADER:
%--------------------------------------------------------------------------

    Pblock=fread(fid,1,'int8'); %number of parameter block
    KeyValue=fread(fid,1,'int8'); %key value indicating a c3d file (should be 80)
    nPoints=fread(fid,1,'int16'); %number of 3D points (markers)
    ApF=fread(fid,1,'int16'); %total number of analogue measurements per 3D frame
    FirstFrame=fread(fid,1,'uint16'); %first 3D frame
    LastFrame=fread(fid,1,'uint16'); %last 3D frame (max = 32767 or 65537)
    MaxInt=fread(fid,1,'int16'); %maximum interpolation gap
    ScaleFactor=fread(fid,1,'single'); %3D scale factor (negative = scaled)
    DataStart=fread(fid,1,'int16'); %start of data block
    SpF=fread(fid,1,'int16'); %number of analogue samples per frame
    Hz=fread(fid,1,'single'); %3D frame rate
    NT1=fread(fid,135,'int16'); %not used
    LabelRange=fread(fid,1,'int16'); %key value (if label and range dat present) (should be 12345)
    LRblock=fread(fid,1,'int16'); %first block of label and range data
    KeyLabel=fread(fid,1,'int16'); %key value (if file supports 4 character event labels) (should be 12345)
    nEvents=fread(fid,1,'int16'); %number of defined time events
    NT2=fread(fid,1,'int16'); %not used
    EventTimes=fread(fid,18,'single'); %event times
    EventFlags=fread(fid,18,'int8'); %event flags
    NT3=fread(fid,1,'int16'); %not used
    EventLabels=fread(fid,18,'single'); %event labels (4 characters long) ************************ NEEDS WORK *******************************
    NT4=fread(fid,22,'int16'); %not used
    
    if SpF > 0
        AC=ApF/SpF; %number of analogue channels
    else
        AC=0;
    end
    
    %Store the original Header information:
    ThisC3D.Header.Pblock=Pblock;
    ThisC3D.Header.KeyValue=KeyValue;
    ThisC3D.Header.nPoints=nPoints;
    ThisC3D.Header.ApF=ApF;
    ThisC3D.Header.FirstFrame=FirstFrame;
    ThisC3D.Header.LastFrame=LastFrame;
    ThisC3D.Header.MaxInt=MaxInt;
    ThisC3D.Header.ScaleFactor=ScaleFactor;
    ThisC3D.Header.DataStart=DataStart;
    ThisC3D.Header.SpF=SpF;
    ThisC3D.Header.Hz=Hz;
    ThisC3D.Header.NT1=NT1;
    ThisC3D.Header.LabelRange=LabelRange;
    ThisC3D.Header.LRblock=LRblock;
    ThisC3D.Header.KeyLabel=KeyLabel;
    ThisC3D.Header.nEvents=nEvents;
    ThisC3D.Header.NT2=NT2;
    ThisC3D.Header.EventTimes=EventTimes;
    ThisC3D.Header.EventFlags=EventFlags;
    ThisC3D.Header.NT3=NT3;
    ThisC3D.Header.EventLabels=EventLabels;
    ThisC3D.Header.NT4=NT4;
        
%==========================================================================
% Parameters:
%--------------------------------------------------------------------------
    fseek(fid,(Pblock-1)*512,'bof'); %go to start of parameter block

    PNT1=fread(fid,1,'int8'); %should be 1
    PNT2=fread(fid,1,'int8'); %should be 80
    ThisC3D.Parameters.nParamBlocks=fread(fid,1,'int8'); %number of Parameter Blocks
    ProType=fread(fid,1,'int8')-83; %processor type
    switch ProType
        case 1 %should be this one
            ThisC3D.Parameters.ProcessorType='Intel';
        case 2
            ThisC3D.Parameters.ProcessorType='DEC';
        case 3
            ThisC3D.Parameters.ProcessorType='MIPS';
    end
    %loop through parameter blocks:
    offset=1;
    while offset~=0 %until end of paramater blocks
        nChar=fread(fid,1,'int8');	%number of characters in name
        N=fread(fid,1,'int8');      %get ID number
        
        if N<0 %is a group
            NAMES(abs(N))=cellstr(fread(fid,[1,nChar],'*char')); %name        
            offset=fread(fid,1,'int16');
            LD=fread(fid,1,'int8'); %length of group description
            ThisC3D.Parameters.(NAMES{abs(N)}).Description=fread(fid,[1,LD],'*char'); %group description
        else %is a parameter
            pName=fread(fid,[1,nChar],'*char'); %name
            FP=ftell(fid);    %current file position
            offset=fread(fid,1,'int16');
            next=FP+offset; %next group/parameter
            if offset==0
                break
            end

            EL=fread(fid,1,'int8'); %length of each data element
            d=fread(fid,1,'int8'); %number of dimensions of this parameter
            if d==0
                D=1; %parameter dimensions
            else
                D=fread(fid,d,'uint8'); %parameter dimensions
            end
            switch EL
                case -1 %Characters
                    if d < 3
                        temp=fread(fid,D','*char')';
                    elseif d==3
                        temp=char;
                        for z=1:D(3)
                            temp(1:D(2),1:D(1),z)=fread(fid,D(1:2)','*char')';
                        end
                    else
                        fclose(fid); %close this file
                        error('Too many dimensions')
                    end
                case 1 %uint8 or int8
                    temp=fread(fid,D,'int8');
                case 2 %uint16 or int16
                    temp=fread(fid,D','int16');
                case 4 %single
                    if d < 3
                        temp=fread(fid,D','single');
                    elseif d==3
                        temp=zeros(D');
                        for z=1:D(3)
                            temp(:,:,z)=fread(fid,D(1:2)','single');
                        end
                    else
                        fclose(fid); %close this file
                        error('Too many dimensions')
                    end
            end
            %check for actual start/end field:
            if strcmp(NAMES{abs(N)},'TRIAL')
                if ~isempty(strfind(pName,'ACTUAL_'))
                    %convert from 2 16-bit integers to 1 32-bit integer:
                    temp=temp(2)*2.^16+temp(1);
                end
            end
            if strcmp(NAMES{abs(N)},'PROCESSING') %model constants
                if length(fieldnames(ThisC3D.Parameters.PROCESSING))==1
                    ThisC3D.Parameters.(NAMES{abs(N)}).MODEL_PARAMETERS={pName,temp};
                else
                    ThisC3D.Parameters.(NAMES{abs(N)}).MODEL_PARAMETERS(end+1,:)={pName,temp};
                end
            else
                ThisC3D.Parameters.(NAMES{abs(N)}).(pName)=temp;
            end

            LD=fread(fid,1,'int8'); %length of group description
            if LD>0
                Q=fread(fid,[1,LD],'*char');
                warning([NAMES{abs(N)},'.',pName,' - ',Q])
            end  
            fseek(fid,next,'bof'); %go to next group/parameter
        end
    end
    
    %update events (if present):
    if ThisC3D.Parameters.EVENT.USED > 0 %events are present
        ThisC3D.Events=ThisC3D.Parameters.EVENT;
    else
        ThisC3D=rmfield(ThisC3D,'Events');
    end
    
%==========================================================================
% DATA:
%--------------------------------------------------------------------------
    fseek(fid,(DataStart-1)*512,'bof'); %go to start of data block
    temp=fread(fid,'single');

    %close this file
    p9=ftell(fid);    %current file position
    fclose(fid); 

    L1=nPoints*4+ApF;
    try
        nFrames=ThisC3D.Parameters.TRIAL.ACTUAL_END_FIELD-ThisC3D.Parameters.TRIAL.ACTUAL_START_FIELD+1;
    catch %need to estimate
        nFrames=floor(length(temp)/L1); %end will be padded with zeros
    end
    if nFrames > 2^16-1 %number of frames too large for header
        try
            ThisC3D.Header.LastFrame=ThisC3D.Parameters.TRIAL.ACTUAL_END_FIELD;
        catch
            ThisC3D.Header.LastFrame=ThisC3D.Header.FirstFrame+nFrames-1;
        end
        ThisC3D.Parameters.POINT.FRAMES=nFrames;
    end
    temp=reshape(temp(1:nFrames*L1),[L1,nFrames])'; %split into frames
    if rem(p9,512)==0 %do this to preserve zeros
        try
            while all(temp(end,:)==0) % this is not needed in newer files, could use the file size to figure this out
                warning('Removing last row of data (all zeros)')
                temp(end,:)=[]; %remove zeros
                nFrames=nFrames-1;
            end
        catch
            warning('There appears to be no data in this file...')
        end
    end
    
    %get marker data (if present):
    if ThisC3D.Parameters.POINT.USED > 0
        MARKERS=reshape(temp(:,1:nPoints*4),[nFrames,4,nPoints]);
        gaps=repmat(MARKERS(:,4,:)<0,[1,4]); %find gaps
        MARKERS(gaps)=nan; %replace gaps with nan
        RESIDUALS=permute(MARKERS(:,4,:),[1,3,2])*abs(ThisC3D.Parameters.POINT.SCALE);
        MARKERS(:,4,:)=[]; %remove residuals
        gaps(:,4,:)=[];
        LABELS=cellstr(ThisC3D.Parameters.POINT.LABELS); %get Marker Labels
        
        %get model outputs (if present):
        if ismember('TYPE_GROUPS',fieldnames(ThisC3D.Parameters.POINT))
            %list of all possible model outputs
            MON1={'Angles','Forces','Moments','Points','Scalars','Powers','Reactions'};
            MON2={'ANGLES','ANGLE';...
                  'FORCES','FORCE';...
                  'MOMENTS','MOMENT';...
                  'MODELED_MARKERS','MODELED_MARKER';...
                  'SCALARS','SCALAR';...
                  'POWERS','POWER';...
                  'REACTIONS','REACTION'};
            for m=1:7
                ThisM=ismember(MON2(m,:),fieldnames(ThisC3D.Parameters.POINT));
                if any(ThisM) %get angles:
                    if m==4 %points found, leave them with the others
                    else
                        if ThisM(1)
                            ThisC3D.ModelOutputs.(MON1{m}).Names=cellstr(ThisC3D.Parameters.POINT.(MON2{m,1}));
                        else
                            ThisC3D.ModelOutputs.(MON1{m}).Names=cellstr(ThisC3D.Parameters.POINT.(MON2{m,2}));
                        end
                        ThisID=ismember(LABELS,ThisC3D.ModelOutputs.(MON1{m}).Names);
                        ThisC3D.ModelOutputs.(MON1{m}).Data=MARKERS(:,:,ThisID);
                        ThisGaps=permute(any(gaps(:,1,ThisID)),[3,2,1]);
                        if any(ThisGaps) %there are gaps in this one
                            ThisC3D.ModelOutputs.(MON1{m}).GapID=find(ThisGaps);
                            ThisC3D.ModelOutputs.(MON1{m}).GapNames=ThisC3D.ModelOutputs.(MON1{m}).Names(ThisGaps);
                        end
                        %Remove this data from the main group:
                        MARKERS(:,:,ThisID)=[];
                        RESIDUALS(:,ThisID)=[]; %these should all be zero
                        LABELS(ThisID)=[];
                        gaps(:,:,ThisID)=[];
                    end
                end
            end
        end
        
        %Save Marker structure
        ThisC3D.Markers.Names=LABELS;
        ThisC3D.Markers.Data=MARKERS;
        ThisC3D.Markers.Residuals=RESIDUALS;
        ThisC3D.Markers.Frames=(ThisC3D.Parameters.TRIAL.ACTUAL_START_FIELD(1):ThisC3D.Parameters.TRIAL.ACTUAL_START_FIELD(1)+nFrames-1)';
        ThisC3D.Markers.Time=(ThisC3D.Markers.Frames-1)/ThisC3D.Parameters.TRIAL.CAMERA_RATE;
        if any(gaps(:))
            ThisC3D.Markers.GapID=find(any(gaps(:,1,:)));
            ThisC3D.Markers.GapNames=ThisC3D.Markers.Names(ThisC3D.Markers.GapID);
        end
    else
        ThisC3D=rmfield(ThisC3D,'Markers');
    end
    
    %get analogue data (if present):
    if ThisC3D.Parameters.ANALOG.USED > 0
        
        ThisC3D.Analogue.Channels=cellstr(ThisC3D.Parameters.ANALOG.LABELS)';
        st=strfind(ThisC3D.Analogue.Channels,'.');
        if any(~cellfun('isempty',st)) %if any have a full stop -> remove ---------------------------------------------MORE NEEDED--------------------------------------------------
            for n1=1:length(st)
                if ~isempty(st{n1})
                    ThisC3D.Analogue.Channels{n1}=ThisC3D.Analogue.Channels{n1}(st{n1}+1:end);
                end
            end
        end
        ThisC3D.Analogue.Units=cellstr(ThisC3D.Parameters.ANALOG.UNITS)';
        ThisC3D.Analogue.Descriptions=cellstr(ThisC3D.Parameters.ANALOG.DESCRIPTIONS)';
        ThisC3D.Analogue.Data=reshape(temp(:,nPoints*4+1:end)',[AC,nFrames*SpF])';
        ThisC3D.Analogue.Time=(0:size(ThisC3D.Analogue.Data,1)-1)'/ThisC3D.Parameters.ANALOG.RATE+(ThisC3D.Parameters.TRIAL.ACTUAL_START_FIELD(1)-1)/ThisC3D.Parameters.TRIAL.CAMERA_RATE;
    else
        ThisC3D=rmfield(ThisC3D,'Analogue');
    end

    %sort out force plates (if present):
    if ThisC3D.Parameters.FORCE_PLATFORM.USED > 0
        nPlates=ThisC3D.Parameters.FORCE_PLATFORM.USED;
        %TYPE 2 = [Fx, Fy, Fz, Mx, My, Mz] {all data is processed not RAW}
        if any(ThisC3D.Parameters.FORCE_PLATFORM.TYPE ~= 2)
            warndlg('The force plate type is not consistent with Vicon')
            break
        end
        
        %re-orientate data:
        ThisC3D.Parameters.FORCE_PLATFORM.CORNERS=permute(ThisC3D.Parameters.FORCE_PLATFORM.CORNERS,[2,1,3]);
        
        %get corners of FP in vicon coordinate system:
        corners=ThisC3D.Parameters.FORCE_PLATFORM.CORNERS;
        
        %calculate centre of FP surface in Vicon coordinate system:
        FPorigin=permute(mean(corners),[3,2,1]);

        %get orientation of force plates in vicon coordinate system:
        X=round(mean(mean(corners([1,4],:,:))-mean(corners([2,3],:,:)),3),3);
        X=X/norm(X);
        Y=round(mean(mean(corners([1,2],:,:))-mean(corners([3,4],:,:)),3),3);
        Y=Y/norm(Y);
        Z=cross(X,Y);
        R=[X;Y;Z];
        
        %get origin of FP in Vicon coordinate system:
        ORIGIN=(R*ThisC3D.Parameters.FORCE_PLATFORM.ORIGIN)';

        %Calculate variables and store in 3D array:
        FP=zeros(nFrames*ThisC3D.Header.SpF,12,nPlates);
        FPnames=repmat({'Fx','Fy','Fz','Mx','My','Mz','COPx','COPy','COPz','Tx','Ty','Tz'},[nPlates,1]);
        for p=1:nPlates
            %Get force data in force plate coordinate system:
            F=ThisC3D.Analogue.Data(:,ThisC3D.Parameters.FORCE_PLATFORM.CHANNEL(1:3,p)); %force
            M=ThisC3D.Analogue.Data(:,ThisC3D.Parameters.FORCE_PLATFORM.CHANNEL(4:6,p))+[-ORIGIN(p,3)*F(:,2),ORIGIN(p,3)*F(:,1),zeros(size(F,1),1)]; %moment
            C=[-M(:,2)./F(:,3),M(:,1)./F(:,3),zeros(size(F,1),1)]; %COP
            T=[zeros(size(F,1),2),M(:,3)-C(:,1).*F(:,2)+C(:,2).*F(:,1)]; %free moment
            FP(:,:,p)=[(R*F')',(R*M')',(R*C')'+repmat(FPorigin(p,:),[size(F,1),1]),(R*T')']; %convert into vicon coordinate system
            FPnames(p,:)=strcat(FPnames(p,:),num2str(p));
        end
        ThisC3D.ForcePlate.Data=FP;
        ThisC3D.ForcePlate.Names=FPnames;
        ThisC3D.ForcePlate.Time=ThisC3D.Analogue.Time;
        ThisC3D.ForcePlate.Corners=corners;
        ThisC3D.ForcePlate.R=R;
        ThisC3D.ForcePlate.Origin=[FPorigin(:,1:2),ORIGIN(:,3)];
    end
    
    %save structure:
    if ~strcmpi(output,'full')
        ThisC3D=rmfield(ThisC3D,{'Header','Parameters'});
    end
    
    if length(filenames) == 1
        c3d=ThisC3D;
    else
        f=strfind(ThisFile,'\');
        if isempty(f)
            ShortName=ThisFile(1:end-4);
        else
            ShortName=ThisFile(f(end)+1:end-4);
        end
        f=strfind(ShortName,' ');
        if ~isempty(f) %replace spaces with underscores (shouldn't get to here)
            ShortName(f)='_';
        end
        f=strfind(ShortName,'.');
        if ~isempty(f) %replace full stops with underscores (shouldn't get to here)
            ShortName(f)='_';
        end
        c3d.(ShortName)=ThisC3D;
    end

end

%============================= END ========================================
end

