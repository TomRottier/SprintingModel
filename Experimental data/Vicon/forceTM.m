function [TM, options] = forceTM(varargin)
%Calculate Force, Moment, and COP for intrumented treadmill

%==========================================================================
% INPUTS:
%--------------------------------------------------------------------------
%
%	 data = voltages for the 12 force sensors in the instrumented treadmill
%           in an n by 12 array [Fx1, Fy1, Fz1,...,Fx4, Fy4, Fz4]
% options = structure to change default options (see notes below)
%  string = string identifier to change a single option
%   input = input paired with the string identifier (such as: 'az0', -54)
%
%--------------------------------------------------------------------------
% NOTES
%--------------------------------------------------------------------------
% 1. If no input is used then the default options structure is outputted as
%    TM (allows adjustment of multiple parameters).
%
% 2. If there is only one input then this must be data (n by 12 array) and
%    the default options will be used.
%
% 3. If there are two inputs the first must be data (n by 12 array) and the
%    second must be the options structure.
%
% 4. If more than two inputs are used the first must be data (n by 12 array)
%    and the rest must be a string and input pair to declare individual
%    inputs, such as:
%
%       - 'corners',[4 by 3 array of corner coordinates]
%       - 'order',[1 by 12 array indicating sensor channel orders]
%       - 'S',[12 by 12 inverted sensitivity matrix]
%
% 5. The 'order' option must include the index for each channel to align
%    the sensors with the global coordinate system, and using '-' to negate
%    the direction. The default is: [2 1 -3 -5 -4 -6 -8 -7 -9 11 10 -12]
%
%==========================================================================
% OUTPUTS:
%--------------------------------------------------------------------------
%
%   TM = a structure containing the following:
%       - TM.Forces = forces in an n by 3 array (XYZ)
%       - TM.Moments = moments in an n by 3 array (XYZ)
%       - TM.COP = centre of pressure in an n by 3 array (XYZ)
%       - TM.FreeMoments = free moments about COP in an n by 3 array (XYZ)
%       - TM.COF = coefficent of friction in an n by 3 array (XY,abs(XY))
%
%   options = the options structure used for generating the above data
%
%==========================================================================


if nargin==0 %if no inputs, output default options
    TM=DefaultOptions;
    return
else
    data=varargin{1};
    if size(data,2)~=12
        error('Data must be in an n by 12 array')
    end
end

%get options
if nargin==2
    if isstruct(varargin{2})
        options=varargin{2};
        options.default=false; %declare default settings are not used
        names1=fieldnames(options);
        names2=fieldnames(DefaultOptions);
        ch=~ismember(names2,names1);
        if any(ch)
            p=find(ch);
            for n=1:length(p)
                warning(['Options input is incorrect for: ',names2{p(n)}])
            end
            warning('Options input is incorrect. Using default settings')
            options=DefaultOptions; %use default settings
            options.default=true; %declare default settings are used
        end
    else
        warning('Options input is incorrect. Using default settings')
        options=DefaultOptions; %use default settings
    end
else
    options=DefaultOptions; %load default settings
    if nargin > 2 %some other inputs are provided
        options.default=false; %declare default settings are not used
        if rem(nargin,2)==0
            error('Extra inputs must be in string and value pairs')
        end        
        names=fieldnames(options);
        for n=2:2:nargin
            if ischar(varargin{n})
                p=strcmpi(varargin{n},names);
                if ~any(p)
                    error(['The following input was not recognised: ',varargin{n}])
                end
                if ischar(options.(names{p})) %is it a charater input
                    if ischar(varargin{n+1})
                        options.(names{p})=varargin{n+1}; %replace this option
                    elseif strcmp(options.(names{p}),'none')
                        options.(names{p})=varargin{n+1}; %replace this option
                    else
                        error(['A character value is required for the following input: ',varargin{n}])
                    end
                elseif all(size(varargin{n+1})==size(options.(names{p})))
                    options.(names{p})=varargin{n+1}; %replace this option
                else
                    error(['Value input is incorrect for input: ',varargin{n},...
                        '. Requires an input of size: ',num2str(size(options.(names{p})))])
                end
            else
                error('Extra inputs must be in string and value pairs')
            end
        end
    end
end

%calculate raw forces from voltages:
A=(options.S*data')'; 

%Calculate sensor forces in global coordinate systems (applied forces):
A=A(:,abs(options.order));
A(:,options.order<0)=-A(:,options.order<0);

%Calculate forces:
F=A(:,1:3)+A(:,4:6)+A(:,7:9)+A(:,10:12);

%Calculate moments (about centre of top surface):
M=[options.b*(A(:,3)+A(:,6)-A(:,9)-A(:,12))-F(:,2)*options.az0,...
   options.a*(+A(:,3)-A(:,6)-A(:,9)+A(:,12))+F(:,1)*options.az0,...
   options.a*(-A(:,2)+A(:,5)+A(:,8)-A(:,11))+options.b*(-A(:,1)-A(:,4)+A(:,7)+A(:,10))];

%Calculate COP:
C=[-M(:,2)./F(:,3),M(:,1)./F(:,3),zeros(size(A,1),1)];

%Calculate free moments (about COP):
T=[zeros(size(A,1),2),M(:,3)+C(:,2).*F(:,1)-C(:,1).*F(:,2)];

%Check force threshold:
if ~strcmp(options.threshold,'none')
    ch=abs(F(:,3)) <= options.threshold;
    %ch = -F(:,3)) <= options.threshold;
    F(ch,:)=0;
    M(ch,:)=0;
    C(ch,:)=0;
    T(ch,:)=0;
end

%Coefficient of Friction (in TM LCS)
COF=[F(:,1)./F(:,3),F(:,2)./F(:,3)];
COF(:,3)=sqrt(COF(:,1).^2+COF(:,2).^2);

%check to see if the default settings are used (if not, correction needed)
if ~options.default 
    %Calculate the position and orientation of the treadmill:
    try
        P=repmat(mean(options.corners),[size(C,1),1]); %position of treadmill centre
        Xdir=mean(options.corners(2:3,:))-mean(options.corners([1,4],:));
        Ydir=mean(options.corners(1:2,:))-mean(options.corners(3:4,:));
        Xdir=Xdir/norm(Xdir);
        Ydir=Ydir/norm(Ydir);
        Zdir=cross(Xdir,Ydir);
        options.R=[Xdir;Ydir;Zdir]; %rotation matrix for treamill orientation
        if round(norm(options.R),10)~=1 %if not orthogonal
            Zdir=Zdir/norm(Zdir); 
            Xdir=cross(Ydir,Zdir);
            options.R=[Xdir;Ydir;Zdir];
            if round(norm(options.R),10)~=1 %if still not orthogonal
                warning('There is an error with the corners input. Applying no correction for the position and orientation of the treadmill')
                options.R=eye(3);
                P=zeros(size(C));
            end
        end
    catch
        warning('There is an error with the corners input. Applying no correction for the position and orientation of the treadmill')
        options.R=eye(3);
        P=zeros(size(C));
    end

    %Correct for position and orientation of the treadmill:
    F=(options.R*F')';
    M=(options.R*M')';
    T=(options.R*T')';
    C=(options.R*C')'+P;

end

%arrange outputs:
if strcmpi(options.output,'reaction') %output reaction forces
    TM.Forces=-F;
    TM.Moments=-M;
    TM.FreeMoments=-T;
    TM.COP=C;
    TM.COF=COF;
elseif strcmpi(options.output,'applied') %output applied forces
    TM.Forces=F;
    TM.Moments=M;
    TM.FreeMoments=T;
    TM.COP=C;
    TM.COF=COF;
else
    error('Output declaration is not recognised')
end

%==========================================================================
% internal function to create default settings
%--------------------------------------------------------------------------
function options=DefaultOptions

    % Global coordinates are:
    %   +X axis = right
    %   +Y axis = forwards
    %   +Z axis = up

    % Sensor coordinates appear to be (applied forces):
    %   +Fx1 = +Y = forwards
    %   +Fy1 = +X = right
    %   +Fz1 = -Z = down
    %   +Fx2 = -Y = backwards
    %   +Fy2 = -X = left
    %   +Fz2 = -Z = down
    %   +Fx3 = -Y = backwards
    %   +Fy3 = -X = left
    %   +Fz3 = -Z = down
    %   +Fx4 = +Y = forwards
    %   +Fy4 = +X = right
    %   +Fz4 = -Z = down
    
    %store order of sensors to recreate forces in global coordinate system:
    options.order=[2 1 -3 -5 -4 -6 -8 -7 -9 11 10 -12];
    
    %Sensor paramters in global coordinates (XYZ = right, forwards, up):
    options.a=400;      % X distance of sensor from origin (in mm)
    options.b=558.8;	% Y distance of sensor from origin (in mm)
    options.az0=-54;	% Z distance of sensor from origin (in mm)
    options.corners=[-options.a options.b 0;... % sensor 1 - front left
                     options.a options.b 0;...  % sensor 2 - front right
                     options.a -options.b 0;... % sensor 3 - back right
                     -options.a -options.b 0];  % sensor 4 - back left
	
	%Create sensitivity matrix:
    options.Gain=1000;  %gain in amplifier
    options.ExV=10;     %excitation voltage (from amplifier)

    %inverted sensitivity matrices for each sensor:
    M5929=[1.45961, 0.00214, -0.00223;...
          -0.01144, 1.45714, -0.00788;...
           0.02507, 0.00983,  6.22379]; %sensor 1

    M5930=[1.48207, 0.00562,  0.00726;...
          -0.02453, 1.48033, -0.01134;...
           0.00838, 0.01212,  6.19223]; %sensor 2

    M5932=[1.47509, 0.01854,  0.00121;...
          -0.01306, 1.47348, -0.00277;...
           0.02002, 0.02805,  6.23182]; %sensor 3

    M5933=[1.46672, 0.02593, -0.00419;...
          -0.03764, 1.46925, -0.00047;...
           0.00974, 0.02004,  6.19051]; %sensor 4

    %full inverted sensitivey matrix:
    S=zeros(12);
    S(1:3,1:3)=M5929;
    S(4:6,4:6)=M5930;
    S(7:9,7:9)=M5932;
    S(10:12,10:12)=M5933;
    options.S=S./(options.Gain*options.ExV*10e-7); %full inverted matrix
    
    %set a force threshold, below which all data will be set to zero:
    options.threshold='none';
    
    %declare which forces to output:
    options.output='reaction';
    
    %decalre default options are used:
    options.default=true;
    
    %declare standard moments (about centre of top surface):
    options.standard=true;
    
    %rotation matrix for output:
    options.R=eye(3);
    
end
%==========================================================================

end

