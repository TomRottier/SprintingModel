function fdata=filtmat2(dt,cutoff,forder,data,ftype)
%filters data with a double pass using a Butterworth filter

%==========================================================================
% INPUTS:
%--------------------------------------------------------------------------
%     dt = time step (1/Hz)
% cutoff = cutoff frequency
% forder = filter order
%   data = data to be filtered
%  ftype = type of filter ('low'[default], 'high', 'stop', 'bandpass')
%
%--------------------------------------------------------------------------
% NOTES
%--------------------------------------------------------------------------
% 1. The functions for padding data have been removed and included in this
%    function using mirrors of the whole data to pad
%
% 2. The cutoff should be a single value for a lowpass or highpass filter,
%    and a 2 by 1 array for a bandstop or bandpass filter 
%
%==========================================================================
% OUTPUTS:
%--------------------------------------------------------------------------
%  fdata = filtered data
%
%==========================================================================

if nargin==4
    ftype='low';
end

n=size(data,1); %how much data

cutoff=cutoff/(sqrt(2)-1)^(0.5/forder); %adjust cut-off for double pass
[b,a] = butter(forder, 2*cutoff*dt, ftype); %compute coefficients

%padd data to allow for end point problems
Ndata=[flipud(-data(2:end,:)+data(1,:)*2);data;flipud(-data(1:end-1,:)+data(end,:)*2)];

%Filter data:
fdata=filtfilt(b,a,Ndata);

%unpad data
fdata=fdata(n:n*2-1,:);


end