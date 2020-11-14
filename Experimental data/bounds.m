% Get variability in change in horizontal velocity and swing time to use as
% bounds for the optimisations
%
%%
clear; close all; clc
load data.mat
%%
% n = dout.Average.Information.StridesUsed;
% n = 1:24;

for i = 1:length(n)
    dvcmx(i) = dout.IndvStrides.CoM.Data{n(i)}(end,2,2) - ...
        dout.IndvStrides.CoM.Data{n(i)}(1,2,2);
    dsw(i) = dout.IndvStrides.Parameters.StrideTime(n(i)) - ...
        dout.IndvStrides.Parameters.ContactTime(n(i));
end

s1 = std(dvcmx)
s2 = std(dsw)