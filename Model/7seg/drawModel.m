clear; close all; clc;
%% Import data
fname = 'Evaluation\VCM1000\7SegSprint.1';
f = importdata(fname, ' ', 8); data_sim = f.data;

%% Draw model
set(figure(1),'WindowStyle','docked'); cla; hold on
set(groot, 'DefaultLineLineWidth', 1.5)
set(gca, 'XColor', 'none', 'YColor', 'none')
% xlim([-1 1.5]); ylim([-.1 2.4]); 
axis equal
for i = 1%:1:n
    cla

    line(data_sim(i,2:2:16), data_sim(i,3:2:17), 'Color', 'k')
    line(data_sim(i,[12 18]), data_sim(i,[13 19]), 'Color', 'k')
    line(data_sim(i,[4 8]), data_sim(i,[5 9]), 'Color', 'k')
    
    % Ground
    line(xlim, [0 0], 'Color', 'k', 'LineStyle', '-', 'LineWidth', 0.5)
        
    % Foot springs
    [xs1, ys1] = spring(data_sim(i,2),data_sim(i,3)-0.04,...
        data_sim(i,2),data_sim(i,3),...
        4,0.1,0.01);
    [xs2, ys2] = spring(data_sim(i,4),data_sim(i,5)-0.04,...
        data_sim(i,4),data_sim(i,5));
    plot(xs1,ys1, 'k', 'LineWidth', 0.1)
    plot(xs2,ys2, 'k', 'LineWidth', 0.1)
    
    % Torque generators
    plot(data_sim(i,[8 10 12]), data_sim(i,[9 11 13]), 'ko',...
        'MarkerFaceColor', 'w', 'MarkerSize', 5.5, 'LineWidth', 1)
    
    % Hip
    semi_circle(data_sim(i,12)-0.001, data_sim(i,13), 0.02);   
    
    % MTP spring
    plot(data_sim(i,4), data_sim(i,5), 'ko',...
        'MarkerFaceColor', [.7 .7 .7], 'MarkerSize', 5.5, 'LineWidth', 1)
    
    % Angle-driven joints
    plot(data_sim(i,14), data_sim(i, 15), 'ko',...
        'MarkerFaceColor', 'k', 'MarkerSize', 5.5, 'LineWidth', 1)

    
    drawnow 
    pause(0.01)
end

%% Spring function
function [xs, ys] = spring(xa,ya,xb,yb,varargin)
% SPRING         Calculates the position of a 2D spring
%    [XS YS] = SPRING(XA,YA,XB,YB,NE,A,R0) calculates the position of
%    points XS, YS of a spring with ends in (XA,YA) and (XB,YB), number
%    of coils equal to NE, natural length A, and natural radius R0. 
%    Useful for mass-spring oscillation animations.
% USAGE: in a first call in your code, call it with the full parameters.
% Then, only you have to give it the coordinates of the ends.
% EXAMPLE:
% xa = 0; ya = 0; xb = 2; yb = 2; ne = 10; a = 1; ro = 0.1;
% [xs,ys] = spring(xa,ya,xb,yb,ne,a,ro); plot(xs,ys,'LineWidth',2)
%...
% [xs,ys]=spring(xa,ya,xb,yb); plot(xs,ys,'LineWidth',2)
%
%   Made by:            Gustavo Morales   UC  08-17-09 gmorales@uc.edu.ve
%
persistent ne Li_2 ei b
if nargin > 4 % calculating some fixed spring parameters only once time
    [ne, a, r0] = varargin{1:3};                  % ne: number of coils - a = natural length - r0 = natural radius
    Li_2 =  (a/(4*ne))^2 + r0^2;                % (large of a quarter of coil)^2
    ei = 0:(2*ne+1);                            % vector of longitudinal positions
    j = 0:2*ne-1; b = [0 (-ones(1,2*ne)).^j 0]; % vector of transversal positions
end
R = [xb, yb] - [xa ya]; mod_R = norm(R); % relative position between "end_B" and "end_A"
L_2 = (mod_R/(4*ne))^2; % (actual longitudinal extensión of a coil )^2
if L_2 > Li_2
   error('Spring:TooEnlargement', ...
   'Initial conditions cause pulling the spring beyond its maximum large. \n Try reducing these conditions.')
else
    r = sqrt(Li_2 - L_2);   %actual radius
end
c = r*b;    % vector of transversal positions
u1 = R/mod_R; u2 = [-u1(2) u1(1)]; % unitary longitudinal and transversal vectors 
xs = xa + u1(1)*(mod_R/(2*ne+1)).*ei + u2(1)*c; % horizontal coordinates
ys = ya + u1(2)*(mod_R/(2*ne+1)).*ei + u2(2)*c; % vertical coordinates
end

%% Semi-circle plot
function semi_circle(x,y,r)
    % Vertical filled semi-circle straight edge pointing right
    n = 100;    % Number of verticies
    t = (1:n)./n;
    xn = x - r*sin(pi*t);
    yn = y + r*cos(pi*t);
    
    fill(xn,yn,'k','LineWidth', 0.5)
    
end