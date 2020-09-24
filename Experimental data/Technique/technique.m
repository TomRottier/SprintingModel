clear; close all; clc;
%% Import data
good = importdata('elite_sprinter_distTOproxxypts.csv', ',', 1);
bad = importdata('college_sprinter_distTOproxxypts.csv', ',', 1);

pointsG = good.data;
pointsB = bad.data;

% Reorder data
% Opposite leg td 175, to 285
% to = 50;
% td = 417;
% pointsG = [pointsG(td:end,:); pointsG(1:td,:)];
% pointsB = [pointsB(td:end,:); pointsB(1:td,:)];
    

pointsG = tr_filterDP(pointsG, 1000, 20, 'low', 2);
pointsB = tr_filterDP(pointsB, 1000, 20, 'low', 2);

set(figure(1),'WindowStyle','docked')
% xlim([0 800]); ylim([0 800])
% for i = 1:length(pointsG)
% %     subplot(2,1,1)
%     xlim([0 800]); ylim([0 800])
%     cla
%     hold on
%     line(pointsG(i,1:2:end), pointsG(i,2:2:end), 'Color', 'b')
%     line(pointsB(i,1:2:end) - 600, pointsB(i,2:2:end), 'Color', 'r')
%     
% %     subplot(2,1,2)
% %     cla
% %     hold on
% %     plot(pocmyG, 'b-')
% %     plot(i,pocmyG(i),'bo')
% %     plot(pocmyB, 'r-')
% %     plot(i,pocmyB(i), 'ro')
%     drawnow
% end

%% Joint angles
mtpG = pointsG(:,1:2); mtpB = pointsB(:,1:2);
ajcG = pointsG(:,3:4); ajcB = pointsB(:,3:4);
kjcG = pointsG(:,5:6); kjcB = pointsB(:,5:6);
hjcG = pointsG(:,7:8); hjcB = pointsB(:,7:8);
trkG = pointsG(:,9:10); trkB = pointsB(:,9:10);

[ankG, q3G] = tr_JointAngle2(mtpG,ajcG,kjcG); [ankB, q3B] = tr_JointAngle2(mtpB,ajcB,kjcB);
[kneG, q4G] = tr_JointAngle2(ajcG,kjcG,hjcG); [kneB, q4B] = tr_JointAngle2(ajcB,kjcB,hjcB);
[hipG, q5G] = tr_JointAngle2(kjcG,hjcG,trkG); [hipB, q5B] = tr_JointAngle2(kjcB,hjcB,trkB);

kneG = 360 - kneG; kneB = 360 - kneB;

set(figure(2),'WindowStyle','docked'); clf
subplot(3,1,3)
hold on
plot(ankG); plot(ankB)
% plot(Aabs_avg(:,1,5))
title('ankle')

subplot(3,1,2)
hold on
plot(kneG); plot(kneB)
% plot(Aabs_avg(:,1,4))

title('knee')

subplot(3,1,1)
hold on
plot(hipG); plot(hipB)
% plot(Aabs_avg(:,1,3))
legend('Good', 'Bad')
title('hip')


%% CoM of swinging limb
% POCMX = Q1 + (L5*MC*COS(Q5)+(L3*MB+L4*MC)*COS(Q4)+(L1*MA+L2*MB+L2*MC)*COS(Q3))/(MA+MB+MC)
% POCMY = Q2 + (L5*MC*SIN(Q5)+(L3*MB+L4*MC)*SIN(Q4)+(L1*MA+L2*MB+L2*MC)*SIN(Q3))/(MA+MB+MC)
% Realtive to HJC:
% POCMX = (L5*MC*COS(Q5)+(L3*MB+L4*MC)*COS(Q4)+(L1*MA+L2*MB+L2*MC)*COS(Q3))/(MA+MB+MC) - L2*COS(Q3) - L4*COS(Q4) - L6*COS(Q5)
% POCMY = (L5*MC*SIN(Q5)+(L3*MB+L4*MC)*SIN(Q4)+(L1*MA+L2*MB+L2*MC)*SIN(Q3))/(MA+MB+MC) - L2*SIN(Q3) - L4*SIN(Q4) - L6*SIN(Q5)

l1=.142; l2=.224; l3=.261; l4=.453; l5=.258; l6=.447;
ma=1.385; mb=5.271; mc=12.54;
    
% pocmxG = (l5*mc.*cosd(q5G) + (l3*mb+l4*mc).*cosd(q4G) + (l1*ma+l2*mb+l2*mc).*cosd(q3G)) ./ (ma+mb+mc);
% pocmyG = (l5*mc.*sind(q5G) + (l3*mb*l4*mc).*sind(q4G) + (l1*ma+l2*mb+l2*mc).*sind(q3G)) ./ (ma+mb+mc);
% pocmxB = (l5*mc.*cosd(q5B) + (l3*mb+l4*mc).*cosd(q4B) + (l1*ma+l2*mb+l2*mc).*cosd(q3B)) ./ (ma+mb+mc);
% pocmyB = (l5*mc.*sind(q5B) + (l3*mb*l4*mc).*sind(q4B) + (l1*ma+l2*mb+l2*mc).*sind(q3B)) ./ (ma+mb+mc);
pocmxG = (l5*mc*cosd(q5G)+(l3*mb*l4*mc)*cosd(q4G)+(l1*ma+l2*mb+l2*mc)*cosd(q3G))/(ma+mb+mc) - l2*cosd(q3G) - l4*cosd(q4G) - l6*cosd(q5G);
pocmyG = (l5*mc*sind(q5G)+(l3*mb*l4*mc)*sind(q4G)+(l1*ma+l2*mb+l2*mc)*sind(q3G))/(ma+mb+mc) - l2*sind(q3G) - l4*sind(q4G) - l6*sind(q5G);
pocmxB = (l5*mc*cosd(q5B)+(l3*mb*l4*mc)*cosd(q4B)+(l1*ma+l2*mb+l2*mc)*cosd(q3B))/(ma+mb+mc) - l2*cosd(q3B) - l4*cosd(q4B) - l6*cosd(q5B);
pocmyB = (l5*mc*sind(q5B)+(l3*mb*l4*mc)*sind(q4B)+(l1*ma+l2*mb+l2*mc)*sind(q3B))/(ma+mb+mc) - l2*sind(q3B) - l4*sind(q4B) - l6*sind(q5B);

td = 175; to = 282;

set(figure(3),'WindowStyle','docked'); clf
subplot(2,1,1)
hold on
plot(pocmxG); plot(pocmxB)
ylim(ylim);
line([td td], ylim)
line([to to], ylim)
title('CoMX of swing leg relative to hip joint')

subplot(2,1,2)
hold on
plot(pocmyG); plot(pocmyB)
ylim(ylim);
line([td td], ylim)
line([to to], ylim)
title('CoMY of swing leg relative to hip joint')
legend('good','bad')

doutG = [pocmxG(td:to) pocmyG(td:to)];
doutB = [pocmxB(td:to) pocmyB(td:to)];

% Opposite thigh angle during stance (taken as same as markered leg during
% stance), to = 50; td = 409;
q5G_stance = tr_filterDP([q5G(409:end); q5G(1:50)], 1000, 20, 'low', 2);
q5B_stance = tr_filterDP([q5B(409:end); q5B(1:50)], 1000, 20, 'low', 2);

% Match stance times
diff = length(doutG) - length(q5G_stance);
q5G_stance = interp1(1:length(q5G_stance),q5G_stance,...
             1:length(q5G_stance)+diff,'linear','extrap')';

q5B_stance = interp1(1:length(q5B_stance),q5B_stance,...
             1:length(q5B_stance)+diff,'linear','extrap')';
         
tout = (0:to-td) .* .001;
aoutG = q5G_stance; aoutB = q5B_stance;

set(figure(),'WindowStyle','docked')
subplot(2,1,1)
hold on
plot(q5G_stance, doutG(:,2))
plot(q5B_stance, doutB(:,2))

subplot(2,1,2)
hold on
plot(tout, doutG(:,2))
plot(tout, doutB(:,2))


%% Fit function to data
doutGp = tr_diff(doutG, 0.001); doutBp = tr_diff(doutB, 0.001);
doutGpp = tr_diff(doutGp, 0.001); doutBpp = tr_diff(doutBp, 0.001);
ppGx_time = polyfit(tout, doutG(:,1), 3);
ppGy_time = polyfit(tout, doutG(:,2), 3);
ppBx_time = polyfit(tout, doutB(:,1), 3);
ppBy_time = polyfit(tout, doutB(:,2), 3);

ppGxp_time = polyfit(tout, doutGp(:,1), 3);
ppGyp_time = polyfit(tout, doutGp(:,2), 3);
ppBxp_time = polyfit(tout, doutBp(:,1), 3);
ppByp_time = polyfit(tout, doutBp(:,2), 3);

ppGxpp_time = polyfit(tout, doutGpp(:,1), 5);
ppGypp_time = polyfit(tout, doutGpp(:,2), 3);
ppBxpp_time = polyfit(tout, doutBpp(:,1), 5);
ppBypp_time = polyfit(tout, doutBpp(:,2), 4);

% ppGxp_time = polyder(ppGx_time); ppBxp_time = polyder(ppBx_time);
% ppGxpp_time = polyder(ppGxp_time); ppBxpp_time = polyder(ppBxp_time);
% ppGyp_time = polyder(ppGy_time); ppByp_time = polyder(ppBy_time);
% ppGypp_time = polyder(ppGyp_time); ppBypp_time = polyder(ppByp_time);

% ppGx_ang = polyfit(aoutG, doutG(:,1), 5);
% ppGy_ang = polyfit(aoutG, doutG(:,2), 5);
% ppBx_ang = polyfit(aoutB, doutB(:,1), 5);
% ppBy_ang = polyfit(aoutB, doutB(:,2), 5);
% 
% ppGxp_ang = polyder(ppGx_ang); ppBxp_ang = polyder(ppBx_ang);
% ppGxpp_ang = polyder(ppGxp_ang); ppBxpp_ang = polyder(ppBxp_ang);
% ppGyp_ang = polyder(ppGy_ang); ppByp_ang = polyder(ppBy_ang);
% ppGypp_ang = polyder(ppGyp_ang); ppBypp_ang = polyder(ppByp_ang);

set(figure(4),'WindowStyle','docked')
subplot(3,1,1)
hold on
plot(tout, doutG(:,1), 'ko')
plot(tout, polyval(ppGx_time, tout), 'k-', 'LineWidth', 1.5)
plot(tout, doutB(:,1), 'ro')
plot(tout, polyval(ppBx_time, tout), 'r-', 'LineWidth', 1.5)

subplot(3,1,2)
hold on
plot(tout, doutGp(:,1), 'ko')
plot(tout, doutBp(:,1), 'ro')
plot(tout, polyval(ppGxp_time, tout), 'k-', 'LineWidth', 1.5)
plot(tout, polyval(ppBxp_time, tout), 'r-', 'LineWidth', 1.5)

subplot(3,1,3)
hold on
plot(tout, doutGpp(:,1), 'ko')
plot(tout, doutBpp(:,1), 'ro')
plot(tout, polyval(ppGxpp_time, tout), 'k-', 'LineWidth', 1.5)
plot(tout, polyval(ppBxpp_time, tout), 'r-', 'LineWidth', 1.5)

set(figure(5),'WindowStyle','docked')
subplot(3,1,1)
hold on
plot(tout, doutG(:,2), 'ko')
plot(tout, polyval(ppGy_time, tout), 'k-', 'LineWidth', 1.5)
plot(tout, doutB(:,2), 'ro')
plot(tout, polyval(ppBy_time, tout), 'r-', 'LineWidth', 1.5)

subplot(3,1,2)
hold on
plot(tout, doutGp(:,2), 'ko')
plot(tout, doutBp(:,2), 'ro')
plot(tout, polyval(ppGyp_time, tout), 'k-', 'LineWidth', 1.5)
plot(tout, polyval(ppByp_time, tout), 'r-', 'LineWidth', 1.5)

subplot(3,1,3)
hold on
plot(tout, doutGpp(:,2), 'ko')
plot(tout, doutBpp(:,2), 'ro')
plot(tout, polyval(ppGypp_time, tout), 'k-', 'LineWidth', 1.5)
plot(tout, polyval(ppBypp_time, tout), 'r-', 'LineWidth', 1.5)

% set(figure(),'WindowStyle','docked')
% subplot(3,1,1)
% hold on
% plot(aoutG, doutG(:,2), 'ks')
% plot(aoutB, doutB(:,2), 'rs')
% plot(aoutG, polyval(ppGy_ang, aoutG), 'k-')
% plot(aoutB, polyval(ppBy_ang, aoutB), 'r-')
% 
% subplot(3,1,2)
% hold on
% plot(aoutG, tr_diff(doutG(:,2), 0.001), 'ks')
% plot(aoutB, tr_diff(doutB(:,2), 0.001), 'rs')
% plot(aoutG, polyval(ppGyp_ang, aoutG), 'k-')
% plot(aoutB, polyval(ppByp_ang, aoutB), 'r-')
% 
% subplot(3,1,3)
% hold on
% plot(aoutG, tr_diff(tr_diff(doutG(:,2), 0.001), 0.001), 'ks')
% plot(aoutB, tr_diff(tr_diff(doutB(:,2), 0.001), 0.001), 'rs')
% plot(aoutG, polyval(ppGypp_ang, aoutG), 'k-')
% plot(aoutB, polyval(ppBypp_ang, aoutB), 'r-')

%% Matching technique 
match = importdata('markers9_7_TM.txt', ' ');
n = 111;
mnames = {'MTP','AJC','KJC','HJC','SJC','EJC','WJC'};

% CoM of leg
footCM = match(:,3:4) + (match(:,6:7)-match(:,3:4)) .* (.076/.139);
shankCM = match(:,6:7) + (match(:,9:10)-match(:,6:7)) .* (.261/.453);
thighCM = match(:,9:10) + (match(:,12:13)-match(:,9:10)) .* (.258/.447);
legCM = (footCM.*1.385 + shankCM.*5.271 + thighCM.*12.54) / (1.385+5.271+12.54);

% Relative to hip joint
dmatch = legCM - match(:,12:13);

% During contralateral stance
td = 242; to = 352;
mout = dmatch(td:to,:);
moutp = tr_diff(mout, 0.001);
moutpp = tr_diff(moutp, 0.001);
tout2 = (0:length(mout)-1).*0.001;

% Fit polynomial
ppMx_time = polyfit(tout2, mout(:,1),5);
ppMy_time = polyfit(tout2, mout(:,2),5);
ppMxp_time = polyfit(tout2, moutp(:,1),5);
ppMyp_time = polyfit(tout2, moutp(:,2),5);
ppMxpp_time = polyfit(tout2, moutpp(:,1),5);
ppMypp_time = polyfit(tout2, moutpp(:,2),5);
% ppMxp_time = polyder(ppMx_time); ppMxpp_time = polyder(ppMxp_time);
% ppMyp_time = polyder(ppMy_time); ppMypp_time = polyder(ppMyp_time);

set(figure(6),'WindowStyle','docked')
subplot(3,1,1)
hold on
plot(tout2, mout(:,1), 'o')
plot(tout2, polyval(ppMx_time, tout2))

subplot(3,1,2)
hold on
plot(tout2, moutp(:,1), 'o')
plot(tout2, polyval(ppMxp_time, tout2))

subplot(3,1,3)
hold on
plot(tout2, moutpp(:,1), 'o')
plot(tout2, polyval(ppMxpp_time, tout2))


set(figure(7),'WindowStyle','docked')
subplot(3,1,1)
hold on
plot(tout2, mout(:,2), 'o')
plot(tout2, polyval(ppMy_time, tout2))

subplot(3,1,2)
hold on
plot(tout2, moutp(:,2), 'o')
plot(tout2, polyval(ppMyp_time, tout2))

subplot(3,1,3)
hold on
plot(tout2, moutpp(:,2), 'o')
plot(tout2, polyval(ppMypp_time, tout2))

% set(figure(),'WindowStyle','docked')
% hold on
% for i = 1:length(match)
%     subplot(2,1,1)
%     xlim([-.5 1.5]); ylim([0 2])
%     hold on
%     cla
%     line(match(i,3:3:end), match(i,4:3:end))
%     plot(legCM(i,1), legCM(i,2), 'ko')
%     drawnow
%     
%     subplot(2,1,2)
%     cla
%     hold on
%     plot(dmatch)
%     plot(i,dmatch(i,:), 'ko')
% end
% 

figure()
subplot(1,2,1)
hold on
plot(tout, doutG(:,2), 'k--', 'LineWidth', 2)
plot(tout2, mout(:,2), 'k-', 'LineWidth', 2)
plot(tout, doutB(:,2), 'k:', 'LineWidth', 2)
legend('Elite', 'Experimental data', 'Team sport athlete', 'location', 'northwest')
xlabel('Time (s)')
ylabel('Y position (m)')

subplot(1,2,2)
hold on
plot(tout, doutG(:,1), 'k--', 'LineWidth', 2)
plot(tout2, mout(:,1), 'k-', 'LineWidth', 2)
plot(tout, doutB(:,1), 'k:', 'LineWidth', 2)
xlabel('Time (s)')
ylabel('X position (m)')
