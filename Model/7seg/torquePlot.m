%% Torque generator plots
hipw = [1.2995 1.0 26.0 7.94]; hipa = [4.93 1.64];
kneew = [1.2981 1.0 36.0 5.44]; kneea = [4.31 0.74];
anklew = [1.375 1.0 30.8 15.38]; anklea = [4.22 0.37];
t0 = [645.0 522.0 341.3];
PV = cat(3, hipw,kneew,anklew); PA = cat(3, hipa,kneea,anklea);
datas1 = datass; datas2 = datast;
lin1 = 'k--'; 
lin2 = 'k:';
titles = {'Hip','Knee','Ankle'};
set(figure(),'WindowStyle','docked'); clf

for i = 1:3
    data1 = datas1{i+6}; data2 = datas2{i+6};
    pv = PV(:,:,i); pa = PA(:,:,i);
    time1 = data1(:,1); time2 = data2(:,1);
    tq1 = data1(:,2); tq2 = data2(:,2);
    act1 = data1(:,3); act2 = data2(:,3);
    [tv1, ta1] = tqfunc(deg2rad(-data1(:,5)), deg2rad(data1(:,4)), pv, pa);  
    [tv2, ta2] = tqfunc(deg2rad(-data2(:,5)), deg2rad(data2(:,4)), pv, pa);  
    
    % Torque
    subplot(4,3,i); hold on; cla
    plot(time1,tq1, lin1)
    plot(time2,tq2, lin2)
    ylim([0 450])
    title(titles{i})
    
    % Activation
    subplot(4,3,i+3); hold on; cla
    plot(time1, act1, lin1)
    plot(time2, act2, lin2)
    ylim([0 1.0])
    
    % Torque-angle
    subplot(4,3,i+6); hold on; cla
    plot(time1, ta1, lin1)
    plot(time2, ta2, lin2)
    ylim([0 1.0])
    
    % Torque-velocity
    subplot(4,3,i+9); hold on; cla
    plot(time1, tv1, lin1)
    plot(time2, tv2, lin2)
    ylim([0 1.5])
    
%     mean_tv(i,:) = [mean(tv1) mean(tv2)];
end    

% Labels
subplot(4,3,1); ylabel('Torque (N.m)')
subplot(4,3,4); ylabel('Activation')
subplot(4,3,7); ylabel('Torque-Angle')
subplot(4,3,10); ylabel('Torque-velocity')
subplot(4,3,11); xlabel('Time (s)')

%% Functions
function [tv, ta] = tqfunc(ccangvel,ccang,pv,pa)

tv = nan(size(ccangvel));

tc = pv(2)*pv(4)/pv(3);
c  = tc*(pv(3)+pv(4));
we = ((pv(1)-pv(2))/(4.3*pv(2)))*((pv(3)*pv(4))/(pv(3)+pv(4)));
e  = -we*(pv(1)-pv(2));


for i = 1:length(tv)
    if ccangvel(i) > 0
        tv(i) = c ./ (pv(4) + ccangvel(i)) - tc;
    else 
        tv(i) = e ./ (we - ccangvel(i)) + pv(1);
    end
end
ta = exp((-(ccang-pa(1)).^2) / (2.0*pa(2).^2));

end
    
