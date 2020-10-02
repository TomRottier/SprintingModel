load data.mat
points = dout.Average.Markers.Data.Avg;
mnames = dout.Average.Markers.Names;
mnames = strrep(mnames, '_', '');       % Remove all '_' from marker names

% Points
rTOE = points(:,:,contains(mnames, 'RToe')); lTOE = points(:,:,contains(mnames, 'LToe'));
rAJC = points(:,:,contains(mnames, 'RAJC')); lAJC = points(:,:,contains(mnames, 'LAJC'));
rKJC = points(:,:,contains(mnames, 'RKJC')); lKJC = points(:,:,contains(mnames, 'LKJC'));
rHJC = points(:,:,contains(mnames, 'RHJC')); lHJC = points(:,:,contains(mnames, 'LHJC'));
rSJC = points(:,:,contains(mnames, 'RSJC')); lSJC = points(:,:,contains(mnames, 'LSJC'));
rEJC = points(:,:,contains(mnames, 'REJC')); lEJC = points(:,:,contains(mnames, 'LEJC'));
rWJC = points(:,:,contains(mnames, 'RWJC')); lWJC = points(:,:,contains(mnames, 'LWJC'));
LTJC = points(:,:,contains(mnames, 'LTJC'));
UTJC = points(:,:,contains(mnames, 'UTJC'));
APEX = points(:,:,contains(mnames, 'APEX'));
HJC = (rHJC + lHJC) ./ 2;

% CoM
WBCM = dout.Average.CoM.Data.Avg(:,:,1);
upperCM = importdata('HAT.txt', ' ', 2);
swingCM = importdata('swing.txt', ' ', 2); 
upperCM = upperCM.data(:,[1 2]) + HJC(:,[2 3]);
swingCM = swingCM.data(:,[1 2]) + HJC(:,[2 3]);

set(figure(4),'WindowStyle','docked'); cla
xlim([-1 1]); ylim([0 2]); hold on
for i = 1%:5:length(points)
    cla
    % Right leg
    line([rTOE(i,2) rAJC(i,2) rKJC(i,2) rHJC(i,2)], ...
         [rTOE(i,3) rAJC(i,3) rKJC(i,3) rHJC(i,3)])    
    % Left leg
    line([lTOE(i,2) lAJC(i,2) lKJC(i,2) lHJC(i,2)], ...
         [lTOE(i,3) lAJC(i,3) lKJC(i,3) lHJC(i,3)])
    % Trunk
    line([HJC(i,2) LTJC(i,2) UTJC(i,2) APEX(i,2)], ...
         [HJC(i,3) LTJC(i,3) UTJC(i,3) APEX(i,3)])
    % Right arm
    line([rSJC(i,2) rEJC(i,2) rWJC(i,2)], ...
         [rSJC(i,3) rEJC(i,3) rWJC(i,3)])
    % Left arm
    line([lSJC(i,2) lEJC(i,2) lWJC(i,2)], ...
         [lSJC(i,3) lEJC(i,3) lWJC(i,3)])
    % Whole-body CoM
    plot(WBCM(i,2), WBCM(i,3), 'ko') 
    plot(upperCM(i,1), upperCM(i,2), 'kx')
    plot(swingCM(i,1), swingCM(i,2), 'kx')
%     plot(legCM(i,2), legCM(i,3), 'kx')
    drawnow
end

