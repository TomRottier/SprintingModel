# Energy and momentum conservation

Torque generator activations and damping coefficents for sprintgs set to zero, therefore no energy input into the model and no losses through damping. Tests whether equations of motion are correct and model conserves energy. Further, setting spring stiffnesses to zero means no external forces and therefore no external torques, showing angular momentum (about the CoM) is also conserved.

To note:

- Model conserves energy  only when mass functions set to 0 as inconsistencies in data. Even with no external forces CoM velocitiy changes slightly with mass functions. Setting mass functions as consistent functions (e.g. polynomials with algebraic derivatives), gives a constant CoM velocity - although not exact energy or momentum conservation, perhaps just due to numerical vs algebraic differentiation.

- Conservation only holds with spring forces if contact model is linear. Contact model changed to linear spring to show conservation holds when external forces applied.
