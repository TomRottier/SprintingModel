# Log

## 29/10/2020

- Refine HAT angle and CoM location relative to stance hip

## 28/10/2020

- Change angle definitions to match model better
- Match segment lengths from experimental data

## 21/10/2020

- Redo energy and momentum checks
- Model evaluation script

## 20/10/2020

- Add torque measure numbers for angle-driven joints
- HAT mass function now using magnitude of distance from hip to specify along HAT axis 1
- Tidy up repository

## 19/10/2020

- Change angle-driven joint definitions to have joint angles as input
- Angular momentum conservation checks for new model

## 18/10/2020

- Get joint angles from technique video
- Spline coefficients for joint angles  

## 17/10/2020

- New six segment model with angle-driven swing leg and HAT segment

## 14/10/2020

- Change model so that mass functions specified in thigh reference frame
- Complete energy and momentum conservation checks

## 13/10/2020

- Add techniqueCM.m which compares the changes in CoM height with different techqniues

## 12/10/2020

- Calculate quintic smoothing spline coefficients for mass positions (*_coef.csv)
- Change update.bat files to copy coefficients not raw data
- Change base model to read in spline coefficients and evaluate in EQNS1

## 04/10/2020

- Energy and momentum conservation checks
- Change some outputs in base model

## 02/10/2020

- Create model Autolev file
- Create plotting scripts for data and model
- Add filter to getData.m

## 01/10/2020

- Change scripts to output .txt files
- Create all data files for simulations as .txt
- Edit getData.m to average across both legs

## 29/09/2020

- Mass functions script for upper body mass
- Tidy up other functions

## 26/09/2020

- Redo technique script with autolev file for equations of leg CoM

## 25/09/2020

- Tidy up CM.m
- Calculate experimental data whole-body CoM with three segment trunk
- Digitise technique video

## 24/09/2020

- Set-up directory and add to GitHub
- Clean up file structure
