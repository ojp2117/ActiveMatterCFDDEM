# ActiveMatterCFDDEM
CFD-DEM Simulations of Vertically Vibrated Gas-Fluidized Bed with an Active Matter Force

Simulations are set up for MFiX version 22.2.2., but can be adapted to any version of MFiX, by updating the required files in this repository.

Vibration is controlled in two files: usr_1_des.f, where the virtual .stl plate is vibrated vertically, and calc_collision_wall_mod.f, where the tangential velocity is applied onto particles contacting the wall (line 340). 

The magnitude of active matter force is controlled in the file output_manager.f in lines 224-226. 

