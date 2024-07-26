#!/bin/bash

pwd  

cd ../
pwd  
mv ./paintDELFIN/points.geo ./gmsh/

# Executing .geo with gmsh
cd gmsh
gmsh points.geo -2 -format vtk -o Gmsh.vtk
cd ..

# Gmesh transfer .vtk to Mesh-5.0;10u
cp ./gmsh/Gmsh.vtk ./Mesh-5.0/input/

# Executing Mesh
cd ./Mesh-5.0/input 
./mesh
cd ../../ # back to the root of the project.

# Mesh-5.0 transfer data.txt to SUFVM_2024/
mv ./Mesh-5.0/input/data.txt ./Mesh-5.0/input/DATA.txt
cp ./Mesh-5.0/input/DATA.txt ./RTSUFVM/input/Example_Cylinder/

# Executing sufvm in order to generate .vtk for ParaView
cd ./RTSUFVM/input
./RTUFVM.cgi 

