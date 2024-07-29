#!/bin/bash

pidfile="/tmp/programa_largo_cpp.pid"

# Verificar si el archivo PID existe y contiene un PID válido
if [ -f "$pidfile" ]; then
    old_pid=$(cat "$pidfile")
    if kill -0 "$old_pid" > /dev/null 2>&1; then
        echo "Deteniendo la ejecución anterior del programa con PID $old_pid..."
        kill -SIGTERM "$old_pid"
        sleep 0.01  # Esperar un poco para asegurarse de que el proceso se detiene
        if kill -0 "$old_pid" > /dev/null 2>&1; then
            echo "No se pudo detener el proceso $old_pid. Saliendo."
            exit 1
        fi
        echo "Ejecución anterior detenida."
    fi
fi

# Guardar el PID actual en el archivo PID
echo $$ > "$pidfile"

cd ../
cp ./paintDELFIN/points.geo ./gmsh/

# Ejecutar .geo con gmsh
cd gmsh
gmsh points.geo -2 -format vtk -o Gmsh.vtk
cd ..

# Transferir el archivo .vtk a Mesh-5.0
cp ./gmsh/Gmsh.vtk ./Mesh-5.0/input/

# Ejecutar Mesh
cd ./Mesh-5.0/input 
./mesh
cd ../../ # volver a la raíz del proyecto

# Transferir data.txt a SUFVM_2024/
mv ./Mesh-5.0/input/data.txt ./Mesh-5.0/input/DATA.txt
cp ./Mesh-5.0/input/DATA.txt ./RTSUFVM/input/Example_Cylinder/

# Ejecutar sufvm para generar .vtk para ParaView
cd ./RTSUFVM/input
./RTUFVM.cgi &
cpp_pid=$!

# Guardar el PID del programa en segundo plano en el archivo PID
echo $cpp_pid > "$pidfile"

# Esperar a que el programa en segundo plano termine
wait $cpp_pid

# Eliminar el archivo PID al terminar
rm "$pidfile"

echo "Ejecución del programa completada."
