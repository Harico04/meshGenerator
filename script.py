import paraview.util
from paraview.simple import *
import os
import time

# Define the root directory and the file pattern
root_dir = "../RTSUFVM/output/Paraview/Serial"
pattern = os.path.join(root_dir, "H*.vtk")

# Find the files matching the pattern
files = paraview.util.Glob(path=pattern)

# Open the files in ParaView
reader = OpenDataFile(files)

# Show the data and render the view
Show()
Render()

# Set the active view
renderView = GetActiveViewOrCreate('RenderView')

# Mostrar los datos en la vista
display = Show(reader, renderView)

# Cambiar la variable a visualizar (Color By) a 'vector_U'
ColorBy(display, ('POINTS', 'vector_U'))  # Reemplaza 'vector_U' con el nombre exacto de la variable de velocidad

# Actualizar la leyenda de color
display.RescaleTransferFunctionToDataRange(True, False)
display.SetScalarBarVisibility(renderView, False)

colorTransferFunction = GetColorTransferFunction('vector_U')
colorTransferFunction.ApplyPreset('Blue Orange (divergent)', True)
display.RescaleTransferFunctionToDataRange(True, False)

# Renderizar la vista para aplicar los cambios
Render()

# Configurar la vista renderizada para pantalla completa
renderView.Background = [0.0, 0.0, 0.0]
renderView.ViewSize = [1920, 1080]
renderView.OrientationAxesVisibility = 0  # Ocultar el eje de orientación
renderView.CenterAxesVisibility = 0  # Ocultar el eje central

# Configurar la cámara para hacer zoom
camera = GetActiveCamera()
camera.SetPosition(2, 1, 10)  # Ajustar la posición de la cámara, centrada en el dominio
camera.SetFocalPoint(2, 1, 0)  # Ajustar el punto focal de la cámara, centrada en el dominio
camera.SetViewUp(0, 1, 0)  # Ajustar la dirección "arriba" de la cámara
camera.Zoom(2.3)  # Aplicar zoom (ajusta según sea necesario)

RenderAllViews()

# Configurar la animación
scene = GetAnimationScene()

while True:
    for i in range(20 - 1):
        scene.GoToNext()
        time.sleep(.01)
    scene.GoToFirst()
        
# scene.NumberOfFrames = 100  # More Frames implies more duration in the animation.
# scene.Loop = True
# scene.Play()
