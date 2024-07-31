import paraview.util
from paraview.simple import *
import os

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

# Show the data in the view
display = Show(reader, renderView)

# Change the variable to visualize (Color By) to 'vector_U'
ColorBy(display, ('POINTS', 'vector_U'))  # Replace 'vector_U' with the exact name of the velocity variable

# Update the color legend
display.RescaleTransferFunctionToDataRange(True, False)
display.SetScalarBarVisibility(renderView, False)

colorTransferFunction = GetColorTransferFunction('vector_U')
colorTransferFunction.ApplyPreset('Blue Orange (divergent)', True)
display.RescaleTransferFunctionToDataRange(True, False)

# Render the view to apply changes
Render()

# Configure the rendered view for full screen
renderView.Background = [0.0, 0.0, 0.0]
renderView.ViewSize = [1920, 1080]
renderView.OrientationAxesVisibility = 0  # Hide the orientation axis
renderView.CenterAxesVisibility = 0  # Hide the center axis

# Configure the camera for zooming
camera = GetActiveCamera()
camera.SetPosition(2, 1, 10)  # Adjust the camera position, centered on the domain
camera.SetFocalPoint(2, 1, 0)  # Adjust the camera focal point, centered on the domain
camera.SetViewUp(0, 1, 0)  # Adjust the camera's "up" direction
camera.Zoom(2.3)  # Apply zoom (adjust as needed)

RenderAllViews()

# Add streamlines
streamTracer = StreamTracer(Input=reader,
                            SeedType='Line')
streamTracer.SeedType.Point1 = [0, 0, 0]
streamTracer.SeedType.Point2 = [4, 4, 0]
streamTracer.Vectors = ['POINTS', 'vector_U']
streamTracer.MaximumStreamlineLength = 10.0

# Show the streamlines
streamTracerDisplay = Show(streamTracer, renderView)
ColorBy(streamTracerDisplay, ('POINTS', 'vector_U'))
streamTracerDisplay.RescaleTransferFunctionToDataRange(True, False)
streamTracerDisplay.SetScalarBarVisibility(renderView, False)

# Apply the same color preset to the streamlines
streamTracerColorTransferFunction = GetColorTransferFunction('vector_U')
streamTracerColorTransferFunction.ApplyPreset('Blue Orange (divergent)', True)
streamTracerDisplay.RescaleTransferFunctionToDataRange(True, False)

# Render the view to apply changes
Render()

# Configure the animation
scene = GetAnimationScene()
scene.Loop = True
scene.NumberOfFrames = 20
scene.Play()
