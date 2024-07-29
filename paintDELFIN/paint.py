import tkinter as tk
from tkinter import *
import copy
import math
from PIL import Image, ImageTk
from enum import Enum
import threading
import subprocess
import os 

#Enum class for the brush types
class BrushType(Enum):
    FREEHAND=0
    SQUARE=1
    TRIANGLE=2
    CIRCLE=3
    DOLPHIN=4
#*******************************************************#
#Enum class for the loop types
class LoopType(Enum):
    FREEHAND=0
    SQUARE=1
    TRIANGLE=2
    CIRCLE=3
    DOLPHIN=4
#*******************************************************#

#Loop class. It defines a drawing in the canvas that properly loops. It really works more as a struct than a class,
#Since all of its parameters are "public", but Python does not support structs :p
class Loop:

    #__init__ methos is called when a Loop object is instanced. Creates two empty lists,
    #one for the points and one for the shapes corresponding to the loop. By default it
    #is a freehand type of loop, and does not intersect.
    def __init__(self):
        self.loopPoints=[]
        self.loopShapes=[]
        self.loopType=LoopType.FREEHAND
        self.intersects=False
        #Error margin between the last point of a loop and the first
        self.errorMargin = 12

    #This method calcualtes the distance between the last point in the loop and the
    # point that is currently being drawn 
    def distanceToLastPoint(self, x,y):
        return math.sqrt((self.loopPoints[-1][0]-x)**2 + (self.loopPoints[-1][1]-y)**2)

    #Checks if the loop properly loops, as in the last point is (close enough) to the first.
    def properlyLoops(self):
        if len(self.loopPoints)<10: return False
        xDiff=abs(self.loopPoints[0][0]-self.loopPoints[-1][0])
        yDiff=abs(self.loopPoints[0][1]-self.loopPoints[-1][1])
        return xDiff<=self.errorMargin and yDiff<=self.errorMargin
    
    #Completely clears the loop
    def clearLoop(self):
        self.loopPoints.clear()
        self.loopShapes.clear()
        self.intersects=False

#*******************************************************#

#GUI class. the window in which the user will
#do their shape(s).

class GUI:

    #init method is called when a GUI object is instanced. Creates all of the elements
    #in the window, from the canvas to the buttons. 
    def __init__(self):
        
        #Initial size of the window
        #Preferably the width and height should be at a proportion of Height=width/2, but it is not
        #mandatory(It just makes the translation from canvas to mesh feel a little weird)
        self.windowWidth,self.windowHeight= 1200,600

        #height of the button frame
        self.buttonFrameHeight=60

        #height of the canvas (should be height of the window minus height of the button frame)
        self.canvasHeight=self.windowHeight-self.buttonFrameHeight

        ####NOTE####
        #The width of both the button frame and the canvas is the same as 
        #the width of the window, which is why we do not specify on for them
        ####NOTE####

        #Brush type as free hand by default
        self.brushType=BrushType.FREEHAND
        #List of loops in the canvas
        self.loops=[]

        #We create a loop object to use for the loop that the user is making at any
        #given point
        self.currentLoop=Loop()
        #For the extra lines when drawing freehand
        self.extraLines=[]
        #Create the Tkinter window
        self.root= Tk()
        #title of the canvas windows
        self.root.title("paint") 
        #Set the width and height of the window
        self.root.geometry(f'{self.windowWidth}x{self.windowHeight}')
        #Disable window resizing
        self.root.resizable(False,False)
        #The width of the brush
        self.brush_width=5
        #The current color of the brush
        self.current_color = "black"

        #Export file when closing window
        self.root.protocol("WM_DELETE_WINDOW",self.onClosing)
        #Set up and configure the button frame
        self.btn_frame = Frame(self.root,bg='#8C003E')
        self.btn_frame.pack(fill=X)
        self.btn_frame.columnconfigure(0, weight =1)
        self.btn_frame.columnconfigure(1, weight =1)
        self.btn_frame.columnconfigure(2, weight =1)
        self.btn_frame.columnconfigure(3, weight =1)
        self.btn_frame.columnconfigure(4, weight =1)
        self.btn_frame.columnconfigure(5, weight =1)
        self.btn_frame.columnconfigure(6, weight =1)
        self.btn_frame.columnconfigure(7, weight =1)
        self.btn_frame.columnconfigure(8, weight =1)
        self.btn_frame.columnconfigure(9, weight =1)

        #Load the button icons
        sqrImg=Image.open('./buttonIcons/square.png')
        sqrImg=sqrImg.resize((50,50))
        self.squareImage=ImageTk.PhotoImage(sqrImg)

        triImg=Image.open('./buttonIcons/triangle.png')
        triImg=triImg.resize((self.buttonFrameHeight-10,self.buttonFrameHeight-10))
        self.triangleImage=ImageTk.PhotoImage(triImg)

        circImg=Image.open('./buttonIcons/circle.png')
        circImg=circImg.resize((self.buttonFrameHeight-10,self.buttonFrameHeight-10))
        self.circleImage=ImageTk.PhotoImage(circImg)

        runImg=Image.open('./buttonIcons/run.png')
        runImg=runImg.resize((self.buttonFrameHeight,self.buttonFrameHeight))
        self.runImage=ImageTk.PhotoImage(runImg)

        frHndImg=Image.open('./buttonIcons/freehand.png')
        frHndImg=frHndImg.resize((self.buttonFrameHeight-10,self.buttonFrameHeight-10))
        self.freehandImage=ImageTk.PhotoImage(frHndImg)

        clrImg=Image.open('./buttonIcons/clear.png')
        clrImg=clrImg.resize((self.buttonFrameHeight-10,self.buttonFrameHeight-10))
        self.clearImage=ImageTk.PhotoImage(clrImg)

        dlphImg=Image.open('./buttonIcons/dolphin.png')
        dlphImg=dlphImg.resize((self.buttonFrameHeight,self.buttonFrameHeight-10))
        self.dolphinImage=ImageTk.PhotoImage(dlphImg)

        ######NOTE######
        #"#000000" is hexadecimal format for colors, a list of all
        #possible colors and their hexadecimal representation can be
        #found here: https://htmlcolorcodes.com/
        ######NOTE######

        #Simulate button, to export the points and begin the simulation
        self.finish_button = Button(self.btn_frame,width=100,height=self.buttonFrameHeight, bg='#DC0062',image=self.runImage, command=self.exportPoints)
        self.finish_button.grid(row=0,column=9,sticky=E)

        #Clear button. To clean the canvas
        self.clear_button = Button(self.btn_frame, width=60,height=self.buttonFrameHeight,image=self.clearImage,bg='#DC0062', command=self.clearCanvas)
        self.clear_button.grid(row=0,column=8,sticky=W+E)

        #Freehand button, to draw in the canvas freely
        self.freehand_button = Button(self.btn_frame, text="trazo",width=80,height=self.buttonFrameHeight,bg='#3E001C',image=self.freehandImage,activebackground='#3E001C', command=self.drawFreeHand)
        self.freehand_button.grid(row=0,column=0,sticky=W+E)

        #Square button. To draw a premade square
        self.square_button = Button(self.btn_frame, width=80,height=self.buttonFrameHeight,bg='#DC0062',image=self.squareImage, command=self.drawSquare)
        self.square_button.grid(row=0,column=1,sticky=W+E)
        
        #Triangle button. To draw a premade triangle
        self.triangle_button = Button(self.btn_frame, width=80,height=self.buttonFrameHeight,bg='#DC0062',image=self.triangleImage, command=self.drawTriangle)
        self.triangle_button.grid(row=0,column=2,sticky=W+E)
        
        #Circle button. To draw a premade Circle
        self.circle_button = Button(self.btn_frame, width=80,height=self.buttonFrameHeight,bg='#DC0062',image=self.circleImage,command=self.drawCircle)
        self.circle_button.grid(row=0,column=3,sticky=W+E)
        
        # Create the scale (slider) and add it to the button_frame
        self.viscositySlider = Scale(self.btn_frame, from_=0.1, to=4.0,resolution=0.1, orient='horizontal', command=self.changeViscosity)
        self.viscositySlider.grid(row=0, column=6, sticky=W+E)
        self.viscositySlider.configure(bg='#DC0062',troughcolor='#3E001C')

        #Dolphin button. To draw a dolphin
        self.dolphin_button = Button(self.btn_frame, text="Delfin",width=80,height=self.buttonFrameHeight,image=self.dolphinImage,bg='#DC0062', command=self.drawDolphin)
        self.dolphin_button.grid(row=0,column=4,sticky=W+E)

         #Set up and configure the canvas,
        self.cnv = Canvas(self.root, width=self.windowWidth, height=self.canvasHeight, bg="white")#give the window a Canvas object
        self.cnv.pack()
        self.cnv.bind("<Button-1>",self.onClick)
        self.cnv.bind("<B1-Motion>", self.paintLoop)#make paint method have an impact on the Canvas object
        self.cnv.bind("<ButtonRelease-1>", self.checkDrawingLoop)

        #image for the background
        self.backgroundImage=Image.open("./background/cimat.png").convert('RGBA')
        self.backgroundImage=self.backgroundImage.resize((int((3*self.canvasHeight)/4),self.canvasHeight))
        #For transparency in the image background
        # Get the alpha channel
        alpha = self.backgroundImage.split()[3]
        # Modify the alpha channel to change transparency
        alpha = alpha.point(lambda p: p * 0.2)
        # Put the modified alpha channel back into the image
        self.backgroundImage.putalpha(alpha)
        #Setup background image
        self.background=ImageTk.PhotoImage(self.backgroundImage)
        #Save its ID to not cause problems in intersections with objects(the background is treated as
        #any other object in the canvas, so we need to check for it individually)
        self.bgID=self.cnv.create_image(0,0,anchor=tk.NW,image=self.background)
    
        #We read from the get-go the points that make the dolphin shape (Otherwise 
        #we would have to read them repeatedly throughout the main program loop
        self.dolphinX=[]
        self.dolphinY=[]
        #Save the max range on the dolphin in the x and y axis (multiplied by a factor of 100)
        self.maxDolphinX=237
        self.maxDolphinY=98

        f=open('./premadeShapes/dolphinPoints.txt','r')
        line=f.readline().strip()
        while line:
            parts= line.split()
            x,y=parts
            self.dolphinX.append(float(x)*100-10)#-10 because the points don't start at 0
            self.dolphinY.append(float(y)*100-51)#-51 because the points don't start at 0
            line=f.readline().strip()

        f.close()
        
        #Draw a rectangle that will work as the canvas' boundary
        self.cnv.create_rectangle(-1,-1,self.windowWidth+1,self.canvasHeight+1)

        #Start the program loop
        self.root.mainloop()
#*******************************************************#
    #This method creates a list of bounding boxes that is more representative
    #of a line, to better check for intersections
    def refinedBbox(self,line,refinement):
        #coords returns a list with the coordinates of the starting and ending points
        #of the line (x1,y1,x2,y2)
        coords=self.cnv.coords(line)
        xInterval=(coords[2]-coords[0])/refinement
        yInterval=(coords[3]-coords[1])/refinement
        currX,currY=coords[0],coords[1]
        #We will make a list of rectangles to simulate the bboxes 
        bboxes=[]
        for i in range(1,refinement+1):
            box=self.cnv.create_rectangle(currX,currY,currX+xInterval,currY+yInterval,width=1)
            bboxes.append(box)
            currX+=xInterval
            currY+=yInterval
        
        return bboxes
#*******************************************************#
    #Saves the position of the cursor when a click is made on the canvas
    def onClick(self,event):
        self.startingX=event.x
        self.startingY=event.y
        if self.brushType==BrushType.FREEHAND:
            self.currentLoop.loopPoints.append((self.startingX,self.startingY))
        print("Saved click position")
#*******************************************************#

    #This method is responsible for all the drawings in the canvas. It is called when the mouse
    #is being dragged, and checks the necesarry logic to see if a drawing properly loops, and if 
    #it intersects any other drawings. 
    def paintLoop(self, event):
        #save current position of the mouse
        self.currentX,self.currentY=event.x,event.y
        #Check which type of brush is currently selected
        match(self.brushType):
            case BrushType.FREEHAND:
                #We draw the line
                line=self.cnv.create_line(self.startingX,self.startingY,self.currentX,self.currentY, fill=self.current_color, width=self.brush_width)
                self.startingX,self.startingY=self.currentX,self.currentY

                #Save the current position of the mouse in loopPoints 
                if not self.currentLoop.properlyLoops():
                    if not self.currentLoop.loopPoints or self.currentLoop.distanceToLastPoint(event.x,event.y)>10:
                        self.currentLoop.loopPoints.append((event.x,event.y))
                    #Save the current loop's ID in loopShapes 
                    self.currentLoop.loopShapes.append(line)
                else:
                    #This means the loop already looped, so any more drawings can be ignored
                    self.current_color='red'
                    self.extraLines.append(line)
                    
                #Check if we didn't pass through an already painted object. If the 
                #shape already loops this is not neccesary
                if(not self.currentLoop.properlyLoops()):
                    #Get a refined bounding box of the last line made
                    bboxes=self.refinedBbox(self.currentLoop.loopShapes[-1],10)
                    for box in bboxes:
                        bbox=self.cnv.bbox(box)
                        objectTag=self.cnv.find_overlapping(*bbox)
                        if(objectTag):
                            for object in objectTag:
                                if object in bboxes or object in self.currentLoop.loopShapes[-6:] or object==self.bgID:
                                    continue
                                for shape in self.currentLoop.loopShapes:
                                    self.cnv.itemconfig(shape,fill='red')
                                self.current_color='red'
                                print('overlapped')
                                self.currentLoop.intersects=True    

                    for box in bboxes:
                        self.cnv.delete(box)

                
            
            case BrushType.SQUARE:
                
                #Check if we already had a square saved, and delete it if so
                if(self.currentLoop.loopShapes):
                    self.cnv.delete(self.currentLoop.loopShapes[0])
                    self.currentLoop.clearLoop()

                #draw the rectangle in the canvas
                rec=self.cnv.create_rectangle(self.startingX,self.startingY,self.currentX,self.currentY,width=self.brush_width,outline=self.current_color)

                #Add the rectangle to the current loop
                self.currentLoop.loopShapes.append(rec)

                #Check if the rectangle overlaps, for this we create the lines of the rectangle
                #as an object and see if any of them overlap
                l1=self.cnv.create_line(self.startingX,self.startingY,self.startingX,self.currentY,width=self.brush_width)
                l2=self.cnv.create_line(self.startingX,self.currentY,self.currentX,self.currentY,width=self.brush_width)
                l3=self.cnv.create_line(self.currentX,self.currentY,self.currentX,self.startingY,width=self.brush_width)
                l4=self.cnv.create_line(self.currentX,self.startingY,self.startingX,self.startingY,width=self.brush_width)

                #Save all the auxiliary lines in a list
                recLines=[]
                recLines.append(l1)
                recLines.append(l2)
                recLines.append(l3)
                recLines.append(l4)
                
                #check if any of the lines are intersecting a loop
                for line in recLines:
                    #get bounding box of the current line
                    bbox=self.cnv.bbox(line)
                    #Returns s list of all the objects that overlap with the current line
                    objectTag=self.cnv.find_overlapping(*bbox)
                    #"If the list is not empty:"
                    if(objectTag):
                        for object in objectTag:
                            #Check that the overlapping object is not the other lines or the rectangle
                            if object in recLines or object==self.currentLoop.loopShapes[0] or object==self.bgID:
                                continue
                            self.cnv.itemconfig(self.currentLoop.loopShapes[0],outline='red')
                            print('overlapped')
                            self.currentLoop.intersects=True
                
                print(self.currentLoop.intersects)
                self.current_color='black'
                #Delete the auxiliary lines from the canvas
                for line in recLines:
                    self.cnv.delete(line)

            case BrushType.TRIANGLE:
                #Check if we already had a triangle saved, and delete it if so
                if(self.currentLoop.loopShapes):
                    for line in self.currentLoop.loopShapes:
                        self.cnv.delete(line)
                    self.currentLoop.clearLoop()

                #Calculate middle vertex height (the x component is the same as startingX)
                midVertexY=(self.startingY+self.currentY)/2.0

                #create list of lines representative of the triangle; we do it this way to make it 
                #easier to detect intersections
                self.currentLoop.loopShapes.append(self.cnv.create_line(self.currentX,self.startingY,self.startingX,midVertexY,width=self.brush_width))
                self.currentLoop.loopShapes.append(self.cnv.create_line(self.startingX,midVertexY,self.currentX,self.currentY,width=self.brush_width))
                self.currentLoop.loopShapes.append(self.cnv.create_line(self.currentX,self.currentY,self.currentX,self.startingY,width=self.brush_width))

                for line in self.currentLoop.loopShapes:
                    refBbox=self.refinedBbox(line,80)
                    for box in refBbox:
                        bbox=self.cnv.bbox(box)
                        objectTag=self.cnv.find_overlapping(*bbox)
                        if(objectTag):
                            for object in objectTag:
                                if object in self.currentLoop.loopShapes or object in refBbox or object==self.bgID:
                                    continue
                                for line in self.currentLoop.loopShapes:
                                    self.cnv.itemconfig(line,fill='red')
                                print('overlapped')
                                self.currentLoop.intersects=True
                                break
                    
                    for box in refBbox:
                        self.cnv.delete(box)
                            
            case BrushType.CIRCLE:

                #Check if we already had a circle saved, and delete it if so
                if(self.currentLoop.loopShapes):
                    self.cnv.delete(self.currentLoop.loopShapes[0])
                    self.currentLoop.clearLoop()
                
                #Create and save the circle
                circ=self.cnv.create_oval(self.startingX,self.startingY,self.currentX,self.currentY,width=self.brush_width)
                self.currentLoop.loopShapes.append(circ)

                #Create list of auxiliary lines
                circLines=[]

                #create the lines
                #Calculate center of the ellipse
                centerX=(self.currentX+self.startingX)/2.0
                centerY=(self.currentY+self.startingY)/2.0
                #Calculate semi-axis sizes of the ellipse
                a=(abs(self.currentX-self.startingX))/2.0
                b=(abs(self.currentY-self.startingY))/2.0

                #Segment partition
                theta=(2*math.pi)/25
                #Create points
                currX=centerX+a
                currY=centerY
                for i in range (1,26):
                   circLines.append(self.cnv.create_line(currX,currY,centerX+a*math.cos(i*theta),centerY+b*math.sin(i*theta)))
                   currX=centerX+a*math.cos(i*theta)
                   currY=centerY+b*math.sin(i*theta)
                
                for line in circLines:
                    refBbox=self.refinedBbox(line,10)
                    for box in refBbox:
                        bbox=self.cnv.bbox(box)
                        objectTag=self.cnv.find_overlapping(*bbox)
                        if(objectTag):
                            for object in objectTag:
                                if object in circLines or object in refBbox or object==self.currentLoop.loopShapes[0] or object==self.bgID:
                                    continue
                                self.cnv.itemconfig(self.currentLoop.loopShapes[0],outline='red')
                                print('overlapped')
                                self.currentLoop.intersects=True
                                break
                    
                    for box in refBbox:
                        self.cnv.delete(box)
                    
                for line in circLines:
                    self.cnv.delete(line)

            case BrushType.DOLPHIN:

                #Check if we already had a dolphin saved, and delete it if so
                if(self.currentLoop.loopShapes):
                    for line in self.currentLoop.loopShapes:
                        self.cnv.delete(line)
                    self.currentLoop.clearLoop()

                #generate dolphin based on starting click and current X and Y distance between
                #the cursor and the starting point
                for i in range(len(self.dolphinX) - 1):  # arrays are of the same size
                    self.currentLoop.loopShapes.append(
                        self.cnv.create_line(
                            #This whole block is the line creation, it's just that getting the points results
                            #in huge sentences and putting it all in the same line would look terrible
                            self.startingX + self.dolphinX[i] * ((self.currentX - self.startingX) / 237.0),  # first point
                            self.startingY + self.dolphinY[i] * ((self.currentY - self.startingY) / 98.0),  # second point
                            self.startingX + self.dolphinX[i + 1] * ((self.currentX - self.startingX) / 237.0),  # third point
                            self.startingY + self.dolphinY[i + 1] * ((self.currentY - self.startingY) / 98.0),  # fourth point
                            width=self.brush_width
                        )
                    )

                #create closing line
                self.currentLoop.loopShapes.append(
                    self.cnv.create_line(
                        self.startingX + self.dolphinX[-1] * ((self.currentX - self.startingX) / 237.0),  # first point
                        self.startingY + self.dolphinY[-1] * ((self.currentY - self.startingY) / 98.0),  # second point
                        self.startingX + self.dolphinX[0] * ((self.currentX - self.startingX) / 237.0),  # third point
                        self.startingY + self.dolphinY[0] * ((self.currentY - self.startingY) / 98.0),   # fourth point
                        width=self.brush_width
                    )
                )

                #check for intersections
                for line in self.currentLoop.loopShapes:
                    refBbox=self.refinedBbox(line,10)
                    for box in refBbox:
                        bbox=self.cnv.bbox(box)
                        objectTag=self.cnv.find_overlapping(*bbox)
                        if(objectTag):
                            for object in objectTag:
                                if (object in self.currentLoop.loopShapes or object in refBbox  or object==self.bgID):
                                    continue
                                for line in self.currentLoop.loopShapes:
                                    self.cnv.itemconfig(line,fill='red')
                                print('overlapped')
                                self.currentLoop.intersects=True
                                break
                    
                    for box in refBbox:
                        self.cnv.delete(box)

        print(f"{event.x},{event.y}")
#*******************************************************#

    #This method is called when the mouse is not being pressed anymore. Checks for intersections
    #and proper loops, and saves them to the list of loops if they are proper.
    def checkDrawingLoop(self,event):
        #Base the checking off of the brush that was used for the drawing
        match(self.brushType):
            case BrushType.FREEHAND:
                if self.currentLoop.properlyLoops() and (not self.currentLoop.intersects):
                    self.cnv.create_line(self.currentLoop.loopPoints[0][0],self.currentLoop.loopPoints[0][1],self.currentLoop.loopPoints[-1][0],self.currentLoop.loopPoints[-1][1],width=self.brush_width)
                    self.currentLoop.loopType=LoopType.FREEHAND
                    self.loops.append(copy.deepcopy(self.currentLoop))
                else:
                    for shape in self.currentLoop.loopShapes:
                        self.cnv.delete(shape)
                
                for line in self.extraLines:
                    self.cnv.delete(line)
                
                self.extraLines.clear()
            
            case BrushType.SQUARE:
                print(self.currentLoop.intersects)
                if(not self.currentLoop.intersects):
                    self.currentLoop.loopType=LoopType.SQUARE

                    self.currentLoop.loopPoints.append((self.startingX,self.startingY))
                    self.currentLoop.loopPoints.append((self.startingX,self.currentY))
                    self.currentLoop.loopPoints.append((self.currentX,self.currentY))
                    self.currentLoop.loopPoints.append((self.currentX,self.startingY))

                    self.loops.append(copy.deepcopy(self.currentLoop))
                else:
                    for shape in self.currentLoop.loopShapes:
                        self.cnv.delete(shape)

            case BrushType.TRIANGLE:
                if not self.currentLoop.intersects:
                    self.currentLoop.loopType=LoopType.TRIANGLE

                    #Calculate height of the vertex
                    midVertexY=(self.startingY+self.currentY)/2.0

                    #Save the points
                    self.currentLoop.loopPoints.append((self.currentX,self.startingY))
                    self.currentLoop.loopPoints.append((self.startingX,midVertexY))
                    self.currentLoop.loopPoints.append((self.currentX,self.currentY))

                    self.loops.append(copy.deepcopy(self.currentLoop))

                else:
                    for shape in self.currentLoop.loopShapes:
                        self.cnv.delete(shape)

            case BrushType.CIRCLE:
                if not self.currentLoop.intersects:
                    self.currentLoop.loopType=LoopType.CIRCLE

                    #Calculate center of the ellipse
                    centerX=(self.currentX+self.startingX)/2.0
                    centerY=(self.currentY+self.startingY)/2.0
                    #Calculate semi-axis sizes of the ellipse
                    a=(abs(self.currentX-self.startingX))/2.0
                    b=(abs(self.currentY-self.startingY))/2.0

                    #Segment partition
                    theta=(2*math.pi)/25
                    #Create points
                    for i in range (25):
                        self.currentLoop.loopPoints.append((centerX+a*math.cos(i*theta),centerY+b*math.sin(i*theta)))

                    self.loops.append(copy.deepcopy(self.currentLoop))

                else:
                    for shape in self.currentLoop.loopShapes:
                        self.cnv.delete(shape)

            case BrushType.DOLPHIN:
                if not self.currentLoop.intersects:
                    self.currentLoop.loopType=LoopType.DOLPHIN
                    #save the points to the current loop
                    for i in range(len(self.dolphinX)):  # arrays are of the same size
                        self.currentLoop.loopPoints.append((
                            self.startingX + self.dolphinX[i] * ((self.currentX - self.startingX) / 237.0),  # first point
                            self.startingY + self.dolphinY[i] * ((self.currentY - self.startingY) / 98.0)  # second point
                        ))
                    #save the loop to the loops list
                    self.loops.append(copy.deepcopy(self.currentLoop))
                    
                else:
                    for shape in self.currentLoop.loopShapes:
                        self.cnv.delete(shape)
        
        #Clear currentLoop to reuse it for the next drawing, and set the 
        #brush color to black in case we had an unproper loop
        self.currentLoop.clearLoop()
        self.current_color='black'
#*******************************************************#

    #This method creates the .geo file that is to be proccessed through Gmsh.
    def exportPoints(self):
        file=open('points.geo','w')
        #Write the parameters to the file
        file.write('tm = 0.1;\n')
        file.write('tmr = 0.05;\n')
        file.write('tmr2 = 0.025;\n')

        #Write the points of the external domain, agreed to be 4x2
        file.write('Point(1) = {0.0, 0.0, 0.0, tm};\n');
        file.write('Point(2) = {4.0, 0.0, 0.0, tm};\n');
        file.write('Point(3) = {4.0, 2.0, 0.0, tm};\n');
        file.write('Point(4) = {0.0, 2.0, 0.0, tm};\n');

        #Write the points of the loops to the file
        pointID=5 #Iterator for the points
        for loop in self.loops:
            if(loop.loopType==LoopType.SQUARE or loop.loopType==LoopType.TRIANGLE):
                tmr='tmr2'
            else:
                tmr='tmr'

            for point in loop.loopPoints:
                #The division is the scaling factor for the points, remember that the canvas
                #works with integers from 0 to its height/width
                file.write(f"Point({pointID}) = {{{0.5+(point[0]/(self.windowWidth/2))},{2-(0.5+point[1]/self.canvasHeight)},0.0,{tmr}}};\n")
                pointID+=1

        #Write the lines of the external domain
        file.write('Line(1) = {1, 2};\n')
        file.write('Line(2) = {2, 3};\n')
        file.write('Line(3) = {3, 4};\n')
        file.write('Line(4) = {4, 1};\n')

        #Write the lines of each loop to the file
        lineID=5
        for loop in self.loops:
            lineStart=lineID
            for point in loop.loopPoints[:-1]:
                file.write(f"Line({lineID}) = {{{lineID},{lineID+1}}};\n")
                lineID+=1

            file.write(f"Line({lineID}) = {{{lineID},{lineStart}}};\n")
            lineID+=1

        #Make computational domain of the external domain
        file.write("Curve Loop(1) = {1,2,3,4};\n")

        #Make computational domain for each loop
        curveLoopTotalLines=''
        curveLoopTotal=2
        currentLine=5
        for loop in self.loops:
            for point in loop.loopPoints:
                curveLoopTotalLines+=f"{currentLine}, "
                currentLine+=1
            
            curveLoopTotalLines=curveLoopTotalLines[:-2]
            file.write(f'Curve Loop({curveLoopTotal}) = {{{curveLoopTotalLines}}};\n')
            curveLoopTotal+=1
            curveLoopTotalLines=''

        #Write the plane surface
        totalLoops='1, '
        for i in range(len(self.loops)):
            totalLoops+=f"{i+2}, "
        
        totalLoops=totalLoops[:-2]
        file.write(f'Plane Surface(1) = {{{totalLoops}}};\n')

        #Apply tags to the objects
        file.write('Physical Line(\'Entrada\') = {4};\n')
        file.write('Physical Line(\'Salida\') = {2};\n')
        file.write('Physical Line(\'ParedArriba\') = {3};\n')
        file.write('Physical Line(\'ParedAbajo\') = {1};\n')
        
        #Tag the interior
        file.write("Physical Surface('Interior') = {1};\n")

        #Generate the mesh
        file.write('Mesh 2;\n')
        file.write('Mesh.SurfaceFaces = 1;\n')
        file.write('Mesh.Points= 1;\n')
        file.write('Save \"Mesh.vtk\";')

        file.close()
        #Start the script to calculate *.vtk
        threading.Thread(target=lambda: subprocess.run(["bash", "../script.sh"])).start()
#*******************************************************#

#This series of methods correspond to the brush type buttons, each is called when
#their corresponding button is pressed

    def drawFreeHand(self):
        self.brushType=BrushType.FREEHAND

        #Change the button's colors to make the "selected" effect
        self.freehand_button.config(bg="#3E001C",activebackground="#3E001C")
        self.square_button.config(bg="#DC0062",activebackground="white")
        self.triangle_button.config(bg="#DC0062",activebackground="white")
        self.circle_button.config(bg="#DC0062",activebackground="white")
        self.dolphin_button.config(bg="#DC0062",activebackground="white")

#*******************************************************#

    def drawSquare(self):
        self.brushType=BrushType.SQUARE

        #Change the button's colors to make the "selected" effect
        self.freehand_button.config(bg="#DC0062",activebackground="white")
        self.square_button.config(bg="#3E001C",activebackground="#3E001C")
        self.triangle_button.config(bg="#DC0062",activebackground="white")
        self.circle_button.config(bg="#DC0062",activebackground="white")
        self.dolphin_button.config(bg="#DC0062",activebackground="white")
#*******************************************************#

    def drawTriangle(self):
        self.brushType=BrushType.TRIANGLE

        #Change the button's colors to make the "selected" effect
        self.freehand_button.config(bg="#DC0062",activebackground="white")
        self.square_button.config(bg="#DC0062",activebackground="white")
        self.triangle_button.config(bg="#3E001C",activebackground="#3E001C")
        self.circle_button.config(bg="#DC0062",activebackground="white")
        self.dolphin_button.config(bg="#DC0062",activebackground="white")
#*******************************************************#

    def drawCircle(self):
        self.brushType=BrushType.CIRCLE

        #Change the button's colors to make the "selected" effect
        self.freehand_button.config(bg="#DC0062",activebackground="white")
        self.square_button.config(bg="#DC0062",activebackground="white")
        self.triangle_button.config(bg="#DC0062",activebackground="white")
        self.circle_button.config(bg="#3E001C",activebackground="#3E001C")
        self.dolphin_button.config(bg="#DC0062",activebackground="white")
#*******************************************************#

    def drawDolphin(self):
        self.brushType=BrushType.DOLPHIN

        #Change the button's colors to make the "selected" effect
        self.freehand_button.config(bg="#DC0062",activebackground="white")
        self.square_button.config(bg="#DC0062",activebackground="white")
        self.triangle_button.config(bg="#DC0062",activebackground="white")
        self.circle_button.config(bg="#DC0062",activebackground="white")
        self.dolphin_button.config(bg="#3E001C",activebackground="#3E001C")
#*******************************************************#
    #This method completely clears the canvas. Called when the clear button is pressed.
    def clearCanvas(self):
        self.cnv.delete("all")
        self.loops.clear()
        self.currentLoop.clearLoop()
        self.bgID=self.cnv.create_image(0,0,anchor=tk.NW,image=self.background)
        #redraw canvas boundary
        self.cnv.create_rectangle(-1,-1,self.windowWidth+1,self.canvasHeight+1)
#*******************************************************#
    #This method is called when the window is closing. Exports whatever was
    #on the canvas at the moment
    def onClosing(self):
        self.exportPoints()
        self.root.destroy()
#*******************************************************#
    #This method changes the viscosity value of the fluid
    def changeViscosity(self,value):
        f=open('viscosity.txt','w')
        f.write(f'Viscosity: {value}')
        f.close()
#*******************************************************#

#Run the program
threading.Thread(target=lambda: subprocess.run(["../ParaView/bin/pvpython", "../script.py"])).start()
GUI()
    
    
        
