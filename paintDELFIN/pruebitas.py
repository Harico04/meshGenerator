file=open('./premadeShapes/dolphinPoints.txt','r')

xComps=[]
yComps=[]
line=file.readline().strip()
while line:
    parts= line.split()
    x,y=parts
    xComps.append(float(x))
    yComps.append(float(y))
    line=file.readline().strip()

file.close()

print(f'{max(xComps)},{max(yComps)}\n')
print(f'{min(xComps)},{min(yComps)}')