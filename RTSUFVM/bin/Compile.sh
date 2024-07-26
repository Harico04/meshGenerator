
#
# Link the modules with the main program.
#
mpifort RTUFVM.cpp uvfm.o utility.o fluid.o  modules.o -lmpi -lstdc++
if [ $? -ne 0 ]; then
  echo "Errors linking objects"
  exit
fi

#
# Rename the program.
#
mv a.out RTUFVM.cgi
mv RTUFVM.cgi ../input/RTUFVM.cgi

#
# Terminate.
#
exit
