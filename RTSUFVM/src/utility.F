!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!---------------------------------------------------------------------!
!                        ***  SUBROUTINES  ****                       !
!---------------------------------------------------------------------!
!                                                                     !
!     SUBROUTINE GRADF                                                !
!     SUBROUTINE INTERPO_WL                                           !
!     SUBROUTINE INTERPO_Q                                            !
!     SUBROUTINE OUTSAV                                               !
!                                                                     !
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                          SUBROUTINE: GRADF                          !
!          Calculation of the gradient by Least Square Method         !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE GRADF(NBE,NO_CP,DXC,DYC,FC,DXC2,DYC2,DXY,FX0,FY0)

!     ____________________________________
!    |                                    |
!    |     Keys and common parameters     |
!    |____________________________________|

#     include "cppdefs.h"
      IMPLICIT NONE
      INCLUDE 'common.mpf'
!     ____________________________________
!     Declaration of variables
      REAL*8, DIMENSION(:)   :: FC(N_CELL)
      REAL*8, DIMENSION(:)   :: FX0(N_CELL),FY0(N_CELL)
      INTEGER,DIMENSION(:)   :: NBE(N_CELL)
      INTEGER,DIMENSION(:,:) :: NO_CP(N_CELL,3)
      REAL*8, DIMENSION(:,:) :: DXC(N_CELL,3),DYC(N_CELL,3)
      REAL*8, DIMENSION(:)   :: DXC2(N_CELL),DYC2(N_CELL)
      REAL*8, DIMENSION(:)   :: DXY(N_CELL)
!     ____________________________________
!     Declaration of local variables
      REAL*8  :: SUMF_X,SUMF_Y,DETER
      INTEGER :: K,L,elem
!     ____________________________________
!    |                                    |
!    |        Gradient calculation        |
!    |____________________________________|

      DO NC=1,N_CELL0
!        _________________________________
!        INTERNAL CELLS L.S.M.
         SUMF_X=0.
         SUMF_Y=0.
         DO K=1,3
            L=NO_CP(NC,K)
            SUMF_X=SUMF_X+DXC(NC,K)*(FC(L)-FC(NC))
            SUMF_Y=SUMF_Y+DYC(NC,K)*(FC(L)-FC(NC))
         ENDDO
         DETER=(DXC2(NC)*DYC2(NC)-DXY(NC)*DXY(NC))
         FX0(NC)=(DYC2(NC)*SUMF_X-DXY(NC)*SUMF_Y)/DETER
         FY0(NC)=(DXC2(NC)*SUMF_Y-DXY(NC)*SUMF_X)/DETER
!        _________________________________
!        BOUNDARY CONDITIONS (Ghost elements)
         IF (NBE(NC).GE.1) THEN
            DO K=1,3
               MK=NO_CP(NC,K)
               IF (MK.GT.N_CELL0) THEN
                   FX0(MK)=FX0(NC)
                   FY0(MK)=FY0(NC)
               ENDIF
            ENDDO
         ENDIF
      ENDDO

      RETURN
      END

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                       SUBROUTINE: INTERPO_WL                        !
!          Interpolation of WL from cell-center to vertex points      !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE INTERPO_WL(FC,FV,NBE_V)

!     ____________________________________
!    |                                    |
!    |     Keys and common parameters     |
!    |____________________________________|

#     include "cppdefs.h"
      USE geometry
      IMPLICIT NONE
      INCLUDE 'common.mpf'
!     ____________________________________
!     Declaration of variables
      REAL*8,DIMENSION (:) :: FC(N_CELL),FV(N_VERT)
      INTEGER,DIMENSION(:) :: NBE_V(N_VERT)
      INTEGER,DIMENSION(:) :: TagInter(N_VERT)
      INTEGER:: j
!     ____________________________________
!    |                                    |
!    |           INTERPOLATION            |
!    |____________________________________|

!     _____________________________________
!     TAGS
      DO NV=1,N_VERT
          TagInter(NV) = 1
          !---> TURN OFF at water level boundaries
          IF (NBE_V(NV).EQ.3) TagInter(NV) = 0
      ENDDO
!     _____________________________________
!     INTERPOLATION
      do nv=1,N_VERT
         if (TagInter(nv).EQ.1) then
            FV(nv) = 0.0d0
            do j=1,Dimsurrounding(nv)
               nc = surrounding(nv,j)
               FV(nv)= FV(nv) + weight(nv,j)*FC(nc)
            enddo
         endif
      enddo

      RETURN
      END

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                       SUBROUTINE: INTERPO_Q                         !
!          Interpolation of Q from cell-center to vertex points       !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE INTERPO_Q(FC,FV,NBE_V,tagQ)

!     ____________________________________
!    |                                    |
!    |     Keys and common parameters     |
!    |____________________________________|

#     include "cppdefs.h"
      USE geometry
      IMPLICIT NONE
      INCLUDE 'common.mpf'
!     ____________________________________
!     Declaration of variables
      REAL*8,DIMENSION(:) :: FC(N_CELL),FV(N_VERT)
      INTEGER,DIMENSION(:):: NBE_V(N_VERT)
      INTEGER:: j,tagQ
      INTEGER,DIMENSION (:) :: TagInter(N_VERT)
!     ____________________________________
!    |                                    |
!    |           INTERPOLATION            |
!    |____________________________________|
!     _____________________________________
!     TAGS 
      DO NV=1,N_VERT
          TagInter(NV) = 1
          !---> TURN OFF discharge boundaries
          IF (NBE_V(NV).EQ.2) TagInter(NV) = 0
          !---> TURN OFF wall for Non-slip
#         ifdef KeyNoSlip
          IF (NBE_V(NV).EQ.1) TagInter(NV) = 0
#         endif
          !---> TURN OFF wall for Slip QY
#         ifdef KeySlip
          IF ((NBE_V(NV).EQ.1).AND.(tagQ.eq.2)) TagInter(NV) = 0
#         endif
          !---> TURN OFF structure boundaries
          IF (NBE_V(NV).EQ.6) TagInter(NV) = 0
      ENDDO
!     _____________________________________
!     INTERPOLATION
      DO nv=1,N_VERT
         if (TagInter(nv).EQ.1) then
            FV(NV) = 0.0d0
            do j=1,Dimsurrounding(nv)
               nc = surrounding(nv,j)
               FV(nv)= FV(nv) + weight(nv,j)*FC(nc)
            enddo
         endif
      ENDDO

      RETURN
      END

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                       SUBROUTINE: OUTSAV                            !
!                  Write data to display at Paraview                  !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE OUTSAV(WLVN,HWV,UVERT,VVERT,
     *                  X,Y,NBE_V,NO_VP,NumPrint)

!     ____________________________________
!    |                                    |
!    |     Keys and common parameters     |
!    |____________________________________|

#     include "cppdefs.h"
      IMPLICIT NONE
      INCLUDE 'common.mpf'
!     ____________________________________
!     Declaration of variables
      REAL*8,DIMENSION(:)   :: WLVN(N_VERT),HWV(N_VERT)
      REAL*8,DIMENSION(:)   :: UVERT(N_VERT),VVERT(N_VERT)
      REAL*8,DIMENSION(:)   :: X(N_VERT),Y(N_VERT)
      INTEGER,DIMENSION(:)  :: NBE_V(N_VERT)
      INTEGER,DIMENSION(:,:):: NO_VP(N_CELL,3)
      INTEGER :: NumPrint
!     ____________________________________
!     Declaration of local variables
      INTEGER:: irec,write_WLVN,write_HWV,write_nbev, write_Uvec
      REAL*8,DIMENSION(:) :: nbev(N_VERT)
      REAL*8, PARAMETER   :: epsilon = 1.0d-15
      CHARACTER*40 filen
!     ____________________________________
!    |                                    |
!    |                WRITE               |
!    |____________________________________|

      write_WLVN = 1
      write_HWV  = 0
      write_nbev = 0
      write_Uvec = 1
      
      do nv=1,N_VERT
         nbev(nv) = nbe_v(nv)*1.d0
      enddo

991   format(1(1x,e12.5))
992   format(2(2x,e12.5))
993   format(3(1x,e12.5))
994   format(4(1x,i10))
995   format(i10)
100   format('# vtk DataFile Version 2.0')
110   format('Unstructured Grid UFVM-ECOMOD')
120   format('ASCII')
130   format('DATASET UNSTRUCTURED_GRID')
 
!     __________________________________
!     Open file
      filen='../output/Paraview/Serial/H0000.vtk'
      write(filen(28:31),'(i4.4)') NumPrint
      irec = 600 
      OPEN(irec,FILE=filen)
      write(irec,100)
      write(irec,110)
      write(irec,120)
      write(irec,130)
!     __________________________________
!     Write points
      write(irec,*) 'POINTS ',N_VERT,' float'
      do NV=1,N_VERT
         write(irec,993) X(NV),Y(NV),0.0d0
      enddo
!     __________________________________
!     Write cells
      write(irec,*) ' '
      write(irec,*) 'CELLS ',N_CELL0,(4*N_CELL0)
      do NC=1,N_CELL0
         write(irec,994) 3,NO_VP(NC,1)-1,NO_VP(NC,2)-1,NO_VP(NC,3)-1
      enddo
!     __________________________________
!     CELL types (triangles)
      write(irec,*) ' '
      write(irec,*) 'CELL_TYPES ',N_CELL0
      do NC=1,N_CELL0
         write(irec,995) 5
      enddo
!     __________________________________
!     POINT DATA
      write(irec,*) ' '
      write(irec,*) 'POINT_DATA ',N_VERT
!     __________________________________
!     Write scalars
      IF (write_WLVN.eq.1) THEN
      write(irec,*) ' '
      write(irec,*) 'SCALARS WL      float 1'
      write(irec,*) 'LOOKUP_TABLE default'
      do NV=1,N_VERT
         if (abs(WLVN(NV)).le.epsilon) WLVN(NV)=0.0d0
         write(irec,991) WLVN(NV)
      enddo
      ENDIF
      !--------
      IF (write_HWV.eq.1) THEN
      write(irec,*) ' '
      write(irec,*) 'SCALARS HW     float 1'
      write(irec,*) 'LOOKUP_TABLE default'
      do NV=1,N_VERT
         if (abs(HWV(NV)).le.epsilon) HWV(NV)=0.0d0
         write(irec,991) HWV(NV)
      enddo
      ENDIF
      !--------
      IF (write_nbev.eq.1) THEN
      write(irec,*) ' '
      write(irec,*) 'SCALARS nbev  float 1'
      write(irec,*) 'LOOKUP_TABLE default'
      do NV=1,N_VERT
         write(irec,991) NBE_V(NV)*1.0d0 !CSV(NV)
      enddo
      ENDIF
!     __________________________________
!     Write vectors
      IF (write_Uvec.eq.1) THEN
      write(irec,*) ' '
      write(irec,*) 'VECTORS vector_U float'
      do NV=1,N_VERT
         if (abs(UVERT(NV)).le.epsilon) UVERT(NV)=0.0d0
         if (abs(VVERT(NV)).le.epsilon) VVERT(NV)=0.0d0
         write(irec,993) UVERT(NV),VVERT(NV),0.0d0
      enddo
      ENDIF
!     __________________________________
!     Close file
      rewind(irec)
      close(irec)
!     ____________________________________
!    |                                    |
!    |               DISPLAY              |
!    |____________________________________|

!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      write(*,'(t25,a20)')      ' __________________ '
      write(*,'(t25,a20)')      ' * Save Paraview *  '
      write(*,'(t25,a20)')      ' __________________ '
      if (NumPrint.lt.10) then
          write(*,'(t30,a3,i1,a4)')'H00',NumPrint,'.vtk'
      elseif ((NumPrint.ge.10).and.(NumPrint.lt.100)) then
          write(*,'(t30,a3,i2,a4)')'H0',NumPrint,'.vtk'
      else
          write(*,'(t30,a2,i3,a4)')'H',NumPrint,'.vtk' 
      endif
      write(*,'(t25,a8,f9.3)') '  time :',time
      write(*,'(t25,a20)')      ' __________________ '
      print*,'  '
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      RETURN
      END

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!---------------------------------------------------------------------!
!                           ***  END  ****                            !
!---------------------------------------------------------------------!
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
