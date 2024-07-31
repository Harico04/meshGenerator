!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!---------------------------------------------------------------------!
!         MAIN PROGRAM: MODULE CONTROL BY RTUFVM.cpp (/bin)           !
!                 UNSTRUCTURED FINITE VOLUME METHOD                   !
!                     2D SHALLOW-WATER EQUATION                       !
!                      Miguel Angel Uh Zapata                         !
!                     Last modification: Jul 2024                     !
!---------------------------------------------------------------------!
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!

      MODULE mained
      
!*********************************************************************!
!                                                                     !
!                            Definitions                              !
!                                                                     !
!*********************************************************************!

!     ____________________________________
!    |                                    |
!    |     Keys and common parameters     |
!    |____________________________________|

#     include "cppdefs.h"
      USE geometry
      IMPLICIT NONE
      INCLUDE 'common.mpf'
!     ____________________________________
!    |                                    |
!    |      Declaration of variables      |
!    |____________________________________|

!     ------------------
!     Variables
      real*8,dimension(:),  allocatable :: WL,WLN,WLV,WLVN
      real*8,dimension(:),  allocatable :: DEWL,DEWLN,DEWLV
      real*8,dimension(:),  allocatable :: H,HN,HV,HVN,HW,HWV
      real*8,dimension(:),  allocatable :: U,V,UN,VN,UVERT,VVERT
      real*8,dimension(:),  allocatable :: QX,QY,QXN,QYN,QXV,QYV
!     ------------------
!     Auxiliar variables
      real*8,dimension(:),  allocatable :: AH,CHEZY
      real*8,dimension(:),  allocatable :: dHWdx,dHWdy
      real*8,dimension(:),  allocatable :: dWLNdx,dWLNdy,dWLdx,dWLdy
      real*8,dimension(:,:),allocatable :: C0,HW0M,QC0,QC0N
!     ------------------
!     Geometry
      real*8,dimension(:),  allocatable :: XC,YC,X,Y
      real*8,dimension(:),  allocatable :: AE,DXC2,DYC2,DXY
      real*8,dimension(:,:),allocatable :: X0M,Y0M
      real*8,dimension(:,:),allocatable :: DXV,DYV,DXV2,DYV2
      real*8,dimension(:,:),allocatable :: DISDIF,A0M,DXC,DYC
      real*8,dimension(:,:),allocatable :: normalX,normalY
!     ------------------
!     Index and tags
      integer,dimension(:),  allocatable :: NBE,NBE_V
      integer,dimension(:),  allocatable :: NO_WB,NO_QB,NO_HB,NO_SB
      integer,dimension(:,:),allocatable :: NO_VP,NO_CP
      integer,dimension(:),  allocatable :: tagBC,tagBCv
!     ----------------------------
      REAL,DIMENSION(2):: tti
      REAL :: tcpu,MAXtcpu,timeIni,timeFin,CPU_TimeStep
!     ----------------------------
      integer:: i,j,k,np,nv1,nv2,nv3,vert,N1,N2,N3,N6,KK,NCC,L,j0,ii
      integer:: WLIterTime,idmin,idhr,idsec
      integer:: TagQ
      real*8 :: ExaQ
      real*8 :: CHEZ
      real*8 :: SUMX,SUMY,SUMXY
      real*8 :: XO,YO,DXDY,DXDY2,DXODY2,AA,DX,DY,DXCV,DYCV
!     ----------------------------
      character*50 TITRE
      character*10 TITREAH
      character*4 TITRE0(10)
      character*4 TITRE1(10)
      character*4 TITRE2(10)
      character*4 TITRE3(10)
      character*4 TITRE4(10)

      CONTAINS

!*********************************************************************!
!                                                                     !
!                            0) PARAMETERS                            !
!                                                                     !
!*********************************************************************!

      SUBROUTINE parameters

      print*,'                                                        '
      print*,'wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww'
      print*,'--------------------------------------------------------'
      print*,'           __________________________________           '
      print*,'          |                                  |          '
      print*,'          |          PROGRAM: NSMP2D         |          '
      print*,'          |__________________________________|          '
      print*,'                                                        '
      print*,'--------------------------------------------------------'
      print*,'wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww'
      print*,'                                                        '

!      _______________________________________________________________
!     |      |                                                        |
!     | 0.1  |                    Parameters                          |
!     |______|________________________________________________________|

      ModPrint = 100        ! <<<--- STEPS TO DISPLAY
      DisplayOnceThis = 1   ! <<<--- ACTIVATE DISPLAY
!     -------------------
      AHDIFF   = 0.1d0      ! DIFFUSION COEFFIENT
!     -------------------
      H00      = 1.0d-5     ! MINIMUM WATER DEPTH
      GAMA     = 1.0d0      ! TIME WEIGHTING COEFFICIENT: 0.5<=GAMA<=1
      GRA      = 9.81d0     ! GRAVITY
      OMEGA    = 1.0d0      ! COEFFICIENT FOR RELAXATION
      COEFN    = 0.012d0    ! MANNING COEFFICIENT (CHEZY-FRICTION)
!     -------------------
      WLO   = 0.0d0         ! INITIAL WL              Duck       Tablet
      HWO   = 0.186d0       ! INITIAL DEPTH        1) 1.0d0,  2) 0.186d0
      QXO   = 0.124d0       ! INITIAL X-VELOCITY   1) 1.48d0, 2) 0.124d0
      QYO   = 0.0d0         ! INITIAL Y-VELOCITY
!      _______________________________________________________________
!     |      |                                                        |
!     | 0.2  |     Read: Number of cell-center and vertex points      |
!     |______|________________________________________________________|

8     format(a60) 
      WRITE(*,8) '         __________________________________         '
      WRITE(*,8) '        |                                  |        '
      WRITE(*,8) '        |        ELEMETS & VERTICES        |        '
      WRITE(*,8) '        |__________________________________|        '
      WRITE(*,8) '                                                    ' 

      OPEN(251,FILE='Example_Cylinder/DATA.txt',STATUS='OLD')
      READ(251,*) TITRE
      READ(251,*) N_VERT
      READ(251,*) N_CELL0
      CLOSE(251)
!      _______________________________________________________________ 
!     |      |                                                        |
!     | 0.3  |               Defition of parameters                   |
!     |______|________________________________________________________|

9     format(t6,a40,i6)
!     ---------------- 
      N_HBMAX = N_VERT
      N_QBMAX = N_VERT
      N_WBMAX = N_VERT
      N_SBMAX = N_VERT
!     ----------------
      N_CELLghost = 500  ! WARNING!! N_CELLghost is set manual
      N_CELL = N_CELL0 + N_CELLghost
!     ----------------
      WRITE(*,9) '           DOMAIN:                      '
      WRITE(*,9) '           Number of vertices         = ',N_VERT
      WRITE(*,9) '           Number of elements         = ',N_CELL0
      WRITE(*,9) '           Number of cells with ghost = ',N_CELL
      WRITE(*,*) '                                        '

      END SUBROUTINE parameters
            
!*********************************************************************!
!                                                                     !
!                         I) Initialization                           !
!                                                                     !
!*********************************************************************!

      SUBROUTINE initialization
!      _______________________________________________________________ 
!     |      |                                                        |
!     | 1.1  |                 Allocate variables                     |
!     |______|________________________________________________________|

!     ------------------
!     Variables
      allocate(WL(N_CELL),WLN(N_CELL),WLV(N_VERT),WLVN(N_VERT))
      allocate(DEWL(N_CELL),DEWLN(N_CELL),DEWLV(N_VERT))
      allocate(H(N_CELL),HN(N_CELL),HV(N_VERT),HVN(N_VERT))
      allocate(HW(N_CELL),HWV(N_VERT))
      allocate(U(N_CELL),V(N_CELL))
      allocate(UN(N_CELL),VN(N_CELL))
      allocate(UVERT(N_VERT),VVERT(N_VERT))
      allocate(QX(N_CELL),QY(N_CELL))
      allocate(QXN(N_CELL),QYN(N_CELL))
      allocate(QXV(N_VERT),QYV(N_VERT))
!     ------------------
!     Auxiliar variables
      allocate(AH(N_CELL),CHEZY(N_CELL))
      allocate(dHWdx(N_CELL),dHWdy(N_CELL))
      allocate(dWLNdx(N_CELL),dWLNdy(N_CELL))
      allocate(dWLdx(N_CELL),dWLdy(N_CELL))
      allocate(C0(N_CELL,3),HW0M(N_CELL,3))
      allocate(QC0(N_CELL,3),QC0N(N_CELL,3))
!     ------------------
!     Geometry
      allocate(XC(N_CELL),YC(N_CELL),X(N_VERT),Y(N_VERT))
      allocate(AE(N_CELL),DXC2(N_CELL),DYC2(N_CELL),DXY(N_CELL))
      allocate(X0M(N_CELL,3),Y0M(N_CELL,3))
      allocate(DXV(N_CELL,3),DYV(N_CELL,3))
      allocate(DXV2(N_CELL,3),DYV2(N_CELL,3))
      allocate(DISDIF(N_CELL,3),A0M(N_CELL,3))
      allocate(DXC(N_CELL,3),DYC(N_CELL,3))
      allocate(normalX(N_CELL,3),normalY(N_CELL,3))
!     ------------------
!     Index and tags
      allocate(NO_WB(N_WBMAX))  ! Boundary elements type wall
      allocate(NO_QB(N_QBMAX))  ! Boundary elements type inflow
      allocate(NO_HB(N_HBMAX))  ! Boundary elements type outflow
      allocate(NO_SB(N_SBMAX))  ! Boundary elements type structure
      allocate(NBE(N_CELL),NBE_V(N_VERT))
      allocate(NO_VP(N_CELL,3),NO_CP(N_CELL,3))
      allocate(tagBC(N_CELL0),tagBCv(N_VERT))
!     ------------------
!     MODULE GEOEMETRY
      call allocate_geometry

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 1.2  |                  Read & assign data                    |
!     |______|________________________________________________________|

8     format(a60)
      WRITE(*,8) '         __________________________________         '
      WRITE(*,8) '        |                                  |        '
      WRITE(*,8) '        |          Read data.txt           |        '
      WRITE(*,8) '        |        Read input_data.dat       |        '
      WRITE(*,8) '        |__________________________________|        '
      WRITE(*,8) '                                                    '

74    FORMAT(10A4)
76    FORMAT(10A4,F15.5)
85    FORMAT(20I7)
86    FORMAT(10F8.3)
87    FORMAT(2F15.5)
88    FORMAT(3I8)

!      ____________________________________
!     |                                    |
!     |  1.2.1 Read: data.txt              |
!     |____________________________________|

!     ________________________________________________________
!     Open file
      OPEN(22,FILE='Example_Cylinder/DATA.txt',STATUS='OLD')
      READ(22,74) TITRE
      READ(22,74) TITRE
      READ(22,74) TITRE
!     ________________________________________________________
!     Read: Numbering of the cell vertices for each element
      READ(22,74) TITRE
      READ(22,88)((NO_VP(I,K),K=1,3),I=1,N_CELL0)
!     ________________________________________________________
!     Read: Coordinates of cell vertices
      READ(22,74) TITRE
      READ(22,87)(X(I),Y(I),I=1,N_VERT)
!     ________________________________________________________
!     Read: Numbering of the surrounding three cell centers
      READ(22,74) TITRE
      READ(22,88)((NO_CP(I,K),K=1,3),I=1,N_CELL0)
!     ________________________________________________________
!     Read: Bottom levels
      READ(22,74) TITRE
      READ(22,86)(HV(I),I=1,N_VERT)
!     ________________________________________________________
!     Read: Type of cell (inside or boundary)
      READ(22,74) TITRE
      READ(22,85)(NBE(I),I=1,N_CELL0)
!     ________________________________________________________
!     Read: Inputting for wall boundary vertices
      READ(22,74) TITRE
      READ(22,*) N_WB
      IF (N_WB.EQ.0) READ(22,74) TITRE
      IF (N_WB.GE.N_WBMAX) STOP
      IF (N_WB.NE.0) READ(22,85) (NO_WB(I),I=1,N_WB)
!     ________________________________________________________
!     Read: Inputting for discharge normal to boundary vert
      READ(22,74) TITRE
      READ(22,*) N_QB
      IF (N_QB.EQ.0) READ(22,74) TITRE
      IF (N_QB.GE.N_QBMAX) STOP
      IF (N_QB.NE.0) READ(22,85) (NO_QB(I),I=1,N_QB)
!     ________________________________________________________
!     Read: Inputting for water level boundary vertices
      READ(22,74) TITRE
      READ(22,*) N_HB
      IF (N_HB.EQ.0) READ(22,74) TITRE
      IF (N_HB.GE.N_HBMAX) stop
      IF (N_HB.NE.0) READ(22,85) (NO_HB(I),I=1,N_HB)
!     ________________________________________________________
!     Read: Number of sampling cells
      READ(22,74) TITRE
      READ(22,*) N_SB
      IF (N_SB.GE.N_SBMAX) stop
      IF (N_SB.GE.1) READ(22,85) (NO_SB(I),I=1,N_SB)
!     ________________________________________________________
!     Close file
      CLOSE(22)

      NBE_V = 0
      if (N_WB.NE.0) NBE_V(NO_WB(1:N_WB)) = 1
      if (N_QB.NE.0) NBE_V(NO_QB(1:N_QB)) = 2
      if (N_HB.NE.0) NBE_V(NO_HB(1:N_HB)) = 3
      !if (N_SB.NE.0) NBE_V(NO_SB(1:N_SB)) = 6

!      ____________________________________
!     |                                    |
!     |  1.2.2 Read: input_data.dat        |
!     |____________________________________|

!     ________________________________________________________
!     Open file
      OPEN(13,FILE='Example_Cylinder/INPUT_DATA.dat',STATUS='OLD')
!     ________________________________________________________
!     Read: Time values
      READ(13,74) TITRE0
      READ(13,74) TITRE0
      READ(13,74) TITRE0
      READ(13,76) TITRE0,TMAX
      READ(13,76) TITRE0,DT
      READ(13,76) TITRE0,TPRINT
!     ________________________________________________________
!     Read: Values of initial conditions
      READ(13,74) TITRE0
#     ifdef KeyDiff_UseAHDIFF
      READ(13,76) TITRE0,AHDIFF
#     endif
!     ________________________________________________________
!     Close file
      CLOSE(13)

      WRITE(*,'(t17,a30,f8.2)') 'Simulation duration  TMAX   =',TMAX
      WRITE(*,'(t17,a30,f8.5)') 'Time step             DT    =',DT
      WRITE(*,'(t17,a30,f8.5)') 'Time interval saving TPRINT =',TPRINT

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 1.3  |           Geometry variables of the mesh               |
!     |______|________________________________________________________|

      WRITE(*,8) '         __________________________________         '
      WRITE(*,8) '        |                                  |        '
      WRITE(*,8) '        |         Calculate GEOMETRY       |        '
      WRITE(*,8) '        |__________________________________|        '
      WRITE(*,8) '                                                    '
!      ____________________________________
!     |                                    |
!     |  1.3.1 Geometry : XC,YC,AE,        |
!     |        Distance : DXV,DYV,DXV2,DYV2|
!     |        Outnormal: (normX,normY)    |
!     |        Mid edge points: X0M,Y0M    |
!     |____________________________________|

      DO NC=1,N_CELL0
         N1=NO_VP(NC,1)
         N2=NO_VP(NC,2)
         N3=NO_VP(NC,3)
!        --------------------------
!        Cell-centered points
         XC(NC)=(X(N1)+X(N2)+X(N3))/3.0
         YC(NC)=(Y(N1)+Y(N2)+Y(N3))/3.0
!        --------------------------
!        Area of a cell
         AE(NC)=0.5*(X(N1)*(Y(N2)-Y(N3))
     *              +X(N2)*(Y(N3)-Y(N1))
     *              +X(N3)*(Y(N1)-Y(N2)))
!        --------------------------
!        Vertex distance
         DXV(NC,1)=X(N2)-X(N1)
         DXV(NC,2)=X(N3)-X(N2)
         DXV(NC,3)=X(N1)-X(N3)
         DYV(NC,1)=Y(N2)-Y(N1)
         DYV(NC,2)=Y(N3)-Y(N2)
         DYV(NC,3)=Y(N1)-Y(N3)
         DXV2(NC,1)=DXV(NC,1)*DXV(NC,1)
         DXV2(NC,2)=DXV(NC,2)*DXV(NC,2)
         DXV2(NC,3)=DXV(NC,3)*DXV(NC,3)
         DYV2(NC,1)=DYV(NC,1)*DYV(NC,1)
         DYV2(NC,2)=DYV(NC,2)*DYV(NC,2)
         DYV2(NC,3)=DYV(NC,3)*DYV(NC,3)
!        -----------------------
!        Outnormal
         normalX(NC,1) = DYV(NC,1)/SQRT(DXV2(NC,1)+DYV2(NC,1))
         normalY(NC,1) =-DXV(NC,1)/SQRT(DXV2(NC,1)+DYV2(NC,1))
         normalX(NC,2) = DYV(NC,2)/SQRT(DXV2(NC,2)+DYV2(NC,2))
         normalY(NC,2) =-DXV(NC,2)/SQRT(DXV2(NC,2)+DYV2(NC,2))
         normalX(NC,1) = DYV(NC,3)/SQRT(DXV2(NC,3)+DYV2(NC,3))
         normalY(NC,1) =-DXV(NC,3)/SQRT(DXV2(NC,3)+DYV2(NC,3))
!        -----------------------
!        Middle edge points
         X0M(NC,1) = 0.5d0*(X(N1)+X(N2))
         Y0M(NC,1) = 0.5d0*(Y(N1)+Y(N2))
         X0M(NC,2) = 0.5d0*(X(N2)+X(N3))
         Y0M(NC,2) = 0.5d0*(Y(N2)+Y(N3))
         X0M(NC,3) = 0.5d0*(X(N3)+X(N1))
         Y0M(NC,3) = 0.5d0*(Y(N3)+Y(N1))
      ENDDO
!      ____________________________________
!     |                                    |
!     |  1.3.2   *** GHOST CELLS ***       |
!     |          Index     : NO_CP         |
!     |          Geometry  : XC,YC,AE      |
!     |____________________________________|

      NCC = N_CELL0
      DO NC=1,N_CELL0
         IF (NBE(NC).GT.0) THEN 
         DO K=1,3
            L = NO_CP(NC,K)
            IF ((L.LE.0).OR.(L.GT.N_CELL0)) THEN
                NCC=NCC+1
!               -------------------
!               Index
                NO_CP(NC,K)=NCC
!               -------------------------------------------
!               Information used in the new BC formulation
                NO_CP(NCC,1) = NC    !<--- Cell i coming from
                NO_CP(NCC,2) = K     !<--- Cell neighborn number j of i
!               -------------------
!               Cell-center
                KK=K+1
                IF(KK.GT.3) KK=KK-3
                KV1=NO_VP(NC,K)
                KV2=NO_VP(NC,KK)
!               -------------------
                DY=DYV(NC,K)
                DX=DXV(NC,K)
                IF (DABS(DY).LT.1.0E-7) THEN
                   YO=Y(KV2)
                   XO=XC(NC)
                ELSE
                   DXDY  = DX/DY
                   DXDY2 = DXDY*DXDY
                   AA=1.0d0+DXDY2
                   YO=(YC(NC)-(X(KV2)-XC(NC))*DXDY+Y(KV2)*DXDY2)/AA
                   XO=X(KV2)+(YO-Y(KV2))*DXDY
                ENDIF
                XC(NCC)=2.0d0*XO-XC(NC)
                YC(NCC)=2.0d0*YO-YC(NC)
!               -------------------
!               Area
                AE(NCC)=AE(NC)
            ENDIF
         ENDDO
         ENDIF
      ENDDO

      N_CELLexact = NCC
      N_CELLghost = N_CELLexact - N_CELL0

      if ((N_CELL-N_CELL0).lt.N_CELLghost) then
         print*,'               Initial N_CELLghost =', N_CELL-N_CELL0
         print*,'               Real    N_CELLghost =', N_CELLghost
         print*,'  ------>>>>>  Correct this value !!! [LINE 144]'
         print*,' '
         stop
      endif
!      ____________________________________
!     |                                    |
!     |  1.3.3 Gradient values:            |
!     |        DXC,DYC,DXC2,DYC2,DXY,DISDIF|
!     |____________________________________|

      DO NC=1,N_CELL0
         SUMX =0.0d0
         SUMY =0.0d0
         SUMXY=0.0d0
         DO K=1,3
            MK = NO_CP(NC,K)
!           -------------------------
            DXC(NC,K) = XC(MK)-XC(NC)
            DYC(NC,K) = YC(MK)-YC(NC)
!           --------------------------
            SUMX  = SUMX  + DXC(NC,K)*DXC(NC,K)
            SUMY  = SUMY  + DYC(NC,K)*DYC(NC,K)
            SUMXY = SUMXY + DXC(NC,K)*DYC(NC,K)
!           -----------------------------------------
!           Combined distance
            DISDIF(NC,K) = (DXC(NC,K)*DXV(NC,K)+DYC(NC,K)*DYV(NC,K))
         ENDDO
         DXC2(NC)= SUMX
         DYC2(NC)= SUMY
         DXY(NC) = SUMXY
      ENDDO
!      ____________________________________
!     |                                    |
!     |  1.3.4 Interpolation values:       |
!     |        dlVsum,weight               |
!     |        Dimsurrounding,surrounding  |
!     |____________________________________|

      weight = 0.0d0
      DO nv=1,N_VERT
         j0 = 0
         dlVsum(nv) = 0.0d0
!        ------------------
!        Inside cells
         do nc=1,N_CELL0
            do j=1,3
               if (nv.eq.No_vp(nc,j)) then
                  j0 = j0 + 1
                  surrounding(nv,j0) = nc
                  dxCV = XC(nc)-X(nv)
                  dyCV = YC(nc)-Y(nv)
                  weight(nv,j0) = 1.0d0/sqrt(dxCV*dxCV+dyCV*dyCV)
                  dlVsum(nv) = dlVsum(nv) + weight(nv,j0)
               endif
            enddo
         enddo
         Dimsurrounding(nv) = j0  ! ---> Number of surroundings
!        ------------------
!        Final weights
         do j=1,Dimsurrounding(nv)
            weight(nv,j) = weight(nv,j)/dlVsum(nv)
         enddo
      ENDDO
!      ____________________________________
!     |                                    |
!     |  1.3.5 AREA OF THE CONTROL VOLUMES |
!     |        FOR DIFFUSION TERMS: A0M    |
!     |____________________________________|

      DO NC=1,N_CELL0
         DO K=1,3
            MK = NO_CP(NC,K)
            KK=K+1
            IF (KK.GT.3) KK=KK-3
            KV1 = NO_VP(NC,K)
            KV2 = NO_VP(NC,KK)
            IF (NBE(NC).EQ.0) THEN
               A0M(NC,K)=0.5*(X(KV1)*(Y(KV2)-YC(NC))+X(KV2)*
     *             (YC(NC)-Y(KV1))+XC(NC)*(Y(KV1)-Y(KV2)))-
     *             0.5*(X(KV1)*(Y(KV2)-YC(MK))+X(KV2)*(YC(MK)-Y(KV1))+
     *             XC(MK)*(Y(KV1)-Y(KV2)))
            ELSE
               IF (MK.LE.N_CELL0) THEN
                  A0M(NC,K)=0.5*(X(KV1)*(Y(KV2)-YC(NC))+X(KV2)*
     *            (YC(NC)-Y(KV1))+XC(NC)*(Y(KV1)-Y(KV2)))-
     *            0.5*(X(KV1)*(Y(KV2)-YC(MK))+X(KV2)*(YC(MK)-Y(KV1))+
     *            XC(MK)*(Y(KV1)-Y(KV2)))
               ELSE
                 A0M(NC,K)=(X(KV1)*(Y(KV2)-YC(NC))+X(KV2)*
     *           (YC(NC)-Y(KV1))+XC(NC)*(Y(KV1)-Y(KV2)))
               ENDIF
            ENDIF
        ENDDO
      ENDDO

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 1.4  |       NEW TAGS CEL-CENTER & VERTEX SET MANUALLY        |
!     |______|________________________________________________________|

#     ifdef  KeyTagsManual_Cylinder

      WRITE(*,8) '         __________________________________         '
      WRITE(*,8) '        |                                  |        '
      WRITE(*,8) '        |       Set new tags manually      |        '
      WRITE(*,8) '        |__________________________________|        '
      WRITE(*,8) '                                                    '

!      ____________________________________
!     |                                    |
!     |  1.4.1 Maxi & Min of the domain    |
!     |____________________________________|

      XDIni = X(1)
      XDFin = X(1)
      YDIni = Y(1)
      YDFin = Y(1)
      DO NV=2,N_VERT
         XDIni = min(XDIni,X(NV))
         XDFin = max(XDFin,X(NV))
         YDIni = min(YDIni,Y(NV))
         YDFin = max(YDFin,Y(NV))
      ENDDO

      print*,'               MAXIMUM & MINIMUM DOMAIN LIMITS'
      print*,'               * XDmin = ',XDIni
      print*,'               * XDmax = ',XDFin
      print*,'               * YDmin = ',YDIni
      print*,'               * YDmax = ',YDFin
      
!      ____________________________________
!     |                                    |
!     |  1.4.2 Tags cell-centers: NBE      |
!     |____________________________________|

      tagBC = 0
      DO ii = N_CELL0+1,N_CELLexact
         NC = NO_CP(ii,1)
         !-----------------------------
         ! Inflow: Discharge 
         if (xc(ii).lt.XDIni) then
             NBE(NC)   = 2
             tagBC(NC) = 1 !<<-- Include two inflow corners
         endif
         !-----------------------------
         ! Walls
         if ((yc(ii).lt.YDIni).and.(tagBC(NC).eq.0)) then
             NBE(NC)   = 1
             tagBC(NC) = 1 !<<-- Include outflow south corner
         endif
         if ((yc(ii).gt.YDFin).and.(tagBC(NC).eq.0)) then
             NBE(NC)   = 1
             tagBC(NC) = 1 !<<-- Include outflow south corner
         endif 
         !-----------------------------
         ! Outflow: Free or Water level
         if ((xc(ii).gt.XDFIn).and.(tagBC(NC).eq.0)) then
             NBE(NC) = 3  !<<-- Water Level
         endif 
         !-----------------------------
         ! Structure
         if ((yc(ii).gt.YDIni).AND.(yc(ii).lt.YDFin).AND.
     *       (xc(ii).gt.XDIni).AND.(xc(ii).lt.XDFIn)) then 
             NBE(NC) = 6
         endif
      ENDDO
!      ____________________________________
!     |                                    |
!     |  1.4.3 Tags cell-centers: NBE_V    |
!     |____________________________________|

      tagBCv = 0
      !-----------------------------
      ! Inflow: Discharge
      n2 = 0
      DO nv=1,N_VERT
         if (X(nv).le.(XDIni+1.0d-7)) then
            n2 = n2 + 1
            NO_QB(n2) = nv
            tagBCv(nv) = 1
         endif
      ENDDO
      N_QB = n2
      !-----------------------------
      ! Walls
      n1 = 0
      DO nv=1,N_VERT
         if (NBE_V(nv).ne.0) then
            if (abs(Y(nv)-YDIni).lt.1d-7.AND.tagBCv(nv).eq.0) then
               n1 = n1 + 1
               NO_WB(n1) = nv
               tagBCv(nv) = 1
            endif
            if (abs(Y(nv)-YDFin).lt.1d-7.AND.tagBCv(nv).eq.0) then
               n1 = n1 + 1
               NO_WB(n1) = nv
               tagBCv(nv) = 1
            endif
         endif
      ENDDO
      N_WB = n1
      !-----------------------------
      ! Outflow: Free or Water level
      n3 = 0
      DO nv=1,N_VERT
         if (X(nv).ge.(XDFin-1.0d-7).and.(tagBCv(nv).eq.0)) then
            n3 = n3 + 1
            NO_HB(n3) = nv
         endif
      ENDDO
      N_HB = n3
      !-----------------------------
      ! Structure
      n6 = 0
      DO nv=1,N_VERT
         if (NBE_V(nv).ne.0) then
             if ((Y(nv).gt.YDIni).AND.(Y(nv).lt.YDFin).AND.
     *           (X(nv).gt.XDIni).AND.(X(nv).lt.XDFin)) then
                 n6 = n6 + 1
                 NO_SB(n6) = nv
             endif
         endif
      ENDDO
      N_SB = n6

      NBE_V = 0
      if (N_WB.NE.0) NBE_V(NO_WB(1:N_WB)) = 1
      if (N_QB.NE.0) NBE_V(NO_QB(1:N_QB)) = 2
      if (N_HB.NE.0) NBE_V(NO_HB(1:N_HB)) = 3
      if (N_SB.NE.0) NBE_V(NO_SB(1:N_SB)) = 6

      print*,'               '
      print*,'               TAGS MANUAL AT BOUNDARY VERTICES'
      print*,'               * Wall (lateral)        =', n1
      print*,'               * Discharge (inflow)    =', n2
      print*,'               * Water level (outflow) =', n3
      print*,'               * Structure (inside)    =', n6
      print*,'               '

#     endif

      END SUBROUTINE initialization
      
!*********************************************************************!
!                                                                     !
!                    II) Initial conditions (t=0)                     !
!                                                                     !
!*********************************************************************!

      SUBROUTINE initialconditions
      
      TIME = 0.0d0
      NT   = 0
      CountWLIterations = 0
      CPU_TimeStep = 0.0

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 2.1  |        Initial conditions of the variables             |
!     |______|________________________________________________________|

8     format(a60)
      WRITE(*,8) '         __________________________________         '
      WRITE(*,8) '        |                                  |        '
      WRITE(*,8) '        |       Set initial conditions     |        '
      WRITE(*,8) '        |__________________________________|        '
      WRITE(*,8) '                                                    '

!      ____________________________________
!     |                                    |
!     |  2.1.1 Initial depth & water level |
!     |____________________________________|

!     ----------------
      H    = HWO
      HN   = HWO
      WL   = WLO
      WLN  = WLO
      HW   = WL + H
!     ----------------
      HV   = HWO
      HVN  = HWO
      WLV  = WLO
      WLVN = WLO
      HWV  = WLV + HV

!      ____________________________________
!     |                                    |
!     |  2.1.2 Initial discharge & velocity|
!     |____________________________________|

      QX  = QXO
      QY  = QYO
      QXN = QXO
      QYN = QYO
!     ----------------
      U   = QXO/HWO
      V   = QYO/HWO
      UN  = QXO/HWO
      VN  = QYO/HWO
!     ----------------
      QXV   = QXO
      QYV   = QYO
      UVERT = QXO/HWO
      VVERT = QYO/HWO
!      ____________________________________
!     |                                    |
!     |  2.1.3 Initial auxiliar variables  |
!     |____________________________________|

      AH    = 0.0d0   ! Diffusive coefficient
      C0    = 0.0d0   ! Mass flux
      QC0   = 0.0d0
      QC0N  = 0.0d0
      CHEZY = 0.0d0
!     -----------
      HW0M  = HWO

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 2.3  |      Boundary Conditions for initial conditions        |
!     |______|________________________________________________________|

      WRITE(*,8) '         __________________________________         '
      WRITE(*,8) '        |                                  |        '
      WRITE(*,8) '        |       Set boundary conditions    |        '
      WRITE(*,8) '        |__________________________________|        '
      WRITE(*,8) '                                                    '

!     ____________________________________
!     Water level 
      CALL BC_WL(DEWL,DEWLV,WLN,WLVN,HW,HWV,
     *           H,HV,WL,WLV,
     *           NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V)

!     ____________________________________
!     Velocity 
      CALL BC_Fluid(UN,UVERT,VN,VVERT,QXN,QXV,QYN,QYV,
     *              HW,NO_CP,NO_WB,NO_QB,NO_HB,NO_SB,NBE)

!      _______________________________________________________________
!     |      |                                                        |
!     | 2.4  |           FLUX for the initial conditions              |
!     |______|________________________________________________________|

      WRITE(*,8) '         __________________________________         '
      WRITE(*,8) '        |                                  |        '
      WRITE(*,8) '        |           Initial flux           |        '
      WRITE(*,8) '        |__________________________________|        '
      WRITE(*,8) '                                                    '

!     ____________________________________
!     Gradients of HW (t=0)
      CALL GRADF(NBE,NO_CP,DXC,DYC,HW,DXC2,DYC2,DXY,dHWdx,dHWdy)
!     ____________________________________
!     MASS FLUX: C0 (t^0)
      CALL CALCO(C0,U,V,NO_CP,DXV,DYV)
!     ____________________________________
!     EDGE WATER DEPTH: QC0 & HW0M (t^0)
      CALL CALFLUX(QC0,HW0M,
!                  --------
     *             HW,HWV,C0,
     *             dHWdx,dHWdy,
!                  --------
     *             NBE,NO_CP,NO_VP,
     *             X,Y,XC,YC,AE,
     *             DXV,DYV,DXC,DYC,
     *             X0M,Y0M)

!      _______________________________________________________________
!     |      |                                                        |
!     | 2.5  |            SAVING INITIAL SOLUTION                     |
!     |______|________________________________________________________|

      WRITE(*,8) '         __________________________________         '
      WRITE(*,8) '        |                                  |        '
      WRITE(*,8) '        |     Saving initial condition     |        '
      WRITE(*,8) '        |__________________________________|        '
      WRITE(*,8) '                                                    '
!     __________________________________________________
!     SAVING INITIAL CONDITIONS (PARAVIEW: H000.vtk)
      
      TPO  = 0.0d0  ! Auxiliar: Previous time saved
      NRES = 0      ! Auxiliar: Counter for saved results

      CALL OUTSAV(WLVN,HWV,UVERT,VVERT,X,Y,NBE_V,NO_VP,NRES)

      END SUBROUTINE initialconditions
            
!*********************************************************************!
!                                                                     !
!             III) Main structure: time step simulations              !
!                       Beginning of the time loop                    !
!                                                                     !
!*********************************************************************!

      SUBROUTINE update(MagU)

      real*8, dimension(:) :: MagU(6161)

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 3.0  |             Initialization of the lopp                 |
!     |______|________________________________________________________|

8     format(a60)

      IF (TIME.EQ.0d0) THEN     
      WRITE(*,8) '         __________________________________         '
      WRITE(*,8) '        |                                  |        '
      WRITE(*,8) '        |         Initial time steps       |        '
      WRITE(*,8) '        |__________________________________|        '
      WRITE(*,8) '                                                    '
      ENDIF

100   CONTINUE

      call cpu_time(timeIni) 

      NT   = NT + 1
      TIME = TIME + DT
      if (mod(NT,ModPrint).EQ.0) write(*,*) NT,TIME,'of',TMAX

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 3.1  |     PREDICTOR STEP: DISCHARGE IN IMPLICITE MODE        |
!     | ==== |                     QX* and QY*                        |
!     |______|________________________________________________________|

!      ____________________________________
!     |                                    |
!     |  3.1.1 Diffusion coefficients: AH  |
!     |____________________________________|

!     ____________________________________
!     CHEZY VALUES
      DO NC=1,N_CELL
         CHEZ=(1./COEFN)*HW(NC)**(1./6.)
         CHEZY(NC)=CHEZ*CHEZ
      ENDDO
 
!     ____________________________________
!     DIFFUSION COEFFICIENT
      AH = 0.0d0
#     ifdef KeyDiffusion
         CALL DispersionAH(AH,CHEZY,U,V,HW)
#     endif

!      ____________________________________
!     |                                    |
!     |  3.1.2  Discharge: QXN & QXV       |
!     |____________________________________|

      TagQ = 1
      ExaQ = QXO
      CALL PredictorQ(QX,QXN,QXV,
!                   ------------- 
     *              C0,ExaQ,TagQ,
!                   ------------- 
     *              QX,QY,QXV,QYV,
!                   -------------
     *              NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V,
!                   -------------     
     *              X,Y,XC,YC,AE,DXV,DYV,DXV2,DYV2,DXC,DYC,
     *              DXC2,DYC2,DXY,
!                   -------------
     *              X0M,Y0M,DISDIF,
     *              A0M,AH)

!      ____________________________________
!     |                                    |
!     |  3.1.3  Discharge: QYN & QYV       |
!     |____________________________________|

      TagQ = 2
      ExaQ = QYO
      CALL PredictorQ(QY,QYN,QYV,
!                   ------------- 
     *              C0,ExaQ,TagQ,
!                   ------------- 
     *              QX,QY,QXV,QYV,
!                   -------------
     *              NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V,
!                   -------------     
     *              X,Y,XC,YC,AE,DXV,DYV,DXV2,DYV2,DXC,DYC,
     *              DXC2,DYC2,DXY,
!                   -------------
     *              X0M,Y0M,DISDIF,
     *              A0M,AH)

!      ____________________________________
!     |                                    |
!     |  3.1.4  Velocity: UN & VN          |
!     |____________________________________|

      DO NC=1,N_CELL
         UN(NC)=QX(NC)/HW(NC)
         VN(NC)=QY(NC)/HW(NC)
      ENDDO
      DO NV=1,N_VERT
         UVERT(NV)=QXV(NV)/HWV(NV)
         VVERT(NV)=QYV(NV)/HWV(NV)
      ENDDO

!      ____________________________________
!     |                                    |
!     |  3.1.5  BOUNDARY CONDITIONS        |
!     |____________________________________| 

      CALL BC_Fluid(UN,UVERT,VN,VVERT,QXN,QXV,QYN,QYV,
     *              HW,NO_CP,NO_WB,NO_QB,NO_HB,NO_SB,NBE)

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 3.2  |    WATER LEVELS USING A POISSON EQN. AND S.O.R.        |
!     | ==== |                      WLN & HW                          |
!     |______|________________________________________________________|

!      ____________________________________
!     |                                    |
!     |  3.2.1  Gradient: WL & HW          |
!     |____________________________________|

      CALL GRADF(NBE,NO_CP,DXC,DYC,WL,DXC2,DYC2,DXY,dWLdx,dWLdy)
      CALL GRADF(NBE,NO_CP,DXC,DYC,HW,DXC2,DYC2,DXY,dHWdx,dHWdy)
     
!      ____________________________________
!     |                                    |
!     |  3.2.2  FLUXES                     |
!     |____________________________________|

!     ____________________________________
!     MASS FLUX: C0 (t^*)
      CALL CALCO(C0,UN,VN,NO_CP,DXV,DYV)
!     ____________________________________
!     EDGE WATER DEPTH: QC0 & HW0M (t^*)
      CALL CALFLUX(QC0N,HW0M,
!                  --------
     *             HW,HWV,C0,
     *             dHWdx,dHWdy,
!                  --------
     *             NBE,NO_CP,NO_VP,
     *             X,Y,XC,YC,AE,
     *             DXV,DYV,DXC,DYC,
     *             X0M,Y0M)
     
!      ____________________________________
!     |                                    |
!     |  3.2.3  CALCULATE WL               |
!     |____________________________________|

      CALL PoissonWL(DEWL,DEWLV,WLN,WLVN,HW,HWV,
!                  -------------
     *             WL,WLV,H,HV,
     *             U,V,UN,VN,UVERT,VVERT,QX,QY,QXN,QYN,QXV,QYV,
!                  -------------
     *             C0,QC0,QC0N,HW0M,
!                  -------------
     *             dWLdx,dWLdy,
     *             dHWdx,dHWdy,
!                  -------------
     *             NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V,
!                  -------------
     *             X,Y,XC,YC,AE,
     *             DXV,DYV,DXV2,DYV2,DXC,DYC,
     *             DXC2,DYC2,DXY,
!                  -------------
     *             X0M,Y0M,DISDIF,
     *             A0M,AH,CHEZY)
     
!      _______________________________________________________________ 
!     |      |                                                        |
!     | 3.3  |    CORRECTION OF THE UNIT DISCHARGE COMPONENTS         |
!     | ==== |                QX^(n+1) and QY^(n+1)                   |
!     |______|________________________________________________________|

!      ____________________________________
!     |                                    |
!     |  3.3.1  Gradient: WLN              |
!     |____________________________________|

      CALL GRADF(NBE,NO_CP,DXC,DYC,WLN,DXC2,DYC2,DXY,dWLNdx,dWLNdy)

!      ____________________________________
!     |                                    |
!     |  3.3.2  CALCULATE (QXN,QYN)        |
!     |____________________________________|
     
      CALL CorrectorQ(UN,UVERT,
     *                VN,VVERT,
     *                QXN,QXV,
     *                QYN,QYV,
!                     -------------
     *                U,V,QX,QY,WL,WLV,WLN,WLVN,HW,HWV,H,HV,
     *                dWLdx,dWLdy,dWLNdx,dWLNdy,
     *                QXO,QYO,     
!                     -------------
     *                NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V,
!                     -------------
     *                X,Y,XC,YC,AE,DXV,DYV,DXV2,DYV2,DXC,DYC,
     *                DXC2,DYC2,DXY,
!                     -------------
     *                X0M,Y0M,DISDIF,
!                     -------------
     *                A0M,AH,CHEZY)

!      ____________________________________
!     |                                    |
!     |  3.3.3  BOUNDARY CONDITIONS        |
!     |____________________________________|

      CALL BC_Fluid(UN,UVERT,VN,VVERT,QXN,QXV,QYN,QYV,
     *              HW,NO_CP,NO_WB,NO_QB,NO_HB,NO_SB,NBE)

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 3.4  |   CALCULATIONS OF THE FLUXES FOR THE NEXT TIME STEP    |
!     |______|________________________________________________________|

!     ____________________________________
!     Gradients of HW 
      CALL GRADF(NBE,NO_CP,DXC,DYC, HW,DXC2,DYC2,DXY,dHWdx,dHWdy)
!     ____________________________________
!     MASS FLUX: C0 (t^new)
      CALL CALCO(C0,UN,VN,NO_CP,DXV,DYV)
!     ____________________________________
!     EDGE WATER DEPTH: QC0 & HW0M (t^new)
      CALL CALFLUX(QC0,HW0M,
!                  --------
     *             HW,HWV,C0,
     *             dHWdx,dHWdy,
!                  --------
     *             NBE,NO_CP,NO_VP,
     *             X,Y,XC,YC,AE,
     *             DXV,DYV,DXC,DYC,
     *             X0M,Y0M)
           
!      _______________________________________________________________ 
!     |      |                                                        |
!     | 3.5  |              Ending of the time loop                   |
!     |______|________________________________________________________| 

!      ____________________________________
!     |                                    |
!     |  3.5.1  Update variables           |
!     |____________________________________|

      DO NC=1,N_CELL
        QX(NC) = QXN(NC)
        QY(NC) = QYN(NC)
        U(NC)  = UN(NC)
        V(NC)  = VN(NC)
        HW(NC) = WLN(NC)+H(NC)
        HW(NC) = DMAX1(HW(NC),H00)
        WL(NC) = WLN(NC)
      ENDDO

      DO NV=1,N_VERT
        DEWLV(NV) = WLVN(NV)-WLV(NV)
        WLV(NV)   = WLVN(NV)
        HWV(NV)   = WLVN(NV)+HV(NV)
        HWV(NV)   = DMAX1(HWV(NV),H00)
        UVERT(NV) = QXV(NV)/HWV(NV)
        VVERT(NV) = QYV(NV)/HWV(NV)
      ENDDO

!      ____________________________________
!     |                                    |
!     |  3.5.2  CPU Time per iteration     |
!     |____________________________________|

      call cpu_time(timeFin)
      CPU_TimeStep = CPU_TimeStep + (timeFin-timeIni)

!      ____________________________________
!     |                                    |
!     |  3.5.3 Calculate MagU (RTUFVM.cpp) |
!     |____________________________________|

      do ii=1,6061
         MagU(ii) = 10.0d0 
      enddo
      
!      ____________________________________
!     |                                    |
!     |    Criteria to finish simulations  |
!     |____________________________________|

      !IF(TIME.LT.TMAX) GOTO 100 !<<- The criteria is now control by RTUFVM.cpp

      END SUBROUTINE update

!*********************************************************************!
!                                                                     !
!                   III-B) Save results in Paraview                   !
!                                                                     !
!*********************************************************************!
      
      SUBROUTINE saveparaview

      !IF (TIME-TPO.GE.TPRINT) THEN ! <<<--- This conditional is now control by RTUFVM.cpp
!   TPO = NRES*TPRINT
      
         NRES = mod(NRES + 1,20)
!        _______________
!        Saving Paraview
         CALL OUTSAV(WLVN,HWV,UVERT,VVERT,X,Y,NBE_V,NO_VP,NRES) !mod(NRES, #) hace que solo genera archivos del 0 al # .vtk
      !ENDIF

         END SUBROUTINE saveparaview

!*********************************************************************!
!                                                                     !
!             III-C) Read the dispersion parameter AH                 !
!                                                                     !
!*********************************************************************!
      
      SUBROUTINE readviscosity

#     ifdef KeyDiff_UseAHDIFF
      open(13,FILE='Example_Cylinder/viscosity.txt',STATUS='OLD')
      read(13,'(a10,F15.5)') TITREAH,AHDIFF
      close(13)
      print*,'AHDIFF=',AHDIFF
#     endif

      END SUBROUTINE readviscosity
      
!*********************************************************************!
!                                                                     !
!                          IV) Finalization                           !
!                                                                     !
!*********************************************************************!

      SUBROUTINE finalization
!      _______________________________________________________________ 
!     |      |                                                        |
!     | 4.1  |               Saving final results                     |
!     |______|________________________________________________________|

!      _______________________________________________________________ 
!      SAVE PARAVIEW AND TECPLOT (final results)
       !IF (TMAX-TPO.GE.1e-08) THEN
       !    CALL OUTSAV(WLVN,HWV,UVERT,VVERT,X,Y,NBE_V,NO_VP,NRES)
       !ENDIF 
!      _______________________________________________________________ 
!     |      |                                                        |
!     | 4.2  |           Display total time of WL iterations          |
!     |______|________________________________________________________|

3333  CONTINUE

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 4.3  |                  Final simulation time                 |
!     |______|________________________________________________________|

      print*,'    ___________________________________________________ '
      print*,'   |                                                   |'
      print*,'   |                   FINALIZATION                    |'
      print*,'   |___________________________________________________|'
      print*,'                                                        '


!     -------------------------------------------------------
!     Total number of iterations for the WL
      WLIterTime = 1.0d0*CountWLIterations/NT
      print*,'      Total number of WL iterations  =',CountWLIterations
      print*,'      Average WL iters for time step =',WLIterTime
      print*,'  '

!     -------------------------------------------------------
!     Calculating the simulation time
      !tcpu  = etime(tti)
      tcpu  = CPU_TimeStep
      idhr  = tcpu/3600
      idmin = tcpu/60-idhr*60
      idsec = tcpu-(idhr*3600+idmin*60)

!     -------------------------------------------------------
!     Display elapsed time (CPU)
      print*,'     -------------------------------------------------- '
      print*,'                      Simulation Time                   '
      print*,'     -------------------------------------------------- '
      print*,'                                                        '
917   format(t16,'Elapsed time:',f10.3,' sec CPU(',i2,':',i2,':',i2,')')
      write(*,917) tcpu,idhr,idmin,idsec
      print*,'  '
      print*,'     -------------------------------------------------- '
      print*,'  '

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 4.4  |                 Deallocate variables                   |
!     |______|________________________________________________________|

!     ------------------
!     Variables
      deallocate(WL,WLN,WLV,WLVN)
      deallocate(DEWL,DEWLN,DEWLV)
      deallocate(H,HN,HV,HVN,HW,HWV)
      deallocate(U,V,UN,VN,UVERT,VVERT)
      deallocate(QX,QY,QXN,QYN,QXV,QYV)
!     ------------------
!     Auxiliar variables
      deallocate(AH,CHEZY)
      deallocate(dHWdx,dHWdy)
      deallocate(dWLNdx,dWLNdy,dWLdx,dWLdy)
      deallocate(C0,HW0M,QC0,QC0N)
!     ------------------
!     Geometry
      deallocate(XC,YC,X,Y)
      deallocate(AE,DXC2,DYC2,DXY)
      deallocate(X0M,Y0M)
      deallocate(DXV,DYV,DXV2,DYV2)
      deallocate(DISDIF,A0M,DXC,DYC)
      deallocate(normalX,normalY)
!     ------------------
!     Index and tags
      deallocate(NBE,NBE_V)
      deallocate(NO_WB,NO_QB,NO_HB,NO_SB)
      deallocate(NO_VP,NO_CP)
      deallocate(tagBC,tagBCv)
!     ------------------
!     MODULE GEOEMETRY
      call deallocate_geometry

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 4.5  |                 Closing programs                       |
!     |______|________________________________________________________|

      print*,'                                                        '
      print*,'wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww'
      print*,'--------------------------------------------------------'
      print*,'           ***  END SEQUENTIAL PROGRAM  ***             '
      print*,'--------------------------------------------------------'
      print*,'wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww'
      print*,'                                                        '
      print*,'                                                        '

      !RETURN
      !END
      
      END SUBROUTINE finalization

      END MODULE mained
      
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!---------------------------------------------------------------------!
!                            END OF UFVM                              !
!---------------------------------------------------------------------!
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
