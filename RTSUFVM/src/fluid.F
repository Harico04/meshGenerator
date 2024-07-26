!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!---------------------------------------------------------------------!
!                                                                     !
!                        ***  SUBROUTINES  ****                       !
!                                                                     !
!---------------------------------------------------------------------!
!                                                                     !
!                         SUBROUTINE: BC_WL                           !
!                         SUBROUTINE: BC_Fluid                        !
!                         SUBROUTINE: BC_Fluid_Individual             !
!                         SUBROUTINE: CALCO                           !
!                         SUBROUTINE: PredictorQ                      !
!                         SUBROUTINE: CALFLUX                         !
!                         SUBROUTINE: PoissonWL                       !
!                         SUBROUTINE: CorrectorQ                      !
!                         SUBROUTINE: DispersionAH                    !
!                                                                     !
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww! 

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                         SUBROUTINE: BC_WL                           !
!                 Boundary conditions of Water Level                  !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE BC_WL(DEWL,DEWLV,WLN,WLVN,HW,HWV,
     *                 H,HV,WL,WLV,
     *                 NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V)

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
      REAL*8,DIMENSION(:) :: DEWL(N_CELL),DEWLV(N_VERT)
      REAL*8,DIMENSION(:) :: WLN(N_CELL),WLVN(N_VERT)
      REAL*8,DIMENSION(:) :: HW(N_CELL),HWV(N_VERT)
!     --------
      REAL*8,DIMENSION(:) :: H(N_CELL),HV(N_VERT)
      REAL*8,DIMENSION(:) :: WL(N_CELL),WLV(N_VERT)
!     --------
      INTEGER,DIMENSION(:,:):: NO_VP(N_CELL,3),NO_CP(N_CELL,3)
      INTEGER,DIMENSION(:)  :: NBE(N_CELL),NBE_V(N_VERT)
      INTEGER,DIMENSION(:)  :: NO_WB(N_WBMAX)
      INTEGER,DIMENSION(:)  :: NO_HB(N_HBMAX)
      INTEGER,DIMENSION(:)  :: NO_QB(N_QBMAX)
      INTEGER,DIMENSION(:)  :: NO_SB(N_SBMAX)

!      ________________________________________________________
!     |                                                        |
!     |                   BC at VERTEX POINTS                  |
!     |________________________________________________________|

!     ________________________________________________
!     WATER LEVEL (OUTFLOW) BOUNDARY TYPE: WL=Exact
      IF (N_HB.GE.1) THEN
         WLVN(NO_HB(1:N_HB)) = WLO
          HWV(NO_HB(1:N_HB)) = HWO
      ENDIF
!     ________________________________________________
!     STRUCTURE BOUNDARY TYPE: WL=0
      !IF (N_SB.GE.1) THEN
      !   WLVN(NO_SB(1:N_SB)) = 0.0d0
      !    HWV(NO_SB(1:N_SB)) = 0.0d0
      !  DEWLV(NO_SB(1:N_SB)) = 0.0d0
      !ENDIF
!      ________________________________________________________
!     |                                                        |
!     |                   BC at CELL-CENTERS                   |
!     |________________________________________________________|

      DO MK = N_CELL0+1,N_CELLexact
         NC = NO_CP(MK,1)
!        ________________________________________________
!        WALL BOUNDARY TYPE: d(WL)/dn = 0
         IF (NBE(NC).EQ.1) THEN
            WLN(MK) = WLN(NC)
            HW(MK)  = HW(NC)
            DEWL(MK)= DEWL(NC)
!        ________________________________________________
!        DISCHARGE (INFLOW) BOUNDARY TYPE: d(WL)/dn = 0
         ELSEIF (NBE(NC).EQ.2) THEN
            WLN(MK) = WLN(NC)
            HW(MK)  = HW(NC)
            DEWL(MK)= DEWL(NC)
!        ________________________________________________
!        WATER LEVEL (OUTFLOW) BOUNDARY TYPE: WL=Exact
         ELSEIF (NBE(NC).EQ.3) THEN
            WLN(MK) = 2.0d0*WLO - WLN(NC)
            HW(MK)  = WLN(MK) + H(MK)
            DEWL(MK)= WLN(MK) - WL(MK)
!        ________________________________________________
!        STRUCTURE BOUNDARY TYPE: d(WL)/dn = 0
         ELSEIF (NBE(NC).EQ.6) THEN
            WLN(MK) = WLN(NC)
            HW(MK)  = HW(NC)
            DEWL(MK)= DEWL(NC)
         ENDIF
      ENDDO

      RETURN
      END

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                         SUBROUTINE: BC_Fluid                        !
!                 Boundary conditions of Water Level                  !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE BC_Fluid(UN,UVERT,VN,VVERT,QXN,QXV,QYN,QYV,
     *                    HW,NO_CP,NO_WB,NO_QB,NO_HB,NO_SB,NBE)

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

      REAL*8,DIMENSION(:) :: UN(N_CELL),UVERT(N_VERT)
      REAL*8,DIMENSION(:) :: VN(N_CELL),VVERT(N_VERT)
      REAL*8,DIMENSION(:) :: QXN(N_CELL),QXV(N_VERT)
      REAL*8,DIMENSION(:) :: QYN(N_CELL),QYV(N_VERT)
!     --------
      REAL*8,DIMENSION(:) :: HW(N_CELL)
!     --------
      INTEGER,DIMENSION(:,:):: NO_CP(N_CELL,3)
      INTEGER,DIMENSION(:)  :: NBE(N_CELL)
      INTEGER,DIMENSION(:)  :: NO_WB(N_WBMAX)
      INTEGER,DIMENSION(:)  :: NO_QB(N_QBMAX)
      INTEGER,DIMENSION(:)  :: NO_HB(N_HBMAX)
      INTEGER,DIMENSION(:)  :: NO_SB(N_SBMAX)

!      ________________________________________________________
!     |                                                        |
!     |                   BC at VERTEX POINTS                  |
!     |________________________________________________________|

!     ________________________________________________
!     WALL BOUNDARY TYPE: No-Slip & Slip
      IF (N_WB.GE.1) THEN
#        ifdef KeyNoSlip
           QXV(NO_WB(1:N_WB)) = 0.0d0
         UVERT(NO_WB(1:N_WB)) = 0.0d0
#        endif
           QYV(NO_WB(1:N_WB)) = 0.0d0
         VVERT(NO_WB(1:N_WB)) = 0.0d0
      ENDIF
!     ________________________________________________
!     DISCHARGE (INFLOW) BOUNDARY TYPE: (QXV,QYV)= Exact
      IF (N_QB.GE.1) THEN
           QXV(NO_QB(1:N_QB)) = QXO
           QYV(NO_QB(1:N_QB)) = QYO
         UVERT(NO_QB(1:N_QB)) = QXO/HWO
         VVERT(NO_QB(1:N_QB)) = QYO/HWO
      ENDIF
!     ________________________________________________
!     STRUCTURE BOUNDARY TYPE: (QXV,QYV)=(0,0)
      IF (N_SB.GE.1) THEN
           QXV(NO_SB(1:N_SB)) = 0.0d0
           QYV(NO_SB(1:N_SB)) = 0.0d0
         UVERT(NO_SB(1:N_SB)) = 0.0d0
         VVERT(NO_SB(1:N_SB)) = 0.0d0
      ENDIF
!      ________________________________________________________
!     |                                                        |
!     |                   BC at CELL-CENTERS                   |
!     |________________________________________________________|

      DO MK = N_CELL0+1,N_CELLexact
         NC = NO_CP(MK,1)
!        ________________________________________________
!        WALL BOUNDARY TYPE: d(Q)/dn = 0
         IF (NBE(NC).EQ.1) THEN
#           ifdef KeySlip
            QXN(MK) = QXN(NC)   ! d(QXN)/dn = 0
            UN(MK)  = UN(NC)    ! d(UN)/dn = 0
#           endif
#           ifdef KeyNoSlip
            QXN(MK) = -QXN(NC)  ! QXN = 0
            UN(MK)  = -UN(NC)   ! UN  = 0
#           endif
            QYN(MK) = -QYN(NC)  ! QYN = 0
            VN(MK)  = -VN(NC)   ! VN  = 0
!        ________________________________________________
!        DISCHARGE (INFLOW) BOUNDARY TYPE: Q = Exact
         ELSEIF (NBE(NC).EQ.2) THEN
            QXN(MK) = 2.0d0*QXO - QXN(NC)
            QYN(MK) = 2.0d0*QYO - QYN(NC)
            UN(MK)  = QXN(MK)/HW(MK)
            VN(MK)  = QYN(MK)/HW(MK)
!        ________________________________________________
!        WATER LEVEL (OUTFLOW) BOUNDARY TYPE: d(Q)/dn = 0
         ELSEIF (NBE(NC).EQ.3) THEN
            QXN(MK) = QXN(NC)
            QYN(MK) = QYN(NC)
            UN(MK)  = UN(NC)
            VN(MK)  = VN(NC)
!        ________________________________________________
!        STRUCTURE BOUNDARY TYPE: Q = 0
         ELSEIF (NBE(NC).EQ.6) THEN
            QXN(MK) = -QXN(NC)
            QYN(MK) = -QYN(NC)
            UN(MK)  = -UN(NC)
            VN(MK)  = -VN(NC)
         ENDIF
      ENDDO

      RETURN
      END

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                   SUBROUTINE: BC_Fluid_Individual                   !
!                 Boundary conditions of Water Level                  !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE BC_Fluid_Individual(FC,FV,TagQ,ExactQ,
     *           NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V)

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

      REAL*8,DIMENSION (:) :: FC(N_CELL)
      REAL*8,DIMENSION (:) :: FV(N_VERT)
      INTEGER :: TagQ
      REAL*8  :: ExactQ
      INTEGER,DIMENSION(:,:):: NO_VP(N_CELL,3),NO_CP(N_CELL,3)
      INTEGER,DIMENSION(:)  :: NBE(N_CELL),NBE_V(N_VERT)
      INTEGER,DIMENSION(:)  :: NO_WB(N_WBMAX)
      INTEGER,DIMENSION(:)  :: NO_HB(N_HBMAX)
      INTEGER,DIMENSION(:)  :: NO_QB(N_QBMAX)
      INTEGER,DIMENSION(:)  :: NO_SB(N_SBMAX)

      IF (TagQ.LT.0.d0) GOTO 1000
!      ________________________________________________________
!     |                                                        |
!     |                   BC at CELL-CENTERS                   |
!     |________________________________________________________|

      DO MK = N_CELL0+1,N_CELLexact
         NC = NO_CP(MK,1)
!        ________________________________________________
!        WALL BOUNDARY TYPE
         IF (NBE(NC).EQ.1) THEN
#           ifdef KeySlip
               if (TagQ.EQ.1) FC(MK) = FC(NC) ! dQX/dn=0
               if (TagQ.EQ.2) FC(MK) =-FC(NC) ! QY=0
#           endif
#           ifdef KeyNoSlip
               FC(MK) = -FC(NC) ! FC=0 
#           endif
!        ________________________________________________
!        DISCHARGE (INFLOW) BOUNDARY TYPE: FC=ExactQ
         ELSEIF (NBE(NC).EQ.2) THEN
            FC(MK) = 2.0d0*ExactQ-FC(NC)
!        ________________________________________________
!        WATER LEVEL (OUTFLOW) BOUNDARY TYPE: d(FC)/dn = 0
         ELSEIF (NBE(NC).EQ.3) THEN
            FC(MK) = FC(NC)
!        ________________________________________________
!        STRUCTURE BOUNDARY TYPE: FC = 0
         ELSEIF (NBE(NC).EQ.6) THEN
            FC(MK) = -FC(NC)
         ENDIF
      ENDDO

      GOTO 2000

!      ________________________________________________________
!     |                                                        |
!     |                   BC at VERTEX POINTS                  |
!     |________________________________________________________|

1000  CONTINUE

!     ________________________________________________
!     WALL BOUNDARY TYPE: No-Slip & Slip
      IF (N_WB.GE.1) THEN
#        ifdef KeySlip
            if (TagQ.EQ.2)  FV(NO_WB(1:N_WB)) = 0.0d0 ! FV*N=0
#        endif
#        ifdef KeyNoSlip
           FV(NO_WB(1:N_WB)) = 0.0d0 ! FV=0
#        endif
      ENDIF
!     ________________________________________________
!     DISCHARGE (INFLOW) BOUNDARY TYPE: FV=ExactQ
      IF (N_QB.GE.1) THEN
           FV(NO_QB(1:N_QB)) = ExactQ
      ENDIF
!     ________________________________________________
!     STRUCTURE BOUNDARY TYPE: FV=0
      IF (N_SB.GE.1) THEN
           FV(NO_SB(1:N_SB)) = 0.0d0
      ENDIF

2000  CONTINUE

      RETURN
      END

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                         SUBROUTINE: CALCO                           !
!               Calculation of the mass flux components               !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE CALCO(C0,U,V,NO_CP,DXV,DYV)

!     ____________________________________
!    |                                    |
!    |     Keys and common parameters     |
!    |____________________________________|

#     include "cppdefs.h"
!     ====================================
!     =====  START PARALLEL OPTION =======
#     ifdef KeyParallelMPI
        USE parallel
        IMPLICIT NONE
!     ====================================
!     ==========  SEQUENTIAL =============
#     else
        IMPLICIT NONE
        INCLUDE 'common.mpf'
#     endif
!     =============== END ================
!     ====================================
!     ____________________________________
!    |                                    |
!    |      Declaration of variables      |
!    |____________________________________|

      REAL*8,DIMENSION(:,:) :: C0(N_CELL,3)
      REAL*8,DIMENSION(:)   :: U(N_CELL),V(N_CELL)
      INTEGER,DIMENSION(:,:):: NO_CP(N_CELL,3)
      REAL*8,DIMENSION(:,:) :: DXV(N_CELL,3),DYV(N_CELL,3)
!     ____________________________________
!    |                                    |
!    |   Declaration of local variables   |
!    |____________________________________|

      INTEGER:: K
      REAL*8 :: U0,V0
    
!      ________________________________________________________
!     |                                                        |
!     |                       Mass flux C0                     |
!     |________________________________________________________|

      DO NC=1,N_CELL0
         DO K=1,3
            MK=NO_CP(NC,K)
!           ________________________
!           MEAN VALUE
            U0 = 0.5d0*(U(NC)+U(MK))
            V0 = 0.5d0*(V(NC)+V(MK))
!           ________________________
!           Mass fluxes: C0
            C0(NC,K) = U0*DYV(NC,K)-V0*DXV(NC,K)
         ENDDO
      ENDDO

      RETURN
      END

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                    SUBROUTINE: Predictor_Q                          !
!   Solution of the advection-diffusion problem (2ND-ORDER UPWIND)    !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE PredictorQ(FC,FCN,FV,
!    ------------- 
     *  C0,ExactQ,TagQ,
!    ------------- 
     *  QX,QY,QXV,QYV,
!    -------------
     *  NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V,
!    -------------     
     *  X,Y,XC,YC,AE,DXV,DYV,DXV2,DYV2,DXC,DYC,
     *  DXC2,DYC2,DXY,
!    -------------
     *  X0M,Y0M,DISDIF,
     *  A0M,AH)

!*********************************************************************!
!                                                                     !
!                        Definition of variables                      !
!                                                                     !
!*********************************************************************! 
!     ____________________________________
!    |                                    |
!    |     Keys and common parameters     |
!    |____________________________________|

#     include "cppdefs.h"
!     ====================================
!     =====  START PARALLEL OPTION =======
#     ifdef KeyParallelMPI
        USE parallel
        USE geometry
        IMPLICIT NONE
!     ====================================
!     ==========  SEQUENTIAL =============
#     else
        USE geometry
        IMPLICIT NONE
        INCLUDE 'common.mpf'
#     endif
!     =============== END ================
!     ====================================
!     ____________________________________
!    |                                    |
!    |      Declaration of variables      |
!    |____________________________________|

      REAL*8,DIMENSION (:) :: FC(N_CELL),FCN(N_CELL),FV(N_VERT)
!     --------
      REAL*8,DIMENSION (:,:):: C0(N_CELL,3)
      REAL*8 :: ExactQ
      INTEGER:: TagQ
!     --------
      REAL*8,DIMENSION (:) :: QX(N_CELL)
      REAL*8,DIMENSION (:) :: QY(N_CELL)
      REAL*8,DIMENSION (:) :: QXV(N_VERT)
      REAL*8,DIMENSION (:) :: QYV(N_VERT)
!     --------
      INTEGER,DIMENSION(:,:):: NO_VP(N_CELL,3),NO_CP(N_CELL,3)
      INTEGER,DIMENSION(:)  :: NBE(N_CELL),NBE_V(N_VERT)
      INTEGER,DIMENSION(:)  :: NO_WB(N_WBMAX)
      INTEGER,DIMENSION(:)  :: NO_HB(N_HBMAX)
      INTEGER,DIMENSION(:)  :: NO_QB(N_QBMAX)
      INTEGER,DIMENSION(:)  :: NO_SB(N_SBMAX)
!     --------
      REAL*8,DIMENSION(:)  :: XC(N_CELL),YC(N_CELL)
      REAL*8,DIMENSION(:)  :: X(N_VERT),Y(N_VERT)
      REAL*8,DIMENSION(:)  :: AE(N_CELL)
      REAL*8,DIMENSION(:,:):: DXV(N_CELL,3),DYV(N_CELL,3)
      REAL*8,DIMENSION(:,:):: DXV2(N_CELL,3),DYV2(N_CELL,3)
      REAL*8,DIMENSION(:,:):: DXC(N_CELL,3),DYC(N_CELL,3)
      REAL*8,DIMENSION(:,:):: X0M(N_CELL,3),Y0M(N_CELL,3)
      REAL*8,DIMENSION(:)  :: DXC2(N_CELL)
      REAL*8,DIMENSION(:)  :: DYC2(N_CELL)
      REAL*8,DIMENSION(:)  :: DXY(N_CELL)
!     --------
      REAL*8,DIMENSION(:,:):: DISDIF(N_CELL,3)
      REAL*8,DIMENSION(:,:):: A0M(N_CELL,3)
      REAL*8,DIMENSION(:)  :: AH(N_CELL)

!     ____________________________________
!    |                                    |
!    |   Declaration of local variables   |
!    |____________________________________|

      real*8,dimension(:)  :: Am0(N_CELL0)
      real*8,dimension(:)  :: Am1(N_CELL0)
      real*8,dimension(:)  :: Am2(N_CELL0)
      real*8,dimension(:)  :: Am3(N_CELL0)
      real*8,dimension(:)  :: AmG(N_CELL0)
      real*8,dimension(:)  :: RHS(N_CELL0)
!     --------
      real*8,dimension(:,:):: AM(N_CELL0,3)
      real*8,dimension(:)  :: SM(N_CELL0)
      real*8,dimension(:)  :: AP(N_CELL0)
!     --------
      REAL*8,DIMENSION(:)  :: PHI(N_CELL)
      real*8,dimension(:)  :: PHI_OLD(N_CELL)
      real*8,dimension(:)  :: FCN_new(N_CELL)
      real*8,dimension(:)  :: dFCdx(N_CELL)
      real*8,dimension(:)  :: dFCdy(N_CELL)
!     --------
      integer:: K,KK,NKK,NP,MK1,MK2,MK3,elem,nc0,nv0,DOFLUX
!     ---------------
      real*8 :: anorm,Residu,xme,yme,anormGlo
      real*8 :: GF0,GFM,GFpos,GFneg,PHImin,PHIIJ,SMM,DIFF
      real*8 :: AMIN,AMAX,A1,A2,PHIpos,PHIneg,C0pos,C0neg,AMM
      real*8 :: AH0M,GF
!     ---------------
      real*8  :: alfa,beta,A1Mod,A2Mod,suma     
!     --------------- 
      real*8 :: rx0,ry0,rxM,ryM
      real*8 :: DE,DENO
      real*8 :: n_x,n_y,na
!     ---------------
      real*8 :: IMx,IMy,OIx,OIy,JIx,JIy
      real*8 :: GF0u,GFMu,GF0v,GFMv
      real*8 :: alphIJ,alphIJC,distOI,distJI
      real*8 :: EPS
!     ---------------
      real*8 :: DXvv,DYvv,DXcc,DYcc
!     ---------------
      real*8 :: a0,aj,r0 
!     ____________________________________
!    |                                    |
!    |           Local parameters         |
!    |____________________________________|

      integer, parameter :: MaxIter = 2000
      real*8,  parameter :: Toler   = 1.0e-5
      real*8,  parameter :: RelaxOmegaQ  = 1.0d0

!*********************************************************************!
!                                                                     !
!                           INITIALIZATION                            !
!                                                                     !
!*********************************************************************!

       FCN = FC
!      ________________________________________________________
!     |                                                        |
!     |              Gradient of FC: dFCdx,dFCdy               |
!     |________________________________________________________|

      CALL GRADF(NBE,NO_CP,DXC,DYC,FC,DXC2,DYC2,DXY,dFCdx,dFCdy)

!*********************************************************************!
!                                                                     !
!                            Flux limiters                            !
!                                                                     !
!*********************************************************************!

      DOFLUX = 0
      PHI = 1.0D0 
!     __________________________________________________
!     Upwind first-order
#     ifdef KeyUpwindFirst
         PHI = 0.0D0
#     endif

!*********************************************************************!
!                                                                     !
!                   LINEAR SYSTEM: ADVECTION-DIFFUSION                !
!                                                                     !
!*********************************************************************!

      AM = 0.0d0
      SM = 0.0d0
      AP = 0.0d0

      DO NC=1,N_CELL0
         DO K=1,3
            MK=NO_CP(NC,K)
!            __________________________________________________
!           |                                                  |
!           |             >>> ADVECTION: UPWIND  <<<           |
!           |__________________________________________________|

               C0pos = 0.5*(C0(NC,K)+DABS(C0(NC,K)))
               C0neg = 0.5*(C0(NC,K)-DABS(C0(NC,K)))
               !-----------
               rx0 = X0M(NC,K)-XC(NC)
               ry0 = Y0M(NC,K)-YC(NC)
               rxM = X0M(NC,K)-XC(MK) 
               ryM = Y0M(NC,K)-YC(MK)
               !-----------
               ! GRADIENT
               GF0 = dFCdx(NC)*rx0 + dFCdy(NC)*ry0
               GFM = dFCdx(MK)*rxM + dFCdy(MK)*ryM
               GF  = PHI(NC)*C0pos*GF0 + PHI(MK)*C0neg*GFM
               !-----------
               ! MATRIX
               AM(NC,K) = -C0neg
               AP(NC)   = AP(NC) + C0pos
               SM(NC)   = SM(NC) - GF
!            __________________________________________________
!           |                                                  |
!           |                     DIFFUSION                    |
!           |__________________________________________________|

#           ifdef KeyDiffusion
               KK=K+1
               IF(KK.GT.3) KK=KK-3
               KV1=NO_VP(NC,K)
               KV2=NO_VP(NC,KK)
               !-----------
               DXvv = DXV(NC,K)
               DYvv = DYV(NC,K)
               DXcc = DXC(NC,K)
               DYcc = DYC(NC,K)
               AH0M = 0.5d0*(AH(NC)+AH(MK))*(0.5d0/A0M(NC,K))
               alfa = AH0M*(DYvv*DYvv + DXvv*DXvv)
               beta = AH0M*(DYcc*DYvv + DXcc*DXvv)
               !-----------
               ! MATRIX
               AM(NC,K) = AM(NC,K) + alfa
               AP(NC)   = AP(NC)   + alfa
               SM(NC)   = SM(NC)   + beta*(FV(KV1)-FV(KV2))
#           endif
         ENDDO
      ENDDO

!     __________________________________________________
!    |                                                  |
!    |                   LINEAR SYSTEM                  |
!    |__________________________________________________|

      Am0 = 0.0d0
      Am1 = 0.0d0
      Am2 = 0.0d0
      Am3 = 0.0d0
      AmG = 0.0d0
      
      DO NC=1,N_CELL0
         Am0(NC) = 1.0d0 + AP(NC)*DT/AE(NC)
         Am1(NC) = -AM(NC,1)*DT/AE(NC)
         Am2(NC) = -AM(NC,2)*DT/AE(NC)
         Am3(NC) = -AM(NC,3)*DT/AE(NC)
         RHS(NC) = FC(NC) + SM(NC)*DT/AE(NC)
      ENDDO
           
!*********************************************************************!
!                                                                     !
!                              SOLUTION                               !
!                                                                     !
!*********************************************************************!

!        ____________________________________
!       |                                    |
!       |               UPDATE               |
!       |____________________________________|

!        ___________________________________
!        PRECONDITIONAL 
         do NC=1,N_CELL0
            AM1(NC) = AM1(NC)/AM0(NC)
            AM2(NC) = AM2(NC)/AM0(NC)
            AM3(NC) = AM3(NC)/AM0(NC)
            RHS(NC) = RHS(NC)/AM0(NC)
            AM0(NC) = 1.0d0
         enddo

!        ___________________________________
!        UPDATE 
         NKK = 0
         
115      NKK = NKK + 1

         anorm = 0.0d0
         DO NC=1,N_CELL0
            MK1=NO_CP(NC,1)
            MK2=NO_CP(NC,2)
            MK3=NO_CP(NC,3)
            Residu = RHS(NC) - (AM0(NC)*FCN(NC) 
     *                         +AM1(NC)*FCN(MK1)
     *                         +AM2(NC)*FCN(MK2)
     *                         +AM3(NC)*FCN(MK3))
            anorm  = anorm + DABS(Residu)
            FCN(NC)= FCN(NC) + RelaxOmegaQ*Residu
         ENDDO
!        ____________________________________
!       |                                    |
!       |          BOUNDARY CONDITION        |
!       |____________________________________|

#       ifdef Key_LoopBoundaryQ
           CALL BC_Fluid_Individual(FCN,FV,TagQ,ExactQ,
     *          NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V)
#       endif
!        ____________________________________
!       |                                    |
!       |            STOP CONDITION          |
!       |____________________________________|


         IF (anorm.LT.Toler) THEN
            IF (DisplayOnceThis.eq.1) THEN
            IF (MOD(NT,ModPrint).EQ.0) THEN
            WRITE(*,*) 'Discharge Q: Iterations =',NKK,anorm
            ENDIF
            ENDIF
            GOTO 410
         ELSEIF (anorm.GT.1e+12) THEN
            IF (DisplayOnceThis.eq.1) THEN
            WRITE(*,*) 'Divergent Q: Iter=',NKK,' anorm=',anorm
            ENDIF
            STOP
         ENDIF
         IF (NKK.LT.MaxIter) GOTO 115
         IF (DisplayOnceThis.eq.1) THEN
         WRITE(*,*) 'Non Convergent discharge: Iterations=',NKK,anorm
         ENDIF

410   CONTINUE

!*********************************************************************!
!                                                                     !
!                              FINALIZATION                           !
!                                                                     !
!*********************************************************************!

!       __________________________________________________
!      |                                                  |
!      |       Calculate FV (VERTEX) by Interpolation     |
!      |__________________________________________________|

       CALL INTERPO_Q(FCN,FV,NBE_V,tagQ)

      RETURN
      END

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                        SUBROUTINE: CALFLUX                          !
!            Calculation of the advection flux coefficients           !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE CALFLUX(QC0,HW0M,
!           --------
     *      HW,HWV,C0,
     *      dHWdx,dHWdy,
!           --------
     *      NBE,NO_CP,NO_VP,
     *      X,Y,XC,YC,AE,DXV,DYV,DXC,DYC,
     *      X0M,Y0M)

!     ____________________________________
!    |                                    |
!    |     Keys and common parameters     |
!    |____________________________________|

#     include "cppdefs.h"
!     ====================================
!     =====  START PARALLEL OPTION =======
#     ifdef KeyParallelMPI
        USE parallel
        IMPLICIT NONE
!     ====================================
!     ==========  SEQUENTIAL =============
#     else
        IMPLICIT NONE
        INCLUDE 'common.mpf'
#     endif
!     =============== END ================
!     ====================================
!     ____________________________________
!    |                                    |
!    |      Declaration of variables      |
!    |____________________________________|

      REAL*8,DIMENSION(:,:):: QC0(N_CELL,3)
      REAL*8,DIMENSION(:,:):: HW0M(N_CELL,3)
!     -------
      REAL*8,DIMENSION(:)  :: HW(N_CELL),HWV(N_VERT)
      REAL*8,DIMENSION(:,:):: C0(N_CELL,3)
      REAL*8,DIMENSION(:)  :: dHWdx(N_CELL),dHWdy(N_CELL)
!     -------
      INTEGER,DIMENSION(:)  :: NBE(N_CELL)
      INTEGER,DIMENSION(:,:):: NO_VP(N_CELL,3)
      INTEGER,DIMENSION(:,:):: NO_CP(N_CELL,3)
!     -------
      REAL*8,DIMENSION(:)  :: X(N_VERT),Y(N_VERT)
      REAL*8,DIMENSION(:)  :: XC(N_CELL),YC(N_CELL)
      REAL*8,DIMENSION(:)  :: AE(N_CELL)
      REAL*8,DIMENSION(:,:):: DXV(N_CELL,3),DYV(N_CELL,3)
      REAL*8,DIMENSION(:,:):: DXC(N_CELL,3),DYC(N_CELL,3)
!     -------
      REAL*8,DIMENSION(:,:):: X0M(N_CELL,3),Y0M(N_CELL,3)

!     ____________________________________
!    |                                    |
!    |   Declaration of local variables   |
!    |____________________________________|

!     -------
      INTEGER:: K,KK
      REAL*8 :: GF0,GFM,GF,GFpos,GFneg
      REAL*8 :: AMIN,AMAX,PHImin,PHIIJ,PHIpos,PHIneg
      REAL*8 :: C0pos,C0neg
      REAL*8 :: rx0,ry0,rxM,ryM
!     -------
      REAL*8 :: IMx,IMy,OIx,OIy,JIx,JIy
      REAL*8 :: GF0u,GFMu,GF0v,GFMv
      REAL*8 :: alphIJ,alphIJC,distOI,distJI,distVI,distVM
      REAL*8 :: distVV,distIM
      REAL*8 :: HW_Up,HW_Ce
      REAL*8 :: som,somGlo
!     -------
      REAL*8,DIMENSION (:) :: PHI(N_CELL)

!      __________________________________________________
!     |                                                  |
!     |                    Flux limiter                  |
!     |__________________________________________________|

      PHI = 1.0d0
!     __________________________________________________
!     Upwind first-order
#     ifdef KeyUpwindFirst
         PHI = 0.0D0
#     endif
!      __________________________________________________
!     |                                                  |
!     |       Advection fluxes HW: UPWIND scheme         |
!     |__________________________________________________|

         DO NC=1,N_CELL0
            DO K=1,3
               MK=NO_CP(NC,K)
!              -----------
               rx0 = X0M(NC,K)-XC(NC)
               ry0 = Y0M(NC,K)-YC(NC)
               rxM = X0M(NC,K)-XC(MK) 
               ryM = Y0M(NC,K)-YC(MK)
!              -----------
               GF0 = dHWdx(NC)*rx0 + dHWdy(NC)*ry0
               GFM = dHWdx(MK)*rxM + dHWdy(MK)*ryM 
!              -------------
               C0pos=0.5d0*(C0(NC,K)+DABS(C0(NC,K)))
               C0neg=0.5d0*(C0(NC,K)-DABS(C0(NC,K)))
!              ------------- 
               IF (C0(NC,K).GT.0.) THEN
                  HW0M(NC,K)=HW(NC)+PHI(NC)*GF0
               ELSE
                  HW0M(NC,K)=HW(MK)+PHI(MK)*GFM
               ENDIF
               QC0(NC,K)= C0(NC,K)*HW0M(NC,K)
            ENDDO
         ENDDO

      RETURN
      END

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                        SUBROUTINE: PoissonWL                        !
!        Calculation of the Poisson equation for delta=WL^(n+1)-WL    !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE PoissonWL(DEWL,DEWLV,WLN,WLVN,HW,HWV,
!    -------------
     *  WL,WLV,H,HV,
     *  U,V,UN,VN,UVERT,VVERT,QX,QY,QXN,QYN,QXV,QYV,
!    -------------
     *  C0,QC0,QC0N,HW0M,
!    -------------
     *  dWLdx,dWLdy,
     *  dHWdx,dHWdy,
!    -------------
     *  NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V,
!    -------------
     *  X,Y,XC,YC,AE,
     *  DXV,DYV,DXV2,DYV2,DXC,DYC,
     *  DXC2,DYC2,DXY,
!    -------------
     *  X0M,Y0M,DISDIF,
     *  A0M,AH,CHEZY)

!*********************************************************************!
!                                                                     !
!                        Definition of variables                      !
!                                                                     !
!*********************************************************************! 

!     ____________________________________
!    |                                    |
!    |     Keys and common parameters     |
!    |____________________________________|

#     include "cppdefs.h"
!     ====================================
!     =====  START PARALLEL OPTION =======
#     ifdef KeyParallelMPI
        USE parallel
        IMPLICIT NONE
!     ====================================
!     ==========  SEQUENTIAL =============
#     else
        IMPLICIT NONE
        INCLUDE 'common.mpf'
#     endif
!     =============== END ================
!     ====================================
!     ____________________________________
!    |                                    |
!    |      Declaration of variables      |
!    |____________________________________|

      REAL*8,DIMENSION(:) :: DEWL(N_CELL),DEWLV(N_VERT)
      REAL*8,DIMENSION(:) :: WLN(N_CELL),WLVN(N_VERT)
      REAL*8,DIMENSION(:) :: HW(N_CELL),HWV(N_VERT)
!     --------
      REAL*8,DIMENSION(:) :: WL(N_CELL),WLV(N_VERT)
      REAL*8,DIMENSION(:) :: H(N_CELL),HV(N_VERT)
      REAL*8,DIMENSION(:) :: U(N_CELL),V(N_CELL)
      REAL*8,DIMENSION(:) :: UN(N_CELL),VN(N_CELL)
      REAL*8,DIMENSION(:) :: UVERT(N_VERT),VVERT(N_VERT)
      REAL*8,DIMENSION(:) :: QX(N_CELL),QY(N_CELL)
      REAL*8,DIMENSION(:) :: QXN(N_CELL),QYN(N_CELL)
      REAL*8,DIMENSION(:) :: QXV(N_VERT),QYV(N_VERT)
!     --------
      REAL*8,DIMENSION(:,:):: C0(N_CELL,3)
      REAL*8,DIMENSION(:,:):: QC0(N_CELL,3)
      REAL*8,DIMENSION(:,:):: QC0N(N_CELL,3)
      REAL*8,DIMENSION(:,:):: HW0M(N_CELL,3)
!     --------
      REAL*8,DIMENSION(:)  :: dWLdx(N_CELL),dWLdy(N_CELL)
      REAL*8,DIMENSION(:)  :: dHWdx(N_CELL),dHWdy(N_CELL)
!     --------
      INTEGER,DIMENSION(:,:):: NO_VP(N_CELL,3),NO_CP(N_CELL,3)
      INTEGER,DIMENSION(:)  :: NBE(N_CELL),NBE_V(N_VERT)
      INTEGER,DIMENSION(:)  :: NO_WB(N_WBMAX)
      INTEGER,DIMENSION(:)  :: NO_HB(N_HBMAX)
      INTEGER,DIMENSION(:)  :: NO_QB(N_QBMAX)
      INTEGER,DIMENSION(:)  :: NO_SB(N_SBMAX)
!     --------
      REAL*8,DIMENSION(:)  :: XC(N_CELL),YC(N_CELL)
      REAL*8,DIMENSION(:)  :: X(N_VERT),Y(N_VERT)
      REAL*8,DIMENSION(:)  :: AE(N_CELL)
      REAL*8,DIMENSION(:,:):: DXV(N_CELL,3),DYV(N_CELL,3)
      REAL*8,DIMENSION(:,:):: DXV2(N_CELL,3),DYV2(N_CELL,3)
      REAL*8,DIMENSION(:,:):: DXC(N_CELL,3),DYC(N_CELL,3)
      REAL*8,DIMENSION(:,:):: X0M(N_CELL,3),Y0M(N_CELL,3)
      REAL*8,DIMENSION(:)  :: DXC2(N_CELL)
      REAL*8,DIMENSION(:)  :: DYC2(N_CELL)
      REAL*8,DIMENSION(:)  :: DXY(N_CELL)
!     --------
      REAL*8,DIMENSION(:,:):: DISDIF(N_CELL,3)
      REAL*8,DIMENSION(:,:):: A0M(N_CELL,3)
      REAL*8,DIMENSION(:)  :: AH(N_CELL)
      REAL*8,DIMENSION(:)  :: CHEZY(N_CELL)
!     ____________________________________
!    |                                    |
!    |   Declaration of local variables   |
!    |____________________________________|

!     --------------------------------
      real*8, dimension(:,:):: AM(N_CELL0,3),BM(N_CELL0,3)
      real*8, dimension(:)  :: AP(N_CELL0),SM(N_CELL)
      real*8, dimension(:)  :: PHIHW(N_CELL)
      real*8, dimension(:)  :: DEWL_new(N_CELL)
!     --------------------------------
      real*8, dimension(:)  :: FX0(N_CELL),FY0(N_CELL)
      real*8, dimension(:)  :: F1X0(N_CELL),F1Y0(N_CELL)
      real*8, dimension(:)  :: FHX(N_CELL),FHY(N_CELL)
      real*8, dimension(:)  :: dDEWLdx(N_CELL),dDEWLdy(N_CELL)
!     --------------------------------
      real*8, dimension(N_CELL0) :: AM0,AM1,AM2,AM3,AMG
      real*8, dimension(N_CELL0) :: BM1,BM2,BM3
      real*8, dimension(N_CELL0) :: RHS
!     --------------------------------
      real*8, dimension(:)  :: DL6(N_VERT)
!     -------------------------------- 
      integer :: MK1,MK2,MK3,IDRY,IBED,K,KK,NKK,i
      real*8  :: anorm,anormGlo
      real*8  :: Residu,CHEZ2,H2,FRIC,A0,G0,P0,SMM,SMK,GAMA2
      real*8  :: SMK0,SMK1,SMK2,SMK3,D0,E0,error,SUMA,AMM
!     --------------------------------
      real*8, dimension(:) :: alfa(3)
      real*8, dimension(:) :: beta(3)
      real*8  :: AR,Dxcc,Dycc,Dxvv,Dyvv,res,HH,QQN,QX2,QY2
      real*8  :: LapWL,DivQ,DivQN,con,con1,con2
!     --------------------------------
      real*8 :: GF0,GFM,GF,GFpos,GFneg
      real*8 :: C0pos,C0neg
      real*8 :: rx0,ry0,rxM,ryM
!     --------------------------------
      integer :: nc1,nc2,nc3,jv1,jv2,jv3
!     --------------------------------
      real*8 :: xvR1,yvR1
      real*8 :: DR0,DR,etaMax
      real*8 :: som,somGlo
!     ____________________________________
!    |                                    |
!    |          Local parameters          |
!    |____________________________________|

      integer, parameter :: UpdateVertex = 1
      integer, parameter :: MaxIter = 500
      real*8,  parameter :: Toler   = 1.0e-5
      real*8,  parameter :: RelaxOmegaWL = 1.0d0
      
!*********************************************************************!
!                                                                     !
!                           INITIALIZATION                            !
!                                                                     !
!*********************************************************************!
!     __________________________________________________
!    |                                                  |
!    |                   Interpolation                  |
!    |__________________________________________________|

      CALL INTERPO_WL(DEWL,DEWLV,NBE_V) 

!*********************************************************************!
!                                                                     !
!           LINEAR SYSTEM: Matriz A,  Matriz B  & RHS                 !
!                                                                     !
!*********************************************************************!

      DO NC=1,N_CELL0
!        -------------------
!        Friction (F)
#        ifdef KeyFriccion
            CHEZ2 = CHEZY(NC)
            H2    = HW(NC)*HW(NC)
            QX2   = QX(NC)*QX(NC)
            QY2   = QY(NC)*QY(NC)
            FRIC  = GRA/(CHEZ2*H2)*DSQRT(QX2+QY2)
#        else
            FRIC = 0.0d0
#        endif
!        -------------------
         A0 = (1/DT+GAMA*FRIC)/DT
!        -------------------
         SMM = 0.0d0
         DO K=1,3
            !-----------
            MK=NO_CP(NC,K)
            KK=K+1
            IF(KK.GT.3) KK=KK-3
            KV1 = NO_VP(NC,K)
            KV2 = NO_VP(NC,KK)
            !----------
            alfa(K) = (0.5d0/A0M(NC,K))*(DXV2(NC,K)+DYV2(NC,K))
            beta(K) = (0.5d0/A0M(NC,K))*DISDIF(NC,K)
            !------------
            HH  = HW0M(NC,K)  !<--- Chose at calflux
            QQN = QC0N(NC,K)  !<--- Chose at calflux
            !------------            
            D0 = (GAMA*GAMA)*GRA*HH/A0
            AM(NC,K) = D0*alfa(K)
            BM(NC,K) = D0*beta(K)
            !-------------
            E0 = GAMA*GRA*HH
            SMK1 = E0*alfa(K)*(WL(MK)-WL(NC))
     *            +E0*beta(K)*(WLV(KV1)-WLV(KV2))
            !-------------
            SMK2 = -(GAMA*QQN+(1.0d0-GAMA)*QC0(NC,K))/DT
            !-----------
            SMM  = SMM + (SMK1+SMK2)/A0 
         ENDDO
!        _________________________________________________
!        FINAL COEFFICIENTS
!        -------------------
!        Matrix coefficients
         AM0(NC) = AE(NC)+(AM(NC,1)+AM(NC,2)+AM(NC,3))
         AM1(NC) = -AM(NC,1)
         AM2(NC) = -AM(NC,2)
         AM3(NC) = -AM(NC,3)
         BM1(NC) = BM(NC,3)-BM(NC,1)
         BM2(NC) = BM(NC,1)-BM(NC,2)
         BM3(NC) = BM(NC,2)-BM(NC,3)
!        -------------------
!        Right-hand side
         RHS(NC) = SMM 
      ENDDO

!     ====================================
!     =====  START PARALLEL OPTION =======
#     ifdef KeyParallelMPI
         call communication2D(RHS)
#     endif
!     =============== END ================
!     ====================================
      
!*********************************************************************!
!                                                                     !
!              SOLUTION BY THE S.O.R. ITERATIVE METHOD:               !
!                                                                     !
!*********************************************************************!

!     __________________________________________________
!    |                                                  |
!    |        Update IMPLICIT: NBE=0,1,2,3,4,5          |
!    |__________________________________________________|

!     ___________________________________
!     Division over the diagonal element 
      do nc=1,N_CELL0
        AM1(nc) = AM1(nc)/AM0(nc)
        AM2(nc) = AM2(nc)/AM0(nc)
        AM3(nc) = AM3(nc)/AM0(nc)
        BM1(nc) = BM1(nc)/AM0(nc)
        BM2(nc) = BM2(nc)/AM0(nc)
        BM3(nc) = BM3(nc)/AM0(nc)
        RHS(nc) = RHS(nc)/AM0(nc)
        AM0(nc) = 1.0d0
      enddo

      NKK=0

 115  CONTINUE
 
      NKK=NKK+1
!     ___________________________________
!     New function calculation 
      anorm=0.0d0
      do nc=1,N_CELL0
         !IF (NBE(NC).NE.6) THEN  ! <<-- It is important!!
            nc1 = No_cp(nc,1)
            nc2 = No_cp(nc,2)
            nc3 = No_cp(nc,3)
            jv1 = No_vp(nc,1)
            jv2 = No_vp(nc,2)
            jv3 = No_vp(nc,3)
            Residu = RHS(nc) - (AM0(nc)*DEWL(nc)
     *                         +AM1(nc)*DEWL(nc1)
     *                         +AM2(nc)*DEWL(nc2)
     *                         +AM3(nc)*DEWL(nc3)
     *                         +BM1(nc)*DEWLV(jv1)
     *                         +BM2(nc)*DEWLV(jv2)
     *                         +BM3(nc)*DEWLV(jv3))
            anorm = anorm + dabs(Residu)
            DEWL(nc) = DEWL(nc) + RelaxOmegaWL*Residu
         !ENDIF
      enddo

!     ___________________________________
!     Interpolation
      IF (UpdateVertex.eq.1) THEN
         CALL INTERPO_WL(DEWL,DEWLV,NBE_V)
      ENDIF
      
!     ___________________________________
!     Boundary conditions (only for DEWL,DEWLV)
      CALL BC_WL(DEWL,DEWLV,WLN,WLVN,HW,HWV,
     *           H,HV,WL,WLV,
     *           NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V)

!     ___________________________________
!     STOP CONDITION
      IF (anorm.LT.Toler) THEN
         IF (MOD(NT,ModPrint).EQ.0) THEN
            WRITE (*,*) 'Water Level: Iterations =',NKK,anorm
            WRITE (*,*) ' '
         ENDIF
         GOTO 410
      ELSEIF (anorm.GT.1e+12) THEN
         WRITE(*,*) 'Divergent WL: Iter=',NKK,' anorm=',anorm
         STOP
      ENDIF
      IF (MOD(NKK,500).EQ.0) Then
         WRITE (*,*) 'Water Level: Iter=',NKK,' anorm=',anorm
      ENDIF
      IF (NKK.LT.MaxIter) GOTO 115
      WRITE(*,*) 'Non Convergent Water Level: Iterations=',NKK


410   CONTINUE

      CountWLIterations = CountWLIterations + NKK
      
!*********************************************************************!
!                                                                     !
!                              FINALIZATION                           !
!                                                                     !
!*********************************************************************!

!     __________________________________________________
!    |                                                  |
!    |     Update final values: WLN, HW, WLVN & HWV     |
!    |__________________________________________________|

      WLN  = WL  + DEWL
      WLVN = WLV + DEWLV
      HW   = H   + WLN
      HWV  = HV  + WLVN
!     __________________________________________________
!    |                                                  |
!    |          Interpolation & boundary condition      |
!    |__________________________________________________|

!     ---------------
      CALL INTERPO_WL(DEWL,DEWLV,NBE_V)
      CALL INTERPO_WL(WLN,WLVN,NBE_V)
      CALL INTERPO_WL(HW,HWV,NBE_V)

!     ---------------
      CALL BC_WL(DEWL,DEWLV,WLN,WLVN,HW,HWV,
     *           H,HV,WL,WLV,
     *           NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V)

!     __________________________________________________
!    |                                                  |
!    |               Minimum depth condition            |
!    |__________________________________________________|
     
      DO NC=1,N_CELL
         HW(NC)  = DMAX1(HW(NC),H00)
      ENDDO
      DO NV=1,N_VERT
         HWV(NV) = DMAX1(HWV(NV),H00)
      ENDDO

      RETURN
      END

!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                        SUBROUTINE: CorrectorQ                       !
!              Correction step of the discharge components            !
!                       Miguel Angel Uh Zapata                        !
!                    Last modification: Jul 2024                      !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE CorrectorQ(UN,UVERT,VN,VVERT,QXN,QXV,QYN,QYV,
!    -------------
     *  U,V,QX,QY,WL,WLV,WLN,WLVN,HW,HWV,H,HV,
     *  FX0,FY0,F1X0,F1Y0,
     *  ExactQXO,ExactQYO,
!    -------------
     *  NO_CP,NO_VP,NO_WB,NO_QB,NO_HB,NO_SB,NBE,NBE_V,
!    -------------     
     *  X,Y,XC,YC,AE,DXV,DYV,DXV2,DYV2,DXC,DYC,
     *  DXC2,DYC2,DXY,
!    -------------
     *  X0M,Y0M,DISDIF,
     *  A0M,AH,CHEZY)
          
!---------------------------------------------------------------------!
!                                                                     !
!       OBJECTIVE: CORRCETION OF THE UNIT DISCHARGE COMPONENTS:       !
!                  UN, VN, UVERT, VVERT, QXN, QYN, QXV, QYV           !
!                                                                     !
!---------------------------------------------------------------------!
!                                                                     !
!   ---> INPUT VARIABLES (PREVIOUS CALCULATIONS):                     !
!                                                                     !
!   WATER LEVEL GRADIENT (OLD STEP):(FX0,FY0)  =[d(WL)/dx,d(WL)/dy]   !
!   WATER LEVEL GRADIENT (NEW STEP):(F1X0,F1Y0)=[d(WL)/dx,d(WL)/dy]   !
!   ExactQXO,ExactQYO = Exact Values at the discharge boundary        !
!                                                                     !
!   ---> INPUT VARIABLES (PREVIOUS CALCULATIONS):                     !
!                                                                     !
!   QC0(N_CELL,3)  = FLUX GRAD*Q^(n)=C0(NC,K)*HW(NC)                  !
!                                                                     !
!---------------------------------------------------------------------!
!     ____________________________________
!    |                                    |
!    |     Keys and common parameters     |
!    |____________________________________|

#     include "cppdefs.h"
!     ====================================
!     =====  START PARALLEL OPTION =======
#     ifdef KeyParallelMPI
        USE parallel
        IMPLICIT NONE
!     ====================================
!     ==========  SEQUENTIAL =============
#     else
        IMPLICIT NONE
        INCLUDE 'common.mpf'
#     endif
!     =============== END ================
!     ====================================
!     ____________________________________
!    |                                    |
!    |      Declaration of variables      |
!    |____________________________________|

      REAL*8,DIMENSION(:) :: UN(N_CELL),UVERT(N_VERT)
      REAL*8,DIMENSION(:) :: VN(N_CELL),VVERT(N_VERT)
      REAL*8,DIMENSION(:) :: QXN(N_CELL),QXV(N_VERT)
      REAL*8,DIMENSION(:) :: QYN(N_CELL),QYV(N_VERT)
!     --------
      REAL*8,DIMENSION(:) :: U(N_CELL)
      REAL*8,DIMENSION(:) :: V(N_CELL)
      REAL*8,DIMENSION(:) :: QX(N_CELL)
      REAL*8,DIMENSION(:) :: QY(N_CELL)
      REAL*8,DIMENSION(:) :: WL(N_CELL),WLV(N_VERT)
      REAL*8,DIMENSION(:) :: WLN(N_CELL),WLVN(N_VERT)
      REAL*8,DIMENSION(:) :: HW(N_CELL),HWV(N_VERT)
      REAL*8,DIMENSION(:) :: H(N_CELL),HV(N_VERT)
!     --------
      REAL*8,DIMENSION(:) :: FX0(N_CELL),FY0(N_CELL)
      REAL*8,DIMENSION(:) :: F1X0(N_CELL),F1Y0(N_CELL)
      REAL*8 :: ExactQXO,ExactQYO
!     --------
      INTEGER,DIMENSION(:,:):: NO_VP(N_CELL,3),NO_CP(N_CELL,3)
      INTEGER,DIMENSION(:)  :: NBE(N_CELL),NBE_V(N_VERT)
      INTEGER,DIMENSION(:)  :: NO_WB(N_WBMAX)
      INTEGER,DIMENSION(:)  :: NO_HB(N_HBMAX)
      INTEGER,DIMENSION(:)  :: NO_QB(N_QBMAX)
      INTEGER,DIMENSION(:)  :: NO_SB(N_SBMAX)
!     --------
      REAL*8,DIMENSION(:)  :: XC(N_CELL),YC(N_CELL)
      REAL*8,DIMENSION(:)  :: X(N_VERT),Y(N_VERT)
      REAL*8,DIMENSION(:)  :: AE(N_CELL)
      REAL*8,DIMENSION(:,:):: DXV(N_CELL,3),DYV(N_CELL,3)
      REAL*8,DIMENSION(:,:):: DXV2(N_CELL,3),DYV2(N_CELL,3)
      REAL*8,DIMENSION(:,:):: DXC(N_CELL,3),DYC(N_CELL,3)
      REAL*8,DIMENSION(:,:):: X0M(N_CELL,3),Y0M(N_CELL,3)
      REAL*8,DIMENSION(:)  :: DXC2(N_CELL)
      REAL*8,DIMENSION(:)  :: DYC2(N_CELL)
      REAL*8,DIMENSION(:)  :: DXY(N_CELL)
!     --------
      REAL*8,DIMENSION(:,:):: DISDIF(N_CELL,3)
      REAL*8,DIMENSION(:,:):: A0M(N_CELL,3)
      REAL*8,DIMENSION(:)  :: AH(N_CELL)
      REAL*8,DIMENSION(:)  :: CHEZY(N_CELL)
!     ____________________________________
!    |                                    |
!    |   Declaration of local variables   |
!    |____________________________________|

      INTEGER:: tagQ
      REAL*8 :: FRIC,QX2,QY2,CHEZ2,XCOS,YCOS,TETA,XX,YY,GX,GY
      REAL*8 :: QTAU,QTAUN,DZTAU,DZTAUN,QT,A0,B0,C0,HH,XL,YSIN,H2
      REAL*8 :: damp,x0,Lx
      INTEGER:: K,KK,K2,NP,elem

!     __________________________________________________
!    |                                                  |
!    |         DEFAULT UPDATES: QXN(NC) & QYN(NC)       |
!    |__________________________________________________|

      DO NC=1,N_CELL0
!        -----------------
#       ifdef KeyFriccion
           CHEZ2 = CHEZY(NC)
           H2    = HW(NC)*HW(NC)
           QX2   = QX(NC)*QX(NC)
           QY2   = QY(NC)*QY(NC)
           FRIC  = GRA/(CHEZ2*H2)*DSQRT(QX2+QY2)
#       else
           FRIC = 0.0d0
#       endif
!       -----------------
         A0 = (1.0d0 + GAMA*FRIC*DT)
         B0 = GRA*HW(NC)*DT
         GX = GAMA*F1X0(NC)+(1.0d0-GAMA)*FX0(NC)
         GY = GAMA*F1Y0(NC)+(1.0d0-GAMA)*FY0(NC)
         QXN(NC) = (QXN(NC)-B0*GX-(1.0d0-GAMA)*FRIC*QX(NC))/A0
         QYN(NC) = (QYN(NC)-B0*GY-(1.0d0-GAMA)*FRIC*QY(NC))/A0
      ENDDO

!     __________________________________________________
!    |                                                  |
!    |       Update velocity components: QXV & QYV      |
!    |__________________________________________________|

      tagQ = 1
      CALL INTERPO_Q(QXN,QXV,NBE_V,tagQ)
      tagQ = 2
      CALL INTERPO_Q(QYN,QYV,NBE_V,tagQ)

!     __________________________________________________
!    |                                                  |
!    |       Update velocity components: U & V          |
!    |__________________________________________________|

      DO NC=1,N_CELL
         IF (HW(NC).LE.H00) THEN
            UN(NC)=0.0d0
            VN(NC)=0.0d0
         ELSE
            UN(NC)=QXN(NC)/HW(NC)
            VN(NC)=QYN(NC)/HW(NC)
         ENDIF
      ENDDO

      DO NV=1,N_VERT
         IF (HWV(NV).LE.H00) THEN
            UVERT(NV)=0.0d0
            VVERT(NV)=0.0d0
         ELSE
            UVERT(NV)=QXV(NV)/HWV(NV)
            VVERT(NV)=QYV(NV)/HWV(NV)
         ENDIF
      ENDDO


      RETURN
      END
      
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!
!---------------------------------------------------------------------!
!                        SUBROUTINE: DispersionAH                     !
!              Calculates the dispersion coefficient AH               !
!                       Miguel Angel Uh Zapata                        !
!                     Last modification: Jul 2024                     !
!---------------------------------------------------------------------!
!sssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss!

      SUBROUTINE DispersionAH(AH,CHEZY,U,V,HW)
     
!     ____________________________________
!    |                                    |
!    |     Keys and common parameters     |
!    |____________________________________|

#     include "cppdefs.h"
      IMPLICIT NONE
      INCLUDE 'common.mpf'
!     ____________________________________
!    |                                    |
!    |      Declaration of variables      |
!    |____________________________________|

      REAL*8,DIMENSION(:) :: AH(N_CELL)
      REAL*8,DIMENSION(:) :: CHEZY(N_CELL)
      REAL*8,DIMENSION(:) :: U(N_CELL)
      REAL*8,DIMENSION(:) :: V(N_CELL)
      REAL*8,DIMENSION(:) :: HW(N_CELL)
!     ____________________________________
!    |                                    |
!    |   Declaration of local variables   |
!    |____________________________________|

      REAL*8:: viscosity_mu,density_rho,nuk
      REAL*8:: Ustar,Norma,chezy2

!     __________________________________________________
!    |                                                  |
!    |             The kinematic viscosity              |
!    |__________________________________________________|

      viscosity_mu = 0.001d0
      density_rho  = 1000.0d0
      nuk = viscosity_mu/density_rho
!     __________________________________________________
!    |                                                  |
!    |                  ELDER'S EQUATION                |
!    |__________________________________________________|

#     ifdef KeyDiff_Elder
         DO NC=1,N_CELL
            Norma  = sqrt(U(NC)**2+V(NC)**2)
            chezy2 = CHEZY(NC)
            Ustar  = sqrt(GRA)*Norma/sqrt(chezy2)
            AH(NC) = nuk + 6.0d0*Ustar*HW(NC)
         ENDDO
#     endif

!     __________________________________________________
!    |                                                  |
!    |              Yulistiyanto EQUATION               |
!    |__________________________________________________|

#     ifdef KeyDiff_Yulistiyanto
         DO NC=1,N_CELL
            Norma  = sqrt(U(NC)**2+V(NC)**2)
            chezy2 = CHEZY(NC)
            Ustar  = sqrt(GRA)*Norma/sqrt(chezy2)
            AH(NC) = nuk + 0.1d0*Ustar*HW(NC)
         ENDDO
#     endif

!     __________________________________________________
!    |                                                  |
!    |              Use AHDIFF in EQUATION              |
!    |__________________________________________________|

#     ifdef KeyDiff_UseAHDIFF
         DO NC=1,N_CELL
            Norma  = sqrt(U(NC)**2+V(NC)**2)
            chezy2 = CHEZY(NC)
            Ustar  = sqrt(GRA)*Norma/sqrt(chezy2)
            AH(NC) = nuk + AHDIFF*Ustar*HW(NC)
         ENDDO
#     endif

      RETURN
      END

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!---------------------------------------------------------------------!
!                                                                     !
!                           ***  END  ****                            !
!                                                                     !
!---------------------------------------------------------------------!
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!