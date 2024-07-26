!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!---------------------------------------------------------------------!
!                Choose a pre-defined model application               !
!                             Jul 2024                                !
!---------------------------------------------------------------------!
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!

!---------------------------------------------------------------------!
!                                                                     !
!    This part of the program is called using the command:            !
!                    #include "cppdefs.h"                             !
!                                                                     !
!    Choose the C-preprocessing options by using the command          !
!                    #define   to activate option or                  !
!                    #undef    to deactivate option.                  !
!                                                                     !
!---------------------------------------------------------------------!

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!                                                                     !
!                           PROBLEM CHOICE                            !
!                                                                     !
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!      ________________________________________________________
!     |                                                        |
!     |   Compiler                                             |
!     |________________________________________________________|  

#undef KeyXLF        /* XLF compiler */
#define KeyG77       /* GNU compiler */
!      ________________________________________________________
!     |                                                        |
!     |    Detailed description of all available CPP options   |
!     |________________________________________________________| 

#undef KeyUpwindFirst         /* First order upwind scheme */
#undef KeySlip                /* No vertical stresses contributions */
#undef KeyNoSlip              /* Velocities (u,v)=0 */
#undef KeyFriccion            /* Includes friction coefficients */
#undef KeyDiffusion           /* Add diffusion (Laplacian: Txx & Tyy) */

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!                                                                     !
!                Particular options for each problem case.            !
!                                                                     !
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!

!      ________________________________________________________
!     |                                                        |
!     |  ******** Choose the wat to choose the tags  ********  |
!     |________________________________________________________|

#     define  KeyTagsManual_Cylinder  /* Set boundary tags manually*/

!      ________________________________________________________
!     |                                                        |
!     |                        Options                         |
!     |________________________________________________________|

!     ------------------------------
!     [1] Componments of the fluid
#     define KeyFriccion          /*  Add friction  in the model    */
#     define KeyDiffusion         /*  Add diffusion in the model    */
!     ------
!#    define KeyDiff_Elder        /*  Diffusive coefficient = 0.6    */
!#    define KeyDiff_Yulistiyanto /*  Diffusive coefficient = 0.1    */
#     define KeyDiff_UseAHDIFF    /*  Diffusive coefficient = AHDIFF */
!     ------------------------------
!     [2] Boundary conditions 
#     define KeySlip              /* KeySlip or KeyNoSlip for wall type */
#     define Key_LoopBoundaryQ    /* Set boundary condition in the loop */


!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!                                                                     !
!                                  END                                !
!                                                                     !
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
