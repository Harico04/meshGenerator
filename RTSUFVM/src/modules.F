!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!---------------------------------------------------------------------!
!                     MODULE GEOMETRY VARIABLES                       !
!                      Miguel Angel Uh Zapata                         !
!                             Jun 2019                                !
!---------------------------------------------------------------------!
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!

      MODULE geometry

#     include "cppdefs.h"
      implicit none
!     ____________________________________
!    |                                    |
!    |               Variables            |
!    |____________________________________|

      real*8, dimension(:)  ,allocatable :: dlVsum
      real*8, dimension(:,:),allocatable :: weight
      integer,dimension(:)  ,allocatable :: Dimsurrounding
      integer,dimension(:,:),allocatable :: surrounding
            
      CONTAINS
!      _______________________________________________________________ 
!     |      |                                                        |
!     | 1.1  |                 allocate_variables                     |
!     |______|________________________________________________________|

      SUBROUTINE allocate_geometry

#     include "common.mpf"

      allocate(dlVsum(N_VERT))
      allocate(Dimsurrounding(N_VERT))
      allocate(surrounding(N_VERT,10))      
      allocate(weight(N_VERT,10))
      
      END SUBROUTINE allocate_geometry

!      _______________________________________________________________ 
!     |      |                                                        |
!     | 1.2  |                 deallocate_variables                   |
!     |______|________________________________________________________|

      SUBROUTINE deallocate_geometry

      deallocate(dlVsum)
      deallocate(Dimsurrounding)
      deallocate(weight)
      deallocate(surrounding)

      END SUBROUTINE deallocate_geometry

      END MODULE geometry
            
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!---------------------------------------------------------------------!
!                         END MODULE VARIABLES                        !
!---------------------------------------------------------------------!
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!