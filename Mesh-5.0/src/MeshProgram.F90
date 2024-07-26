!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!---------------------------------------------------------------------!
!                      	      PROGRAM                                 !
!                  Mesh generator (independent program)               !
!                            Jul 2024                                 !
!---------------------------------------------------------------------!
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!
      PROGRAM MeshProgram
!
!---------------------------------------------------------------------!   
!                                                                     !
!     This program converts the corresponding data                    !
!     coming from BlueKenue & Gmesh files to the new format required. ! 
!                                                                     !
!---------------------------------------------------------------------!

      implicit none
      integer :: ChooseMeshType
      
      ! print*,' '
      ! print*,' -------------------------------------------------------------- '
      ! print*,' METHODSTO TO GENERATE DATA: '
      ! print*,' 1) BlueKenue (you must have input/MeshBK.t3s)'
      ! print*,' 2) Gmsh      (you must have input/Gmsh.vtk)'
      ! print*,' -------------------------------------------------------------- '
      ! print*,' '

73    format(a,$)

      ChooseMeshType = 0
      ! write (*,73)'   CHOOSE OPTION: '
      ! read*,ChooseMeshType 
      ! if (ChooseMeshType.eq.1) then
      !    call MeshBlueKenue
      ! elseif (ChooseMeshType.eq.2) then
      !    call MeshGmsh   
      ! else
      !    print*,'Not option available'
      ! endif

      call MeshGmsh   
      END PROGRAM 

!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
!---------------------------------------------------------------------!
!                            END OF MeshProgram                       !
!---------------------------------------------------------------------!
!wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww!
