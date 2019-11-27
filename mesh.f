      module mesh
      implicit none
      real*8,allocatable,dimension(:) :: rr,rw ! radial mesh
      real*8,allocatable,dimension(:) :: angx,angw !angular mesh
      real*8 :: rmax, hcm
      integer :: nx  ! angular mesh points number
      integer :: nr ! radial mesh points number
      end module
