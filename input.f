      module input
      use precision
      implicit none
      real*8,allocatable,dimension(:) :: dens ! density of the nuclei
      real*8 :: elab_n
      character*999 :: dens_file
      contains


      subroutine readin()
      use mesh
      use gauss
      implicit none


      namelist /global/ hcm, rmax, elab_n, dens_file, nx

      hcm=0.05_dpreal; rmax=40.0_dpreal
      elab_n=5.0_dpreal; nx=40

      read(5,nml=global)

      nr=nint(rmax/hcm)
      allocate(rr(nr),rw(nr))
      allocate(angx(nx),angw(nx))
      allocate(dens(nr))
      call simpson(nr,rmax,rr,rw)
      call gauleg(nx,-1.0_dpreal,1.0_dpreal,angx,angw)


      call read_dens()


      end subroutine



      subroutine read_dens()
      use interpolation
      use precision
      use mesh
      implicit none
      logical uu
      real*8 :: x, y
      real*8, allocatable,dimension(:) :: xv(:), faux(:)
      integer :: n_dens, n
      real*8,parameter:: alpha=0
      real*8 :: r

      uu=.false.
      inquire(file=trim(dens_file), exist=uu)
      if (.not.uu) then
        write(0,*) 'Density file:',trim(dens_file),' not found!'
        stop
      endif
      write(*,'(8x, "Potential file:",a20)') trim(dens_file)


      open(unit=99, file=trim(dens_file),status="old") ! dependent on the format of density  decide later
      rewind(99)
      n_dens=0
20    read(99,*,end=200) x,y
      n_dens=n_dens+1
      goto 20

200   if(n_dens < 1) write(*,*) "error reading density ! "

      ! readin the desity file
      rewind(99)
      allocate(xv(n_dens), faux(n_dens))
      do n=1, n_dens
        read(99,*)x, y
        xv(n)=x
        faux(n)=y
      end do

      do n=1, nr
        r=rr(n)
        dens(n) = fival(r, xv, faux, n_dens, alpha)
      end do


      close(99)

      end subroutine





      end module
