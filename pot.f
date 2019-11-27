      module pot
      use precision
      use n9bemod
      implicit none
      contains


      subroutine single_fold()
      use input
      use mesh
      use interpolation
      implicit none
      integer  :: ir, irr ! index for r and R
      integer :: ix ! index for angular integration
      complex*16,dimension(1:nr) :: pot_n9Be ! n-9Be potential
      complex*16, dimension(1:nr) :: pot_fold ! folding potential
      real*8 :: r, r1 ! r and R
      real*8 :: r2 ! radius for n-n
      real*8 :: x
      complex*16 :: upot
! obtain the angela n-9Be potential
      do ir=1, nr
        call n9BeAB(ir,elab_n,pot_n9Be(ir))
      end do

! single fold the A-9Be potential

      pot_fold=0.0_dpreal
      do irr=1, nr  ! R
       r1= rr(irr)
       do ir=1, nr   ! r
         r=rr(ir)
         do ix=1, nx
           x=angx(ix)
           r2=sqrt( r1**2 + r**2 - 2.* r * r1 * x ) ! r for nn interaction
           upot = FFC( (r2-rr(1))/hcm , pot_n9Be , nr )
           pot_fold(irr) = pot_fold(irr) + 2.*pi*r*r*dens(ir)*upot*rw(ir)*angw(ix)
         end do
       end do
      end do





      write(41,*) 'Single folding potential based on n9Be AB'
      write(41,*) nr,hcm,rr(1)
      do ir=1, nr
        write(41,*) real(pot_fold(ir)), aimag(pot_fold(ir))
        write(99,*) real(pot_fold(ir)), aimag(pot_fold(ir))
      end do




      end subroutine








      end module
