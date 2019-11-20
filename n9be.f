      module n9bemod
      implicit none
      contains

      subroutine n9BeAB(ir,E,VAB,sopot)
      use mesh,only:hcm
      implicit none
      complex*16 :: VAB
      integer :: ir
      real*8 :: E,vr,r0,ar,wv,ws,rw,aw,vso,rso,aso,alfa,r,A
      real*8 :: pot,impot,sopot
c      write(0,*) 'Write r0,dr,rmax'
c      read(*,*) ri,dr,rmax
c      nr=nint((rmax-ri)/dr+1d0)
c      write(0,*) 'Write energy'
c      read(*,*) E

      A=9
      r0=1.647
      ar=0.3
      rso=1.27
      aso=0.7
      rw=1.3
      aw=0.3

      if(E.lt.2.4d0) then
      alfa=-26.05
      else if (E.lt.10d0) then
      alfa=3
      else
      alfa=3.2-0.02*E
      endif

      if(E.lt.2d0) then
      vso=7.9d0
      else
      vso=5.5
      endif

      if (E.lt.3d0) then
      vr=32d0-0.377*E
      wv=0d0
      ws=0d0
      else
      vr=31.304-0.145*E
      wv=0.1+1.025*(E-3d0)
      ws=0.4+1.543*(E-3d0)
      endif

      if (E.gt.5d0) then
      wv=1d0+0.23*E
      ws=1.65d0+0.365*E
      r0=1.647-0.005*(E-5d0)
      endif

      if (E.gt.20d0) then
      wv=5.6-0.005*(E-20d0)
      ar=0.3-0.0001*E
      endif

      if (E.gt.40d0) then
      wv=5.5-0.01*(E-40d0)
      ws=16.25-0.05*(E-40d0)
      endif

      if (E.gt.111d0) then
      ar=0.288
      wv=4.8
      endif

c      open(5,file='potn9Be.pot')
c      do ir=1,nr
c      r=ri+dr*(ir-1d0)
       r=ir*hcm
       if(ir==0) r=0.00000001
       pot=-vr/(1d0+exp((r-r0*A**0.333333)/ar))-16*alfa*
     & exp(2d0*(r-r0*A**0.33333)/ar)/(1d0+exp((r-r0*A**0.333333)/ar))**4
       impot=-wv/(1d0+exp((r-rw*A**0.333333)/aw))-4d0*ws*
     & exp(-(r-rw*A**0.333333)/aw)/(1d0+exp(-(r-rw*A**0.333333)/aw))**2



       VAB=cmplx(pot,impot,kind=8)
       sopot=-2d0*vso/r/aso*exp(-(r-rso*A**0.333333)/aso)/
     & (1d0+exp(-(r-rso*A**0.333333)/aso))**2



c      write(5,*)r,pot(ir),impot(ir),sopot(ir)
c      enddo
c      close(5)



C       open(4,file='fresco.pot',status='OLD')
C      write(4,*) 'Central potential n9Be AB'
C      write(4,*) nr,dr,ri
C      write(4,*) pot,impot
C      write(4,'(1P,6E12.4)') (pot(ir),impot(ir),ir=1,nr)
C      write(4,*) 'Spin-Orbit potential n9Be AB'c
C      write(4,*) nr,dr,ri
C      write(4,'(1P,6E12.4)') (sopot(ir),ir=1,nr)

      end subroutine
      end module
