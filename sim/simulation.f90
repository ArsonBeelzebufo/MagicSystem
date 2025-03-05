program simulation
  use fieldModule
  use gravModule
  implicit none
  
  contains
    subroutine smallSimulate(dt)
      real::dt
      real,dimension(n,3)::bF
      bF=bigStrength()+bigGrav()
      integer::i
      do i=1,n
        type(particle)::p
        real::m
        real,dimension(3)::F
        F=bF(i)
        m=k*(1-p%PE)/c**2
        p=field(i)
        p%pos=p%pos+p%v*dt+.5*dt**2*F/m
        p%v=p%v+dt/m*F
      end do
      t=t+dt
    end subroutine smallSimulate

    subroutine bigSimulate(bigDt,gen)
      real::bigDt,dt
      integer::gen,i
      dt=bigDt/gen
      do i=1,gen
        smallSimulate(dt)
      end do
    end subroutine bigSimulate
end program simulation
