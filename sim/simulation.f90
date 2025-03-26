program simulation
  use gravModule
  implicit none
  
  n=10
  allocate(field(n))

  contains
    subroutine smallSimulate(dt)
      real::dt,m
      real,dimension(n,3)::bF
      real,dimension(3)::F
      integer::i
      type(particle)::p
      bF=bigStrength()+bigGrav()
      do i=1,n
        F=bF(i,:)
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
        call smallSimulate(dt)
      end do
    end subroutine bigSimulate
end program simulation
