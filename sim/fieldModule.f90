module fieldModule
  use particleModule
  implicit none

  real::epsilon,sigma,t,n,ep,c,k
  type(particle),dimension(n)::field

  contains
    function strength(p)
      real,dimension(3)::strength,position
      strength=(/0,0,0/)
      integer::p
      position=field(p)%pos
      do i=1,n
        if(i/=p)then
          real,dimension(3)::d,other
          real::r,FReduced,temp
          other=field(i)%pos
          d=other-position
          r=dot_product(d,d)**.5
          temp=(sigma/r)**6
          FReduced=48*epsilon*temp*(temp-.5)/(r**2)
          strength=strength+other*FReduced
        end if
      end do
    end function strength

    function bigStrength()
      real,dimension(n,3)::bigStrength
      integer::i
      do i=1,n
        bigStrength(i,:)=strength(i)
      end do
    end function bigStrength
    
    subroutine smallSimulate(dt)
      real::dt
      real,dimension(n,3)::bs
      bs=bigStrength()
      integer::i
      do i=1,n
        type(particle)::p
        real::m
        real,dimension(3)::F
        F=bs(i)
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
end module fieldModule
