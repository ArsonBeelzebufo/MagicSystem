module fieldModule
  use particleModule
  implicit none

  type(particle),dimension(:)::field
  real::epsilon,sigma

  contains
    function strength(p)
      real,dimension(3)::strength,pos
      strength=(/0,0,0/)
      integer::p
      pos=field(p)
      do i=1,field.size
        if(i/=p)then
          real,dimension(3)::d,other
          real::r,FReduced,temp
          other=field(i,:)
          d=other-pos
          r=dot_product(d,d)**.5
          temp=(sigma/r)**6
          FReduced=48*epsilon*temp*(temp-.5)/(r**2)
          strength=strength+other*FReduced
        end if
      end do
    end function strength
  
    subroutine simulate(dt)
      real::dt
    end subroutine simulate
end module fieldModule
