module fieldModule
  use particleModule
  implicit none

  integer::n
  real::epsilon,sigma,t,ep,c,k
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
          strength=strength+d*FReduced
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
end module fieldModule
