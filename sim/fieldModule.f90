module fieldModule
  use particleModule
  implicit none

  type(particle),dimension(:)::field

  contains
    function strength(p)
      real,dimension(3)::strength
      integer::p
      
    end function strength
  
    subroutine simulate(dt)
      real::dt
    end subroutine simulate
end module fieldModule
