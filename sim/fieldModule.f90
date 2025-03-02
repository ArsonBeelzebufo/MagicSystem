module fieldModule
  use particleModule
  implicit none

  type(particle),dimension(:)::field

  contains
    function strength(pos)
      real,dimension(3)::strength,pos
    end function strength
  
    subroutine simulate(dt)
      real::dt
    end subroutine simulate
end module fieldModule
