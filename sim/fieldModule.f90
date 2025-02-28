module fieldModule
  use particleModule
  implicit none

  type(particle),dimension(:)::field

  contains
    real,dimension(3) function strength(pos,t)
    end function strength
  
    subroutine simulate()
    end subroutine simulate
end module fieldModule
