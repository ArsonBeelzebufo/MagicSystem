module fieldModule
  use particleModule
  implicit none

  type(particle),dimension(:)::field

  contains
    real,dimension(3) function strength(pos)
      real,dimension(3)::pos
    end function strength
  
    subroutine smallSimulate(dt)
      real::dt

    end subroutine smallSimulate
end module fieldModule
