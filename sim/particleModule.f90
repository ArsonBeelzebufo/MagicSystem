module particleModule
  type particle
    real,dimension(:,4)::position !x,y,z,t
  end type particle

  contains
    integer function search(p,t) !binary search
      type(particle)::p
      int::l,r
      l=1
      r=size(p%position)
    end function search
    
    real,dimension(3) function derivative(p,n,t) !nth derivative of position over time
      type(particle)::p
      int::n
      real::t
      
    end function derivative
end module particleModule