module particleModule
  type particle
    real,dimension(3)::pos,v
    real::PE !PE=percent energy; amount of intrinsic energy divided by the energy constant (in decimal not percent)
  end type particle

  contains
    function search(A,t)result(m) !binary search, returns the index that t would be inserted into position to keep the list sorted
      real,dimension(:)::A
      integer::l,r,m
      l=1
      r=size(A)
      m=(l+r)/2
      do while(l<r)
        if(A(m)<t)then
          l=m+1
        else if(A(m)>t)then
          r=m
        else
          return
        end if
        m=(l+r)/2
      end do
    end function search
end module particleModule
