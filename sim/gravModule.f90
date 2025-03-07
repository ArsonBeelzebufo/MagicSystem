module gravModule
  use fieldModule
  implicit none

  real::G

  contains
    function grav(p)
      real,dimension(3)::grav,position
      grav=(/0,0,0/)
      integer::p
      type(particle)::part
      part=field(p)
      position=part%pos
      do i=1,n
        if(i/=p)then
          real,dimension(3)::d
          real::r,FReduced
          type(particle)::other
          other=field(i)
          d=other%pos-position
          r=dot_product(d,d)**.5
          FReduced=G*(k*(1-part%PE)/c**2)*(k*(1-other%PE)/c**2)/r**3
          grav=grav+d*FReduced
        end if
      end do
    end function grav
    
    function bigGrav()
      real,dimension(n,3)::bigGrav
      integer::i
      do i=1,n
        bigGrav(i,:)=grav(i)
      end do
    end function bigGrav
end module gravModule
