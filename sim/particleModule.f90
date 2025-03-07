module particleModule
  implicit none

  type particle
    real,dimension(3)::pos,v
    real::PE !PE=percent energy; amount of intrinsic energy divided by the energy constant (in decimal not percent)
  end type particle
end module particleModule
