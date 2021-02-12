! check performance of associate and pointer

program main
  use easifem
  implicit none

  type(groupdof_) :: rhs, rhs_fixed, v, v0, u0, a0
  type(groupdof_), pointer :: ptr1, ptr2

  integer :: i, n = 2, m = 2, j

  u0 = groupdof( [m, m], ["u", "v"], [3, 2], [2, 2])

  call u0 % display( )




end program main