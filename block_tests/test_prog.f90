
program test_prog
  use extend_test
  use abstract_test
  implicit none

  integer :: fop
  type(extend_class), target :: ext
  class(test_class), pointer :: ext_ptr=>ext
  ext%a = 1
  ext%b = 2

  fop = ext_ptr%test_proc()
  write(*,*) fop
end program test_prog
