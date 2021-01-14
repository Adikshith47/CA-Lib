module factory_test
  use abstract_test
  implicit none

contains
  subroutine test_make(test_ptr, op)
    class(test_class), pointer, intent(in) :: test_ptr
    integer, intent(out) :: op
    op = test_ptr%test_proc()
  end subroutine test_make
end module factory_test
