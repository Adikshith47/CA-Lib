module extend_test
  use abstract_test
  implicit none

  type, extends(test_class) :: extend_class
  contains
    procedure :: test_proc
  end type extend_class

contains
  function test_proc(this) result(c)
    class(extend_class) :: this
    integer             :: c
    c = this%a + this%b
  end function test_proc
end module extend_test
