module abstract_test
  implicit none

  public :: test_class

  type, abstract :: test_class
    integer :: a, b
  contains
    procedure(test_template), deferred :: test_proc
  end type test_class

  interface
    function test_template(this) result(c)
      import test_class
      class(test_class) :: this
      integer           :: c
    end function test_template
  end interface
end module abstract_test

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


program test_prog
  use extend_test
  use abstract_test
  use factory_test
  implicit none

  integer :: fop
  type(extend_class), target :: ext
  class(test_class), pointer :: ext_ptr=>null()
  ext%a = 1
  ext%b = 2
  ext_ptr=>ext
  call test_make(ext_ptr, fop)
  write(*,*) fop
end program test_prog
