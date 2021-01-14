module init_state_test_2
  use abstract_base_class
  implicit none

contains
  subroutine create_cellular_automaton(rule_class, dimen, features)
    class(ca_base_class), pointer :: rule_class
    integer, intent(in) :: dimen(:)
    integer, intent(in) :: features
    allocate(rule_class%all_cell_state(dimen(1), dimen(2), features))
    allocate(rule_class%cell_state(features))
    allocate(rule_class%cell_coordinates(dimen(1)*dimen(2)))
  end subroutine create_cellular_automaton

  subroutine init_all_cells(rule_class, dimen)
    class(ca_base_class), pointer :: rule_class
    integer :: i, j, counter
    integer, intent(in) :: dimen(:)
    counter = 0
    do i = 1, dimen(1)
      do j = 1, dimen(2)
        counter = counter+1
        rule_class%all_cell_state(i,j,:) = rule_class%init_state()
        rule_class%cell_coordinates(counter)%co_array = (/i,j/)
      end do
    end do
  end subroutine init_all_cells
end module init_state_test_2
