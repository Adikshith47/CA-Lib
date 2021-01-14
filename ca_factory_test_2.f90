module ca_factory_test_2
  use abstract_base_class
  use init_state_test_2
  use draw_cell_test_2
  use evolve_cell_test_2
  real, allocatable :: cell_state(:)

contains

  subroutine make_cellular_automaton(rc_ptr, neighbourhood, dimen, features, grid_type)
    class(ca_base_class), pointer, intent(in) :: rc_ptr
    character(*), intent(in)      :: neighbourhood
    integer, intent(in)           :: dimen(:)
    integer, intent(in)           :: features
    character, intent(in)         :: grid_type
    call create_cellular_automaton(rc_ptr, dimen, features)
    call init_all_cells(rc_ptr, dimen)
    do while(is_running)
      call evolve_all_cells(rc_ptr, neighbourhood, dimen)
      call draw_cellular_automaton(rc_ptr, dimen, grid_type)
    end do
  end subroutine make_cellular_automaton
end module CA_Factory_test_2
