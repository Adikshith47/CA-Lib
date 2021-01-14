module ising_model_test_2
  use abstract_base_class
  implicit none

  type, extends(ca_base_class) :: ising_class
  contains
    procedure :: init_state
    procedure :: evolve_cell
    procedure :: get_state_draw_color
  end type ising_class

  real :: temperature=300

contains

  function init_state(this) result(dummy_cell_state)
    class(ising_class), intent(in)  :: this
    real, allocatable   :: dummy_cell_state(:)
    real                :: r
    real                :: up=1.0, down=-1.0
    allocate(dummy_cell_state(2))
    call random_number(r)
    if(r>0.5) then
      dummy_cell_state(1)=up
    else
      dummy_cell_state(1)=down
    end if
    dummy_cell_state(2)=0.0
  end function init_state

  function evolve_cell(this, neighbours, dummy_cell_state) result(dcs)
    class(ising_class)  :: this
    real, intent(in)    :: dummy_cell_state(:)
    real, intent(in)    :: neighbours(:,:)
    real, allocatable   :: dcs(:)
    real                :: r(3)
    real                :: temp_energy, energy_change
    call random_number(r)
    dcs=this%all_cell_state(1+int(r(1)*ubound(this%all_cell_state, dim=1)),&
                            1+int(r(2)*ubound(this%all_cell_state, dim=2)),:)

    if(dcs(1)==dummy_cell_state(1).and.dcs(2)==dummy_cell_state(2))then

      dcs(2) = calculate_energy(dcs, neighbours)
      dcs(1)=-dcs(1)
      temp_energy = calculate_energy(dcs, neighbours)
      energy_change = temp_energy - dcs(2)

      if(energy_change<0) then
        dcs(1)=dcs(1)
        dcs(2)=temp_energy
      elseif(exp(energy_change/temperature)<=r(3)) then
        dcs(1)=dcs(1)
        dcs(2)=temp_energy
      else
        dcs=dummy_cell_state
      end if

    else
      dcs=dummy_cell_state
    end if
  end function evolve_cell

  pure function get_state_draw_color(this, current_state) result(clr_tuple)
    class(ising_class), intent(in) :: this
    real, intent(in) :: current_state(:)
    real               :: clr_tuple(3)
    if(current_state(1)==1.0) then
      clr_tuple = (/255,0,0/)
    else
      clr_tuple = (/0,255,0/)
    end if
  end function get_state_draw_color

   function calculate_energy(dummy_cell_state, neighbours) result(energy)
    real, intent(in) :: dummy_cell_state(:)
    real, intent(in) :: neighbours(:,:)
    real             :: energy
    integer          :: iter

    energy = 0
    do iter = 1, ubound(neighbours, dim=1)
      energy = energy + 5*dummy_cell_state(1)*neighbours(iter,1)
    end do

  end function calculate_energy
end module ising_model_test_2

program ising_model_program
  use ising_model_test_2
  use ca_factory_test_2
  use abstract_base_class
  implicit none

  integer :: ca_dimension(2)=[100,100]
  integer :: features = 2
  character(40) :: neighbourhood="Moore"
  character(40) :: grid_type="cells"
  type(ising_class), target :: ising
  class(ca_base_class), pointer :: ising_ptr=>ising

  call make_cellular_automaton(ising_ptr, neighbourhood, ca_dimension, features, grid_type)
end program
