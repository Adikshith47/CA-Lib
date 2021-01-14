module abstract_base_class
  implicit none

  public  :: ca_base_class
  private :: coordinates

  type :: coordinates
    integer :: co_array(2)
  end type coordinates

  type, abstract :: ca_base_class

    real, allocatable :: cell_state(:)
    real, allocatable :: all_cell_state(:,:,:)
    type(coordinates), allocatable :: cell_coordinates(:)

  contains
    procedure(initalise_state), deferred :: init_state
    procedure(evolution_rule),  deferred :: evolve_cell
    procedure(draw_ca),         deferred :: get_state_draw_color
  end type ca_base_class

  interface

    function initalise_state(this) result(dummy_cell_state)
      import ca_base_class
      class(ca_base_class), intent(in) :: this
      real, allocatable    :: dummy_cell_state(:)
    end function initalise_state

    function evolution_rule(this, neighbours, dummy_cell_state) result(dcs)
      import ca_base_class
      class(ca_base_class) ::  this
      real, intent(in)     :: dummy_cell_state(:)
      real, intent(in)     :: neighbours(:,:)
      real, allocatable    :: dcs(:)
    end function evolution_rule

    pure function draw_ca(this, current_state) result(clr_tuple)
      import ca_base_class
      class(ca_base_class), intent(in) :: this
      real, intent(in)     :: current_state(:)
      real                 :: clr_tuple(3)
   end function draw_ca
  end interface

end module abstract_base_class
