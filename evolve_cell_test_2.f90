module evolve_cell_test_2
  use abstract_base_class
  implicit none

!idea is to parallelise with openmp by using the shared data type
contains

  subroutine evolve_all_cells(rule_class, neighbourhood, dimen)
    class(ca_base_class), intent(in), pointer :: rule_class
    character(*), intent(in) :: neighbourhood
    integer, intent(in)      :: dimen(:)
    real, allocatable        :: neighbours(:,:)
    integer                  :: counter = 0, i, j
    real, allocatable        :: temp_cell_state(:,:,:)
    integer                  :: evolution_step=0

    temp_cell_state = rule_class%all_cell_state

    !main loop to be parallelised
    do i = 1, dimen(1)
      do j = 1, dimen(2)
        counter = counter+1
        if(i/=1.and.j/=1.and.i/=dimen(1).and.j/=dimen(2)) then

          neighbours = get_cell_neighbours(rule_class%all_cell_state, neighbourhood, &
                                        rule_class%cell_coordinates(counter)%co_array)

          temp_cell_state(i,j,:) = rule_class%evolve_cell(neighbours, &
                                                        temp_cell_state(i,j,:))
        end if

        if(mod(counter, dimen(1)*dimen(2))==0) then
          counter = 0
          evolution_step = evolution_step+1
        end if
      end do
    end do

    rule_class%all_cell_state = temp_cell_state
  end subroutine evolve_all_cells

  function get_cell_neighbours(all_cell_state, neighbourhood, cell_coordinates) result(neighbours)
    real, intent(in)              :: all_cell_state(:,:,:)
    character(*), intent(in)      :: neighbourhood
    integer, intent(in)           :: cell_coordinates(:)
    real, allocatable             :: neighbours(:,:)
    integer                       :: allstat1
    integer                       :: i, j
    integer                       :: counter1

    if(neighbourhood=='Moore') then
      allocate(neighbours(8,2), stat=allstat1)
      counter1=0
      do concurrent(i=-1:1)
        do concurrent(j=-1:1)
          if(i/=0.or.j/=0) then
            counter1 = counter1+1
            neighbours(counter1,:) = all_cell_state(cell_coordinates(1)+i,&
                                                cell_coordinates(2)+j,:)
          end if
        end do
      end do

    else if(neighbourhood=='VonNeumann') then
      allocate(neighbours(4,2), stat=allstat1)
      counter1=0
      do concurrent(i=-1:1)
        do concurrent(j=-1:1)
          if((i/=0.or.j/=0).and.(abs(i)+abs(j)/=2)) then

            counter1=counter1+1
            neighbours(counter1,:) = all_cell_state(cell_coordinates(1)+i,&
                                                cell_coordinates(2)+j,:)
          end if
        end do
      end do

    end if
  end function get_cell_neighbours
end module evolve_cell_test_2
