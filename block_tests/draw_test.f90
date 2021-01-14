program draw_test
  use, intrinsic :: iso_c_binding, only: c_associated, c_int8_t, c_null_char, c_null_ptr, c_ptr
  use, intrinsic :: iso_fortran_env, only: stdout=> output_unit, stderr => error_unit
  use :: sdl2
  implicit none

  integer, parameter :: SCREEN_WIDTH=800
  integer, parameter :: SCREEN_HEIGHT=800

  integer, parameter :: width_screen = 800
  integer, parameter :: height_screen = 800
  integer, parameter :: dimen(2) = (/100,100/)
  integer :: cell_size(2)=(/int(width_screen/dimen(1)), int(height_screen/dimen(2))/)

  type(c_ptr) :: window
  type(c_ptr) :: renderer
  type(sdl_event) :: event
  type(sdl_rect) :: rect(dimen(1), dimen(2))
  integer    :: rc
  logical    :: is_running = .true.
  integer    :: i, j
  integer    :: acs(dimen(1), dimen(2))
  integer    :: up = 1, down = -1
  integer    :: clr_tuple(3)
  real       :: r

  if(sdl_init(SDL_INIT_VIDEO)<0) then
    write(stderr, *) 'SDL Error: ', sdl_get_error()
    stop
  end if

  window = sdl_create_window('Fortran SDL 2.0' // c_null_char, &
                              SDL_WINDOWPOS_UNDEFINED, &
                              SDL_WINDOWPOS_UNDEFINED, &
                              SCREEN_WIDTH, &
                              SCREEN_HEIGHT, &
                              SDL_WINDOW_SHOWN)

  if(.not. c_associated(window)) then
    write(stderr, *) 'SDL Error: ', sdl_get_error()
    stop
  end if

  renderer = sdl_create_renderer(window, -1, ior(SDL_RENDERER_ACCELERATED, &
                                                 SDL_RENDERER_PRESENTVSYNC))

  do concurrent(i=0:dimen(1)-1)
    do concurrent(j=0:dimen(2)-1)
      rect(i+1,j+1) = sdl_rect(cell_size(1)*i, cell_size(2)*j, cell_size(1), cell_size(2))
    end do
  end do

  do while (is_running)
    !Catch events.
    do while (sdl_poll_event(event)>0)
      select case (event%type)
      case (SDL_QUITEVENT)
        is_running = .false.
      end select
    end do

    rc = sdl_set_render_draw_color(renderer, &
                                   uint8(0), &
                                   uint8(0), &
                                   uint8(0), &
                                   uint8(SDL_ALPHA_OPAQUE))

    rc = sdl_render_clear(renderer)

    do i = 1, dimen(1)
      do j = 1, dimen(1)
        call random_number(r)
        if(r>0.5) then
          acs(i,j) = up
        else
          acs(i,j) = down
        end if
      end do
    end do

    do concurrent(i = 1:dimen(1))
      do concurrent(j = 1:dimen(2))
        if(acs(i,j)==up) then
          clr_tuple = (/0,255,0/)
        else
          clr_tuple = (/255,0,0/)
        end if
        rc = sdl_set_render_draw_color(renderer, &
                                       uint8(clr_tuple(1)), &
                                       uint8(clr_tuple(2)), &
                                       uint8(clr_tuple(3)), &
                                       uint8(SDL_ALPHA_OPAQUE))
        rc = sdl_render_fill_rect(renderer, rect(i,j))
      end do
    end do

    call sdl_render_present(renderer)
    call sdl_delay(20)
  end do

  call sdl_destroy_renderer(renderer)
  call sdl_destroy_window(window)
  call sdl_quit()
end program draw_test
