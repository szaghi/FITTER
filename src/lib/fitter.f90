!< FITTER, Fortran tIc Toc TimER
module fitter
!-----------------------------------------------------------------------------------------------------------------------------------
!< FITTER, Fortran tIc Toc TimER
!-----------------------------------------------------------------------------------------------------------------------------------
use fitter_snippet
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: timer
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: timer
  !< **Timer** class.
  private
  type(snippet), allocatable :: snippets(:)         !< Snippets tracked.
  integer(I4P)               :: snippets_number=0   !< Snippets number.
  logical                    :: is_tracking=.false. !< Sentinel of tracking status.
  contains
    ! public methods
    procedure, pass(self) :: clean => timer_clean !< Clean timer.
    procedure, pass(self) :: print => timer_print !< Print time of a snippet or of all ones.
    procedure, pass(self) :: start => timer_start !< Start a new snippet tracking.
    procedure, pass(self) :: stop  => timer_stop  !< Stop current tracking.
    procedure, pass(self) :: time  => timer_time  !< Get time of a snippet or whole time.
endtype timer
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  elemental subroutine timer_clean(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Clean timer.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout) :: self !< The timer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%snippets)) then
    call snippets%clean
    deallocate(self%snippets)
  endif
  self%snippets_number = 0
  self%is_tracking = .false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_clean

  subroutine timer_print(self, name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print time of a snippet or of all ones.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(in)           :: self !< The timer.
  character(*), intent(in), optional :: name !< Snippet name.
  integer(I4P)                       :: s    !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%snippets_number>0) then
    if (present(name)) then
      do s=1, self%snippets_number
        if (self%snippets(s)%name==name) then
          call self%snippets(s)%print
          exit
        endif
      enddo
    else
      do s=1, self%snippets_number
        call self%snippets(s)%print
      enddo
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_print

  subroutine timer_start(self, name)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Start a new snippet tracking.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout)        :: self        !< The timer.
  character(*), intent(in), optional :: name        !< Snippet name.
  type(snippet), allocatable         :: snippets(:) !< The new snippet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_tracking) error stop 'error: cannot start a new snippet tracking before stop the current'
  if (self%snippets_number>0) then
    allocate(snippets(1:self%snippets_number+1))
    snippets(1:self%snippets_number) = self%snippets
    snippets(self%snippets_number+1) = snippet(name=name, number=self%snippets_number+1)
    call move_alloc(from=snipptes, to=self%snippets)
  else
    allocate(self%snippets(1:1))
    self%snippets(1) = snippet(name=name, number=self%snippets_number+1)
  endif
  self%snippets_number = self%snippets_number + 1
  self%is_tracking = .true.
  call self%snippets(self%snippets_number)%start
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_start

  subroutine timer_stop(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Stop current tracking.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout)        :: self        !< The timer.
  character(*), intent(in), optional :: name        !< Snippet name.
  type(snippet), allocatable         :: snippets(:) !< The new snippet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%snippets(self%snippets_number)%stop
  if (.not.self%is_tracking) error stop 'error: there is not a snippet tracking to stop'
  self%is_tracking = .false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_stop

  pure function timer_time(self) result(time)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Clean timer.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(in)           :: self !< The timer.
  character(*), intent(in), optional :: name !< Snippet name.
  real(R8P)                          :: time !< Snippet(s) time.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_tracking) error stop 'error: cannot get elapsed time during traking'
  time = 0._R8P
  if (self%snippets_number>0) then
    if (present(name)) then
      do s=1, self%snippets_number
        if (self%snippets(s)%name==name) then
          time = self%snippets(s)%time
          exit
        endif
      enddo
    else
      do s=1, self%snippets_number
        time = time + self%snippets(s)%time
      enddo
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction timer_time
endmodule fitter
