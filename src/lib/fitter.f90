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
    procedure, pass(self) :: clean      => timer_clean      !< Clean timer.
    procedure, pass(self) :: print      => timer_print      !< Print time of a snippet or of all ones.
    procedure, pass(self) :: start      => timer_start      !< Start a new snippet tracking.
    procedure, pass(self) :: statistics => timer_statistics !< Return timer statistics.
    procedure, pass(self) :: stop       => timer_stop       !< Stop current tracking.
    procedure, pass(self) :: time       => timer_time       !< Get time of a snippet or whole time.
    procedure, pass(self) :: times      => timer_times      !< Get time-array of all snippets.
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
    call self%snippets%clean
    deallocate(self%snippets)
  endif
  self%snippets_number = 0
  self%is_tracking = .false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_clean

  subroutine timer_print(self, name, statistics)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print time of a snippet or of all ones.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(in)           :: self       !< The timer.
  character(*), intent(in), optional :: name       !< Snippet name.
  logical,      intent(in), optional :: statistics !< Print statistics.
  integer(I4P)                       :: s          !< Counter
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
    if (present(statistics)) then
      if (statistics) print '(A)', self%statistics()
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
    call move_alloc(from=snippets, to=self%snippets)
  else
    allocate(self%snippets(1:1))
    self%snippets(1) = snippet(name=name, number=self%snippets_number+1)
  endif
  self%snippets_number = self%snippets_number + 1
  self%is_tracking = .true.
  call self%snippets(self%snippets_number)%start
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_start

  function timer_statistics(self) result(statistics)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return timer statistics.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(in)      :: self             !< The timer.
  character(len=:), allocatable :: statistics       !< Timer statistics.
  real(R8P)                     :: time             !< Snippets whole time.
  integer(I4P)                  :: s                !< Counter
  character(1), parameter       :: nl=new_line('a') !< New line character.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_tracking) error stop 'error: cannot get timer statistics during traking'
  statistics = ''
  if (self%snippets_number>0) then
    time = self%time()
    statistics = 'Number of snippets tracked: '//trim(str(self%snippets_number, .true.))//nl
    statistics = statistics//'Total elapsed time: '//trim(str(time, .true.))//' [s]'//nl
    statistics = statistics//'Average (snippet) elapsed time: '//trim(str(time/self%snippets_number, .true.))//' [s]'//nl
    statistics = statistics//'Relative elapsed time into each snippet:'//nl
    do s=1, self%snippets_number
      statistics = statistics//'  + '//self%snippets(s)%name//': '//trim(str('(F6.3)', self%snippets(s)%time/time*100))//'%'//nl
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction timer_statistics

  subroutine timer_stop(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Stop current tracking.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout) :: self !< The timer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%snippets(self%snippets_number)%stop
  if (.not.self%is_tracking) error stop 'error: there is not a snippet tracking to stop'
  self%is_tracking = .false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_stop

  function timer_time(self, name) result(time)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get time of a snippet or whole time.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(in)           :: self !< The timer.
  character(*), intent(in), optional :: name !< Snippet name.
  real(R8P)                          :: time !< Snippet(s) time.
  integer(I4P)                       :: s    !< Counter
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

  function timer_times(self) result(times)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get time-array of all snippets.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(in) :: self     !< The timer.
  real(R8P), allocatable   :: times(:) !< Snippets time.
  ! integer(I4P)             :: s        !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_tracking) error stop 'error: cannot get elapsed time during traking'
  if (self%snippets_number>0) then
    allocate(times(1:self%snippets_number))
    times = self%snippets(:)%time
    ! do s=1, self%snippets_number
    !   time = time + self%snippets(s)%time
    ! enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction timer_times
endmodule fitter
