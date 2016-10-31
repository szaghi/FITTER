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
  integer(I4P)               :: snippet_current=0   !< Current snippet index.
  logical                    :: is_tracking=.false. !< Sentinel of tracking status.
  contains
    ! public methods
    procedure, pass(self) :: clean      => timer_clean      !< Clean timer.
    procedure, pass(self) :: print      => timer_print      !< Print time of a snippet or of all ones.
    procedure, pass(self) :: statistics => timer_statistics !< Return timer statistics.
    procedure, pass(self) :: tic        => timer_tic        !< Start a new snippet tracking.
    procedure, pass(self) :: time       => timer_time       !< Get time of a snippet or whole time.
    procedure, pass(self) :: times      => timer_times      !< Get time-array of all snippets.
    procedure, pass(self) :: toc        => timer_toc        !< Stop current tracking.
    ! private methods
    procedure, pass(self), private :: add_snippet   => timer_add_snippet   !< Add new snippet.
    procedure, pass(self), private :: snippet_index => timer_snippet_index !< Return the snippet index given the name.
endtype timer

character(1), parameter :: NL=new_line('a') !< New line character.
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
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
  self%snippet_current = 0
  self%is_tracking = .false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_clean

  subroutine timer_print(self, name, statistics, zpad)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print time of a snippet or of all ones.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(in)           :: self       !< The timer.
  character(*), intent(in), optional :: name       !< Snippet name.
  logical,      intent(in), optional :: statistics !< Print statistics.
  integer(I4P), intent(in), optional :: zpad       !< Zero padding for integer counters.
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
      if (statistics) print '(A)', self%statistics(zpad=zpad)
    endif
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_print

  function timer_statistics(self, zpad) result(statistics)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return timer statistics.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(in)           :: self       !< The timer.
  integer(I4P), intent(in), optional :: zpad       !< Zero padding for integer counters.
  character(len=:), allocatable      :: statistics !< Timer statistics.
  real(R8P)                          :: time       !< Snippets whole time.
  integer(I4P)                       :: s          !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_tracking) error stop 'error: cannot get timer statistics during traking'
  statistics = ''
  if (self%snippets_number>0) then
    time = self%time()
    statistics = 'Number of snippets tracked: '//trim(str(self%snippets_number, .true.))//NL
    statistics = statistics//'Total elapsed time: '//trim(str(time, .true.))//' [s]'//NL
    statistics = statistics//'Average (snippet) elapsed time: '//trim(str(time/self%snippets_number, .true.))//' [s]'//NL
    statistics = statistics//'Relative elapsed time into each snippet:'//NL
    do s=1, self%snippets_number
      statistics = statistics//'  + '//self%snippets(s)%name//': '//trim(str('(F6.3)', self%snippets(s)%time/time*100))//'%'//NL
      if (self%snippets(s)%tic_toc_number()>1) statistics = statistics//self%snippets(s)%statistics(prefix='    ', zpad=zpad)
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction timer_statistics

  subroutine timer_tic(self, name, zpad)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Start a new snippet tracking.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout)        :: self !< The timer.
  character(*), intent(in), optional :: name !< Snippet name.
  integer(I4P), intent(in), optional :: zpad !< Zero padding for integer counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_tracking) error stop 'error: cannot start a new snippet tracking before stop the current'
  call self%add_snippet(name=name, zpad=zpad)
  self%is_tracking = .true.
  call self%snippets(self%snippet_current)%tic
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_tic

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
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%is_tracking) error stop 'error: cannot get elapsed time during traking'
  if (self%snippets_number>0) then
    allocate(times(1:self%snippets_number))
    times = self%snippets(:)%time
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction timer_times

  subroutine timer_toc(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Stop current tracking.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout) :: self !< The timer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call self%snippets(self%snippet_current)%toc
  if (.not.self%is_tracking) error stop 'error: there is not a snippet tracking to stop'
  self%is_tracking = .false.
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_toc

  ! private methods
  subroutine timer_add_snippet(self, name, zpad)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add new snippet.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(inout)        :: self        !< The timer.
  character(*), intent(in), optional :: name        !< Snippet name.
  integer(I4P), intent(in), optional :: zpad        !< Zero padding for integer counter.
  type(snippet), allocatable         :: snippets(:) !< The new snippet.
  character(len=:), allocatable      :: name_       !< Snippet name, local variable.
  integer(I4P)                       :: zpad_       !< Zero padding for integer counter, local variable.
  integer(I4P)                       :: s           !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  zpad_ = 3 ; if (present(zpad)) zpad_ = zpad
  name_ = 'snippet-'//trim(strz(self%snippets_number+1, zpad_)) ; if (present(name)) name_ = name
  if (self%snippets_number>0) then
    s = self%snippet_index(name=name_)
    if (s==0) then
      ! add new snippet
      allocate(snippets(1:self%snippets_number+1))
      snippets(1:self%snippets_number) = self%snippets
      snippets(self%snippets_number+1) = snippet(name=name_)
      call move_alloc(from=snippets, to=self%snippets)
      self%snippets_number = self%snippets_number + 1
      self%snippet_current = self%snippets_number
    else
      ! snippet alredy exist, just reset current snippet index
      self%snippet_current = s
    endif
  else
    ! there are not snippets at all, add the first one
    allocate(self%snippets(1:1))
    self%snippets(1) = snippet(name=name_)
    self%snippets_number = self%snippets_number + 1
    self%snippet_current = self%snippets_number
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine timer_add_snippet

  elemental function timer_snippet_index(self, name) result(snippet_index)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return the snippet index given the name.
  !<
  !< Return 0 is not present
  !---------------------------------------------------------------------------------------------------------------------------------
  class(timer), intent(in) :: self          !< The timer.
  character(*), intent(in) :: name          !< Snippet name.
  integer(I4P)             :: snippet_index !< Snippet index, 0 is not present.
  integer(I4P)             :: s             !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  snippet_index = 0
  if (self%snippets_number>0) then
    do s=1, self%snippets_number
      if (self%snippets(s)%name==name) then
        snippet_index = s
        exit
      endif
    enddo
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction timer_snippet_index
endmodule fitter
