!< FITTER **snippet** class.
module fitter_snippet
!-----------------------------------------------------------------------------------------------------------------------------------
!< FITTER **snippet** class.
!-----------------------------------------------------------------------------------------------------------------------------------
use penf
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public :: snippet
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type :: snippet
  !< **Snippet** class.
  character(len=:), allocatable      :: name              !< Name of the snippet.
  real(R8P)                          :: time=0._R8P       !< Elapsed time in the snippet.
  integer(I8P), private, allocatable :: tic_toc_(:,:)     !< Tic toc storage.
  integer(I8P), private              :: count_rate_=0     !< Count rate.
  integer(I8P), private              :: tic_toc_number_=0 !< Tic toc pairs number.
  contains
    ! public methods
    procedure, pass(self) :: clean          => snippet_clean          !< Clean snippet.
    procedure, pass(self) :: print          => snippet_print          !< Print time of the snippet.
    procedure, pass(self) :: statistics     => snippet_statistics     !< Return snippet statistics.
    procedure, pass(self) :: tic            => snippet_tic            !< Start tracking.
    procedure, pass(self) :: toc            => snippet_toc            !< Stop tracking.
    procedure, pass(self) :: tic_toc_number => snippet_tic_toc_number !< Return tic toc pairs number.
endtype snippet

character(1), parameter :: NL=new_line('a') !< New line character.
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! public methods
  elemental subroutine snippet_clean(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Clean snippet.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(inout) :: self !< The snippet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(self%name)) deallocate(self%name)
  self%time = 0._R8P
  if (allocated(self%tic_toc_)) deallocate(self%tic_toc_)
  self%count_rate_ = 0
  self%tic_toc_number_ = 0
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine snippet_clean

  subroutine snippet_print(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print time of the snippet.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(in) :: self !< The snippet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  print '(A)', 'Elapsed time into "'//self%name//'": '//trim(str(self%time, .true.))//' [s]'
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine snippet_print

  pure function snippet_statistics(self, prefix, zpad) result(statistics)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return snippet statistics.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(in)           :: self       !< The timer.
  character(*),   intent(in), optional :: prefix     !< Prefixing string.
  integer(I4P),   intent(in), optional :: zpad       !< Zero padding of hits number counter.
  character(len=:), allocatable        :: statistics !< Timer statistics.
  character(len=:), allocatable        :: prefix_    !< Prefixing string, local variable.
  real(R8P)                            :: time       !< Snippets whole time.
  integer(I4P)                         :: zpad_      !< Zero padding of hits number counter, local variable.
  integer(I4P)                         :: h          !< Counter
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefix_ = '' ; if (present(prefix)) prefix_ = prefix
  zpad_ = 3 ; if (present(zpad)) zpad_ = zpad
  statistics = ''
  if (self%tic_toc_number_>0) then
    statistics = prefix_//'Number of snippet hits: '//trim(str(self%tic_toc_number_, .true.))
    statistics = statistics//NL//prefix_//'Total elapsed time: '//trim(str(self%time, .true.))//' [s]'
    statistics = statistics//NL//prefix_//'Average elapsed time: '//trim(str(self%time/self%tic_toc_number_, .true.))//' [s]'
    statistics = statistics//NL//prefix_//'Relative elapsed time into each hit:'
    do h=1, self%tic_toc_number_
      time = real(self%tic_toc_(2, h) - self%tic_toc_(1, h), kind=R8P)/self%count_rate_
      statistics = statistics//NL//prefix_//'  + '//trim(strz(h, zpad_))//': '//trim(str('(F6.3)', time/self%time*100))//'%'
    enddo
    statistics = statistics//NL
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction snippet_statistics

  subroutine snippet_tic(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Start tracking.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(inout) :: self         !< The snippet.
  integer(I8P), allocatable     :: tic_toc(:,:) !< Tic toc storage.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (self%tic_toc_number_>0) then
    allocate(tic_toc(1:2, 1:self%tic_toc_number_+1))
    tic_toc(:, 1:self%tic_toc_number_) = self%tic_toc_
    self%tic_toc_number_ = self%tic_toc_number_ + 1
    call move_alloc(from=tic_toc, to=self%tic_toc_)
  else
    self%tic_toc_number_ = 1
    allocate(self%tic_toc_(1:2, 1:self%tic_toc_number_))
  endif
  self%tic_toc_(1:2, self%tic_toc_number_) = 0
  call system_clock(self%tic_toc_(1, self%tic_toc_number_), self%count_rate_)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine snippet_tic

  subroutine snippet_toc(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Stop tracking.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(inout) :: self !< The snippet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call system_clock(self%tic_toc_(2, self%tic_toc_number_), self%count_rate_)
  self%time = self%time + &
              real(self%tic_toc_(2, self%tic_toc_number_) - self%tic_toc_(1, self%tic_toc_number_), kind=R8P)/self%count_rate_
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine snippet_toc

  elemental function snippet_tic_toc_number(self) result(tic_toc_number)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Return snippet statistics.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(in) :: self           !< The timer.
  integer(I4P)               :: tic_toc_number !< Tic toc pairs number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  tic_toc_number = self%tic_toc_number_
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction snippet_tic_toc_number
endmodule fitter_snippet
