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
  character(len=:), allocatable :: name               !< Name of the snippet.
  real(R8P)                     :: time=0._R8P        !< Elapsed time in the snippet.
  integer(I8P), private         :: tic_toc(1:2)=[0,0] !< Tic toc storage.
  integer(I8P), private         :: count_rate=0       !< Counte rate.
  contains
    ! public methods
    procedure, pass(self) :: clean => snippet_clean !< Clean snippet.
    procedure, pass(self) :: print => snippet_print !< Print time of the snippet.
    procedure, pass(self) :: start => snippet_start !< Start tracking.
    procedure, pass(self) :: stop  => snippet_stop  !< Stop tracking.
endtype snippet

interface snippet
  !< Overload [[snippet]] name with a creator procedure.
  module procedure snippet_careator
endinterface snippet
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! non TBP
  pure function snippet_creator(self, name, number) result(snip)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create a new instance of [[snippet]].
  !---------------------------------------------------------------------------------------------------------------------------------
  character(*), intent(in), optional :: name   !< Snippet name.
  integer(I4P), intent(in), optional :: number !< Snippet name.
  type(snippet)                      :: snip   !< The snippet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(name)) then
    self%name = name
  elseif (present(number)) then
    self%name = 'snippet-'//trim(str(number, .true.))
  endif
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction snippet_creator

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
  self%tic_toc(1:2) = [0,0]
  self%count_rate = 0
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine snippet_clean

  subroutine snippet_print(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print time of the snippet.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(in) :: self !< The snippet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  print '(A)', 'Elapsed time into "'//self%name'": '//trim(str(self%time, .true.))//' [s]'
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine snippet_print

  subroutine snippet_start(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Start tracking.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(inout) :: self !< The snippet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call system_clock(self%tic_toc(1), self%count_rate)
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine snippet_start

  subroutine snippet_stop(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Stop tracking.
  !---------------------------------------------------------------------------------------------------------------------------------
  class(snippet), intent(inout) :: self !< The snippet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call system_clock(self%tic_toc(2), self%count_rate)
  self%time = real(self%tic_toc(2) - self%tic_toc(1), kind=R8P)/self%count_rate
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine snippet_snippet
endmodule fitter_snippet
