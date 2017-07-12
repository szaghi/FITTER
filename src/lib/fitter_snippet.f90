!< FITTER **snippet** class.

module fitter_snippet
!< FITTER **snippet** class.
use penf

implicit none
private
public :: snippet

type :: snippet
   !< **Snippet** class.
   character(len=:), allocatable      :: name              !< Name of the snippet.
   integer(I4P)                       :: level=0_I4P       !< Level of snippet nesting.
   real(R8P)                          :: time=0._R8P       !< Elapsed time in the snippet.
   integer(I8P), private, allocatable :: tic_toc_(:,:)     !< Tic toc storage.
   integer(I8P), private              :: count_rate_=0     !< Count rate.
   integer(I8P), private              :: tic_toc_number_=0 !< Tic toc pairs number.
   contains
      ! public methods
      procedure, pass(self) :: analyze        !< Analyze snippet timing data.
      procedure, pass(self) :: clean          !< Clean snippet.
      procedure, pass(self) :: description    !< Return pretty formatted snippet description string.
      procedure, pass(self) :: last_tic       !< Return last tic value.
      procedure, pass(self) :: last_toc       !< Return last toc value.
      procedure, pass(self) :: statistics     !< Return snippet statistics.
      procedure, pass(self) :: tic            !< Add new tic to the snippet.
      procedure, pass(self) :: tic_toc_number !< Return tic toc pairs number.
      procedure, pass(self) :: toc            !< Add new toc to last opened snippet tic.
endtype snippet

character(1), parameter :: NL=new_line('a') !< New line character.
contains
   ! public methods
   pure subroutine analyze(self)
   !< Analyze snippet timing data.
   class(snippet), intent(inout) :: self !< The snippet.
   integer(I4P)                  :: t    !< Counter.

   if (self%tic_toc_number_ > 0) then
      self%time = 0._R8P
      do t=1, self%tic_toc_number_
         self%time = self%time + real(self%tic_toc_(2, t) - self%tic_toc_(1, t), kind=R8P) / self%count_rate_
      enddo
   endif
   endsubroutine analyze

   elemental subroutine clean(self)
   !< Clean snippet.
   class(snippet), intent(inout) :: self !< The snippet.

   if (allocated(self%name)) deallocate(self%name)
   self%level = 0_I4P
   self%time = 0._R8P
   if (allocated(self%tic_toc_)) deallocate(self%tic_toc_)
   self%count_rate_ = 0
   self%tic_toc_number_ = 0
   endsubroutine clean

   pure function description(self, prefix) result(desc)
   !< Return pretty formatted snippet description string.
   !<
   !< @note Snippet data must be already analyzed before calling this method.
   class(snippet),   intent(in)           :: self    !< The snippet.
   character(len=*), intent(in), optional :: prefix  !< Prefixing string.
   character(len=:), allocatable          :: desc    !< Pretty formatted timer description.
   character(len=:), allocatable          :: prefix_ !< Prefixing string, local variable.

   prefix_ = '' ; if (present(prefix)) prefix_ = prefix
   desc = prefix_//'elapsed time into "'//self%name//'" (level='//trim(str(self%level, no_sign=.true.))//'): '//&
          trim(str(self%time, .true.))//' [s]'
   endfunction description

   elemental function last_tic(self)
   !< Return last tic value.
   class(snippet), intent(in) :: self     !< The timer.
   integer(I8P)               :: last_tic !< Last tic value.

   last_tic = 0
   if (self%tic_toc_number_ > 0) last_tic = self%tic_toc_(1, self%tic_toc_number_)
   endfunction last_tic

   elemental function last_toc(self)
   !< Return last toc value.
   class(snippet), intent(in) :: self     !< The timer.
   integer(I8P)               :: last_toc !< Last toc value.

   last_toc = 0
   if (self%tic_toc_number_ > 0) last_toc = self%tic_toc_(2, self%tic_toc_number_)
   endfunction last_toc

   pure function statistics(self, prefix, zpad, hits_time)
   !< Return snippet statistics.
   !<
   !< @note Snippet data must be already analyzed before calling this method.
   class(snippet), intent(in)           :: self       !< The timer.
   character(*),   intent(in), optional :: prefix     !< Prefixing string.
   integer(I4P),   intent(in), optional :: zpad       !< Zero padding of hits number counter.
   logical,        intent(in), optional :: hits_time  !< Add relative time consumed for each snippet hit.
   character(len=:), allocatable        :: statistics !< Timer statistics.
   character(len=:), allocatable        :: prefix_    !< Prefixing string, local variable.
   real(R8P)                            :: time       !< Snippets whole time.
   integer(I4P)                         :: zpad_      !< Zero padding of hits number counter, local variable.
   logical                              :: hits_time_ !< Add relative time consumed for each snippet hit, local variable.
   integer(I4P)                         :: h          !< Counter

   prefix_ = '' ; if (present(prefix)) prefix_ = prefix
   zpad_ = 3 ; if (present(zpad)) zpad_ = zpad
   hits_time_ = .false. ; if (present(hits_time)) hits_time_ = hits_time
   statistics = ''
   if (self%tic_toc_number_ > 0 .and. hits_time_) then
      statistics = NL//prefix_//'number of snippet hits: '//trim(str(self%tic_toc_number_, .true.))
      statistics = statistics//NL//prefix_//'total elapsed time: '//trim(str(self%time, .true.))//' [s]'
      statistics = statistics//NL//prefix_//'average elapsed time: '//trim(str(self%time/self%tic_toc_number_, .true.))//' [s]'
      statistics = statistics//NL//prefix_//'relative elapsed time into each hit:'
      do h=1, self%tic_toc_number_
         time = real(self%tic_toc_(2, h) - self%tic_toc_(1, h), kind=R8P) / self%count_rate_
         statistics = statistics//NL//prefix_//'  + '//trim(strz(h, zpad_))//': '//trim(str('(F7.3)', time/self%time*100))//'%'
      enddo
   endif
   endfunction statistics

   subroutine tic(self, tic_count)
   !< Add new tic to the snippet.
   class(snippet), intent(inout) :: self         !< The snippet.
   integer(I8P),   intent(in)    :: tic_count    !< Snippet tic timing.
   integer(I8P), allocatable     :: tic_toc(:,:) !< Tic toc storage.
   integer(I8P)                  :: temporary    !< Temporary variable.

   if (self%tic_toc_number_ > 0) then
      allocate(tic_toc(1:2, 1:self%tic_toc_number_+1))
      tic_toc(:, 1:self%tic_toc_number_) = self%tic_toc_
      call move_alloc(from=tic_toc, to=self%tic_toc_)
      self%tic_toc_number_ = self%tic_toc_number_ + 1
   else
      self%tic_toc_number_ = 1
      allocate(self%tic_toc_(1:2, 1:self%tic_toc_number_))
      call system_clock(temporary, self%count_rate_) ! only for store the value of count rate
   endif
   self%tic_toc_(1, self%tic_toc_number_) = tic_count
   endsubroutine tic

   elemental function tic_toc_number(self)
   !< Return snippet statistics.
   class(snippet), intent(in) :: self           !< The timer.
   integer(I4P)               :: tic_toc_number !< Tic toc pairs number.

   tic_toc_number = self%tic_toc_number_
   endfunction tic_toc_number

   pure subroutine toc(self, toc_count)
   !< Add new toc to last opened snippet tic.
   class(snippet), intent(inout) :: self      !< The snippet.
   integer(I8P),   intent(in)    :: toc_count !< Snippet toc timing.

   if (self%tic_toc_number_ > 0) then
      self%tic_toc_(2, self%tic_toc_number_) = toc_count
   endif
   call self%analyze
   endsubroutine toc
endmodule fitter_snippet
