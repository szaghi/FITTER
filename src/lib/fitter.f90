!< FITTER, Fortran tIc Toc TimER

module fitter
!< FITTER, Fortran tIc Toc TimER
use fitter_snippet
use penf

implicit none
private
public :: timer

type :: timer
   !< **Timer** class.
   integer(I4P),  private              :: scratch_unit_=0          !< Scratch file unit.
   logical,       private              :: is_scratch_open_=.false. !< Sentinel of scratch file status.
   type(snippet), private, allocatable :: snippets_(:)             !< Snippets tracked.
   integer(I4P),  private              :: snippets_number_=0       !< Snippets number.
   contains
      ! public methods
      procedure, pass(self) :: analyze     !< Analyze timer collected data.
      procedure, pass(self) :: clean       !< Clean timer.
      procedure, pass(self) :: description !< Return pretty formatted timer description string.
      procedure, pass(self) :: statistics  !< Return pretty formatted timer statistics.
      procedure, pass(self) :: tic         !< Start a snippet tracking.
      procedure, pass(self) :: time        !< Get time of a snippet or whole time.
      procedure, pass(self) :: times       !< Get time-array of all snippets.
      procedure, pass(self) :: toc         !< Stop a snippet tracking.
      ! private methods
      procedure, pass(self), private :: add_snippet   !< Add new snippet.
      procedure, pass(self), private :: clean_stats   !< Clean timer statistics data.
      procedure, pass(self), private :: snippet_index !< Return the snippet index given the name.
endtype timer

character(1), parameter :: NL=new_line('a') !< New line character.
contains
   ! public methods
   subroutine analyze(self)
   !< Analyze timer collected data.
   !<
   !< Timer data are stored into the scratch file that is properly parsed.
   class(timer), intent(inout) :: self            !< The timer.
   character(3)                :: act             !< Timing action, tic or toc.
   character(99)               :: snippet_name    !< Snippet name.
   integer(I8P)                :: snippet_tic_toc !< Snippet timing.
   integer(I4P), save          :: s               !< Counter

   if (self%is_scratch_open_) then
      call self%clean_stats
      rewind(unit=self%scratch_unit_)
      do
         snippet_tic_toc = 0
         read(unit=self%scratch_unit_, fmt=*, end=10) act, snippet_name, snippet_tic_toc
         select case(act)
         case('tic')
            s = self%snippet_index(name=trim(snippet_name))
            if (s == 0) then ! create new snippet and add new tic
               call self%add_snippet(name=trim(snippet_name))
               s = self%snippets_number_
               call self%snippets_(s)%tic(tic_count=snippet_tic_toc)
            else ! add new tic to existing snippet
               call self%snippets_(s)%tic(tic_count=snippet_tic_toc)
            endif
         case('toc')
            ! add new toc to last opened snippet tic
            call self%snippets_(s)%toc(toc_count=snippet_tic_toc)
         endselect
      enddo
      10 continue
   endif
   endsubroutine analyze

   subroutine clean(self)
   !< Clean timer.
   class(timer), intent(inout) :: self !< The timer.

   self%scratch_unit_ = 0
   if (self%is_scratch_open_) close(unit=self%scratch_unit_) ; self%is_scratch_open_ = .false.
   call self%clean_stats
   endsubroutine clean

   pure function description(self, name, statistics, zpad) result(desc)
   !< Return pretty formatted timer description string.
   !<
   !< @note Timer data must be already analyzed before calling this method.
   class(timer), intent(in)           :: self       !< The timer.
   character(*), intent(in), optional :: name       !< Snippet name.
   logical,      intent(in), optional :: statistics !< Print statistics.
   integer(I4P), intent(in), optional :: zpad       !< Zero padding for snippet sequential naming.
   character(len=:), allocatable      :: desc       !< Pretty formatted timer description.
   integer(I4P)                       :: s          !< Counter

   desc = ''
   if (self%snippets_number_ > 0) then
      if (present(name)) then
         s = self%snippet_index(name=name)
         if (s > 0) desc = desc//self%snippets_(s)%description()
      else
         do s=1, self%snippets_number_
            desc = desc//NL//self%snippets_(s)%description()
         enddo
      endif
      if (present(statistics)) then
         if (statistics) desc = desc//NL//self%statistics(zpad=zpad)
      endif
   endif
   endfunction description

   pure function statistics(self, zpad)
   !< Return timer statistics.
   !<
   !< @note Timer data must be already analyzed before calling this method.
   class(timer), intent(in)           :: self       !< The timer.
   integer(I4P), intent(in), optional :: zpad       !< Zero padding for snippet sequential naming.
   character(len=:), allocatable      :: statistics !< Timer statistics.
   real(R8P)                          :: time       !< Snippets whole time.
   integer(I4P)                       :: s          !< Counter

   statistics = ''
   if (self%snippets_number_ > 0) then
      time = self%time()
      statistics = 'number of snippets tracked: '//trim(str(self%snippets_number_, .true.))//NL
      statistics = statistics//'total elapsed time: '//trim(str(time, .true.))//' [s]'//NL
      statistics = statistics//'average (snippet) elapsed time: '//trim(str(time/self%snippets_number_, .true.))//' [s]'//NL
      statistics = statistics//'relative elapsed time into each snippet:'//NL
      do s=1, self%snippets_number_
         statistics = statistics//'  + '//self%snippets_(s)%name//': '//&
            trim(str('(F6.3)', self%snippets_(s)%time/time * 100))//'%'//NL
         if (self%snippets_(s)%tic_toc_number() > 1) statistics = statistics//self%snippets_(s)%statistics(prefix='    ', zpad=zpad)
      enddo
   endif
   endfunction statistics

   subroutine tic(self, name, zpad)
   !< Start a snippet tracking.
   class(timer), intent(inout)        :: self  !< The timer.
   character(*), intent(in), optional :: name  !< Snippet name.
   integer(I4P), intent(in), optional :: zpad  !< Zero padding for snippet sequential naming.
   character(len=:), allocatable      :: name_ !< Snippet name, local variable.
   integer(I4P)                       :: zpad_ !< Zero padding for snippet sequential naming, local variable.
   integer(I8P)                       :: tic_  !< Tic storage.

   if (.not.self%is_scratch_open_) open(newunit=self%scratch_unit_, status='scratch', form='formatted')
   self%is_scratch_open_ = .true.
   zpad_ = 3 ; if (present(zpad)) zpad_ = zpad
   if (present(name)) then
      name_ = name
   else
      self%snippets_number_ = self%snippets_number_ + 1
      name_ = 'snippet-'//trim(strz(self%snippets_number_, zpad_))
   endif
   call system_clock(tic_)
   write(unit=self%scratch_unit_, fmt='(2A,1X,'//FI8P//')') "'tic'", " '"//name_//"'", tic_
   endsubroutine tic

   pure function time(self, name)
   !< Get time of a snippet or whole time.
   !<
   !< @note Timer data must be already analyzed before calling this method.
   class(timer), intent(in)           :: self !< The timer.
   character(*), intent(in), optional :: name !< Snippet name.
   real(R8P)                          :: time !< Snippet(s) time.
   integer(I4P)                       :: s    !< Counter

   time = 0._R8P
   if (self%snippets_number_ > 0) then
      if (present(name)) then
         s = self%snippet_index(name=name)
         if (s > 0) time = self%snippets_(s)%time
      else
         do s=1, self%snippets_number_
            time = time + self%snippets_(s)%time
         enddo
      endif
   endif
   endfunction time

   pure function times(self)
   !< Get time-array of all snippets.
   !<
   !< @note Timer data must be already analyzed before calling this method.
   class(timer), intent(in) :: self     !< The timer.
   real(R8P), allocatable   :: times(:) !< Snippets time.
   integer(I4P)             :: s        !< Counter

   if (self%snippets_number_ > 0) then
      allocate(times(1:self%snippets_number_))
      do s=1, self%snippets_number_
         times(s) = self%snippets_(s)%time
      enddo
   endif
   endfunction times

   subroutine toc(self)
   !< Stop a snippet tracking.
   class(timer), intent(inout) :: self !< The timer.
   integer(I8P)                :: toc_ !< Toc storage.

   call system_clock(toc_)
   write(unit=self%scratch_unit_, fmt='(2A,1X,'//FI8P//')') "'toc'", " 'unused'", toc_
   endsubroutine toc

   ! private methods
   pure subroutine add_snippet(self, name)
   !< Add new snippet.
   class(timer), intent(inout) :: self        !< The timer.
   character(*), intent(in)    :: name        !< Snippet name.
   type(snippet), allocatable  :: snippets(:) !< The new snippet.
   integer(I4P)                :: s           !< Counter

   if (self%snippets_number_ > 0) then
      s = self%snippet_index(name=name)
      if (s == 0) then
         ! add new snippet
         allocate(snippets(1:self%snippets_number_+1))
         snippets(1:self%snippets_number_) = self%snippets_
         snippets(self%snippets_number_+1) = snippet(name=name)
         call move_alloc(from=snippets, to=self%snippets_)
         self%snippets_number_ = self%snippets_number_ + 1
      endif
   else
      ! there are not snippets at all, add the first one
      allocate(self%snippets_(1:1))
      self%snippets_(1) = snippet(name=name)
      self%snippets_number_ = self%snippets_number_ + 1
   endif
   endsubroutine add_snippet

   elemental subroutine clean_stats(self)
   !< Clean timer statistics data.
   class(timer), intent(inout) :: self !< The timer.

   if (allocated(self%snippets_)) then
      call self%snippets_%clean
      deallocate(self%snippets_)
   endif
   self%snippets_number_ = 0
   endsubroutine clean_stats

   elemental function snippet_index(self, name)
   !< Return the snippet index given the name.
   !<
   !< Return 0 if not present
   class(timer), intent(in) :: self          !< The timer.
   character(*), intent(in) :: name          !< Snippet name.
   integer(I4P)             :: snippet_index !< Snippet index, 0 is not present.
   integer(I4P)             :: s             !< Counter

   snippet_index = 0
   if (self%snippets_number_ > 0) then
      do s=1, self%snippets_number_
         if (self%snippets_(s)%name==name) then
            snippet_index = s
            exit
         endif
      enddo
   endif
   endfunction snippet_index
endmodule fitter
