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
   type(snippet), allocatable  :: tic_toc(:)      !< Tic-toc array.
   integer(I4P)                :: s, t               !< Counter.

   if (self%is_scratch_open_) then
      call self%clean_stats
      call get_tic_toc(tic_toc=tic_toc)
      do t=1, size(tic_toc, dim=1)
         s = self%snippet_index(name=tic_toc(t)%name)
         if (s == 0) then ! create new snippet and add new tic/toc
            call self%add_snippet(name=tic_toc(t)%name, level=tic_toc(t)%level)
            s = self%snippets_number_
         endif
         call self%snippets_(s)%tic(tic_count=tic_toc(t)%last_tic())
         call self%snippets_(s)%toc(toc_count=tic_toc(t)%last_toc())
      enddo
   endif
   if (self%snippets_number_ > 0) call sort(first=1, last=self%snippets_number_)
   contains
      function max_tic_toc_nesting_level()
      !< Return the the maximum level of tic-toc nesting.
      integer(I4P) :: max_tic_toc_nesting_level !< Maximum level of tic-toc nesting.
      character(3) :: act                       !< Timing action, tic or toc.
      integer(I4P) :: level                     !< Current level of tic-toc nesting.
      character(3) :: previous                  !< Previous status.

      previous = 'nul'
      max_tic_toc_nesting_level = 0
      level = 0
      rewind(unit=self%scratch_unit_)
      do
         read(unit=self%scratch_unit_, fmt=*, end=10) act
         select case(act)
         case('tic')
            if (previous=='tic'.or.previous=='nul') level = level + 1
            previous = 'tic'
            max_tic_toc_nesting_level = max(max_tic_toc_nesting_level, level)
         case('toc')
            if (previous=='toc') level = level - 1
            previous = 'toc'
         endselect
      enddo
      10 rewind(unit=self%scratch_unit_)
      endfunction max_tic_toc_nesting_level

      subroutine get_tic_toc(tic_toc)
      !< Return tic-toc pairs arrays stored into scratch file.
      type(snippet), allocatable, intent(out) :: tic_toc(:)      !< Tic array.
      character(3)                            :: act             !< Timing action, tic or toc.
      character(99)                           :: snippet_name    !< Snippet name.
      integer(I8P)                            :: snippet_tic_toc !< Snippet timing.
      type(snippet), allocatable              :: tic_l(:,:)      !< Tic array organized by nested levels.
      type(snippet), allocatable              :: toc_l(:,:)      !< Toc array organized by nested levels.
      integer(I4P)                            :: tic_toc_num     !< Tic-toc pairs number.
      character(3)                            :: previous        !< Previous status.
      integer(I4P)                            :: max_level       !< Max level of tic-toc nesting (concurrent tic-toc).
      integer(I4P)                            :: l, level        !< Level counter.
      integer(I4P), allocatable               :: t1(:), t2(:)    !< Counter.
      integer(I4P)                            :: i, t            !< Counter.

      tic_toc_num = tic_toc_number()
      max_level = max_tic_toc_nesting_level()
      allocate(t1(1:max_level))
      allocate(t2(1:max_level))
      allocate(tic_l(1:max_level, 1:tic_toc_num))
      allocate(toc_l(1:max_level, 1:tic_toc_num))
      previous = 'nul'
      level = 0
      t1 = 0
      t2 = 0
      rewind(unit=self%scratch_unit_)
      do
         read(unit=self%scratch_unit_, fmt=*, end=10) act, snippet_name, snippet_tic_toc
         select case(act)
         case('tic')
            if (previous=='tic'.or.previous=='nul') level = level + 1
            previous = 'tic'
            t1(level) = t1(level) + 1
            tic_l(level, t1(level)) = snippet(name=trim(snippet_name), level=level)
            call tic_l(level, t1(level))%tic(tic_count=snippet_tic_toc)
         case('toc')
            if (previous=='toc') level = level - 1
            previous = 'toc'
            t2(level) = t2(level) + 1
            toc_l(level, t2(level)) = snippet(name=trim(snippet_name), level=level)
            call toc_l(level, t2(level))%tic(tic_count=0_I8P) ! only to initialize snippet timing
            call toc_l(level, t2(level))%toc(toc_count=snippet_tic_toc)
         endselect
      enddo
      10 rewind(unit=self%scratch_unit_)

      allocate(tic_toc(1:tic_toc_num))
      i = 0
      do t=1, tic_toc_num
         do l=1, max_level
            if (allocated(tic_l(l, t)%name)) then
               i = i + 1
               tic_toc(i) = tic_l(l, t)
               call tic_toc(i)%toc(toc_count=toc_l(l, t)%last_toc())
            endif
         enddo
      enddo
      endsubroutine get_tic_toc

      recursive subroutine sort(first, last)
      !< Sort snippets from heaviest to lightest.
      integer(I4P), intent(in) :: first, last !< Current snippets extensions.
      integer(I4P)             :: i, j        !< Counter.
      real(R8P)                :: mean        !< Mean time.
      type(snippet)            :: temporary   !< Temporary snippet buffer.

      mean = self%snippets_((first+last) / 2)%time
      i = first
      j = last
      do
         do while(self%snippets_(i)%time > mean)
            i = i + 1
         enddo
         do while(mean > self%snippets_(j)%time)
            j = j - 1
         enddo
         if (i >= j) exit
         temporary = self%snippets_(i) ; self%snippets_(i) = self%snippets_(j) ; self%snippets_(j) = temporary
         i = i + 1
         j = j - 1
      enddo
      if (first < i-1) call sort(first=first, last=i-1 )
      if (j+1 < last)  call sort(first=j+1,   last=last)
      endsubroutine sort

      function tic_toc_number()
      !< Return the number of tic-toc pairs stored into scratch file.
      integer(I4P) :: tic_toc_number !< Tic-toc pairs number.

      tic_toc_number = 0
      rewind(unit=self%scratch_unit_)
      do
         tic_toc_number = tic_toc_number + 1
         read(unit=self%scratch_unit_, fmt=*, end=10)
      enddo
      10 rewind(unit=self%scratch_unit_)
      tic_toc_number = tic_toc_number / 2
      endfunction tic_toc_number
   endsubroutine analyze

   subroutine clean(self)
   !< Clean timer.
   class(timer), intent(inout) :: self !< The timer.

   self%scratch_unit_ = 0
   if (self%is_scratch_open_) close(unit=self%scratch_unit_) ; self%is_scratch_open_ = .false.
   call self%clean_stats
   endsubroutine clean

   pure function description(self, name, statistics, zpad, hits_time) result(desc)
   !< Return pretty formatted timer description string.
   !<
   !< @note Timer data must be already analyzed before calling this method.
   class(timer), intent(in)           :: self       !< The timer.
   character(*), intent(in), optional :: name       !< Snippet name.
   logical,      intent(in), optional :: statistics !< Print statistics.
   integer(I4P), intent(in), optional :: zpad       !< Zero padding for snippet sequential naming.
   logical,      intent(in), optional :: hits_time  !< Add relative time consumed for each snippet hit.
   character(len=:), allocatable      :: desc       !< Pretty formatted timer description.
   integer(I4P)                       :: s          !< Counter

   if (self%snippets_number_ > 0) then
      if (present(name)) then
         s = self%snippet_index(name=name)
         if (s > 0) desc = self%snippets_(s)%description()
      else
         if (present(statistics)) then
            if (statistics) desc = self%statistics(zpad=zpad, hits_time=hits_time)
         else
            desc = ''
            do s=1, self%snippets_number_
               desc = desc//self%snippets_(s)%description()//NL
            enddo
         endif
      endif
   endif
   endfunction description

   pure function statistics(self, zpad, hits_time)
   !< Return timer statistics.
   !<
   !< @note Timer data must be already analyzed before calling this method.
   class(timer), intent(in)           :: self       !< The timer.
   integer(I4P), intent(in), optional :: zpad       !< Zero padding for snippet sequential naming.
   logical,      intent(in), optional :: hits_time  !< Add relative time consumed for each snippet hit.
   character(len=:), allocatable      :: statistics !< Timer statistics.
   real(R8P)                          :: time       !< Snippets whole time.
   integer(I4P)                       :: s          !< Counter

   statistics = ''
   if (self%snippets_number_ > 0) then
      time = self%time()
      statistics = 'number of snippets tracked: '//trim(str(self%snippets_number_, .true.))//NL
      statistics = statistics//'total elapsed time: '//trim(str(time, .true.))//' [s]'//NL
      statistics = statistics//'average (snippet) elapsed time: '//trim(str(time/self%snippets_number_, .true.))//' [s]'//NL
      statistics = statistics//'relative elapsed time into each snippet:'
      do s=1, self%snippets_number_
         statistics = statistics//NL//'  + '//self%snippets_(s)%name//               &
            ' (level='//trim(str(self%snippets_(s)%level, no_sign=.true.)) //'): '// &
            trim(str(self%snippets_(s)%time, .true.))//' [s], '//                    &
            trim(str('(F7.3)', self%snippets_(s)%time/time * 100))//'%'
         if (self%snippets_(s)%tic_toc_number() > 1) &
            statistics = statistics//self%snippets_(s)%statistics(prefix='    ', zpad=zpad, hits_time=hits_time)
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
            if (self%snippets_(s)%level==1) time = time + self%snippets_(s)%time
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
   pure subroutine add_snippet(self, name, level)
   !< Add new snippet.
   class(timer), intent(inout) :: self        !< The timer.
   character(*), intent(in)    :: name        !< Snippet name.
   integer(I4P), intent(in)    :: level       !< Level of nesting.
   type(snippet), allocatable  :: snippets(:) !< The new snippet.
   integer(I4P)                :: s           !< Counter

   if (self%snippets_number_ > 0) then
      s = self%snippet_index(name=name)
      if (s == 0) then
         ! add new snippet
         allocate(snippets(1:self%snippets_number_+1))
         snippets(1:self%snippets_number_) = self%snippets_
         snippets(self%snippets_number_+1) = snippet(name=name, level=level)
         call move_alloc(from=snippets, to=self%snippets_)
         self%snippets_number_ = self%snippets_number_ + 1
      endif
   else
      ! there are not snippets at all, add the first one
      allocate(self%snippets_(1:1))
      self%snippets_(1) = snippet(name=name, level=level)
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
