!< FITTER test.

program fitter_test_multiple_hits
!< FITTER test.
use, intrinsic :: iso_fortran_env, only : real64
use fitter

implicit none
type(timer) :: chronos !< The timer.
integer     :: s       !< Counter.

call chronos%tic(name='foo')
call foo
call chronos%toc

do s=1, 4
   call chronos%tic(name='bar')
   call bar
   call chronos%toc
enddo

call chronos%tic(name='foo')
call foo
call chronos%toc

call chronos%analyze
print '(A)', chronos%description(statistics=.true., hits_time=.true.)
contains
   subroutine foo
   !< Waste some times.
   integer :: i !< Counter.
   real    :: r !< Dummy real.

   do i=1, 100000
      r = i**0.3
   enddo
   endsubroutine foo

   subroutine bar
   !< Waste some more times.
   integer :: i !< Counter.
   real    :: r !< Dummy real.

   do i=1, 150000
      r = i**0.3
   enddo
   endsubroutine bar
endprogram fitter_test_multiple_hits
