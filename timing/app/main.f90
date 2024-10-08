program demo_nospace
use M_hello,  only : say_hello
use M_tictoc, only : catstat, timer
use testprocedures, only : nospace1, nospace2, nospace3, nospace4
implicit none
character(len=*),parameter :: gen='(*(g0,1x))'
character(len=:),allocatable  :: s
type(timer)     :: clock

namelist /vals/ clock
! 
! To test variants of a procedure change HOW_MANY_TIMES
! and replace the NOSPACE* names with your procedure names and
! change the interface block to describe your procedure name

! the testprocedures module is where to put your variants
! or use some other module with the routines in it.

integer,parameter :: how_many_times=1000000

   call say_hello()

   ! first a little confidence test
   s='  This     is      a     test  '
   write(*,*) 'original input string is ....',s
   write(*,*) 'processed output string is ...',nospace1(s), nospace1(s).eq.'Thisisatest'
   write(*,*) 'processed output string is ...',nospace2(s), nospace2(s).eq.'Thisisatest'
   write(*,*) 'processed output string is ...',nospace3(s), nospace3(s).eq.'Thisisatest'
   write(*,*) 'processed output string is ...',nospace4(s), nospace4(s).eq.'Thisisatest'

   ! now some timing
   clock=timer()

   call timeit('BASELINE',baseline)
   call timeit('NOSPACE1',nospace1)
   call timeit('NOSPACE2',nospace2)
   call timeit('NOSPACE3',nospace3)
   call timeit('NOSPACE4',nospace4)

block
use,intrinsic :: iso_fortran_env, only : int8,int16,int32,int64,real32,real64,real128
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
character(len=*),parameter :: g='(*(g0,1x))'
integer(kind=int64) :: icount64, icount_rate64, icount_max64
real(kind=real64) :: frequency
   print g,':SYSTEM_CLOCK PRECISION:'
   call system_clock(icount64, icount_rate64, icount_max64)
   print g,'COUNT_MAX(64bit)='    ,  icount_max64                
   print g,'COUNT_RATE(64bit)='   ,  icount_rate64               
   frequency=real(icount_max64,kind=int64)/real(icount_rate64,kind=real64)
   print g,'FLIP EVERY N(secs)='  ,  frequency ,' (days)==>',frequency/86400_real64
   print g,'CURRENT COUNT(64bit)=' , icount64
   print g,'next flip(secs)=',dble(icount_max64-icount64)/icount_rate64,'(days)==>', dble(icount_max64-icount64)/icount_rate64/86400
endblock

   call catstat()
contains

subroutine timeit(label,procedure)
! OPTIMIZING LOOP AWAY

! assuming the loop itself is not complex and possibly in need of unrolling and so on
! and that it is so simple the compiler is optimizing it away because nothing in the 
! loop is used the loop may be identified as "dead code" and eliminated.
!
!  + passing the procedure tends to prevent the loop being optimized away
!  + using the volatile property also helps prevent loops from being optimized away
!  + a write statement after the loop might also help, as well
!  + finally, calling a function, particulary one writing values from a random pass helps

character(len=*),intent(in)            :: label
character(len=:),allocatable,volatile  :: t

!character(len=:),allocatable,external :: procedure
interface 
   function procedure(string)
   character(len=*),intent(in) :: string
   character(len=:),allocatable   ::  procedure 
   end function
end interface
integer :: i

   print gen,':'//label//':'
   call clock%tic()
   do i=1,how_many_times
     t=procedure('this     is  a     test')
   enddo
   call clock%toc()
   call clock%print()
   write(*,*)t

!   write(*,*)clock
!   write (*, nml=vals)  ! using NAMELIST creates easy-to-postprocess logs
!   print gen, 'CPU TIME = ', clock%cputime()

end subroutine timeit

function baseline(line)
character(len=*),intent(in)    ::  line
character(len=:),allocatable   ::  baseline
   baseline=line
end function baseline

end program demo_nospace
