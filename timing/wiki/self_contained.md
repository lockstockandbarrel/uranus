## Timing

In-depth timing of entire applications is best suited to profiling tools
such as the commonly available gprof(1) command and vendor-supplied
tooling and utilities designed for that purpose.

However, instrumentation affects and compiler options required
for profiling along with  portability issues can make tools like gprof(1)
inappropriate or overkill for some simple situations.

So for those times where do-it-yourself timing is the better choice you
will find a few of the most useful standard Fortran intrinsics for basic
macro-level timing are

+ [system_clock](https://github.com/urbanjost/M_intrinsics/blob/master/md/SYSTEM_CLOCK.md)
+ [cpu_time](https://github.com/urbanjost/M_intrinsics/blob/master/md/CPU_TIME.md)
+ [date_and_time](https://github.com/urbanjost/M_intrinsics/blob/master/md/DATE_AND_TIME.md)

It is not much of an overstatement to say these routines  are
implementation-dependent and often generally not a good choice for
testing timing of short events.

On the other hand, 

 + the calls can generally be left in the code to
   provide general run-time timing information when used judiciously,
   (ie. at a macro level).
   
 + If already using pre-processors it is usually very easy to make the
   calls only in development and debug versions and to selectively exclude
   them in production releases to assist opimization.
   
 + they are usually a great tool for timing throw-away development programs
   used to time different versions of a procedure you already have identified
   as being important to optimize.
   
 + they can be used portably in testing frameworks to capture and identify
   large changes in performance between releases and different compilers
   and compiler switch affects.
   
 + Since they are intrinsics a big advantage is that they are available
   on virtually all platforms, unlike most higher-level profiling
   packages.

Note that calls to any procedures that call the underlying operating
system such as timing routines (and even I/O and practically any call that
queries the sysetm) can have a major detrimental impact on optimization
when placed in loops and can cause parallel threads to lock or sync
while waiting on a system response (see "mutex" (mutually exclusive lock)
calls, for example).

#### NOTE
It is usually a good idea to run strace(1) where available on
performance-critical codes that should be floating-point intensive to
see if a lot of system calls are being made that can be eliminated. They
very often cause locks and interupts that slow performance.

The resolution (or even availability) of the procedures is totally
dependent on the hardware they are running on, so take note the resolution
available for timing varies widely. On some hardware (typically GPUs)
there may not even be a user-callable clock procedure, in which case all
that may be available is the date and time from the main code. Even that
might not be on some specialized hardware!

But the timing routines are usually available and functional. It is
still useful to package and wrap them in a module so they can easily be
changed to suite different platforms.  An example of such a wrapper is
included here.

A simple example program calling the module illustrates using it
to time a loop of calls. 

```fortran
program testit
use M_tictoc,only : timer, say_hello
!
! using a volatile variable usually keeps compilers from
! optimizing away a loop
real,volatile               :: sum
!
! a more accurate measure comes from running repeated
! calls till at least a few seconds have passed.
! therefore this number would vary depending on the platform.
integer,parameter           :: how_many_times=100000000

! create an instance of a timer
type(timer)                 :: clock

integer                     :: i
character(len=*),parameter  :: all='(*(g0,1x))'

! some useful info for record keeping
call say_hello()

! initialize the clock
clock=timer()

print all, 'time sqrt(r) versus r**0.5'

print all, 'time calls to sqrt:'
sum=0.0
call clock%tic()          ! <= start a timer
do i=1,how_many_times
   sum=sum+sqrt(real(i))
enddo
print all, sum
call clock%toc()          ! <= end the timer
call clock%print()        ! <= print the timing information

print all, 'time calls to x**0.5:'
sum=0.0
call clock%tic()
do i=1,how_many_times
   sum=sum+i**0.5
enddo
print all, sum
call clock%toc()
call clock%print()

end program testit
```
Expecting there to be no difference in timing?
On a lot of platforms there will be a rather large one
for 32-bit in particular. For example:
```text
run date.....: 2024-09-18T21:09:12-04:00
program name.:./testit
compiled by..: GCC version 13.2.1 20240426
using options:

 -mtune=generic 
 -march=x86-64

time sqrt(r) versus r**0.5

time calls to sqrt:
0.274877907E+12
Elapsed dat  (sec) ::.405
Elapsed time (sec) ::.405112200
CPU time     (sec) ::0.31200000000000000
Percentage         :: 77.02

time calls to x**0.5:
0.274877907E+12
Elapsed dat  (sec) ::2.586
Elapsed time (sec) ::2.585699600
CPU time     (sec) ::1.7660000000000000
Percentage         :: 68.30
```
So maybe there is a reason there is a redundant-looking procedure
for square roots (Note some compilers will recognize the mathematical
equivalency and call an optimal solution!).

### Other considerations

Timing can be more complicated for parallel code; there is the issue
of System time versus User time and Idle time; sometimes call counting
might be useful in lieu of timing; and there are many many other aspects
to optimizing code....  memory thrashing and high-water marks, excessive
system calls, I/O layout and cacheing, context switching .,, .

But, a little module might be handy for the many simpler cases ...

### A module defining a clock type and related procedures
```fortran
module M_tictoc
use,intrinsic :: iso_fortran_env, only : int32,int64,real32,dp=>real64
use,intrinsic :: iso_fortran_env, only : stdout=>OUTPUT_UNIT
implicit none
private

type timer
   real(kind=dp)   :: cpu_start
   real(kind=dp)   :: cpu_end
   integer(kind=int64) :: clock_start
   integer(kind=int64) :: clock_end
   integer             :: wall_start(8)
   integer             :: wall_end(8)
   contains
       procedure  ::  tic        =>  clock_tic
       procedure  ::  toc        =>  clock_toc
       procedure  ::  print      =>  clock_print
       procedure  ::  walltime   =>  clock_walltime
       procedure  ::  cputime    =>  clock_cputime
       procedure  ::  dattime    =>  clock_dattime
end type

interface timer
     procedure :: clock_new
end interface timer

! type for unix epoch time and julian days
integer,parameter,public   :: realtime=dp 

public :: timer
public :: say_hello

character(len=*),parameter :: gen='(*(g0))'
character(len=*),parameter  :: all='(*(g0,1x))'

contains

! initialization constructor
type(timer) function clock_new(this)
type(timer),intent(in),optional :: this

   call cpu_time(clock_new%cpu_start)
   call system_clock(clock_new%clock_start)
   call date_and_time(values=clock_new%wall_start)

   clock_new%cpu_end= clock_new%cpu_start
   clock_new%clock_end= clock_new%clock_start
   clock_new%wall_end= clock_new%wall_start

end function clock_new

subroutine clock_tic(this)
class(timer) :: this

   call cpu_time(this%cpu_start)
   call system_clock(this%clock_start)
   call date_and_time(values=this%wall_start)

   this%cpu_end   = this%cpu_start
   this%clock_end = this%clock_start
   this%wall_end  = this%wall_start

end subroutine clock_tic

subroutine clock_toc(this)
class(timer) :: this

   call cpu_time(this%cpu_end)
   call system_clock(this%clock_end)
   call date_and_time(values=this%wall_end)

end subroutine clock_toc

subroutine clock_print(this,string,lun)
class(timer),intent(in)                  :: this
character(len=*),intent(in),optional     :: string
integer(kind=int32),intent(in),optional  :: lun
integer(kind=int32)                      :: lun_
real(kind=dp)                            :: elapsed_time
real(kind=realtime)                      :: elapsed_date_and_time
real(kind=dp)                            :: cpu_time
character(len=105)                       :: biggest
integer(kind=int64)                      :: count_rate

   if(present(lun))then
      lun_=lun
   else
      lun_=stdout
   endif

   elapsed_time           =  this%walltime()
   elapsed_date_and_time  =  this%dattime()
   cpu_time               =  this%cputime()

   if(present(string)) write( lun_,gen ) string

   if(elapsed_date_and_time >= 0)then
      write( lun_,'(a,f0.3)')    'Elapsed dat  (sec) ::',elapsed_date_and_time
   else
      write( lun_,'(a)')         'Elapsed dat  (sec) :: N/A'
   endif

   ! try to make a reasonable format for the number of digits of precision
   call system_clock(count_rate=count_rate) ! Find the time rate
   write(biggest,'("(a,f0.",i0,")")')ceiling(log10(real(count_rate,kind=dp)))

   write( lun_,biggest)          'Elapsed time (sec) ::',elapsed_time
   write( lun_,gen)              'CPU time     (sec) ::',cpu_time
   write( lun_,'(a,1x,f0.2)')    'Percentage         ::',(cpu_time/elapsed_time)*100

end subroutine clock_print

function clock_walltime(this) result(elapsed_time)
class(timer)        :: this
integer(kind=int64) :: count_rate
real(kind=dp)       :: elapsed_time
real(kind=dp)       :: cpu_time
   call system_clock(count_rate=count_rate) 
   elapsed_time = real(this%clock_end-this%clock_start,kind=dp)/real(count_rate,kind=dp)
end function clock_walltime

function  clock_cputime(this)  result(cpu_time)
class(timer)         :: this
real(kind=dp)        :: cpu_time
   cpu_time = real(this%cpu_end-this%cpu_start,kind=dp)
end function clock_cputime

function  clock_dattime(this)  result(cpu_time)
class(timer)         :: this
real(kind=dp)        :: cpu_time
real(kind=realtime)  :: endit,startit
integer              :: ierr
   call date_to_julian(this%wall_end,endit,ierr)
   call date_to_julian(this%wall_start,startit,ierr)
   if(ierr == 0)then
      cpu_time = real((endit-startit)*86400,kind=dp)
   else
      cpu_time = -huge(cpu_time)
   endif
end function clock_dattime

subroutine date_to_julian(dat,julian,ierr)
! @(#)M_time::date_to_julian(3f): Converts proleptic Gregorian DAT date-time array to Julian Date
! REFERENCE: From Wikipedia, the free encyclopedia 2015-12-19
! correction for time zone should or should not be included?
integer,intent(in)               ::  dat(8)! array like returned by DATE_AND_TIME(3f)
real(kind=realtime),intent(out)  ::  julian
integer,intent(out) :: ierr ! 0 =successful, -1=bad year, -4=bad date 29 Feb, non leap-year, -6 negative value -9 bad input
integer                          ::  a , y , m , jdn
integer                          ::  utc
utc=dat(4)*60
julian = -huge(99999)               ! this is the date if an error occurs and IERR is < 0
if(any(dat == -huge(dat)))then
   ierr=-9
   return
endif
associate&
&(year=>dat(1),month=>dat(2),day=>dat(3),utc=>utc,hour=>dat(5),minute=>dat(6),second=>dat(7)-utc+dat(8)/1000.0d0)
   if ( year==0 .or. year<-4713 ) then
      ierr = -1
   else
      ierr=0
   !  You must compute first the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
      a = (14-month)/12    ! A will be 1 for January or February, and 0 for other months, with integer truncation
      y = year + 4800 - a
      m = month + 12*a - 3 ! M will be 0 for March and 11 for February
   !  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
   !  Convert to a negative number, then increment towards zero
   !  Staring from a Gregorian calendar date
      jdn = day + (153*m+2)/5 + 365*y + y/4 - y/100 + y/400 - 32045 !  with integer truncation
   !  Finding the Julian Calendar date given the JDN (Julian day number) and time of day
      julian = jdn + (hour-12)/24.0_realtime + (minute)/1440.0_realtime + second/86400.0_realtime
      ierr=merge(-6,ierr, julian<0.0_realtime ) ! Julian Day must be non-negative
   endif
end associate
end subroutine date_to_julian

subroutine say_hello()
use, intrinsic :: iso_fortran_env, only : compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_options
character(len=*),parameter :: all='(*(g0,1x))'
character(len=*),parameter :: chs='(*(g0))'
character(len=2)           :: ch, split
integer                      :: argument_length, istat, posix, dos, i
character(len=:),allocatable :: progname, options
   call get_command_argument(number=0,length=argument_length)
   if(allocated(progname))deallocate(progname)
   allocate(character(len=argument_length) :: progname)
   call get_command_argument (0, progname, status=istat)
   print all, 'run date.....:',iso_8601()
   if (istat == 0) then
      print all, "program name.:" // trim (progname)
   else
      print all, "Could not get the program name " // trim (progname)
   endif
   print all, 'compiled by..:', compiler_version()
   options=' '//compiler_options()
   if(options /= '')then
      print all, 'using options:'
      ! guess which one
      posix=0
      dos=0
      do i=2,len(options)
         ch=options(i-1:i)
         select case(ch)
         case(' -');    posix=posix+1
         case(' /');    dos=dos+1
         end select
      enddo
      split=merge(' -',' /',posix > 0)
      do i=2,len(options)
         ch=options(i-1:i)
         if(ch == split)then
            write(*,chs,advance='no')char(10),ch
         else
            write(*,chs,advance='no')ch(2:2)
         endif
      enddo
      print all
   endif
   print all
end subroutine say_hello

function iso_8601()
! return date using ISO   8601 format at a resolution of seconds
character(len=8)  :: dt
character(len=10) :: tm
character(len=5)  :: zone
character(len=25) :: iso_8601
   call date_and_time(dt, tm, zone)
   ISO_8601 = dt(1:4)//'-'//dt(5:6)//'-'//dt(7:8)   &
      & //'T'//                                     &
      & tm(1:2)//':'//tm(3:4)//':'//tm(5:6)         &
      & //zone(1:3)//':'//zone(4:5)
end function iso_8601

end module M_tictoc
```
