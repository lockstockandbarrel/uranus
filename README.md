# Fortran Examples

## Timing

### timing/

Timing entire applications is best suited to profiling tools such as gprof(1).

However, instrumentation and compiler options required for profiling
can make tools like gprof(1) inappropriate or overkill for some simple
situations.

But Fortran does include a few intrinsics useful for basic macro-level
timing. They are simplistic and not generally a good choice for testing
timing of short events but can generally be left in the code to provide
general run-time timing information when used judiciously.

+ [system_clock](https://github.com/urbanjost/M_intrinsics/blob/master/md/SYSTEM_CLOCK.md)
+ [cpu_time](https://github.com/urbanjost/M_intrinsics/blob/master/md/CPU_TIME.md)
+ [date_and_time](https://github.com/urbanjost/M_intrinsics/blob/master/md/DATE_AND_TIME.md)

The resolution (or even availability) of the procedures is totally
dependent on the hardware they are running on, so take note it varies
widely. On some hardware (typically GPUs) there may not even be a
user-callable clock procedure, in which case all that may be available
is the date and time. Even that might not be on some specialized hardware!

The "timing" example included here provides a simple module using the
built-in Fortran intrinsics for simply instrumenting a region of code
to get macro-level timing information.

So to time sqrt(x) versus x**0.5, for example:
```fortran
program testit
use M_tictoc, only : timer
real,volatile     :: sum
integer           :: i
integer,parameter :: how_many_times=100000000
type(timer)       :: clock

clock=timer()

sum=0.0
call clock%tic()
do i=1,how_many_times
   sum=sum+sqrt(real(i))
enddo 
write(*,*)sum
call clock%toc()
call clock%print()

sum=0.0
call clock%tic()
do i=1,how_many_times
   sum=sum+i**0.5
enddo 
write(*,*)sum
call clock%toc()
call clock%print()

end program testit
```
```text
   2.74877907E+11
Elapsed date (sec) ::0.40301531553268433
Elapsed time (sec) ::0.402509302
CPU time     (sec) ::0.264999986
Percentage         :: 65.84
   2.74877907E+11
Elapsed date (sec) ::2.6180102825164795
Elapsed time (sec) ::2.61780310
CPU time     (sec) ::2.15599990
Percentage         :: 82.36
```
So maybe there is a reason there is a redundant-looking procedure for
square roots! (Note some compilers will recognize the mathematical 
equivalency and call an optimal solution).

A larger example shows setting up a skeleton for timing many different
versions of a procedure.

### fpm-time/

A plugin prototype for fpm(1) that helps run gprof(1). Install this
program and then in a simple fpm(1) package that has a test program run
```bash
   fpm time
```
```bash
   fpm time --help
```
describes how to use it when you have more than one test program.


### Timing parallel code

### User time versus System time

### Other commonly available C timing routines

### More comprehensive timing-related modules

  + [M_stopwatch](https://github.com/urbanjost/M_stopwatch)
  + [Beliavsky List](https://github.com/Beliavsky/Fortran-code-on-GitHub) lists many references to timing code
### 
