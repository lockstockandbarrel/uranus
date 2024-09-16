# Fortran Examples

## Timing

### timing/

Timing entire applications is best suited to profiling tools such as gprof(1).

Instrumentation and compiler options required for profiling can make tools
like gprof(1) inappropriate or overkill for some simple situations. 

But Fortran does include a few intrinsics useful for basic macro-level
timing. They are simplistic and not generally a good choice for testing
timing of short events but can generally be left in the code to provide
general run-time timing information when used judiciously.

The resolution of the procedures is totally dependent on the hardware they
are running on, so take not it varies widely. On some hardware (typically
GPUs) there may not even be a user-callable clock procedure, in which case
all that may be available is the date and time.

The "timing" example provides a simple module using the built-in Fortran
intrinsics for simply instrumenting a region of code to get macro-level
timing information.

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

