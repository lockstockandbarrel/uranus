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
