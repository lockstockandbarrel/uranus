program demo_system_clock
use, intrinsic :: iso_fortran_env, only: real32, real64, real128, int32, int64
implicit none
character(len=*), parameter :: g ='(1x,*(g0,1x))'

integer(kind=int64) :: icount64, icount_rate64, icount_max64
integer(kind=int64) :: istart64, ifinish64

integer(kind=int32) :: icount32, icount_rate32, icount_max32
integer(kind=int32) :: istart32, ifinish32
real(kind=real32)   :: rcount_rate32
real(kind=real64)   :: rcount_rate64
real(kind=real128)  :: rcount_rate128

real(kind=real64)   :: time_read
real(kind=real64)   :: sum
integer             :: i

call system_clock(icount64, icount_rate64, icount_max64)
print g,'COUNT_MAX(64bit)='  , icount_max64
print g,'COUNT_RATE(64bit)='  , icount_rate64
print g,'FLIP1(secs)='  , icount_max64/icount_rate64
print g,'CURRENT COUNT(64bit)='  , icount64
print g,'next flip(secs)=',dble(icount_max64-icount64)/icount_rate64,'(days)==>', dble(icount_max64-icount64)/icount_rate64/86400

call system_clock(icount32, icount_rate32, icount_max32)
print g,'COUNT_MAX(32bit)='  , icount_max32
print g,'COUNT_RATE(32bit)='  , icount_rate32
print g,'FLIP2(secs)='  , icount_max32/icount_rate32
print g,'CURRENT COUNT(32bit)='  , icount32
print g,'next flip(secs)=',dble(icount_max32-icount32)/icount_rate32,'(days)==>', dble(icount_max32-icount32)/icount_rate32/86400

call system_clock(icount64, rcount_rate32, icount_max64)
print g,'COUNT_MAX(64bit)='  , icount_max64
print g,'COUNT_RATE(64bit)='  , rcount_rate32
print g,'CURRENT COUNT(64bit)='  , icount64

call system_clock(icount64, rcount_rate64, icount_max64)
print g,'COUNT_MAX(64bit)='  , icount_max64
print g,'COUNT_RATE(64bit)='  , rcount_rate64
print g,'CURRENT COUNT(64bit)='  , icount64

call system_clock(icount64, rcount_rate128, icount_max64)
print g,'COUNT_MAX(64bit)='  , icount_max64
print g,'COUNT_RATE(128bit)='  , rcount_rate128
print g,'CURRENT COUNT(64bit)='  , icount64

print g,'time some computation'
call system_clock(istart64)

! some code to time
sum = 0.0_real64
do i =0, huge(0)-1
   sum = sum + sqrt(real(i))
end do
print g,'SUM='  , sum

call system_clock(ifinish64)

time_read = (ifinish64-istart64)/real(icount_rate64)
write (*,'(1x,a,1x,g0,1x,a)')'time :', time_read,'seconds'

end program demo_system_clock
