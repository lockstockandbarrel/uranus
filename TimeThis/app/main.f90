program TimeThis_exe
use M_time, only : fmtdate
use,intrinsic :: iso_fortran_env, only : int64
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT

! @(#) given a command on the command line run the command and print wallclock time and echo command

implicit none
character(len=*),parameter   :: all='(*(g0,1x))'
integer                      :: ier
character(len=:),allocatable :: command, starttime,endtime
integer(kind=int64)          :: jtime(2)

   call get_cmd(command,ier) ! get the command including an attempt to requote quoted strings
   if(ier.eq.0)then
      call millijulian(jtime(1),starttime)
      if(command.ne.'')ier=run(command)
      call millijulian(jtime(2),endtime)
      write(*,all)'TimeThis :  Command Line : ', trim(adjustl(command))
      write(*,all)'TimeThis :    Start Time : ', starttime
      write(*,all)'TimeThis :      End Time : ', endtime
      write(*,all)'TimeThis :  Elapsed Time : ', millisec2days( jtime(2)-jtime(1) )
   else
      write(*,all)'<ERROR>*TimeThis* STATUS:',ier
   endif

contains

function run(command)
! @(#) M_system run(3f) call execute_command_line as a function
character(len=*),intent(in) :: command
integer                     :: exitstat
integer                     :: cmdstat
integer                     :: run
character(len=256)          :: cmdmsg

   cmdmsg=' '
   call execute_command_line(trim(command), wait=.true., exitstat=exitstat, cmdstat=cmdstat, cmdmsg=cmdmsg)
   if(cmdstat.ne.0)then
      write(stderr,*)trim(cmdmsg)
   endif
   run=cmdstat

end function run

function millisec2days(milliseconds) result(dhms)
! @(#) M_time millisec2days(3f) converts milliseconds to string showing days of form D-HH:MM:SS.SSS
integer(kind=int64),intent(in) :: milliseconds
integer(kind=int64)            :: days, hours, minutes, secsleft, left
integer(kind=int64),parameter  :: ONE_DAY=86400, ONE_HOUR=3600, ONE_MINUTE=60
character(len=:),allocatable   :: dhms
character(len=40)              :: scratch
   secsleft=milliseconds/1000
   left=mod(milliseconds,1000)
   days=secsleft/ONE_DAY                  ! get whole number of days
   secsleft=secsleft-days*ONE_DAY         ! calculate remainder
   hours=secsleft/ONE_HOUR                ! get whole number of hours
   secsleft=secsleft-hours*ONE_HOUR
   minutes=secsleft/ONE_MINUTE            ! get whole number of minutes
   secsleft=secsleft-minutes*ONE_MINUTE
   write(scratch,'(i0,"-",i2.2,":",i2.2,":",i2.2,".",i0)')days,hours,minutes,secsleft,left
   dhms=trim(scratch)
end function millisec2days

subroutine get_cmd(command,status)
! @(#) compose a command from all the arguments passed to the program
character(len=*),parameter               :: gen='(*(g0))'
character(len=:),allocatable,intent(out) :: command               ! string of all arguments to create
integer,intent(out)                      :: status                ! status (non-zero means error)
integer                                  :: i, j
character(len=:),allocatable             :: value,valueb          ! hold individual arguments one at a time
character(len=255)                       :: errmsg
integer                                  :: length                ! length of individual arguments
   command=""                                                     ! initialize returned output string
   errmsg=""
   status=0
   ERRORS: BLOCK
      do i=1,command_argument_count()
         !call get_command_argument(i,length=length,status=status,errmsg=errmsg)   ! get length of next argument
         call get_command_argument(i,length=length,status=status)                 ! get length of next argument
         if(status.ne.0)exit ERRORS
         value=repeat(' ',length)
         !call get_command_argument(i,value=value,status=status,errmsg=errmsg)     ! get next argument
         call get_command_argument(i,value=value,status=status)                   ! get next argument
         if(status /= 0)exit ERRORS
         if(length.gt.0)then ! SIMPLISTIC GUESS AT RE-QUOTING STRING
            ! assuming an operating system shell that strips the quotes from quoted strings on the command line.
            ! if argument contains a space and does not contain a double-quote
            ! assume this argument was quoted but that the shell stripped the quotes and add double quotes.
            if(index(value,' ').ne.0.and.index(value,'"').eq.0)then
               value='"'//value//'"'
            elseif(index(value,'"').ne.0)then
               ! assume you double doublequotes to escape them and short enough that reallocating a lot not an issue
               valueb=''
               do j=1,len(value)
                  if(value(j:j)=='"') valueb=valueb//'"'
                  valueb=valueb//value(j:j)
               enddo
               value='"'//valueb//'"'
            endif
            command=command//' '//value ! append strings together
         else
            command=command//'""'
         endif
      enddo
      return
   endblock ERRORS
   write(stderr,gen)'*get_cmd* error obtaining argument ',i,'errmsg=',trim(errmsg)
   stop
end subroutine get_cmd

subroutine millijulian(jm,timestr)
! @(#)millijulian(3f): Converts proleptic Gregorian DAT date-time array to Julian Date in milliseconds in Zulu timezone
integer                    ::  dat(8)
integer(kind=int64)        ::  a , y , m , jdn, utc
integer(kind=int64),intent(out) :: jm
character(len=:),allocatable :: timestr
call date_and_time(values=dat)
timestr=fmtdate(dat)
timestr=fmtdate( dat,'wkday Mth day hour:minute:second year')
associate &
&(year=>dat(1),month=>dat(2),day=>dat(3),hour=>dat(5),minute=>dat(6),second=>dat(7),milli=>dat(8))
   !  You must first compute the number of years (Y) and months (M) since March 1st -4800 (March 1, 4801 BC)
      a = (14_int64-month)/12_int64 ! A will be 1 for January or February, and 0 for other months, with integer truncation
      y = year + 4800_int64 - a
      m = month + 12_int64*a - 3_int64 ! M will be 0 for March and 11 for February
   !  All years in the BC era must be converted to astronomical years, so that 1BC is year 0, 2 BC is year "-1", etc.
   !  Convert to a negative number, then increment towards zero
      ! intentionally computing with integer truncation
      jdn = day + (153_int64*m+2_int64)/5_int64 + 365_int64*y + y/4_int64 - y/100_int64 + y/400_int64 - 32045_int64
   !  Finding the Julian time in milliseconds given the JDN (Julian day number) and time of day
      jm = (jdn*86400_int64 + hour*3600_int64 + minute*60_int64 + second)*1000_int64 + milli
end associate
   utc=dat(4)*60*1000              ! Time difference with UTC in minutes converted to milliseconds
   jm = jm + utc ! set all values to Zulu time
end subroutine millijulian

end program TimeThis_exe
