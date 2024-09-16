module testprocedures

contains

function nospace1(line)
!$@(#) M_strings::nospace1(3f): remove all whitespace from input string
character(len=*),intent(in)    ::  line             ! remove whitespace from this string and return it
character(len=:),allocatable   ::  nospace1         ! returned string
integer                        ::  ipos             ! position to place next output character at
integer                        ::  i                ! counter to increment from beginning to end of input string
   allocate(nospace1,mold=line)                      ! initially make output line length of input line
   nospace1(:len_trim(nospace1))=' '
   ipos=0
   do i=1,len_trim(line)                            ! increment from first to last character of the input line
      if ( isspace( line(i:i) ) ) cycle             ! if a blank is encountered skip it
      ipos=ipos+1                                   ! increment count of non-blank characters found
      nospace1(ipos:ipos)=line(i:i)                  ! store non-blank character in output
   enddo
   nospace1=trim(nospace1)                            ! blank out unpacked part of line
end function nospace1

elemental function isspace(ch) result(res)
!$@(#) M_strings::isspace(3f): true if null,space,tab,return,new line,vertical tab, or formfeed
character,intent(in) :: ch
logical              :: res
   select case(ch)
   case(' ')                 ! space(32)
     res=.true.
   case(char(0))             ! null(0)
     res=.true.
   case(char(9):char(13))    ! tab(9), new line(10), vertical tab(11), formfeed(12), carriage return(13),
     res=.true.
   case default
     res=.false.
   end select
end function isspace

pure function a2s(array)  result (string)
!$@(#) M_strings::a2s(3fp): function to copy char array to string
character(len=1),intent(in) :: array(:)
character(len=SIZE(array))  :: string
integer                     :: i
   forall( i = 1:size(array)) string(i:i) = array(i)
!  string=transfer(array,string)
end function a2s

pure function s2a(string)  RESULT (array)
!$@(#) M_strings::s2a(3fp): function to copy string(1:Clen(string)) to char array
character(len=*),intent(in) :: string
character(len=1)            :: array(len(string))
integer                     :: i
   forall(i=1:len(string)) array(i) = string(i:i)
!  array=transfer(string,array)
end function s2a

function nospace2(line)
character(len=*),intent(in)    ::  line             ! remove whitespace from this string and return it
character(len=1),allocatable   ::  chs(:)
character(len=:),allocatable   ::  nospace2         ! returned string
chs=s2a(line)
nospace2=a2s(pack(chs,.not.isspace(chs)))
end function nospace2

function nospace3(line)
character(len=*),intent(in)    ::  line             ! remove whitespace from this string and return it
character(len=:),allocatable   ::  nospace3         ! returned string
integer                        :: i, j
allocate(character(len=len(line)) :: nospace3)
j=0
do i=1,len_trim(line)
   if ( isspace( line(i:i) ) ) cycle             ! if whitespace is encountered skip it
   j=j+1
   nospace3(j:j)=line(i:i)
enddo
nospace3=nospace3(:j)
end function nospace3

function nospace4(line)
character(len=*),intent(in)    :: line             ! remove whitespace from this string and return it
integer,allocatable            :: ic(:)
character(len=:),allocatable   :: nospace4         ! returned string
integer                        :: i
integer                        :: icount
   !------------------------------------------------
   ! At line 305 of file app/main.f90
   ! Fortran runtime error: Substring out of bounds: upper bound (32763) of 'line' exceeds string length (31)
   !BUG!ic=ichar([(line(i:i),i=1,len_trim(line))])
   icount=len_trim(line)
   if(allocated(ic))deallocate(ic)
   allocate(ic(icount))
   do i=1,icount
      ic(i)=ichar(line(i:i))
   enddo
   !------------------------------------------------
   ic=pack(ic,ic>32)
   nospace4=repeat(' ',size(ic))
   do i=1,size(ic)
      nospace4(i:i)=char(ic(i))
   enddo
end function nospace4

end module testprocedures
