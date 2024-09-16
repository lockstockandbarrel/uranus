module fpm_time
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fpm-time!"
  end subroutine say_hello
end module fpm_time
