program MP

implicit none
integer, parameter :: m=4, n=10
character(LEN=m) :: x='CTAG'
character(LEN=n) :: y='CCTAGCTAGC'

call MP_METHOD(x, m, y, n)

contains

subroutine MP_METHOD(x, m, y, n)
implicit none
interface
!declare
integer, intent(in) :: m, n
character(LEN=m), intent(in) :: x
character(LEN=n), intent(in) :: y
integer :: i, j
integer, dimension(0:m-1) :: T
end interface
!/declare

call MP_TABLE(x, m, T)

i=0
j=0
do while(j < n)
  do while(i > -1 .AND. x(i+1:i+1) /= y(j+1:j+1))
    i=T(i+1)
  end do
  i=i+1
  j=j+1
  if(i >= m) then
    print *, j-1
    i=T(i+1)
  end if
end do
end subroutine MP_METHOD




subroutine MP_TABLE(x, m, T)
implicit none

!declare
integer, intent(in) :: m
character(LEN=m), intent(in) :: x
integer, dimension(0:m-1), intent(out) :: T
integer :: i, j
!/declare

do while(i < m)
  do while(j > -1 .AND. x(i+1:i+1) /= x(j+1:j+1))
    j=T(j+1)
  end do
  i=i+1
  j=j+1
  T(i+1)=j
end do


end subroutine MP_TABLE
end program MP