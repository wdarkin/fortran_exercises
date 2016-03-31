program KMP
implicit none
integer, parameter :: m=4, n=10
character(len=m) :: x='abcd'
character(len=n) :: y='adbabdabcd'

call KMP_METHOD(x, m, y, n)

contains
subroutine KMP_METHOD(x, m, y, n)
implicit none
integer, intent(in) :: m, n
character(len=m), intent(in) :: x
character(len=n), intent(in) :: y
integer :: i, j
integer, dimension(0:m-1) :: Table

call KMP_TABLE(x, m, Table)

i=0
j=0
do while(j < n)
  do while((i > -1) .AND. (x(i:i) /= y(j:j)))
    i=Table(i)
  end while
  i=i+1
  j=j+1
  if(i > m) then
    print *, j-1
    i=Table(i)
  end if
end while
end subroutine KMP_METHOD

subroutine KMP_TABLE(x, m, Table)
implicit none
integer, intent(in) :: m
character(len=m), intent(in) :: x
integer :: i, j
integer, dimension(0:m-1) :: Table
i=0
Table(0)=-1
j=-1
do while(i < m)
  do while((j > -1) .AND. (x(i:i) /= x(j:j)))
    j=Table(j)
  end while
  i=i+1
  j=j+1

  if((i < m) .AND. (x(i:i) == x(j:j))) then
    Table(i:i)=Table(j:j)
  else
    Table(i:i)=j
  end if
end while

end subroutine KMP_TABLE
end program KMP