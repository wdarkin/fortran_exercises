program MP_ALGORITHM
implicit none
integer, parameter :: m=4
character(LEN=m) :: x='abab'
real, dimension(4) :: T
integer :: i, j

i=0
T(1)=-1
j=-1

do while(i < m)
  do while((j > -1) .AND. (x(i+1:i+1) /= (x(j+i+1:j+i+1))))
    j=T(1)
  end do
  i=i+1
  j=j+1
  T(i)=j
end do
end  program MP_ALGORITHM