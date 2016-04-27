program insertion_sort
implicit none
integer, parameter :: n=16
integer, dimension(0:n-1) :: a = (/16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1/)
integer :: t, j, i, lenght
lenght=n
do i=1, lenght-1
  t=a(i)
  j=i-1
  do while (j >= 0 .and. a(j) > t)
    a(j+1) = a(j)
    j=j-1
  end do
  a(j+1)=t
end do
print *, a
end program insertion_sort