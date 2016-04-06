program kmp_algoritmo_professor
implicit none
integer, parameter :: m=4, n=25
character(len=m) :: x='GATA'
character(len=n) :: y='CTCTAGADTATCATAGTGCTTAGAG'
integer, dimension(0:m-1) :: T

call kmp_method(x, y, m, n)

contains

	subroutine kmp_method(x, y, m, n)
    implicit none
    integer, intent(in) :: m, n
    character, intent(in) :: x, y
    integer, dimension(0:m-1) :: T
    integer :: i, j
    call kmp_table(x, m, T)
    	i=0
        j=0
        do while(j < n)
          do while(i > -1 .and. x(i+1:i+1) /= y(j+1:j+1))
            i=T(i)
          end do
          i=i+1
          j=j+1
          if(i >= m-1) then
            print *, j-i
            i=T(i)
          end if
        end do
    endsubroutine kmp_method

	subroutine kmp_table(x, m, T)
    implicit none
    integer, intent(in) :: m
    character, intent(in) :: x
    integer, dimension(0:m-1) :: T
    integer :: i, j
    	i=0
        T(0)=-1
        j=-1
        do while(i < m-1)
          do while(j > -1 .and. x(i:i) /= x(j+i+1:j+i+1))
            j=T(j)
          end do
          i=i+1
          j=j+1
          if(i < m .and. x(i:i) == x(j:j)) then
            T(i)=T(j)
          else
            T(i)=j
          end if
        end do
    end subroutine kmp_table
end program kmp_algoritmo_professor