program KMP_ALGORITHM
implicit none
integer, parameter :: m=5, n=33
character(len=m) :: x='CTTCC'
character(len=n) :: y='TCGATGCTCCTAGGCTACTCAACTTCCTTCCAG'
integer, dimension(m) :: T

call kmp_method(x, y, m, n)

contains

	subroutine kmp_method(x, y, m, n)
    implicit none
    integer, intent(in) :: m, n
    character, intent(in) :: x, y
    integer, dimension(m) :: T
    integer :: i, j
    call kmp_table(x, m, T)
    	i=0
        j=0

        do while(j < n)
          do while(i > 0 .AND. x(i:i) /= y(j:j))
            i=T(i)
          end do
          i=i+1
          j=j+1
          if(i >= m) then
            print *, j-i+1
            i=T(i)
          end if
        end do    
    end subroutine kmp_method	


	subroutine kmp_table(x, m, T)
    implicit none
    integer, intent(in) :: m
    character, intent(in) :: x
    integer, dimension(m), intent(out) :: T
    integer :: i, j
    	i=1
        T(1)=0
        j=0

        do while(i < m)
          do while(j > 0 .AND. x(i:i) /= x(j:j))
            j=T(j)
          end do
          i=i+1
          j=j+1
          if(i < m .AND. x(i:i) == x(j:j)) then
            T(i)=T(j)
          else
            T(i)=j
          end if
        end do
    end subroutine kmp_table
end program KMP_ALGORITHM