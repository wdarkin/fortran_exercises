program mp_algoritmo_professor
implicit none
integer, parameter :: m=4, n=20
character(len=m) :: x='TTCG'
character(len=n) :: y='ATCTGCTTCCTGCTTCTTCG'
integer, dimension(0:m-1) :: T

call mp_method(x, y, m, n)

contains
	subroutine mp_method(x, y, m, n)
    implicit none
    integer, intent(in) :: m, n
    character, intent(in) :: x, y
    integer, dimension(0:m) :: T
    integer :: i, j, counter
    call mp_table(x, m, T)
    	i=0
        j=0
        counter=0
        do while(j < n)
          do while(i > -1 .and. x(i+1:i+1) /= y(j+1:j+1))
            counter=counter+1
            i=T(i)
          end do
          i=i+1
          j=j+1
          if(i >= m) then
            print *, j-i
            i=T(i)
          end if
        end do
        print *, counter
    end subroutine mp_method

	subroutine mp_table(x, m, T)
    implicit none
    integer, intent(in) :: m
    character, intent(in) :: x
    integer, dimension(0:m), intent(out) :: T
    integer :: i, j
    	i=0
        T(0)=-1
        j=-1

        do while(i < m)
          do while(j > -1 .and. x(i:i) /= x(j+i+1:j+i+1))
            j=T(j)
          end do
          i=i+1
          j=j+1
          T(i)=j
        end do
    end subroutine mp_table
end program mp_algoritmo_professor