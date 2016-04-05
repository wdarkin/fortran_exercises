program MP_ALGORITHM
implicit none
integer, parameter :: m=5, n=33
character(LEN=m) :: x='CTTCC'
character(LEN=n) :: y='TCGATGCTCCTAGGCTACTCAACTTCCTTCCAG'
integer, dimension(4) :: T

call mp_method(x, y, m, n)

contains

	subroutine mp_method(x, y, m, n)
    implicit none
    character, intent(in) :: x, y
    integer, intent(in) :: m, n
    integer, dimension(m) :: T
    integer :: i, j
    call mp_table(x, m, T)
    	i=0
        j=0

        do while(j < n)
          do while(i > 0 .AND. x(i:i) /= y(j:j))
            i=T(i)
          end do
          i=i+1
          j=j+1
          if(i>=m) then
            print *, j-i+1
            i=T(i)
          end if
        end do
    
    end subroutine mp_method


	subroutine mp_table(x, m, T)
	implicit none
	integer, intent(in) :: m
    character, intent(in) :: x
	integer, dimension(m), intent(out) :: T
	integer :: i, j
		i=1
		T(1)=0
		j=0

		do while(i < m)
  			do while(j > 0 .and. x(i:i) /= x(j:j))
    			j=T(j)
  			end do
  			i=i+1
  			j=j+1
  			T(i)=j
		end do
	end subroutine mp_table
    
end  program MP_ALGORITHM