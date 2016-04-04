program MP_ALGORITHM
implicit none
integer, parameter :: m=4
character(LEN=m) :: x='aaaa'
integer, dimension(4) :: T

call mp_table(x, m, T)

contains

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