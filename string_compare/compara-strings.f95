program STRING_COMPARE
implicit none
integer, parameter :: m=4, n=10
character(len=m) :: x = 'ctag'
character(len=n) :: y = 'cctagctagc'

call BRUTEFORCE_METHOD(x, y)
contains
	subroutine BRUTEFORCE_METHOD(x, y)
	implicit none
	character, intent(in) :: x, y
	integer :: i, j, count=0
	!busca
    print *, 'BruteForce Method:', char(10)
    	do j=0, n-m
      	i=0
      		do while(i<m .and. x(i+1:i+1) == y(i+j+1:i+j+1))
        		i = i+1
      		end do
      		if(i>=m) then
            	count = count+1
        		print *, 'Ocorrencia em:', j+1
      		end if
    	end do
        print *, 'Tentativas:', j
        print *, 'Foi achado em:', count, ' lacos'
	end subroutine BRUTEFORCE_METHOD

    subroutine MORRISPRATT_METHOD(x, y)
    implicit none
    character, intent(in) :: x, y
    integer :: i, j
    	do while(j=0 < n)
        i=0
        	do while(i< -1 .and. x(i) /= y(j))
            	i=call MP_TABLE(x,i)
            end do
            i=i+1
            j=j+1
            if(i>=m) then
              print *, j-i
              i=call MP_TABLE(x,i)
            end if
        end do
    end subroutine

    subroutine MP_TABLE(x, m, T)
    implicit none
    character, intent(in) :: x
    integer, intent(in) :: m
    integer, intent(out) :: T
    integer :: i, j
    i=0
    j=0
    	do while(i<m)
        T(0)= -1
        	do while(j > -1 .and. x(i+1:i+1) /= x(j))
            	j=T(j)
            end do
            i=i+1
            j=j+1
            T(i)=j
        end do
    end subroutine
end program STRING_COMPARE

