program STRING_COMPARE
implicit none
integer, parameter :: m=4, n=10
character(len=m) :: x = 'ctag'
character(len=n) :: y = 'cctagctagc'

call BRUTEFORCE_METHOD(x, y)
call MORRISPRATT_METHOD(x, m, y)

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

    subroutine MORRISPRATT_METHOD(x, m, y)
    implicit none
    character(len=m), intent(in) :: x    
    integer, intent(in) :: m
    character, intent(in) :: y
    integer :: i, j, Table(0:m-1)
    call MP_TABLE(x, m, Table)
    i=0
    j=0    
    	do while(j < n)
        	do while(i > -1 .and. x(i) /= y(j))
            	i=Table(i)
            end do
            i=i+1
            j=j+1
            if(i>=m) then
              print *, j-i
              i=Table(i)
            end if
        end do
    end subroutine MORRISPRATT_METHOD

    subroutine MP_TABLE(x, m, Table)
    implicit none
    character(len=m), intent(in) :: x
    integer, intent(in) :: m
    integer, intent(out) :: Table(0:m-1)
    integer :: i, j
    i=0
    Table(0)=-1
    j=0
    	do while(i < m)
        	do while(j > -1 .and. x(i) /= x(j))
            	j=Table(j)
            end do
            i=i+1
            j=j+1
            Table(i)=j
        end do
    end subroutine MP_TABLE
end program STRING_COMPARE
