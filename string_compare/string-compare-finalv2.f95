program STRING_COMPAREv2
implicit none
integer, parameter :: m=1, n=18
character(len=m) :: x = 'C'
character(len=n) :: y = 'TAGATGCTACCTAGTAGG'

print *, 'x=', x
print *, 'y=', y
print *, char(10), "=================================="

call bruteforce_method(x, y)
call mp_method(x, y, m, n)
call kmp_method(x, y, m, n)

print *, char(7)

contains

	subroutine bruteforce_method(x, y)
	implicit none
	character, intent(in) :: x, y
	integer :: i, j, counter
	!busca
    print *, 'BruteForce Method:', char(10)
    counter=0
    	do j=0, n-m
      	i=0
      		do while(i<m .and. x(i+1:i+1) == y(i+j+1:i+j+1))
            	counter=counter+1
        		i = i+1
      		end do
      		if(i>=m) then
        		print *, 'Ocorrencia na posicao:', j+1, char(10)
      		end if
    	end do
        print *, 'Numero de comparacoes:', counter, char(10)
        print *, "=================================="
	end subroutine bruteforce_method



	subroutine mp_method(x, y, m, n)
    implicit none
    integer, intent(in) :: m, n
    character, intent(in) :: x, y
    integer, dimension(0:m-1) :: T
    integer :: i, j, counter
    call mp_table(x, m, T)
    print *, 'Morris-Pratt Method:', char(10)
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
          if(i >= m-1) then
            print *, 'Ocorrencia na posicao:', j-i+1, char(10)
            i=T(i)
          end if
        end do
        print *, 'Numero de comparacoes:', counter, char(10)
        print *, "=================================="
    end subroutine mp_method

	subroutine mp_table(x, m, T)
    implicit none
    integer, intent(in) :: m
    character, intent(in) :: x
    integer, dimension(0:m-1), intent(out) :: T
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
          T(i)=j
        end do
    end subroutine mp_table



	subroutine kmp_method(x, y, m, n)
    implicit none
    integer, intent(in) :: m, n
    character, intent(in) :: x, y
    integer, dimension(0:m-1) :: T
    integer :: i, j, counter
    call kmp_table(x, m, T)    
    print *, 'Knuth-Morris-Pratt Method:', char(10)
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
          if(i >= m-1) then
            print *, 'Ocorrencia na posicao:', j-i+1, char(10)
            i=T(i)
          end if
        end do        
        print *, 'Numero de comparacoes:', counter, char(10)
        print *, "=================================="
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
end program STRING_COMPAREv2
