program STRING_COMPARE
implicit none
integer, parameter :: m=5, n=15
character(len=m) :: x = 'ATATA'
character(len=n) :: y = 'CTCATATATAATATA'

call bruteforce_method(x, y)
call mp_method(x, y, m, n)
call kmp_method(x, y, m, n)

contains

	subroutine bruteforce_method(x, y)
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
        		print *, 'Ocorrencia na posicao:', j+1
      		end if
    	end do
        print *, 'Foi achado em:', count, ' loops', char(10)
        print *, "=================================="
	end subroutine bruteforce_method



	subroutine mp_method(x, y, m, n)
    implicit none
    character, intent(in) :: x, y
    integer, intent(in) :: m, n
    integer, dimension(m) :: T
    integer :: i, j, count
    call mp_table(x, m, T)
    print *,  'Morris-Pratt Method:', char(10)
    	i=0
        j=0
        count=0

        do while(j < n)
          do while(i > 0 .AND. x(i:i) /= y(j:j))
            i=T(i)
          end do
          i=i+1
          j=j+1
          if(i>=m) then
            count = count+1
            print *, 'Ocorrencia na posicao', j-i+1
            i=T(i)
          end if
        end do
        print *, 'Foi achado em:', count, ' loops', char(10)
        print *, "=================================="
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



    subroutine kmp_method(x, y, m, n)
    implicit none
    integer, intent(in) :: m, n
    character, intent(in) :: x, y
    integer, dimension(m) :: T
    integer :: i, j, count
    call kmp_table(x, m, T)
    print *,  'Knuth-Morris-Pratt Method:', char(10)
    	i=0
        j=0
        count=0

        do while(j < n)
          do while(i > 0 .AND. x(i:i) /= y(j:j))
            i=T(i)
          end do
          i=i+1
          j=j+1
          if(i >= m) then
            count=count+1
            print *, 'Ocorrencia na posicao', j-i+1
            i=T(i)
          end if
        end do
        print *, 'Foi achado em:', count, ' loops', char(10)
        print *, "=================================="
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
end program STRING_COMPARE
