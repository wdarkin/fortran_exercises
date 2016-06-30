PROGRAM matijk
implicit none
integer, parameter :: m = 512, r = 512, n = 512
real, dimension(m, r) :: A
real, dimension(r, n) :: B
real, dimension(m, n) :: C
integer :: i, j, k, loop
real(kind=2) :: t1, t2, high_res_clock@, tempo

C=0

call random_number(A)
call random_number(B)

t1=0
t2=0
tempo=0

do loop=1, 10
t1=high_res_clock@(.TRUE.)

    do i=1, m
      do j=1, n
        do k=1, r
          C(i,j)=C(i,j)+A(i,k)*B(k,j)
        end do
      end do
    end do
    
t2=high_res_clock@(.FALSE.)
tempo = tempo + (t2-t1)
print *, "Tempo em ", loop, ":", t2-t1
end do

    print *, 'Matriz IJK -> (media) calculada em: ', tempo / 10
END PROGRAM matijk