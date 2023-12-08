module m
        use iso_fortran_env

        contains
        subroutine read_file()
                implicit none

                integer :: io, sum, n
                integer :: i !, a, b
                character :: a, b, c
                character :: digits*20

		sum=0

                open(unit=20, file='data.dat', status='old')

                do
                        read(20,*,iostat=io) digits
                       if (io==iostat_end) exit
                        print*, trim(digits)
                        n=len_trim(digits)
                        
                        do i=1,n
                        	if (chiffre(digits(i:i)))  exit
                        end do
                        
                        a=digits(i:i)
                       ! print*, a
                        
                        do i=n,1,-1
                        	if (chiffre(digits(i:i)))  exit
                        end do
                        
                        b=digits(i:i)
                      !  print*, b
                        
                        print*, a,b
                        
                end do
                close(20)
        end subroutine
        logical function chiffre(digit)
        	implicit none
        	character(len=1) :: digit
        	
        	if (digit.ge.'0'.and.digit.le.'9') then
        		chiffre=.true.
      		end if
      
        end function
end module

program hello
        use m
	implicit none 
		
        print*, "start"

        call read_file()

        print*, "end"

end program


