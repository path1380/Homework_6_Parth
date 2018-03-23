program Part4_par2
	use type_defs
	!$ use omp_lib
	implicit none
	integer :: nx,ny,nthreads
	real(dp) :: x1,x2,y1,y2,Integral
	real(kind = 8) :: time1,time2,time
	OPEN(unit = 29, file = 'Output_par2.txt', status="REPLACE")
	nthreads = 12
	!$ call OMP_set_num_threads(nthreads)
	nx = 24000
	ny = 24000
	x1 = -1
	x2 = 1
	y1 = -1
	y2 = 1
	Integral = 0
	call cpu_time(time1)
	call Surface_integral(x1,x2,y1,y2,nx,ny,Integral)
	call cpu_time(time2)
	time = time2 - time1
	write(29,'(2es12.4)') Integral,time
	close(29)
contains
	subroutine Surface_integral(x1,x2,y1,y2,nx,ny,Integral)
		use type_defs
		implicit none
		integer :: i,j,k,nx,ny
		real(dp) :: x1,x2,y1,y2,hx,hy,Integral
		hx=(x2-x1)/nx
		hy=(y2-y1)/ny
		Integral = 0
		do k=0,1
			!$OMP PARALLEL DO PRIVATE(i) REDUCTION(+:Integral) COLLAPSE(2)
			do i=1,nx/2
				do j=1,ny
					Integral = Integral + (integrand(x1+((i+(k*nx/2))*hx),y1+(j*hy))*hx*hy)
				end do
			end do
			!$OMP END PARALLEL DO
		end do
	end subroutine Surface_integral
	real(dp) function integrand(x,y)
		use type_defs
		implicit none
		real(dp), intent(in) :: x,y
		!real(dp), intent(out) :: integrand
		integrand = 9*(x**2)*(y**2)/4.0_dp
	end function integrand
end program Part4_par2
