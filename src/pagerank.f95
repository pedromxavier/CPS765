function prank(A, t, m, n) result(x)
    implicit none
    integer :: m, n, k
    double precision :: t, s
    double precision, dimension(m, n) :: x
    double precision, dimension(n, n) :: A, D

    x(1, :) = 1.0D0 / n

    do k = 1, n
        s = SUM(A(k, :))
        if (s /= 0.0D0) then
            D(:, k) = A(k, :) / s
        else
            D(:, k) = 0.0D0
        end if
    end do

    do k = 2, m
        x(k, :) = t * MATMUL(D(:, :), x(k - 1, :)) + (1 - t) / n
    end do

    return
end function

program pagerank

    use Plotlib

    integer :: m, n
    double precision, dimension(:), allocatable :: x
    double precision, dimension(:, :), allocatable :: y, z
    double precision, dimension(:, :), allocatable :: A
    type(StringArray), dimension(:), allocatable :: legend

    interface 
        function prank(A, t, m, n) result (x)
            integer :: m, n, k
            double precision :: t
            double precision, dimension(m, n) :: x
            double precision, dimension(n, n) :: A, D
        end function
    end interface

    m = 31
    n = 5
    allocate(A(n, n), x(m), y(m, n), z(m, n))

    A(:, :) = 0.0D0
    A(1, 3) = 1.0D0
    A(1, 4) = 1.0D0
    A(2, 1) = 1.0D0
    A(2, 5) = 1.0D0
    A(3, 4) = 1.0D0
    A(4, 2) = 1.0D0
    A(4, 5) = 1.0D0
    A(5, 4) = 1.0D0

    x = (/ (k, k=0, m) /)
    y = prank(A, 0.1D0, m, n)
    z = prank(A, 0.9D0, m, n)

    allocate(legend(n))
    do k=1,n
        legend(k)%str = STR(k)
    end do

    call begin_plot(fname='pagerank', size_w='9in')
    call subplots(2, 1)
    do k=1, n
        call subplot(1, 1, x, y(:, k), m)
    end do
    call subplot_config(1, 1, title='α = 0.1', xlabel='iterações', ylabel='x(k)', legend=legend, ymin=0.18D0, ymax=0.25D0)
    do k=1, n
        call subplot(2, 1, x, z(:, k), m)
    end do
    call subplot_config(2, 1, title='α = 0.9', xlabel='iterações', ylabel='x(k)', legend=legend, ymin=0.00D0, ymax=0.50D0)

    call render_plot(clean=.TRUE.)

    ! call info("a = 0.1")
    ! call info("5 : ")
    ! do k=1,n
    !     call info(STR(k)//" : "//DSTR(y(6, k)))
    ! end do
    ! call info("10 : ")
    ! do k=1,n
    !     call info(STR(k)//" : "//DSTR(y(11, k)))
    ! end do
    ! call info("30 : ")
    ! do k=1,n
    !     call info(STR(k)//" : "//DSTR(y(31, k)))
    ! end do
    ! call info("")
    ! call info("")
    ! call info("a = 0.9")
    ! call info("5 : ")
    ! do k=1,n
    !     call info(STR(k)//" : "//DSTR(z(6, k)))
    ! end do
    ! call info("10 : ")
    ! do k=1,n
    !     call info(STR(k)//" : "//DSTR(z(11, k)))
    ! end do
    ! call info("30 : ")
    ! do k=1,n
    !     call info(STR(k)//" : "//DSTR(z(31, k)))
    ! end do

end program pagerank

