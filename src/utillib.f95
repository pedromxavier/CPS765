!   Util Module
    module Util
        implicit none
        character, parameter :: ENDL = ACHAR(10)
        character, parameter :: TAB = ACHAR(9)

        double precision :: DINF, DNINF, DNAN
        DATA DINF/x'7ff0000000000000'/, DNINF/x'fff0000000000000'/, DNAN/x'7ff8000000000000'/

        double precision :: PI = 4.0D0 * DATAN(1.0D0)

        logical :: DEBUG_MODE = .FALSE.
        logical :: QUIET_MODE = .FALSE.

        type StringArray
            character (:), allocatable :: str
        end type StringArray
    contains

    function EDGE(x) result (y)
        double precision, intent(in) :: x
        logical :: y

        y = ISNAN(x) .OR. (x == DINF) .OR. (x == DNINF)
        return
    end function

    function VEDGE(x) result (y)
        double precision, dimension(:), intent(in) :: x
        logical :: y

        y = ANY(ISNAN(x)) .OR. ANY(x == DINF) .OR. ANY(x == DNINF)
        return
    end function

    function MEDGE(x) result (y)
        double precision, dimension(:, :), intent(in) :: x
        logical :: y

        y = ANY(ISNAN(x)) .OR. ANY(x == DINF) .OR. ANY(x == DNINF)
        return
    end function

        function quote(s, q) result (r)
            character(len=*), intent(in) :: s
            character(len=*), optional, intent(in) :: q
            character(len=:), allocatable :: t_q
            character(len=:), allocatable :: r

            if (.NOT. PRESENT(q)) then
                t_q = '"'
            else
                t_q = q
            end if

            r = t_q//s//t_q
        end function

        function DLOG2(x) result (y)
            implicit none
            double precision, intent(in) :: x
            double precision :: y

            y = DLOG(x) / DLOG(2.0D0)
            return
        end function

        function ILOG2(n) result (k)
            integer, intent(in) :: n
            integer :: k
            double precision :: x
            x = n
            x = DLOG2(x)
            k = FLOOR(x)
            return
        end function

!       ==== Random seed Initialization ====
        subroutine init_random_seed()
            integer :: i, n, clock
            integer, allocatable :: seed(:)
        
            call RANDOM_SEED(SIZE=n)
            allocate(seed(n))
            call SYSTEM_CLOCK(COUNT=clock)
            seed = clock + 37 * (/ (i - 1, i = 1, n) /)
            call RANDOM_SEED(PUT=seed)
            deallocate(seed)
        end subroutine

        function DRAND(a, b) result (y)
            implicit none
            double precision :: a, b, x, y
            ! x in [0, 1)            
            call RANDOM_NUMBER(x)
            y = (x * (b - a)) + a
            return
        end function

!       ===== I/O Metods =====
        function STR(k) result (t)
!       "Convert an integer to string."
            integer, intent(in) :: k
            character(len=128) :: s
            character(len=:), allocatable :: t
            write(s, *) k
            t = TRIM(ADJUSTL(s))
            return
            return
        end function

        function DSTR(x) result (q)
            integer :: j, k
            double precision, intent(in) :: x
            character(len=64) :: s
            character(len=:), allocatable :: p, q
            
            if (ISNAN(x)) then
                q = '?'
                return
            else if (x == DINF) then
                q = '∞'
                return
            else if (x == DNINF) then
                q = '-∞'
                return
            end if

            write(s, *) x
            p = TRIM(ADJUSTL(s))
            do j=LEN(p), 1, -1
                if (p(j:j) == '0') then
                    continue
                else if (p(j:j) == '.') then
                    k = j - 1
                    exit
                else 
                    k = j
                    exit
                end if
            end do
            q = p(:k)
            return
        end function

        subroutine display(text, ansi_code)
            implicit none
            character(len=*) :: text
            character(len=*), optional :: ansi_code
            if (QUIET_MODE) then
                return
            else
                if (PRESENT(ansi_code)) then
                    write (*, *) ''//achar(27)//'['//ansi_code//'m'//text//''//achar(27)//'[0m'
                else
                    write (*, *) text
                end if
            end if
        end subroutine

        subroutine error(text)
!           Red Text
            implicit none
            character(len=*) :: text
            call display(text, '31')
        end subroutine

        subroutine warn(text)
!           Yellow Text
            implicit none
            character(len=*) :: text
            call display(text, '93')
        end subroutine

        subroutine debug(text)
!           Yellow Text
            implicit none
            character(len=*) :: text
            if (DEBUG_MODE) then
                call display('[DEBUG] '//text, '93')
            end if
        end subroutine

        subroutine info(text)
!           Green Text
            implicit none
            character(len=*) :: text
            call display(text, '32')
        end subroutine

        subroutine blue(text)
!           Blue Text
            implicit none
            character(len=*) :: text
            call display(text, '36')
        end subroutine

        subroutine show(var, val)
!           Violet Text
            implicit none
            character(len=*) :: var
            double precision :: val
            write (*, *) ''//achar(27)//'[36m'//var//' = '//DSTR(val)//''//achar(27)//'[0m'
        end subroutine

        recursive subroutine cross_quick_sort(x, y, u, v, n)
            integer :: n, i, j, u, v
            double precision :: p, aux, auy
            double precision :: x(n), y(n)

            i = u
            j = v

            p = x((u + v) / 2)

            do while (i <= j)
                do while (x(i) < p)
                    i = i + 1
                end do
                do while(x(j) > p)
                    j = j - 1
                end do
                if (i <= j) then
                    aux = x(i)
                    auy = y(i)
                    x(i) = x(j)
                    y(i) = y(j)
                    x(j) = aux
                    y(j) = auy
                    i = i + 1
                    j = j - 1
                end if
            end do

            if (u < j) then
                call cross_quick_sort(x, y, u, j, n)
            end if
            if (i < v) then
                call cross_quick_sort(x, y, i, v, n)
            end if
            return
        end subroutine

        subroutine cross_sort(x, y, n)
            implicit none
            integer :: n
            double precision :: x(n), y(n)
            
            call cross_quick_sort(x, y, 1, n, n)
        end subroutine

        subroutine linspace(a, b, dt, n, t)
            implicit none
            integer :: k, n
            double precision, intent(in) :: a, b, dt
            double precision, dimension(:), allocatable :: t
            n = 1 + FLOOR((b - a) / dt)
            allocate(t(n))
            t(:) = dt * (/ (k, k=0, n-1) /)
        end subroutine
    end module Util
