module PlotLib
    use Util
    implicit none
    character(len=*), parameter :: DEFAULT_FONT = 'Helvetica, 12'
    character(len=*), parameter :: DEFAULT_FNAME = 'plotfile'
    character(len=*), parameter :: DEFAULT_SIZE_W = '12in'
    character(len=*), parameter :: DEFAULT_SIZE_H = '9in'
    character(len=*), parameter :: PLOT_ENDL = ',\'//ENDL

    logical :: g_INPLOT = .FALSE.
    logical :: g_INMULTIPLOT = .FALSE.
    
    character(len=:), allocatable :: g_FNAME, g_OUTP_FNAME, g_PLOT_FNAME, g_SIZE_W, g_SIZE_H, g_FONT

    integer :: g_M, g_N

    type SPLOT
        integer :: i, j
        integer :: n = 0
        logical :: grid = .FALSE.
        logical :: done = .FALSE.
    
        character(len=:), allocatable :: title, xlabel, ylabel
        type(StringArray), dimension(:), allocatable :: legend
        type(StringArray), dimension(:), allocatable :: with

!       Plot boundaries
        logical :: l_xmin = .FALSE., l_xmax = .FALSE.
        logical :: l_ymin = .FALSE., l_ymax = .FALSE.
        double precision :: xmin, xmax, ymin, ymax
    end type

    type(SPLOT), dimension(:, :), allocatable :: g_SUBPLOTS

    contains

    function REMOVE_TEMP_FILES(fname) result (cmd)
        implicit none
        character(len=*) :: fname
        character(len=:), allocatable :: plt
        character(len=:), allocatable :: dat
        character(len=:), allocatable :: cmd

        plt = 'plot/'//fname//'*.plt'
        dat = 'plot/'//fname//'*.dat'

        cmd = 'rm '//plt//' '//dat
        return
    end function

    function PLOT_FNAME(fname) result (path)
        implicit none
        character(len=*) :: fname
        character(len=:), allocatable :: path
        path = 'plot/'//fname//'.plt'
        return
    end function

    function DATA_FNAME(fname, i, j, n) result (path)
        implicit none
        integer, optional, intent(in) :: i, j, n
        character(len=*) :: fname
        character(len=:), allocatable :: path, t_i, t_j, t_n

        if (.NOT. PRESENT(i)) then
            t_i = '1'
        else
            t_i = STR(i)
        end if 

        if (.NOT. PRESENT(j)) then
            t_j = '1'
        else
            t_j = STR(j)
        end if

        if (.NOT. PRESENT(n)) then
            t_n = '1'
        else
            t_n = STR(n)
        end if

        path = 'plot/'//fname//'_'//t_i//'_'//t_j//'_'//t_n//'.dat'
        return
    end function

    function OUTP_FNAME(fname) result (path)
        implicit none
        character(len=*) :: fname
        character(len=:), allocatable :: path
        path = 'plot/'//fname//'.pdf'
        return
    end function

    function GNU_PLOT_CMD(fname) result (cmd)
        implicit none
        character(len=*) :: fname
        character(len=:), allocatable :: cmd
        cmd = 'gnuplot -p '//PLOT_FNAME(fname)
    end function
    
    subroutine subplot_config(i, j, title, xlabel, ylabel, xmin, xmax, ymin, ymax, legend, with, grid)
        integer, intent(in) :: i, j
        integer :: k, n

        logical, optional :: grid

        character(len=*), optional :: title, ylabel, xlabel

        double precision, optional :: xmin, xmax, ymin, ymax

        type(StringArray), dimension(:), optional :: legend, with

        if (g_SUBPLOTS(i, j)%done) then
            call error("Duplicate configuration of subplot ("//STR(i)//", "//STR(j)//")")
            stop "ERROR"
        end if

        n = g_SUBPLOTS(i, j)%n

        allocate(g_SUBPLOTS(i, j)%legend(n))
        allocate(g_SUBPLOTS(i, j)%with(n))

        if (PRESENT(xmin)) then
            g_SUBPLOTS(i, j)%xmin = xmin
            g_SUBPLOTS(i, j)%l_xmin = .TRUE.
        end if

        if (PRESENT(xmax)) then
            g_SUBPLOTS(i, j)%xmax = xmax
            g_SUBPLOTS(i, j)%l_xmax = .TRUE.
        end if

        if (PRESENT(ymin)) then
            g_SUBPLOTS(i, j)%ymin = ymin
            g_SUBPLOTS(i, j)%l_ymin = .TRUE.
        end if

        if (PRESENT(ymax)) then
            g_SUBPLOTS(i, j)%ymax = ymax
            g_SUBPLOTS(i, j)%l_ymax = .TRUE.
        end if

        if (.NOT. PRESENT(legend)) then
            do k=1,n
                g_SUBPLOTS(i, j)%legend(k)%str = 't '//quote(STR(i))
            end do
        else
            do k=1,n
                g_SUBPLOTS(i, j)%legend(k)%str = 't '//quote(legend(k)%str)
            end do
        end if

        if (.NOT. PRESENT(with)) then
            do k=1,n
                g_SUBPLOTS(i, j)%with(k)%str = 'w lines'
            end do
        else
            do k=1,n
                g_SUBPLOTS(i, j)%with(k)%str = 'w '//with(k)%str
            end do
        end if

        if (.NOT. PRESENT(grid)) then
            g_SUBPLOTS(i, j)%grid = .TRUE.
        else
            g_SUBPLOTS(i, j)%grid = grid
        end if

        if (.NOT. PRESENT(title)) then
            g_SUBPLOTS(i, j)%title = ''
        else
            g_SUBPLOTS(i, j)%title = title
        end if

        if (.NOT. PRESENT(xlabel)) then
            g_SUBPLOTS(i, j)%xlabel = 'x'
        else
            g_SUBPLOTS(i, j)%xlabel = xlabel
        end if

        if (.NOT. PRESENT(ylabel)) then
            g_SUBPLOTS(i, j)%ylabel = 'y'
        else
            g_SUBPLOTS(i, j)%ylabel = ylabel
        end if

        g_SUBPLOTS(i, j)%done = .TRUE.

    end subroutine

    subroutine subplot(i, j, x, y, n)
        integer :: file, k
        integer, intent(in) :: i, j, n
        double precision, dimension(n), intent(in) :: x
        double precision, dimension(n), intent(in) :: y

        character(len=:), allocatable :: s_data_fname

        if (g_SUBPLOTS(i, j)%done) then
            call error("Plot over finished subplot ("//STR(i)//", "//STR(j)//")")
            stop "ERROR"
        else
            g_SUBPLOTS(i, j)%n = g_SUBPLOTS(i, j)%n + 1
        end if

        s_data_fname = DATA_FNAME(g_FNAME, i, j, g_SUBPLOTS(i, j)%n)

!       =================== Touch Plot File ===================================        
        open(newunit=file, file=s_data_fname, status="replace", action="write")
            write(file, *) "# file: "//s_data_fname
        close(file)
!       =======================================================================        

!       ==================== Write to Plot File ==============================================
10      format(F16.8, ' ')
11      format(F16.8, ' ')
        open(newunit=file, file=s_data_fname, status="old", position="append", action="write")
        write(file, *)
        do k=1, n
            write(file, 10, advance='no') x(k)
            write(file, 11, advance='yes') y(k)
        end do
        close(file)
!       ======================================================================================
    end subroutine

!   ======= Pipeline ============
    subroutine begin_plot(fname, size_w, size_h, font)
        integer :: file
        character(len=*), optional :: fname, size_w, size_h, font

        if (.NOT. PRESENT(font)) then
            g_FONT = DEFAULT_FONT
        else
            g_FONT = font
        end if

        if (.NOT. PRESENT(size_w)) then
            g_SIZE_W = DEFAULT_SIZE_W
        else
            g_SIZE_W = size_w
        end if

        if (.NOT. PRESENT(size_h)) then
            g_SIZE_H = DEFAULT_SIZE_H
        else
            g_SIZE_H = size_h
        end if

        if (.NOT. PRESENT(fname)) then
            g_FNAME = DEFAULT_FNAME
        else
            g_FNAME = fname
        end if

        g_PLOT_FNAME = PLOT_FNAME(g_FNAME)
        g_OUTP_FNAME = OUTP_FNAME(g_FNAME)

        open(newunit=file, file=g_PLOT_FNAME, status="new", action="write")
        write(file, *) 'set terminal pdf size '//g_SIZE_W//', '//g_SIZE_H//' font '//quote(g_FONT)//';'
        write(file, *) 'set output '//quote(g_OUTP_FNAME)//';'
        close(file)

        g_INPLOT = .TRUE.

    end subroutine

    subroutine subplots(m, n)
        integer, optional, intent(in) :: m, n
        integer :: t_m, t_n

        if ((.NOT. PRESENT(m)) .OR. (m <= 0)) then
            t_m = 1
        else
            t_m = m
        end if

        if ((.NOT. PRESENT(n)) .OR. (n <= 0)) then
            t_n = 1
        else
            t_n = n
        end if

        if (.NOT. g_INPLOT) then
            call begin_plot()
        end if

!       ===== Allocate Variables =====
        allocate(g_SUBPLOTS(t_m, t_n))
        g_M = t_m
        g_N = t_n
        g_INMULTIPLOT = .TRUE.
!       ==============================
    end subroutine

    subroutine render_plot(clean)
        integer :: file, i, j, k, m, n

        logical, optional :: clean
        logical :: t_clean

        if (.NOT. PRESENT(clean)) then
            t_clean = .FALSE.
        else
            t_clean = clean
        end if

!       === Check Plot =========
        if (.NOT. g_INPLOT) then
            call error("No active plot to render.")
            stop "ERROR"
        end if
!       ========================

        m = g_M
        n = g_N

!       ==================== Write to Plot File ==============================================      
        open(newunit=file, file=g_PLOT_FNAME, status="old", position="append", action="write")
        write(file, *) 'set origin 0,0;'

        if (g_INMULTIPLOT) then
            write(file, *) 'set multiplot layout '//STR(m)//','//STR(n)//' rowsfirst;'        
        end if

10      format(A, ' ')
!       =========== Plot data ============
        do i= 1, m
            do j = 1, n
                g_SUBPLOTS(i, j) = g_SUBPLOTS(i, j)

                write(file, *) 'set title '//quote(g_SUBPLOTS(i, j)%title)//';'
                write(file, *) 'set xlabel '//quote(g_SUBPLOTS(i, j)%xlabel)//';'
                write(file, *) 'set ylabel '//quote(g_SUBPLOTS(i, j)%ylabel)//';'

!               ============= Set XRANGE ======================================                
                if (g_SUBPLOTS(i, j)%l_xmin .AND. g_SUBPLOTS(i, j)%l_xmax) then
                    write(file, *) 'set xrange ['//DSTR(g_SUBPLOTS(i, j)%xmin)//':'//DSTR(g_SUBPLOTS(i, j)%xmax)//'];'
                else if (g_SUBPLOTS(i, j)%l_xmin) then
                    write(file, *) 'set xrange ['//DSTR(g_SUBPLOTS(i, j)%xmin)//':*];'
                else if (g_SUBPLOTS(i, j)%l_xmax) then
                    write(file, *) 'set xrange [*:'//DSTR(g_SUBPLOTS(i, j)%xmax)//'];'
                else
                    write(file, *) 'set xrange [*:*];'
                end if
!               ===============================================================
                
!               ============= Set YRANGE ======================================                
                if (g_SUBPLOTS(i, j)%l_ymin .AND. g_SUBPLOTS(i, j)%l_ymax) then
                    write(file, *) 'set yrange ['//DSTR(g_SUBPLOTS(i, j)%ymin)//':'//DSTR(g_SUBPLOTS(i, j)%ymax)//'];'
                else if (g_SUBPLOTS(i, j)%l_ymin) then
                    write(file, *) 'set yrange ['//DSTR(g_SUBPLOTS(i, j)%ymin)//':*];'
                else if (g_SUBPLOTS(i, j)%l_ymax) then
                    write(file, *) 'set yrange [*:'//DSTR(g_SUBPLOTS(i, j)%ymax)//'];'
                else
                    write(file, *) 'set yrange [*:*];'
                end if
!               ===============================================================

                if (g_SUBPLOTS(i, j)%grid) then
                    write(file, *) 'set grid;'
                else
                    write(file, *) 'unset grid;'
                end if

                write(file, 10, advance='no') 'plot'

                do k = 1, g_SUBPLOTS(i, j)%n
                    write(file, 10, advance='no') quote(DATA_FNAME(g_FNAME, i, j, k))
                    write(file, 10, advance='no') 'u 1:2'
                    write(file, 10, advance='no') g_SUBPLOTS(i, j)%legend(k)%str
                    write(file, 10, advance='no') g_SUBPLOTS(i, j)%with(k)%str

                    if (k == (g_SUBPLOTS(i, j)%n)) then
                        write(file, *) ';'
                    else
                        write(file, *) ',\'
                    end if
                end do
            end do
        end do
!       ===================================

!       == Finish Multiplot ===        
        if (g_INMULTIPLOT) then
            write(file, *) 'unset multiplot'
            g_INMULTIPLOT = .FALSE.
        end if
!       =======================        
        close(file)
!       ======================================================================================

!       ===== Call GNUPLOT and remove temporary files ===========
        call EXECUTE_COMMAND_LINE(GNU_PLOT_CMD(g_FNAME))

        if (t_clean) then 
            call EXECUTE_COMMAND_LINE(REMOVE_TEMP_FILES(g_FNAME))
        end if
!       =========================================================

!       ========= Free Variables ======================
        deallocate(g_FNAME, g_OUTP_FNAME, g_PLOT_FNAME)
        deallocate(g_SUBPLOTS)
        g_INPLOT = .FALSE.
!       ===============================================
    end subroutine
end module PlotLib