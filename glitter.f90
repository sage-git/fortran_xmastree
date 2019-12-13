module sys_ioctl
    use iso_c_binding
    implicit none
    type, bind(C) :: winsize
        integer(c_short) :: ws_row
        integer(c_short) :: ws_col
        integer(c_short) :: ws_xpixel
        integer(c_short) :: ws_ypixel
    end type winsize
    integer(c_int), parameter :: TIOCGWINSZ = Z'5413'
    integer(c_int), parameter :: STDOUT_FILENO = 1

    interface 
        function ioctl(fd, cmd, arg) result(r) bind(C, name="ioctl")
            import :: c_int, c_ptr
            integer(c_int) :: r
            integer(c_int), value :: fd, cmd
            type(c_ptr), value :: arg
        end function ioctl
    end interface
end module sys_ioctl

module xmastree
    use iso_fortran_env
    use sys_ioctl
    implicit none
    integer :: width, height
    integer :: h_half, w_half 
    character(len=9) :: STAR_COLOR = char(27) // "[33m"
    character(len=5) :: TREE_COLOR = char(27) // "[32m"
    character(len=5) :: BASE_COLOR = char(27) // "[31m"
    character(len=5) :: ORNAMENT_COLOR = char(27) // "[34m"
    character(len=4) :: RESET_COLOR = char(27) // "[0m"
    character(len=6) :: RESET_TERMINAL = char(27) // "[1;1H"

contains
    subroutine get_windowsize()
        integer(c_int) :: ret
        type(winsize), target :: ws
        ret = ioctl(STDOUT_FILENO, TIOCGWINSZ, c_loc(ws))
        width = iand(256*256 - 1, ws%ws_col) - 1 ! write statement puts a space at the beginning of a line
        height = iand(256*256 - 1, ws%ws_row) - 1 ! line buffer of shell command input
        w_half = width / 2
        h_half = height / 2
    end subroutine get_windowsize
    
    subroutine clean_disp()
        integer :: i
        write(*, '(A)', advance="no") RESET_TERMINAL
        do i = 1, height
            write(*, *) repeat(" ", width)
        enddo
        write(*, '(A)', advance="no") RESET_TERMINAL
    end subroutine clean_disp

    subroutine draw_star()
        write(*, *) repeat(" ", w_half - 5) // STAR_COLOR // "|" // RESET_COLOR
        write(*, *) repeat(" ", w_half - 6) // STAR_COLOR // "\|/" // RESET_COLOR
        write(*, *) repeat(" ", w_half - 9) // STAR_COLOR // "----*----" // RESET_COLOR
        write(*, *) repeat(" ", w_half - 6) // STAR_COLOR // "/ \" // RESET_COLOR
    end subroutine draw_star

    function add_ornament(rate) result(s)
        character(:), allocatable :: s
        real(REAL64), intent(in) :: rate
        real(REAL64) :: x
        call random_number(x)
        if(x < rate/2.0) then
            s = ORNAMENT_COLOR // "O" // RESET_COLOR
        elseif( x < rate) then
            s = "*"
        else
            s = " "
        endif
    end function add_ornament

    subroutine draw_leaf()
        integer :: i, j
        integer :: n1, n2
        n1 = w_half - 1
        do i = 5, height - 5
            if (mod(i, 4) == 0) n1 = n1 + 1
            n1 = n1 - 1
            n2 = w_half*2 - 2*n1 - 2
            if (n1 == 0) exit
            write(*, '(a)', advance="no") repeat(" ", n1) // TREE_COLOR // "/" // RESET_COLOR
            do j = 1, n2
                write(*, '(a)', advance="no") add_ornament(0.12d0)
            enddo
            write(*, *) TREE_COLOR // "\" // RESET_COLOR
        enddo
        write(*, *) repeat(" ", n1) // TREE_COLOR // repeat("-", n2 + 2)  // RESET_COLOR
    end subroutine draw_leaf

    subroutine draw_base()
        write(*, *) repeat(" ", w_half - 3) // BASE_COLOR // "|:::|" // RESET_COLOR
        write(*, *) repeat(" ", w_half - 6) // BASE_COLOR // "[[_______]]" // RESET_COLOR
        write(*, *) repeat(" ", w_half - 5) // BASE_COLOR // "|XXXXXXX|" // RESET_COLOR
        write(*, *) repeat(" ", w_half - 5) // BASE_COLOR // "|XXXXXXX|" // RESET_COLOR
    end subroutine draw_base

    subroutine draw_tree(overwrite)
        logical, intent(in) :: overwrite
        integer :: i
        call get_windowsize()
        ! if (overwrite) write(*, '(A)', advance="no") RESET_TERMINAL
        if (overwrite) call clean_disp()
        if (height < 8 .or. width < 20 )then
            write(*, *) "Merry X'mas!"
            return
        endif
        call draw_star()
        call draw_leaf()
        call draw_base()
    end subroutine draw_tree

end module xmastree

program main
    use xmastree, only: draw_tree
    implicit none
    integer :: N

    call draw_tree(.false.)
    do N = 1, 100
        call sleep(1)
        call draw_tree(.true.)
    enddo
   
    stop
end program main
    
