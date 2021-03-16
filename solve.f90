program solve
    implicit none
    double precision, dimension(10) :: E
    double precision :: b
    double precision :: Ne ! constants
    double precision :: guess = 1
    double precision :: precision = 0.0000000000001
    double precision :: diff
    double precision :: lowerBound = -10000
    double precision :: upperBound = 10000
    !read E_i
    open(1, file = "input.txt", status = "old")
    read(1, *) E
    read(1, *) b
    read(1, *) Ne
    close(1)
    diff = f(E, b, guess) - Ne
    print *, diff
    if ((b == 0 .and. diff /= 0) .or. (Ne < 0)) then
        open(2, file = "output.txt", status = "replace")
        write(2, *) "No solution."
        close(2)
        call exit(1)
    end if
    do while (abs(diff) > precision)
        ! binary search for mu
        if (diff > 0) then
            if (b < 0) then
                lowerBound = guess
            else
                upperBound = guess
            end if
        else
            if (b < 0) then
                upperBound = guess
            else
                lowerBound = guess
            end if
        end if
        guess = (upperBound + lowerBound) / 2
        diff = f(E, b, guess) - Ne
    end do
    ! print *, "Solution: ", guess, "Precision: ", precision
    open(2, file = "output.txt", status = "replace")
    write(2, *) "Solution: μ = ", guess, "(Precision: ", precision, ")"
    write(2, *) "Ne = ", Ne, ", f(μ) = ", f(E, B, guess)
    close(2)
contains

double precision function f (E, b, mu)
    implicit none
    double precision, dimension(10) :: E 
    double precision :: mu
    double precision :: b
    double precision, dimension(10) :: vals
    integer :: i
    do i = 1, 10
        vals(i) = 1/(dexp(b * (E(i) - mu)) + 1)
    end do
    f = sum(vals)
return
end function f
end program solve