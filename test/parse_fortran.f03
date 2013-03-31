program test
    use, intrinsic :: iso_c_binding
    use iniparser
    implicit none

    integer(c_int) :: n
    type(c_ptr) :: ini_dic
    character(len=40, kind=c_char) :: ini_result
    character(len=40, kind=c_char), dimension(:), allocatable :: result_strarray
    integer(c_int), dimension(:), pointer :: result_intarray
    real(c_double), dimension(:), pointer :: result_doublearray
    integer(c_int), dimension(:), pointer :: result_boolarray
    integer(c_int) :: result_size

    ini_dic = iniparser_load("twisted-arrays.ini" // c_null_char)

    write(*,'(A,I1)') "Number of sections: ", iniparser_getnsec(ini_dic)
    call iniparser_getsecname(ini_result, ini_dic, 0)
    write(*,'(A,A)') "Name of first section: ", ini_result
    write(*,'(A,A,A,I1)') "Number of keys in section ", trim(ini_result), ": ", &
        & iniparser_getsecnkeys(ini_dic, trim(ini_result) // c_null_char)
    call iniparser_getseckeys(result_strarray, ini_dic, trim(ini_result) // c_null_char, result_size)
    write(*,'(A,A,A)') "Keys in section ", trim(ini_result), ": "
    do n = 1, result_size
        write(*,*) n, trim(result_strarray(n))
    end do
    write(*,'(A,I1)') "Is 'Test:string' there? ", iniparser_find_entry(ini_dic, "Test:string" // c_null_char)

    call iniparser_getstring(ini_result, ini_dic, "Test_Single:String" // c_null_char, "---" // c_null_char)
    write(*,*) trim(ini_result)

    write(*,*) iniparser_getint(ini_dic, "Test_Single:Integer" // c_null_char, 0)
    write(*,*) iniparser_getint(ini_dic, "Test_Single:String" // c_null_char, -1)
    write(*,*) iniparser_getdouble(ini_dic, "Test_Single:Double" // c_null_char, 0.0_8)
    write(*,*) iniparser_getboolean(ini_dic, "Test_Single:Bool" // c_null_char, 2)
    write(*,*)

    call iniparser_getstring_array(result_strarray, ini_dic, "Test:String" // c_null_char, ".!" // c_null_char, result_size)
    write(*,*) "String Size: ", result_size
    do n = 1, result_size
        write(*,*) n, trim(result_strarray(n))
    end do
    write(*,*)

    call iniparser_getint_array(result_intarray, ini_dic, "Test:Integer" // c_null_char, "," // c_null_char, result_size)
    write(*,*) "Integer Size: ", result_size
    do n = 1, result_size
        write(*,*) n, result_intarray(n)
    end do
    write(*,*)

    call iniparser_getdouble_array(result_doublearray, ini_dic, "Test:Double" // c_null_char, "," // c_null_char, result_size)
    write(*,*) "Double Size: ", result_size
    do n = 1, result_size
        write(*,*) n, result_doublearray(n)
    end do
    write(*,*)

    call iniparser_getboolean_array(result_boolarray, ini_dic, "Test:Bool" // c_null_char, "," // c_null_char, -1, &
        & result_size)
    write(*,*) "Boolean Size: ", result_size
    do n = 1, result_size
        write(*,*) n, result_boolarray(n)
    end do
    write(*,*)

    call iniparser_freedict(ini_dic)
end program test
