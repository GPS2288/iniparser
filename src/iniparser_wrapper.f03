!Wrapper for iniparser
!This file contains the interfaces and helpers in Fortran
!Not all functions are implented
!
!Copyright 2013 Georg Poppe
!
!Licensed under the Apache License, Version 2.0 (the "License");
!you may not use this file except in compliance with the License.
!You may obtain a copy of the License at
!
!   http://www.apache.org/licenses/LICENSE-2.0
!
!Unless required by applicable law or agreed to in writing, software
!distributed under the License is distributed on an "AS IS" BASIS,
!WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
!See the License for the specific language governing permissions and
!limitations under the License.

module iniparser
    use, intrinsic :: iso_c_binding
    implicit none

    type, bind(c) :: ptr_package
        type(c_ptr) :: c, l
    end type

    interface
        function iniparser_load(ininame) bind(c)
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: iniparser_load
            character(c_char), dimension(*), intent(in) :: ininame
        end function

        subroutine iniparser_freedict(dic) bind(c)
            use, intrinsic :: iso_c_binding
            type(c_ptr), value, intent(in) :: dic
        end subroutine

        function C_iniparser_getstring(dic, key, notfound, result_len) bind(c, name="iniparser_getstring_wrapper")
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: C_iniparser_getstring
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            character(c_char), dimension(*), intent(in) :: notfound
            integer(c_long), intent(out) :: result_len
        end function

        function C_iniparser_getstring_array(dic, key, delimiters, size) &
        & bind(c, name="iniparser_getstring_array_wrapper")
            use, intrinsic :: iso_c_binding
            import ptr_package
            type(ptr_package) :: C_iniparser_getstring_array
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            character(c_char), dimension(*), intent(in) :: delimiters
            integer(c_int), intent(in) :: size
        end function

        function iniparser_getint(dic, key, notfound) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int) :: iniparser_getint
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            integer(c_int), value, intent(in) :: notfound
        end function

        function C_iniparser_getint_array(dic, key, delimiters, size) bind(c, name="iniparser_getint_array")
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: C_iniparser_getint_array
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            character(c_char), dimension(*), intent(in) :: delimiters
            integer(c_int), intent(in) :: size
        end function

        function iniparser_getdouble(dic, key, notfound) bind(c)
            use, intrinsic :: iso_c_binding
            real(c_double) :: iniparser_getdouble
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            real(c_double), value, intent(in) :: notfound
        end function

        function C_iniparser_getdouble_array(dic, key, delimiters, size) bind(c, name="iniparser_getdouble_array")
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: C_iniparser_getdouble_array
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            character(c_char), dimension(*), intent(in) :: delimiters
            integer(c_int), intent(in) :: size
        end function

        function iniparser_getboolean(dic, key, notfound) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int) :: iniparser_getboolean
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            integer(c_int), value, intent(in) :: notfound
        end function

        function C_iniparser_getboolean_array(dic, key, delimiters, notfound, size) bind(c, name="iniparser_getboolean_array")
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: C_iniparser_getboolean_array
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            character(c_char), dimension(*), intent(in) :: delimiters
            integer(c_int), value, intent(in) :: notfound
            integer(c_int), intent(in) :: size
        end function

        function iniparser_getnsec(dic) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int) :: iniparser_getnsec
            type(c_ptr), value, intent(in) :: dic
        end function

        function C_iniparser_getsecname(dic, n, result_len) bind(c, name="iniparser_getsecname_wrapper")
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: C_iniparser_getsecname
            type(c_ptr), value, intent(in) :: dic
            integer(c_int), value, intent(in) :: n
            integer(c_long), intent(out) :: result_len
        end function

        function iniparser_getsecnkeys(dic, section) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int) :: iniparser_getsecnkeys
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: section
        end function

        function iniparser_find_entry(dic, key) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int) :: iniparser_find_entry
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
        end function

        function C_iniparser_getseckeys(dic, sec, size) &
        & bind(c, name="iniparser_getseckeys_wrapper")
            use, intrinsic :: iso_c_binding
            import ptr_package
            type(ptr_package) :: C_iniparser_getseckeys
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: sec
            integer(c_int), intent(in) :: size
        end function

        function iniparser_set(dic, key, value) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int) :: iniparser_set
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            character(c_char), dimension(*), intent(in) :: value
        end function

        subroutine iniparser_unset(dic, key) bind(c)
            use, intrinsic :: iso_c_binding
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
        end subroutine

        subroutine iniparser_dump_ini(dic, filename, mode) bind(c, name="iniparser_dump_ini_wrapper")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: filename
            character(c_char), dimension(*), intent(in) :: mode
        end subroutine

        subroutine iniparser_dumpsection_ini(dic, sec, filename, mode) bind(c, name="iniparser_dumpsection_ini_wrapper")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: sec
            character(c_char), dimension(*), intent(in) :: filename
            character(c_char), dimension(*), intent(in) :: mode
        end subroutine

        subroutine iniparser_dump(dic, filename, mode) bind(c, name="iniparser_dump_wrapper")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: filename
            character(c_char), dimension(*), intent(in) :: mode
        end subroutine
    end interface

    contains

    subroutine iniparser_getstring(result, dic, key, notfound)
        character(len=*), intent(out) :: result
        type(c_ptr), value, intent(in) :: dic
        character(c_char), dimension(*), intent(in) :: key
        character(c_char), dimension(*), intent(in) :: notfound

        type(c_ptr) :: result_Cptr = c_null_ptr
        character(c_char), dimension(:), pointer :: result_Fptr
        integer(c_long) :: result_len
        integer(c_long) :: n

        result_Cptr = C_iniparser_getstring(dic, key, notfound, result_len)
        call c_f_pointer(result_Cptr, result_Fptr, [result_len]);

        !Convert from character array to scalar string
        result = ""
        do n = 1, result_len
            result(n:n) = result_Fptr(n)
        end do
    end subroutine

    subroutine iniparser_getstring_array(result, dic, key, delimiters, size)
        character(len=*,kind=c_char), dimension(:), allocatable, intent(out) :: result
        type(c_ptr), value, intent(in) :: dic
        character(c_char), dimension(*), intent(in) :: key
        character(c_char), dimension(*), intent(in) :: delimiters
        integer(c_int), intent(in) :: size

        type(ptr_package) :: result_struct
        integer(c_long), dimension(:), pointer :: result_len_Fptr
        type(c_ptr), dimension(:), pointer :: result_Cptrs_Fptr
        character(c_char), dimension(:), pointer :: result_str_Fptr
        integer(c_long) :: n, m

        result_struct = C_iniparser_getstring_array(dic, key, delimiters, size)
        call c_f_pointer(result_struct%l, result_len_Fptr, [size]);
        call c_f_pointer(result_struct%c, result_Cptrs_Fptr, [size])

        !Convert from character arrays to scalar strings
        if(allocated(result)) then
            deallocate(result)
        end if
        allocate(result(size))
        do m = 1, size
            call c_f_pointer(result_Cptrs_Fptr(m), result_str_Fptr, [result_len_Fptr(m)])
            result(m) = ""
            do n = 1, result_len_Fptr(m)
                result(m)(n:n) = result_str_Fptr(n)
            end do
        end do
    end subroutine

    subroutine iniparser_getint_array(result_Fptr, dic, key, delimiters, size)
        integer(c_int), dimension(:), pointer, intent(out) :: result_Fptr
        type(c_ptr), value, intent(in) :: dic
        character(c_char), dimension(*), intent(in) :: key
        character(c_char), dimension(*), intent(in) :: delimiters
        integer(c_int), intent(in) :: size

        type(c_ptr) :: result_Cptr = c_null_ptr

        result_Cptr = C_iniparser_getint_array(dic, key, delimiters, size)
        call c_f_pointer(result_Cptr, result_Fptr, [size]);
    end subroutine

    subroutine iniparser_getdouble_array(result_Fptr, dic, key, delimiters, size)
        real(c_double), dimension(:), pointer, intent(out) :: result_Fptr
        type(c_ptr), value, intent(in) :: dic
        character(c_char), dimension(*), intent(in) :: key
        character(c_char), dimension(*), intent(in) :: delimiters
        integer(c_int), intent(in) :: size

        type(c_ptr) :: result_Cptr = c_null_ptr

        result_Cptr = C_iniparser_getdouble_array(dic, key, delimiters, size)
        call c_f_pointer(result_Cptr, result_Fptr, [size]);
    end subroutine

    subroutine iniparser_getboolean_array(result_Fptr, dic, key, delimiters, notfound, size)
        integer(c_int), dimension(:), pointer, intent(out) :: result_Fptr
        type(c_ptr), value, intent(in) :: dic
        character(c_char), dimension(*), intent(in) :: key
        character(c_char), dimension(*), intent(in) :: delimiters
        integer(c_int), intent(in) :: notfound
        integer(c_int), intent(in) :: size

        type(c_ptr) :: result_Cptr = c_null_ptr

        result_Cptr = C_iniparser_getboolean_array(dic, key, delimiters, notfound, size)
        call c_f_pointer(result_Cptr, result_Fptr, [size]);
    end subroutine

    subroutine iniparser_getsecname(result, dic, n)
        character(len=*), intent(out) :: result
        type(c_ptr), value, intent(in) :: dic
        integer(c_int), intent(in) :: n

        type(c_ptr) :: result_Cptr = c_null_ptr
        character(c_char), dimension(:), pointer :: result_Fptr
        integer(c_long) :: result_len
        integer(c_long) :: m

        result_Cptr = C_iniparser_getsecname(dic, n, result_len)
        call c_f_pointer(result_Cptr, result_Fptr, [result_len]);

        !Convert from character array to scalar string
        result = ""
        do m = 1, result_len
            result(m:m) = result_Fptr(m)
        end do
    end subroutine

    subroutine iniparser_getseckeys(result, dic, sec, size)
        character(len=*,kind=c_char), dimension(:), allocatable, intent(out) :: result
        type(c_ptr), value, intent(in) :: dic
        character(c_char), dimension(*), intent(in) :: sec
        integer(c_int), intent(in) :: size

        type(ptr_package) :: result_struct
        integer(c_long), dimension(:), pointer :: result_len_Fptr
        type(c_ptr), dimension(:), pointer :: result_Cptrs_Fptr
        character(c_char), dimension(:), pointer :: result_str_Fptr
        integer(c_long) :: n, m

        result_struct = C_iniparser_getseckeys(dic, sec, size)
        call c_f_pointer(result_struct%l, result_len_Fptr, [size]);
        call c_f_pointer(result_struct%c, result_Cptrs_Fptr, [size])

        !Convert from character arrays to scalar strings
        if(allocated(result)) then
            deallocate(result)
        end if
        allocate(result(size))
        do m = 1, size
            call c_f_pointer(result_Cptrs_Fptr(m), result_str_Fptr, [result_len_Fptr(m)])
            result(m) = ""
            do n = 1, result_len_Fptr(m)
                result(m)(n:n) = result_str_Fptr(n)
            end do
        end do
    end subroutine
end module iniparser
