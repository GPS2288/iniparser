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

        function C_iniparser_getstring(reslen, dic, key, notfound) bind(c, name="iniparser_getstring_wrapper")
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: C_iniparser_getstring
            integer(c_long), intent(out) :: reslen
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            character(c_char), dimension(*), intent(in) :: notfound
        end function

        function iniparser_getint(dic, key, notfound) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int) :: iniparser_getint
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            integer(c_int), value, intent(in) :: notfound
        end function

        function iniparser_getdouble(dic, key, notfound) bind(c)
            use, intrinsic :: iso_c_binding
            real(c_double) :: iniparser_getdouble
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            real(c_double), value, intent(in) :: notfound
        end function

        function iniparser_getboolean(dic, key, notfound) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int) :: iniparser_getboolean
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            integer(c_int), value, intent(in) :: notfound
        end function

        function iniparser_getnsec(dic) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int) :: iniparser_getnsec
            type(c_ptr), value, intent(in) :: dic
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

        result_Cptr = C_iniparser_getstring(result_len, dic, key, notfound)
        call c_f_pointer(result_Cptr, result_Fptr, [result_len]);

        result = ""
        do n = 1, result_len
            result(n:n) = result_Fptr(n)
        end do
    end subroutine iniparser_getstring
end module iniparser
