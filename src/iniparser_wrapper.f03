!> @brief
!! Wrapper for iniparser
!
!> This file contains the interfaces to C and the library functions for Fortran.
!
!> @author
!> Georg Poppe
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!   http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

module iniparser
    use, intrinsic :: iso_c_binding
    implicit none
    private
    public :: iniparser_getnsec, iniparser_getsecname, iniparser_dump_ini, iniparser_dumpsection_ini, iniparser_dump, &
              iniparser_getsecnkeys, iniparser_getseckeys, iniparser_getstring, iniparser_getstring_array, &
              iniparser_getint, iniparser_getint_array, iniparser_getdouble, iniparser_getdouble_array, &
              iniparser_getboolean, iniparser_getboolean_array, iniparser_set, iniparser_unset, iniparser_find_entry, &
              iniparser_load, iniparser_freedict

    type, bind(c) :: ptr_package
        type(c_ptr) :: c, l
    end type

    interface
        function C_iniparser_getnsec(dic) bind(c, name="iniparser_getnsec")
            use, intrinsic :: iso_c_binding
            integer(c_int) :: C_iniparser_getnsec
            type(c_ptr), value, intent(in) :: dic
        end function

        function C_iniparser_getsecname(dic, n, result_len) bind(c, name="iniparser_getsecname_wrapper")
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: C_iniparser_getsecname
            type(c_ptr), value, intent(in) :: dic
            integer(c_int), value, intent(in) :: n
            integer(c_long), intent(out) :: result_len
        end function

        subroutine C_iniparser_dump_ini(dic, filename, mode) bind(c, name="iniparser_dump_ini_wrapper")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: filename
            character(c_char), dimension(*), intent(in) :: mode
        end subroutine

        subroutine C_iniparser_dumpsection_ini(dic, sec, filename, mode) bind(c, name="iniparser_dumpsection_ini_wrapper")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: sec
            character(c_char), dimension(*), intent(in) :: filename
            character(c_char), dimension(*), intent(in) :: mode
        end subroutine

        subroutine C_iniparser_dump(dic, filename, mode) bind(c, name="iniparser_dump_wrapper")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: filename
            character(c_char), dimension(*), intent(in) :: mode
        end subroutine

        function C_iniparser_getsecnkeys(dic, sec) bind(c, name="iniparser_getsecnkeys")
            use, intrinsic :: iso_c_binding
            integer(c_int) :: C_iniparser_getsecnkeys
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: sec
        end function

        function C_iniparser_getseckeys(dic, sec, size) bind(c, name="iniparser_getseckeys_wrapper")
            use, intrinsic :: iso_c_binding
            import ptr_package
            type(ptr_package) :: C_iniparser_getseckeys
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: sec
            integer(c_int), intent(in) :: size
        end function

        function C_iniparser_getstring(dic, key, notfound, result_len) bind(c, name="iniparser_getstring_wrapper")
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: C_iniparser_getstring
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            character(c_char), dimension(*), intent(in) :: notfound
            integer(c_long), intent(out) :: result_len
        end function

        function C_iniparser_getstring_array(dic, key, delimiters, size) bind(c, name="iniparser_getstring_array_wrapper")
            use, intrinsic :: iso_c_binding
            import ptr_package
            type(ptr_package) :: C_iniparser_getstring_array
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            character(c_char), dimension(*), intent(in) :: delimiters
            integer(c_int), intent(in) :: size
        end function

        function C_iniparser_getint(dic, key, notfound) bind(c, name="iniparser_getint")
            use, intrinsic :: iso_c_binding
            integer(c_int) :: C_iniparser_getint
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

        function C_iniparser_getdouble(dic, key, notfound) bind(c, name="iniparser_getdouble")
            use, intrinsic :: iso_c_binding
            real(c_double) :: C_iniparser_getdouble
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

        function C_iniparser_getboolean(dic, key, notfound) bind(c, name="iniparser_getboolean")
            use, intrinsic :: iso_c_binding
            integer(c_int) :: C_iniparser_getboolean
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

        function C_iniparser_set(dic, key, value) bind(c, name="iniparser_set")
            use, intrinsic :: iso_c_binding
            integer(c_int) :: C_iniparser_set
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
            character(c_char), dimension(*), intent(in) :: value
        end function

        subroutine C_iniparser_unset(dic, key) bind(c, name="iniparser_unset")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
        end subroutine

        function C_iniparser_find_entry(dic, key) bind(c, name="iniparser_find_entry")
            use, intrinsic :: iso_c_binding
            integer(c_int) :: C_iniparser_find_entry
            type(c_ptr), value, intent(in) :: dic
            character(c_char), dimension(*), intent(in) :: key
        end function

        function C_iniparser_load(ininame) bind(c, name="iniparser_load")
            use, intrinsic :: iso_c_binding
            type(c_ptr) :: C_iniparser_load
            character(c_char), dimension(*), intent(in) :: ininame
        end function

        subroutine C_iniparser_freedict(dic) bind(c, name="iniparser_freedict")
            use, intrinsic :: iso_c_binding
            type(c_ptr), value, intent(in) :: dic
        end subroutine
    end interface

    contains

    !> @brief    Get number of sections in a dictionary
    !> @param    dic   Dictionary to examine
    !> @return   integer(c_int) Number of sections found in dictionary
    !
    !> This function returns the number of sections found in a dictionary.
    !! The test to recognize sections is done on the string stored in the
    !! dictionary: a section name is given as "section" whereas a key is
    !! stored as "section:key", thus the test looks for entries that do not
    !! contain a colon.
    !!
    !! This clearly fails in the case a section name contains a colon, but
    !! this should simply be avoided.
    !!
    !! This function returns -1 in case of error.
    function iniparser_getnsec(dic)
        integer(c_int) :: iniparser_getnsec
        type(c_ptr), value, intent(in) :: dic

        iniparser_getnsec = C_iniparser_getnsec(dic)
    end function

    !>@brief    Get name for section n in a dictionary.
    !!@param    result   String variable which contains the result after runtime
    !!@param    dic   Dictionary to examine
    !!@param    n   Section number (from 0 to nsec-1)
    !!
    !!This function locates the n-th section in a dictionary and returns its name.
    !!
    !!This function returns an empty string in case of error.
    subroutine iniparser_getsecname(result, dic, n)
        character(len=*), intent(out) :: result
        type(c_ptr), value, intent(in) :: dic
        integer(c_int), intent(in) :: n

        type(c_ptr) :: result_Cptr = c_null_ptr
        character(c_char), dimension(:), pointer :: result_Fptr
        integer(c_long) :: result_len
        integer(c_long) :: m

        result_Cptr = C_iniparser_getsecname(dic, n, result_len)
        call c_f_pointer(result_Cptr, result_Fptr, [result_len])

        !Convert from character array to scalar string
        result = ""
        do m = 1, result_len
            result(m:m) = result_Fptr(m)
        end do
    end subroutine

    !>@brief    Save a dictionary to a loadable ini file
    !!@param    dic   Dictionary to dump
    !!@param    filename   Name of the file where to dump
    !!@param    mode   String containing the C file access mode. Take a look at the C-function fopen() to get to know the allowed access modes.
    !!
    !!This function dumps a given dictionary into a loadable ini file.
    subroutine iniparser_dump_ini(dic, filename, mode)
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: filename
        character(len=*, kind=c_char), intent(in) :: mode

        call C_iniparser_dump_ini(dic, filename // c_null_char, mode // c_null_char)
    end subroutine

    !>@brief    Save a dictionary section to a loadable ini file
    !!@param    dic   Dictionary to dump
    !!@param    sec   Section name of dictionary to dump
    !!@param    filename   Name of the file where to dump
    !!@param    mode   String containing the C file access mode. Take a look at the C-function fopen() to get to know the allowed access modes.
    !!
    !!This function dumps a given section of a given dictionary into a loadable ini file.
    subroutine iniparser_dumpsection_ini(dic, sec, filename, mode)
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: sec
        character(len=*, kind=c_char), intent(in) :: filename
        character(len=*, kind=c_char), intent(in) :: mode

        call C_iniparser_dumpsection_ini(dic, sec // c_null_char, filename // c_null_char, mode // c_null_char)
    end subroutine

    !>@brief    Dump a dictionary to an opened file pointer.
    !!@param    dic   Dictionary to dump.
    !!@param    filename   Name of the file where to dump
    !!@param    mode   String containing the C file access mode. Take a look at the C-function fopen() to get to know the allowed access modes.
    !!
    !!This function prints out the contents of a dictionary, one element by
    !!line, onto the provided file pointer. This function is meant for debugging
    !!purposes mostly.
    subroutine iniparser_dump(dic, filename, mode)
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: filename
        character(len=*, kind=c_char), intent(in) :: mode

        call C_iniparser_dump(dic, filename // c_null_char, mode // c_null_char)
    end subroutine

    !>@brief    Get the number of keys in a section of a dictionary.
    !!@param    dic   Dictionary to examine
    !!@param    sec   Section name of dictionary to examine
    !!@return   Number of keys in section
    function iniparser_getsecnkeys(dic, sec)
        integer(c_int) :: iniparser_getsecnkeys
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: sec

        iniparser_getsecnkeys = C_iniparser_getsecnkeys(dic, sec // c_null_char)
    end function

    !>@brief    Get the number of keys in a section of a dictionary.
    !!@param    result  String array which contains the results after runtime
    !!@param    dic     Dictionary to examine
    !!@param    sec     Section name of dictionary to examine
    !!@param    size    Contains the number of result elements after runtime
    !!
    !!This function queries a dictionary and finds all keys in a given section.
    !!Each pointer in the returned char pointer-to-pointer is pointing to
    !!a string allocated in the dictionary; do not free or modify them.
    !!
    !!This function returns a zero-sized array in case of error.
    subroutine iniparser_getseckeys(result, dic, sec, size)
        character(len=*,kind=c_char), dimension(:), allocatable, intent(out) :: result
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: sec
        integer(c_int), intent(in) :: size

        type(ptr_package) :: result_struct
        integer(c_long), dimension(:), pointer :: result_len_Fptr
        type(c_ptr), dimension(:), pointer :: result_Cptrs_Fptr
        character(c_char), dimension(:), pointer :: result_str_Fptr
        integer(c_long) :: n, m

        result_struct = C_iniparser_getseckeys(dic, sec // c_null_char, size)
        call c_f_pointer(result_struct%l, result_len_Fptr, [size])
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

    !>@brief    Get the string associated to a key
    !!@param    result  String variable which contains the result after runtime
    !!@param    dic     Dictionary to search
    !!@param    key     Key string to look for
    !!@param    notfound    Default value to return if key not found.
    !!
    !!This function queries a dictionary for a key. A key as read from an
    !!ini file is given as "section:key". If the key cannot be found,
    !!the pointer passed as 'def' is returned.
    !!The returned char pointer is pointing to a string allocated in
    !!the dictionary, do not free or modify it.
    subroutine iniparser_getstring(result, dic, key, notfound)
        character(len=*), intent(out) :: result
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: key
        character(len=*, kind=c_char), intent(in) :: notfound

        type(c_ptr) :: result_Cptr = c_null_ptr
        character(c_char), dimension(:), pointer :: result_Fptr
        integer(c_long) :: result_len
        integer(c_long) :: n

        result_Cptr = C_iniparser_getstring(dic, key // c_null_char, notfound // c_null_char, result_len)
        call c_f_pointer(result_Cptr, result_Fptr, [result_len])

        !Convert from character array to scalar string
        result = ""
        do n = 1, result_len
            result(n:n) = result_Fptr(n)
        end do
    end subroutine

    !>@brief    Get the string associated to a key, split to an array
    !!@param    result  String array which contains the results after runtime
    !!@param    dic   Dictionary to search
    !!@param    key   Key string to look for
    !!@param    delimiters   Delimiter characters
    !!@param    size   Contains the number of result elements after runtime
    !!
    !!This function queries a dictionary for a key. A key as read from an
    !!ini file is given as "section:key". If the key cannot be found,
    !!an empty array (size = 0) is returned.
    !!The string will be split at every delimeter character.
    !!
    !!This function returns a zero-sized array in case of error.
    subroutine iniparser_getstring_array(result, dic, key, delimiters, size)
        character(len=*,kind=c_char), dimension(:), allocatable, intent(out) :: result
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: key
        character(len=*, kind=c_char), intent(in) :: delimiters
        integer(c_int), intent(in) :: size

        type(ptr_package) :: result_struct
        integer(c_long), dimension(:), pointer :: result_len_Fptr
        type(c_ptr), dimension(:), pointer :: result_Cptrs_Fptr
        character(c_char), dimension(:), pointer :: result_str_Fptr
        integer(c_long) :: n, m

        result_struct = C_iniparser_getstring_array(dic, key // c_null_char, delimiters // c_null_char, size)
        call c_f_pointer(result_struct%l, result_len_Fptr, [size])
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

    !>@brief    Get the string associated to a key, convert to an int
    !!@param    dic Dictionary to search
    !!@param    key Key string to look for
    !!@param    notfound Value to return in case of error
    !!@return   integer
    !!
    !!This function queries a dictionary for a key. A key as read from an
    !!ini file is given as "section:key". If the key cannot be found,
    !!the notfound value is returned.
    !!
    !!Supported values for integers include the usual C notation
    !!so decimal, octal (starting with 0) and hexadecimal (starting with 0x)
    !!are supported. Examples:
    !!
    !!- "42"      ->  42
    !!- "042"     ->  34 (octal -> decimal)
    !!- "0x42"    ->  66 (hexa  -> decimal)
    !!
    !!Warning: the conversion may overflow in various ways. Conversion is
    !!totally outsourced to strtol(), see the associated man page for overflow
    !!handling.
    function iniparser_getint(dic, key, notfound)
        integer(c_int) :: iniparser_getint
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: key
        integer(c_int), intent(in) :: notfound

        iniparser_getint = C_iniparser_getint(dic, key //c_null_char, notfound)
    end function

    !>@brief    Get the string associated to a key, convert to an int array
    !!@param    dic Dictionary to search
    !!@param    key Key string to look for
    !!@param    delimiters   Delimiter characters
    !!@param    size   Contains the number of result elements after runtime
    !!@return   Allocatable integer array
    !!
    !!This function queries a dictionary for a key. A key as read from an
    !!ini file is given as "section:key". If the key cannot be found,
    !!an empty array (size = 0) is returned.
    !!The individual numbers are seperated by the delimeter characters.
    !!
    !!Supported values for integers include the usual C notation
    !!so decimal, octal (starting with 0) and hexadecimal (starting with 0x)
    !!are supported. Examples:
    !!
    !!"42"      ->  42
    !!"042"     ->  34 (octal -> decimal)
    !!"0x42"    ->  66 (hexa  -> decimal)
    !!
    !!Warning: the conversion may overflow in various ways. Conversion is
    !!totally outsourced to strtol(), see the associated man page for overflow
    !!handling.
    !!
    !!This function returns a NULL pointer in case of error.
    function iniparser_getint_array(dic, key, delimiters, size)
        integer(c_int), dimension(:), allocatable :: iniparser_getint_array
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: key
        character(len=*, kind=c_char), intent(in) :: delimiters
        integer(c_int), intent(in) :: size

        type(c_ptr) :: result_Cptr = c_null_ptr
        integer(c_int), dimension(:), pointer :: result_Fptr

        result_Cptr = C_iniparser_getint_array(dic, key // c_null_char, delimiters // c_null_char, size)
        call c_f_pointer(result_Cptr, result_Fptr, [size])
        iniparser_getint_array = result_Fptr
    end function

    !>@brief    Get the string associated to a key, convert to a double
    !!@param    dic Dictionary to search
    !!@param    key Key string to look for
    !!@param    notfound Value to return in case of error
    !!@return   real(c_double)
    !!
    !!This function queries a dictionary for a key. A key as read from an
    !!ini file is given as "section:key". If the key cannot be found,
    !!the notfound value is returned.
    function iniparser_getdouble(dic, key, notfound)
        real(c_double) :: iniparser_getdouble
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: key
        real(c_double), intent(in) :: notfound

        iniparser_getdouble = C_iniparser_getdouble(dic, key // c_null_char, notfound)
    end function

    !>@brief    Get the string associated to a key, convert to an double array
    !!@param    dic Dictionary to search
    !!@param    key Key string to look for
    !!@param    delimiters   Delimiter characters
    !!@param    size   Contains the number of result elements after runtime
    !!@return   Allocatable real(kind=c_double) array
    !!
    !!This function queries a dictionary for a key. A key as read from an
    !!ini file is given as "section:key". If the key cannot be found,
    !!an empty array (size = 0) is returned.
    !!The individual numbers are seperated by the delimeter characters.
    !!
    !!Handling of numbers is completly delegated to atof() function. For more info look
    !!into the manual.
    !!
    !!This function returns a NULL pointer in case of error.
    function iniparser_getdouble_array(dic, key, delimiters, size)
        real(c_double), dimension(:), allocatable :: iniparser_getdouble_array
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: key
        character(len=*, kind=c_char), intent(in) :: delimiters
        integer(c_int), intent(in) :: size

        type(c_ptr) :: result_Cptr = c_null_ptr
        real(c_double), dimension(:), pointer :: result_Fptr

        result_Cptr = C_iniparser_getdouble_array(dic, key // c_null_char, delimiters // c_null_char, size)
        call c_f_pointer(result_Cptr, result_Fptr, [size])
        iniparser_getdouble_array = result_Fptr
    end function

    !>@brief    Get the string associated to a key, convert to a boolean
    !!@param    dic Dictionary to search
    !!@param    key Key string to look for
    !!@param    notfound Value to return in case of error
    !!@return   integer
    !!
    !!This function queries a dictionary for a key. A key as read from an
    !!ini file is given as "section:key". If the key cannot be found,
    !!the notfound value is returned.
    !!
    !!A true boolean is found if one of the following is matched:
    !!
    !!- A string starting with 'y'
    !!- A string starting with 'Y'
    !!- A string starting with 't'
    !!- A string starting with 'T'
    !!- A string starting with '1'
    !!
    !!A false boolean is found if one of the following is matched:
    !!
    !!- A string starting with 'n'
    !!- A string starting with 'N'
    !!- A string starting with 'f'
    !!- A string starting with 'F'
    !!- A string starting with '0'
    !!
    !!The notfound value returned if no boolean is identified, does not
    !!necessarily have to be 0 or 1.
    function iniparser_getboolean(dic, key, notfound)
        integer(c_int) :: iniparser_getboolean
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: key
        integer(c_int), intent(in) :: notfound

        iniparser_getboolean = C_iniparser_getboolean(dic, key // c_null_char, notfound)
    end function

    !>@brief    Get the string associated to a key, convert to a boolean array
    !!@param    dic Dictionary to search
    !!@param    key Key string to look for
    !!@param    delimiters   Delimiter characters
    !!@param    notfound Value to return in case of error
    !!@param    size   Contains the number of result elements after runtime
    !!@return   Allocatable integer array
    !!
    !!This function queries a dictionary for a key. A key as read from an
    !!ini file is given as "section:key". If the key cannot be found,
    !!an empty array (size = 0) is returned.
    !!The individual values are seperated by the delimeter characters.
    !!
    !!A true boolean is found if one of the following is matched:
    !!
    !!- A string starting with 'y'
    !!- A string starting with 'Y'
    !!- A string starting with 't'
    !!- A string starting with 'T'
    !!- A string starting with '1'
    !!
    !!A false boolean is found if one of the following is matched:
    !!
    !!- A string starting with 'n'
    !!- A string starting with 'N'
    !!- A string starting with 'f'
    !!- A string starting with 'F'
    !!- A string starting with '0'
    !!
    !!The notfound value returned if no boolean is identified, does not
    !!necessarily have to be 0 or 1.
    !!
    !!This function returns a NULL pointer in case of error.
    function iniparser_getboolean_array(dic, key, delimiters, notfound, size)
        integer(c_int), dimension(:), allocatable :: iniparser_getboolean_array
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: key
        character(len=*, kind=c_char), intent(in) :: delimiters
        integer(c_int), intent(in) :: notfound
        integer(c_int), intent(in) :: size

        type(c_ptr) :: result_Cptr = c_null_ptr
        integer(c_int), dimension(:), pointer :: result_Fptr

        result_Cptr = C_iniparser_getboolean_array(dic, key // c_null_char, delimiters // c_null_char, notfound, size)
        call c_f_pointer(result_Cptr, result_Fptr, [size])
        iniparser_getboolean_array = result_Fptr
    end function

    !>@brief    Set an entry in a dictionary.
    !!@param    dic   Dictionary to modify.
    !!@param    key   Key to modify
    !!@param    value   New value to associate to the entry.
    !!@return   integer: 0 if Ok, -1 otherwise.
    !!
    !!If the given entry can be found in the dictionary, it is modified to
    !!contain the provided value. If it cannot be found, -1 is returned.
    !!It is Ok to set value to an empty string ("").
    function iniparser_set(dic, key, value)
        integer(c_int) :: iniparser_set
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: key
        character(len=*, kind=c_char), intent(in) :: value

        iniparser_set = C_iniparser_set(dic, key // c_null_char, trim(value) // c_null_char)
    end function

    !>@brief    Delete an entry in a dictionary
    !!@param    dic   Dictionary to modify
    !!@param    key   Key to delete
    !!
    !!If the given entry can be found, it is deleted from the dictionary.
    subroutine iniparser_unset(dic, key)
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: key

        call C_iniparser_unset(dic, key // c_null_char)
    end subroutine

    !>@brief    Finds out if a given entry exists in a dictionary
    !!@param    dic     Dictionary to search
    !!@param    key   Name of the key to look for
    !!@return   integer 1 if entry exists, 0 otherwise
    !!
    !!Finds out if a given entry exists in the dictionary. Since sections
    !!are stored as keys with NULL associated values, this is the only way
    !!of querying for the presence of sections in a dictionary.
    function iniparser_find_entry(dic, key)
        integer(c_int) :: iniparser_find_entry
        type(c_ptr), value, intent(in) :: dic
        character(len=*, kind=c_char), intent(in) :: key

        iniparser_find_entry = C_iniparser_find_entry(dic, key // c_null_char)
    end function

    !>@brief    Parse an ini file and return an allocated dictionary object
    !!@param    ininame Name of the ini file to read.
    !!@return   Pointer(c_ptr) to newly allocated dictionary
    !!
    !!This is the parser for ini files. This function is called, providing
    !!the name of the file to be read. It returns a dictionary object that
    !!should not be accessed directly, but through accessor functions
    !!instead.
    !!
    !!The returned dictionary must be freed using iniparser_freedict().
    function iniparser_load(ininame)
            type(c_ptr) :: iniparser_load
            character(len=*, kind=c_char), intent(in) :: ininame

            iniparser_load = C_iniparser_load(ininame // c_null_char)
    end function

    !>@brief    Free all memory associated to an ini dictionary
    !!@param    dic Dictionary to free
    !!
    !!Free all memory associated to an ini dictionary.
    !!It is mandatory to call this function before the dictionary object
    !!gets out of the current context.
    subroutine iniparser_freedict(dic)
        type(c_ptr), value, intent(in) :: dic

        call C_iniparser_freedict(dic)
    end subroutine
end module iniparser
