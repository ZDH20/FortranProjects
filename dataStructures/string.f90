!------------------------------------------------------------------------------
! Character Array String v1.0.5
!------------------------------------------------------------------------------
!
! MODULE: stringmod
!
!> @author
!> Zachary Haskins}
!
! DESCRIPTION: 
!> Provides string support using a character array.
!
! REVISION HISTORY:
! 
! 1/4/2022 - Initial Version
! 
! 1/5/2022 - Added the equal() function             - v1.0.1
! 
! 1/6/2022 - Added the remove() function 
!          - Added the  trim() function             - v1.0.2
! 
! 1/6/2022 - Added safety to functions.             - v1.0.3
! 
! 1/6/2022 - Overloaded function equal()            - v1.0.4
! 
! 1/6/2022 - Added concat() function (overloaded)
!          - removed assign() function
!          - removed clear() function
!          - fixed memory leaks and invalid jumps   - v1.0.5 (current)
!------------------------------------------------------------------------------
module stringmod

    implicit none
    private

    ! derrived types
    public::string

    ! public subroutines
    public::init
    public::destroy
    public::append
    public::concat
    public::print
    public::replace
    public::equal
    public::remove
    public::trim
    public::toint

    ! public functions
    public::size
    public::contains
    public::get

    ! types
    type string
        character(len = 1), allocatable, dimension(:):: arr
        integer::                                       currentsize
        integer::                                       maxsize
        integer::                                       index
        integer::                                       panic
        logical, private::                              big = .false., created = .false.
    end type string

    ! public interfaces
    interface init
        procedure::initialize_string
    end interface init

    interface destroy
        procedure::destroy_string
    end interface destroy

    interface trim
        procedure::remove_front_and_back_whitespace
    end interface trim

    interface toint ! todo
        procedure::string_to_integer
    end interface toint

    interface remove 
        procedure::remove_item_in_string
    end interface remove 

    interface append
        procedure::append_to_string
    end interface append

    interface concat
        procedure::concatinate_string_to_character
        procedure::concatinate_string_to_string
    end interface concat

    interface print
        procedure::print_string
    end interface print

    interface size
        procedure::get_string_size
    end interface size

    interface contains
        procedure::does_string_contain
    end interface contains

    interface get
        procedure::retrieve_item_in_string
    end interface get

    interface replace
        procedure::replace_data_at_index
    end interface replace

    interface equal
        procedure::check_if_equal
    end interface equal

    interface equal
        procedure::check_if_equal_strings
    end interface equal

    ! private interfaces
    interface resize
        procedure::resize_string
    end interface resize

contains

    ! public subroutines
    subroutine initialize_string(this, data)

        implicit none

        class(string), intent(inout)::this
        character(*), intent(in), optional::data
        integer::i

        this%currentsize = 0
        this%maxsize = 1
        this%index = 1
        this%panic = -999

        allocate(this%arr(1))

        this%created = .true.

        if(len(data) > 100) this%big = .true.

        if(present(data)) then
            do i = 1, len(data)
                call append(this, data(i:i))
            end do
        end if

    end subroutine initialize_string

    pure subroutine remove_item_in_string(this, index)

        implicit none

        class(string), intent(inout)::this
        integer, intent(in)::index
        integer::i

        if(.not. this%created) return

        if(index .gt. this%currentsize) return

        do i = index, this%currentsize - 1
            this%arr(i) = this%arr(i + 1)
        end do

        this%currentsize = this%currentsize - 1

    end subroutine remove_item_in_string

    pure subroutine remove_front_and_back_whitespace(this)

        implicit none

        class(string), intent(inout)::this

        if(.not. this%created) return

        do
            if(this%currentsize .eq. 0) return
            if(this%arr(1) .ne. ' ') exit
            call remove(this, 1)
        end do

        do
            if(this%currentsize .eq. 0) return
            if(this%arr(this%currentsize) .ne. ' ') exit 
            call remove(this, this%currentsize)
        end do

    end subroutine remove_front_and_back_whitespace

    pure subroutine destroy_string(this)

        implicit none

        class(string), intent(inout)::this

        if(.not. this%created) return
        deallocate(this%arr)

    end subroutine destroy_string

    subroutine append_to_string(this, data)

        implicit none

        class(string), intent(inout)::this
        character(len = 1), intent(in)::data

        if(.not. this%created) return

        if(this%maxsize .eq. 256) this%big = .true.

        if(this%index .gt. this%maxsize) call resize(this)

        this%arr(this%index) = data
        this%index = this%index + 1
        this%currentsize = this%currentsize + 1

    end subroutine append_to_string

    subroutine concatinate_string_to_character(this, data)

        implicit none

        class(string), intent(inout)::this
        character(*), intent(in)::data
        integer::i

        if(.not. this%created) return

        do i = 1, len(data)
            call append(this, data(i:i))
        end do

    end subroutine concatinate_string_to_character

    subroutine concatinate_string_to_string(this, otherstring)

        implicit none

        class(string), intent(inout)::this
        class(string), intent(in)::otherstring
        integer::i

        if(.not. this%created) return

        do i = 1, size(otherstring)
            call append(this, get(otherstring, i))
        end do       

    end subroutine concatinate_string_to_string

    subroutine print_string(this)

        implicit none

        class(string), intent(in)::this
        integer::i

        if(.not. this%created) return

        do i = 1, this%currentsize
            write(*, "(A)", advance = "no") this%arr(i)
        end do
        print*, ''

    end subroutine print_string

    pure subroutine replace_data_at_index(this, index, data)

        implicit none

        class(string), intent(inout)::this
        character(len = 1), intent(in)::data
        integer, intent(in)::index

        if(.not. this%created) return

        this%arr(index) = data

    end subroutine replace_data_at_index

    ! public functions
    integer function string_to_integer(this) result(res)

        implicit none

        class(string), intent(in)::this

        res = ichar('4')

    end function string_to_integer

    logical function check_if_equal(this, comparison) result(res)

        implicit none

        class(string), intent(in)::this
        character(*), intent(in)::comparison
        integer::i

        if(.not. this%created) return

        res = .false.

        if(this%currentsize .ne. len(comparison)) return

        do i = 1, this%currentsize
            if(this%arr(i) .ne. comparison(i:i)) return
        end do

        res = .true.

    end function check_if_equal

    logical function check_if_equal_strings(this, comparison) result(res)

        implicit none

        class(string), intent(in)::this
        class(string), intent(in)::comparison
        integer::i

        if(.not. this%created) return

        res = .false.

        if(this%currentsize .ne. size(comparison)) return

        do i = 1, this%currentsize
            if(this%arr(i) .ne. get(comparison, i)) return
        end do

        res = .true.

    end function check_if_equal_strings

    integer function get_string_size(this)

        implicit none

        class(string), intent(in)::this

        get_string_size = this%currentsize

    end function get_string_size

    logical function does_string_contain(this, data)

        implicit none

        class(string), intent(in)::this
        character(len = 1), intent(in)::data
        integer::i

        does_string_contain = .false.
        
        if(.not. this%created) return

        do i = 1, size(this)
            if(this%arr(i) .eq. data) then
                does_string_contain = .true.
                exit
            end if
        end do

    end function does_string_contain
    
    character(len = 1) function retrieve_item_in_string(this, index) result(result)

        implicit none

        class(string), intent(in)::this
        integer, intent(in)::index
        
        result = '|'

        if(.not. this%created) return

        if(index .le. this%maxsize) result = this%arr(index)

    end function retrieve_item_in_string

    ! private subroutines
    subroutine resize_string(this)

        implicit none

        class(string), intent(inout)::this
        character(len = 1), dimension(this%currentsize)::newarr
        integer::i, multiple

        if(this%big) then
            multiple = this%currentsize * 2
        else
            multiple = this%currentsize + 1
        end if

        this%maxsize = multiple

        do i = 1, this%currentsize
            newarr(i) = this%arr(i)
        end do

        deallocate(this%arr)
        allocate(this%arr(multiple))

        do i = 1, size(newarr)
            this%arr(i) = newarr(i)
        end do

    end subroutine resize_string

    ! private functions

end module stringmod
