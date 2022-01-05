!------------------------------------------------------------------------------
! Character Array String
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
! 1/4/2022 - Initial Version
! 1/5/2022 - Added the equal() function - v1.0.1 
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
    public::assign
    public::print
    public::replace
    public::clear

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
        logical, private::                              big, created = .false.
    end type string

    ! public interfaces
    interface init
        procedure::initialize_string
    end interface init

    interface destroy
        procedure::destroy_string
    end interface destroy

    interface append
        procedure::append_to_string
    end interface append

    interface assign
        procedure::assign_to_string
    end interface assign

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

    interface clear
        procedure::clear_string
    end interface clear

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

    pure subroutine destroy_string(this)

        implicit none

        class(string), intent(inout)::this

        if(this%created .eqv. .false.) return

        deallocate(this%arr)

    end subroutine destroy_string

    subroutine append_to_string(this, data)

        implicit none

        class(string), intent(inout)::this
        character(len = 1), intent(in)::data

        if(this%created .eqv. .false.) return

        if(this%maxsize .eq. 256) this%big = .true.

        if(this%index .gt. this%maxsize) call resize(this)

        this%arr(this%index) = data
        this%index = this%index + 1
        this%currentsize = this%currentsize + 1

    end subroutine append_to_string

    subroutine assign_to_string(this, data)

        implicit none

        class(string), intent(inout)::this
        character(*), intent(in)::data
        integer::i

        do i = 1, len(data)
            call append(this, data(i:i))
        end do

    end subroutine assign_to_string

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

        if(this%created .eqv. .false.) return

        this%arr(index) = data

    end subroutine replace_data_at_index

    subroutine clear_string(this)

        implicit none

        class(string), intent(inout)::this

        deallocate(this%arr)

        call init(this, '')

    end subroutine clear_string

    ! public functions
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

        result = 'z'
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
