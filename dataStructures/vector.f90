module vectormod

    implicit none
    private

    ! derrived types
    public::vector

    ! public subroutines
    public::init
    public::destroy
    public::pushback
    public::print
    public::savememory
    public::read
    public::replace
    public::clear
    public::sort

    ! public functions
    public::size
    public::contains
    public::get

    ! types
    type vector
        integer, allocatable, dimension(:)::arr
        integer::                           currentsize
        integer::                           maxsize
        integer::                           index
        integer::                           panic
        logical, private::                  precise, created = .false.
    end type vector

    ! public interfaces
    interface init
        procedure::initialize_vector
    end interface init

    interface destroy
        procedure::destroy_vector
    end interface destroy

    interface pushback
        procedure::push_into_vector
    end interface pushback

    interface print
        procedure::print_vector
    end interface print

    interface size
        procedure::get_vector_size
    end interface size

    interface contains
        procedure::does_vector_contain
    end interface contains 

    interface get
        procedure::retrieve_item_in_vector
    end interface get

    interface savememory
        procedure::make_vector_save_memory 
    end interface savememory

    interface read
        procedure::read_from_file
    end interface read

    interface replace
        procedure::replace_data_at_index
    end interface replace

    interface clear
        procedure::clear_vector
    end interface clear

    interface sort
        procedure::bubble_sort_vector
    end interface sort

    ! private interfaces
    interface resize
        procedure::resize_vector
    end interface resize

contains

    ! public subroutines
    pure subroutine initialize_vector(this)

        implicit none

        class(vector), intent(inout)::this

        this%currentsize = 0
        this%maxsize = 1
        this%index = 1
        this%panic = -999

        allocate(this%arr(1))

        this%created = .true.

    end subroutine initialize_vector

    pure subroutine destroy_vector(this)

        implicit none

        class(vector), intent(inout)::this

        if(this%created .eqv. .false.) return

        deallocate(this%arr)

    end subroutine destroy_vector

    subroutine push_into_vector(this, data)

        implicit none

        class(vector), intent(inout)::this
        integer, intent(in)::data

        if(this%created .eqv. .false.) return
        
        if(this%index .gt. this%maxsize) call resize(this) 
        
        this%arr(this%index) = data
        this%index = this%index + 1
        this%currentsize = this%currentsize + 1

    end subroutine push_into_vector

    subroutine print_vector(this)
        
        implicit none

        class(vector), intent(in)::this
        integer::i

        print*,''
        print*, '   ---printing---'
        do i = 1, this%currentsize
            print*, this%arr(i)
        end do
        print*, ' ---end printing---'
        print*,''

    end subroutine print_vector

    pure subroutine make_vector_save_memory(this)

        implicit none

        class(vector), intent(inout)::this
        this%precise = .true.

    end subroutine make_vector_save_memory

    subroutine read_from_file(this, file)

        implicit none

        class(vector), intent(inout)::this
        character(len=*), intent(in)::file
        integer::data

        if(this%created .eqv. .false.) return

        open(10, file = file)

        do while(.true.)
            read(10, *, end = 999) data
            call push_into_vector(this, data)
        end do

        999 continue

    end subroutine read_from_file

    pure subroutine replace_data_at_index(this, index, data)

        implicit none

        class(vector), intent(inout)::this
        integer, intent(in)::index, data

        if(this%created .eqv. .false.) return

        this%arr(index) = data 

    end subroutine replace_data_at_index

    pure subroutine clear_vector(this)

        implicit none

        class(vector), intent(inout)::this
        
        deallocate(this%arr)
       
        call init(this)

    end subroutine clear_vector

    subroutine bubble_sort_vector(this)

        implicit none

        class(vector), intent(inout)::this
        integer::i, j, temp

        do i = 1, size(this) - 1
            do j = 1, size(this) - i
                if(this%arr(j) .gt. this%arr(j + 1)) then
                    temp = this%arr(j)
                    this%arr(j) = this%arr(j + 1)
                    this%arr(j + 1) = temp
                end if
            end do
        end do

    end subroutine bubble_sort_vector

    ! public functions
    integer function get_vector_size(this)

        implicit none

        class(vector), intent(in)::this

        get_vector_size = this%currentsize
        
    end function get_vector_size

    logical function does_vector_contain(this, data)
    
        implicit none

        class(vector), intent(in)::this
        integer, intent(in)::data
        integer::i

        does_vector_contain = .false.

        do i = 1, size(this)
            if(this%arr(i) .eq. data) then
                does_vector_contain = .true.
                exit
            end if
        end do

    end function does_vector_contain

    integer function retrieve_item_in_vector(this, index)

        implicit none

        class(vector), intent(in)::this
        integer, intent(in)::index

        retrieve_item_in_vector = this%panic

        if(index .le. this%maxsize) retrieve_item_in_vector = this%arr(index)

    end function retrieve_item_in_vector

    ! private subroutines
    subroutine resize_vector(this)

        implicit none

        class(vector), intent(inout)::this
        integer, dimension(this%currentsize)::newarr
        integer::i, multiple
        
        if(this%precise .eqv. .true.) then
            multiple = this%currentsize + 1
        else
            multiple = this%maxsize * 2
        end if
       
        this%maxsize = multiple
      
        do i = 1, this%currentsize
            newarr(i) = this%arr(i)
        end do
     
        deallocate(this%arr)
        allocate(this%arr(multiple))
    
        this%arr = newarr

    end subroutine resize_vector

    ! private functions

end module vectormod
