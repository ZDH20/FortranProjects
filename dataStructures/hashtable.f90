module hashtablemod
   
    implicit none
    private

    ! derrived types
    public::hashtable
    private::bucket

    ! public subroutines
    public::init
    public::destroy
    public::assign

    ! pubic functions

    ! type definitions
    type bucket
        integer, allocatable::key
        integer, allocatable::data
        class(bucket), pointer::next => null()
    end type bucket
   
    type hashtable
        class(bucket), dimension(:), pointer::table => null()
        integer::size = 0
        integer::maxsize
        logical::created = .false. 
    end type hashtable

    ! public interfaces
    interface init
        procedure::initialize_hashtable
    end interface init

    interface destroy
        procedure::destroy_hashtable
    end interface destroy

    interface assign
        procedure::assign_to_hashtable
    end interface assign

    ! private interfaces
    interface newbucket
        procedure::bucket_constructor
    end interface newbucket

    interface hash
        procedure::hash_item
    end interface hash

contains

    ! public subroutines
    pure subroutine initialize_hashtable(this)

        implicit none

        class(hashtable), intent(inout)::this

        allocate(this%table(1))
        this%size = 0
        this%maxsize = 1
        this%created = .true.

    end subroutine initialize_hashtable

    pure subroutine destroy_hashtable(this)

        implicit none
   
        class(hashtable), intent(inout)::this
        
        deallocate(this%table)
        this%size = 0
        this%maxsize = 0
        this%created = .false.
        
    end subroutine destroy_hashtable

    subroutine assign_to_hashtable(this, key, data)

        implicit none

        class(hashtable), intent(inout)::this
        integer, intent(in)::key, data
        integer::index
       
        index = hash(this, key)
        
        print*, index

    end subroutine assign_to_hashtable

    ! public functions

    ! private subroutines

    ! private functions
    pure function bucket_constructor(key, data) result(result)

        implicit none

        integer, intent(in)::key, data
        type(bucket)::result

        allocate(result%key, source = key)
        allocate(result%data, source = data)

    end function bucket_constructor

    integer function hash_item(this, key)

        implicit none

        class(hashtable), intent(inout)::this
        integer, intent(in)::key
        integer::s

        s = this%size

        hash_item = (key + 3)
        hash_item = mod(hash_item, s)

    end function hash_item

end module hashtablemod
