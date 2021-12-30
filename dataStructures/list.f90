module listmod
    implicit none
    private

    ! derrived types
    public::list

    ! public subroutines
    public::append, print, destroy
    
    ! public functions
    public::getsize

    ! types
    type node
        integer, allocatable::value
        class(node), pointer::next => null()
    end type node

    type list
        class(node), pointer::head => null()
        class(node), pointer::tail => null()
        integer::size
    end type list

    ! interfaces
    interface node
        module procedure::n_constructor
    end interface node

    interface append
        module procedure::append_node
    end interface append

    interface print
        module procedure::print_list
    end interface print

    interface destroy
        module procedure::destroy_list
    end interface destroy


contains

    ! public subroutines
    
    pure subroutine append_node(self, value)
        implicit none
        class(list), intent(inout)::self
        integer, intent(in)::value
        if(.not. associated(self%head)) then
            allocate(self%head, source = n_constructor(value))
            self%tail => self%head
        else
            allocate(self%tail%next, source = n_constructor(value))
            self%tail => self%tail%next
        end if
    end subroutine append_node

    subroutine print_list(self)
        implicit none
        class(list), intent(inout)::self
        class(node), pointer::nav
        nav => self%head
        do while(associated(nav)) 
            print *, nav%value
            nav => nav%next
        end do
    end subroutine print_list

    pure function n_constructor(item)
        integer, intent(in)::item
        type(node)::n_constructor
        allocate(n_constructor%value, source = item)
    end function n_constructor

    subroutine destroy_list(self)
        implicit none
        class(list), intent(inout)::self
        class(node), pointer::nav, temp
        nav => self%head
        do while(associated(nav))
            temp => nav; 
            nav => nav%next
            deallocate(temp%value)
            deallocate(temp)
        end do
    end subroutine destroy_list

    ! public functions
    integer function getsize(self) result(res)
        implicit none
        class(list), intent(in)::self
        res = self%size 
    end function getsize

end module listmod
