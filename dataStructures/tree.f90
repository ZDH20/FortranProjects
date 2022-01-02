module treemod

    implicit none
    private

    ! public derrived types
    public::tree

    ! private derrived types
    private::node

    ! public subroutines
    public::insert
    public::print
    public::destroy

    ! private subroutines

    ! type definitions
    type node
        integer, allocatable::data
        class(node), pointer::left => null()
        class(node), pointer::right => null()
    end type node

    type tree
        class(node), pointer::root => null()
        integer::size = 0
    end type tree

    ! public interfaces
    interface insert
        procedure::insert_into_tree
    end interface insert

    interface print
        procedure::print_tree
    end interface print

    interface destroy
        procedure::destroy_tree
    end interface destroy

    ! private interfaces
    interface newnode
        procedure::node_constructor
    end interface newnode

    interface recinsert
        procedure::recursive_insert
    end interface recinsert

    interface recprint
        procedure::recursive_print
    end interface recprint

    interface recdestroy
        procedure::recursive_destroy
    end interface recdestroy

contains

    ! public subroutines
    pure subroutine insert_into_tree(this, data)

        implicit none

        class(tree), intent(inout)::this
        integer, intent(in)::data

        if(.not. associated(this%root)) then
            allocate(this%root, source = newnode(data))
            this%size = this%size + 1
            return
        end if

        call recursive_insert(this, this%root, data)

    end subroutine insert_into_tree

    subroutine print_tree(this)

        implicit none

        class(tree), intent(inout)::this
        call recprint(this, this%root)

    end subroutine print_tree

    pure subroutine destroy_tree(this)

        implicit none

        class(tree), intent(inout)::this
        call recdestroy(this, this%root)

    end subroutine destroy_tree

    ! public functions

    ! private subroutines
    recursive pure subroutine recursive_destroy(this, nav)

        implicit none

        class(tree), intent(inout)::this
        class(node), pointer, intent(inout)::nav

        if(associated(nav)) then
            call recursive_destroy(this, nav%left)
            call recursive_destroy(this, nav%right)
            deallocate(nav)
        end if

    end subroutine recursive_destroy

    recursive pure subroutine recursive_insert(this, nav, data)

        implicit none

        class(tree), intent(inout)::this
        class(node), pointer, intent(inout)::nav
        integer, intent(in)::data

        if(.not. associated(nav)) then
            allocate(nav, source = newnode(data))
            this%size = this%size + 1
            return
        end if

        if(data .gt. nav%data) then
            call recursive_insert(this, nav%right, data)
        else if(data .lt. nav%data) then
            call recursive_insert(this, nav%left, data)
        end if

    end subroutine recursive_insert

    recursive subroutine recursive_print(this, nav)

        implicit none

        class(tree), intent(inout)::this
        class(node), pointer, intent(inout)::nav

        if(.not. associated(nav)) return

        call recursive_print(this, nav%left)
        print*, nav%data
        call recursive_print(this, nav%right)

    end subroutine recursive_print

    ! private functions
    pure function node_constructor(data)

        integer, intent(in)::data
        type(node)::node_constructor

        allocate(node_constructor%data, source = data)

    end function node_constructor

end module treemod
