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
    public::contains
    public::remove
    public::size

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

    interface size
        procedure::get_size_of_tree
    end interface size

    interface destroy
        procedure::destroy_tree
    end interface destroy

    interface contains
        procedure::does_tree_contain
    end interface contains

    interface remove
        procedure::remove_from_tree
    end interface remove

    ! private interfaces
    interface newnode
        procedure::node_constructor
    end interface newnode

    interface reccontain
        procedure::recursive_does_tree_contain
    end interface reccontain

    interface recinsert
        procedure::recursive_insert
    end interface recinsert

    interface recprint
        procedure::recursive_print
    end interface recprint

    interface recdestroy
        procedure::recursive_destroy
    end interface recdestroy

    interface recremove
        procedure::recursive_remove
    end interface recremove

contains

    ! public subroutines
    pure subroutine remove_from_tree(this, data)

        implicit none

        class(tree), intent(inout)::this
        integer, intent(in)::data

        call recremove(this, this%root, data)

    end subroutine remove_from_tree

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
        this%size = 0

    end subroutine destroy_tree

    ! public functions
    logical function does_tree_contain(this, data)

        implicit none

        class(tree), intent(inout)::this
        integer, intent(in)::data

        does_tree_contain = recursive_does_tree_contain(this, this%root, data)

    end function does_tree_contain

    integer function get_size_of_tree(this)

        implicit none

        class(tree), intent(in)::this
        get_size_of_tree = this%size

    end function get_size_of_tree

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

    recursive pure subroutine recursive_remove(this, nav, data)

        implicit none

        class(tree), intent(inout)::this
        class(node), pointer, intent(inout)::nav
        integer, intent(in)::data
        class(node), pointer::temp

        temp => null()

        if(.not. associated(nav)) return ! value not in tree

        if(data .gt. nav%data) then
            call recremove(this, nav%right, data)
        else if(data .lt. nav%data) then
            call recremove(this, nav%left, data)
        else ! found node to delete
            if(.not. associated(nav%right) .and. .not. associated(nav%left)) then
                ! leaf node
                deallocate(nav)
            else if(.not. associated(nav%left) .and. associated(nav%right)) then
                ! right child
                temp => nav%right
                nav%left => temp%left
                nav%right => temp%right
                nav%data = temp%data
                deallocate(temp)
            else if(associated(nav%left) .and. .not. associated(nav%right)) then
                ! left child
                temp => nav%left
                nav%left => temp%left
                nav%right => temp%right
                nav%data = temp%data
                deallocate(temp)
            else
                ! need to find successor node
                temp => nav%right
                do while(associated(temp%left))
                    temp => temp%left
                end do
                nav%data = temp%data
                call recremove(this, nav%right, nav%data)
            end if
            this%size = this%size - 1
        end if

    end subroutine recursive_remove

    ! private functions
    recursive logical function recursive_does_tree_contain(this, nav, data) result(result)

        implicit none

        class(tree), intent(in)::this
        class(node), pointer, intent(inout)::nav
        integer, intent(in)::data

        result = .false.

        if(.not. associated(nav)) return ! not found

        if(data .gt. nav%data) then
            result = recursive_does_tree_contain(this, nav%right, data)
        else if(data .lt. nav%data) then
            result = recursive_does_tree_contain(this, nav%left, data)
        else
            result = .true.
        end if

    end function recursive_does_tree_contain

    pure function node_constructor(data)

        integer, intent(in)::data
        type(node)::node_constructor

        allocate(node_constructor%data, source = data)

    end function node_constructor

end module treemod
