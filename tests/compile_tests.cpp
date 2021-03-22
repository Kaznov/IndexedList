#include "IndexedList.h"

#if defined(__cpp_lib_concepts)
#include <ranges>

static_assert(
    std::bidirectional_iterator<indexed_list::IndexedList<int>::iterator>,
    "IndexedList iterator must model std::bidirectional_iterator ");

static_assert(
    std::bidirectional_iterator<indexed_list::IndexedList<int>::const_iterator>,
    "IndexedList const_iterator must model std::bidirectional_iterator");

static_assert(
    std::bidirectional_iterator<
        indexed_list::IndexedList<int>::reverse_iterator>,
    "IndexedList reverse_iterator must model std::bidirectional_iterator ");

static_assert(
    std::bidirectional_iterator<
        indexed_list::IndexedList<int>::const_reverse_iterator>,
    "IndexedList const_reverse_iterator must model "
    "std::bidirectional_iterator");

static_assert(
    std::ranges::sized_range<indexed_list::IndexedList<int>>,
    "IndexedList must model std::sized_range");

static_assert(
    std::ranges::bidirectional_range<indexed_list::IndexedList<int>>,
    "IndexedList must model std::bidirectional_range");

static_assert(
    std::ranges::output_range<indexed_list::IndexedList<int>, int>,
    "IndexedList must model std::output_range");

#endif

struct MoveOnlyType {
    MoveOnlyType() = default;
    MoveOnlyType(const MoveOnlyType& other) = delete;
    MoveOnlyType& operator=(const MoveOnlyType& other) = delete;
    MoveOnlyType(MoveOnlyType&& other) = default;
    MoveOnlyType& operator=(MoveOnlyType&& other) = default;
    ~MoveOnlyType() = default;
};

struct CreateOnlyType {
    CreateOnlyType() {}
    CreateOnlyType(const CreateOnlyType& other) = delete;
    CreateOnlyType& operator=(const CreateOnlyType& other) = delete;
    CreateOnlyType(CreateOnlyType&& other) = delete;
    CreateOnlyType& operator=(CreateOnlyType&& other) = delete;
    ~CreateOnlyType() = default;
};

using MOT = MoveOnlyType;
using COT = CreateOnlyType;

template <typename T>
using IL = indexed_list::IndexedList<T>;

void indexedListConstructors() {
    // default constructor
    IL<int> l1;
    IL<MOT> l2;
    IL<COT> l3;

    // copy constructor
    IL<int> l4(l1);
    // IL<MOT> l5(l2);  // <== doesn't compile
    // IL<COT> l6(l3);  // <== doesn't compile

    //move constructor
    IL<int> l7(std::move(l1));
    IL<MOT> l8(std::move(l2));
    IL<COT> l9(std::move(l3));  // note, that type does not need to be movable

    // range constructor
    IL<int> l10(l1.begin(), l1.end());
    // IL<MOT> l11(l2.begin(), l2.end());  // <== doesn't compile
    // IL<COT> l12(l3.begin(), l3.end());  // <== doesn't compile

    // range constructor, move iterators
    IL<int> l12(std::make_move_iterator(l1.begin()),
        std::make_move_iterator(l1.end()));
    IL<MOT> l13(std::make_move_iterator(l2.begin()),
        std::make_move_iterator(l2.end()));
    /*IL<COT> l14(std::make_move_iterator(l3.begin()),  // <== doesn't compile
         std::make_move_iterator(l3.end()));*/
}

void indexedListAssignment() {
    IL<int> l1a, l1b;
    IL<MOT> l2a, l2b;
    IL<COT> l3a, l3b;

    // copy-assignment
    l1a = l1b;
    // l2a = l2b;  // <== doesn't compile
    // l3a = l3b;  // <== doesn't compile

    // move-assignment
    l1a = std::move(l1b);
    l2a = std::move(l2b);
    l3a = std::move(l3b);

    // range assignment
    l1a.assign(l1b.begin(), l1b.end());
    // l2a.assign(l2b.begin(), l2b.end());  // <== doesn't compile
    // l3a.assign(l3b.begin(), l3b.end());  // <== doesn't compile

    // range assignment, move iterators
    l1a.assign(std::make_move_iterator(l1b.begin()),
        std::make_move_iterator(l1b.end()));
    l2a.assign(std::make_move_iterator(l2b.begin()),
        std::make_move_iterator(l2b.end()));
    /*l3a.assign(std::make_move_iterator(l3b.begin()),  // <== doesn't compile
        std::make_move_iterator(l3b.end()));*/
}
