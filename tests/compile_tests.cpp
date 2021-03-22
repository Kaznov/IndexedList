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
    std::bidirectional_iterator<indexed_list::IndexedList<int>::reverse_iterator>,
    "IndexedList reverse_iterator must model std::bidirectional_iterator ");

static_assert(
    std::bidirectional_iterator<indexed_list::IndexedList<int>::const_reverse_iterator>,
    "IndexedList const_reverse_iterator must model std::bidirectional_iterator");

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
    MoveOnlyType() {}
    MoveOnlyType(const MoveOnlyType& other) = delete;
    MoveOnlyType& operator=(const MoveOnlyType& other) = delete;
    MoveOnlyType(MoveOnlyType&& other) = default;
    MoveOnlyType& operator=(MoveOnlyType&& other) = default;
    ~MoveOnlyType() = default;
};

void indexedListConstructors() {
    // default constructor

}
