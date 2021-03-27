// This file contains a bunch of static assertions and compilation checks
// It should not be executed

#include "indexed_list.h"

#include <iterator>
#include <type_traits>

// Note, that some of the tests contain "Does not compile" lines.
// One would think, that checks like that could be done with type_traits,
// or custom SFINAE constructs. However, using these methods would yield
// false positives: the IndexedLists methods are not SFINAE-d out, so even if
// they would fail to compile, they still participate in overload resolution.
// Because of that checks like "std::is_copy_constructible" will always return
// true, same as for standard library containers.

// For now only tests with types with trivial constructors / assignments

#if defined(__cpp_lib_concepts)
#include <ranges>

static_assert(
    std::bidirectional_iterator<indexed_list::indexed_list<int>::iterator>,
    "indexed_list iterator must model std::bidirectional_iterator ");

static_assert(
    std::bidirectional_iterator<indexed_list::indexed_list<int>::const_iterator>,
    "indexed_list const_iterator must model std::bidirectional_iterator");

static_assert(
    std::bidirectional_iterator<
        indexed_list::indexed_list<int>::reverse_iterator>,
    "indexed_list reverse_iterator must model std::bidirectional_iterator ");

static_assert(std::bidirectional_iterator<
                  indexed_list::indexed_list<int>::const_reverse_iterator>,
              "indexed_list const_reverse_iterator must model "
              "std::bidirectional_iterator");

static_assert(std::ranges::sized_range<indexed_list::indexed_list<int>>,
              "indexed_list must model std::sized_range");

static_assert(std::ranges::bidirectional_range<indexed_list::indexed_list<int>>,
              "indexed_list must model std::bidirectional_range");

static_assert(std::ranges::output_range<indexed_list::indexed_list<int>, int>,
              "indexed_list must model std::output_range");

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
    CreateOnlyType() = default;

    CreateOnlyType(const CreateOnlyType& other) = delete;
    CreateOnlyType& operator=(const CreateOnlyType& other) = delete;
    CreateOnlyType(CreateOnlyType&& other)                 = delete;
    CreateOnlyType& operator=(CreateOnlyType&& other) = delete;

    ~CreateOnlyType() = default;
};

template <class T>
struct is_const_lreference : std::false_type {};
template <class T>
struct is_const_lreference<T&&> : std::false_type {};
template <class T>
struct is_const_lreference<T&> : std::false_type {};
template <class T>
struct is_const_lreference<const T&&> : std::false_type {};
template <class T>
struct is_const_lreference<const T&> : std::true_type {};

template <class T>
struct is_mutable_lreference : std::false_type {};
template <class T>
struct is_mutable_lreference<T&&> : std::false_type {};
template <class T>
struct is_mutable_lreference<T&> : std::true_type {};
template <class T>
struct is_mutable_lreference<const T&&> : std::false_type {};
template <class T>
struct is_mutable_lreference<const T&> : std::false_type {};

using MOT = MoveOnlyType;
using COT = CreateOnlyType;

template <typename T>
using IL = indexed_list::indexed_list<T>;

void indexedListConstructors() {
    // default constructor
    IL<int> l1;
    IL<MOT> l2;
    IL<COT> l3;

    // copy constructor
    IL<int> l4(l1);
    // IL<MOT> l5(l2);  // <== doesn't compile
    // IL<COT> l6(l3);  // <== doesn't compile

    // move constructor
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

void indexedListCapacity() {
    const IL<int> l1;
    const IL<MOT> l2;
    const IL<COT> l3;

    size_t s1 = l1.size();
    size_t s2 = l2.size();
    size_t s3 = l3.size();

    bool e1 = l1.empty();
    bool e2 = l2.empty();
    bool e3 = l3.empty();

    (void)s1;
    (void)s2;
    (void)s3;
    (void)e1;
    (void)e2;
    (void)e3;
}

template <typename T>
void indexedListIterators() {
    IL<T> l1;

    using It = typename IL<T>::iterator;
    // default constructor
    It it1;

    // begin() / end()
    It it2 = l1.begin();
    It it3 = l1.end();

    // direct iterator getter
    It it4 = l1.iterator_at(0);

    (void)it3;
    (void)it4;

    // incrementation, decrementaion, movement
    ++it1;
    --it1;
    it1++;
    it1--;

    it1 += 1;
    it1 += -1;
    it1 -= 1;
    it1 -= -1;

    (void)(it1 + 1);
    (void)(it1 - 1);
    (void)(it1 + -1);
    (void)(it1 - -1);
    (void)(it1 - it2);

    (void)(it1 == it2);
    (void)(it1 != it2);
    (void)(it1 < it2);
    (void)(it1 <= it2);
    (void)(it1 > it2);
    (void)(it1 >= it2);

    static_assert(
        is_mutable_lreference<decltype(*it1)>::value,
        "dereferencing non-const iterator must return non-const reference");

    for (auto&& el : l1) {
        static_assert(
            is_mutable_lreference<decltype(el)>::value,
            "dereferencing non-const iterator must return non-const reference");
    }
}

template <typename T>
void indexedListConstIterators() {
    const IL<T> l1;

    using Cit = typename IL<T>::const_iterator;
    // default constructor
    Cit it1;

    // begin() / end()
    Cit it2 = l1.begin();
    Cit it3 = l1.end();

    Cit it4 = l1.cbegin();
    Cit it5 = l1.cend();

    // direct iterator getter
    Cit it6 = l1.iterator_at(0);

    (void)it3;
    (void)it4;
    (void)it5;
    (void)it6;

    // incrementation, decrementaion, movement
    ++it1;
    --it1;
    it1++;
    it1--;

    it1 += 1;
    it1 += -1;
    it1 -= 1;
    it1 -= -1;

    (void)(it1 + 1);
    (void)(it1 - 1);
    (void)(it1 + -1);
    (void)(it1 - -1);
    (void)(it1 - it2);

    (void)(it1 == it2);
    (void)(it1 != it2);
    (void)(it1 < it2);
    (void)(it1 <= it2);
    (void)(it1 > it2);
    (void)(it1 >= it2);

    static_assert(is_const_lreference<decltype(*it1)>::value,
                  "dereferencing const iterator must return const reference");

    for (auto&& el : l1) {
        static_assert(
            is_const_lreference<decltype(el)>::value,
            "dereferencing const iterator must return const reference");
    }
}

template <typename T>
void indexedListConstAndNonConstIterators() {
    using It  = typename IL<T>::iterator;
    using Cit = typename IL<T>::const_iterator;

    It it1;
    Cit cit1;

    // copy-constructor const_iterator from iterator
    Cit cit2 = it1;
    Cit cit3(it1);

    // copy-assignment const_iterator from iterator
    cit2 = it1;

    // copy-constructor iterator from const_iterator
    // It it2 = cit1;  // <== doesn't compile
    // It it3(cit1);  // <== doesn't compile
    // copy-assignment iterator from const_iterator
    // it2 = cit1;  // <== doesn't compile

    (void)it1;
    (void)cit1;
    (void)cit2;
    (void)cit3;
}

void indexedListIteratorPack() {
    indexedListIterators<int>();
    indexedListConstIterators<int>();
    indexedListConstAndNonConstIterators<int>();
    indexedListIterators<MOT>();
    indexedListConstIterators<MOT>();
    indexedListConstAndNonConstIterators<MOT>();
    indexedListIterators<COT>();
    indexedListConstIterators<COT>();
    indexedListConstAndNonConstIterators<COT>();
}

void indexedListInserts() {
    IL<int> l1;
    IL<MOT> l2;
    IL<COT> l3;

    typename IL<int>::const_iterator cit1 = l1.end();
    typename IL<MOT>::const_iterator cit2 = l2.end();
    typename IL<COT>::const_iterator cit3 = l3.end();

    // emplace
    l1.emplace(cit1);
    l2.emplace(cit2);
    l3.emplace(cit3);

    int v1{};
    MOT v2;
    COT v3;
    (void)v3;

    // insert via copy (const&)
    l1.insert(cit1, v1);
    // l2.insert(cit2, v2);  // <== doesn't compile
    // l3.insert(cit3, v3);  // <== doesn't compile

    // insert via move
    l1.insert(cit1, std::move(v1));
    l2.insert(cit2, std::move(v2));
    // l3.insert(cit3, std::move(v3));  // <== doesn't compile

    // insert of temporary
    l1.insert(cit1, int{});
    l2.insert(cit2, MOT{});
    // l3.insert(cit3, COT{});  // <== doesn't compile

    // emplace_at
    l1.emplace_at(0);
    l2.emplace_at(0);
    l3.emplace_at(0);

    // insert_at via copy (const&)
    l1.insert_at(0, v1);
    // l2.insert_at(0, v2);  // <== doesn't compile
    // l3.insert_at(0, v3);  // <== doesn't compile

    // insert_at via move
    l1.insert_at(0, std::move(v1));
    l2.insert_at(0, std::move(v2));
    // l3.insert_at(0, std::move(v3));  // <== doesn't compile

    // insert_at of temporary
    l1.insert_at(0, int{});
    l2.insert_at(0, MOT{});
    // l3.insert_at(0, COT{});  // <== doesn't compile
}

void indexedListErasures() {
    IL<int> l1;
    IL<MOT> l2;
    IL<COT> l3;

    l1.clear();
    l2.clear();
    l3.clear();

    typename IL<int>::const_iterator cit1 = l1.end();
    typename IL<MOT>::const_iterator cit2 = l2.end();
    typename IL<COT>::const_iterator cit3 = l3.end();

    // single element erasure
    l1.erase(cit1);
    l2.erase(cit2);
    l3.erase(cit3);

    // single element positional erasure
    l1.erase_at(0);
    l2.erase_at(0);
    l3.erase_at(0);

    // multi element erasure
    l1.erase(cit1, cit1);
    l2.erase(cit2, cit2);
    l3.erase(cit3, cit3);

    // multi element positional erasure
    l1.erase_at(0, 1);
    l2.erase_at(0, 1);
    l3.erase_at(0, 1);
}

void indexedListElementAccess() {
    IL<int> l1;
    IL<MOT> l2;
    IL<COT> l3;
    const IL<int> l4;
    const IL<MOT> l5;
    const IL<COT> l6;

    (void)l1[0];
    (void)l2[0];
    (void)l3[0];
    (void)l4[0];
    (void)l5[0];
    (void)l6[0];

    static_assert(is_mutable_lreference<decltype(l1[0])>::value, "");
    static_assert(is_mutable_lreference<decltype(l2[0])>::value, "");
    static_assert(is_mutable_lreference<decltype(l3[0])>::value, "");
    static_assert(is_const_lreference<decltype(l4[0])>::value, "");
    static_assert(is_const_lreference<decltype(l5[0])>::value, "");
    static_assert(is_const_lreference<decltype(l6[0])>::value, "");

    int v1{};
    MOT v2;
    COT v3;
    (void)v3;

    // getting value via []
    v1 = l1[0];
    // v2 = l2[0];  // <== doesn't compile
    // v3 = l3[0];  // <== doesn't compile

    v1 = std::move(l1[0]);
    v2 = std::move(l2[0]);
    // v3 = std::move(l3[0]);  // <== doesn't compile

    // setting value via []
    l1[0] = v1;
    // l2[0] = v2;  // <== doesn't compile
    // l3[0] = v3;  // <== doesn't compile

    l1[0] = std::move(v1);
    l2[0] = std::move(v2);
    // l3[0] = std::move(v3);  // <== doesn't compile
}

void indexedListBulkOperations() {
    IL<int> l1;
    IL<MOT> l2;
    IL<COT> l3;

    // split by iterator
    l1.split(l1.cend());
    l2.split(l2.cend());
    l3.split(l3.cend());

    // split by iterator range
    l1.split(l1.cend(), l1.cend());
    l2.split(l2.cend(), l2.cend());
    l3.split(l3.cend(), l3.cend());

    // split by position
    l1.split_at(0);
    l2.split_at(0);
    l3.split_at(0);

    // split by position range
    l1.split_at(0, 0);
    l2.split_at(0, 0);
    l3.split_at(0, 0);

    // concat
    l1.concat(IL<int>{});
    l2.concat(IL<MOT>{});
    l3.concat(IL<COT>{});

    // merge at iterator
    l1.merge(IL<int>{}, l1.end());
    l2.merge(IL<MOT>{}, l2.end());
    l3.merge(IL<COT>{}, l3.end());

    // merge at position
    l1.merge_at(IL<int>{}, 0);
    l2.merge_at(IL<MOT>{}, 0);
    l3.merge_at(IL<COT>{}, 0);
}
