#ifndef INDEXED_LIST_H_
#define INDEXED_LIST_H_

// IndexedList library - open-source, single-header library for C++14
// It provides an stl-compatibile sequence container with O(log n) positional
// operations, splicing and merging. It is distributed under MIT License.
//
// Copyright(c) 2021-present Kamil Kaznowski
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <climits>
#include <cstddef>
#include <cstdint>
#include <ctime>
#include <iterator>
#include <memory>
#include <type_traits>

#include <boost/stl_interfaces/iterator_interface.hpp>

#undef INDEXED_LIST_CHECK_PRECONDITIONS
#if defined(INDEXED_LIST_ALWAYS_CHECK_PRECONDITIONS) || !defined(NDEBUG)
#define INDEXED_LIST_CHECK_PRECONDITIONS

#include <iostream>

#define INDEXED_LIST_CONTRACT_ASSERT_PREFIX "Violated IndexedList contract: "
#define INDEXED_LIST_LIBRARY_ASSERT_PREFIX                                    \
    "If you've got this error, you have found a bug in IndexedList library\n" \
    "Please, report it at: https://github.com/Kaznov/IndexedList \n"

#define INDEXED_LIST_CUSTOM_ASSERT(expr, msg1, msg2)                         \
    (!!(expr)) ? ((void)0)                                                   \
               : detail::indexedListCustomAssert(#expr, msg1 msg2, __FILE__, \
                                                 __LINE__)

#define INDEXED_LIST_CONTRACT_ASSERT(expr, msg) \
    INDEXED_LIST_CUSTOM_ASSERT(expr, INDEXED_LIST_CONTRACT_ASSERT_PREFIX, msg)
#define INDEXED_LIST_LIBRARY_ASSERT(expr, msg) \
    INDEXED_LIST_CUSTOM_ASSERT(expr, INDEXED_LIST_LIBRARY_ASSERT_PREFIX, msg)

#if defined(INDEXED_LIST_SHOW_STACKTRACE_ON_ASSERT)
#include <boost/stacktrace.hpp>
#endif

#else

#define INDEXED_LIST_CONTRACT_ASSERT(expr, msg) ((void)0)
#define INDEXED_LIST_LIBRARY_ASSERT(expr, msg) ((void)0)

#endif  // defined(INDEXED_LIST_ALWAYS_CHECK_PRECONDITIONS) || !NDEBUG

namespace indexed_list {

namespace detail {

#if defined(INDEXED_LIST_CHECK_PRECONDITIONS)
void indexedListCustomAssert(const char* expr,
                             const char* message,
                             const char* file,
                             int line) {
    std::cerr << message << "\nAssertion " << expr << " failed in " << file
              << ":" << line << "\n";

#if defined INDEXED_LIST_SHOW_STACKTRACE_ON_ASSERT
    auto stacktrace = boost::stacktrace::stacktrace(/*skip*/ 1, /*depth*/ 64);
    std::cerr << "Stacktrace:\n" << stacktrace;
#endif

    std::abort();
}
#endif

// RNG based on LCG with Knuth's constants,
// with added rotation similar to one in PCG (see pcg-random.org)
class RNG32 {
  public:
    // uses address-space layout randomization and time for seeding
    // it doesn't provide great entropy, but it's fast
    RNG32()
        : state_{static_cast<std::uint64_t>(
                     reinterpret_cast<std::uintptr_t>(this)) ^
                 static_cast<std::uint64_t>(
                     reinterpret_cast<std::uintptr_t>(&time)) ^
                 static_cast<std::uint64_t>(time(nullptr))} {}

    std::uint32_t operator()() {
        std::uint64_t oldstate = state_;
        state_ = oldstate * 6364136223846793005ULL + 1442695040888963407ULL;
        std::uint32_t shifted  = static_cast<std::uint32_t>(oldstate >> 27u);
        std::uint32_t rotation = static_cast<std::uint32_t>(oldstate >> 59u);
        if (rotation == 0)
            return shifted;
        return (shifted >> rotation) | (shifted << (32 - rotation));
    }

  private:
    std::uint64_t state_;
};

class RNG64 {
  public:
    std::uint64_t operator()() {
        return (std::uint64_t{rng_()} << 32) | rng_();
    }

  private:
    RNG32 rng_;
};

/**
 * @brief Treap implementation, that instead of ordering elements by keys,
 * keeps them in order and allows for positional/index access and modifications
 * in logarithmic time. Oblivious to stored elements.
 *
 * @detail
 * To learn more about treap data structure, read:
 * https://en.wikipedia.org/wiki/Treap
 *
 * SequencedTreap is in some ways similar to rope data structure:
 * https://en.wikipedia.org/wiki/Rope_(data_structure)
 * But instead of keeping sequences of elements in leafs, it keeps one element
 * in each node.
 *
 * This implementation operates on nodes, but doesn't manage their creation,
 * allocation nor destruction. Operations like that are provided as functors
 * to methods that need them.
 *
 * Standard usage is based on creating custom Node class,
 * derived from `SequencedTreap::Node`. Created nodes can be passed to
 * `SequencedTreap` methods, that will integrate the nodes into the treap.
 */
class SequencedTreap {
  public:
    using size_type       = std::size_t;
    using difference_type = std::ptrdiff_t;

    /**
     * @brief Treap node base class.
     * Consist only of methods checking node state, position and traversing
     * the tree. None of the methods change the state of the tree.
     */
    struct Node {
        using weight_type = size_type;
        size_type subtree_size;
        weight_type weight;

        Node* parent;
        Node* left;
        Node* right;

        ////////////////////////////////////////////////////////////////////////
        //                        Node properties                             //
        ////////////////////////////////////////////////////////////////////////
        size_type getLeftSubtreeSize() const {
            if (left == nullptr)
                return 0;
            else
                return left->subtree_size;
        }

        size_type getRightSubtreeSize() const {
            if (right == nullptr)
                return 0;
            else
                return right->subtree_size;
        }

        bool isLeftChild() const { return parent->left == this; }

        bool isRightChild() const { return parent->right == this; }

        bool isLeaf() const { return left == nullptr && right == nullptr; }

        bool isHead() const { return parent == nullptr; }

        ////////////////////////////////////////////////////////////////////////
        //                     Binary tree traversal                          //
        ////////////////////////////////////////////////////////////////////////
#define NON_CONST_NODE_BASE_METHOD(method) \
    Node* method() { return const_cast<Node*>(asConst()->method()); }

        const Node* getTreeHead() const {
            const Node* node = this;
            while (node->parent != nullptr) {
                node = node->parent;
            }

            return node;
        }
        NON_CONST_NODE_BASE_METHOD(getTreeHead);

        const Node* getLeftMostNode() const {
            const Node* node = this;
            while (node->left) {
                node = node->left;
            }

            return node;
        }
        NON_CONST_NODE_BASE_METHOD(getLeftMostNode);

        const Node* getRightMostNode() const {
            const Node* node = this;
            while (node->right) {
                node = node->right;
            }

            return node;
        }
        NON_CONST_NODE_BASE_METHOD(getRightMostNode);

        /**
         * @brief Finds parent of the left branch, that `this` is part of
         * @return If `this` is part of some left branch, return parent of this
         * branch. Otherwise, return `nullptr`.
         */
        const Node* getLeftBranchAncestor() const {
            const Node* child  = this;
            const Node* parent = this->parent;

            while (parent != nullptr && parent->left != child) {
                child  = parent;
                parent = parent->parent;
            }

            return parent;
        }
        NON_CONST_NODE_BASE_METHOD(getLeftBranchAncestor);

        /**
         * @brief Finds parent of the right branch, that `this` is part of
         * @return If `this` is part of some right branch, return parent of this
         * branch. Otherwise, return `nullptr`.
         */
        const Node* getRightBranchAncestor() const {
            const Node* child  = this;
            const Node* parent = this->parent;

            while (parent != nullptr && parent->right != child) {
                child  = parent;
                parent = parent->parent;
            }

            return parent;
        }
        NON_CONST_NODE_BASE_METHOD(getRightBranchAncestor);

        const Node* getNextNodeInOrder() const {
            if (right != nullptr) {
                return right->getLeftMostNode();
            }
            else {
                return getLeftBranchAncestor();
            }
        }
        NON_CONST_NODE_BASE_METHOD(getNextNodeInOrder);

        const Node* getPrevNodeInOrder() const {
            if (left != nullptr) {
                return left->getRightMostNode();
            }
            else {
                return getRightBranchAncestor();
            }
        }
        NON_CONST_NODE_BASE_METHOD(getPrevNodeInOrder);

        ////////////////////////////////////////////////////////////////////////
        //            Subtree-size-aware binary tree traversal                //
        ////////////////////////////////////////////////////////////////////////
        /**
         * @brief Finds nth node in the subtree, in-order
         * @param n Requested node position, must be less than this subtree size
         * @detail
         * @b Complexity: O(log((*this).subtree_size))
         */
        const Node* getNthNodeInSubtree(size_type n) const {
            INDEXED_LIST_LIBRARY_ASSERT(n < this->subtree_size,
                                        "Requested index outside of range");
            const Node* node = this;
            while (true) {
                while (node->getLeftSubtreeSize() > n) {
                    node = node->left;
                }

                n -= node->getLeftSubtreeSize();
                if (n == 0) {
                    return node;
                }
                --n;
                node = node->right;
            }

            return node;
        }

        Node* getNthNodeInSubtree(size_type n) {
            return const_cast<Node*>(asConst()->getNthNodeInSubtree(n));
        }

        /**
         * @brief Returns position of the node in the tree in in-order traversal
         * @detail
         * @b Complexity: O(log(n)), where n is tree size
         */
        size_type getPositionInTree() const {
            const Node* node = this;
            size_type pos    = 0;
            while (node != nullptr) {
                pos += getLeftSubtreeSize();
                node = node->getRightBranchAncestor();
            }

            return pos;
        }

        /**
         * @brief Returns node `distance` from `this`, in in-order traversal
         * @param distance - Requested distance from `this` node, must not go
         * out of the tree bounds.
         * @detail
         * @b Complexity: O(log(n)), where n is tree size
         */
        const Node* getAdvancedByInOrder(difference_type distance) const {
            const Node* node = this;
            if (distance < 0) {
                distance    = -distance;
                auto offset = static_cast<size_type>(distance);
                while (offset != 0) {
                    INDEXED_LIST_LIBRARY_ASSERT(
                        node != nullptr, "Requested move went into NULL");

                    auto left_subtree_size = node->getLeftSubtreeSize();
                    if (left_subtree_size >= offset) {
                        size_type node_idx = left_subtree_size - offset;
                        node   = node->left->getNthNodeInSubtree(node_idx);
                        offset = 0;
                    }
                    else {
                        node = node->getRightBranchAncestor();
                        offset -= (left_subtree_size + 1);
                    }
                }
            }
            else {
                auto offset = static_cast<size_type>(distance);

                while (offset != 0) {
                    INDEXED_LIST_LIBRARY_ASSERT(
                        node != nullptr, "Requested move went into NULL");

                    auto right_subtree_size = node->getRightSubtreeSize();
                    if (right_subtree_size >= offset) {
                        size_type node_idx = offset - 1;
                        node   = node->right->getNthNodeInSubtree(node_idx);
                        offset = 0;
                    }
                    else {
                        node = node->getLeftBranchAncestor();
                        offset -= (right_subtree_size + 1);
                    }
                }
            }

            return node;
        }

        Node* getAdvancedByInOrder(difference_type distance) {
            return const_cast<Node*>(asConst()->getAdvancedByInOrder(distance));
        }

#undef NON_CONST_NODE_BASE_METHOD

      private:
        const Node* asConst() const { return this; }
    };

    ////////////////////////////////////////////////////////////////////////////
    //                   Object creation, assigning value                     //
    ////////////////////////////////////////////////////////////////////////////
    SequencedTreap() { initHead(); }

    SequencedTreap(const SequencedTreap&) = delete;
    SequencedTreap(SequencedTreap&&)      = delete;
    SequencedTreap& operator=(const SequencedTreap&) = delete;
    SequencedTreap& operator=(SequencedTreap&&) = delete;
    ~SequencedTreap() {
        INDEXED_LIST_LIBRARY_ASSERT(isEmpty(), "Destructor would leak data");
    }

    void initHead() {
        head_.left         = nullptr;
        head_.right        = nullptr;
        head_.parent       = nullptr;
        head_.subtree_size = 1;
        head_.weight       = 0;
    }

    ////////////////////////////////////////////////////////////////////////////
    //                             Capacity                                   //
    ////////////////////////////////////////////////////////////////////////////
    size_type size() const { return head_.subtree_size - 1; }

    bool isEmpty() const { return head_.left == nullptr; }

    ////////////////////////////////////////////////////////////////////////////
    //                          Nodes getters                                 //
    ////////////////////////////////////////////////////////////////////////////
#define NON_CONST_NODE_BASE_TREAP_METHOD(method) \
    Node* method() { return const_cast<Node*>(asConst()->method()); }

    const Node* getHeadNode() const { return &head_; }
    NON_CONST_NODE_BASE_TREAP_METHOD(getHeadNode);

    const Node* getFirstNode() const { return head_.getLeftMostNode(); }
    NON_CONST_NODE_BASE_TREAP_METHOD(getFirstNode);

#undef NON_CONST_NODE_BASE_METHOD

    const Node* getNthNode(size_type n) const {
        return head_.getNthNodeInSubtree(n);
    }

    Node* getNthNode(size_type n) {
        return const_cast<Node*>(asConst()->getNthNode(n));
    }

    ////////////////////////////////////////////////////////////////////////////
    //                     Node-based modifiers                               //
    ////////////////////////////////////////////////////////////////////////////
    void insertNodeBefore(Node* node, Node* position) {
        if (position->left == nullptr) {
            assignSureLeftChild(position, node);
        }
        else {
            position = position->left->getRightMostNode();
            assignSureRightChild(position, node);
        }

        bubbleUp(node);
        fixSizeUpToRoot(node);
    }

    void extractNode(Node* node) {
        Node* parent = node->parent;
        INDEXED_LIST_LIBRARY_ASSERT(parent != nullptr,
                                    "Requested to extract head sentinel");
        Node* merged = mergeSubtrees(node->left, node->right);
        if (node->isLeftChild()) {
            assignLeftChild(parent, merged);
        }
        else {
            INDEXED_LIST_LIBRARY_ASSERT(node->isRightChild(),
                                        "Tree continuation");
        }

        fixSizeUpToRoot(merged);
        node->left = node->right = node->parent = nullptr;
    }

    ////////////////////////////////////////////////////////////////////////////
    //                        Bulk operations                                 //
    ////////////////////////////////////////////////////////////////////////////

    Node* extractAll() {
        Node* node = head_.left;
        if (node != nullptr)
            node->parent = nullptr;

        initHead();
        return node;
    }

    Node* extractRange(Node* first, Node* last) {
        Node* middle_and_right = split(first);
        SequencedTreap tmp;
        tmp.concat(middle_and_right);
        concat(tmp.split(last));
        return tmp.extractAll();
    }

    template <typename NodeEraser>
    void clear(NodeEraser& eraser) {
        clearSubtree(head_.left, eraser);
        initHead();
    }

    template <typename NodeEraser>
    void clearSubtree(Node* node, NodeEraser& eraser) {
        if (node == nullptr)
            return;

        clearSubtree(node->left, eraser);
        clearSubtree(node->right, eraser);
        eraser(node);
    }

    template <typename NodeEraser>
    void clearRange(Node* first, Node* last, NodeEraser& eraser) {
        SequencedTreap tmp;
        tmp.concat(extractRange(first, last));
        tmp.clear(eraser);
    }

    template <typename NodeCloner>
    void clone(const SequencedTreap& other, NodeCloner& cloner) {
        INDEXED_LIST_LIBRARY_ASSERT(isEmpty(), "Clone would leak");
        assignLeftChild(&head_, cloneSubtree(other.head_.left, cloner));
        updateNodeSize(&head_);
    }

    template <typename NodeCloner>
    Node* cloneSubtree(Node* node, NodeCloner& cloner) {
        if (node == nullptr)
            return nullptr;

        Node* cloned = cloner(node);
        assignLeftChild(cloned, cloneSubtree(node->left, cloner));
        assignRightChild(cloned, cloneSubtree(node->right, cloner));
        return cloned;
    }

    void takeOwnership(SequencedTreap&& other) {
        INDEXED_LIST_LIBRARY_ASSERT(isEmpty(), "Taking ownership would leal");
        assignLeftChild(&head_, other.extractAll());
    }

    ////////////////////////////////////////////////////////////////////////////
    //                       Splitting and merging                            //
    ////////////////////////////////////////////////////////////////////////////

    /**
     * @brief Splits the treap in two
     * @param position First node of the second treap
     * @return Node being head of the right tree after split
     */
    Node* split(Node* position) {
        Node splitter;
        splitter.weight       = 0;
        splitter.subtree_size = 1;
        insertNodeBefore(&splitter, position);
        assignLeftChild(&head_, splitter.left);
        updateNodeSize(&head_);

        Node* right   = splitter.right;
        right->parent = nullptr;
        return right;
    }

    void concat(Node* subtree) {
        assignLeftChild(&head_, mergeSubtrees(head_.left, subtree));
        updateNodeSize(&head_);
    }

  private:
    ////////////////////////////////////////////////////////////////////////////
    //                    Simple operations wrappers                          //
    ////////////////////////////////////////////////////////////////////////////
    const SequencedTreap* asConst() const { return this; }

    static void assignLeftChild(Node* parent, Node* left) {
        parent->left = left;
        if (left != nullptr)
            left->parent = parent;
    }

    static void assignSureLeftChild(Node* parent, Node* left) {
        parent->left = left;
        left->parent = parent;
    }

    static void assignRightChild(Node* parent, Node* right) {
        parent->right = right;
        if (right != nullptr)
            right->parent = parent;
    }

    static void assignSureRightChild(Node* parent, Node* right) {
        parent->right = right;
        right->parent = parent;
    }

    static void updateNodeSize(Node* node) {
        node->subtree_size =
            node->getLeftSubtreeSize() + node->getRightSubtreeSize() + 1;
    }

    static void rebindParent(Node* oldChild, Node* newChild) {
        INDEXED_LIST_LIBRARY_ASSERT(!oldChild->isHead(),
                                    "Cannot rebind parent of head");
        Node* parent = oldChild->parent;
        if (oldChild->isLeftChild()) {
            assignSureLeftChild(parent, newChild);
        }
        else {
            INDEXED_LIST_LIBRARY_ASSERT(oldChild->isRightChild(),
                                        "Tree continuity");
            assignSureRightChild(parent, newChild);
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    //                      Fixing treap structure                            //
    ////////////////////////////////////////////////////////////////////////////
    void fixSizeUpToRoot(Node* node) {
        while (node->parent != nullptr) {
            node = node->parent;
            updateNodeSize(node);
        }
    }

    /**
     * @brief Moves node up the tree, to fix heap property
     * @param node - Node to be moved
     */
    void bubbleUp(Node* node) {
        checkNodeIsValid(node);
        while (node->weight < node->parent->weight) {
            if (node->isLeftChild()) {
                rotateRight(node);
            }
            else {
                INDEXED_LIST_LIBRARY_ASSERT(node->isRightChild(),
                                            "Tree continuity");
                rotateLeft(node);
            }
            checkNodeIsValid(node);
            INDEXED_LIST_LIBRARY_ASSERT(node->parent != nullptr,
                                        "Broken through weight sentinel");
        }
    }

    /**
     * @brief Tree rotation, input node takes place of its parent
     * @param r Node to be rotated, must be a right child
     * @detail
     *      p                  r
     *    /   \              /   \
     *   l     r    ---\    p     b
     *        / \   ---/   / \
     *       a   b        l   a
     *
     * Input node must have a parent and a grandparent
     */
    void rotateLeft(Node* r) {
        Node* p = r->parent;
        rebindParent(p, r);
        Node* a = r->left;
        assignRightChild(p, a);
        assignSureLeftChild(r, p);
        updateNodeSize(p);
        updateNodeSize(r);
    }

    /**
     * @brief Tree rotation, input node takes place of its parent
     * @param l Node to be rotated, must be a left child
     * @detail
     *       p                 l
     *     /   \             /   \
     *    l     r   ---\    a     p
     *   / \        ---/         / \
     *  a   b                   b   r
     *
     * Input node must have a parent and a grandparent
     */
    void rotateRight(Node* l) {
        Node* p = l->parent;
        rebindParent(p, l);
        Node* b = l->right;
        assignLeftChild(p, b);
        assignSureRightChild(l, p);
        updateNodeSize(p);
        updateNodeSize(l);
    }

    Node* mergeSubtrees(Node* left, Node* right) {
        if (left == nullptr)
            return right;
        if (right == nullptr)
            return left;

        left->parent  = nullptr;
        right->parent = nullptr;

        Node* root;
        if (left->weight < right->weight) {
            root = left;
            assignSureRightChild(root, mergeSubtrees(root->right, right));
        }
        else {
            root = right;
            assignSureLeftChild(root, mergeSubtrees(left, root->left));
        }
        updateNodeSize(root);
        return root;
    }

    void checkTreeIsValid() { checkSubtreeIsValid(&head_); }

    void checkSubtreeIsValid(Node* node) {
        if (node == nullptr)
            return;

        checkNodeIsValid(node);
        checkSubtreeIsValid(node->left);
        checkSubtreeIsValid(node->right);
    }

    void checkNodeIsValid(Node* node) {
        INDEXED_LIST_LIBRARY_ASSERT(
            node->subtree_size ==
                node->getLeftSubtreeSize() + node->getRightSubtreeSize() + 1,
            "Size continuity");

        INDEXED_LIST_LIBRARY_ASSERT(
            node->left == nullptr || node->left->parent == node,
            "Tree continuity");

        INDEXED_LIST_LIBRARY_ASSERT(
            node->right == nullptr || node->right->parent == node,
            "Tree continuity");

        INDEXED_LIST_LIBRARY_ASSERT(
            node->left == nullptr || node->weight < node->left->weight,
            "Heap property");

        INDEXED_LIST_LIBRARY_ASSERT(
            node->right == nullptr || node->weight < node->right->weight,
            "Heap property");
        (void)node;
    }
    ////////////////////////////////////////////////////////////////////////////
    //                      Private data members                              //
    ////////////////////////////////////////////////////////////////////////////
    Node head_; /** Head of the tree, sentinel, always last in the order.
                Its weight is always 0, it has no element, parent and right
                child are always `nullptr`s. */
};

/**
 * @brief Iterator type for `IndexedList`
 * It meets named requirements of LecacyBidirectionalIterator
 * and models C++20 concept `std::bidirectional_iterator`.
 *
 * Additionaly, it provides O(log n) `+=`, `-=` and `-` operators.
 */
template <typename Container,
          typename ValueType = typename Container::value_type>
class IndexedListIterator : public boost::stl_interfaces::iterator_interface<
                                IndexedListIterator<Container, ValueType>,
                                std::bidirectional_iterator_tag,
                                ValueType,
                                ValueType&,
                                ValueType*,
                                typename Container::difference_type> {
    using base_type = boost::stl_interfaces::iterator_interface<
        IndexedListIterator<Container, ValueType>,
        std::bidirectional_iterator_tag,
        ValueType,
        ValueType&,
        ValueType*,
        typename Container::difference_type>;
    using this_type = IndexedListIterator<Container, ValueType>;

  public:
    using typename base_type::difference_type;
    using typename base_type::value_type;
    using base_type::operator++;
    using base_type::operator--;

    typename base_type::reference operator*() const {
        checkIteratorNotEmpty();
        INDEXED_LIST_CONTRACT_ASSERT(node->parent != nullptr,
                                     "Cannot dereference end iterator");

        return Container::getFullNode(node).value;
    }

    bool operator==(const this_type& other) const { return node == other.node; }

    this_type& operator++() {
        checkIteratorNotEmpty();
        node = node->getNextNodeInOrder();

        INDEXED_LIST_CONTRACT_ASSERT(node != nullptr,
                                     "Iterator moves beyond valid range");

        return *this;
    }

    this_type& operator--() {
        checkIteratorNotEmpty();
        node = node->getPrevNodeInOrder();

        INDEXED_LIST_CONTRACT_ASSERT(node != nullptr,
                                     "Iterator moves beyond valid range");

        return *this;
    }

    this_type& operator+=(difference_type distance) {
        checkIteratorNotEmpty();
        INDEXED_LIST_CONTRACT_ASSERT(
            static_cast<typename Container::size_type>(
                distance +
                static_cast<difference_type>(node->getPositionInTree())) <
                node->getTreeHead()->subtree_size,
            "Iterator moves beyond valid range");
        node = node->getAdvancedByInOrder(distance);
        return *this;
    }

    difference_type operator-(const this_type& other) const {
        checkIteratorNotEmpty();
        other.checkIteratorNotEmpty();
        return static_cast<difference_type>(node->getPositionInTree()) -
               static_cast<difference_type>(other.node->getPositionInTree());
    }

    operator IndexedListIterator<Container, std::add_const_t<ValueType>>() {
        return {node};
    }

    IndexedListIterator() = default;

  protected:
    using NodeBase =
        std::conditional_t<std::is_const<ValueType>::value,
                           std::add_const_t<typename Container::NodeBase>,
                           typename Container::NodeBase>;

    IndexedListIterator(NodeBase* node) : node(node) {}

  private:
    void checkIteratorNotEmpty() const {
        INDEXED_LIST_CONTRACT_ASSERT(
            node != nullptr, "Cannot operate on default-constructed iterator");
    }

    NodeBase* node = nullptr;

    friend Container;
    friend IndexedListIterator<Container, std::remove_const_t<ValueType>>;
};

template <typename Container>
using IndexedListConstIterator =
    IndexedListIterator<Container,
                        std::add_const_t<typename Container::value_type>>;
}  // namespace detail

/**
 * @brief Structure that allows for indexed operations in average O(log n) time
 * @tparam T Type of elements to be stored in the container. Does not need to be
 * copyable nor movable.
 *
 * @detail
 * Indexed list allows for:
 * Accessing element at position in O(log n)
 * Inserting or erasing an element at position in O(log n)
 * Splitting into to `IndexedList`s at position, in O(log n)
 * Merging in another `IndexedList` at position, in O(log n)
 *
 * You can think of it as a std::vector without iterator invalidation,
 * that allows for quite fast operations in the middle, splitting and merging.
 *
 * It provides standard-compatibile bidirectional iterators, that meet
 * LegacyBidirectionalIterator named requirement. The ++ and -- operations
 * complexity is amortized to O(1), when iterating over the entire container.
 *
 * Additionally, iterators provide O(log n) operators `+=`, `-=`, `-`.
 *
 * Only erasing an element invalidates iterators to it.
 * After splitting or merging, that results in element being moved to another
 * `IndexedList` container, existing iterators become iterators to the new
 * container.
 *
 * The `end()` iterator is never invalidated, but doesn't move between
 * containers.
 *
 * This container is implemented as heap (randomized binary search tree).
 * The height of the tree is O(log n) 'with high probability', but in case a
 * malicious party gets access to RNG state / nodes weight, it may perform an
 * attack against the RNG and increase the tree height up to O(n). In this case,
 * it may result in stack overflow, as some of the operations are recursive.
 *
 * IndexedList is NOT allocator-aware. Internally it uses std::allocator.
 *
 * IndexedList does NOT give any exception quarantees. If an allocation,
 * object constructor or assignment throws, the object might enter invalid state
 * and program has an Undefined Behaviour. You are on your own. It is planned
 * to add exceptions guarantees in the future.
 *
 * IndexedList does NOT meet the requirements of SequenceContainer, even though
 * logically it is a sequence container. I don't agree with some API decisions,
 * especially with "repeat n times" insertions and std::initializer_list.
 *
 * Public methods check most preconditions as assertions.
 *
 * <b> C++20 Concepts</b>
 * IndexedList models std::bidirectional_range and std::sized_range concepts.
 * Iterators model std::bidirectional_iterator concept.
 */
template <typename T>
class IndexedList {
  private:
    static_assert(!std::is_const<T>::value,
                  "Container must have a non-const value type");
    static_assert(!std::is_volatile<T>::value,
                  "Container must have a non-volatile value type");
    static_assert(
        !std::is_reference<T>::value,
        "Cannot create a container of references, use std::reference_wrapper");

    using Impl      = detail::SequencedTreap;
    using this_type = IndexedList;

  public:
    using value_type      = T;
    using size_type       = Impl::size_type;
    using difference_type = Impl::difference_type;
    using reference       = value_type&;
    using const_reference = const value_type&;
    using pointer         = value_type*;
    using const_pointer   = const value_type*;

    using iterator               = detail::IndexedListIterator<this_type>;
    using const_iterator         = detail::IndexedListConstIterator<this_type>;
    using reverse_iterator       = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    ////////////////////////////////////////////////////////////////////////////
    //                   Object creation, assigning value                     //
    ////////////////////////////////////////////////////////////////////////////

    /**
     * @brief Creates an empty container
     */
    IndexedList();
    /**
     * @brief Constructs container from range [`first`, `last`)
     * @tparam InputIt Iterator type, must model LegacyInputIterator
     * @param first Left border of the assigned range, inclusive
     * @param last Right border of the assigned range, exclusive
     * @detail
     * @b Complexity: O(n * log(n)), where n is distance between
     * `first` and `last`
     */
    template <class InputIt>
    IndexedList(InputIt first, InputIt last);

    /**
     * @brief Constructs a copy of `other` container
     * @param other IndexedList to be cloned
     * @detail
     * @b Complexity: O(other.size())
     */
    IndexedList(const IndexedList& other);
    /**
     * @brief Constructs container by taking ownership of the elements of the
     * `other` container. The `other` container becomes empty in the process.
     * @param other Rvalue to other IndexedList
     * @detail
     * @b Complexity: O(1)
     */
    IndexedList(IndexedList&& other);
    /**
     * @brief Replaces content of `*this` with copy of content of the `other`.
     * All the elements currently stored in `*this` are discarded.
     * @param other IndexedList to be cloned
     * @detail
     * @b Complexity: O((*this).size() + other.size())
     */
    IndexedList& operator=(const IndexedList& other);
    /**
     * @brief Replaces content of `*this` with content of the`other`.
     * All the elements currently stored in `*this` are discarded.
     * `other` becomes empty in the process.
     * @param other Rvalue to other IndexedList
     * @detail
     * @b Complexity: O((*this).size() + 1)
     */
    IndexedList& operator=(IndexedList&& other);
    ~IndexedList();

    /**
     * @brief Replaces content of `*this` with content of the range
     * [`first`, `last`)
     * @tparam InputIt Iterator type, must model LegacyInputIterator
     * @param first Left border of the assigned range, inclusive
     * @param last Right border of the assigned range, exclusive
     * @detail
     * @b Complexity: O((*this).size() + std::distance(first, last))
     */
    template <class InputIt>
    void assign(InputIt first, InputIt last);

    ////////////////////////////////////////////////////////////////////////////
    //                            Element access                              //
    ////////////////////////////////////////////////////////////////////////////
    /**
     * @brief Returns a mutable reference to element at position `pos`
     * @param pos Position of the element to be accessed
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    reference operator[](size_type pos);
    /**
     * @brief Returns a const reference to element at position `pos`
     * @param pos Position of the element to be accessed
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    const_reference operator[](size_type pos) const;

    ////////////////////////////////////////////////////////////////////////////
    //                              Iterators                                 //
    ////////////////////////////////////////////////////////////////////////////

    /**
     * @brief Returns an iterator to the first element.
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    iterator begin();
    /**
     * @brief Returns a const iterator to the first element.
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    const_iterator begin() const;
    /**
     * @brief Returns a const iterator to the first element.
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    const_iterator cbegin() const;
    /**
     * @brief Returns a past-the-end iterator. It is not dereferenceable.
     * @detail
     * @b Complexity: O(1)
     */
    iterator end();
    /**
     * @brief Returns a past-the-end const iterator. It is not dereferenceable.
     * @detail
     * @b Complexity: O(1)
     */
    const_iterator end() const;
    /**
     * @brief Returns a past-the-end const iterator. It is not dereferenceable.
     * @detail
     * @b Complexity: O(1)
     */
    const_iterator cend() const;

    /**
     * @brief Returns a reverse iterator to the first element of the reversed
     * container. It corresponds to the last element of the non-reversed
     * container.
     * @detail
     * @b Complexity: O(1)
     */
    reverse_iterator rbegin();
    /**
     * @brief Returns a const reverse iterator to the first element of the
     * reversed container. It corresponds to the last element of the
     * non-reversed container.
     * @detail
     * @b Complexity: O(1)
     */
    const_reverse_iterator rbegin() const;
    /**
     * @brief Returns a const reverse iterator to the first element of the
     * reversed container. It corresponds to the last element of the
     * non-reversed container.
     * @detail
     * @b Complexity: O(1)
     */
    const_reverse_iterator crbegin() const;
    /**
     * @brief Returns a past-the-end reverse iterator.
     * It corresponds to the element preceeding the first element of the
     * non-reversed container. It is not dereferenceable.
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    reverse_iterator rend();
    /**
     * @brief Returns a past-the-end const reverse iterator.
     * It corresponds to the element preceeding the first element of the
     * non-reversed container. It is not dereferenceable.
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    const_reverse_iterator rend() const;
    /**
     * @brief Returns a past-the-end const reverse iterator.
     * It corresponds to the element preceeding the first element of the
     * non-reversed container. It is not dereferenceable.
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    const_reverse_iterator crend() const;

    /**
     * @brief Returns an iterator to element at given position.
     * If `pos == size()`, returns past-the-end iterator.
     * It is usually faster, than `begin() + pos`
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    iterator iterator_at(size_type pos);
    /**
     * @brief Returns a const iterator to element at given position.
     * If `pos == size()`, returns past-the-end const iterator.
     * It is usually faster, than `cbegin() + pos`
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    const_iterator iterator_at(size_type pos) const;

    ////////////////////////////////////////////////////////////////////////////
    //                               Capacity                                 //
    ////////////////////////////////////////////////////////////////////////////
    /**
     * @brief Checks if the container is empty
     * @return `true` if the container is empty, `false` otherwise
     * @detail
     * @b Complexity: O(1)
     */
    bool empty() const;
    /**
     * @brief Returns size of the container
     * @return Number of elements in the container
     * @detail
     * @b Complexity: O(1)
     */
    size_type size() const;

    ////////////////////////////////////////////////////////////////////////////
    //                              Modifiers                                 //
    ////////////////////////////////////////////////////////////////////////////
    /**
     * @brief Clears the content of the container.
     * @detail
     * @b Complexity: O((*this).size())
     */
    void clear();
    /**
     * @brief Inserts a new element at the specified location in the container.
     * @param pos Iterator before which the element will be inserted. Might be
     * an `end()` iterator.
     * @param value Element to be inserted.
     * @return Iterator pointing to the inserted value
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    iterator insert(const_iterator pos, const value_type& value);
    /**
     * @brief Inserts a new element at the specified location in the container.
     * @param pos Iterator before which the element will be inserted. Might be
     * an `end()` iterator.
     * @param value Element to be inserted.
     * @return Iterator pointing to the inserted value
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    iterator insert(const_iterator pos, value_type&& value);
    /**
     * @brief Inserts a new element at the specified location in the container.
     * @param pos Index, at which the element will be inserted. It might be
     * equal to `size()`
     * @param value Element to be inserted.
     * @return Iterator pointing to the inserted value
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    iterator insert_at(size_type pos, const value_type& value);
    /**
     * @brief Inserts a new element at the specified location in the container.
     * @param pos Index, at which the element will be inserted. It might be
     * equal to `size()`
     * @param value Element to be inserted.
     * @return Iterator pointing to the inserted value
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    iterator insert_at(size_type pos, value_type&& value);

    /**
     * @brief Inserts a new element at the specified location in the container,
     * constructed in-place with the given `args`.
     * @param pos Iterator before which the element will be inserted. Might be
     * an `end()` iterator.
     * @param value Element to be inserted.
     * @return Iterator pointing to the inserted value
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    template <class... Args>
    iterator emplace(const_iterator pos, Args&&... args);
    /**
     * @brief Inserts a new element at the specified location in the container,
     * constructed in-place with the given `args`.
     * @param pos Index, at which the element will be inserted. It might be
     * equal to `size()`.
     * @param value Element to be inserted.
     * @return Iterator pointing to the inserted value
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    template <class... Args>
    iterator emplace_at(size_type pos, Args&&... args);

    /**
     * @brief Removes specified element from the container.
     * @param pos Iterator to the element to remove
     * @return Iterator following the removed element
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    iterator erase(const_iterator pos);
    /**
     * @brief Removes a range of elements from the container.
     * Removes the elements in the range [`first`; `last`).
     * @param first Iterator to the first element to remove
     * @param last Iterator following the last element to remove
     * @return Iterator following the last removed element
     * @detail
     * @b Complexity: O(log((*this).size()) + std::distance(first, last))
     */
    iterator erase(const_iterator first, const_iterator last);
    /**
     * @brief Removes element at specified position from the container.
     * @param pos Position of the element to remove
     * @return Iterator following the removed element
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    iterator erase_at(size_type pos);
    /**
     * @brief Removes a range of elements from the container.
     * Removes the elements at positions in range [`pos_first`, `pos_last`).
     * @param first Position of the first element to remove
     * @param last Position 1 past the position of the last element to remove
     * @return Iterator following the last removed element
     * @detail
     * @b Complexity: O(log((*this).size()) + (pos_last - pos_first))
     */
    iterator erase_at(size_type pos_first, size_type pos_last);

    ////////////////////////////////////////////////////////////////////////////
    //                          Bulk operations                               //
    ////////////////////////////////////////////////////////////////////////////
    /**
     * @brief Splits container into two.
     * Elements from the `first` onward will be moved to a new container.
     * @param first Iterator to the first element in the new container
     * @return A new container, with elements from range [`first`, `end()`)
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    IndexedList split(const_iterator first);
    /**
     * @brief Splits container into two. Elements from range described by
     * input parameters will be moved to a new container.
     * @param Iterator to the first element in the new container
     * @param Iterator past-the-last element in the new container
     * @return A new container, with elements from range [`first`, `last`)
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    IndexedList split(const_iterator first, const_iterator last);
    /**
     * @brief Splits container into two. Elements from position `pos_first`
     * onward will be moved to a new container.
     * @param pos_first Position of the first element in the new container
     * @return A new container, with elements from positions
     * [`pos_first`, `size()`)
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    IndexedList split_at(size_type pos_first);
    /**
     * @brief Splits container into two. Elements from positions in range
     * described by input parameters will be moved to to a new container.
     * @param pos_first Position of the first element in the new container
     * @param pos_last Position 1 past the position of the last element in the
     * new container
     * @return A new container, with elements from position
     * [`pos_first`, `pos_last`)
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    IndexedList split_at(size_type pos_first, size_type pos_last);

    /**
     * @brief Places content of the `other` at the end of `*this`. Equivalent to
     * `merge(other, end())`.
     * @param other Other container, its content will be moved to this container
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    void concat(IndexedList&& other);
    /**
     * @brief Merges in the content of `other` at given position.
     * @param other Other container, its content will be moved to this container
     * @param pos Iterator, before which the elements will be inserted, might be
     * an `end()` iterator.
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    void merge(IndexedList&& other, const_iterator pos);
    /**
     * @brief Merges in the content of `other` at given position.
     * @param other Other container, its content will be moved to this container
     * @param pos Position, at which the elements will be inserted, might be
     * `size()`.
     * @detail
     * @b Complexity: O(log((*this).size()))
     */
    void merge_at(IndexedList&& other, size_type pos);

#if defined(INDEXED_LIST_TESTING)
    void checkTreeIsValid();
#endif  // INDEXED_LIST_TESTING

  private:
    using NodeBase = Impl::Node;
    friend struct Node;
    friend class detail::IndexedListIterator<this_type>;
    friend class detail::IndexedListIterator<this_type, const value_type>;

    class Node : public NodeBase {
      public:
        template <typename... Args>
        Node(Args&&... args) : value(std::forward<Args>(args)...) {}

        Node(const Node&) = delete;
        Node(Node&&)      = delete;
        Node& operator=(const Node&) = delete;
        Node& operator=(Node&&) = delete;

        value_type value;
    };

    using RNG =
        std::conditional_t<CHAR_BIT * sizeof(NodeBase::weight_type) <= 32,
                           detail::RNG32,
                           detail::RNG64>;
    using real_allocator_type = std::allocator<Node>;

    ////////////////////////////////////////////////////////////////////////////
    //                     Precondition assertions                            //
    ////////////////////////////////////////////////////////////////////////////

    void checkIndexesInOrder(size_type pos1, size_type pos2) const;
    void checkIndexIsValid(size_type pos) const;
    void checkIndexIsValidInclusive(size_type pos) const;
    void checkIteratorsInOrder(const_iterator it1, const_iterator it2) const;
    void checkIteratorIsValid(const_iterator it) const;
    template <typename InputIt>
    void checkIteratorNotFromContainer(InputIt first, InputIt last) const;

    ////////////////////////////////////////////////////////////////////////////
    //                          Node casting                                  //
    ////////////////////////////////////////////////////////////////////////////

    static const Node& getFullNode(const NodeBase* node);
    static Node& getFullNode(NodeBase* node);
    NodeBase* getNotConstNode(const_iterator it);

    ////////////////////////////////////////////////////////////////////////////
    //                    Node creation and destruction                       //
    ////////////////////////////////////////////////////////////////////////////

    void clearNode(NodeBase* node);
    NodeBase* cloneNode(const NodeBase* node);
    real_allocator_type getAllocator();
    template <typename... Args>
    NodeBase* getNewNode(Args&&...);

    class NodeCloner {
        this_type* thiz_;

      public:
        NodeCloner(this_type* thiz) : thiz_(thiz) {}
        NodeBase* operator()(const NodeBase* other) {
            return thiz_->cloneNode(other);
        }
    };
    NodeCloner getNodeCloner() { return NodeCloner(this); }

    class NodeEraser {
        this_type* thiz_;

      public:
        NodeEraser(this_type* thiz) : thiz_(thiz) {}
        void operator()(NodeBase* node) { thiz_->clearNode(node); }
    };
    NodeEraser getNodeEraser() { return NodeEraser(this); }

    ////////////////////////////////////////////////////////////////////////////
    //                      Private data members                              //
    ////////////////////////////////////////////////////////////////////////////
    Impl impl_;
    RNG rng_;
};

template <typename T>
inline IndexedList<T>::IndexedList() = default;

template <typename T>
template <class InputIt>
inline IndexedList<T>::IndexedList(InputIt first, InputIt last) {
    assign(first, last);
}

template <typename T>
inline IndexedList<T>::IndexedList(const IndexedList& other) {
    INDEXED_LIST_CONTRACT_ASSERT(&other != this,
                                 "You can't initialize an item with itself");
    auto cloner = getNodeCloner();
    impl_.clone(other.impl_, cloner);
}

template <typename T>
inline IndexedList<T>::IndexedList(IndexedList&& other) {
    INDEXED_LIST_CONTRACT_ASSERT(&other != this,
                                 "You can't initialize an item with itself");
    impl_.takeOwnership(std::move(other.impl_));
}

template <typename T>
inline IndexedList<T>&  //
IndexedList<T>::operator=(const IndexedList& other) {
    if (&other == this)
        return *this;

    clear();
    auto cloner = getNodeCloner();
    impl_.clone(other.impl_, cloner);

    return *this;
}

template <typename T>
inline IndexedList<T>&  //
IndexedList<T>::operator=(IndexedList&& other) {
    if (&other == this)
        return *this;

    clear();
    impl_.takeOwnership(std::move(other.impl_));

    return *this;
}

template <typename T>
inline IndexedList<T>::~IndexedList() {
    clear();
}

template <typename T>
template <class InputIt>
inline void  //
IndexedList<T>::assign(InputIt first, InputIt last) {
    static_assert(std::is_constructible<T, decltype(*first)>::value,
                  "Cannot construct container object from given iterator");
    checkIteratorNotFromContainer(first, last);
    clear();
    for (; first != last; ++first) {
        insert(end(), *first);
    }
}

template <typename T>
inline typename IndexedList<T>::reference  //
IndexedList<T>::operator[](size_type pos) {
    checkIndexIsValid(pos);
    return getFullNode(impl_.getNthNode(pos)).value;
}

template <typename T>
inline typename IndexedList<T>::const_reference  //
IndexedList<T>::operator[](size_type pos) const {
    checkIndexIsValid(pos);
    return getFullNode(impl_.getNthNode(pos)).value;
}

template <typename T>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::begin() {
    return iterator(impl_.getFirstNode());
}

template <typename T>
inline typename IndexedList<T>::const_iterator  //
IndexedList<T>::begin() const {
    return const_iterator(impl_.getFirstNode());
}

template <typename T>
inline typename IndexedList<T>::const_iterator  //
IndexedList<T>::cbegin() const {
    return begin();
}

template <typename T>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::end() {
    return iterator(impl_.getHeadNode());
}

template <typename T>
inline typename IndexedList<T>::const_iterator  //
IndexedList<T>::end() const {
    return const_iterator(impl_.getHeadNode());
}

template <typename T>
inline typename IndexedList<T>::const_iterator  //
IndexedList<T>::cend() const {
    return end();
}

template <typename T>
inline typename IndexedList<T>::reverse_iterator  //
IndexedList<T>::rbegin() {
    return reverse_iterator(end());
}

template <typename T>
inline typename IndexedList<T>::const_reverse_iterator  //
IndexedList<T>::rbegin() const {
    return const_reverse_iterator(end());
}

template <typename T>
inline typename IndexedList<T>::const_reverse_iterator  //
IndexedList<T>::crbegin() const {
    return rbegin();
}

template <typename T>
inline typename IndexedList<T>::reverse_iterator  //
IndexedList<T>::rend() {
    return reverse_iterator(begin());
}

template <typename T>
inline typename IndexedList<T>::const_reverse_iterator  //
IndexedList<T>::rend() const {
    return const_reverse_iterator(begin());
}

template <typename T>
inline typename IndexedList<T>::const_reverse_iterator  //
IndexedList<T>::crend() const {
    return rend();
}

template <typename T>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::iterator_at(size_type pos) {
    checkIndexIsValidInclusive(pos);

    return iterator(impl_.getNthNode(pos));
}

template <typename T>
inline typename IndexedList<T>::const_iterator  //
IndexedList<T>::iterator_at(size_type pos) const {
    checkIndexIsValidInclusive(pos);
    return const_iterator(impl_.getNthNode(pos));
}

template <typename T>
inline bool  //
IndexedList<T>::empty() const {
    return impl_.isEmpty();
}

template <typename T>
inline typename IndexedList<T>::size_type  //
IndexedList<T>::size() const {
    return impl_.size();
}

template <typename T>
inline void  //
IndexedList<T>::clear() {
    auto eraser = getNodeEraser();
    impl_.clear(eraser);
}

template <typename T>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::insert(const_iterator pos, const value_type& value) {
    checkIteratorIsValid(pos);

    NodeBase* node     = getNewNode(value);
    NodeBase* pos_node = getNotConstNode(pos.node);
    impl_.insertNodeBefore(node, pos_node);
    return iterator(node);
}

template <typename T>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::insert(const_iterator pos, value_type&& value) {
    checkIteratorIsValid(pos);

    NodeBase* node     = getNewNode(std::move(value));
    NodeBase* pos_node = getNotConstNode(pos.node);
    impl_.insertNodeBefore(node, pos_node);
    return iterator(node);
}

template <typename T>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::insert_at(size_type pos, const value_type& value) {
    checkIndexIsValidInclusive(pos);
    return insert(iterator_at(pos), value);
}

template <typename T>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::insert_at(size_type pos, value_type&& value) {
    checkIndexIsValidInclusive(pos);
    return insert(iterator_at(pos), std::move(value));
}

template <typename T>
template <class... Args>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::emplace(const_iterator pos, Args&&... args) {
    checkIteratorIsValid(pos);
    NodeBase* node     = getNewNode(std::forward<Args>(args)...);
    NodeBase* pos_node = getNotConstNode(pos.node);
    impl_.insertNodeBefore(node, pos_node);
    return iterator(node);
}

template <typename T>
template <class... Args>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::emplace_at(size_type pos, Args&&... args) {
    checkIndexIsValidInclusive(pos);
    return emplace(iterator_at(pos), std::forward<Args>(args)...);
}

template <typename T>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::erase(const_iterator pos) {
    checkIteratorIsValid(pos);
    INDEXED_LIST_CONTRACT_ASSERT(pos != end(), "Cannot erase end iterator");
    NodeBase* node        = getNotConstNode(pos);
    iterator next_element = ++iterator(node);
    impl_.extractNode(node);
    return next_element;
}

template <typename T>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::erase(const_iterator first, const_iterator last) {
    checkIteratorIsValid(first);
    checkIteratorIsValid(last);
    checkIteratorsInOrder(first, last);

    auto eraser     = getNodeEraser();
    auto first_node = getNotConstNode(first);
    auto last_node  = getNotConstNode(last);
    impl_.clearRange(first_node, last_node, eraser);
    return {last_node};
}

template <typename T>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::erase_at(size_type pos) {
    checkIndexIsValid(pos);
    return erase(iterator_at(pos));
}

template <typename T>
inline typename IndexedList<T>::iterator  //
IndexedList<T>::erase_at(size_type pos_first, size_type pos_last) {
    checkIndexIsValidInclusive(pos_first);
    checkIndexIsValidInclusive(pos_last);
    checkIndexesInOrder(pos_first, pos_last);

    return erase(iterator_at(pos_first), iterator_at(pos_last));
}

template <typename T>
inline IndexedList<T>  //
IndexedList<T>::split(const_iterator first) {
    checkIteratorIsValid(first);
    if (first == end())
        return {};
    NodeBase* pos_node = getNotConstNode(first);
    IndexedList right_part;
    right_part.impl_.concat(impl_.split(pos_node));
    return right_part;
}

template <typename T>
inline IndexedList<T>  //
IndexedList<T>::split(const_iterator first, const_iterator last) {
    checkIteratorIsValid(first);
    checkIteratorIsValid(last);
    checkIteratorsInOrder(first, last);
    IndexedList right_part = split(first);
    if (last == end())
        return right_part;
    NodeBase* pos_node = getNotConstNode(last);
    impl_.concat(right_part.impl_.split(pos_node));
    return right_part;
}

template <typename T>
inline IndexedList<T>  //
IndexedList<T>::split_at(size_type pos_first) {
    checkIndexIsValidInclusive(pos_first);
    return split(iterator_at(pos_first));
}

template <typename T>
inline IndexedList<T>  //
IndexedList<T>::split_at(size_type pos_first, size_type pos_last) {
    checkIndexIsValidInclusive(pos_first);
    checkIndexIsValidInclusive(pos_last);
    checkIndexesInOrder(pos_first, pos_last);
    return split(iterator_at(pos_first));
}

template <typename T>
inline void  //
IndexedList<T>::concat(IndexedList&& other) {
    impl_.concat(other.impl_.extractAll());
}

template <typename T>
inline void  //
IndexedList<T>::merge(IndexedList&& other, const_iterator pos) {
    checkIteratorIsValid(pos);
    NodeBase* pos_node   = getNotConstNode(pos);
    NodeBase* right_part = impl_.split(pos_node);
    impl_.concat(other.impl_.extractAll());
    impl_.concat(right_part);
}

template <typename T>
inline void  //
IndexedList<T>::merge_at(IndexedList&& other, size_type pos) {
    checkIndexIsValidInclusive(pos);
    return merge(std::move(other), iterator_at(pos));
}

#if defined(INDEXED_LIST_TESTING)
template <typename T>
inline void  //
IndexedList<T>::checkTreeIsValid() {
    impl_.checkTreeIsValid();
}
#endif

template <typename T>
inline void  //
IndexedList<T>::checkIndexesInOrder(size_type pos1, size_type pos2) const {
    INDEXED_LIST_CONTRACT_ASSERT(
        pos1 <= pos2, "Second index of the pair is lower than the first");
    (void)pos1;
    (void)pos2;
}

template <typename T>
inline void  //
IndexedList<T>::checkIndexIsValid(size_type pos) const {
    INDEXED_LIST_CONTRACT_ASSERT(pos < size(), "Index outside of valid range");
    (void)pos;
}

template <typename T>
inline void  //
IndexedList<T>::checkIndexIsValidInclusive(size_type pos) const {
    INDEXED_LIST_CONTRACT_ASSERT(pos <= size(), "Index outside of valid range");
    (void)pos;
}

template <typename T>
inline void  //
IndexedList<T>::checkIteratorsInOrder(const_iterator it1,
                                      const_iterator it2) const {
    INDEXED_LIST_CONTRACT_ASSERT(
        it1 <= it2,
        "Second iterator of the pair is earlier in container than the first");
    (void)it1;
    (void)it2;
}

template <typename T>
inline void  //
IndexedList<T>::checkIteratorIsValid(const_iterator it) const {
    INDEXED_LIST_CONTRACT_ASSERT(it.node->getTreeHead() == impl_.getHeadNode(),
                                 "Iterator does not belong to this container");
    (void)it;
}

template <typename T>
template <typename InputIt>
inline void  //
IndexedList<T>::checkIteratorNotFromContainer(InputIt first,
                                              InputIt last) const {
// I don't even hope that the optimizer will remove it by itself
#if defined(INDEXED_LIST_CHECK_PRECONDITIONS)
    auto getAddress = [](auto&& v) { return &v; };
    if (first == last)
        return;
    for (const auto& el : *this) {
        INDEXED_LIST_CONTRACT_ASSERT(getAddress(*first) == &el,
                                     "Invalid iterator range");
    }
#endif
    (void)first;
    (void)last;
}

// static
template <typename T>
inline const typename IndexedList<T>::Node&  //
IndexedList<T>::getFullNode(const NodeBase* node) {
    return static_cast<const Node&>(*node);
}

// static
template <typename T>
inline typename IndexedList<T>::Node&  //
IndexedList<T>::getFullNode(NodeBase* node) {
    return static_cast<Node&>(*node);
}

template <typename T>
inline void  //
IndexedList<T>::clearNode(NodeBase* node) {
    auto&& aloc     = getAllocator();
    Node* full_node = &getFullNode(node);
    std::allocator_traits<real_allocator_type>::destroy(aloc,
                                                        &full_node->value);
    aloc.deallocate(full_node, 1);
}

template <typename T>
inline typename IndexedList<T>::NodeBase*  //
IndexedList<T>::cloneNode(const NodeBase* node) {
    auto&& aloc      = getAllocator();
    Node* new_node   = aloc.allocate(1);
    new_node->weight = node->weight;
    new_node->value  = getFullNode(node).value;
    return new_node;
}

template <typename T>
inline typename IndexedList<T>::real_allocator_type  //
IndexedList<T>::getAllocator() {
    return real_allocator_type();
}

template <typename T>
template <typename... Args>
inline typename IndexedList<T>::NodeBase*  //
IndexedList<T>::getNewNode(Args&&... args) {
    auto&& aloc = getAllocator();
    Node* node  = aloc.allocate(1);
    std::allocator_traits<real_allocator_type>::construct(
        aloc, node, std::forward<Args>(args)...);
    node->subtree_size = 1;
    do {
        node->weight = rng_();
    } while (node->weight == 0);
    node->left = node->right = node->parent = nullptr;
    return node;
}

template <typename T>
inline typename IndexedList<T>::NodeBase*  //
IndexedList<T>::getNotConstNode(const_iterator it) {
    // This const_cast is valid, because:
    // 1. pos is an iterator to this container
    // 2. we are inside a non-const method, so this container isn't const
    // 3. because of that, the pointed-to element isn't const
    return const_cast<NodeBase*>(it.node);
}

}  // namespace indexed_list

#undef INDEXED_LIST_CONTRACT_ASSERT_PREFIX
#undef INDEXED_LIST_LIBRARY_ASSERT_PREFIX
#undef INDEXED_LIST_CONTRACT_ASSERT
#undef INDEXED_LIST_LIBRARY_ASSERT
#undef INDEXED_LIST_CHECK_PRECONDITIONS

#endif  // INDEXED_LIST_H_
