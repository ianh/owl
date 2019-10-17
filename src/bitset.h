#ifndef BITSET_H
#define BITSET_H

#include <stdbool.h>
#include <stdint.h>

struct bitset {
    uint64_t *bit_groups;
    uint32_t number_of_bit_groups;

    // We track of the number of elements in order to detect mismatches.
    uint32_t number_of_elements;
};

static inline struct bitset bitset_move(struct bitset *set)
{
    struct bitset s = *set;
    *set = (struct bitset){0};
    return s;
}

static inline bool bitset_contains(struct bitset *set, uint32_t element)
{
    return set->bit_groups[element / 64] & (1ULL << (element % 64));
}

static inline void bitset_add(struct bitset *set, uint32_t element)
{
    set->bit_groups[element / 64] |= (1ULL << (element % 64));
}

struct bitset bitset_create_empty(uint32_t number_of_elements);
void bitset_clear(struct bitset *set);
void bitset_destroy(struct bitset *set);
void bitset_union(struct bitset *set, struct bitset *other);
bool bitset_intersects(struct bitset *set, struct bitset *other);
// Like `union`, but returns true if any new elements were added.
bool bitset_union_added(struct bitset *set, struct bitset *other);
bool bitset_is_empty(struct bitset *set);
int bitset_compare(struct bitset *a, struct bitset *b);

#endif
