#ifndef _BITSET_H_
#define _BITSET_H_

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

struct bitset {
    uint64_t *bit_groups;
    uint32_t number_of_bit_groups;

    // We track of the number of elements in order to detect mismatches.
    uint32_t number_of_elements;
};

static inline struct bitset bitset_create_empty(uint32_t number_of_elements)
{
    uint32_t number_of_groups = (number_of_elements + 63) / 64;
    return (struct bitset){
        .bit_groups = calloc(number_of_groups, sizeof(uint64_t)),
        .number_of_bit_groups = number_of_groups,
        .number_of_elements = number_of_elements,
    };
}

static inline void bitset_destroy(struct bitset *set)
{
    free(set->bit_groups);
}

static inline bool bitset_contains(struct bitset *set, uint32_t element)
{
    return set->bit_groups[element / 64] & (1ULL << (element % 64));
}

static inline void bitset_add(struct bitset *set, uint32_t element)
{
    set->bit_groups[element / 64] |= (1ULL << (element % 64));
}

void bitset_clear(struct bitset *set);
void bitset_union(struct bitset *set, struct bitset *other);
// Like `union`, but returns true if any new elements were added.
bool bitset_union_added(struct bitset *set, struct bitset *other);
int bitset_compare(struct bitset *a, struct bitset *b);

#endif
