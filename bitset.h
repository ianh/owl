#ifndef _BITSET_H_
#define _BITSET_H_

#include <stdint.h>
#include <stdlib.h>

struct bitset {
    uint64_t *bit_groups;
    uint32_t number_of_bit_groups;
};

static inline struct bitset bitset_create_empty(uint32_t number_of_elements)
{
    uint32_t number_of_groups = (number_of_elements + 63) / 64;
    return (struct bitset){
        .bit_groups = calloc(number_of_groups, sizeof(uint64_t)),
        .number_of_bit_groups = number_of_groups,
    };
}

static inline void bitset_destroy(struct bitset *set)
{
    free(set->bit_groups);
}

static inline void bitset_clear(struct bitset *set)
{
    for (uint32_t i = 0; i < set->number_of_bit_groups; ++i)
        set->bit_groups[i] = 0;
}

static inline bool bitset_contains(struct bitset *set, uint32_t element)
{
    return set->bit_groups[element / 64] & (1ULL << (element % 64));
}

static inline void bitset_add(struct bitset *set, uint32_t element)
{
    set->bit_groups[element / 64] |= (1ULL << (element % 64));
}

static inline void bitset_union(struct bitset *set, struct bitset *other)
{
    if (set->number_of_bit_groups != other->number_of_bit_groups)
        abort();
    for (uint32_t i = 0; i < set->number_of_bit_groups; ++i)
        set->bit_groups[i] |= other->bit_groups[i];
}

#endif
