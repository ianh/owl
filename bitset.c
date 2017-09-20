#include "bitset.h"

void bitset_clear(struct bitset *set)
{
    for (uint32_t i = 0; i < set->number_of_bit_groups; ++i)
        set->bit_groups[i] = 0;
}

void bitset_union(struct bitset *set, struct bitset *other)
{
    if (set->number_of_bit_groups != other->number_of_bit_groups)
        abort();
    for (uint32_t i = 0; i < set->number_of_bit_groups; ++i)
        set->bit_groups[i] |= other->bit_groups[i];
}

int bitset_compare(struct bitset *a, struct bitset *b)
{
    if (a->number_of_bit_groups != b->number_of_bit_groups)
        abort();
    uint32_t n = a->number_of_bit_groups;
    for (uint32_t i = 0; i < n; ++i) {
        if (a->bit_groups[i] < b->bit_groups[i])
            return -1;
        if (a->bit_groups[i] > b->bit_groups[i])
            return 1;
    }
    return 0;
}
