#include "bitset.h"

#include "alloc.h"

struct bitset bitset_create_empty(uint32_t number_of_elements)
{
    uint32_t number_of_groups = (number_of_elements + 63) / 64;
    return (struct bitset){
        .bit_groups = calloc(number_of_groups, sizeof(uint64_t)),
        .number_of_bit_groups = number_of_groups,
        .number_of_elements = number_of_elements,
    };
}

void bitset_clear(struct bitset *set)
{
    for (uint32_t i = 0; i < set->number_of_bit_groups; ++i)
        set->bit_groups[i] = 0;
}

void bitset_destroy(struct bitset *set)
{
    free(set->bit_groups);
}

void bitset_union(struct bitset *set, struct bitset *other)
{
    if (set->number_of_elements != other->number_of_elements)
        abort();
    for (uint32_t i = 0; i < set->number_of_bit_groups; ++i)
        set->bit_groups[i] |= other->bit_groups[i];
}

bool bitset_intersects(struct bitset *set, struct bitset *other)
{
    if (set->number_of_elements != other->number_of_elements)
        abort();
    uint32_t hits = 0;
    for (uint32_t i = 0; i < set->number_of_bit_groups; ++i) {
        if (set->bit_groups[i] & other->bit_groups[i])
            hits++;
    }
    return hits > 0;
}

bool bitset_union_added(struct bitset *set, struct bitset *other)
{
    if (set->number_of_elements != other->number_of_elements)
        abort();
    bool added = false;
    for (uint32_t i = 0; i < set->number_of_bit_groups; ++i) {
        uint64_t group = set->bit_groups[i];
        group |= other->bit_groups[i];
        if (group == set->bit_groups[i])
            continue;
        set->bit_groups[i] = group;
        added = true;
    }
    return added;
}

bool bitset_is_empty(struct bitset *set)
{
    for (uint32_t i = 0; i < set->number_of_bit_groups; ++i) {
        if (set->bit_groups[i] != 0)
            return false;
    }
    return true;
}

int bitset_compare(struct bitset *a, struct bitset *b)
{
    if (a->number_of_elements != b->number_of_elements)
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
