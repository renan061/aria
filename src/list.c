#include <stdio.h>

#include "alloc.h"
#include "list.h"

#define for_each_node_n(l) for (ListNode* n = l->first; n; n = n->next)

// -----------------------------------------------------------------------------

static ListNode* newnode(ListValue value) {
    ListNode* node;
    MALLOC(node, ListNode);
    node->value = value;
    node->previous = NULL;
    node->next = NULL;
    return node;
}

static void unlink(List* list, ListNode* node) {
    if (list->first == node) {
        list->first = node->next;
    }
    if (list->last == node) {
        list->last = node->previous;
    }
    if (node->previous) {
        node->previous->next = node->next;
    }
    if (node->next) {
        node->next->previous = node->previous;
    }
    free(node);
}

// -----------------------------------------------------------------------------

List* list_new(void) {
    List* list;
    MALLOC(list, List);
    list->first = NULL;
    list->last = NULL;
    return list;
}

List* list_copy(List* original) {
    List* new = list_new();
    for_each_node_n(original) {
        list_append(new, n->value);
    }
    return new;
}

void list_destroy(List* list) {
    ListNode* n = NULL;
    ListNode* node = list->first;
    while (node) {
        n = node->next;
        free(node);
        node = n;
    }
    free(list);
}

int list_size(List* list) {
    int i = 0;
    for_each_node_n(list) {
        i++;
    }
    return i;
}

void list_prepend(List* list, ListValue value) {
    ListNode* node = newnode(value);
    if (list->first) {
        list->first->previous = node;
    } else {
        list->last = node;
    }
    node->next = list->first;
    list->first = node;
}

void list_append(List* list, ListValue value) {
    ListNode* node = newnode(value);
    if (list->last) {
        list->last->next = node;
    } else {
        list->first = node;
    }
    node->previous = list->last;
    list->last = node;
}

ListValue list_remove(List* list, ListValueCompare cmp, ListValue value) {
    ListValue found = NULL;
    for (ListNode* n = list->first; n; n = n->next) {
        if (!cmp) {
            if (n->value == value) {
                unlink(list, n);
                return value;
            }
        } else if ((found = cmp(n->value, value))) {
            unlink(list, n);
            return found;
        }
    }
    return NULL;
}

void list_dump(List* list, ListValuePrint print) {
    printf("[\n");

    ListNode* n = list->first;
    while (n) {
        printf("\t");
        print(n->value);
        if (!n->next) {
            break;
        }
        printf(",\n");
        n = n->next;
    }

    printf("\n]\n");
}
