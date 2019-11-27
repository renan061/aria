#if !defined(list_h)
#define list_h

#include <stddef.h>

// types

typedef void* ListValue;

typedef ListValue (*ListValueCompare)(ListValue, ListValue);

typedef void (*ListValuePrint)(ListValue);

typedef struct ListNode ListNode;
struct ListNode {
    ListValue value;
    ListNode* previous;
    ListNode* next;
};

typedef struct List List;
struct List {
    ListNode* first;
    ListNode* last;
};

// functions

extern List* list_new(void);
extern List* list_copy(List*);
extern void list_destroy(List*);

extern int list_size(List* list);
#define list_empty(list) (!list_size(list))

extern void list_prepend(List*, ListValue);
extern void list_append(List*, ListValue);

extern ListValue list_remove(List*, ListValueCompare, ListValue);

extern void list_dump(List*, ListValuePrint);

#endif
