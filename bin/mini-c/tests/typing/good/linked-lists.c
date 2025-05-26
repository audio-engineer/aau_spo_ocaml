// #include <stdlib.h>
// #include <stdio.h>

struct list {
  int val;
  struct list * next;
};

struct list * nil() {
  return 0;
}


struct list * cons(int x, struct list * lst)
{
  struct list * head;
  head = malloc(sizeof(struct list));
  head->next = lst;
  head->val = x;
  return head;
}

int print(struct list *lst)
{
  struct list * tmp;
  while (lst != 0) {
    tmp = lst->next;
    putchar(lst->val);
    lst = tmp;
  };
  return 0;
}


int main() {
  struct list* l;
  l = nil();
  l = cons(99, l);  // c
  l = cons(45, l);  // -
  l = cons(105, l); // i
  l = cons(110, l); // n
  l = cons(105, l); // i
  l = cons(109, l); // m
  print(l);
  return 0;
}
