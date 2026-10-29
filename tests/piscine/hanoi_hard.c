#include <stdio.h>

void __han(unsigned start, unsigned mid, unsigned end, unsigned n)
{
    if (n == 1)
        printf("%d->%d\n", start, end);
    else
    {
        __han(start, end, mid, n - 1);
        printf("%d->%d\n", start, end);
        __han(mid, start, end, n - 1);
    }
}

void hanoi(unsigned n)
{
    __han(1, 2, 3, n);
}


int main () {
  /*
    This should give :
        $ ./out | wc -l
        1048575

    */
  hanoi (20);
}
