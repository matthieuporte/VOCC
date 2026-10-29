
#include <stdio.h>

int n = 0;

void p(){
    printf("hello %d\n", n);
    n++;
}

int main()
{
    int a = (printf("Hello world\n"), 5);

    if (((p(), 1) || (p(), 0)) && (p(), 1)){
        1;
    }

    return a;
}
