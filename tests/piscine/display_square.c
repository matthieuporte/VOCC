#include <stdio.h>

void display_full_line(int width)
{
    for (int i = 0; i < width; i++)
    {
        putchar('*');
    }
    putchar('\n');
}

// shoud only be called with at least 3
void display_empty_line(int width)
{
    putchar('*');
    for (int i = 0; i < width - 2; i++)
    {
        putchar(' ');
    }
    putchar('*');
    putchar('\n');
}

void display_square(int width)
{
    if (width <= 0)
        return;
    if (width % 2 == 0)
        width++;
    if (width == 1)
    {
        putchar('*');
        putchar('\n');
    }
    else
    {
        display_full_line(width);
        for (int i = 1; i < width / 2; i++)
            display_empty_line(width);
        display_full_line(width);
    }
}

int main(void)
{
    display_square(0);
    display_square(1);
    display_square(2);
    display_square(6);
    display_square(11);
}
