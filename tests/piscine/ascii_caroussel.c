#include <stddef.h>
#include <stdio.h>

void rot_x(char *s, int x)
{
    if (s == NULL)
        return;
    size_t i = 0;
    int y;
    if (x < 0)
        x = 26 + x;
    while (s[i] != 0)
    {
        if (s[i] <= 'z' && s[i] >= 'a')
        {
            y = 'a' + ((s[i] - 'a') + x) % 26;
            s[i] = y;
        }
        else if (s[i] <= 'Z' && s[i] >= 'A')
        {
            y = 'A' + ((s[i] - 'A') + x) % 26;
            s[i] = y;
        }
        i++;
    }
}

int main(void)
{
    char in[] = "Shhh ShE is ZZZzZ059%";
    printf("In: %s\n", in);
    rot_x(in, 13);
    printf("Out: %s\n", in);
    return 0;
}
