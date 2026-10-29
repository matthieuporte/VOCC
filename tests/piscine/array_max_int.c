
#include <stddef.h>

void array_max_min(int tab[], size_t len, int *max, int *min)
{
    if (tab == NULL || len == 0)
        return;
    int mini = tab[0];
    int maxi = tab[0];
    for (size_t i = 1; i < len; i++)
    {
        if (tab[i] < mini)
            mini = tab[i];
        if (tab[i] > maxi)
            maxi = tab[i];
    }
    *max = maxi;
    *min = mini;
}

int main (){
  int tab[] = {1,43,23,999,3};

  int max = 0;
  int min = 0;

  array_max_min(tab, 5, &max, &min);

  return max + min;
}
