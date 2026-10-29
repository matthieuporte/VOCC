#include <stdio.h>

/* this is a
   multiline comment
   */

int main()

{

  int fahr, celsius;
  int lower, upper, step;

  lower = 0; // lower limit
  upper = 300; /* upper limit */
  step = 20; // step

  fahr = lower;

    printf("Celsius\tFahrenheit\n");
  while (fahr <= upper) {
    celsius = 5 * (fahr-32) / 9;

    printf("%d\t%d\n", celsius, fahr);

    fahr = fahr + step;
  }
}
