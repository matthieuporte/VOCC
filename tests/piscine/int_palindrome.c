int int_palindrome(int n)
{
    if (n < 0)
        return 0;
    int arr[5] = { 0 };
    int i = 0;
    while (n > 0)
    {
        arr[i] = n % 10;
        n /= 10;
        i++;
    }
    for (int j = 0; j < i / 2; j++)
    {
        if (arr[j] != arr[i - j - 1])
            return 0;
    }
    return 1;
}


int main () {

  return int_palindrome(131) || int_palindrome(144) && int_palindrome(111);
}
