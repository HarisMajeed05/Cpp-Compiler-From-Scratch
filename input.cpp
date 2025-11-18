#include <iostream>
using namespace std;
int add(int a, int b)
{
    return a + b;
}
int main()
{
    int f = 5;
    int b = 10;
    int z = 2;
    int a = 3;
    int h = 6 * f + 3 * 2 - b * b * z * z;
    int c = 10 - (-b * 10 + 12) + (5 - 10);
    // control flow
    if (a == 3)
    {
        a = a + 1;
    }
    else
    {
        a = a - 1;
    }
    for (int i = 0; i < 5; i = i + 1)
    {
        b = b + i;
    }
    // bitwise & shifts
    int y = (a & b) | (z ^ 3);
    int s = (b << 2) >> 1;
    int nb = ~a;

    return add(a, b);
    // return 0;
}
// trailing comment