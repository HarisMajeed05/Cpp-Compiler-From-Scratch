#include <iostream>
#include <string>
using namespace std;
int main()
{
    int a = 0;
    a = a + 1;
    if (a == 1)
    {
        a = a + 2;
    }
    else
    {
        a = a - 1;
    }
    while (a < 10)
    {
        a = a + 1;
    }
    for (int i = 0; i < 3; i = i + 1)
    {
        a = a + i;
    }
    return a;
    // This is a comment
    // Another comment
}
