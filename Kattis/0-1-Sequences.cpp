#include <iostream>
#include <string>
#include <tgmath.h>
using namespace std;

int64_t power(int64_t base, int64_t exponent, int64_t mod)
{
    if (mod == 1) return 0;
    int64_t result = 1;
    base = base % mod;
    while (exponent > 0)
    {
        if (exponent % 2 == 1) result = (result * base) % mod;
        exponent = exponent >> 1;
        base = (base * base) % mod;
    }
    return result;
}

int64_t inversion(string seq)
{
    //int64_t so that it doesnt overflow
    int64_t num1 = 0;
    int numQ = 0;
    int64_t T = 0;
    int mod = 1000000007;
    for (int i = 0; i < seq.size(); i++)
    {
        int64_t foo,bar;
        switch (seq[i])
        {
            case '1':
                foo = num1 + power(2, numQ, mod);
                num1 = foo % mod;
                break;
            case '0':
                foo = T + num1;
                T = foo % mod;
                break;
            case '?':
                foo = (2 * T) + num1;//T
                bar = (2 * num1) + power(2, numQ, mod); //N
                T = foo % mod;
                num1 = bar % mod;
                numQ++;
                break;
            default:
                break;
        }
    }
    return T;
}



int main()
{
    string seq;
    std::cin >> seq;//get input from stream
    //int64_t tst = (power(2, 500000, 1000000007));
    //std::cout << tst << endl;
    std::cout << inversion(seq) << endl; //output results
}