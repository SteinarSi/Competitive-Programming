#include <iostream>
#include <vector>
#include <iomanip>
#include <cstdint>
#include <cmath>

using namespace std;
using u64 = uint64_t;
using u128 = __uint128_t;

vector<u64> PRIMES = {2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997,1009};
u64 MOD = 1000000007;

bool is_prime(u64 x) {
    if (x < 2) return false;
    if (x == 2) return true;
    u64 s = sqrt(x)+1;
    for (u64 p : PRIMES) {
        if (p > s) return true;
        if ( ! (x % p)) return false;
    }
    return true;
}

u64 binpower(u64 base, u64 e, u64 mod) {
    u64 result = 1;
    base %= mod;
    while (e) {
        if (e & 1) result = (u128)result * base % mod;
        base = (u128)base * base % mod;
        e >>= 1;
    }
    return result;
}

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    u64 e, l, r;
    cin >> e >> l >> r;
    u64 ret {1};
    for (u64 x {l}; x <= r; x++) {
        if (is_prime(x)) {
            ret = (ret * binpower(x,e,MOD)) % MOD;
        }
    }
    cout << ret << '\n';
}
