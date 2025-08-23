#include <iostream>
#include <map>

using namespace std;

int maxi(long p, long n, map<long, bool> &memo);
int mini(long p, long n, map<long, bool> &memo);

int maxi(long p, long n, map<long, bool> &memo){
    if (p * 9 >= n || (memo.count(p) && memo[p])) return 1;
    int r = -1;
    for (int i {2}; i <= 9; i++){
        r = max(r, mini(p*i, n, memo));
        if (r == 1) break;
    }
    memo[p] = r == 1;
    return r;
}

int mini(long p, long n, map<long, bool> &memo){
    if (p * 9 >= n || (memo.count(p) && memo[p])) return -1;
    int r = 1;
    for (int i {2}; i <= 9; i++){
        r = min(r, maxi(p*i, n, memo));
        if (r == -1) break;
    }
    memo[p] = r == -1;
    return r;
}

int main(){
    long n;
    map<long, bool> memo;
    while (cin >> n){
        memo = map<long, bool>();
        if (maxi(1, n, memo) == 1) printf("Stan wins.\n");
        else printf("Ollie wins.\n");
    }
}
