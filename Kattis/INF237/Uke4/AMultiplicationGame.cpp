#include <iostream>
#include <map>

using namespace std;

int maxi(long p, map<long, int> &memo, long n);
int mini(long p, map<long, int> &memo, long n);

int maxi(long p, map<long, int> &memo, long n){
    if (p * 9 >= n) return 1;
    if (memo.find(p) != memo.end()) return memo[p];
    int ret = mini(p*9, memo, n) == 1 || mini(p*2, memo, n) == 1 ? 1 : -1;
    memo[p] = ret;
    return ret;
}

int mini(long p, map<long, int> &memo, long n){
    if (p * 9 >= n) return -1;
    if (memo.find(p) != memo.end()) return memo[p];
    int ret = maxi(p*9, memo, n) == -1 || maxi(p*2, memo, n) == -1 ? -1 : 1;
    memo[p] = ret;
    return ret;
}

void simon(long n){
    long p {1};
    bool player {false};
    while (p < n){
        player = ! player;
        if (player) p *= 9;
        else p *= 2;
    }
    if (player) printf("Stan wins.\n");
    else printf("Ollie wins.\n");
}

int main(){
    long n;
    while (cin >> n){
        simon(n);
    }
}
