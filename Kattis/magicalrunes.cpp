#include <string>
#include <iostream>

using namespace std;
using u64 = uint64_t;

string str(u64 c, int n){
    string ret;
    for (int i {0}; i < n; i++){
        if (c & (1 << i)) ret += 'B';
        else              ret += 'A';
    }
    return ret;
}

int main(){
    string init;
    int d;
    cin >> init >> d;
    int n = init.size();
    int c = 0;
    for (int i {0}; i < n; i++) c |= (init[i] == 'B') << i;
    c += d;
    cout << str(c, n) << '\n';
}
