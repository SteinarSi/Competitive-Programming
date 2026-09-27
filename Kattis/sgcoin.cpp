#include <iostream>
#include <string>

using namespace std;
using i64 = int64_t;

const i64 M = 1000000007;

i64 next(i64 hash, string transaction) {
    for (char c : transaction) {
        hash = (hash * 31 + c) % M;
    }
    hash = (hash * 7) % M;
    i64 token = (1000000000 - hash) % M;
    cout << transaction << ' ' << token << '\n';
    return (hash + token) % M;
}

int main(){
    i64 hash;
    cin >> hash;

    hash = next(hash, "steinar-quickly-adds-dummy-transaction");
    hash = next(hash, "steinar-scams-everyone-mouhahahahahahahaha");
}
