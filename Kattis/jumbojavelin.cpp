#include <iostream>

using namespace std;

int main(){
    int n;
    int l;
    cin >> n;
    int sum {0};
    for (int i {0}; i < n; i++){
        cin >> l;
        sum += l;
    }
    cout << sum - n + 1 << '\n';
}