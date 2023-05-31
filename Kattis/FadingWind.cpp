#include <iostream>
#include <algorithm>

using namespace std;

int main(){
    int h, k, v, s, r;
    cin >> h >> k >> v >> s;
    for (r = 0; h; v += s, v -= v < 10, v -= v/10, h += v >= k, h -= v < k, h *= v > 0, v *= h > 0, r += v, s -= s > 0);
    cout << r << endl;
}