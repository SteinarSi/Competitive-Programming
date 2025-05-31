#include <vector>
#include <cmath>
#include <string>
#include <iostream>
#include <algorithm>
#include <iomanip>

using namespace std;

class SegmentTree{
    vector<long long> tree;
    int n;

    public:
    SegmentTree(int size, int p){
        n = size;
        tree = vector<long long>(pow(2,ceil(log2(n))+1), p);
    };

    int left(int i){ return 2 * i; };
    int right(int i){ return 2 * i + 1; }
    int parent(int i){ return i / 2; }
    int index(int i){ return n + i; }
    long long lookup(int i){ return tree[index(i)]; }

    void update(int i, long long value){
        int idx = index(i);
        tree[idx] = value;
        while ((idx = parent(idx)) > 0){
            tree[idx] = min(tree[left(idx)], tree[right(idx)]);
        }
    }
    
    long long query(int l, int r){
        l = index(l);
        r = index(r);
        long long ret {tree[l]};
        if (l == r) return ret;
        ret = min(ret, tree[r]);
        int pl, pr;
        while (true){
            pl = parent(l);
            pr = parent(r);
            if (pl == pr) return ret;
            if (l % 2 == 0) ret = min(ret, tree[right(pl)]);
            if (r % 2 == 1) ret = min(ret, tree[left(pr)]);
            l = pl;
            r = pr;
        }
    }
};

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int cases, n, communication, programming, algorithms, ret, lowest;
    cin >> cases;
    for (int cs {0}; cs < cases; cs++){
        cin >> n;
        vector<pair<int, pair<int, int>>> data(n);
        ret = 0;
        for (int i {0}; i < n; i++){
            cin >> communication >> programming >> algorithms;
            communication--;
            programming--;
            algorithms--;
            data[i] = {communication, {programming, algorithms}};
        }
        sort(data.begin(), data.end());
        SegmentTree st = SegmentTree(n, n+1);
        for (auto [x, yz] : data){
            if (yz.first == 0){
                ret += 1;
                st.update(0, yz.second);
            }else if (yz.second < st.query(0, yz.first)){
                ret += 1;
                st.update(yz.first, yz.second);
            }
        }

        cout << ret << '\n';
    }
}
