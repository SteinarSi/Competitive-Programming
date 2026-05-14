#include <vector>
#include <cmath>
#include <string>
#include <iostream>
#include <cstdint>
#include <algorithm>

using namespace std;

template <typename Data, typename Op>
class SegmentTree{
    vector<Data> tree;
    int n;
    bool incl;
    Op f;

    public:
    SegmentTree(vector<Data> data, bool inclusive, Op f) : f(f) {
        n = data.size();
        incl = inclusive;
        tree = vector<Data>(pow(2,ceil(log2(n))+1), 0);
        for (int i {n}; i < 2 * n; i++){
            tree[i] = data[i-n];
        }

        for (int i {n-1}; i > 0; i--){
            tree[i] = f(tree[left(i)], tree[right(i)]);
        }
    };

    int left(int i){ return 2 * i; };
    int right(int i){ return 2 * i + 1; }
    int parent(int i){ return i / 2; }
    int index(int i){ return n + i; }
    Data lookup(int i){ return tree[index(i)]; }

    void update(int i, Data value){
        int idx = index(i);
        tree[idx] = value;
        while ((idx = parent(idx)) > 0){
            tree[idx] = f(tree[left(idx)], tree[right(idx)]);
        }
    }

    Data query(int l, int r){
        l = index(l);
        r = index(r);
        Data ret {tree[l]};
        if (l == r) return ret;
        if (incl) ret = f(ret, tree[r]);
        int pl, pr;
        while (true){
            pl = parent(l);
            pr = parent(r);
            if (pl == pr) return ret;
            if (l % 2 == 0) ret = f(ret, tree[right(pl)]);
            if (r % 2 == 1) ret = f(ret, tree[left(pr)]);
            l = pl;
            r = pr;
        }
    }
};

int main(){
    int n, q, a, b;
    char t;
    cin >> n >> q;

    vector<int> arr(n,0);
    SegmentTree st = SegmentTree(arr, true, [](int a, int b){
        return min(a,b);
    });

    for (int i {q}; i --> 0;) {
        cin >> t >> a >> b;
        if (t == 'U') st.update(a-1, b);
        else cout << st.query(a-1,b-1) << '\n';
    }
}
