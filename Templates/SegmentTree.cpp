#include <vector>
#include <cmath>
#include <string>
#include <iostream>

using namespace std;

template <typename Data, typename Op>
class SegmentTree{
    vector<Data> tree;
    int n;
    bool incl;

    public:
    SegmentTree(vector<Data> data, bool inclusive, Op f){
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

    void print(){
        for (auto n : tree) cout << n << ' ';
        cout << endl;
    }

    Data query(int l, int r){
        l = index(l);
        r = index(r);
        Data ret {tree[l]};
        if (l == r) return ret;
        if (incl) ret = f(ret, tree[r]);
        // ret = f(ret, tree[r]); // ta med denne om vi vi skal ha [l, r] istedet for [l,r>
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
    vector<int> arr {0, 1, 2, 3, 4, 5};
    SegmentTree st = SegmentTree(arr, true, [](int a, int b){
        return a + b;
    });
}