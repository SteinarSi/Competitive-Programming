#include <vector>
#include <cmath>
#include <string>
#include <iostream>

using namespace std;

class SegmentTree{
    vector<long long> tree;
    int n;

    public:
    SegmentTree(vector<long long> data){
        n = data.size();
        tree = vector<long long>(pow(2,ceil(log2(n))+1), 0);
        for (int i {n}; i < 2 * n; i++){
            tree[i] = data[i-n];
        }

        for (int i {n-1}; i > 0; i--){
            tree[i] = tree[left(i)] + tree[right(i)];    // Endre på plussen til op
        }
    };

    int left(int i){ return 2 * i; };
    int right(int i){ return 2 * i + 1; }
    int parent(int i){ return i / 2; }
    int index(int i){ return n + i; }
    long long lookup(int i){ return tree[index(i)]; }

    void update(int i, long long value){
        int idx = index(i);
        tree[idx] = value;
        while ((idx = parent(idx)) > 0){ //TODO: sjekk at dette trikset fungerer
            tree[idx] = tree[left(idx)] + tree[right(idx)];    // Endre på plussen til op
        }
    }

    void print(){
        for (auto n : tree) cout << n << ' ';
        cout << endl;
    }

    long long query(int l, int r){
        l = index(l);
        r = index(r);
        long long ret {0};
        ret += tree[l];             // erstatt + med op
        if (l == r) return ret;
        ret += tree[r];             //pluss på r bare om vi skal ha [l,r], ikke om vi skal ha [l, r>
        int pl, pr;
        while (true){
            pl = parent(l);
            pr = parent(r);
            if (pl == pr) return ret;
            if (l % 2 == 0) ret += tree[right(pl)];        //erstatt + med op
            if (r % 2 == 1) ret += tree[left(pr)];
            l = pl;
            r = pr;
        }
    }
};