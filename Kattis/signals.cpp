#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace std;

using namespace std;

class SegmentTree{
    vector<long long> tree;
    int n;

    public:
    SegmentTree(vector<long long> &data){
        n = data.size();
        tree = vector<long long>(pow(2,ceil(log2(n))+1), 0);
        for (int i {n}; i < 2 * n; i++){
            tree[i] = data[i-n];
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
        while ((idx = parent(idx)) > 0){ 
            tree[idx] = max(tree[left(idx)], tree[right(idx)]);
        }
    }

    void print(){
        for (auto n : tree) cout << n << ' ';
        cout << endl;
    }

    long long query(int l, int r){
        l = index(l);
        r = index(r);
        long long ret {tree[l]};
        if (l == r) return ret;
        int pl, pr;
        while (true){
            pl = parent(l);
            pr = parent(r);
            if (pl == pr) return ret;
            if (l % 2 == 0) ret = max(tree[right(pl)], ret);
            if (r % 2 == 1) ret = max(tree[left(pr)], ret);
            l = pl;
            r = pr;
        }
    }
};

int main(){
    int p;
    cin >> p;
    vector<int> graph;
    vector<long long> data(p, 0);
    SegmentTree opt(data);
    int b;
    for (int a {0}; a < p; a++){
        cin >> b;
        b--;
        if (b == 0) opt.update(b, 1);
        else opt.update(b, opt.query(0, b)+1);
    }
    cout << opt.query(0, p) << endl;
}