#include <vector>
#include <cmath>
#include <string>
#include <iostream>

using namespace std;
using i64 = int64_t;

vector<i64> add(vector<i64> &a, vector<i64> &b){
    vector<i64> ret(a.size());
    for (int i {0}; i < a.size(); i++){
        ret[i] = a[i] + b[i];
    }
    return ret;
}


class SegmentTree{
    vector<vector<i64>> tree;
    int n;

    public:
    SegmentTree(vector<vector<i64>> data){
        n = data.size();
        tree = vector<vector<i64>>(pow(2,ceil(log2(n))+1));
        for (int i {n}; i < 2 * n; i++){
            tree[i] = data[i-n];
        }

        for (int i {n-1}; i > 0; i--){
            tree[i] = add(tree[left(i)],tree[right(i)]);
        }
    };

    int left(int i){ return 2 * i; };
    int right(int i){ return 2 * i + 1; }
    int parent(int i){ return i / 2; }
    int index(int i){ return n + i; }
    vector<i64> lookup(int i){ return tree[index(i)]; }

    void update(int i, vector<i64> value){
        int idx = index(i);
        tree[idx] = value;
        while ((idx = parent(idx)) > 0){
            tree[idx] = add(tree[left(idx)], tree[right(idx)]);
        }
    }

    vector<i64> query(int l, int r){
        l = index(l);
        r = index(r);
        vector<i64> ret {0, 0, 0, 0, 0, 0};
        ret = add(ret, tree[l]);
        if (l == r) return ret;
        ret = add(ret, tree[r]);
        int pl, pr;
        while (true){
            pl = parent(l);
            pr = parent(r);
            if (pl == pr) return ret;
            if (l % 2 == 0) ret = add(ret, tree[right(pl)]);
            if (r % 2 == 1) ret = add(ret, tree[left(pr)]);
            l = pl;
            r = pr;
        }
    }
};

int main(){
    cin.exceptions(ios::failbit);
    int n, q;
    cin >> n >> q;
    vector<i64> gem_values(6);
    int value;
    for (int i {0}; i < 6; i++) {
        cin >> value;
        gem_values[i] = value;
    }
    char p;
    vector<i64> count;
    vector<vector<i64>> data(n);
    for(int i {0}; i < n; i++){
        count = {0, 0, 0, 0, 0, 0};
        cin >> p;
        count[p-'0'-1] = 1;
        data[i] = count;
    }

    SegmentTree st = SegmentTree(data);

    i64 c, a, b;
    i64 r;
    for (int i {0}; i < q; i++){
        cin >> c >> a >> b;
        if (c == 1){
            count = {0, 0, 0, 0, 0, 0};
            count[b-1] = 1;
            st.update(a-1, count);
        }
        else if (c == 2){
            gem_values[a-1] = b;
        }
        else{
            count = st.query(a-1, b-1);
            r = 0;
            for (int j {0}; j < 6; j++){
                r += count[j] * gem_values[j];
            }
            cout << r << '\n';
        }
    }
}