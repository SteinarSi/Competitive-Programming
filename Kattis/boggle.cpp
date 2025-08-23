#include <iostream>
#include <iomanip>
#include <vector>

using namespace std;

bool possible_from(vector<string> &boggle, string &word, int i, int mask, int x, int y) {
    if (i >= word.size()) return true;
    if (x < 0 || y < 0 || x >= 4 || y >= 4) {
        return false;
    }
    int s = 1 << (y*4 + x);
    if (mask & s) return false;
    mask |= s;
    if (boggle[y][x] != word[i]) return false;

    for (int a {y-1}; a <= y+1; a++) {
        for (int b {x-1}; b <= x+1; b++) {
            if (possible_from(boggle,word,i+1,mask,b,a)) return true;
        }
    }

    return false;
}

bool possible(vector<string> &boggle, string &word) {
    for (int y {0}; y < 4; y++) {
        for (int x {0}; x < 4; x++) {
            if (possible_from(boggle,word,0,0,x,y)) return true;
        }
    }
    return false;
}

const vector<int> points = {0,0,0,1,1,2,3,5,11};

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;

    int w, b;
    cin >> w;
    vector<string> dict(w);
    
    for (int i {0}; i < w; cin >> dict[i++]);
    
    cin >> b;
    vector<string> boggle(4);
    for (;b-->0;) {
        for (int i {0}; i < 4; cin >> boggle[i++]);
        int p = 0;
        int f = 0;
        string l = "";

        for (string &s : dict) {
            if (possible(boggle,s)) {
                p += points[s.size()];
                f += 1;
                if (s.size() > l.size() || s.size() == l.size() && s < l) l = s;
            }
        }

        cout << p << ' ' << l << ' ' << f << '\n';
    }
}
