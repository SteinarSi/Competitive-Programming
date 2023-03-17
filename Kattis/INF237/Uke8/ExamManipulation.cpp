#include <iostream>
#include <vector>
#include <string>
#include <cmath>

using namespace std;

int main(){
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    int n, k;
    cin >> n >> k;
    vector<int> answers(n, 0);
    int answer;
    string line;
    for (int i {0}; i < n; i++){
        answer = 0;
        cin >> line;
        for (int j {0}; j < k; j++){
            if (line[j] == 'T') answer |= 1 << j;
        }
        answers[i] = answer;
    }

    int best = 0;
    int curr;
    for (int bit {0}; bit < (1 << k); bit++){
        curr = 999999999;
        for (int i {0}; i < n; i++){
            int answer = answers[i];
            int count = 0;
            for (int j {0}; j < k; j++){
                if ((answer & (1 << j)) != (bit & (1 << j))) {
                    count++;
                }
            }
            curr = min(curr, count);
        }
        best = max(curr, best);
    }
    cout << best << '\n';
}