#include <iostream>
#include <string>

using namespace std;

int main(){
    string s;
    cin >> s;
    for (int r {2}; r <= s.size(); r++){
        int c = s.size() / r;
        if (s.size() % r == 0 && r >= c){
            for (int i {0}; i < c; i++){
                for (int j {i}; j < s.size(); j+=c){
                    cout << s[j];
                }
            }

            cout << '\n';
            break;
        }
    }
}