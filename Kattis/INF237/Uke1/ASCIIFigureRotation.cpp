#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
using namespace std;

void solve(int size){
    vector<string> lines;
    string inn;
    int m {0};
    for (int i {0}; i < size; i++){
        getline(cin, inn);
        m = max(m, (int) inn.length());
        lines.push_back(inn);
    }
    for (int i=0; i < lines.size(); i++){
        string spaces(m - lines[i].length(), ' ');
        lines[i] += spaces;
    }
    
    vector<string> result;
    for (int i {0}; i < m; i++){
        string line {""};
        for (int j {size-1}; j >= 0; j--){
            char c = lines[j][i];
            if (c == '-'){
                line += '|';
            }else if (c == '|'){
                line += '-';
            }else{
                line += c;
            }
        }
        line.erase(line.find_last_not_of(" ")+1);
        result.push_back(line);
    }
    for (string s : result){
        cout << s << '\n';
    }
}

int main(){
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);
    cout << setprecision(10) << fixed;
    string size;
    int space {0};
    while (1){
        getline(cin, size);
        if (stoi(size) == 0){
            break;
        }
        if (space){
            cout << '\n';
        }else{
            space = 1;
        }
        solve(stoi(size));
    }
}
