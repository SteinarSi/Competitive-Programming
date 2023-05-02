#include <string>
#include <iostream>
#include <cmath>

using namespace std;

int main(){
    string encoding, msg;
    getline(cin, encoding);
    cin >> msg;

    for (int i {0}; i < msg.size(); i += 3){
        cout << encoding[(msg[i  ] - '0') * 10 * 10 + (msg[i+1] - '0') * 10 + (msg[i+2] - '0') - 1];
    }
    cout << '\n';
}