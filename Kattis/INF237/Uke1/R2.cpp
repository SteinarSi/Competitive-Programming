#include <iostream>
#include <string>
using namespace std;

int main(){
    string a;
    string b;
    getline(cin, a, ' ');
    getline(cin, b);
    cout << stoi(a) + 2 * (stoi(b) - stoi(a)) << '\n';
}