#include <iostream>
#include <string>
using namespace std;

int main(){
    string input_line;
    getline(cin, input_line);
    int n = stoi(input_line);

    for (int i {1}; i <= n; i++){
        cout << i << " Abracadabra\n";
    }
}
