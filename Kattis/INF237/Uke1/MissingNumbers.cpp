#include <iostream>
#include <string>
using namespace std;

int main(){
    int count;
    scanf("%d", &count);
    int prev {0};
    int current;
    for (int i {0}; i < count; i++){
        scanf("%d", &current);
        for (int j {prev+1}; j < current; j++){
            cout << j << '\n';
        }
        prev = current;

        if (i == count - 1 && current == count){
            cout << "good job\n";
        }
    }
}