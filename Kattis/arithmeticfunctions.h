#include <iostream>

int cube(int x);
int max(int x, int y);
int difference(int x, int y);

int main() {
    int cube2 = cube(2);
    if (cube2 != 8) {
        std::cout << "ERROR: cube(2) = " << cube2 << ", expected 8" << std::endl;
    }
    int max85 = max(8, 5);
    if (max85 != 8) {
        std::cout << "ERROR: max(8, 5) = " << max85 << ", expected 8" << std::endl;
    }
    int diff102 = difference(10, 2);
    if (diff102 != 8) {
        std::cout << "ERROR: difference(10, 2) = " << diff102 << ", expected 8" << std::endl;
    }
}