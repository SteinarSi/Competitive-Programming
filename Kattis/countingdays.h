#include <iostream>

void lookAtClock(int hours, int minutes);
int getDay();

void checkDay(int expected) {
    int day = getDay();
    std::cout << "Program says day: " << day << std::endl;
    if (day != expected) {
        std::cout << "FAIL - expected " << expected << std::endl;
        exit(1);
    }
}

int main() {
    std::cout << "Time: 07:13 (day 1)" << std::endl;
    lookAtClock(7, 13);
    checkDay(1);
    std::cout << "Time: 23:59 (day 1)" << std::endl;
    lookAtClock(23, 59);
    checkDay(1);
    std::cout << "Time: 00:00 (day 2)" << std::endl;
    lookAtClock(0, 0);
    checkDay(2);
    std::cout << "Time: 23:59 (day 2)" << std::endl;
    lookAtClock(23, 59);
    checkDay(2);
    std::cout << "Time: 23:58 (day 3)" << std::endl;
    lookAtClock(23, 58);
    checkDay(3);
    std::cout << "Time: 07:13 (day 4)" << std::endl;
    lookAtClock(7, 13);
    checkDay(4);
    std::cout << "OK" << std::endl;
}
