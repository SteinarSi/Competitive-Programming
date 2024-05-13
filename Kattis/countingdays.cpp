#include "countingdays.h"

int d = 1;
int h = 0;
int m = 0;

void lookAtClock(int hours, int minutes) {
    if (hours < h || (hours == h && minutes < m)) {
        d += 1;
    }
    h = hours;
    m = minutes;
}

int getDay() {
    return d;
}
