#include <iomanip>
#include <iostream>
#include <vector>

using namespace std;

struct Person {
    string name;
    struct Person* front; 
    struct Person* back;
    struct Person* partner;

    void attach(Person *other) {
        if (other->back != nullptr) {
            back = other->back;
            other->back->front = this;
        }
        front = other;
        other->back = this;
    }

    void deattach() {
        if (front != nullptr) front->back = back;
        if (back != nullptr) back->front = front;
        front = nullptr;
        back = nullptr;
    }
};

int main() {
    cin.tie(nullptr);
    ios::sync_with_stdio(false);
    cin.exceptions(ios::failbit);

    int n, q;
    string name;

    cin >> n >> q;

    vector<Person> storage(2*n);
    for (int i {0}; i < 2*n; i++) {
        cin >> name;
        storage[i] = Person(name,nullptr,nullptr,nullptr);
    }
    for (int i {0}; i < 2*n; i++) {
        storage[i].partner = &storage[i^1];
        if (i != 0) {
            storage[i].front = &storage[i-1];
            storage[i-1].back = &storage[i];
        }
    }

    Person* start = &storage[0];
    Person* end   = &storage[2*n-1];
    Person* curr  = &storage[0];

    string queries;
    cin >> queries;
    for (char c : queries) {
        if (c == 'F') curr = curr->front;
        else if (c == 'B') curr = curr->back;
        else if (c == 'R') {
            if (curr->back == nullptr) curr = start;
            else {
                Person* next = curr->back;
                if (curr == start) start = next;
                curr->deattach();
                curr->attach(end);
                end = curr;
                curr = next;
            }
        }
        else if (c == 'C') {
            Person* next;
            if (curr->back != nullptr) next = curr->back;
            else next = start;
            if (start == curr) start = curr->back;
            if (end == curr->partner) end = curr;
            curr->deattach();
            curr->attach(curr->partner);
            curr = next;
        }
        else if (c == 'P') cout << curr->partner->name << '\n';
        else cout << "bruh\n";
    }

    curr = start;
    cout << '\n' << start->name << '\n';
    while (curr->back != nullptr) {
        curr = curr->back;
        cout << curr->name << '\n';
    }
}
