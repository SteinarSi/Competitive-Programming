#include <vector>
#include <iostream>
#include "heap.h"

using namespace std;

int left(int i){ return 2 * i; };
int right(int i){ return 2 * i + 1; }
int parent(int i){ return i / 2; }

int bound = 256;
vector<int> heap = vector<int>(bound);
int SIZE = 0;

void bubbleUp(int index) {
    while (index != 1 && heap[index] > heap[parent(index)]) {
        int temp = heap[parent(index)];
        heap[parent(index)] = heap[index];
        heap[index] = temp;
        index = parent(index);
    }
}

int bubbleDown(int index) {
    while (true) {
        int l = left(index);
        int r = right(index);
        if (r <= SIZE && heap[r] >= heap[l]) {
    
            heap[index] = heap[r];
            index = r;
        }
        else if (l <= SIZE) {
    
            heap[index] = heap[l];
            index = l;
        }
        else return index;
    }
}

void grow() {
    int newBound = bound * 2;
    vector<int> newHeap(newBound);
    for (int i {1}; i < bound; i++) {
        newHeap[i] = heap[i];
    }
    heap = newHeap;
    bound = newBound;
}

int getMax(){
    return heap[1];
}

int getSize(){
    return SIZE;
}

void insert(int element){
    if (SIZE + 1 >= bound) {
        grow();
    }
    int index = ++SIZE;
    heap[index] = element;
    bubbleUp(index);
}

void removeMax(){
    int emptyIndex = bubbleDown(1);
    heap[emptyIndex] = heap[SIZE--];
    bubbleUp(emptyIndex);
}
