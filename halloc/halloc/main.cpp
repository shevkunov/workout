#include <iostream>
#include <vector>
#include <mutex>
#include <thread>


using namespace std;

extern void* mtalloc(size_t bytes);
extern void mtfree(void* ptr);

void write(void* ptr, size_t size, char id) {
    //log("write - begin", ptr);
    char* mem = reinterpret_cast<char*> (ptr);
    for (size_t i = 0; i < size; ++i) {
        mem[i] = id;
    }
    //log("write - end");
}

bool read(void* ptr, size_t size, char id) {
    //log("read - begin", ptr);
    char* mem = reinterpret_cast<char*> (ptr);
    char lo = mem[0];
    char hi = mem[0];
    for (size_t i = 0; i < size; ++i) {
        lo = (mem[i] < lo) ? mem[i] : lo;
        hi = (mem[i] > hi) ? mem[i] : hi;
    }
    //log("read - end");
    return (lo == hi) && (hi == id);
}

int main(void)
{
    {
    size_t MAX = 1;
    std::mutex mtx;
    vector<void*> alloc(MAX);
    void * a = mtalloc(8192);
    void * b = mtalloc(512);
    mtfree(a);
    mtfree(b);
/*
    std::thread t1([&mtx, &alloc]() {
        for (size_t i = 0; i < alloc.size(); ++i) {
            size_t size = (i + 1) * 10000;
            alloc[i] = mtalloc(size);
            write(alloc[i], size, (char)i);
        }
        return 0;
    });

    std::thread t2([&mtx, &alloc]() {
        std::this_thread::sleep_for(std::chrono::seconds(1));
        for (size_t i = 0; i < alloc.size(); ++i) {
            //log("t2", i);
            size_t size = (i + 1) * 100;
            if (!read(alloc[i], size, (char)i)) {
                cout << "HALT!\n";
                return 0;
            }
            //mtfree(alloc[i]);
        }
        return 0;
    });

    log("JOIN");
    t1.join();
    t2.join();
*/
    cout << "OK\n";
    }

    return 0;
}
