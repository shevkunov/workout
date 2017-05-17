
/** предыдущая версия аллокатора не проходила тесты
 * и была отправлена в Helheim
 **/

#include <vector>
#include <iostream>
#include <mutex>


#define _Log

class Log {
public:
    template <typename T>
    static inline void out(const T& s) {
        (void) s; // чтоб не был предупреждений
    #ifdef _Log
        std::cout << "Log:" << s << "\n";
        std::cout.flush();
    #endif
    }

    template <typename T, typename ... Ts>
    static inline void out(const T& s, Ts ... ss) {
        (void) s; // чтоб не был предупреждений
    #ifdef _Log
        std::cout << "Log:" << s << "\n";
        std::cout.flush();
    #endif
        out(ss...);
    }

private:
    Log() {
        // я не такая
    }
};



class Halloc {
public:
    void* malloc(const size_t& alloc_size);
    void free(void* pointer);
private:

};


/** костыль под тестирующую систему через
 * 3
 * 2
 * 1
 **/

extern Halloc _hoard_allocator; // Стоило бы сделать singleton, но пофиг

extern void* mtalloc(size_t alloc_size) {
    Log::out("Attempt to alloc:", alloc_size);
    void* allocated = _hoard_allocator.malloc(alloc_size);
    Log::out("Allocated:", allocated);
    return allocated;
}

extern void mtfree(void* pointer) {
    Log::out("Attempt to free:", pointer);
    _hoard_allocator.free(pointer);
    Log::out("Free:", pointer);
}
