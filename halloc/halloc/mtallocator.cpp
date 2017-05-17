
/** предыдущая версия аллокатора не проходила тесты
 * и была отправлена в Helheim
 **/

#include <vector>
#include <mutex>

class Logger {
private:
    Logger() {
        // я не такая
    }
};

class Halloc {

};


/** костыль под тестирующую систему через
 * 3
 * 2
 * 1
 **/

extern Halloc _hoard_allocator; // Стоило бы сделать singleton, но пофиг

extern void* mtalloc(size_t alloc_size) {
    return _hoard_allocator.malloc(alloc_size);
}

extern void mtfree(void* pointer) {
    return _hoard_allocator.free(pointer);
}
