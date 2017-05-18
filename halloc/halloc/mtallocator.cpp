
/** предыдущая версия аллокатора не проходила тесты
 * и была отправлена в Helheim
 **/

#include <vector>
#include <iostream>
#include <mutex>
#include <thread>

#define _Log

class Log {
public:
    template <typename T>
    static inline void out(const T& s)
    {
        (void) s; // чтоб не был предупреждений
    #ifdef _Log
        std::cout << "Log:" << s << "\n";
        std::cout.flush();
    #endif
    }

    template <typename T, typename ... Ts>
    static inline void out(const T& s, Ts ... ss)
    {
        (void) s; // чтоб не был предупреждений
    #ifdef _Log
        std::cout << "Log:" << s << "\n";
        std::cout.flush();
    #endif
        out(ss...);
    }

private:
    Log()
    {
        // я не такая
    }
};

const size_t& _hash_base = std::thread::hardware_concurrency() << 1;
const size_t _global_heap_id = ~static_cast <size_t>(0);
const thread_local size_t _current_thread_id = std::hash <std::thread>() (
        std::this_thread::get_id()
    ) % hash_base;

class Halloc {
public:
    Halloc(const size_t& superblock_size = 1 << 16,
           // 640 килобайт хватит всем!
           const size_t& minimal_block_per_superblock_ratio = 4,
           const size_t& empty_fraction_ratio = 3,
           const size_t& superblocks_free_ratio = 3
           ) : _superblock_size(superblock_size),
               _minimal_block_per_superblock_ratio(
                   minimal_block_per_superblock_ratio),
               _empty_fraction_ratio(empty_fraction_ratio),
               _superblocks_free_ratio(superblocks_free_ratio),
               _thread_heaps_count(_hash_base),
               // new нужно тоже в Вальгаллу, ибо
               // позорный аллокатор получится, если new
               // им переопределить. Но тогда долой и весь stl.
               // Ну нафиг
               _thread_heaps_array(new heap*[_thread_heaps_count]),
               _global_heap(new heap)
    {
        for (size_t i = 0; i <_thread_heaps_count; ++i) {
            _thread_heaps_array[i] = new heap;
        }
    }

    ~Halloc() {
        for (size_t i = 0; i < _thread_heaps_count; ++i) {
            delete _thread_heaps_array[i];
        }

        delete _thread_heaps_array;
        delete _global_heap;
    }

    void* malloc(const size_t& alloc_size)
    {
        // сначала идёт заголовок, потом - мясо.
        size_t _size = alloc_size + sizeof(BlockHeader);
        void* pointer;
        if (bytes >= _superblock_size / _minimal_block_per_superblock_ratio) {
            pointer = mmap(NULL,
                       _size,
                       PROT_READ | PROT_WRITE | MAP_PRIVATE | MAP_ANONYMOUS,
                       -1,
                       0);

            if (_mapped == MAP_FAILED) {
                abort();  /// всё очень плохо
            }

            BlockHeader* header = reinterpret_cast <BlockHeader*> (pointer);
            header->_owner = nullptr;
            header->_mmap_size = _size;
            // собственно, мЯсо
            return header + sizeof(BlockHeader);
        } else {
            Heap* my_heap = _thread_heaps_array[_current_thread_id];
            std::unique_lock <std::mutex> lock(my_heap->mutex);
            Tray& my_tray = my_heap->get_tray_by_size(_size);
            SuperBlock* provider = my_tray.cut_almost_full_superblock();
            void* block = provider->catch_block();
            if (provider == nullptr) {
                std::unique_lock <std::mutex> _global_heap(_global_heap->mutex);
                SuperBlock* global_provider = _global_heap->
                        get_tray_by_size(_size).cut_almost_full_superblock();
                if (global_provider == nullptr) {
                    provider = new SuperBlock(_size);
                    provider->_owner = _current_thread_id;
                    block = provider->catch_block();
                    my_heap->_allocated += _superblock_size;
                } else {
                    // передаём суперблок (пустой) из глобальной кучи
                    // в локальную
                    block = global_provider->catch_block();
                    provider = global_provider;
                    global_provider->_owner = _current_thread_id;

                    my_heap->_used += global_provider->used;
                    my_heap->_allocated += _superblock_size;

                    _global_heap->_used -= global_provider->_used;
                    _global_heap->_allocated -= _superblock_size;
                }
            }

            // provider - это суперблок, т.е. _size -
            // размер блока внутри суперблока
            provider->_used += provider->_size;
            my_head._used += provider->_size;

            // оборачиваем кусок памяти заголовком, чтобы различать
            // черную и белую магию, если есть _owner - игнорируем
            // размер
            BlockHeader* header = reinterpret_cast<char*>(block);
            header->_owner = provider;

            // вырезали - нужно вернуть
            my_tray.push_superblock(provider);

            return reinterpret_cast <char*> (block) + sizeof(BlockHeader);
        }
    }

    void free(void* pointer)
    {
        // так делает free из stl, мы должны так же
        if (ptr == nullptr)
        {
            return;
        }

        BlockHeader* header = reinterpret_cast <char*> (pointer)
                                - sizeof(BlockHeader);
        SuperBlock* provider = header->_owner;

        if (provider != nullptr) {
            // захвачен halloc'ом
            size_t current
        } else {
            // захвачен напрямую
            munmap(pointer, header->_mmap_size);
        }
    }

private:
    const size_t _superblock_size;
    const size_t _empty_fraction_ratio;
    const size_t _superblocks_free_ratio;
    const size_t _minimal_block_per_superblock_ratio;

    size_t _thread_heaps_count;
    Heap** _thread_heaps_array;
    Heap* _global_heap;



    class SuperBlock {
    public:
        SuperBlock(size_t _size)
            : _used(0), _owner(0), _size(_size),
              _free_blocks(std::vector <size_t> (_superblock_size / _size)),
              _current_position(-1), _blocks_count(0),
              // да, это системный вызов, детка
              // (хз зачем, если мы везде new пихаем)
              _mapped(mmap(NULL,
                           _superblock_size,
                           PROT_READ | PROT_WRITE | MAP_PRIVATE | MAP_ANONYMOUS,
                           -1,
                           0))
        {
            if (_mapped == MAP_FAILED) {
                abort();  /// всё очень плохо
            }

            for (void* i  = _mapped;
                 i + _size <= _mapped + _superblock_size;
                 i += _size) {
                _free_blocks[++_current_position] = i;
            }

            _blocks_count = _current_position + 1;
        }

        ~SuperBlock()
        {
            munmap(_mapped, _superblock_size);
        }

        void* catch_block()
        {
            if (_current_position < _blocks_count) {
                    // void* нельзя складывать
                return  _free_blocks[_current_position--];
            } else {
                return nullptr;
            }
        }

        void release_block(void* first_byte_of_block) {
            // вся память последовательно разбита на куски по (_size)
            _free_blocks[_current_position++] = first_byte_of_block;
        }

        bool full() const {
            return _current_position == -1;
        }

    private:
        // на самом деле, _free_blocks и _сurrent_position задают
        // стек _free_blocks[0.._current_position], но std::stack
        // написан настолько через жопу (через std::deque), что
        // std::vector быстрее. Такие дела.
        //     да, в стеке - указатели на первые адрес блоков.
        //
        // по-хорошему, надо всё выделять только mmapом,
        // и этот массив - тоже.
        std::vector <void*> _free_blocks;
        std::mutex _mutex;

        size_t _used;
        size_t _size;
        size_t _owner;

        int _current_position;
        int _blocks_count;

        void* _mapped;
    };

    class Tray {
    public:
        Tray() : _used(0), _allocated(0) {}

        ~Tray()
        {
            for (size_t i = 0; i < _full_superblocks.size(); ++i) {
                delete _full_superblocks[i];
            }

            for (size_t i = 0; i < _almost_full_superblocks.size(); ++i) {
                delete _almost_full_superblocks[i];
            }
        }

        void push_superblock(SuperBlock* block)
        {
            if (block->full()) {
                _full_superblocks.push_back(block);
            } else {
                _almost_full_superblocks.push_back(block);
            }
        }

        SuperBlock* cut_almost_full_superblock()
        {
            if (_almost_full_superblocks.empty()) {
                return nullptr;
            } else {
                SuperBlock* block = _almost_full_superblocks.back();
                _almost_full_superblocks.pop_back();
                return block;
            }
        }

        void release_block_from_superblock(SuperBlock* superblock, void* block)
        {
            bool full = superblock->full();
            superblock->release_block(block);

            if (superblock->full()) {
                size_t i = 0;
                for (;(i < _full_superblocks.size())
                     && (_full_superblocks[i] != superblock);
                     ++i);

                swap(_full_superblocks[i], _full_superblocks[0]);
                _full_superblocks.pop_back();
                _almost_full_superblocks.push_back(superblock);
            }
        }

        SuperBlock* catch_non_full_superblock()
        {
            if (!_almost_full_superblocks.empty()) {
                SuperBlock* superblock = _almost_full_superblocks.back();
                _almost_full_superblocks.pop_back();
                return superblock;
            } else {
                // без шансов
                return nullptr;
            }
        }

    private:
        // да, это тоже стеки. По-хорошему надо очередь, но пофиг.
        std::vector <SuperBlock*> _almost_full_superblocks;
        std::vector <SuperBlock*> _full_superblocks;
        size_t _allocated;
        size_t _used;
    };

    class Heap {
    public:
        Heap(const size_t& minimal_size = 1 << 3) : _minimal_size(minimal_size)
        {
            size_t _size = _minimal_size;
            size_t _count = 0;
            while (_size < _superblock_size) {
                ++_count;
                _size <<= 1;
            }
            trays.resize(_count);
        }

        Tray& get_tray_by_size(const size_t& size)
        {
            return trays[_get_tray_index_by_size(size)];
        }

    private:
        std::vector <Tray> _trays;
        const size_t _minimal_size;

        // суперобьекты в разных треях имеют разные размеры
        // блоков. Здесь они - степени двойки
        size_t _get_tray_index_by_size(size_t size)
        {
            size_t id = 0;
            size_t size_i  = _minimal_size;
            while (size_i < size) {
                size_i <<= 1;
                ++i;
            }
            return i;
        }
    };

    class BlockHeader {
    public:
        SuperBlock* _owner;
        size_t _mmap_size;
    };
};


/** костыль под тестирующую систему через
 * 3
 * 2
 * 1
 **/

extern Halloc _hoard_allocator; // Стоило бы сделать singleton, но пофиг

extern void* mtalloc(size_t alloc_size)
{
    Log::out("Attempt to alloc:", alloc_size);
    void* allocated = _hoard_allocator.malloc(alloc_size);
    Log::out("Allocated:", allocated);
    return allocated;
}

extern void mtfree(void* pointer)
{
    Log::out("Attempt to free:", pointer);
    _hoard_allocator.free(pointer);
    Log::out("Free:", pointer);
}
