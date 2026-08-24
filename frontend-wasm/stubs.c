#include <stdint.h>
#include <stddef.h>

// Stub implementations for GHC RTS threading primitives
int32_t forkOS_createThread(void* param) { return 0; }
void setTimerManagerControlFd(int32_t fd) {}
void setIOManagerWakeupFd(int32_t fd) {}
void blockUserSignals(void) {}
void unblockUserSignals(void) {}
void osReleaseFreeMemory(void) {}
void osFreeMBlocks(void* addr, uint32_t len) {}

// Forward declarations
extern void hs_init(int *argc, char **argv[]);
extern void reactor_start(void);

// Wrapper that initializes RTS and starts the reactor
void start_reactor(void) {
    static int argc = 1;
    static char arg0[] = "wasm-reactor";
    
    // Correctly structure argv for hs_init(int*, char**[])
    // hs_init expects a char***, so we need a char** that points to the array
    static char *argv_arr[] = { arg0, NULL };
    static char **argv = argv_arr;
    
    hs_init(&argc, &argv);
    reactor_start();
}
